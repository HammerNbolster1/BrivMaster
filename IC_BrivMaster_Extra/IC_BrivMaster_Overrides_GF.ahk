class IC_BrivMaster_BrivGemFarm_Class extends IC_BrivGemFarm_Class
{
	Static IRI_LOOP_WAIT_FAST:=10 ;Milliseconds to wait for fast loop control (eg main loop)
	Static IRI_LOOP_WAIT_INPUT:=15 ;Milliseconds to wait for input related delays

	GemFarm()
    {
        static lastResetCount := 0
        this.TriggerStart:=true
        g_SF.Hwnd := WinExist("ahk_exe " . g_UserSettings[ "ExeName"])
        existingProcessID := g_UserSettings[ "ExeName"]
        Process, Exist, %existingProcessID%
        g_SF.PID := ErrorLevel
        Process, Priority, % g_SF.PID, Realtime ;Raises IC's priority if needed - the SH launch will just leave it at normal. Trying script High and game Realtime
        g_SF.Memory.OpenProcessReader()
		g_SF.Memory.GetChampIDToIndexMap() ;This is normally in the effect key handler, which is unhelpful for us, so having to call manually. TODO: Put somewhere sensible if using, or move everything to the LevelManager champ objects
        if (g_SF.VerifyAdventureLoaded() < 0)
            return
        g_SF.CurrentAdventure := g_SF.Memory.ReadCurrentObjID()
        g_ServerCall.UpdatePlayServer()
        g_SF.ResetServerCall()
        g_SF.PatronID := g_SF.Memory.ReadPatronID()
        g_SF.GameStartFormation := "Q"
        g_SaveHelper.Init() ; slow call, loads briv dictionary (3+s)
        if (this.PreFlightCheck() == -1) ; Did not pass pre flight check.
            return -1
        g_PreviousZoneStartTime := A_TickCount
		FormatTime, formattedDateTime,, yyyyMMddTHHmmss ;Can't include : in a filename so using the less human friendly version here
		LogBase:=A_LineFile . "\..\RunLog_" . formattedDateTime ;A separate variable so other logs can use a matching start time
		offRamp:=false ;Irisiri - trying to stop the script failing to stop a new run on time by limiting the code that runs at the end of a run
		this.Logger:=new IC_BrivMaster_Logger_Class(LogBase . ".csv")
		this.inputManager:=new IC_BrivMaster_InputManager_Class()
		this.levelManager:=new IC_BrivMaster_LevelManager_Class(g_BrivUserSettingsFromAddons["IBM_Route_Combine"])
		this.routeMaster:=new IC_BrivMaster_RouteMaster_Class(g_BrivUserSettingsFromAddons["IBM_Route_Combine"],LogBase)
		this.routeMaster.LoadRoute() ;Once per script run load of route
		this.EllywickCasino:=new IC_BrivMaster_EllywickDealer_Class()
		this.DialogSwatter_Setup() ;This needs to be built in a more organised way, but will do for now
		g_SharedData.IBM_SmartChests_Time:=0
		Loop
        {
			currentZone := g_SF.Memory.ReadCurrentZone()
			if (currentZone == "")
			{
				g_SF.SafetyCheck()
			}
			if (!this.TriggerStart AND offRamp AND currentZone <= this.routeMaster.thelloraTarget) ;Additional reset detection
			{
				this.TriggerStart:=true
				this.Logger.AddMessage("Missed Reset: Offramp set and z[" . currentZone . "] is at or before Thellora target z[" . this.routeMaster.thelloraTarget . "]")
			}
			if (this.TriggerStart OR g_SF.Memory.ReadResetsCount() > lastResetCount) ; first loop or Modron has reset
            {
				g_SharedData.IBM_SmartChests_Time:=0
				currentZone:=this.IBM_WaitForZoneLoad(currentZone)
				this.routeMaster.ToggleAutoProgress(this.routeMaster.combining ? 1 : 0) ;Set initial autoprogess ASAP. routeMaster.combining can't change run-to-run as loaded at script start
                if (g_SharedData.BossesHitThisRun)
					this.Logger.AddMessage("Bosses:" . g_SharedData.BossesHitThisRun) ;Boss hits from previous run
				g_SharedData.BossesHitThisRun := 0
				this.Logger.NewRun()
				offRamp:=false
				needToStack:=true ;Irisiri - added initialisation to make sure the offramp doesn't trigger if we've never checked
                this.levelManager.Reset()
                this.routeMaster.Reset()
				this.EllywickCasino.Reset()
				this.IBM_FirstZone(currentZone)
                lastResetCount := g_SF.Memory.ReadResetsCount()
				if (!this.routeMaster.ExpectingGameRestart()) ;Don't do standard online chests during offline runs as there will be an early save when closing the game
					g_SharedData.IBM_SmartChests_Time:=2500
                g_PreviousZoneStartTime := A_TickCount
				this.TriggerStart:=false
                g_SharedData.LoopString := "Main Loop"
                previousZone:=currentZone ;Update these as we may have progressed during first-zone logic
				currentZone:=g_SF.Memory.ReadCurrentZone()
            }
			g_SharedData.LoopString := offRamp ? "Off Ramp" : "Main Loop"
			if (g_SF.Memory.ReadResetting())
			{
				this.Logger.ResetReached()
				this.ModronResetCheck()
			}
			else if (currentZone <= this.routeMaster.targetZone) ;If we've passed the reset but the modron has yet to trigger we don't want to spam the game with inputs
			{
				if (!Mod( g_SF.Memory.ReadCurrentZone(), 5 ) AND Mod( g_SF.Memory.ReadHighestZone(), 5 ) AND !g_SF.Memory.ReadTransitioning())
					this.routeMaster.ToggleAutoProgress( 1, true ) ; Toggle autoprogress to skip boss bag
				if (this.routeMaster.TestForSteelBonesStackFarming()) ;Returns true on failure case (out of stacks and retarting due to having enough for another run)
				{
					this.TriggerStart:=true
					Continue ;Go straight back to the start of the loop
				}
				this.routeMaster.SetFormation(true)
				this.RouteMaster.TestForBlankOffline(currentZone)
				if (!offRamp) ;Only do the below until near the end
				{
					needToStack := this.routeMaster.NeedToStack()
					; Check for failed stack conversion
					if (g_SF.Memory.ReadHasteStacks() < 50 AND needToStack) ;TODO: Settings for this
						this.levelManager.SetupFailedConversion() ;TODO: This gets nuked by the next LevelManager.Reset() in most cases; we need to avoid doing it when TestForSteelBonesStackFarming() is going to ForceReset us
					if (currentZone>1)
						this.levelManager.LevelFormation("Q", "min", 0) ;TODO: Should this call on Q? We might be on E and it's technically possible E has champs Q doesn't (although that would be odd). Probably need a union of Q and E
				}
				if(currentZone > previousZone) ;Things to be done every new zone
				{
					this.Logger.UpdateZone(currentZone)
					previousZone:=CurrentZone
					this.RouteMaster.InitZone()
					if ((!Mod( g_SF.Memory.ReadCurrentZone(), 5 )) AND (!Mod( g_SF.Memory.ReadHighestZone(), 5)))
					{
						g_SharedData.TotalBossesHit++
						g_SharedData.BossesHitThisRun++
					}
					if (!offRamp) ;Only until we're nearly at the end of the run
					{
						;Check for offRamp
						if (!needToStack and (currentZone >= this.routeMaster.GetOffRampZone())) ;Eg 50 zones for 9J
						{
							If (this.routeMaster.EnoughHasteForCurrentRun())
							{
								offRamp:=True
								this.EllywickCasino.Stop() ;Stop the Ellywick checker, to avoid it running as the next run starts
								g_SharedData.IBM_SmartChests_Time:=0 ;Cancel any pending chest order at this point
							}
						}
					}
				}
				this.routeMaster.StartAutoProgressSoft()
			}
			else
			{
				this.Logger.ResetReached()
				g_SharedData.LoopString := "Pending modron reset"
			}
            if (g_SF.CheckifStuck())
            {
                this.TriggerStart := true
            }
			Sleep IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
		}
    }

	IBM_WaitForZoneLoad(existingZone) ;Waits for a valid zone. Used because force restarts seem to go into the main loop before the game has loaded z1
	{
		if (existingZone!="")
			return existingZone
		currentZone:=existingZone
		startTime:=A_TickCount
		ElapsedTime:=0
		while (currentZone=="" and ElapsedTime < 2000) ;Was 1s - possibly not enough for potatotablet
		{
			currentZone:=g_SF.Memory.ReadCurrentZone()
			sleep IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
			ElapsedTime:=A_TickCount-startTime
		}
		return currentZone
	}

	IBM_FirstZone(currentZone)
	{
		if (currentZone==1)
		{
			thelloraPresent:=this.levelManager.IsChampInFormation(139, "M") ;Maybe these need to be a table
			melfPresent:=this.levelManager.IsChampInFormation(59, "M")
			tatyanaPresent:=this.levelManager.IsChampInFormation(97, "M")
			BBEGPresent:=this.levelManager.IsChampInFormation(125, "M")
			melfSpawningMore:=melfPresent AND this.routeMaster.MelfManager.IsMelfEffectSpawnMore()
			
			PH_OPTION_DIANA_CHEESE:=true 
			PH_OPTION_DIANA_RESET_HOUR:=20 ;Local reset hour, 0 for midnight
			PH_OPTION_DIANA_CYCLE_SET_ESTIMATE:=30 ;How many minutes we expect between restarts, must be <60
			if (PH_OPTION_DIANA_CHEESE) ;Diana can give excess chests after the daily reset, as it seems things don't get synced up until a restart. Level her to 200 only in that window
			{
				prevHour:=PH_OPTION_DIANA_RESET_HOUR==0 ? 23 : PH_OPTION_DIANA_RESET_HOUR - 1
				if ( (A_Hour==PH_OPTION_DIANA_RESET_HOUR AND A_Min<=PH_OPTION_DIANA_CYCLE_SET_ESTIMATE) OR (A_Hour==prevHour AND A_Min>=58) ) ;Either up to the specified number of minutes after reset, or 2min before
					this.levelManager.OverrideLevelByIDRaiseToMin(148,"min",200)
			}	

			if (this.routeMaster.combining)
			{
				this.routeMaster.CheckThelloraBossRecovery() ;Try to avoid Combining into bosses after a failed run by breaking the combine
				melfSpawningMoreAfterRush:=melfPresent AND this.routeMaster.MelfManager.IsMelfEffectSpawnMore(this.routeMaster.thelloraTarget) ;TODO: This will not give the right zone if Thellora cant reach her max target, might need to consider current?
				if (!melfSpawningMore)
				{
					this.levelManager.OverrideLevelByID(59,"z1c", true) ;Do not level melf until after zone completion if not spawning more, to avoid the multiple-credit buff ruining the combine
				}
				if (g_BrivUserSettingsFromAddons["IBM_Level_Options_Limit_Tatyana"])
				{
					if (!melfSpawningMoreAfterRush and tatyanaPresent) ;If Melf won't be spawning more in the waitroom level Tatyana if present
					{
						this.levelManager.OverrideLevelByIDRaiseToMin(97,"z1",100)
					}
				}
				if (BBEGPresent)
				{
					if (melfSpawningMore) ;It doesn't matter if BBEG is spawning zombies post-rush as there is no need to preserve targets for Thellora, so we don't have to consider that here. Without we don't want waves being insta-killed at bad times
						this.levelManager.OverrideLevelByIDRaiseToMin(125,"z1",200)
					else
						this.levelManager.OverrideLevelByIDLowerToMax(125,"z1",100)
				}

				frontColumn:=this.levelManager.GetFrontColumnNoBriv() ;This assumes Briv is appropriately prioritised already - which he should be
				for _, v in frontColumn
				{
					if (g_BrivUserSettingsFromAddons["IBM_Level_Options_Suppress_Front"]) ;Avoid levelling any front-row champion but Briv - in which case don't prioritise
					{
						this.levelManager.OverrideLevelByIDLowerToMax(v,"z1",0)
						this.levelManager.OverrideLevelByIDLowerToMax(v,"min",0)
					}
					else
					{
						this.levelManager.RaisePriorityForFrontRow(v)
					}
				}
				g_SharedData.LoopString := "Start Zone Levelling"
				;OutputDebug % A_TickCount . ":Start Zone Levelling`n"
				this.levelManager.LevelFormation("M", "z1",,true,[28],true) ;Level until priority champions hit target only
				;OutputDebug % A_TickCount . ":Done Start Zone Levelling - raising BBEG level if needed`n"
				if (BBEGPresent AND (melfSpawningMoreAfterRush OR tatyanaPresent))
					this.levelManager.OverrideLevelByIDRaiseToMin(125,"min",200) ;No 'else' as already set on z1 TODO: No it hasn't for the "min" setting. Update: But he will still be levelled to some degree
				;OutputDebug % A_TickCount . ":Pre-RushWait`n"
				if (thelloraPresent)
					g_SF.DoRushWait(true)
				;OutputDebug % A_TickCount . ":Post-RushWait - Force stop progress`n"
				this.routeMaster.ToggleAutoProgress(0, false, true) ;We may or may not have been stopped by DoRushWait()
				;OutputDebug % A_TickCount . ":Progress stopped - Starting Casino`n"
				this.EllywickCasino.Start(melfSpawningMoreAfterRush) ;Start the Elly handler before rushwaiting, using the post-rush Melf status
				g_SharedData.LoopString := "Standard Levelling: M"
				;OutputDebug % A_TickCount . ":Casino Started - Standard Levelling: M`n"
				this.levelManager.LevelFormation("M","min") ;Level M to minimum
				;OutputDebug % A_TickCount . ":Done Standard Levelling - Updating Thellora`n"
				this.routeMaster.UpdateThellora()
				;OutputDebug % A_TickCount . ":Updated Thellora - Calling Casino`n"
				g_SharedData.LoopString := "Elly Wait: Post-rush Casino"
				this.IBM_EllywickCasino(frontColumn,"min",g_BrivUserSettingsFromAddons["IBM_Level_Options_Ghost"])

				if (!this.routeMaster.IsFeatSwap()) ;If featswapping Briv will jump with whatever value he had at zone completion, so checking here isn't useful, for non-feat swap, check if Briv is correctly placed so we do/don't jump out of the waitroom
				{
					brivShouldBeinEConfig:=this.routeMaster.ShouldWalk(g_SF.Memory.ReadCurrentZone())
					swapAttempts:=0
					Loop
					{
						this.routeMaster.SetFormation() ;Move to standard formation after waiting for the Casino if necessary
						swapAttempts++
					} until (brivShouldBeinEConfig == !g_SF.IsChampInFormation(58, g_SF.Memory.GetCurrentFormation()) OR swapAttempts > 10)
				}
				this.routeMaster.StartAutoProgressSoft() ;Start moving ASAP
				if (this.routeMaster.IsFeatSwap()) ;Swap formation here as we can't be blocked in the transition
					this.routeMaster.SetFormationHighZone() ;Special version for use here on the immediate exit
				this.levelManager.LevelFormation("Q","min",500) ;Apply min so BBEG->Dyna swap, Tatyana->Hew swap etc happens. Trying 500ms to allow for Hew x10 levelling to happen
			}
			else
			{
				this.levelManager.OverrideLevelByID(58,"z1c", true) ;Prevent z1 Briv levelling until zone complete to force separate jumps, and avoid wierd jumping-with-metalborn-but-using-4%-of-stacks issues
				;Melf-dependant BBEG levelling, so we can kill the hordes with spawn more, without stealing all the kills from Thellora for the other buffs
				;TODO: Update to check BBEGPresent
				if (melfSpawningMore)
					this.levelManager.OverrideLevelByIDRaiseToMin(125,"z1",200)
				else if (tatyanaPresent AND g_BrivUserSettingsFromAddons["IBM_Level_Options_Limit_Tatyana"]) ;If Melf won't be spawning more in the waitroom level Tatyana if present
				{
					this.levelManager.OverrideLevelByIDRaiseToMin(97,"z1",100)
				}
				else if (!tatyanaPresent)
				{
					BBEGInQ:=this.levelManager.IsChampInFormation(125, "Q")
					this.levelManager.OverrideLevelByIDLowerToMax(125,"z1",BBEGInQ ? 100 : 0)
				}
				;83 is Elly, 58 is Briv, 59 is Melf only levels the prio champs to max so that the waitroom can move on
				;Only put Melf in early with his spawn more effect because of the spawn speed bug with teleporting enemies, and keep  Widdle (91) or Deekin(28) out at this stage due to their spawn speed effects as well - they'll be levelled by the first tick in the waitroom
				;Update: Removed Widdle for now as her spawn-faster is at level 260, and so shouldn't block other champs being placed as long as she isn't set as a priority

				;this.levelManager.LevelClickDamage() ;Do one tick of click damage levelling to make sure we oneshot things in z1. Calls in wait for gold mode: TODO: No such mode exists?!


				frontColumn:=this.levelManager.GetFrontColumnNoBriv() ;This assumes Briv is appropriately prioritised already - which he should be
				for _, v in frontColumn
				{
					if (g_BrivUserSettingsFromAddons["IBM_Level_Options_Suppress_Front"]) ;Avoid levelling any front-row champion but Briv - in which case don't prioritise
					{
						this.levelManager.OverrideLevelByIDLowerToMax(v,"z1",0)
						this.levelManager.OverrideLevelByIDLowerToMax(v,"min",0)
					}
					else
					{
						this.levelManager.RaisePriorityForFrontRow(v)
					}
				}
				this.levelManager.LevelFormation("M", "z1",, true, melfSpawningMore ? [28]:[28, 59], true)
				if (melfSpawningMore)
				{
					g_SharedData.LoopString := "Elly Wait: Casino with Melf spawning more"
					this.EllywickCasino.Start(melfSpawningMore) ;Start the Elly handler
					this.IBM_EllywickCasino(frontColumn,"z1") ;TODO: Think about ghost levelling in this case
				}
				else
				{
					g_SharedData.LoopString := "Elly Wait: Express Casino"
					this.EllywickCasino.Start() ;Start the Elly handler
					this.IBM_EllywickCasino(frontColumn,"z1") ;TODO: Think about ghost levelling in this case
				}
				;Wait for zone completion so we can level Briv - this should perhaps have a timeout in case things get weird (no familiars in modron formation? Which would mean no gold anyway)
				quest := g_SF.Memory.ReadQuestRemaining()
				while (quest > 0)
				{
					this.levelManager.LevelWorklist() ;Level existing M worklist whilst waiting
					Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
					quest := g_SF.Memory.ReadQuestRemaining()
				}
				this.levelManager.LevelWorklist(,true) ;Force briv to z1 level (due to z1c he won't have been levelled by the earlier calls)
				;TODO: This will stall without Thellora, or if formation is zerged. Need a cap, and need to actually compare Q/E to what we have
				;It seems this fails due to the ranged fairies Minsc spawns attacking the formation
				swapAttempts:=0
				Loop
				{
					this.routeMaster.SetFormation() ;Move to z1 formation after waiting for the Casino if necessary
					swapAttempts++
				} until (g_SF.IsChampInFormation(139, g_SF.Memory.GetCurrentFormation())) OR (swapAttempts > 10) ;139 is Thellora
				;if (swapAttempts > 1)
					;OutputDebug % "IBM_FirstZone: Done loading z1 Formation. Required attempts: " . swapAttempts . "`n"
				;Sleep, IC_RNGWaitingRoom_Class.IRI_LOOP_WAIT_FAST ;sleep to allow the change to actually apply - Do we need to verify this?
				;TODO: Is using Min here appropriate?
				this.levelManager.LevelFormation("Q","min",0) ;One tap of levelling after the change so that BBEG->Dyna swap or such happens
				if (thelloraPresent)
				{
					g_SF.DoRushWait()
					this.routeMaster.UpdateThellora()
				}

			}
		}
		else ;Not z1. Level click damage to make sure we can move - otherwise we can be stuck since it's normally called in InitZone()
		{
			this.levelManager.LevelClickDamage()
		}
	}

	IBM_EllywickCasino(lockedFrontColumnChamps,formationToLevelPostUnlock,allowGhostLevelling:=false) ;lockedFrontColumnChamps is a list of champions who have had levelling suppressed, who will be levelled once conditions in the Casino or met (or if we bypass due to no Elly)
    {
        if (this.EllywickCasino.IsEllyWickOnTheField())
        {
			frontColumnLevellingAllowed:=lockedFrontColumnChamps.Count()>0 ? false : true ;If there are no locked champions there's no need to check for unlocking them
			ghostLevellingAllowed:=!allowGhostLevelling
			timeout := 60000 ;Casino takes ~5s max at x10, so this is reasonable but might be worth scaling with game speed
            ElapsedTime := 0
            StartTime := A_TickCount
			while (!this.EllywickCasino.Complete AND ElapsedTime < timeout )
            {
				this.levelManager.LevelWorklist()
				this.levelManager.LevelClickDamage()
				if (!frontColumnLevellingAllowed) ;Check if we can allow this, the aim is to level whilst the formation is engauged so the champion is NOT placed, saving time without interfering with Briv
				{
					if (this.IBM_EllywickCasino_UnderAttackCheck())
					{
						this.IBM_EllywickCasino_UnlockChamps(lockedFrontColumnChamps,formationToLevelPostUnlock)
						frontColumnLevellingAllowed:=True
					}
				}
				if (!ghostLevellingAllowed AND (frontColumnLevellingAllowed OR g_SF.Memory.IBM_IsCurrentFormationFull())) ;Either front row levelling is allowed (we've dealt with that champ, or doesn't care about the front row), or the formation is full so we can level away
				{
					this.levelManager.LevelFormation("A",formationToLevelPostUnlock)
					ghostLevellingAllowed:=true
				}
				Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
				ElapsedTime := A_TickCount - StartTime
            }
			if (!frontColumnLevellingAllowed) ;If not released in the loop, reset levels but don't level as we need to get on with progression
				this.IBM_EllywickCasino_UnlockChamps(lockedFrontColumnChamps)
			this.Logger.AddMessage("Casino{z" . g_SF.Memory.ReadCurrentZone() . " T=" . ElapsedTime . " R=" . this.EllywickCasino.Redraws . " M=" . this.RouteMaster.MelfManager.GetCurrentMelfEffect() .  " SB=" . g_SF.Memory.ReadSBStacks() . (this.EllywickCasino.StatusString ? " " . this.EllywickCasino.StatusString : "") . "}")
		}
		else
		{
			this.IBM_EllywickCasino_UnlockChamps(lockedFrontColumnChamps,formationToLevelPostUnlock)
			this.Logger.AddMessage("No Elly{z" . g_SF.Memory.ReadCurrentZone() . "}")
		}
		;if (g_SF.Memory.ReadCurrentZone()>321)
		;	Send !{f10}
    }

	IBM_EllywickCasino_UnderAttackCheck()
	{
		melee:=g_SF.Memory.ReadNumAttackingMonstersReached()
		return (melee>1) OR (melee + g_SF.Memory.ReadNumRangedAttackingMonsters() > 5) ;TODO: The numbers needs to be a setting
	}

	IBM_EllywickCasino_UnlockChamps(lockedFrontColumnChamps,formationToLevelPostUnlock:="") ;Separated as this must be called either during the Casino, or if Elly is MIA
	{
		if (lockedFrontColumnChamps.Count()>0)
		{
			for _,v in lockedFrontColumnChamps
			{
				this.levelManager.ResetLevelByID(v)
			}
			if (formationToLevelPostUnlock)
				this.levelManager.LevelFormation("M",formationToLevelPostUnlock) ;Re-create job. This could do without being a duplicate of the call in FirstZone (things will go weird when we change one and forget to change the other)
		}
	}

	;Overidden to allow for feat swap (Briv can be in E)
	; Tests to make sure Gem Farm is properly set up before attempting to run.
    PreFlightCheck()
    {
        memoryVersion := g_SF.Memory.GameManager.GetVersion()
        ; Test Favorite Exists
        txtCheck := "`n`nOther potential solutions:"
        txtCheck .= "`n`n1. Be sure Imports are up to date. Current imports are for: v" . g_SF.Memory.GetImportsVersion()
        txtCheck .= "`n`n2. Check the correct memory file is being used. Current version: " . memoryVersion
        txtcheck .= "`n`n3. If IC is running with admin privileges, then the script will also require admin privileges."
        if (_MemoryManager.is64bit)
            txtcheck .= "`n4. Check AHK is 64-bit. (Currently " . (A_PtrSize = 4 ? 32 : 64) . "-bit)"

        champion := 58   ; briv
        formationQ := g_SF.FindChampIDinSavedFavorite( champion, favorite := 1, includeChampion := True )
        if (formationQ == -1 AND this.RunChampionInFormationTests(champion, favorite := 1, includeChampion := True, txtCheck) == -1)
            return -1

        formationW := g_SF.FindChampIDinSavedFavorite( champion, favorite := 2, includeChampion := True  )
        if (formationW == -1 AND this.RunChampionInFormationTests(champion, favorite := 2, includeChampion := True, txtCheck) == -1)
            return -1

		featSwapping:=g_BrivUserSettingsFromAddons["IBM_Route_BrivJump_E"]!=0 ;Can't check via routeMaster as that won't have been instantiated yet
        formationE := g_SF.FindChampIDinSavedFavorite( champion, favorite := 3, includeChampion := featSwapping  )
        if (formationE == -1 AND this.RunChampionInFormationTests(champion, favorite := 3, includeChampion := featSwapping, txtCheck) == -1)
            return -1

        if ((ErrorMsg := g_SF.FormationFamiliarCheckByFavorite(favorite := 1, True)))
            MsgBox, %ErrorMsg%
        while (ErrorMsg := g_SF.FormationFamiliarCheckByFavorite(favorite := 2, False))
        {
            MsgBox, 5,, %ErrorMsg%
            IfMsgBox, Retry
            {
                g_SF.OpenProcessReader()
                ErrorMsg := g_SF.FormationFamiliarCheckByFavorite(favorite := 2, False)
            }
            IfMsgBox, Cancel
            {
                MsgBox, Canceling Run
                return -1
            }
        }
        if (ErrorMsg := g_SF.FormationFamiliarCheckByFavorite(favorite := 3, True))
            MsgBox, %ErrorMsg%

        return 0
    }

	;Overidden to set TriggerStart for new run check
	;Waits for modron to reset. Closes IC if it fails.
    ModronResetCheck()
    {
        modronResetTimeout := 75000
        if (!g_SF.WaitForModronReset(modronResetTimeout))
            g_SF.CheckifStuck(True)
            ;g_SF.CloseIC( "ModronReset, resetting exceeded " . Floor(modronResetTimeout/1000) . "s" )
        g_PreviousZoneStartTime := A_TickCount
		this.TriggerStart := true
    }

	;DIALOGSWATTER BLOCK
	DialogSwatter_Setup()
    {
        this.SwatterTimer :=  ObjBindMethod(this, "DialogSwatter_Swat")
		this.KEY_ESC:=this.inputManager.getKey("Esc")
    }

    DialogSwatter_Start()
    {
		timerFunction:=this.SwatterTimer
		SetTimer, %timerFunction%, 100, 0
		this.SwatterStartTime:=A_TickCount
    }

    DialogSwatter_Stop()
    {
        timerFunction:=this.SwatterTimer
		SetTimer, %timerFunction%, Off
    }

    DialogSwatter_Swat()
    {
        if (g_SF.Memory.ReadWelcomeBackActive())
		{
            ;g_SF.Hwnd := WinExist("ahk_exe " . g_UserSettings[ "ExeName" ]) ;Is this necessary here? It shouldn't be, OpenIC()->SetLastActiveWindowWhileWaingForGameExe should set it as it opens
            this.KEY_ESC.KeyPress()
        }
		else if (A_TickCount > this.SwatterStartTime + 3000) ;3s should be enough to get the swat done
			this.DialogSwatter_Stop() ;Stop the timer since we don't have anything to swat
    }

	;END DIALOGSWATTER BLOCK

}
