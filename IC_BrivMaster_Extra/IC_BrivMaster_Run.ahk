#Requires AutoHotkey 1.1.37+ <1.2
#SingleInstance Force
;Based on BrivGemFarm Performance by MikeBaldi and Antilectual, and on various addons created by ImpEGamer. Refer to the ReadMe.

;=======================
;Script Optimization
;=======================
#HotkeyInterval 1000  ; The default value is 2000 (milliseconds).
#MaxHotkeysPerInterval 70 ; The default value is 70
#NoEnv ; Avoids checking empty variables to see if they are environment variables (recommended for all new scripts). Default behavior for AutoHotkey v2.
SetWorkingDir %A_ScriptDir%
SetWinDelay, 33 ; Sets the delay that will occur after each windowing command, such as WinActivate. (Default is 100)
SetControlDelay, 0 ; Sets the delay that will occur after each control-modifying command. -1 for no delay, 0 for smallest possible delay. The default delay is 20.
SetBatchLines, -1 ; How fast a script will run (affects CPU utilization).(Default setting is 10ms - prevent the script from using any more than 50% of an idle CPU's time.
                  ; This allows scripts to run quickly while still maintaining a high level of cooperation with CPU sensitive tasks such as games and video capture/playback.
ListLines Off
Process, Priority,, High
CoordMode, Mouse, Client

#include %A_LineFile%\..\IC_BrivMaster_SharedFunctions.ahk ;Indirectly #includes IC_BrivMaster_Memory.ahk
#include %A_LineFile%\..\IC_BrivMaster_Functions.ahk
#include %A_LineFile%\..\IC_BrivMaster_Overrides.ahk
#include %A_LineFile%\..\IC_BrivMaster_GameMaster.ahk
#include %A_LineFile%\..\IC_BrivMaster_RouteMaster.ahk
#include %A_LineFile%\..\IC_BrivMaster_LevelManager.ahk
#include %A_LineFile%\..\IC_BrivMaster_Heroes.ahk
#include %A_LineFile%\..\Lib\IC_BrivMaster_JSON.ahk
#include %A_LineFile%\..\Lib\IC_BrivMaster_Zlib.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\SH_GUIFunctions.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\SH_UpdateClass.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\ObjRegisterActive.ahk ;TODO: This was the very last line in IC_BrivGemFarm_Functions.ahk, why?

global g_SF:=New IC_BrivMaster_SharedFunctions_Class ; includes IBM MemoryFunctions in g_SF.Memory
global g_IBM_Settings:={}
global g_IBM:=New IC_BrivMaster_GemFarm_Class
global g_zlib:=New IC_BrivMaster_Budget_Zlib_Class() ;Created global as it has a lot of one-time setup and we want to avoid re-creating it
global g_ServerCall ;This is instantiated by g_SF.ResetServerCall()
global g_IBM_Settings_Addons:={}
global g_Heroes:={} ;Has to be instantiated after memory reads are available
global g_InputManager:=New IC_BrivMaster_InputManager_Class()
global g_SharedData:=New IC_BrivMaster_SharedData_Class

#include *i %A_LineFile%\..\IC_BrivMaster_Mods.ahk

SH_UpdateClass.AddClassFunctions(GameObjectStructure, IC_BrivMaster_GameObjectStructure_Add)
SH_UpdateClass.UpdateClassFunctions(_MemoryManager, IBM_Memory_Manager)

g_SharedData.Init() ;Loads settings so must be prior to the icon set and Window:Show in CreateWindow()
g_IBM.CreateWindow()

if(A_Args[1])
{
    ObjRegisterActive(g_SharedData, A_Args[1])
    g_SF.WriteObjectToAHKJSON(A_LineFile . "\..\LastGUID_IBM_GemFarm.json", A_Args[1])
}
else
{
    GuidCreate := ComObjCreate("Scriptlet.TypeLib")
    guid := GuidCreate.Guid ;TODO: Would it be useful to store this somewhere?
    ObjRegisterActive(g_SharedData, guid)
    g_SF.WriteObjectToAHKJSON(A_LineFile . "\..\LastGUID_IBM_GemFarm.json", guid)
}

g_IBM.GemFarm()

OnExit(ComObjectRevoke())

ComObjectRevoke()
{
    ObjRegisterActive(g_SharedData, "")
    ExitApp
}

IBM_GemFarmGuiClose()
{
    MsgBox, 35, Close, Really close the gem farm script? `n`nWarning: This script is required for gem farming. `n"Yes" will close the gem farm script. `n"No" will miniize the script to the tray.`nYou can open it again by pressing the play button in Script Hub.
    IfMsgBox, Yes
        ExitApp
    IfMsgBox, No
        Gui, BrivPerformanceGemFarm:hide
    IfMsgBox, Cancel
        return true
}

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class IC_BrivMaster_GemFarm_Class
{
	GemFarm()
    {
        static lastResetCount:=0
        this.TriggerStart:=true
        DllCall("QueryPerformanceFrequency", "Int64*", PerformanceCounterFrequency) ;Get the performance counter frequency once TODO: I think the frequency can be changed, so this might not be safe?
		this.CounterFrequency:=PerformanceCounterFrequency//1000 ;Convert from seconds to milliseconds as that is our main interest
		this.GameMaster:=New IC_BrivMaster_GameMaster_Class()
		this.RefreshImportCheck() ;Does the initial population of the import check
        g_ServerCall.UpdatePlayServer() ;TODO: Does doing this before ResetServerCall() make any sense? It won't have an instance yet?
        g_SF.ResetServerCall()
        g_SF.PatronID:=g_SF.Memory.ReadPatronID() ;TODO: Move to GameMaster
        g_Heroes:=New IC_BrivMaster_Heroes_Class() ;Global to allow consitency between uses in main script and hub (e.g. Ellywick for gold farming). We have to wait with initalising it until memory reads are available, however TODO: More reason for bringing some order to initial startup
		this.Logger:=New IC_BrivMaster_Logger_Class(A_LineFile . "\..\Logs\")
		this.LevelManager:=New IC_BrivMaster_LevelManager_Class() ;Must be before the PreFlightCheck() call as we use the formation data the LevelManager loads
		this.RouteMaster:=New IC_BrivMaster_RouteMaster_Class(g_IBM_Settings["IBM_Route_Combine"],this.Logger.logBase)
		if (!this.PreFlightCheck()) ; Did not pass pre flight check.
            return false
		this.offRamp:=false ;Limit the code that runs at the end of a run
		this.EllywickCasino:=New IC_BrivMaster_EllywickDealer_Class()
		this.DialogSwatter:=New IC_BrivMaster_DialogSwatter_Class()
		if (g_IBM_Settings["IBM_Level_Diana_Cheese"]) ;Diana Electrum Chest Cheese things
			this.DianaCheeseHelper:=New IC_BrivMaster_DianaCheese_Class
		g_SharedData.UpdateOutbound("IBM_BuyChests",false)
		this.PreviousZoneStartTime:=A_TickCount ;TODO: These 3 variables are for CheckifStuck, could maybe using encapsulating somewhere else (simple object for it?)
		this.CheckifStuck_lastCheck:=0 
        this.CheckifStuck_fallBackTries:=0	
		Loop
        {
			this.currentZone:=g_SF.Memory.ReadCurrentZone() ;Class level variable so it can be reset during rollbacks TODO: Move to routeMaster
			if (this.currentZone=="")
				g_IBM.GameMaster.SafetyCheck()
			if (!this.TriggerStart AND this.offRamp AND this.currentZone <= this.routeMaster.thelloraTarget) ;Additional reset detection
			{
				this.TriggerStart:=true
				this.Logger.AddMessage("Missed Reset: Offramp set and z[" . this.currentZone . "] is at or before Thellora target z[" . this.routeMaster.thelloraTarget . "]")
			}
			if (this.TriggerStart OR g_SF.Memory.ReadResetsCount() > lastResetCount) ; first loop or Modron has reset
            {
				g_SharedData.UpdateOutbound("IBM_BuyChests",false)
				if (g_SharedData.BossesHitThisRun)
				{
					this.Logger.AddMessage("Bosses:" . g_SharedData.BossesHitThisRun) ;Boss hits from previous run
					g_SharedData.UpdateOutbound("BossesHitThisRun",0)
				}
				this.Logger.NewRun()
				this.currentZone:=this.IBM_WaitForZoneLoad(this.currentZone)
				this.routeMaster.ToggleAutoProgress(this.routeMaster.combining ? 1 : 0) ;Set initial autoprogess ASAP. routeMaster.combining can't change run-to-run as loaded at script start
				this.offRamp:=false ;TODO: There's a lot of resetting that could probably be wrapped together. Or possibly this whole block carved out
				this.failedConversionMode:=false
				needToStack:=true ;Irisiri - added initialisation to make sure the offramp doesn't trigger if we've never checked
                this.levelManager.Reset()
                this.routeMaster.Reset()
				this.EllywickCasino.Reset()
				this.IBM_FirstZone(this.currentZone)
                lastResetCount:=g_SF.Memory.ReadResetsCount()
				if (!this.routeMaster.ExpectingGameRestart() OR this.routeMaster.cycleMax==1) ;When running hybrid don't do standard online chests during offline runs as there will be an early save when closing the game. Without hybrid we don't have a choice
					g_SharedData.UpdateOutbound("IBM_BuyChests",true)
                this.PreviousZoneStartTime:=A_TickCount
				this.TriggerStart:=false
				DllCall("QueryPerformanceCounter", "Int64*", lastLoopEndTime) ;Set for the first loop
				g_SharedData.UpdateOutbound("LoopString","Main Loop")
                this.previousZone:=this.currentZone ;Update these as we may have progressed during first-zone logic. Previous zone is an object variable so it can be reset if a fallback is detected TODO: This should be in the RouteMaster
				this.currentZone:=g_SF.Memory.ReadCurrentZone()
            }
			g_SharedData.UpdateOutbound("LoopString",this.offRamp ? "Off Ramp" : "Main Loop")
			if (g_SF.Memory.ReadResetting())
			{
				this.Logger.ResetReached()
				this.ModronResetCheck()
			}
			else if (this.currentZone <= this.routeMaster.targetZone) ;If we've passed the reset but the modron has yet to trigger we don't want to spam the game with inputs
			{
				if (!Mod( g_SF.Memory.ReadCurrentZone(), 5 ) AND Mod( g_SF.Memory.ReadHighestZone(), 5 ) AND !g_SF.Memory.ReadTransitioning())
					this.routeMaster.ToggleAutoProgress( 1, true ) ; Toggle autoprogress to skip boss bag
				if (this.routeMaster.TestForSteelBonesStackFarming()) ;Returns true on failure case (out of stacks and restarted due to having enough for another run)
					Continue ;Go straight back to the start of the loop
				this.routeMaster.SetFormation(true)
				this.RouteMaster.TestForBlankOffline(this.currentZone)
				if (!this.offRamp) ;Only do the below until near the end
				{
					needToStack:=this.routeMaster.NeedToStack()
					; Check for failed stack conversion
					if (this.currentZone>1)
						this.levelManager.LevelFormation("Q", "min", 0) ;TODO: Should this call on Q? We might be on E and it's technically possible E has champs Q doesn't (although that would be odd). Probably need a union of Q and E
				}
				if(this.currentZone > this.previousZone) ;Things to be done every new zone
				{
					this.Logger.UpdateZone(this.currentZone)
					this.previousZone:=this.currentZone
					this.RouteMaster.InitZone()
					if ((!Mod( g_SF.Memory.ReadCurrentZone(), 5 )) AND (!Mod( g_SF.Memory.ReadHighestZone(), 5)))
					{
						g_SharedData.UpdateOutbound_Increment("TotalBossesHit")
						g_SharedData.UpdateOutbound_Increment("BossesHitThisRun")
						if (g_IBM_Settings["IBM_Level_Recovery_Softcap"] AND !this.offRamp AND !this.failedConversionMode AND needToStack AND g_Heroes[58].ReadHasteStacks() < 50) ;Only check for recovery levelling when we hit a boss. Checks offramp as needtostack won't be updated if true
						{
							this.failedConversionMode:=true
							this.levelManager.SetupFailedConversion()
						}
					}
					if (!this.offRamp) ;Only until we're nearly at the end of the run
					{
						;Check for offRamp
						if (!needToStack and (this.currentZone >= this.routeMaster.GetOffRampZone())) ;Eg 50 zones for 9J
						{
							If(this.routeMaster.EnoughHasteForCurrentRun())
							{
								this.offRamp:=True
								this.EllywickCasino.Stop() ;Stop the Casino, to avoid it running as the next run starts
								g_SharedData.UpdateOutbound("IBM_BuyChests",false) ;Cancel any pending chest order at this point
							}
						}
					}
				}
				else
					this.routeMaster.StartAutoProgressSoft() ;InitZone() will handle this for new zones (which makes it odd it is separate...) TODO: Checking this every single tick seems excessive?
			}
			else
			{
				this.Logger.ResetReached()
				g_SharedData.UpdateOutbound("LoopString","Pending modron reset")
			}
            this.CheckifStuck() ;Does not need to set TriggerStart as any exit that would require it will also call RestartAdventure() which sets it to true
			;Loop frequency check
			this.IBM_SleepOffset(lastLoopEndTime,30)
			DllCall("QueryPerformanceCounter", "Int64*", lastLoopEndTime)
		}
    }

	IBM_Sleep(sleepTime) ;A more accurate sleep. Relevant for any short sleep (<100ms?)
	{
		DllCall("QueryPerformanceCounter", "Int64*", currentTime)
		targetEndTime:=currentTime+this.CounterFrequency*sleepTime
		while (currentTime < targetEndTime)
		{
			targetTick:=(targetTime - currentTime)//this.CounterFrequency
			if (targetTick<=5) ;With <5ms to go make individual 1ms calls
				tick:=1
			else
				tick:=Min(15,targetTick) ;Make calls of no more than 15ms to ensure timers run etc
			DllCall("Sleep", "UInt", tick)
			DllCall("QueryPerformanceCounter", "Int64*", currentTime)
		}
	}

	IBM_SleepOffset(baseTime,offsetMilliseconds) ;baseTime is in performance counter ticks, acquired from DllCall("QueryPerformanceCounter", "Int64*", var). Use to sleep until a specific time has elapsed from a previous event (rather than the call, per IBM_Sleep)
	{
		targetTime:=baseTime+this.CounterFrequency*offsetMilliseconds
		DllCall("QueryPerformanceCounter", "Int64*", currentTime)
		while (currentTime < targetTime)
		{
			targetTick:=(targetTime - currentTime)//this.CounterFrequency
			if (targetTick <= 5) ;With <5ms to go make individual 1ms calls
				tick:=1
			else
				tick:=Min(15,targetTick) ;Make calls of no more than 15ms to ensure timers run etc
			DllCall("Sleep", "UInt", tick)
			DllCall("QueryPerformanceCounter", "Int64*", currentTime)
		}
	}

	IBM_WaitForZoneLoad(existingZone) ;Waits for a valid zone. Used because force restarts seem to go into the main loop before the game has loaded z1. Note that this doesn't mean that the zone is active (per g_SF.Memory.ReadAreaActive())
	{
		if (existingZone!="") ;TODO: Do we need to check for this being -1 here and in the loop? The zone also becomes 0 during resets
			return existingZone
		currentZone:=existingZone
		startTime:=A_TickCount
		ElapsedTime:=0
		while (currentZone=="" and ElapsedTime < 2000) ;Was 1s - possibly not enough for potatotablet
		{
			this.IBM_Sleep(15)
			currentZone:=g_SF.Memory.ReadCurrentZone()
			ElapsedTime:=A_TickCount-startTime
		}
		return currentZone
	}

	IBM_FirstZone(currentZone)
	{
		if (currentZone==1)
		{
			melfPresent:=g_Heroes[59].inM ;TODO: Copying these flags doesn't seem monsterously useful? It makes things a tiny bit easier to read I suppose...
			tatyanaPresent:=g_Heroes[97].inM
			BBEGPresent:=g_Heroes[125].inM
			melfSpawningMore:=melfPresent AND this.routeMaster.MelfManager.IsMelfEffectSpawnMore()
			if (g_IBM_Settings["IBM_Level_Diana_Cheese"] AND this.DianaCheeseHelper.InWindow()) ;Diana can give excess chests after the daily reset, as it seems things don't get synced up until a restart. Level her to 200 only in that window
				this.levelManager.OverrideLevelByIDRaiseToMin(148,"min",200)
			if (this.routeMaster.combining)
			{
				this.routeMaster.CheckThelloraBossRecovery() ;Try to avoid Combining into bosses after a failed run by breaking the combine
				melfSpawningMoreAfterRush:=melfPresent AND this.routeMaster.MelfManager.IsMelfEffectSpawnMore(this.routeMaster.thelloraTarget) ;TODO: This will not give the right zone if Thellora cant reach her max target, might need to consider current?
				if (!melfSpawningMore)
				{
					this.levelManager.OverrideLevelByID(59,"z1c", true) ;Do not level melf until after zone completion if not spawning more, to avoid the multiple-credit buff ruining the combine
				}
				if (g_IBM_Settings["IBM_Level_Options_Limit_Tatyana"])
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
					if (g_IBM_Settings["IBM_Level_Options_Suppress_Front"]) ;Avoid levelling any front-row champion but Briv - in which case don't prioritise
					{
						this.levelManager.OverrideLevelByIDLowerToMax(v,"z1",0)
						this.levelManager.OverrideLevelByIDLowerToMax(v,"min",0)
					}
					else
					{
						this.levelManager.RaisePriorityForFrontRow(v)
					}
				}
				g_SharedData.UpdateOutbound("LoopString","Start Zone Levelling")
				this.levelManager.LevelFormation("M", "z1",,true,[28],true) ;Level until priority champions hit target only
				if (BBEGPresent AND (melfSpawningMoreAfterRush OR tatyanaPresent))
					this.levelManager.OverrideLevelByIDRaiseToMin(125,"min",200) ;No 'else' as already set on z1 TODO: No it hasn't for the "min" setting. Update: But he will still be levelled to some degree
				if (g_Heroes[139].inM)
					g_SF.DoRushWait(true)
				this.routeMaster.ToggleAutoProgress(0, false, true) ;We may or may not have been stopped by DoRushWait()
				this.EllywickCasino.Start() ;Start the Elly handler before rushwaiting, using the post-rush Melf status
				g_SharedData.UpdateOutbound("LoopString","Standard Levelling: M")
				this.levelManager.LevelFormation("M","min") ;Level M to minimum
				this.routeMaster.UpdateThellora()
				g_SharedData.UpdateOutbound("LoopString","Ellywick's Casino")
				this.IBM_EllywickCasino(frontColumn,"min",g_IBM_Settings["IBM_Level_Options_Ghost"])
				if (!this.routeMaster.IsFeatSwap()) ;If featswapping Briv will jump with whatever value he had at zone completion, so checking here isn't useful, for non-feat swap, check if Briv is correctly placed so we do/don't jump out of the waitroom
				{
					brivShouldBeinEConfig:=this.routeMaster.ShouldWalk(g_SF.Memory.ReadCurrentZone())
					swapAttempts:=0
					Loop
					{
						this.routeMaster.SetFormation() ;Move to standard formation after waiting for the Casino if necessary
						swapAttempts++
					} until (brivShouldBeinEConfig==g_Heroes[58].ReadBenched() OR swapAttempts > 10)
				}
				this.routeMaster.StartAutoProgressSoft() ;Start moving ASAP
				if (this.routeMaster.IsFeatSwap()) ;Swap formation here as we can't be blocked in the transition
					this.routeMaster.SetFormationHighZone() ;Special version for use here on the immediate exit
				this.levelManager.LevelFormation("Q","min",500) ;Apply min so BBEG->Dyna swap, Tatyana->Hew swap etc happens. Trying 500ms to allow for Hew x10 levelling to happen
			}
			else ;Non-combining
			{
				this.levelManager.OverrideLevelByID(58,"z1c", true) ;Prevent z1 Briv levelling until zone complete to force separate jumps, and avoid wierd jumping-with-metalborn-but-using-4%-of-stacks issues
				;Melf-dependant BBEG levelling, so we can kill the hordes with spawn more, without stealing all the kills from Thellora for the other buffs
				;TODO: Update to check BBEGPresent
				if (melfSpawningMore)
					this.levelManager.OverrideLevelByIDRaiseToMin(125,"z1",200)
				else if (tatyanaPresent AND g_IBM_Settings["IBM_Level_Options_Limit_Tatyana"]) ;If Melf won't be spawning more in the waitroom level Tatyana if present
				{
					this.levelManager.OverrideLevelByIDRaiseToMin(97,"z1",100)
				}
				else if (!tatyanaPresent)
				{
					this.levelManager.OverrideLevelByIDLowerToMax(125,"z1",g_Heroes[125].inQ ? 100 : 0)
				}
				;83 is Elly, 58 is Briv, 59 is Melf only levels the prio champs to max so that the waitroom can move on
				;Only put Melf in early with his spawn more effect because of the spawn speed bug with teleporting enemies, and keep  Widdle (91) or Deekin(28) out at this stage due to their spawn speed effects as well - they'll be levelled by the first tick in the waitroom
				;Update: Removed Widdle for now as her spawn-faster is at level 260, and so shouldn't block other champs being placed as long as she isn't set as a priority
				frontColumn:=this.levelManager.GetFrontColumnNoBriv() ;This assumes Briv is appropriately prioritised already - which he should be
				for _, v in frontColumn
				{
					if (g_IBM_Settings["IBM_Level_Options_Suppress_Front"]) ;Avoid levelling any front-row champion but Briv - in which case don't prioritise
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
				g_SharedData.UpdateOutbound("LoopString","Ellywick's Casino")
				this.EllywickCasino.Start() ;Start the Elly handler
				this.IBM_EllywickCasino(frontColumn,"z1") ;TODO: Think about ghost levelling in this case
				quest:=g_SF.Memory.ReadQuestRemaining() ;Wait for zone completion so we can level Briv - TODO: this should perhaps have a timeout in case things get weird (no familiars in modron formation? Which would mean no gold anyway)
				while (quest > 0)
				{
					this.levelManager.LevelWorklist() ;Level existing M worklist whilst waiting
					this.IBM_Sleep(15)
					quest:=g_SF.Memory.ReadQuestRemaining()
				}
				this.levelManager.LevelWorklist(,true) ;Force briv to z1 level (due to z1c he won't have been levelled by the earlier calls)
				;TODO: This will stall without Thellora, or if formation is zerged. Need a cap, and need to actually compare Q/E to what we have
				;It seems this fails due to the ranged fairies Minsc spawns attacking the formation
				swapAttempts:=0
				Loop
				{
					this.routeMaster.SetFormation() ;Move to z1 formation after waiting for the Casino if necessary
					swapAttempts++
				} until (!g_Heroes[139].ReadBenched() OR (swapAttempts > 10)) ;139 is Thellora
				this.levelManager.LevelFormation("Q","min",0) ;One tap of levelling after the change so that BBEG->Dyna swap or such happens
				if (g_Heroes[139].inQ OR g_Heroes[139].inE)
				{
					g_SF.DoRushWait()
					this.routeMaster.UpdateThellora()
				}
			}
		}
		else ;Not z1
			this.routeMaster.InitZone() ;Includes levelling click damage to make sure we can move
	}

	IBM_EllywickCasino(lockedFrontColumnChamps,formationToLevelPostUnlock,allowGhostLevelling:=false) ;lockedFrontColumnChamps is a list of champions who have had levelling suppressed, who will be levelled once conditions in the Casino or met (or if we bypass due to no Elly)
    {
        if (!g_Heroes[83].ReadBenched())
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
				this.IBM_Sleep(15)
				ElapsedTime := A_TickCount - StartTime
            }
			if (!frontColumnLevellingAllowed) ;If not released in the loop, reset levels but don't level as we need to get on with progression
				this.IBM_EllywickCasino_UnlockChamps(lockedFrontColumnChamps)
			this.Logger.AddMessage("Casino{z" . g_SF.Memory.ReadCurrentZone() . " T=" . ElapsedTime . " R=" . this.EllywickCasino.Redraws . " M=" . this.RouteMaster.MelfManager.GetCurrentMelfEffect() .  " SB=" . g_Heroes[58].ReadSBStacks() . (this.EllywickCasino.StatusString ? " " . this.EllywickCasino.StatusString : "") . "}")
		}
		else
		{
			this.IBM_EllywickCasino_UnlockChamps(lockedFrontColumnChamps,formationToLevelPostUnlock)
			this.Logger.AddMessage("No Elly{z" . g_SF.Memory.ReadCurrentZone() . "}")
		}
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
	
    CheckifStuck() ;A test if stuck on current area. After 35s, toggles autoprogress every 5s. After 45s, attempts falling back up to 2 times. After 65s, restarts level.
    {
		dtCurrentZoneTime:=A_TickCount - this.PreviousZoneStartTime
		if (dtCurrentZoneTime<=35000) ;Irisiri - added fast exit for the standard case
			return false
        else if (dtCurrentZoneTime>35000 AND dtCurrentZoneTime<=45000 AND dtCurrentZoneTime - this.CheckifStuck_lastCheck > 5000) ; first check - ensuring autoprogress enabled
        {
            this.RouteMaster.ToggleAutoProgress(1, true)
            if(dtCurrentZoneTime < 40000) ;TODO: What purpose does this serve? To avoid interfering with the next check block?
                this.CheckifStuck_lastCheck:=dtCurrentZoneTime
        }
        if (dtCurrentZoneTime > 45000 AND this.CheckifStuck_fallBackTries < 3 AND dtCurrentZoneTime - this.CheckifStuck_lastCheck > 15000) ; second check - Fall back to previous zone and try to continue
        {
            ; reset memory values in case they missed an update.
            this.GameMaster.Hwnd:=WinExist("ahk_exe " . g_IBM_Settings["IBM_Game_Exe"]) ;TODO: This can screw things up if the there is more than one process open. At least align with .PID?
            g_SF.Memory.OpenProcessReader()
            g_SF.ResetServerCall()
            this.RouteMaster.FallBackFromZone() ;Try a fall back
            this.RouteMaster.SetFormation() ;In the base script this just goes to Q, which might not be ideal, especially for feat swap
            this.RouteMaster.ToggleAutoProgress(1, true)
            this.CheckifStuck_lastCheck:=dtCurrentZoneTime
            this.CheckifStuck_fallBackTries++
        }
        if (dtCurrentZoneTime > 65000)
        {
            this.GameMaster.RestartAdventure("Game is stuck z[" . g_SF.Memory.ReadCurrentZone() . "]" )
            this.GameMaster.SafetyCheck()
            this.PreviousZoneStartTime:=A_TickCount
            this.CheckifStuck_lastCheck:=0
            this.CheckifStuck_fallBackTries:=0
            return true
        }
        return false
    }

	;START PRE-FLIGHT CHECK

    PreFlightCheck() ;TODO: Pack some of this into functions - it's getting a bit large
    {
		;Check for active adventure
		if(this.GameMaster.CurrentAdventure=="" OR this.GameMaster.CurrentAdventure<=0)
		{
			errorMsg:="Unable to read adventure data."
			errorMsg.="`nPlease load into a valid adventure. Current adventure shows as: " . (CurrentObjID ? CurrentObjID : "-- Error --`n")
			errorMsg.=this.PreFlightCheck_GenericMessage()
			this.PreFlightErrorMessage("Adventure",errorMsg)
			return false
		}
		;Check Briv is saved in the expected formations
		brivInM:=g_Heroes[58].inM
        brivInQ:=g_Heroes[58].inQ
		brivInW:=g_Heroes[58].inW
		brivInE:=g_Heroes[58].inE ;Briv should be present in E if and only if we are feat swapping
		if (!brivInM OR !brivInQ OR !brivInW OR (this.RouteMaster.IsFeatSwap() != brivInE))
		{
			errorMsg:="Briv's presence in the saved formations is not as expected:`n"
			errorMsg.="M	Expected: Yes	Saved: " . (brivInM ? "Yes" : "No") . "`n"
			errorMsg.="Q	Expected: Yes	Saved: " . (brivInQ ? "Yes" : "No") . "`n"
			errorMsg.="W	Expected: Yes	Saved: " . (brivInW ? "Yes" : "No") . "`n"
			errorMsg.="E	Expected: " . (this.RouteMaster.IsFeatSwap() ? "Yes (FS)" : "No") . "	Saved: " . (brivInE ? "Yes" : "No") . "`n"
			errorMsg.=this.PreFlightCheck_GenericMessage()
			this.PreFlightErrorMessage("Briv Formations",errorMsg)
			return false
		}
		;Check for Metalborn
		if(!g_Heroes[58].HasCoreSpec(3455))
		{
			errorMsg:="Briv must have the Metalborn specialisation saved in the Modron formation.`n"
			errorMsg.=this.PreFlightCheck_GenericMessage()
			this.PreFlightErrorMessage("Briv Formations",errorMsg)
			return false
		}
		;Check for familiars, M, Q and E should have 3, W always 0
		familiarCountM:=g_SF.Memory.IBM_GetFormationFieldFamiliarCountBySlot(g_SF.Memory.GetActiveModronFormationSaveSlot())
		familiarCountQ:=g_SF.Memory.IBM_GetFormationFieldFamiliarCountBySlot(g_SF.Memory.GetSavedFormationSlotByFavorite(1))
		familiarCountW:=g_SF.Memory.IBM_GetFormationFieldFamiliarCountBySlot(g_SF.Memory.GetSavedFormationSlotByFavorite(2))
		familiarCountE:=g_SF.Memory.IBM_GetFormationFieldFamiliarCountBySlot(g_SF.Memory.GetSavedFormationSlotByFavorite(3))
        if (familiarCountM=="" OR familiarCountQ=="" OR familiarCountW=="" OR familiarCountE=="") ;Check for bad reads
		{
			errorMsg:="Familiars in saved formations could not be checked`n"
			errorMsg.=this.PreFlightCheck_GenericMessage()
			this.PreFlightErrorMessage("Familiars",errorMsg)
			return false
		}
        if (familiarCountM==0 OR familiarCountQ==0 OR familiarCountW>0 OR familiarCountE==0) ;Check for the minimum viable config - failing this check causes an abort
		{
			errorMsg:="Familiars in saved formations are not as expected:`n"
			errorMsg.="M	Expected: 3	Saved: " . familiarCountM . "`n"
			errorMsg.="Q	Expected: 3	Saved: " . familiarCountQ . "`n"
			errorMsg.="W	Expected: 0	Saved: " . familiarCountW . "`n"
			errorMsg.="E	Expected: 3	Saved: " . familiarCountE . " (Feat Swap)`n"
			this.PreFlightErrorMessage("Familiars",errorMsg) ;No generic message because we checked the memory reads for these are working above
			return false
		}
		if (familiarCountM!=3 OR familiarCountQ!=3 OR familiarCountW>0 OR familiarCountE!=3) ;Check for the expected config - the user may choose to proceed in this case
		{
			errorMsg:="Familiars in saved formations are not as expected but do meet the minimum requirements:`n"
			errorMsg.="M	Expected: 3	Saved: " . familiarCountM . "`n"
			errorMsg.="Q	Expected: 3	Saved: " . familiarCountQ . "`n"
			errorMsg.="W	Expected: 0	Saved: " . familiarCountW . "`n"
			errorMsg.="E	Expected: 3	Saved: " . familiarCountE . " (Feat Swap)`n"
			errorMsg.="Do you wish to continue?"
			this.PreFlightErrorMessage("Familiars",errorMsg,32+4) ;32 is Question, 1 is Yes/No
            IfMsgBox, No
				return false
		}
		;Check Modron automation is active
		modronEnabledF:=g_SF.Memory.ReadModronAutoFormation()==1
		modronEnabledR:=g_SF.Memory.ReadModronAutoReset()==1
		modronEnabledB:=g_SF.Memory.ReadModronAutoBuffs()==1
		modronStatusB:=g_IBM_Settings["IBM_Allow_Modron_Buff_Off"] OR modronEnabledB ;Request to allow this for those who don't want to have the modron core use potions, and instead save familiars in the formation. Which is apparently a thing. Not recommended, and the setting is not available in the GUI as a result
		if (!modronEnabledF OR !modronEnabledR OR !modronStatusB) ;If any of the Modron core functions are not set
		{
			errorMsg:="All 3 Mordon Core automation functions must be enabled before starting the gem farm. Current status:`n"
			errorMsg.="Set Formation: " . (modronEnabledF ? "Enabled" : "Disabled") . "`n"
			errorMsg.="Set Area Goal: " . (modronEnabledR ? "Enabled" : "Disabled") . "`n"
			errorMsg.="Set Buffs: " . (modronEnabledB ? "Enabled" : "Disabled") . "`n"
			errorMsg.=this.PreFlightCheck_GenericMessage()
			this.PreFlightErrorMessage("Modron",errorMsg)
			return false
		}
		;Check the Heroes collection has been able to read the heroIndex map
		if (!g_Heroes.Init())
		{
			errorMsg:="Unable to generate HeroID to HeroIndex map`n"
			errorMsg.=this.PreFlightCheck_GenericMessage()
			this.PreFlightErrorMessage("Hero Manager",errorMsg)
			return false
		}
		;Check availability
		gameInstanceID:=g_SF.Memory.IBM_GetActiveGameInstanceID() ;1 to 4, for the four parties
		lockedHeroesString:=""
		locked:=false
		for heroID, _ in this.LevelManager.savedFormationChamps["A"] ;A is a meta-formation that is the union of the other 4 TODO: Should have levelManager return this via a function?
		{
			heroInstanceID:=g_Heroes[heroID].ReadActiveGameInstanceID()
			if(heroInstanceID>0 AND heroInstanceID!=gameInstanceID) ;heroInstanceID of 0 means not assigned to an instance, which is fine
			{
				locked:=true
				lockedHeroesString.=g_Heroes[heroID].ReadName() . " (" . heroID . ") - Party " . heroInstanceID . "`n"
			}
		}
		if (locked)
		{
			errorMsg:="The following champions are configured for Briv Master but are currently active in other adventure parties:`n`n" . lockedHeroesString . "`nEither recall them or end their current adventures"
			this.PreFlightErrorMessage("Hero Manager",errorMsg)
			return false
		}
		;Check Feat Guard
		levelSettings:=g_IBM_Settings["IBM_LevelManager_Levels",this.RouteMaster.combining] ;Currently the feat data is not being loaded into the hero objects, as it's only relevant to the pre-flight check
		for heroID, _ in this.LevelManager.savedFormationChamps["A"] ;A is a meta-formation that is the union of the other 4 TODO: Should have levelManager return this via a function?
		{
			if(levelSettings.hasKey(heroID) AND levelSettings[heroID].hasKey("Feat_List") AND levelSettings[heroID].hasKey("Feat_Exclusive")) ;Data available
			{
				HERO_FEATS:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.FeatHandler.heroFeatSlots[heroID].List
				size:=HERO_FEATS.size.Read()
				if (size<0 or size>6) ;Allow an expansion of the number of feat slots in the future
				{
					this.PreFlightErrorMessage("Feat Guard","Unable to read equipped feats for heroID: " . heroID . "`n" . this.PreFlightCheck_GenericMessage())
					return false
				}
				extraFeats:={}
				checkList:=levelSettings[heroID,"Feat_List"].Clone() ;A copy is made so that found feats can be removed from it, leaving only those that are missing
				loop, %size%
				{
					id:=HERO_FEATS[A_Index - 1].ID.Read()
					name:=HERO_FEATS[A_Index - 1].Name.Read()
					if(id) ;heroFeatSlots always has the 4 slots
					{
						if (checkList.hasKey(id))
							checklist.Delete(id)
						else if (levelSettings[heroID,"Feat_Exclusive"]) ;In exclusive mode, track extra feats
						{
							extraFeats[id]:=name
						}
					}
				}
				if (checkList.Count()>0 OR extraFeats.Count()>0) ;Any fail
				{
					errorMsg:="Feat Guard found inconsistencies with the equipped feats of " . g_Heroes[heroID].ReadName() . " (" . heroID . ").`n"
					if (checkList.Count()>0)
					{
						errorMsg.="`nNot all of the required feats are present:`n"
						for featID, featName in levelSettings[heroID,"Feat_List"]
						{
							errorMsg.="	" . featName . " (" . featID . ") - " . (checkList.hasKey(featID) ? "Missing" : "Present") . "`n"
						}
					}
					if (extraFeats.Count()>0)
					{
						errorMsg.="`nExclusive mode is enabled and the following extra feats were found:`n"
						for featID, featName in extraFeats
						{
							errorMsg.="	" . featName . " (" . featID . ")`n"
						}
					}
					this.PreFlightErrorMessage("Feat Guard",errorMsg)
					return false
				}
			}
		}
        return true
    }

	PreFlightCheck_GenericMessage() ;Generic error text for PreFlightCheck() errors that might relate to reading from the game
	{
        genericMsg:="`nOther potential solutions:`n"
        genericMsg.="1. Be sure Imports are up to date. Current imports are for: v" . g_SF.Memory.GetImportsVersion() . "`n"
        genericMsg.="2. Check the correct memory file is being used. Current version: " . g_SF.Memory.GameManager.GetVersion() . "`n"
        genericMsg.="3. If IC is running with admin privileges, then the script will also require admin privileges.`n"
        if (_MemoryManager.is64bit)
            genericMsg.="4. Check AHK is 64-bit. (Currently " . (A_PtrSize = 4 ? 32 : 64) . "-bit)"
		return genericMsg
	}

	PreFlightErrorMessage(failingStep,message,options:=16) ;16 is Stop/Error icon, the default of just an OK button (option 0) is used as standard
	{
		title:="Briv Master Startup: " . failingStep
		Msgbox, % options, %title%, %message%
	}

	;END PRE-FLIGHT CHECK

    ModronResetCheck() 	;Waits for modron to reset. Closes IC if it fails.
    {
        if (g_SF.WaitForModronReset(45000)) ;Don't use timeout factor here as this isn't related to host performance
            this.TriggerStart:=true ;Only set this if the reset works - at the time of writing RestartAdventure() sets it anyway in all fail cases, but that needs to change. Older comment follows | TODO: If the reset fails, we might still be in the original run - need to detect this. Only force if CheckifStuck() not triggered? This creates a difficulty with run 1, where forcing a restart creates another run 1. Possibly force ONLY for run 1, just to reduce the total impact, as a workaround. Maybe we need to process the return values from the RestartAdventure() server calls to determine if it actually went through?
		else
        {
            this.GameMaster.RestartAdventure("Modron reset timed out z[" . g_SF.Memory.ReadCurrentZone() . "]",true) ;true flags this as a modron reset restart, where we should try and return to the adventure we're in if the server appears to be down
            this.GameMaster.SafetyCheck()
            this.CheckifStuck_lastCheck:=0 ;This used to be done by passing a 'force' option to CheckifStuck(), which seemed clunky - but we still need to reset these as we are no longer stuck. Or at least we hope not. TODO: Make a stuck-checker object to contain this stuff?
            this.CheckifStuck_fallBackTries:=0
		}
		this.PreviousZoneStartTime:=A_TickCount
    }

	;GEM FARM WINDOW
	CreateWindow()
	{
		global
		try
		{
			if (g_IBM_Settings["IBM_Window_Dark_Icon"])
				Menu Tray, Icon, %A_LineFile%\..\Resources\IBM_D.ico
			else
				Menu Tray, Icon, %A_LineFile%\..\Resources\IBM_L.ico
		}

		Gui, IBM_GemFarm:New, -Resize -MaximizeBox
		FormatTime, formattedDateTime,,% g_IBM_Settings["IBM_Format_Date_Display"]
		Gui IBM_GemFarm:Add, Text, w95 xm+5, % "Gem Farm Started:"
		Gui IBM_GemFarm:Add, Text, w105 x+3, % formattedDateTime
		Gui IBM_GemFarm:Add, Text, w95 xm+5, % "Settings Updated:"
		Gui IBM_GemFarm:Add, Text, w105 x+3 vIBM_GemFarm_Settings_Update_Time, % formattedDateTime
		Gui IBM_GemFarm:Add, Text, w95 xm+5, % "Game Version:"
		Gui IBM_GemFarm:Add, Text, w105 x+3 vIBM_GemFarm_Version_Game, % "Checking..."
		Gui IBM_GemFarm:Add, Text, w95 xm+5, % "Imports Version:"
		Gui IBM_GemFarm:Add, Text, w105 x+3 vIBM_GemFarm_Version_Imports, % "Checking..."

		if(!g_IBM_Settings["IBM_Window_Hide"])
		{
			Gui, IBM_GemFarm:Show,% "x" . g_IBM_Settings["IBM_Window_X"] . " y" . g_IBM_Settings["IBM_Window_Y"], Briv Master
		}
	}

	RefreshGemFarmWindow() ;Updates the time settings were updated
	{
	   FormatTime, formattedDateTime,,% g_IBM_Settings["IBM_Format_Date_Display"]
	   GuiControl, IBM_GemFarm:, IBM_GemFarm_Settings_Update_Time, % formattedDateTime
	}

	RefreshImportCheck()
	{
		gameMajor:=g_SF.Memory.ReadBaseGameVersion() ;Major version, e.g. 636.3 will return 636
		gameMinor:=g_SF.Memory.IBM_ReadGameVersionMinor() ;If the game is 636.3, return .3, 637 will return empty as it has no minor version
		importsMajor:=g_SF.Memory.Versions.Import_Version_Major
		importsMinor:=g_SF.Memory.Versions.Import_Version_Minor
		colour:="cRed" ;Default
		if (gameMajor!="" AND importsMajor!="") ;If both major versions are populated
		{
			if (gameMajor==importsMajor AND gameMinor==importsMinor) ;Full matching
				colour:="cBlack"
			else if (gameMajor==importsMajor) ;In this case the minor versions necessarily do not match
				colour:="cFFA000" ;"cFFC000" Amber had insuffient contrast so darkened a bit
		}
		gameString:=gameMajor ? (gameMajor . (gameMinor ? gameMinor : "")) : "Unable to detect"
		importString:=importsMajor ? (importsMajor . (importsMinor ? importsMinor : "") . " " . g_SF.Memory.Versions.Import_Revision) : "Unable to detect"
		GuiControl, IBM_GemFarm:+%colour%, IBM_GemFarm_Version_Game
		GuiControl, IBM_GemFarm:+%colour%, IBM_GemFarm_Version_Imports
		GuiControl, IBM_GemFarm:, IBM_GemFarm_Version_Game, % gameString
		GuiControl, IBM_GemFarm:, IBM_GemFarm_Version_Imports, % importString
		GuiControl, IBM_GemFarm:MoveDraw,IBM_GemFarm_Version_Game
		GuiControl, IBM_GemFarm:MoveDraw,IBM_GemFarm_Version_Imports
	}
	;END GEM FARM WINDOW
}
