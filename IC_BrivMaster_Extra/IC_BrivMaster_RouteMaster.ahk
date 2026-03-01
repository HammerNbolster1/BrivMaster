#include %A_LineFile%\..\..\..\SharedFunctions\CSharpRNG.ahk ;Used for Melf things

class IC_BrivMaster_RouteMaster_Class ;A class for managing routes
{
	zoneCap:=2501
	zones:={}
	leftoverCalculated:=false ;True once this has been calculated - has to be done after Thellora has been fielded
	leftoverHaste:=48
	cycleCount:=0 ;Counts the number of runs since the last game restart
	cycleMax:=1 ;Maximum runs per offline
	cycleForceOffline:=false ;Stack offline in all cases
	cycleDisableOffline:=false ;Stack online in all cases
	offlineSaveTime:=-1 ;Tracks the offline start time so it can be accessed globally
	;Below has to be a string because array literals can't be this long. Going to 400 jumps is a bit overkill
	static IRI_BRIVMASTER_JUMPCOST_METALBORN := "50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,81,84,87,90,93,96,99,102,105,108,112,116,120,124,128,132,136,140,145,150,155,160,165,170,176,182,188,194,200,207,214,221,228,236,244,252,260,269,278,287,296,306,316,326,337,348,359,371,383,396,409,423,437,451,466,481,497,513,530,548,566,585,604,624,645,666,688,711,734,758,783,809,836,864,893,923,953,984,1017,1051,1086,1122,1159,1197,1237,1278,1320,1364,1409,1456,1504,1554,1605,1658,1713,1770,1828,1888,1950,2014,2081,2150,2221,2294,2370,2448,2529,2613,2699,2788,2880,2975,3073,3175,3280,3388,3500,3616,3736,3859,3987,4119,4255,4396,4541,4691,4846,5006,5171,5342,5519,5701,5889,6084,6285,6493,6708,6930,7159,7396,7640,7893,8154,8424,8702,8990,9287,9594,9911,10239,10577,10927,11288,11661,12046,12444,12855,13280,13719,14173,14642,15126,15626,16143,16677,17228,17798,18386,18994,19622,20271,20941,21633,22348,23087,23850,24638,25452,26293,27162,28060,28988,29946,30936,31959,33015,34106,35233,36398,37601,38844,40128,41455,42825,44241,45703,47214,48775,50387,52053,53774,55552,57388,59285,61245,63270,65362,67523,69755,72061,74443,76904,79446,82072,84785,87588,90483,93474,96564,99756,103054,106461,109980,113616,117372,121252,125260,129401,133679,138098,142663,147379,152251,157284,162483,167854,173403,179135,185057,191175,197495,204024,210769,217737,224935,232371,240053,247989,256187,264656,273405,282443,291780,301426,311390,321684,332318,343304,354653,366377,378489,391001,403927,417280,431074,445324,460045,475253,490964,507194,523961,541282,559176,577661,596757,616484,636864,657917,679666,702134,725345,749323,774094,799684,826120,853430,881643,910788,940897,972001,1004133,1037327,1071619,1107044,1143640,1181446,1220502,1260849,1302530,1345589,1390071,1436024,1483496,1532537,1583199,1635536,1689603,1745458,1803159,1862768,1924347,1987962,2053680,2121570,2191705,2264158,2339006,2416328,2496207,2578726,2663973,2752038,2843014,2936998,3034089,3134389,3238005,3345046,3455626,3569862,3687874,3809787,3935730,4065837,4200245,4339096,4482537,4630720,4783802,4941944,5105314,5274085,5448435,5628549,5814617,6006836,6205409,6410546,6622465,6841389,7067551,7301189,7542551,7791892,8049475,8315573,8590468,8874450,9167820,9470888,9783975,10107412,10441541,10786716,11143302,11511676,11892227,12285358,12691486,13111039,13544462,13992213,14454765,14932608,15426248,15936207,16463024,17007256,17569479,18150288,18750298,19370143,20010478,20671981,21355352"

	__New(combine,logBase)
	{
		this.combining:=combine
		this.zonesPerJumpQ:=g_IBM_Settings["IBM_Route_BrivJump_Q"] + 1 ; We want the actual number of zones so adding 1 here, eg 9 jump goes from z1 to z11, so covers 10 zones (because it's the normal +1 progress plus the 9)
		if (g_Heroes[58].inE) ;Feat swap, ignored if Briv is not saved in E
			this.zonesPerJumpE:=g_IBM_Settings["IBM_Route_BrivJump_E"] + 1 ;As above
		else
			this.zonesPerJumpE:=1 ;Walking progresses 1 zone per 'jump'
		this.zonesPerJumpM:=g_IBM_Settings["IBM_Route_BrivJump_M"] + 1 ;Used when combining
		this.targetZone:=g_SF.Memory.GetModronResetArea()
		this.UpdateThellora(true) ;Must be done after the zones per jump are populated
		this.jumpCosts:=strsplit(IC_BrivMaster_RouteMaster_Class.IRI_BRIVMASTER_JUMPCOST_METALBORN,",")
		if (g_IBM_Settings[ "IBM_Online_Use_Melf"])
		{
			this.MelfManager:=new IC_BrivMaster_MelfMaster_Class(this.targetZone)
			this.UpdateMelfPatterns(true) ;We may not be on z1 when we start the script, so won't call Reset() initially
		}
		if (this.BrivHasThunderStep()) ;Multiplier for Briv stacks on conversion, to accomodate Thunder Step feat (2131)
			this.stackConversionRate:=1.2
		else
			this.stackConversionRate:=1
		this.KEY_autoProgress:=g_InputManager.getKey("g")
		this.KEY_Q:=g_InputManager.getKey("q")
		this.KEY_W:=g_InputManager.getKey("w")
		this.KEY_E:=g_InputManager.getKey("e")
		this.KEY_LEFT:=g_InputManager.getKey("Left")
		this.KEY_RIGHT:=g_InputManager.getKey("Right")
		this.HybridBlankOffline:=g_IBM_Settings["IBM_OffLine_Blank"] ;Should we avoid trying to get stacks when restarting during hybrid?
		this.RelayBlankOffline:=g_IBM_Settings["IBM_OffLine_Blank_Relay"]
		if (this.RelayBlankOffline)
		{
			this.RelaySetup(logBase)
			revokeFunc:=ObjBindMethod(this, "RelayComObjectRevoke")
			OnExit(revokeFunc)
		}
		this.useFaridehUlt:=g_IBM.LevelManager.savedFormationChamps["W"].HasKey(33)
		if (this.useFaridehUlt) ;Create a hero object for Farideh only if she's saved in W, this is so we don't get a pause whilst it's created at first call, which is in the stack setup
		{
			g_Heroes[33]
			this.FaridehUltThreshold:=g_IBM_Settings["IBM_Online_Farideh_Threshold"]
		}
		this.useBrivBoost:=g_IBM_Settings["IBM_LevelManager_Boost_Use"]
		if (this.useBrivBoost)
			this.BrivBoost:=new IC_BrivMaster_BrivBoost_Class(g_IBM_Settings["IBM_LevelManager_Boost_Multi"])
		this.CombineModeThelloraBossAvoidance:=g_IBM_Settings["IBM_Route_Combine_Boss_Avoidance"] ;Should we try to avoid combining into a boss by delaying the combine?
		g_SharedData.UpdateOutbound("IBM_RestoreWindow_Enabled",g_IBM_Settings["IBM_Route_Offline_Restore_Window"])
		g_SharedData.UpdateOutbound("IBM_RunControl_DisableOffline",false) ;Default to off
		g_SharedData.UpdateOutbound("IBM_RunControl_ForceOffline",false) ;Default to off
		this.LastSafeStackZone:=this.GetLastSafeStackZone() ;No reason to re-calcuate this every zone
		g_SharedData.UpdateOutbound("IBM_ProcessSwap",false) ;Allows the hub to detect process changes on restarts prompty
		this.LoadRoute()
	}

	Reset()
	{
		this.leftoverCalculated:=false
		this.leftoverHaste:=48
		this.cycleCount++
		g_IBM.Logger.SetRunCycle(this.cycleCount)
		this.cycleMax:=g_IBM_Settings["IBM_OffLine_Freq"]
		;Melf
		if(g_IBM_Settings["IBM_Online_Use_Melf"])
		{
			this.UpdateMelfPatterns(true) ;Calling with (true) cleans up old data from this call; no need to do that regularly
			this.MelfManager.Reset(g_IBM_Settings["IBM_Online_Melf_Min"],g_IBM_Settings["IBM_Online_Melf_Max"],5)
		}
		;Only process Run Control input from the hub at the start of a run, as changing mid-run could make a mess
		this.cycleDisableOffline:=g_SharedData.IBM_RunControl_DisableOffline
		if (g_SharedData.IBM_RunControl_ForceOffline)
		{
			this.cycleForceOffline:=true ;Queue
			g_SharedData.UpdateOutbound("IBM_RunControl_ForceOffline",false) ;Clear as this is a one-off
		}
		if (this.RelayBlankOffline)
		{
			this.RelayData.Reset()
		}
		this.UpdateStatusString()
		this.SetInitialStackString()
		g_SharedData.UpdateOutbound("IBM_ProcessSwap",false)
	}

	RelaySetup(logbase) ;One-time relay setup
	{
		this.RelayData:=new IC_BrivMaster_Relay_SharedData_Class()
		GuidCreate := ComObjCreate("Scriptlet.TypeLib")
		this.RelayData.GUID := GuidCreate.Guid
		this.RelayData.LogFile:=logBase . "_Relay.csv"
		ObjRegisterActive(this.RelayData, this.RelayData.GUID)
	}

	RelayComObjectRevoke()
	{
		ObjRegisterActive(this.RelayData, "")
	}

	CheckRelayRelease()
	{
		if(this.RelayBlankOffline)
			this.RelayData.PreRelease()
	}

	UpdateStatusString()
	{
		g_SharedData.UpdateOutbound("IBM_RunControl_CycleString","Cycle " . this.cycleCount . "/" . this.cycleMax . (this.cycleForceOffline ? " FO" : ""))
		g_SharedData.UpdateOutbound("IBM_RunControl_StatusString",this.GetStrategyString())
	}

	GetStrategyString() ;Separated to allow it to be placed into the log
	{
		targetStacks:=this.GetTargetStacks(true)
		return "Strategy: " . (this.combining ? "Combining" : "Non-combined") . " to z" . this.thelloraTarget . " then using " . targetStacks . " stacks (stacking " . (this.stackConversionRate!=1 ? CEIL((targetStacks-48)/this.stackConversionRate) . " w/TS" : targetStacks-48) . ") @" . this.zonesPerJumpQ . (this.zonesPerJumpE>1 ? "&&" . this.zonesPerJumpE : "") . "z/J to z" . this.targetZone
	}

	SetInitialStackString() ;Return the pre-stacking intent, i.e. on/offline and zone
	{
		if (this.ShouldOfflineStack()) ;Offline
			stackString:="Stacking: Expecting offline at z" . g_IBM_Settings["IBM_Offline_Stack_Zone"]
		else
		{
			if (g_IBM_Settings["IBM_Online_Use_Melf"]) ;Online with melf
			{
				melfRange:=this.MelfManager.GetFirstMelfSpawnMoreRange()
				if (melfRange)
					stackString:="Stacking: Expecting online with Melf in range z" . melfRange[1] . " to z" . melfRange[2]
				else
					stackString:="Stacking: Expecting online with Melf at z" . g_IBM_Settings[ "IBM_Online_Melf_Min" ] . " (no spawn more segment available)"
			}
			else
			{
				stackString:="Stacking: Expecting online at z" . g_IBM_Settings["IBM_Online_Melf_Min"]
			}
			if (this.ShouldBlankRestart())
			{
				if (this.RelayBlankOffline)
					stackString.=" with relay blank restart"
				else
					stackString.=" with blank restart"
			}
		}
		g_SharedData.UpdateOutbound("IBM_RunControl_StackString",stackString)
	}

	NeedToStack() ;Is stacking this run required, i.e. do we have less Steelbones than needed for the *next* run
	{
		return g_Heroes[58].ReadSBStacks() < this.GetTargetStacks()
	}

	GetTargetStacks(ignoreHaste:=false, forceRecalc:=false) ;Number of Steelbones stacks needed for the next run. Ignore haste is used for the status string showing the expected per run stack usage, rather than in-run calculation
	{
		if(ignoreHaste)
			return this.GetTargetStacksForFullRun(true)
		else
		{
			this.UpdateLeftoverHaste(forceRecalc)
			stacksToGenerate:=this.GetTargetStacksForFullRun() - this.leftoverHaste
			return CEIL(stacksToGenerate / this.stackConversionRate) ;Ceiling as the feat rounds down
		}
	}

	UpdateThellora(force:=false)
	{
		if (g_Heroes[139].UpdateRushTarget() OR force)
			this.thelloraTarget:=this.GetThelloraTarget(g_Heroes[139].rushCap,this.combining)
	}

	IsFeatSwap()
	{
		return this.zonesPerJumpE > 1
	}

	GetThelloraTarget(baseJump,combine)
	{
		if (combine) ;This is determining the Thellora jump, so when combining must use the jump value for M
			return baseJump + this.zonesPerJumpM ;No +1 as already included in this.zonesPerJumpM
		else
			return baseJump + 1
	}

	CheckThelloraBossRecovery() ;If option set, avoid Thellora combining into bosses due to a run that didn't complete by breaking the combine
	{
		if(this.CombineModeThelloraBossAvoidance AND this.combining)
		{
			thelloraCharges:=Floor(g_Heroes[139].GetCappedRushCharges()) ;Floor as the part-charges are presented as decimals, eg 307.2 = 307 zones plus 20% of the way to another
			rushTargetCombining:=this.GetThelloraTarget(thelloraCharges,true)
			if (rushTargetCombining < this.thelloraTarget AND MOD(rushTargetCombining,5)==0 AND MOD(this.GetThelloraTarget(thelloraCharges,false),5)!=0) ;If we are short on stacks and going to hit a boss, and not combining will land us on anything but a boss
			{
				g_IBM.levelManager.OverrideLevelByID(58,"z1c", true) ;Prevent Briv being levelled prior to completion of z1, breaking the combine
				g_IBM.Logger.AddMessage("CTBR: Broke combine to avoid hitting boss")
			}
		}
	}

	GetTargetStacksForFullRun(assumeStandardRush:=false) ;Returns the expected total stacks for a full run
	{
		assumeStandardRush ? rushNext:=0 : rushNext:=g_Heroes[139].rushNext ;This is set by the prior UpdateLeftoverHaste() call TODO: Why this weird use of separate assignments?
		if (rushNext)
			thelloraTarget:=this.GetThelloraTarget(rushNext,this.combining)
		else
			thelloraTarget:=this.thelloraTarget
		if (this.combining) ;We need to do one jump to reach ThelloraTarget in this case, and will leave the Casino on an M jump, not whatever fits the zone
		{
			jumps:=this.zones[thelloraTarget + this.zonesPerJumpM].jumpsToFinish + 2 ;1 for the combine, 1 for the M-jump after the Casino
			if (rushNext AND this.CombineModeThelloraBossAvoidance AND this.IsFeatSwap() AND this.zonesPerJumpM > this.zonesPerJumpE) ;If Thellora won't reach her target, we have boss recovery on, we are using feat swapping and the M jump would have been larger than an E jump, we need to generate an additional jump's worth of stacks, as replacing an M with an E would result in us needing 1 more jump Note: As this is a recovery mode trying to work out if the jump being replaced is Q or E doesn't seem worthwhile (it's made complex by her erratic behaviour if not in W)
			{
				jumps++
				g_IBM.Logger.AddThelloraCompensationMessage("GetTargetStacksForFullRun: Added extra jump for Thellora recovery for a total of: ",jumps)
			}
		}
		else
			jumps:=this.zones[thelloraTarget].jumpsToFinish ;Simple case
		return this.jumpCosts[jumps]
	}

	UpdateLeftoverHaste(forceRecalc:=false)
	{
		if (this.leftoverCalculated AND !forceRecalc)
			return
		else
		{
			g_Heroes[139].rushNext:=0
			calcResult:=this.UpdateLeftoverHaste_Calculate()
			this.leftoverHaste:=calcResult.haste
			if (g_Heroes[139].inA) ;If Thellora is in use
			{
				targetCharges:=g_Heroes[139].rushCap + (this.combining ? 0 : 1/5) ;If not combining Thellora will not get credit for z1. Note we can't use this.ThelloraTarget as that includes a possible combined jump and the +1. TODO: Check for her presence in W here?
				currentCharges:=g_Heroes[139].ReadRushAreaCharges()
				remainingCharges:=MAX(0,targetCharges-currentCharges)
				if (calcResult.partialRun) ;We can't make the end of this run and will reset early. We need to work out if we need to get extra stacks to make up for Thellora's rush shortfall in the next run
				{
					zonesRemaining:=MAX(0,this.GetStackDepletionZone(calcResult.zone,calcResult.jumpsToDepletion)-calcResult.zone)
				}
				else
					zonesRemaining:=MAX(0,this.targetZone-calcResult.zone)
				if (zonesRemaining < remainingCharges*5)
				{
					g_Heroes[139].rushNext:=FLOOR(currentCharges + (zonesRemaining/5)) ;Number of charges she will have. Note the floor is required as this will be used as an array index and must be an INT as a result. The // operator returns a float because AHK is dumb. TODO: Like most the Thellora code, should read the feat
				}
				if (g_SF.Memory.ReadHighestZone()>=this.thelloraTarget) ;If we've calculated post-Thellora, don't do so again - whilst technically we could reduce jumps by drifting that is not something we plan to do!
					this.leftoverCalculated:=true
			}
			else
				this.leftoverCalculated:=true
		}
	}

	GetStackDepletionZone(zoneNumber,jumps)
	{
		while (jumps>0)
		{
			currentZone:=this.zones[zoneNumber]
			if (currentZone.jumpZone) ;On Q
			{
				nextZoneNumber:=currentZone.z+this.zonesPerJumpQ
				jumps--
			}
			else
			{
				nextZoneNumber:=currentZone.z+this.zonesPerJumpE
				if (this.zonesPerJumpE>1) ;If Briv is in E this also costs a jump
					jumps--
			}
			zoneNumber:=nextZoneNumber
		}
		return zoneNumber
	}

	UpdateLeftoverHaste_Calculate() ;Returns the number of haste stacks expected to remain at the end of this run, the number of jumps made at the point stacks will run out (normally 0), whether we will run out early, and the zone is also returned to further processing. Examples:
	;.haste=48, .partialRun=false, .jumpsToDepletion=0 and .zone=349 would mean we will expect to make it to the end, having done the calc on z349
	;.haste=48, .partialRun=true, .jumpsToDepletion=80 and .zone=501 would mean would mean we can jump 80 times, then will be out of stacks, having done the calculation on z501
	{
		calcResult:={}
		calcResult.haste:=g_Heroes[58].ReadHasteStacks()
		if (!g_SF.Memory.ReadTransitioning()) ;If we're not in a transition at all, we need to use the current zone as the next zone may be unlocked (eg if stacking) - TODO: Needs to go in a function, as it's used in EnoughHasteForCurrentRun() too. Also TODO: The transition override was removing from this as the memory read is no longer available as of v637 (Nov25) - can we use one of the other transition reads to keep this robust?
			calcResult.zone:=g_SF.Memory.ReadCurrentZone()
		else ;Use the highest zone, as we should have spent the stacks as we left the previous one
			calcResult.zone:=g_SF.Memory.ReadHighestZone()
		jumps:=this.zones[calcResult.zone].jumpsToFinish
		calcResult.jumpsToDepletion:=0
		calcResult.partialRun:=false
		if (jumps < 1) ;No stacks needed if no jumps required
		{
			return calcResult
		}
        while (jumps > 0)
        {
            if (calcResult.haste < 50) ;Won't jump with <50 stacks, script will in most cases abort the run when they run out
            {
				calcResult.partialRun:=true
				calcResult.jumpsToDepletion:=this.zones[calcResult.zone].jumpsToFinish - jumps
				return calcResult
			}
            calcResult.haste:=Round(calcResult.haste*0.968)
            jumps--
        }
        return calcResult
	}

	EnoughHasteForCurrentRun() ;True if we have enough haste stacks to complete the run
	{
		if (!g_SF.Memory.ReadTransitioning()) ;If we're not in a transition at all, we need to use the current zone as the next zone may be unlocked (eg if stacking)
			zone:=g_SF.Memory.ReadCurrentZone()
		else ;Use the highest zone, as we should have spent the stacks as we left the previous one
			zone:=g_SF.Memory.ReadHighestZone()
		return g_Heroes[58].ReadHasteStacks() >= this.zones[zone].stacksToFinish
	}

	ShouldOfflineStack()
    {
        if (this.HybridBlankOffline) ;This logic is not used if we are doing blank offlines
			return false
		else if (this.cycleForceOffline) ;Force offline takes priority, as it will often be used with offline disabled below
			return True
		else if (this.cycleDisableOffline)
			return False
		else if (this.cycleMax==1) ;Hybrid disabled
            return True
        else if (this.cycleCount>=this.cycleMax) ;Hybrid Offline
			return True
		else ;Stack online
			return False
    }

	ExpectingGameRestart()
	{
		return this.ShouldOfflineStack() OR this.ShouldBlankRestart()
	}

	ShouldBlankRestart() ;This is run-based intent, other conditions (per TestForBlankOffline()) may cause a different result
	{
		return this.HybridBlankOffline AND (this.cycleCount >= this.cycleMax OR this.cycleForceOffline) AND (!this.cycleDisableOffline OR this.cycleForceOffline)
	}

	TestForBlankOffline(currentZone)
	{
		if ((this.ShouldBlankRestart() AND this.EnoughHasteForCurrentRun()) OR (this.RelayBlankOffline AND this.RelayData.IsActive())) ;Do not attempt relay if we don't have enough haste to complete the run, as that will require a forced restart. Once we start the relay manager, we are committed
		{
			restartZone:=g_IBM_Settings["IBM_Offline_Stack_Zone"] ;Default
			if (currentZone > restartZone) ;CycleCount will be reset on return from offline, so this will only trigger once
			{
				this.BlankRestart()
			}
			else if (this.RelayBlankOffline AND !this.RelayData.HasTriggered()) ;Check for relay only if it isn't already active
			{
				relayZone:=this.RelayData.GetRelayZone(restartZone,this)
				if (currentZone>relayZone) ;If beyond the relay threshold TODO: If we need to stack this has to wait. Maybe it could be set to go 500 zones before the expected stack zone if that many are available?
				{
					this.RelayData.Start()
				}
			}
		}
	}

	BlankRestart() ;Restart without stacking TODO: We need an option to stop progress here for potatoes
    {
		startStacks:=g_Heroes[58].ReadSBStacks()
		offlineStartTime:=A_TickCount
		startZone:=g_SF.Memory.ReadCurrentZone() ; record current zone before saving for bad progression checks
		g_IBM.Logger.AddMessage("BlankRestart Entry:z" . startZone)
		g_IBM.GameMaster.CloseIC("BlankRestart",this.RelayBlankOffline) ;2nd arg is to use PID only, so we don't close the relay copy of the game when in that mode
		if (this.RelayBlankOffline)
		{
			g_IBM.Logger.AddMessage("BlankRestart() returning game in Relay mode")
			this.RelayData.Release()
			g_IBM.routeMaster.ResetCycleCount() ;TODO: Do these make sense here? Might need to be after picked up
			g_IBM.DialogSwatter.Start() ;This seems a bit low-priority to happen this early, can we make it check later?
		}
		else ;The sleep is to allow launcher like EGS to detect the game has closed, but that is not applicable to relay (which can't use the EGS launcher)
		{
			if (g_IBM_Settings["IBM_OffLine_Sleep_Time"])
			{
				g_SharedData.UpdateOutbound("LoopString","BlankRestart: Sleep")
				ElapsedTime := 0
				while (ElapsedTime < g_IBM_Settings["IBM_OffLine_Sleep_Time"])
				{
					g_SharedData.UpdateOutbound("LoopString","BlankRestart Sleep: " . g_IBM_Settings["IBM_OffLine_Sleep_Time"] - ElapsedTime)
					g_IBM.IBM_Sleep(15)
					ElapsedTime := A_TickCount
				}
			}
		}
		g_IBM.GameMaster.SafetyCheck() ;TODO: Does this do more harm than good during Blank offlines? It can potentially swap the process back to the wrong one if the window is still in existance? Need to roll our own for the blank codepath? Possibly needs to be changed for all runs
		totalTime:=A_TickCount-offlineStartTime
		generatedStacks:=g_Heroes[58].ReadSBStacks() - startStacks
		returnZone:=g_SF.Memory.ReadCurrentZone()
		if (returnZone<startZone) ;We've gone backwards, this is expected as we don't stop autoprogress, although it can also happen if the exit save fails
		{
			g_IBM.RollBackAction(returnZone)
			g_IBM.Logger.AddMessage("BlankRestart() Exit Rollback Detected,Start@z" . startZone . ",End@z" . returnZone . "," . generatedStacks . ",Time:" . totalTime . ",OfflineTime:" . g_SF.Memory.ReadOfflineTime() . ",Server:" . g_SF.Memory.IBM_GetWebRootFriendly())
		}
		else
			g_IBM.Logger.AddMessage("BlankRestart() Exit, End@z" . returnZone . "," . generatedStacks . ",Time:" . totalTime . ",OfflineTime:" . g_SF.Memory.ReadOfflineTime() . ",Server:" . g_SF.Memory.IBM_GetWebRootFriendly())
        g_SharedData.UpdateOutbound("IBM_RunControl_StackString","Restarted at z" . returnZone . " in " . Round(totalTime/ 1000,2) . "s")
		g_IBM.PreviousZoneStartTime:= A_TickCount
    }

	TestForSteelBonesStackFarming() ;Returns true if we have a failure, namely the out of stacks and need to force restart case. TODO: Are we covering needing to stack at the recovery minimum when only offline stacking?
    {
		currentZone:=g_SF.Memory.ReadCurrentZone()
        if (currentZone < 0 OR currentZone>=this.targetZone) ;Don't test while modron resetting
            return 0
		stacks:=g_Heroes[58].ReadSBStacks()
		targetStacks:=this.GetTargetStacks()
 		if (stacks<targetStacks)
		{
			shouldOffline:=this.ShouldOfflineStack()
			if(shouldOffline AND currentZone>=g_IBM_Settings["IBM_Offline_Stack_Zone"]) ;This is now >= so we don't have to go around taking 1 off the stackzone all the time
			{
				this.StackRestart()
				this.StartAutoProgressSoft()
				return 0
			}
			else if (!shouldOffline AND !this.PostponeStacking(currentZone)) ;TODO: Bit silly to have to invert this, change to 'AllowStacking()' or something
			{
				this.StackNormal()
				this.StartAutoProgressSoft()
				return 0
			}
		}
        ; Briv ran out of jumps but has enough stacks for a new adventure, restart adventure. With protections from repeating too early. Irisiri - changed >z10 to >Thell target, but this will fail if Thell isn't present
		;04Jul25: Added check for transitioning, so we actually spend the last jump before resetting, otherwise we'll go as soon as the stacks are spent which is before we benefit from them
        if (g_Heroes[58].ReadHasteStacks() < 50 AND stacks>=targetStacks AND g_SF.Memory.ReadHighestZone()>this.thelloraTarget AND (g_SF.Memory.ReadHighestZone()<=this.targetZone) AND !g_SF.Memory.ReadTransitioning()) ;Removed the 5-zones-from-end check; if there's an armoured boss we'll not be able to be progress. TODO: With adventure-aware routing we could determine the last safe zone to walk from. Updated to not try and reset during relay restart (which shouldn't really happen since we don't blank if we don't have enough stacks...) Even more TODO: Should we check ReadAreaActive() here as well?
        {
            if (this.RelayBlankOffline AND this.RelayData.IsActive()) ;TODO: Something smart here
			{
				g_IBM.Logger.AddMessage("TestForSteelBonesStackFarming() force restart suppressed due to Relay")
			}
			else
			{
				g_IBM.Logger.AddMessage("Out of stacks:z" . currentZone)
				g_IBM.GameMaster.RestartAdventure("Out of Haste and have Steelbones for next")
				return 1
			}
        }
		return 0
    }

	ResetCycleCount()
	{
		this.cycleForceOffline:=false
		this.cycleCount:=0 ;Reset the count of runs in a cycle at offline. TODO: Could resetting this during the run cause problems? Might need to set a variable and process in Reset() - Note at this point the script expects this to happen
	}

	GetOffRampZone() ;returns the zone 5 Q-jumps from the reset, used to trigger offramp
	{
		return this.targetZone - this.zonesPerJumpQ * 5 ;TODO: Is it useful to check if this is after the Thellora target?
	}

	StackNormal()
    {
		g_Heroes[58].InitFastSB()
		startStacks:=stacks:=g_Heroes[58].FastReadSBStacks()
		targetStacks:=this.GetTargetStacks(,true) ;Force recalculation of remaining haste stacks
        if (this.ShouldAvoidRestack(stacks, targetStacks))
			return
		this.SetFormation() ;Ensure the correct formation is set for the zone before we stop progress and try to stack
		DllCall("QueryPerformanceCounter", "Int64*", startTime) ;Start counting time from the point we go to stop autoprogress - SetFormation() is a normal part of zone completion
		this.ToggleAutoProgress(0, false, true)
		currentZone:=g_SF.Memory.ReadCurrentZone()
	    if(this.useFaridehUlt)
		{
			if(currentZone<g_IBM_Settings["IBM_Online_Melf_Min"]) ;Avoid levelling Farideh in recovery - as a decent DPS she massively increases the stack zone, forcing us to walk much further
			{
				g_IBM.LevelManager.OverrideLevelByIDLowerToMax(33, "min", 0)
				activateFariUlt:=false
			}
			else
			{
				MEMORY_ACTIVE_MONSTERS_SIZE_ADDRESS:=_MemoryManager.instance.getAddressFromOffsets(g_SF.Memory.GameManager.game.gameInstances[0].Controller.area.activeMonsters.size.BasePtr.BaseAddress,g_SF.Memory.GameManager.game.gameInstances[0].Controller.area.activeMonsters.size.FullOffsets*)
				activateFariUlt:=true
			}
		}
		fastLevelList:={} ;Champions to be levelled at the start of the formation swap to W
		for heroID,_ in g_IBM.LevelManager.savedFormationChamps["XW"] ;eXclusive W
		{
			if(g_Heroes[heroID].NeedsLevelling())
			{
				if (g_Heroes[heroID].GetLevelsRequired() < 100)
					g_Heroes[heroID].Current.UseModifierForFast:=true ;Modifier press TODO: If this works out, encapsulate it better?
				else
					g_Heroes[heroID].Current.UseModifierForFast:=false ;Normal press
				fastLevelList.Push(g_Heroes[heroID])
			}
		}
		flames:=g_Heroes[83].inW ? g_Heroes[83].GetNumFlamesCards() : 0 ;Only check Elly's cards if present in W
		gameSpeed:=g_SF.Memory.IBM_ReadBaseGameSpeed()
		if(this.useFaridehUlt)
		{
			if(currentZone<g_IBM_Settings["IBM_Online_Melf_Min"]) ;Avoid levelling Farideh in recovery - as a decent DPS she massively increases the stack zone, forcing us to walk much further
			{
				g_IBM.LevelManager.OverrideLevelByIDLowerToMax(33, "min", 0)
				activateFariUlt:=false
			}
			else
			{
				MEMORY_ACTIVE_MONSTERS_SIZE_ADDRESS:=_MemoryManager.instance.getAddressFromOffsets(g_SF.Memory.GameManager.game.gameInstances[0].Controller.area.activeMonsters.size.BasePtr.BaseAddress,g_SF.Memory.GameManager.game.gameInstances[0].Controller.area.activeMonsters.size.FullOffsets*)
				activateFariUlt:=true
			}
		}
		this.OnlineStackFarmSetup(fastLevelList,activateFariUlt,15000/gameSpeed) ;Allow 1500ms at x10 for each state, 1200ms at x12.5
        ElapsedTime:=0
        g_SharedData.UpdateOutbound("LoopString","Stack Normal")
		if (this.useBrivBoost) ;Should this be moved before StackFarmSetup()? Or possibly into StartFarmSetup(this.useBrivboost) (as online only) - we want the first W press to occur before we start doing Other Stuff so the formation switch happens ASAP
			this.BrivBoost.Apply()
		g_IBM.levelManager.LevelFormation("W", "min") ;Ensures we're levelled, and applies any changes made based by Briv Boost if used
		maxOnlineStackTime:=(200000*g_IBM.CounterFrequency)/gameSpeed ;Reduces the 200s to 16s @ 12.5. Factoring the CounterFrequency in here means we can avoid doing it every loop
		if(g_IBM.failedConversionMode) ;In this case we're probably killing things as we've levelled champions, allow significantly more time
			maxOnlineStackTime*=5
		precisionMode:=false
		precisionTrigger:=Floor(targetStacks * 0.90) ;At a steady-state stack rate of 240/s, for 600 stacks this is 60 => ~250ms - which is plenty of time to activate precision mode. Note that because attacks can get synced we can't get too tight with this TODO: This might need lowering as salvos of 100 will skip right over it?
		currentZone:=g_SF.Memory.ReadCurrentZone() ;Used to report the stack zone, here as it is recorded before we toggle progress back on
		;++++++++++++++++++++++
		;START FARI DEBUG BLOCK
		/*
		EK_HANDLER:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.HeroHandler.heroes[g_Heroes[33].HeroIndex].effects.effectKeysByHashedKeyName
		EK_HANDLER_SIZE:=EK_HANDLER.size.Read()
		DEBUG_FARI_READ:=""
		loop, %EK_HANDLER_SIZE%
		{
			PARENT_HANDLER:=EK_HANDLER["value", A_Index - 1].List[0].parentEffectKeyHandler
			if ("farideh_infernal_aspect_handler"==PARENT_HANDLER.def.Key.Read())
			{
				DEBUG_FARI_READ:=PARENT_HANDLER.activeEffectHandlers[0].QuickClone()
				DEBUG_FARI_READ.FullOffsets.Push(192)
				Break
			}
		}
		EK_HANDLER:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.HeroHandler.heroes[g_Heroes[97].HeroIndex].effects.effectKeysByHashedKeyName
		EK_HANDLER_SIZE:=EK_HANDLER.size.Read()
		DEBUG_TATYANA_OFFSCREEN:=""
		DEBUG_TATYANA_AWAIT_TIMER:=""
		loop, %EK_HANDLER_SIZE%
		{
			PARENT_HANDLER:=EK_HANDLER["value", A_Index - 1].List[0].parentEffectKeyHandler
			if("tatyana_find_a_feast"==PARENT_HANDLER.def.Key.Read())
			{
				DEBUG_TATYANA_OFFSCREEN:=PARENT_HANDLER.activeEffectHandlers[0].QuickClone()
				DEBUG_TATYANA_OFFSCREEN.FullOffsets.Push(201) ;isOffscreen=Boolean (Char)
				DEBUG_TATYANA_AWAIT_TIMER:=PARENT_HANDLER.activeEffectHandlers[0].QuickClone()
				DEBUG_TATYANA_AWAIT_TIMER.FullOffsets.Push(176,80) ;awaitReturnTimer=object pointer, t=double
				break
			}
		}
		DllCall("QueryPerformanceCounter", "Int64*", loopTime)
		DEBUG_FARI_LOG:="Fari Init," . Round(loopTime/g_IBM.CounterFrequency,3) . ",S," . g_Heroes[58].FastReadSBStacks() . ","
		DEBUG_TATY_LOG:=""
		DEBUG_FARI_ALT_ACTIVE:=FALSE
		MEMORY_MELEE_ADDRESS:=_MemoryManager.instance.getAddressFromOffsets(g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numAttackingMonstersReached.BasePtr.BaseAddress,g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numAttackingMonstersReached.FullOffsets*)
		MEMORY_MELEE_TYPE:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numAttackingMonstersReached.ValueType
		MEMORY_RANGED_ADDRESS:=_MemoryManager.instance.getAddressFromOffsets(g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numRangedAttackingMonsters.BasePtr.BaseAddress,g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numRangedAttackingMonsters.FullOffsets*)
		MEMORY_RANGED_TYPE:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numRangedAttackingMonsters.ValueType
		DEBUG_FARI_ZONE_FULL:=false
		*/
		;END FARI DEBUG BLOCK
		;++++++++++++++++++++
		while (stacks<targetStacks AND ElapsedTime<maxOnlineStackTime)
        {
			if (activateFariUlt AND _MemoryManager.instance.Read(MEMORY_ACTIVE_MONSTERS_SIZE_ADDRESS,"Int")>=this.FaridehUltThreshold)
			{
				g_Heroes[33].UseUltimate(,true) ;Using ExitOnceQueued so we don't stay waiting for the activation and potentially overstack
				activateFariUlt:=false
				;++++++++++++++++++++++
				;START FARI DEBUG BLOCK
				;DEBUG_FARI_LOG.="Fari Ult Press," . Round(loopTime/g_IBM.CounterFrequency,3) . ",S," . stacks . ","
				;END FARI DEBUG BLOCK
				;++++++++++++++++++++
			}
			if (precisionMode OR activateFariUlt) ;This will mean if we never reach the FaridehUltThreshold we will be stuck on fast sleeps and not trigger the .gameFocus(). That being a problem requires 2 failures though, and we shouldn't have lost focus since the formation switch in general, so is probably okay
			{
				DllCall("Sleep", "UInt", 0)
			}
			else
			{
				if (stacks>precisionTrigger) ;Once we have hit precisionTrigger stacks go critical and check faster to get maximum precision
				{
					Critical On
					g_InputManager.gameFocus() ;Set Game Focus so we don't have to do it when releasing from the stack (this will cause issues if the game loses focus in the last few hundred ms of stacking)
					precisionMode:=true
				}
				DllCall("Sleep", "UInt", 10)
				;++++++++++++++++++++++
				;START FARI DEBUG BLOCK
				;DllCall("Sleep", "UInt", 0) ;FARI DEBUG - So as to not delay the debug checks
				;END FARI DEBUG BLOCK
				;++++++++++++++++++++
			}
			DllCall("QueryPerformanceCounter", "Int64*", loopTime)
			ElapsedTime:=loopTime - startTime
			stacks:=g_Heroes[58].FastReadSBStacks()
			;++++++++++++++++++++++
			;START FARI DEBUG BLOCK
			/*
			T_offscreen:=DEBUG_TATYANA_OFFSCREEN.Read("Char")
			T_await:=DEBUG_TATYANA_AWAIT_TIMER.Read("Double")
			if(T_offscreen OR T_await)
				DEBUG_TATY_LOG.="Taty:" Round(loopTime/g_IBM.CounterFrequency,3) . ":Off:" . DEBUG_TATYANA_OFFSCREEN.Read("Char") . ":Await:" . DEBUG_TATYANA_AWAIT_TIMER.Read("Double") . ","
			DEBUG_FARI_ULT_NOW:=DEBUG_FARI_READ.Read("Char")
			if(DEBUG_FARI_ULT_NOW!=DEBUG_FARI_ALT_ACTIVE)
			{
				if(DEBUG_FARI_ULT_NOW)
					DEBUG_FARI_LOG.="Fari Ult Start," . Round(loopTime/g_IBM.CounterFrequency,3) . ",S," . stacks . ","
				else
					DEBUG_FARI_LOG.="Fari Ult End," . Round(loopTime/g_IBM.CounterFrequency,3) . ",S," . stacks . ","
				DEBUG_FARI_ALT_ACTIVE:=DEBUG_FARI_ULT_NOW
			}
			if(!DEBUG_FARI_ZONE_FULL AND _MemoryManager.instance.Read(MEMORY_MELEE_ADDRESS,MEMORY_MELEE_TYPE) + _MemoryManager.instance.Read(MEMORY_RANGED_ADDRESS,MEMORY_RANGED_TYPE)>=100)
			{
				DEBUG_FARI_LOG.="Fari 100 Atk," . Round(loopTime/g_IBM.CounterFrequency,3) . ",S," . stacks . ","
				DEBUG_FARI_ZONE_FULL:=true
			}
			*/
			;END FARI DEBUG BLOCK
			;++++++++++++++++++++
        }
		;++++++++++++++++++++++
		;START FARI DEBUG BLOCK
		;DllCall("QueryPerformanceCounter", "Int64*", loopTime)
		;DEBUG_FARI_LOG.="Fari Stacked," . Round(loopTime/g_IBM.CounterFrequency,3) . ",S," . stacks
		;END FARI DEBUG BLOCK
		;++++++++++++++++++++
		this.KEY_autoProgress.KeyPress_Bulk() ;Enable autoprogress as fast as we can. If we're stuck the following will handle it. Using _Bulk for this reason-game focus is set when precision is turned on
		;++++++++++++++++++++++
		;START FARI DEBUG BLOCK
		;g_IBM.Logger.AddMessage("Flames:" . flames . "," . DEBUG_FARI_LOG . "," . DEBUG_TATY_LOG)
		;END FARI DEBUG BLOCK
		;++++++++++++++++++++
		if (ElapsedTime>=maxOnlineStackTime)
        {
            Critical Off
			g_IBM.GameMaster.RestartAdventure("Normal took too long (" . ROUND(ElapsedTime/(g_IBM.CounterFrequency*1000),1) . "s)") ;TODO: This seems a bit extreme?
            g_IBM.GameMaster.SafetyCheck()
            g_IBM.PreviousZoneStartTime:=A_TickCount
            return
        }
        g_IBM.PreviousZoneStartTime:=A_TickCount
        runComplete:=g_SF.Memory.ReadHighestZone()>=this.targetZone ;If we'll jump from stack zone straight to reset zone things get a bit weird as the game behaves differently transitioning to the reset zone
		if (!runComplete)
		{
			if (g_SF.Memory.ReadQuestRemaining()>0)
			{
				g_IBM.Logger.AddMessage("Online stack zone not complete - falling back")
				this.FallBackFromZone()
			}
			else
				this.ToggleAutoProgress(1, false, true)
		}
		Critical Off
		generatedStacks:=stacks - startStacks
		g_SharedData.UpdateOutbound("IBM_RunControl_StackString","Stacking: Completed online at z" . currentZone . " generating " . generatedStacks . " stacks in " . Round(ElapsedTime/g_IBM.CounterFrequency,0) . "ms")
		g_IBM.Logger.AddMessage("Online{M=" . this.MelfManager.GetCurrentMelfEffect() . " F=" . flames . " z" . currentZone . " Tar=" . targetStacks . "}," . generatedStacks . "," . ROUND(ElapsedTime/g_IBM.CounterFrequency,0)) ;TODO: The melf effect call is after we resume progress, should we pass it the stack zone?
		if (!runComplete)
		{
			this.SetFormation(,true) ;Use the high zone, as the current zone is complete
			this.WaitForTransition() ;Wait for the zone transition so that a normal SetFormation() doesn't overwrite the highzone call TODO: Can we just wait for the jump-off part?
		}
    }

	OnlineStackFarmSetup(fastLevelList,expectFari,timeOut:=1000)
    {
        MEMORY_QUEST_ADDRESS:=g_SF.Memory.ResolvePointers(g_SF.Memory.GameManager.game.gameInstances[0].ActiveCampaignData.currentArea.QuestRemaining)
		MEMORY_QUEST_TYPE:=g_SF.Memory.GameManager.game.gameInstances[0].ActiveCampaignData.currentArea.QuestRemaining.ValueType
		this.WaitForTransition()
		endTime:=A_TickCount+timeOut
        active:=g_SF.Memory.ReadAreaActive()
        while(!active AND A_TickCount<endTime) ;Wait for the zone to become active. The zone should complete ~35ms later at 12.5x, so we can briefly use a faster loop there without thrashing our CPU too much
        {
            DllCall("Sleep", "UInt", 1)
            active:=g_SF.Memory.ReadAreaActive()
        }
		endTime:=A_TickCount+timeOut ;Reset end time, this is applied once - if one fastLevelList times out the rest need to as well
		quest:=_MemoryManager.instance.Read(MEMORY_QUEST_ADDRESS,MEMORY_QUEST_TYPE)
		for _,Hero in fastLevelList
		{
			while(quest>0 AND A_TickCount<endTime)
			{
				quest:=_MemoryManager.instance.Read(MEMORY_QUEST_ADDRESS,MEMORY_QUEST_TYPE) ;No delay, single read - we want to catch completion as closely as possible
			}
			this.KEY_W.KeyPress_Bulk() ;Trying _Bulk here as we just stopped progress
			loopCount:=0
			while(!Hero.ReadSelectedInSeat() and loopCount<12) ;On my desktop usually managed by 8 before swap improvements were made - so this should be plenty, but probably needs checking on a slower system
			{
				if(loopCount) ;As there's no sleep before the first loop, avoid sending an immediate second input - we could potentially use an initial 'if' block to avoid this check, but whether it matters much will depend on how often it actually gets run
					this.KEY_W.KeyPress_Bulk()
				loopCount++
			}
			if (Hero.Current.UseModifierForFast)
			{
				g_IBM.LevelManager.SetModifierKey(true)
				Hero.Key.KeyPress_Bulk()
				g_IBM.LevelManager.SetModifierKey(false)
			}
			else
				Hero.Key.KeyPress_Bulk()
		}
		if(quest>0) ;If the fast handlers didn't wait for quest completion (e.g. because there were none)
		{
			while(quest>0 AND A_TickCount<endTime) 
			{
				DllCall("Sleep", "UInt", 1) ;Sleep in this version as no massive rush to deploy champions
				quest:=_MemoryManager.instance.Read(MEMORY_QUEST_ADDRESS,MEMORY_QUEST_TYPE)
			}
			this.KEY_W.KeyPress_Bulk()
		}
        StartTime:=A_TickCount
		ElapsedTime:=0
		g_SharedData.UpdateOutbound("LoopString","Setting stack farm formation")
		while(!this.FormationCheckWithFari(expectFari) AND ElapsedTime<timeOut) ;TODO: We might want to make a check that returns true if the formation is selected, either on field or in their bench seat, as this will fail if someone doesn't get placed after levelling due to the formation being under attack
        {
			this.KEY_W.KeyPress() ;Not using _Bulk here as the swap here is a failure mode
			g_IBM.levelManager.LevelFormation("W", "min",0) ;TODO: Can we do something specific to prioritise getting everyone fielded here? There might be both Melf and Fari to get out
			DllCall("Sleep", "UInt", 10)
            ElapsedTime:=A_TickCount - StartTime
        }
		if(ElapsedTime>=timeOut)
		{
			g_IBM.Logger.AddMessage("FAIL: OnlineStackFarmSetup() did not set W formation within " . timeOut . "ms")
			g_IBM.Logger.AddMessage(">DEBUG: Melf Level=[" . g_Heroes[59].ReadLevel() . "] Fari Level=[" . g_Heroes[33].ReadLevel() . "] Formation=" . this.DEBUG_FORMATION_STRING())
		}
    }

	PostponeStacking(currentZone) ;Used to delay stacking whilst waiting for Melf's spawn-more buff, or for a preferred stacking zone for non-Melf online
    {
        if(currentZone<g_IBM_Settings["IBM_Offline_Stack_Min"]) ;Never attempt to stack below minimum recovery stack zone
			return true
		if (g_Heroes[58].ReadHasteStacks()<50) ;Stack immediately if Briv can't jump anymore.
            return false
		if (currentZone>this.LastSafeStackZone) ; Stack immediately to prevent resetting before stacking.
			return false
		if(currentZone<g_IBM_Settings["IBM_Online_Melf_Min"]) ;Below target minimum online zone (for Melf or otherwise, bad name). Here as this will be called once we pass the recovery minimum
			return true
		if(g_IBM_Settings["IBM_Online_Use_Melf"]) ;Melf mode
		{
			nextSpawnMoreRange:=this.MelfManager.GetFirstMelfSpawnMoreRange(currentZone)
			if(nextSpawnMoreRange)
			{
				if (currentZone<nextSpawnMoreRange[1]) ;We're below the desired stack range, and (per the above check) one exists
					return true
				else if (!this.zones[currentZone].stackZone) ;Not on a stack zone
					return true
				else if (!this.MelfManager.IsMelfEffectSpawnMore(currentZone)) ;Not spawning more
					return true
			}
			else ;No Spawn More available
			{
				if (this.zones[currentZone].stackZone==false) ;Even without spawn more, try to use a desired stackzone
					return true
			}
		}
		else ;Non-Melf, this just needs to consider preferred zones
		{
			if (this.zones[currentZone].stackZone==false)
					return true
		}
		return false
    }

	GetLastSafeStackZone()
    {
        lastZone:=this.targetZone - 1
        ; Move back one zone if the last zone before reset is a boss.
        if (Mod(lastZone,5)==0)
            lastZone--
        return lastZone - this.zonesPerJumpQ
    }

    ShouldAvoidRestack(stacks, targetStacks) 	; avoids attempts to stack again after stacking has been completed and level not reset yet.
    {
        if ( stacks >= targetStacks )
            return 1
        if (g_SF.Memory.ReadCurrentZone() == 1) ; likely modron has reset
            return 1
        if (g_SF.Memory.ReadCurrentZone() < g_IBM_Settings["IBM_Offline_Stack_Min"]) ; don't stack below min stack zone ;TODO: Is this useful?
            return 1
        return 0
    }

	StackRestart() ;TODO: Put rollback detection back into this?
    {
		startStacks:=lastStacks:=stacks:=g_Heroes[58].ReadSBStacks()
		targetStacks:=this.GetTargetStacks(,true) ;Force recalculation of remaining haste stacks
        if (this.ShouldAvoidRestack(stacks, targetStacks))
        {
			return
		}
        retryAttempt := 0
        if (this.cycleMax == 1) ;If doing hybrid we should never retry - the purpose of going offline is to clear memory bloat, and that is fulfilled whether we stack or not
			maxRetries:= 2
		else
			maxRetries:=0
		offlineStartTime:=A_TickCount
        while (stacks < targetStacks AND retryAttempt <= maxRetries )
        {
			this.StackFailRetryAttempt++ ; per run
            retryAttempt++               ; pre stackfarm call
            this.StackFarmSetup()
            if (this.targetZone != "" AND g_SF.Memory.ReadCurrentZone() > this.targetZone)
            {
                g_SharedData.UpdateOutbound("LoopString","Attempted to offline stack after modron reset - verify settings")
                break
            }
			this.offlineSaveTime:=g_IBM.GameMaster.CloseIC( "StackRestart" . (this.StackFailRetryAttempt > 1 ? (" - Warning: Retry #" . this.StackFailRetryAttempt - 1 . ". Check Stack Settings."): "") )
			g_SharedData.UpdateOutbound("LoopString","Stack Sleep: ")
            ElapsedTime:=0
			sleepStart:=A_TickCount ;Seperate to the save timer, this is the delay in restarting the game specifically
			while ( ElapsedTime < g_IBM_Settings["IBM_OffLine_Sleep_Time"] )
            {
                g_SharedData.UpdateOutbound("LoopString","Stack Sleep: " . g_IBM_Settings["IBM_OffLine_Sleep_Time"] - ElapsedTime)
                g_IBM.IBM_Sleep(15)
				ElapsedTime := A_TickCount - sleepStart
            }
			g_IBM.GameMaster.SafetyCheck()
            stacks:=g_Heroes[58].ReadSBStacks()
            ;check if save reverted back to below stacking conditions
            if (g_SF.Memory.ReadCurrentZone() < g_IBM_Settings["IBM_Offline_Stack_Min"])
            {
                g_SharedData.UpdateOutbound("LoopString","Stack Sleep: Failed (zone < min)")
                Break  ; "Bad Save? Loaded below stack zone, see value."
            }
            ;g_SharedData.PreviousStacksFromOffline := stacks - lastStacks ;Doesn't appear to be used for anything
            lastStacks := stacks
			g_IBM.Logger.AddMessage("Offline:" . g_SF.Memory.ReadCurrentZone() . "," . stacks . ",Time:" . A_TickCount - this.offlineSaveTime . ",Attempt:" . retryAttempt . ",OfflineTime:" . g_SF.Memory.ReadOfflineTime() . ",Server:" . g_SF.Memory.IBM_GetWebRootFriendly())
			this.offlineSaveTime:=-1 ;Flags as not active
        }
        g_IBM.PreviousZoneStartTime:=A_TickCount
		generatedStacks:=g_Heroes[58].ReadSBStacks() - startStacks
		totalTime:=A_TickCount-offlineStartTime
		if (retryAttempt > maxRetries+1) ;We're a bit screwed at this point, +1 as retryAttempt is really 'tryAttempt'
        {
			g_SharedData.UpdateOutbound("LoopString","Failed to generate target " . targetStacks . " stacks in " . maxRetries . " attempts. Verify settings")
			g_SharedData.UpdateOutbound("IBM_RunControl_StackString","FAIL: Attempted to stack offline at z" . g_SF.Memory.ReadCurrentZone() . " generating " . generatedStacks . " stacks in " . Round(totalTime/ 1000,2) . "s" . (retryAttempt>1 ? " using " . retryAttempt . " attempts" : ""))
        }
        else
		{
			g_SharedData.UpdateOutbound("IBM_RunControl_StackString","Stacking: Completed offline at z" . g_SF.Memory.ReadCurrentZone() . " generating " . generatedStacks . " stacks in " . Round(totalTime/ 1000,2) . "s" . (retryAttempt>1 ? " using " . retryAttempt . " attempts" : ""))
		}
    }

	StackFarmSetup()
    {
		if (!this.KillCurrentBoss())
            this.FallBackFromBossZone()
        this.KEY_W.KeyPress()
        this.ToggleAutoProgress(0,false,true)
		g_IBM.levelManager.LevelFormation("W", "min")
		this.WaitForTransition(this.KEY_W)
		StartTime := A_TickCount
        ElapsedTime := 0
		TimeOut:=5000
        g_SharedData.UpdateOutbound("LoopString","Setting stack farm formation")
        while (!this.FormationCheckWithFari() AND ElapsedTime < TimeOut)
        {
			this.KEY_W.KeyPress() ;Not using _Bulk here as the swap here is a failure mode
            g_IBM.levelManager.LevelFormation("W", "min") ;Should this be here?
			g_IBM.IBM_Sleep(15)
            ElapsedTime := A_TickCount - StartTime
        }
		if (elapsedTime >= TimeOut)
			g_IBM.Logger.AddMessage("FAIL: StackFarmSetup() did not set W formation within " . TimeOut . "ms")
    }

	;Override to remove swap to E when feat swapping. TODO: Why did this swap to E anyway? Just using a normal SetFormation
	;This is called when trying to stack, if for some reason we're trying to stack on a boss zone A) things have gone weird (fallback maybe?) and B) We should complete on the expected formation to stay on-route. If that jumps us into the Modron reset that's a route setup issue (although perhaps we should check for it)
	KillCurrentBoss(maxLoopTime:=25000)
    {
        currentZone := g_SF.Memory.ReadCurrentZone()
        if mod(currentZone, 5)
            return 1
        StartTime := A_TickCount
        ElapsedTime := 0
        g_SharedData.UpdateOutbound("LoopString","Killing boss before stacking")
        while ( !mod( g_SF.Memory.ReadCurrentZone(), 5 ) AND ElapsedTime<maxLoopTime )
        {
            ElapsedTime := A_TickCount - StartTime
            this.SetFormation()
            if(!g_SF.Memory.ReadQuestRemaining()) ; Quest complete, still on boss zone. Skip boss bag.
                this.ToggleAutoProgress(1,0,false)
            g_IBM.IBM_Sleep(50)
        }
        if(ElapsedTime >= maxLoopTime)
            return 0
        this.WaitForTransition()
        return 1
    }

	FormationCheckWithFari(faridehRequired:=false) ;True if the formations exactly match, EXCEPT for Farideh (33), is ignored if faridehRequired=false, or if true she must be present, but in any slot. TODO: Might be best to change this to only check Briv is alone in the first column, and the required champions are present otherwise?
	{
		w:=g_IBM.levelManager.GetFormation("W")
        currentFormation:=g_SF.Memory.GetCurrentFormation()
		fariFound:=false
        if(!IsObject(currentFormation))
            return false
        loop, % currentFormation.Count()
        {
			if(currentFormation[A_Index]==33) ;Fari, set flag but do any further checks since we don't care where she is. Note this means Fari could have taken the place of another champion who is not placed, but we shouldn't be levelling most champions in W
			{
				fariFound:=true
			}
			else ;Fari might still be in this spot in the W formation, in which case ignore her
			{
				if(w[A_Index]!=currentFormation[A_Index] AND w[A_Index]!=33) ;Ignore if the W match is Fari
					return false
			}
		}
        return !faridehRequired OR fariFound
	}

	DEBUG_FORMATION_STRING() ;Returns the formation size and members as a string
	{
		size := g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.slots.size.Read()
		if(size <= 0 OR size > 14) ; sanity check, 12 is the max number of concurrent champions possible.
			return "X:[]"
		formation:=":["
		champCount:=0
		loop, %size%
		{
			heroID := g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.slots[A_index - 1].hero.def.ID.Read()
			if (heroID>0)
				champCount++
			else
				heroID:="_"
			formation.=heroID . ";"
		}
		formation:=champCount . formation . "]"
		return formation
	}

	WaitForTransition(KEY:="", maxLoopTime:=5000) ;KEY is a IC_BrivMaster_InputManager_Key_Class object
    {
        if !g_SF.Memory.ReadTransitioning()
            return
        StartTime:=A_TickCount
        ;g_SharedData.UpdateOutbound("LoopString","Waiting for transition...") ;Not sure if this will generally be displayed long enough to be useful
        if (KEY)
			g_InputManager.gameFocus() ;Set focus once and use _Bulk()
		while (g_SF.Memory.ReadTransitioning()==1 AND A_TickCount - StartTime < maxLoopTime)
        {
			If (KEY)
				KEY.KeyPress_Bulk()
			g_IBM.IBM_Sleep(15)
        }
        return
    }

	FallBackFromBossZone(KEY:="", maxLoopTime:=5000)
    {
        fellBack:=false
        currentZone := g_SF.Memory.ReadCurrentZone()
        if (Mod(currentZone, 5))
            return fellBack
        StartTime:=A_TickCount
        ElapsedTime:=0
        g_SharedData.UpdateOutbound("LoopString","Falling back from boss zone")
        while (!Mod(g_SF.Memory.ReadCurrentZone(), 5) AND ElapsedTime < maxLoopTime)
        {
            this.KEY_LEFT.KeyPress()
			fellBack:=true
			g_IBM.IBM_Sleep(15)
			ElapsedTime:=A_TickCount - StartTime
        }
        this.WaitForTransition(KEY)
        return fellBack
    }

	FallBackFromZone(maxLoopTime:=5000)
    {
        StartTime:=A_TickCount
        ElapsedTime:=0
        while(g_SF.Memory.ReadCurrentZone() == -1 AND ElapsedTime < maxLoopTime)
        {
			g_IBM.IBM_Sleep(15)
			ElapsedTime:=A_TickCount-StartTime
        }
        currentZone:=g_SF.Memory.ReadCurrentZone()
        StartTime:=A_TickCount
        ElapsedTime:=0
        g_SharedData.UpdateOutbound("LoopString","Falling back from zone...")
        while(!g_SF.Memory.ReadTransitioning() AND ElapsedTime < maxLoopTime)
        {
            this.KEY_LEFT.KeyPress()
			g_IBM.IBM_Sleep(15) ;Sleep as we don't want to go back multiple zones
			ElapsedTime:=A_TickCount - StartTime
        }
        this.WaitForTransition()
    }

	SetFormation(fastCheck:=false,useHighZone:=false) ;To be called with FastCheck during straightforward progression, e.g. not after stacking, falling back, other fun things. Note we can't always use highZone because when a zone completes, the highzone is momentarily this+1 before the jump applies - if we can find a good check for that we could move over. ReadTransitioning() would appear to work as a test - if true, can use highzone safely, if false then?
    {
		static trustRecent:=false ;Do we believe that the ReadMostRecentFormationFavorite() is respresentative? Needed as it changes even if the formation swap fails
		if (!fastCheck)
			trustRecent:=false ;Reset to false for all normal calls
		isEZone:=this.ShouldWalk(useHighZone ? g_SF.Memory.ReadHighestZone() : g_SF.Memory.ReadCurrentZone())
		Thread, NoTimers ;Here to handle the animation skip, maybe isn't needed for feat swap as a result?
		benchReturn:=this.BenchBrivConditions(isEZone) ;check to bench briv
		lastFormation:=g_SF.Memory.ReadMostRecentFormationFavorite() ;New Sep25 read, used in all cases as it is part of the bad formation check
        if (benchReturn AND lastFormation!=3) ;New Sep25 read. Formation 3 is E
        {
			this.KEY_E.KeyPress()
			if (benchReturn==2)
			{
				if (this.zones[g_SF.Memory.ReadHighestZone()].jumpZone) ;Only put Briv back in urgently if we need to jump right away. Note this does not have to consider featswap because we'll never enter this block with Briv in E, as we can't animation skip in that case
				{
					g_IBM.IBM_Sleep(15) ;Avoid swapping back instantly, given issues with multiple key presses
					startTime:=A_TickCount
					while (g_SF.Memory.ReadFormationTransitionDir()==4 AND !g_Heroes[58].ReadBenched() AND (A_TickCount-startTime)<1000) ;Whilst we're in the transition and Briv is still on the field
					{
						g_IBM.IBM_Sleep(15)
					}
					this.KEY_Q.KeyPress_Bulk() ;_Bulk as follows the E.KeyPress()
					while (g_SF.Memory.ReadFormationTransitionDir()==4 AND (A_TickCount-startTime)<1000) ;Having gone back to Q, wait for the transition to end (so we don't swap Briv straight back out again) TODO: We could block via a static variable or something instead of sleeping here? Not that transitions take overly long
					{
						g_IBM.IBM_Sleep(15)
					}
				}
			}
			Thread, NoTimers, False
			return
        }
		else
			Thread, NoTimers, False
		;check to unbench briv
        if (this.UnBenchBrivConditions(isEZone) AND lastFormation!=1) ;Formation 1 is Q
        {
			this.KEY_Q.KeyPress()
			return
        }
		if (trustRecent AND fastCheck)
		{
			if !(lastFormation==1 OR lastFormation==3)
				isEZone ? this.KEY_E.KeyPress() : this.KEY_Q.KeyPress()
		}
		else
		{
			if !(g_SF.IsCurrentFormation(g_IBM.levelManager.GetFormation("Q")) OR g_SF.IsCurrentFormation(g_IBM.levelManager.GetFormation("E")))
				isEZone ? this.KEY_E.KeyPress() : this.KEY_Q.KeyPress()
			else
				trustRecent:=true ;As we've checked we're on Q or E via formation read, we should be in normal progression
		}
    }

	 ;Should be benched based on game conditions. As part of drift checking, return as follows:
	 ;0 - as false before, do not bench
	 ;1 - as true before for most conditions, bench
	 ;2 - bench for animation override
    BenchBrivConditions(isEZone)
    {
		;ReadTransitionDirection() 		| 0 = Static (instant), 1 = Forward, 2 = Backward, 3=JumpDown, 4=FallDown
		;ReadFormationTransitionDir() 	| 0 = OnFromLeft, 1 = OnFromRight, 2 = OnFromTop, 3 = OffToLeft, 4 = OffToRight, 5 = OffToBottom
		if (this.zonesPerJumpE == 1 AND g_SF.Memory.ReadTransitionDirection() == 1 AND g_SF.Memory.ReadFormationTransitionDir() == 4 )
			return 2
        if (isEZone)
			return 1
        return 0
    }

    UnBenchBrivConditions(isEZone) ;True/False on whether Briv should be unbenched based on game conditions.
    {
        if (isEZone)
            return false
		if (this.zonesPerJumpE > 1) ;Don't do transition-based checks when feat swapping
			return true ;Not a walk zone so go to Q
		if (g_SF.Memory.ReadFormationTransitionDir()!=4) ;if transition direction is not "OffToRight"
			return true
        return false
    }

	ShouldWalk(zone)
	{
		return this.zones[zone].jumpZone==False
	}

	GetStandardFormationKey(zone) ;Returns the key object for Q or E as appropriate for the zone
	{
		if (this.ShouldWalk(zone))
			return this.Key_E
		return this.KEY_Q
	}

	GetStandardFormation(zone) ;Returns Q or E formation from the level manager as appropriate for the zone
	{
		if (this.ShouldWalk(zone))
			return g_IBM.levelManager.GetFormation("E")
		return g_IBM.levelManager.GetFormation("Q")
	}

	LoadRoute() ;Once per script-run loading of the route
	{
		loop, % this.targetZone
		{
			if (!this.zones.hasKey(A_Index)) ;For most routes the majority will be calculated on the first iteration, with subsequent calls only populating a few zones until it meets the existing route
			{
				currentZone:=new IC_BrivMaster_RouteMaster_Zone_Class
				currentZone.z:=A_Index
				this.zones[A_Index]:=currentZone
				this.ProcessRoute(currentZone)
			}
		}
		;Pre-calculate the jumps, by looking at all the end nodes. This can be targetZone to targetZone + jump -1 (eg for 9J (10z/jump) to z1060, we can at most hit the reset by jumping from 1059 and hitting 1069)
		endZone:=this.targetZone
		while (endZone < this.targetZone + this.zonesPerJumpQ AND endZone <= this.zoneCap+1) ;Less than due to the above
		{
			if (this.zones.hasKey(endZone)) ;We can only jump beyond the reset, not walk, so not every zone in the range will be hittable (eg for 1069 above with 9J, 1059 must be a jump)
			{
				;OutputDebug % "`nendZone recurse:" . endZone . "`n"
				this.JumpsRecurse(this.zones[endZone],0) ;When combining we include the jump with Thellora as a baseline. This is so measuring to the thelloraTarget gives the true number of jumps (and jumps before Thellora don't have meaning)
			}
			endZone++
		}
	}

	JumpsRecurse(currentZone, startingJumps) ;Calculates the number of jumps from z1 to currentZone. TODO: Doing this by recursion seems to cause problems sometimes, do it in a fixed loop?
	{
		for _,inZone in currentZone.incomingZones
		{
			jumpsDone:=startingJumps
			if (inZone.jumpZone) ;jump on Q
			{
				jumpsDone++
				if (inZone.jumpsToFinish:=-1) ;Not yet processed
				{
					inZone.jumpsToFinish:=jumpsDone
					inZone.stacksToFinish:=this.jumpCosts[jumpsDone] ;Currently assuming Metalborn always
				}
			}
			else ;walk on E, or with feat swap jump on E
			{
				if (this.IsFeatSwap())
					jumpsDone++
				if (inZone.jumpsToFinish:=-1) ;Not yet processed
				{
					inZone.jumpsToFinish:=jumpsDone
					inZone.stacksToFinish:=this.jumpCosts[jumpsDone] ;Currently assuming Metalborn always
				}
			}
			;OutputDebug % inZone.z . ","
			this.JumpsRecurse(inZone, jumpsDone)
		}
	}

	ProcessRoute(currentZone) ;currentZone is the zone we are starting the the calculation from
	{
		while (currentZone.z < this.targetZone) ;Less than as we can't proceed from the reset zone
		{
			typeIndex:=MOD(currentZone.z,50)
			if (typeIndex==0)
				typeIndex:=50 ;Deal with the array being 1-indexed
			currentZone.jumpZone:=g_IBM_Settings["IBM_Route_Zones_Jump"][typeIndex]==1
			currentZone.stackZone:=g_IBM_Settings[ "IBM_Route_Zones_Stack" ][typeIndex]==1
			if (currentZone.jumpZone) ;On Q
				nextZoneNumber:=currentZone.z+this.zonesPerJumpQ
			else
				nextZoneNumber:=currentZone.z+this.zonesPerJumpE
			if (this.zones.hasKey(nextZoneNumber)) ;Already processed, just link
			{
				currentZone.nextZone:=this.zones[nextZoneNumber] ;Set the next zone
				this.zones[nextZoneNumber].incomingZones[currentZone.z]:=currentZone ;Add to the incoming zones - TODO: Decide if this should be a simple or k,v Array
				break ;We've joined an existing route, so no further calculation required
			}
			else
			{
				nextZone:=new IC_BrivMaster_RouteMaster_Zone_Class
				nextZone.z:=nextZoneNumber
				nextZone.incomingZones[currentZone.z]:=currentZone
				currentZone.nextZone:=nextZone
				this.zones[nextZoneNumber]:=nextZone
			}
			currentZone:=nextZone
		}
	}

	BrivHasThunderStep() ;Thunder step 'Gain 20% More Sprint Stacks When Converted from Steelbones', feat 2131. TODO: This requires that the feat is saved, which you don't really want for non-featswap
	{
		If (g_SF.Memory.HeroHasAnyFeatsSavedInFormation(58, g_SF.Memory.GetSavedFormationSlotByFavorite(1)) or g_SF.Memory.IBM_HeroHasAnyFeatsSavedInFormation(58, g_SF.Memory.GetSavedFormationSlotByFavorite(3))) ;If there are feats saved in Q or E (which would overwrite any others in M)
		{
			thunderInQ:=g_SF.Memory.HeroHasFeatSavedInFormation(58, 2131, g_SF.Memory.GetSavedFormationSlotByFavorite(1))
			thunderInE:=g_SF.Memory.HeroHasFeatSavedInFormation(58, 2131, g_SF.Memory.GetSavedFormationSlotByFavorite(3))
			return (thunderInQ OR thunderInE)
		}
		else if (g_SF.Memory.HeroHasAnyFeatsSavedInFormation(58, g_SF.Memory.GetActiveModronFormationSaveSlot())) ;Briv has feats in M
			return g_SF.Memory.HeroHasFeatSavedInFormation(58, 2131 , g_SF.Memory.GetActiveModronFormationSaveSlot())
		else ;Non-feat swap might not have feats saved in formations at all
		{
			feats:=g_SF.Memory.GetHeroFeats(58)
			for k, v in feats
				if (v == 2131)
					return true
		}
		return false
	}

	; IsToggled is 0 for off or 1 for on. ForceToggle always hits G. ForceState will press G until AutoProgress is read as on (<5s).
    ToggleAutoProgress( isToggled := 1, forceToggle := false, forceState := false )
    {
        Critical, On
        StartTime:=A_TickCount
        if ( forceToggle )
            this.KEY_autoProgress.KeyPress()
        if ( g_SF.Memory.ReadAutoProgressToggled() != isToggled )
            this.KEY_autoProgress.KeyPress() ;Irisiri: If forceToggle is true, this will be a 2nd press without giving the game a chance to process?
        while ( g_SF.Memory.ReadAutoProgressToggled() != isToggled AND forceState AND A_TickCount - StartTime < 1000 )
        {
            this.KEY_autoProgress.KeyPress_Bulk()
			g_IBM.IBM_Sleep(15)
        }
        Critical, Off
    }

	StartAutoProgressSoft() ;Simplified autoprogress submission for optimising exit from stacking
	{
		if (g_SF.Memory.ReadAutoProgressToggled()!=1)
            this.KEY_autoProgress.KeyPress()
	}

	InitZone()
    {
        g_IBM.levelManager.LevelClickDamage()
        this.StartAutoProgressSoft()
        g_IBM.PreviousZoneStartTime:=A_TickCount
    }
}


class IC_BrivMaster_RouteMaster_Zone_Class ;A class representing a single zone
{
	z:=0
	nextZone:=""
	jumpZone:=false ;Jump or walk (or jump on Q vs jump on E for featswap)
	stackZone:=false ;Online stacking
	incomingZones:={} ;Zones which connect to this one (via walk or via jump), used to back-calculate costs
	jumpsToFinish:=-1
	stacksToFinish:=-1
}

class IC_BrivMaster_Relay_SharedData_Class ;Allows for communication between this main script and the Relay script
{
	/*
	States:
		0: Not running
		1: Main script has launched Relay
		2: Connected (Relay has accessed COM object)
		3: Game started
		4: Game started and Relay ended before platform login
		5: Game held after platform login
		6: Complete (any outcome)
		-1: Failed to launch
		-2: Failed to suspend (game will have started, current instance will be invalid)
	*/

	__New()
	{
		this.RelayZones:=g_IBM_Settings["IBM_OffLine_Blank_Relay_Zones"] ;Number of zones prior to the restart the relay should start TODO: Option for this
		this.MEMORY_baseAddress:=g_SF.Memory.GameManager.game.gameUser.Loaded.basePtr.ModuleOffset + 0 ;Memory structure data for the reads we need TODO: This has been changed from the whole address to the module offset, since if the module moves in a new process the base address for the old one is worthless... Maybe rename throughout
		this.MEMORY_LOADED_Type:=g_SF.Memory.GameManager.game.gameUser.Loaded.valueType
		offSets:=g_SF.Memory.GameManager.game.gameUser.Loaded.GetOffsets() ;We need to turn this into a SafeArray for access via COM
		offsetSize:=offSets.Count()
		ArrayObj := ComObjArray(12, offsetSize)
		loop %offsetSize%
			ArrayObj[A_Index-1]:=offSets[A_Index] ;Com Array is 0-indexed, vs AHK 1-indexed
		this.MEMORY_LOADED_Offsets:=ArrayObj
		this.LaunchCommand:=g_IBM_Settings["IBM_Game_Launch"]
		this.HideLauncher:=g_IBM_Settings["IBM_Game_Hide_Launcher"]
		this.ExeName:=g_IBM_Settings["IBM_Game_Exe"]
		this.Reset()
	}

	Reset()
	{
		this.RelayPID:=0
		this.RelayHwnd:=0
		this.HelperPID:=0
		this.State:=0
		this.RelayZone:=""
		this.RequestRelease:=false
	}

	Start()
	{
		if (this.State==0)
		{
			this.RelayPID:=0 ;Make sure things are reset
			this.RelayHwnd:=0
			this.HelperPID:=0
			this.RelayZone:=""
			this.RequestRelease:=false
			this.MainPID:=g_IBM.GameMaster.PID
			this.MainHwnd:=g_IBM.GameMaster.Hwnd
			this.RestoreWindow:=g_SharedData.IBM_RestoreWindow_Enabled ;This can be changed at run time
			scriptLocation := A_LineFile . "\..\IC_BrivMaster_RouteMaster_Relay.ahk"
			guid:=this.GUID
			Run, %A_AhkPath% "%scriptLocation%" "%guid%",,,helperPID
			g_IBM.Logger.AddMessage("Relay Start() ran helper script at z=[" . g_SF.Memory.ReadCurrentZone() . "] with PID=[" . helperPID . "]")
			this.HelperPID:=helperPID
			this.State:=1
		}
	}

	IsActive() ;Currently running
	{
		return this.State!=0 AND this.State!=6 ;Any any state but unstarted and complete
	}

	HasTriggered() ;Has been activated this run
	{
		return this.State!=0
	}

	PreRelease() ;Resume the process ASAP
	{
		if (this.State==5) ;Expected state, just resume process and move on
		{
			g_IBM.GameMaster.SuspendProcess(this.RelayPID,False)
			g_IBM.Logger.AddMessage("Relay PreRelease() state 5 - resuming")
		}
		else if (this.State==6) ;DEBUG: Relay is in a complete state. This might be possible during relay run recovery? TODO: This can be called when a second CloseIC() is called after the relay handover, e.g. because the run gets stuck
		{
			g_IBM.GameMaster.SuspendProcess(this.RelayPID,False)
			g_IBM.Logger.AddMessage("Relay PreRelease() state 6 - resuming - DEBUG")
		}
		else if (this.State>0) ;Request release
		{
			this.RequestRelease:=true
			g_IBM.Logger.AddMessage("Relay PreRelease() state 1 to 4 - request release")
		}
	}

	Release()
	{
		if (this.State==5) ;Expected state, just resume process and move on
		{
			g_IBM.GameMaster.SuspendProcess(this.RelayPID,False)
			this.ProcessSwap()
			g_IBM.Logger.AddMessage("Relay Release() state 5")
			this.State:=6 ;Complete
			return
		}
		else if (this.State==4) ;Never suspended, either because the relay missed the login, or because the main script asked the relay to abort via RequestRelease
		{
			this.ProcessSwap()
			g_IBM.Logger.AddMessage("Relay Release() state 4")
			this.State:=6 ;Complete
			return
		}
		else if (this.State==3 OR this.State==2) ;Relay started (2), and maybe started the game (3) but has yet to suspend it, in this case we need to take care that we don't get stuck by a race condition with the Relay suspending the process after we set RequestRequest:=true, but before it is read through the COM object
		{
			this.RequestRelease:=true
			g_IBM.Logger.AddMessage("Relay Release() state [" . this.State . "]")
			maxTime:=A_TickCount + 5000 ;Time for the relay to finish opening the game if necessary
			while (A_TickCount < maxTime)
			{
				if (this.State!=3 AND this.State!=2) ;Once the state changes re-call
				{
					g_IBM.Logger.AddMessage("Relay Release() state changed to [" . this.State . "] - recursing Release()")
					this.Release()
				}
				g_IBM.IBM_Sleep(15)
			}
			g_IBM.Logger.AddMessage("Relay Release() state [" . this.State . "] recursion exit or failed to detect state change")
			this.CleanUpOnFail()
		}
		else if (this.State==1) ;Relay never connected
		{
			g_IBM.Logger.AddMessage("Relay Release() state [" . this.State . "]")
			this.CleanUpOnFail()
		}
		else if (this.State==0 OR this.State==-1) ;We never actually started the relay, or it failed to start the game
		{
			g_IBM.Logger.AddMessage("Relay Release() state [" . this.State . "]")
			this.CleanUpOnFail()
		}
		else if (this.State==-2) ;Relay failed to stop the game after platform login, game should have been closed via RelayCloseMain() already
		{
			g_IBM.Logger.AddMessage("Relay Release() state [" . this.State . "]")
			this.ProcessSwap()
		}
		else
			g_IBM.Logger.AddMessage("Relay Release() with invalid state [" . this.State . "]")
		this.State:=6 ;Complete
	}

	LogZone(message) ;DEBUG - remove later?
	{
		g_IBM.Logger.AddMessage("Relay LogZone() at z[" . g_SF.Memory.ReadCurrentZone() . "] message=[" . message . "]")
	}

	CleanUpOnFail()
	{
		if (g_SF.GetProcessName(this.HelperPID) == "AutoHotkey.exe") ;Kill the relay script
		{
			g_IBM.Logger.AddMessage("CleanUpOnFail() found Relay AHK script PID=[" . this.HelperPID . "] still running - killing")
			closeString:="ahk_pid " . this.HelperPID
			WinKill, %closeString% ;TODO: Should this use GameMaster.TerminateProcess?
		}
		WinGet, recoveryPID, PID, % "ahk_exe " . g_IBM_Settings["IBM_Game_Exe"] ;Check for IC processes
		if (recoveryPID)
		{
			g_IBM.Logger.AddMessage("CleanUpOnFail() recovery PID found=[" . recoveryPID . "]")
			g_IBM.GameMaster.PID:=recoveryPID
			g_IBM.GameMaster.SuspendProcess(g_IBM.GameMaster.PID,False) ;Ensure the process is not stuck suspended
			g_IBM.GameMaster.Hwnd:=WinExist("ahk_pid " . recoveryPID)
			g_SF.Memory.OpenProcessReader(recoveryPID) ;Open this PID specifically
			g_SF.ResetServerCall()
		}
		else ;Otherwise open as normal
		{
			g_IBM.GameMaster.OpenIC("CleanUpOnFail()")
		}
	}

	ProcessSwap()
	{
		logText:="ProcessSwap() changing PID=[" . g_IBM.GameMaster.PID . "] and Hwnd=[" . g_IBM.GameMaster.Hwnd . "] "
		g_IBM.GameMaster.PID:=this.RelayPID
		g_IBM.GameMaster.Hwnd:=this.RelayHwnd
		g_IBM.Logger.AddMessage(logText . "to PID=[" . g_IBM.GameMaster.PID . "] and Hwnd=[" . g_IBM.GameMaster.Hwnd . "]")
		g_SF.Memory.OpenProcessReader(g_IBM.GameMaster.PID)
		if (g_IBM.GameMaster.WaitForGameReady(10000*g_IBM_Settings["IBM_OffLine_Timeout"],true)) ;Default is 5, so 50s. Call WaitForGameReady() with skipFinal:=true as we won't know where in the offline calc we are if we happen to trigger one
			g_IBM.Logger.AddMessage("ProcessSwap() completed switching process")
		else
			g_IBM.Logger.AddMessage("ProcessSwap() WaitForGameReady() call failed whilst switching process")
		g_SF.ResetServerCall()
		g_SharedData.UpdateOutbound("IBM_ProcessSwap",true) ;Allows the hub to react
	}

	RelayCloseMain() ;Called from the Relay script via COM to close the main IC process during recovery
	{
		g_IBM.GameMaster.CloseIC("Relay failed to halt at platform login",true) ;Close via PID
		this.Release()
	}

	GetRelayZone(restartZone,routeMaster)
	{
		if (this.RelayZone) ;Use cache
			return this.RelayZone
		relayZone:=restartZone - this.RelayZones
		if (g_IBM_Settings["IBM_Online_Use_Melf"]) ;Online with melf - try to avoid starting the game as we're online stacking
		{
			melfRange:=routeMaster.MelfManager.GetFirstMelfSpawnMoreRange() ;TODO: Fix 'this' to a levelmanager reference
			if (melfRange AND melfRange[1] > relayZone AND melfRange[1] < relayZone + this.RelayZones) ;If the target online stack zone is at the start of the blank range
				relayZone:=melfRange[1] - this.RelayZones ;Move the relay zone ahead
		}
		this.RelayZone:=MAX(relayZone,routeMaster.thelloraTarget) ;Do not try and relay restart until after Thellora's jump (which will generally have the casino)
		return this.RelayZone
	}
}

class IC_BrivMaster_BrivBoost_Class ;A class used to work out what level Briv needs to be to survive on a given zone
{

	__New(targetMulti)
	{
		this.BuildBrivLevelTable(130,{70:95,180:165,265:290,340:510,455:890,575:1560,695:2730,815:4775,935:7800,1050:14200,1170:24000,1300:42500})
		this.ZoneCache:={} ;Store results so we don't have to recalculate the same zone again
		this.DPSGrowthRateCurve:=g_SF.Memory.IBM_ReadDPSGrowthCurve()
		if (this.DPSGrowthRateCurve.Count()==0)
		{
			MSGBOX Briv Boost failed to read the DPS growth rate curve at adventure start. If this error persists please disable Briv Boost
			ExitApp
		}
		this.areaAndCampaignMonsterDamageMultiplier:=g_SF.Memory.IBM_ReadAreaMonsterDamageMultiplier()*g_SF.Memory.IBM_ReadCampaignMonsterDamageMultiplier()
		this.monsterBaseDPS:=g_SF.Memory.IBM_ReadMonsterBaseDPS()
		this.maxMonsters:=100
		this.overwhelmAdditivePenalty:=0.1
		this.targetMultiplier:=targetMulti ;If we exactly matched Briv's HP to enemy damage he would be one-shot as soon as we reached 100 enemies attaching . This factor allows us to survive that and some enrage. 8 seems to be good for a fast stack, might need a bit more for long ones
	}

	Apply()
	{
		currentLevel:=g_Heroes[58].ReadLevel()
		if (!currentLevel) ;If Briv is somehow unlevelled
			currentLevel:=0
		targetLevel:=this.GetBrivBoostTargetLevel(g_SF.Memory.ReadHighestZone(),currentLevel)
		if(targetLevel > currentLevel)
		{
			g_IBM.levelManager.OverrideLevelByIDRaiseToMin(58, "min", targetLevel)
			g_IBM.Logger.AddMessage("BrivBoost{C=" . currentLevel . " T=" . targetLevel . "}")
		}
	}

	GetBrivBoostTargetLevel(zone,currentLevel) ;This is the main function to be called when using this class
	{
		if (!this.ZoneCache.HasKey(zone))
			this.ZoneCache[zone]:=this.GetPreFlamesDamage(zone)
		flamesAdjusted:=this.ZoneCache[zone]*(2**g_Heroes[83].GetNumFlamesCards())
		brivHPMultiplier:=g_Heroes[58].ReadMaxHealth() / this.GetBrivBaseHPforLevel(currentLevel)
		targetBrivLevel:=this.GetBrivLevelForBaseHP(flamesAdjusted/brivHPMultiplier)
		targetBrivLevel100:=CEIL(targetBrivLevel/100)*100 ;Adjust for x100 levelling
		return targetBrivLevel100
	}

	GetPreFlamesDamage(zone) ;This takes the curve, area/campaign, monster totals and overwhelm into account (overwhelm should not change as we only check on W). It does not take Flames into account as that will vary
	{
		damage:=this.GetCurveValue(zone) ;Mimcing ComputeMonsterAttackDPS
		damage*=this.areaAndCampaignMonsterDamageMultiplier
		damage*=this.maxMonsters ;Monster count
		damage*=1+Max(this.maxMonsters-g_Heroes[58].ReadOverwhelm(),0)*this.overwhelmAdditivePenalty ;Overwhelm
		damage*=this.targetMultiplier ;HP margin factor
		return damage
	}

	GetBrivLevelForBaseHP(baseHP)
	{
		for level, HP in this.BrivLevelTable
		{
			if (HP>=baseHP)
				return level
			maxlevel:=level
		}
		return maxlevel
	}

	GetBrivBaseHPforLevel(brivLevel)
	{
		for level, HP in this.BrivLevelTable
		{
			if (level<=brivLevel)
				lastHP:=HP
			else
				break
		}
		return lastHP
	}

	GetCurveValue(index)
	{
		result:=this.monsterBaseDPS
		Loop % this.DPSGrowthRateCurve.Count()
		{
			i:=A_Index
			if (this.DPSGrowthRateCurve[i].level>index)
				break
			value:=this.DPSGrowthRateCurve[i].value
			num:=(i!=this.DPSGrowthRateCurve.Count AND index > this.DPSGrowthRateCurve[i+1].level) ? this.DPSGrowthRateCurve[i+1].level - this.DPSGrowthRateCurve[i].level : index - this.DPSGrowthRateCurve[i].level ;Apply for zones from either the next data point, or from the current zone
			result*=value**num
		}
        return result
	}

	BuildBrivLevelTable(baseHP,upgradeList) ;Produce a table of Level:Total HP so we don't have to step through upgrades all the time TODO: This should build from Defs
	{
		level:=1
		HP:=baseHP
		this.BrivLevelTable:={}
		this.BrivLevelTable[level]:=HP
		for uLevel, uHP in upgradeList
		{
			level:=uLevel
			HP+=uHP
			this.BrivLevelTable[level]:=HP
		}
	}

	;Below is code for reading upgrades for reference
	/*
	DEBUG_UpgradeList()
	{
		heroIndex:=g_SF.Memory.GetHeroHandlerIndexByChampID(58) ;Legacy, this probably becomes part of the hero object?
		;size:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId.size.Read()
		size := g_SF.Memory.ReadHeroUpgradesSize(58) ;Would need replacing as removed, probably becomes part of the hero object?
		upgradeList:={}
		Loop, %size%
        {
			id:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId["value",A_Index-1].Id.Read()
            ;OutputDebug % "Calling g_SF.Memory.IBM_ReadHeroUpgradeRequiredLevelByIndex`n" ;Note - removed, take from IC Core if needed
			level:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId[id].RequiredLevel.Read()
			effectString:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId[id].Def.baseEffectString.Read()
			effectSplit:=StrSplit(effectString,",")
            if (effectSplit[1]=="health_add")
			{
				upgradeList[A_Index]:={}
				upgradeList[A_Index].id:=id
				upgradeList[A_Index].level:=level
				upgradeList[A_Index].health:=effectSplit[2]
			}
        }
        return upgradeList
	}
	*/

}

class IC_BrivMaster_MelfMaster_Class ;A class for tracking Melf's buffs
{
	Patterns:={} ;Stores a breakdown of Melf's buff types by reset #, array of buff number for each block of 50 (aka Segment)
	NextSpawnMore:={} ;Stores for each segment the next segment with the spawn-more buff
	NextSpawnFaster:={} ;As above, but for the spawn-faster buff for fallback
	lookahead:=5 ;Number of Melf runs to calculate ahead of the current run
	minZone:=1 ;Min online stack zone
	maxZone:=2500 ;Max online stack zone
	zoneCap:=2500 ;This is the reset zone

	__New(zoneCap) ;Called with the zone cap to avoid duplicating it everywhere
	{
		this.zoneCap:=zoneCap
	}

	Reset(minZone,maxZone,lookahead) ;To be called once per run at the start, this deletes old patterns and handles possible changes of settings TODO: We currently support changing the min/max stack zone at runtime, which seems unnecessary? Likewise the lookhead is hard-coded
	{
		curReset:=g_SF.Memory.ReadResetsTotal()
		removeAll:=(minZone!=this.minZone OR maxZone!=this.maxZone) ;if either change the NextSpawnMore segment field needs to be recalculated. We only need to do that part so removing everything is overkill, but we shouldn't be changing these mid-run with any frequency
		this.minZone:=minZone
		this.maxZone:=maxZone
		this.lookhead:=lookahead
		for reset, _ in this.melfPatterns
		{
			if (removeAll OR reset < curReset)
				this.Patterns.Delete(reset)
				this.NextSpawnMore.Delete(reset)
				this.NextSpawnFaster.Delete(reset)
		}
		this.Update(curReset)
	}

	Update(curReset:="") ;Generates patterns
	{
		if (curReset=="")
			curReset:=g_SF.Memory.ReadResetsTotal()
		;Calculate this and any needed future value
		loop, % this.lookahead + 1
		{
			reset:=curReset + A_INDEX - 1
			minSegment:=this.ZoneToSegment(this.minZone)
			maxSegment:=this.ZoneToSegment(this.maxZone)
			if (!this.Patterns.HasKey(reset))
			{
				this.Patterns[reset] := []
				this.NextSpawnMore[reset] := []
				this.NextSpawnFaster[reset] := []
				rng := new CSharpRNG(reset * 10)
				segments := Ceil(this.zoneCap / 50)
				Loop, % segments
					this.Patterns[reset,A_Index] := rng.NextRange(0, 3)
				index:=segments ;Now we iterate backwards to fill NextSpawnMore / Next Spawn Faster
				lastSpawnMore:=0 ;For false / none
				lastSpawnFaster:=0
				Loop
				{
					If (index <= maxSegment AND index>=minSegment) ;If this is in range
					{
						If (this.Patterns[reset,index] == 0) ;...and spawning more
						{
							this.NextSpawnMore[reset,index]:=index
							lastSpawnMore:=index
						}
						Else
						{
							this.NextSpawnMore[reset,index]:=lastSpawnMore
						}
						If (this.Patterns[reset,index] == 1) ;...and spawning faster
						{
							this.NextSpawnFaster[reset,index]:=index
							lastSpawnFaster:=index
						}
						Else
						{
							this.NextSpawnFaster[reset,index]:=lastSpawnFaster
						}
					}
					index--
				} Until (index < 1)
			}
		}
	}

	CheckReset(reset) ;Calculates data for the current reset if needed, e.g. because of background party updates and a small lookahead
	{
		if (!this.Patterns.HasKey(reset)) ;If the reset is not in the data we need to calculate it
			this.Update(reset)
	}

	GetCurrentMelfEffect(zone:="") ;0 is spawn amount, 1 is spawn speed, 2 is quest drops
	{
		if (zone=="")
			zone:=g_SF.Memory.ReadCurrentZone()
		reset:=g_SF.Memory.ReadResetsTotal()
		this.CheckReset(reset) ;Ensure we have data for the current reset
		return this.Patterns[reset,this.ZoneToSegment(zone)]
	}

	IsMelfEffectSpawnMore(zone:="")
	{
		return this.GetCurrentMelfEffect(zone)==0
	}

	IsMelfEffectSpawnFaster(zone:="")
	{
		return this.GetCurrentMelfEffect(zone)==1
	}

	SegmentToZonePair(segment)
	{
		if (segment==0) ;Segment 0 means no range found
			return False
		lastZone:=segment*50
		return [lastZone-49,lastZone]
	}

	ZoneToSegment(zone)
	{
		return ceil(zone/50)
	}

	GetFirstMelfSpawnMoreSegment(curZone:="") ;If a zone is supplied, the segment at or after that will be returned instead of the minimum
	{
		reset:=g_SF.Memory.ReadResetsTotal()
		this.CheckReset(reset) ;Ensure we have data for the current reset
		if (curZone=="")
			startZone:=this.minZone
		else
			startZone:=max(curZone,this.minZone) ;Use the highest of the two
		segment:=this.ZoneToSegment(startZone)
		return this.NextSpawnMore[reset,segment]
	}

	GetFirstMelfSpawnFasterSegment(curZone:="") ;If a zone is supplied, the segment at or after that will be returned instead of the minimum
	{
		reset:=g_SF.Memory.ReadResetsTotal()
		this.CheckReset(reset) ;Ensure we have data for the current reset
		if (curZone=="")
			startZone:=this.minZone
		else
			startZone:=max(curZone,this.minZone) ;Use the highest of the two
		segment:=this.ZoneToSegment(startZone)
		return this.NextSpawnFaster[reset,segment]
	}

	GetFirstMelfSpawnMoreRange(curZone:="") ;Returns a range as a simple array eg [401,450], or false/0 if no range exists
	{
		return this.SegmentToZonePair(this.GetFirstMelfSpawnMoreSegment(curZone))
	}

	GetFirstMelfSpawnFasterRange(curZone:="") ;Returns a range as a simple array eg [401,450], or false/0 if no range exists
	{
		return this.SegmentToZonePair(this.GetFirstMelfSpawnFasterSegment(curZone))
	}

	GetFirstMelfSpawnMoreRangeString(curZone:="") ;Returns a range as a string, eg 401-450, or None if no range exists
	{
		segment:=this.GetFirstMelfSpawnMoreSegment(curZone)
		if (segment)
			return segment[1] . "-" . segment[2]
		return "None"
	}

	GetFirstMelfSpawnFasterRangeString(curZone:="") ;Returns a range as a string, eg 401-450, or None if no range exists
	{
		segment:=this.GetFirstMelfSpawnFasterSegment(curZone)
		if (segment)
			return segment[1] . "-" . segment[2]
		return "None"
	}
}