#include %A_LineFile%\..\..\..\SharedFunctions\CSharpRNG.ahk ;Used for Melf things

class IC_BrivMaster_RouteMaster_Class ;A class for managing routes
{
	zoneCap:=2501
	zones:={}
	thelloraCap:=1 ;The ablity cap, so at 300 favour, 300
	thelloraDirty:=true ;True if we have not actually read Thellora's data this run, due to her not being deployed yet
	leftoverCalculated:=false ;True once this has been calculated - has to be done after Thellora has been fielded
	leftoverHaste:=48
	cycleCount:=0 ;Counts the number of runs since the last game restart
	cycleMax:=1 ;Maximum runs per offline
	cycleForceOffline:=false ;Stack offline in all cases
	cycleDisableOffline:=false ;Stack online is all cases
	offlineSaveTime:=-1 ;Tracks the offline start time so it can be accessed globally
	;Below has to be a string because array literals can't be this long. Going to 400 jumps is a bit overkill
	static IRI_BRIVMASTER_JUMPCOST_METALBORN := "50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,81,84,87,90,93,96,99,102,105,108,112,116,120,124,128,132,136,140,145,150,155,160,165,170,176,182,188,194,200,207,214,221,228,236,244,252,260,269,278,287,296,306,316,326,337,348,359,371,383,396,409,423,437,451,466,481,497,513,530,548,566,585,604,624,645,666,688,711,734,758,783,809,836,864,893,923,953,984,1017,1051,1086,1122,1159,1197,1237,1278,1320,1364,1409,1456,1504,1554,1605,1658,1713,1770,1828,1888,1950,2014,2081,2150,2221,2294,2370,2448,2529,2613,2699,2788,2880,2975,3073,3175,3280,3388,3500,3616,3736,3859,3987,4119,4255,4396,4541,4691,4846,5006,5171,5342,5519,5701,5889,6084,6285,6493,6708,6930,7159,7396,7640,7893,8154,8424,8702,8990,9287,9594,9911,10239,10577,10927,11288,11661,12046,12444,12855,13280,13719,14173,14642,15126,15626,16143,16677,17228,17798,18386,18994,19622,20271,20941,21633,22348,23087,23850,24638,25452,26293,27162,28060,28988,29946,30936,31959,33015,34106,35233,36398,37601,38844,40128,41455,42825,44241,45703,47214,48775,50387,52053,53774,55552,57388,59285,61245,63270,65362,67523,69755,72061,74443,76904,79446,82072,84785,87588,90483,93474,96564,99756,103054,106461,109980,113616,117372,121252,125260,129401,133679,138098,142663,147379,152251,157284,162483,167854,173403,179135,185057,191175,197495,204024,210769,217737,224935,232371,240053,247989,256187,264656,273405,282443,291780,301426,311390,321684,332318,343304,354653,366377,378489,391001,403927,417280,431074,445324,460045,475253,490964,507194,523961,541282,559176,577661,596757,616484,636864,657917,679666,702134,725345,749323,774094,799684,826120,853430,881643,910788,940897,972001,1004133,1037327,1071619,1107044,1143640,1181446,1220502,1260849,1302530,1345589,1390071,1436024,1483496,1532537,1583199,1635536,1689603,1745458,1803159,1862768,1924347,1987962,2053680,2121570,2191705,2264158,2339006,2416328,2496207,2578726,2663973,2752038,2843014,2936998,3034089,3134389,3238005,3345046,3455626,3569862,3687874,3809787,3935730,4065837,4200245,4339096,4482537,4630720,4783802,4941944,5105314,5274085,5448435,5628549,5814617,6006836,6205409,6410546,6622465,6841389,7067551,7301189,7542551,7791892,8049475,8315573,8590468,8874450,9167820,9470888,9783975,10107412,10441541,10786716,11143302,11511676,11892227,12285358,12691486,13111039,13544462,13992213,14454765,14932608,15426248,15936207,16463024,17007256,17569479,18150288,18750298,19370143,20010478,20671981,21355352"

	__New(combine,logBase)
	{
		this.combining:=combine
		this.thelloraTarget:=this.GetThelloraTarget(1,this.combining) ;Set as a default before we can get our first memory read, as that is only possible once Thellora is fielded
		this.zonesPerJumpQ:=g_IBM_Settings["IBM_Route_BrivJump_Q"] + 1 ; We want the actual number of zones so adding 1 here, eg 9 jump goes from z1 to z11, so covers 10 zones (because it's the normal +1 progress plus the 9)
		if (g_IBM.levelManager.IsChampInFormation(58, "E")) ;Feat swap, ignored if Briv is not saved in E
			this.zonesPerJumpE:=g_IBM_Settings["IBM_Route_BrivJump_E"] + 1 ;As above
		else
			this.zonesPerJumpE:=1 ;Walking progresses 1 zone per 'jump'
		this.zonesPerJumpM:=g_IBM_Settings["IBM_Route_BrivJump_M"] + 1 ;Used when combining
		this.targetZone:=g_SF.Memory.GetModronResetArea()
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
			revokeFunc := ObjBindMethod(this, "RelayComObjectRevoke")
			OnExit(revokeFunc)
		}
		this.UltraStacking:=g_IBM_Settings["IBM_Online_Ultra_Enabled"]
		PH_OPTION_USE_FARIDEH_ULT_FOR_STACK_NORMAL:=false
		this.useFaridehUlt:=PH_OPTION_USE_FARIDEH_ULT_FOR_STACK_NORMAL
		if (this.UltraStacking OR this.useFaridehUlt) ;Both need to check BUD
		{
			this.BUDTracker:= new IC_BrivMaster_BUD_Tracker_Class()
		}
		this.useBrivBoost:=g_IBM_Settings["IBM_LevelManager_Boost_Use"]
		if (this.useBrivBoost)
			this.BrivBoost:=new IC_BrivMaster_BrivBoost_Class(g_IBM_Settings["IBM_LevelManager_Boost_Multi"])
		
		this.UpdateThellora() ;Here so that she is read in at script start. Not in Reset() as her handler won't be available then
		this.CombineModeThelloraBossAvoidance:=g_IBM_Settings["IBM_Route_Combine_Boss_Avoidance"] ;Should we try to avoid combining into a boss by delaying the combine?
		g_SharedData.IBM_UpdateOutbound("IBM_RestoreWindow_Enabled",g_IBM_Settings["IBM_Route_Offline_Restore_Window"])
		g_SharedData.IBM_UpdateOutbound("IBM_RunControl_DisableOffline",false) ;Default to off
		g_SharedData.IBM_UpdateOutbound("IBM_RunControl_ForceOffline",false) ;Default to off
		this.LastSafeStackZone:=this.GetLastSafeStackZone() ;No reason to re-calcuate this every zone
		g_SharedData.IBM_UpdateOutbound("IBM_ProcessSwap",false) ;Allows the hub to detect process changes on restarts prompty
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
		if (g_IBM_Settings[ "IBM_Online_Use_Melf"])
		{
			this.UpdateMelfPatterns(true) ;Calling with (true) cleans up old data from this call; no need to do that regularly
			this.MelfManager.Reset(g_IBM_Settings["IBM_Online_Melf_Min"],g_IBM_Settings["IBM_Online_Melf_Max"],5) ;TODO: Setting for lookahead
		}
		;Only process Run Control input from the hub at the start of a run, as changing mid-run could make a mess
		this.cycleDisableOffline:=g_SharedData.IBM_RunControl_DisableOffline
		if (g_SharedData.IBM_RunControl_ForceOffline)
		{
			this.cycleForceOffline:=true ;Queue
			g_SharedData.IBM_UpdateOutbound("IBM_RunControl_ForceOffline",false) ;Clear as this is a one-off
		}
		if (this.RelayBlankOffline)
		{
			this.RelayData.Reset()
		}
		thelloraDirty:=true ;Cannot re-read her data yet as won't be fielded
		this.UpdateStatusString()
		this.SetInitialStackString()
		g_SharedData.IBM_UpdateOutbound("IBM_ProcessSwap",false)
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
		targetStacks:=this.GetTargetStacks(true)
		g_SharedData.IBM_UpdateOutbound("IBM_RunControl_CycleString","Cycle " . this.cycleCount . "/" . this.cycleMax . (this.cycleForceOffline ? " FO" : ""))
		g_SharedData.IBM_UpdateOutbound("IBM_RunControl_StatusString","Strategy: " . (this.combining ? "Combining" : "Non-combined") . " to z" . this.thelloraTarget . ", using " . targetStacks . " stacks (stacking " . (this.stackConversionRate!=1 ? CEIL((targetStacks-48)/this.stackConversionRate) . " w/TS" : targetStacks-48) . ") @" . this.zonesPerJumpQ . (this.zonesPerJumpE>1 ? "&&" . this.zonesPerJumpE : "") . "z/J to z" . this.targetZone)
	}

	SetInitialStackString() ;Return the pre-stacking intent, ie on/offline and zone TODO: This doesn't know about blanks
	{
		if (this.ShouldOfflineStack()) ;Offline
		{
			if (g_IBM_Settings[ "IBM_OffLine_Flames_Use"])
				stackString:="Stacking: Expecting offline at z" . this.GetStackZone() . " (subject to Flames-based adjustment)"
			else
				stackString:="Stacking: Expecting offline at z" . this.GetStackZone()
		}
		else
		{
			if (g_IBM_Settings[ "IBM_Online_Use_Melf"]) ;Online with melf
			{
				melfRange:=this.MelfManager.GetFirstMelfSpawnMoreRange()
				if (melfRange)
					stackString:="Stacking: Expecting online with Melf in range z" . melfRange[1] . " to z" . melfRange[2]
				else
					stackString:="Stacking: Expecting online with Melf at z" . g_IBM_Settings[ "IBM_Online_Melf_Min" ] . " (no spawn more segment available)"
			}
			else
			{
				stackString:="Stacking: Expecting online at z" . g_IBM_Settings["IBM_Offline_Stack_Min"]
			}
			if (this.ShouldBlankRestart())
			{
				if (this.RelayBlankOffline)
					stackString.=" with relay blank restart"
				else
					stackString.=" with blank restart"
			}
		}
		g_SharedData.IBM_UpdateOutbound("IBM_RunControl_StackString",stackString)
	}

	NeedToStack() ;Is stacking this run required, i.e. do we have less Steelbones than needed for the *next* run
	{
		return g_SF.Memory.ReadSBStacks() < this.GetTargetStacks()
	}

	GetTargetStacks(ignoreHaste:=false, forceRecalc:=false) ;Number of Steelbones stacks needed for the next run
	{
		if ignoreHaste
			return this.GetTargetStacksForFullRun()
		else
		{
			expectedNextRush:=this.UpdateLeftoverHaste(forceRecalc) ;Returns the number of Thellora rush stacks expected
			stacksToGenerate:=this.GetTargetStacksForFullRun(expectedNextRush) - this.leftoverHaste
			return CEIL(stacksToGenerate / this.stackConversionRate) ;Ceiling as the feat rounds down
		}
	}

	UpdateThellora() ;TODO: Encapsulate the cap/dirty part of this in the Hero object? The target is route based and should probably stay here
	{
		if (this.thelloraDirty)
		{
			cap:=g_Heroes[139].ReadRushTarget()
			if (cap)
			{
				this.thelloraCap:=cap
				this.thelloraTarget:=this.GetThelloraTarget(this.thelloraCap,this.combining)
				this.thelloraDirty:=false
			}
		}
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

	CheckThelloraBossRecovery() ;If option set, avoid Thellora combining into bosses are a run that didn't complete by breaking the combine
	{
		if(this.CombineModeThelloraBossAvoidance AND this.combining)
		{
			thelloraChargesUncapped:=FLOOR(g_Heroes[139].ReadRushAreaCharges()) ;Floor as the part-charges are presented as decimals, eg 307.2 = 307 zones plus 20% of the way to another
			thelloraCharges:=MIN(thelloraChargesUncapped,this.thelloraCap) ;Cap
			rushTargetCombining:=this.GetThelloraTarget(thelloraCharges,true) ;TODO: These have been split out for debug reasons, can be streamlined later
			rushTargetNonCombining:=this.GetThelloraTarget(thelloraCharges,false)
			if (rushTargetCombining < this.thelloraTarget AND MOD(rushTargetCombining,5)==0 AND MOD(rushTargetNonCombining,5)!=0) ;If we are short on stacks and going to hit a boss, and not combining will land us on anything but a boss. Note that as this is run at the very start of the script, this.thelloraTarget
			{
				g_IBM.levelManager.OverrideLevelByID(58,"z1c", true) ;Prevent Briv being levelled prior to completion of z1, breaking the combine
				g_IBM.Logger.AddMessage("CTBR: Broke combine to avoid hitting boss")
			}
		}
	}

	GetTargetStacksForFullRun(expectedNextRush:=false) ;Returns the expected total stacks for a full run
	{
		if (expectedNextRush)
			thelloraTarget:=this.GetThelloraTarget(expectedNextRush,this.combining)
		else
			thelloraTarget:=this.thelloraTarget
		jumps:=this.zones[thelloraTarget].jumpsToFinish
		if (this.combining) ;We need to do one jump to reach ThelloraTarget in this case
		{
			jumps++
			if (expectedNextRush AND this.CombineModeThelloraBossAvoidance AND this.IsFeatSwap() AND this.zonesPerJumpM > this.zonesPerJumpE) ;If Thellora won't reach her target, we have boss recovery on, we are using feat swapping and the M jump would have been larger than an E jump, we need to generate an additional jump's worth of stacks, as replacing an M with an E would result in us needing 1 more jump Note: As this is a recovery mode trying to work out if the jump being replaced is Q or E doesn't seem worthwhile (it's made complex by her erratic behaviour if not in W)
			{
				jumps++
				g_IBM.Logger.AddThelloraCompensationMessage("GetTargetStacksForFullRun: Added extra jump for Thellora recovery for a total of: ",jumps)
			}
		}
		return this.jumpCosts[jumps]
	}

	UpdateLeftoverHaste(forceRecalc:=false) ;Returns 0, or if Thellora won't make her target then it returns her skip number (NOT target)
	{
		if (this.leftoverCalculated AND !forceRecalc)
			return 0
		else
		{
			returnValue:=0
			this.UpdateThellora() ;Ensure we have her cap read (mostly impacts the first run)
			calcResult:=this.UpdateLeftoverHaste_Calculate()
			this.leftoverHaste:=calcResult.haste
			;TODO: Consider changing the below to deal in charges not zones, it's rather confusing at the moment doing 5x then /5...
			thelloraNeedsZones:=this.thelloraCap * 5 + (this.combining ? 0 : 1) ;If not combining Thellora will not get credit for z1. Note we can't use this.ThelloraTarget as that includes a possible combined jump and the +1. TODO: Check for her presence in W here?
			currentChargesInZones:=g_Heroes[139].ReadRushAreaCharges() * 5 ;The memory read will for example be 50.2 for 50 zones and 1 of 5 towards the next, so with the x5 will be 251 in this example
			thelloraNeedsAdditional:=MAX(0,thelloraNeedsZones-currentChargesInZones)
			if (calcResult.partialRun) ;We can't make the end of this run and will reset early. We need to work out if we need to get extra stacks to make up for Thellora's rush shortfall in the next run
				zonesRemaining:=MAX(0,this.GetStackDepletionZone(calcResult.zone,calcResult.jumpsToDepletion)-calcResult.zone)
			else
				zonesRemaining:=MAX(0,this.targetZone-calcResult.zone)
			if (zonesRemaining < thelloraNeedsAdditional)
			{
				returnValue:=FLOOR((currentChargesInZones + zonesRemaining) / 5) ;Number of charges she will have. Note the floor is required as this will be used as an array index and must be an INT as a result. The // operator returns a float because AHK is dumb. TODO: Like most the Thellora code, should read the feat
			}
			if (g_SF.Memory.ReadHighestZone() >= this.thelloraTarget) ;If we've calculated post-Thellora, don't do so again - whilst technically we could reduce jumps by drifting that is not something we plan to do!
				this.leftoverCalculated:=true
			return returnValue
		}
	}

	GetStackDepletionZone(zoneNumber, jumps)
	{
		while (jumps > 0)
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
				if (this.zonesPerJumpE > 1) ;If Briv is in E this also costs a jump
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
		calcResult.haste:=g_SF.Memory.ReadHasteStacks()
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
        while jumps > 0
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
		return g_SF.Memory.ReadHasteStacks() >= this.zones[zone].stacksToFinish
	}

	GetStackZone() ;Dynamic to allow the Ellywick Flames card based option
    {
        If (g_IBM_Settings["IBM_Online_Use_Melf"] AND !this.ShouldOfflineStack()) ;Melf online is enabled and we shouldn't offline stack
			stackZone:=g_IBM_Settings["IBM_Online_Melf_Min"] ;Melf online zone
		else ;Offline
		{
			stackZone:=g_IBM_Settings[ "IBM_Offline_Stack_Zone"] ;Default
			if (g_IBM_Settings["IBM_OffLine_Flames_Use"] AND g_IBM.levelManager.IsChampInFormation(83, "W")) ;if enabled and Elly is specifically in formation 2, the stacking formation TODO: Check if Elly is actually enabled somewhere?
			{
				flames:=g_Heroes[83].GetNumFlamesCards()
				if (flames>0)
					stackZone:=g_IBM_Settings["IBM_OffLine_Flames_Zones"][flames]
			}
        }
		return stackZone
    }

	ShouldOfflineStack()
    {
        if (this.HybridBlankOffline) ;This logic is not used if we are doing blank offlines
			return false
		else if (this.cycleForceOffline) ;Force offline takes priority, as it will often be used with offline disabled below
			return True
		else if (this.cycleDisableOffline)
			return False
		else if (this.cycleMax == 1) ;Hybrid disabled
            return True
        else if (this.cycleCount >= this.cycleMax) ;Hybrid Offline
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
			;OutputDebug % A_TickCount . ":TestForBlankOffline() ShouldBlankRestart=[" . this.ShouldBlankRestart() . "] EnoughHasteForCurrentRun=[" . this.EnoughHasteForCurrentRun() . "] RelayBlankOffline=[" . this.RelayBlankOffline . "] RelayData.IsActive=[" . this.RelayData.IsActive() . "] RelayData.State=[" . this.RelayData.State . "]`n"
			restartZone:=g_IBM_Settings[ "IBM_Offline_Stack_Zone"] ;Default
			if (currentZone > restartZone) ;CycleCount will be reset on return from offline, so this will only trigger once
			{
				this.BlankRestart()
			}
			else if (this.RelayBlankOffline AND !this.RelayData.HasTriggered()) ;Check for relay only if it isn't already active
			{
				relayZone:=this.RelayData.GetRelayZone(restartZone,this)
				if (currentZone > relayZone) ;If beyond the relay threshold TODO: If we need to stack this has to wait. Maybe it could be set to go 500 zones before the expected stack zone if that many are available?
				{
					this.RelayData.Start()
				}
			}
		}
	}

	BlankRestart() ;Restart without stacking TODO: We need an option to stop progress here for potatoes
    {
		startStacks:=g_SF.Memory.ReadSBStacks()
		offlineStartTime:=A_TickCount
		startZone:=g_SF.Memory.ReadCurrentZone() ; record current zone before saving for bad progression checks
		g_SF.CurrentZone:=startZone
		g_IBM.Logger.AddMessage("BlankRestart Entry:z" . startZone)
		if(this.ShouldWalk(g_SF.CurrentZone))
		{
			g_SF.KEY_GameStartFormation:=this.KEY_E ;TODO: Does this make sense for a blank? We could end up anywhere as we don't stop autoprogress
			g_SF.GameStartFormation:=g_IBM.levelManager.GetFormation("E")
		}
		else
		{
			g_SF.KEY_GameStartFormation:=this.KEY_Q ;TODO: Does this make sense for a blank? We could end up anywhere as we don't stop autoprogress
			g_SF.GameStartFormation:=g_IBM.levelManager.GetFormation("Q")
		}
		g_SF.CloseIC("BlankRestart",this.RelayBlankOffline) ;2nd arg is to use PID only, so we don't close the relay copy of the game when in that mode
		if (this.RelayBlankOffline)
		{
			g_IBM.Logger.AddMessage("BlankRestart() returning game in Relay mode")
			this.RelayData.Release()
			g_IBM.routeMaster.ResetCycleCount() ;TODO: Do these make sense here? Might need to be after picked up
			g_IBM.DialogSwatter_Start() ;This seems a bit low-priority to happen this early, can we make it check later?
		}
		else ;The sleep is to allow launcher like EGS to detect the game has closed, but that is not applicable to relay (which can't use the EGS launcher)
		{
			if (g_IBM_Settings["IBM_OffLine_Sleep_Time"])
			{
				g_SharedData.IBM_UpdateOutbound("LoopString","BlankRestart: Sleep")
				ElapsedTime := 0
				while ( ElapsedTime < g_IBM_Settings["IBM_OffLine_Sleep_Time"] )
				{
					g_SharedData.IBM_UpdateOutbound("LoopString","BlankRestart Sleep: " . g_IBM_Settings["IBM_OffLine_Sleep_Time"] - ElapsedTime)
					g_IBM.IBM_Sleep(15)
					ElapsedTime := A_TickCount
				}
			}
		}
		g_SF.SafetyCheck() ;TODO: Does this do more harm than good during Blank offlines? It can potentially swap the process back to the wrong one if the window is still in existance? Need to roll our own for the blank codepath? Possibly needs to be changed for all runs
		totalTime:=A_TickCount-offlineStartTime
		generatedStacks:=g_SF.Memory.ReadSBStacks() - startStacks
		returnZone:=g_SF.Memory.ReadCurrentZone()
		if (returnZone < startZone) ;We've gone backwards, this is expected as we don't stop autoprogress, although it can also happen if the exit save fails
		{
			if (g_IBM.offramp) ;Not checking the offramp zone here as simply overwriting false with false is almost certainly faster than doing so
				g_IBM.offramp:=false ;Reset offramp
			g_IBM.previousZone:=returnZone ;Otherwise the currentZone > previousZone check will be false until we pass the original zone
			g_IBM.currentZone:=returnZone ;Must also be reset, otherwise previousZone will be updated straight to the old current zone
			g_SharedData.IBM_UpdateOutbound_Increment("TotalRollBacks")
			g_IBM.Logger.AddMessage("BlankRestart() Exit Rollback Detected,Start@z" . startZone . ",End@z" . returnZone . "," . generatedStacks . ",Time:" . totalTime . ",OfflineTime:" . g_SF.Memory.ReadOfflineTime() . ",Server:" . g_SF.Memory.IBM_GetWebRootFriendly())
		}
		else
			g_IBM.Logger.AddMessage("BlankRestart() Exit, End@z" . returnZone . "," . generatedStacks . ",Time:" . totalTime . ",OfflineTime:" . g_SF.Memory.ReadOfflineTime() . ",Server:" . g_SF.Memory.IBM_GetWebRootFriendly())
        g_SharedData.IBM_UpdateOutbound("IBM_RunControl_StackString","Restarted at z" . returnZone . " in " . Round(totalTime/ 1000,2) . "s")
		g_PreviousZoneStartTime := A_TickCount
    }

	TestForSteelBonesStackFarming() ;Returns true if we have a failure, namely the out of stacks and need to force restart case
    {
		currentZone := g_SF.Memory.ReadCurrentZone()
        if (currentZone < 0 OR currentZone >= this.targetZone) ; Don't test while modron resetting
            return false
        stackZone:=this.GetStackZone()
		stacks := g_SF.Memory.ReadSBStacks()
		targetStacks:=this.GetTargetStacks()
 		if (stacks < targetStacks)
		{
			;HybridUltra
			if (this.UltraStacking)
			{
				highZone:=g_SF.Memory.ReadHighestZone()
				if (highZone >= stackZone AND this.BUDTracker.ReadBUD(5)>g_SF.Memory.IBM_ReadCurrentZoneMonsterHealthExponent()) ;TODO: Currently checks HP in THIS zone, which is kinda dumb
				{
					this.StackUltra(highZone)
					return false
				}
			}
			;End HybridUltra
			if (currentZone >= stackZone) ;This is now >= so we don't have to go around taking 1 off the stackzone all the time
			{
				this.StackFarm()
				return false
			}
		}
        ; Briv ran out of jumps but has enough stacks for a new adventure, restart adventure. With protections from repeating too early or resetting within 5 zones of a reset.
		;Irisiri - changed >z10 to >Thell target, but this will fail if Thell isn't present
		;04Jul25: Added check for transitioning, so we actually spend the last jump before resetting, otherwise we'll go as soon as the stacks are spent which is before we benefit from them
        if (g_SF.Memory.ReadHasteStacks() < 50 AND stacks >= targetStacks AND g_SF.Memory.ReadHighestZone() > this.thelloraTarget AND (g_SF.Memory.ReadHighestZone() <= this.targetZone) AND !g_SF.Memory.ReadTransitioning()) ;Removed the 5-zones-from-end check; is there's an armoured boss we'll not be able to be progress. TODO: With adventure-aware routing we could determine the last safe zone to walk from. Updated to not try and reset during relay restart (which shouldn't really happen since we don't blank if we don't have enough stacks...)
        {
            if (this.RelayBlankOffline AND this.RelayData.IsActive()) ;TODO: Something smart here
			{
				g_IBM.Logger.AddMessage("TestForSteelBonesStackFarming() force restart supressed due to Relay")
			}
			else
			{
				g_IBM.Logger.AddMessage("Out of stacks:z" . currentZone)
				g_SF.RestartAdventure("Out of haste @ z" . currentZone . " && have SB for next")
				return true
			}
        }
		return false
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

	StackFarm()
    {
        if (this.ShouldOfflineStack())
        {
			this.StackRestart()
		}
        else
        {
			this.StackNormal()
		}
        this.StartAutoProgressSoft()
    }

	;START ULTRA STACK TESTING

	StackUltra(highZone,maxOnlineStackTime := 300000)
    {
        if (this.PostponeStacking(highZone))
            return 0
		MEMORY_SB_STAT:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.StatHandler.BrivSteelbonesStacks
		ADDRESS_SB:=_MemoryManager.instance.getAddressFromOffsets(MEMORY_SB_STAT.BasePtr.BaseAddress,MEMORY_SB_STAT.FullOffsets*)
		TYPE_SB:=MEMORY_SB_STAT.ValueType
		startStacks:= stacks := _MemoryManager.instance.read(ADDRESS_SB,TYPE_SB)
		targetStacks:=this.GetTargetStacks(,true) ;Force recalculation of remaining haste stacks
			return
		StartTime := A_TickCount
		this.UltraStackFarmSetup()
		ElapsedTime := 0
        g_SharedData.IBM_UpdateOutbound("LoopString","Stack Ultra")
        g_SF.FallBackFromBossZone() ;Moved this out the loop, which might be a bad idea...
		if (this.useBrivBoost) ;Should this be moved before StackFarmSetup()? Or possibly into StartFarmSetup(this.useBrivboost) (as online only) - we want the first W press to occur before we start doing Other Stuff so the formation switch happens ASAP
			this.BrivBoost.Apply()
		g_IBM.levelManager.LevelFormation("W", "min") ;Ensures we're levelled, and applies any changes made based by Briv Boost if used
		maxOnlineStackTime/=g_SF.Memory.IBM_ReadBaseGameSpeed() ;Factor timescale into the timeout
		precisionMode:=false
		precisionTrigger:=Floor(targetStacks * 0.90)
		while (stacks < targetStacks AND ElapsedTime < maxOnlineStackTime )
        {
			if (precisionMode)
			{
				Sleep, 0 ;Fast sleep
			}
			else
			{
				if (stacks > precisionTrigger) ;Once we have hit precisionTrigger stacks go critical and check faster to get maximum precision
				{
					Critical On
					g_InputManager.gameFocus() ;Set Game Focus so we don't have to do it when releasing from the stack (this will cause issues if the game loses focus in the last few hundred ms of stacking)
					precisionMode:=true
				}
				g_IBM.IBM_Sleep(15)
			}
			ElapsedTime := A_TickCount - StartTime
			stacks := _MemoryManager.instance.read(ADDRESS_SB,TYPE_SB)
        }
		StartTime:=A_TickCount
		ultRetryCount:=g_Heroes[58].UseUltimate()
		if (ultRetryCount=="") ;No key found - ult not available (not fielded, not levelled enough)
		{
			g_IBM.Logger.AddMessage("Unable to exit Ultra stack as Briv ult key not available - falling back")
			g_SF.FallBackFromZone()
		}
		else
		{
			ReleaseTime:=0
			maxTime:=200 + (5000 / g_SF.Memory.IBM_ReadBaseGameSpeed()) ;200ms real time for the ult to activate, then 5 in-game seconds to resolve and give kill credit
			while (g_SF.Memory.ReadHighestZone()==highZone AND ReleaseTime <= maxTime) ;Whilst still on this zone
			{
				g_IBM.IBM_Sleep(15)
				ReleaseTime:=A_TickCount - StartTime
			}
			if (g_SF.Memory.ReadHighestZone()==highZone) ;If we still didn't proceed
			{
				g_IBM.Logger.AddMessage("Failed to exit Ultra stack after firing Briv's ultimate - falling back")
				g_SF.FallBackFromZone()
			}
		}
        Critical Off
		if (ElapsedTime >= maxOnlineStackTime)
        {
			g_SF.RestartAdventure( "Ultra@z" . g_SF.Memory.ReadCurrentZone() . " took too long (" . ROUND(ElapsedTime/1000,1) . "s)") ;TODO for both this and StackNormal() - this seems a bit extreme?
            g_SF.SafetyCheck()
            g_PreviousZoneStartTime:=A_TickCount
            return
        }
        g_PreviousZoneStartTime:=A_TickCount
		generatedStacks:=stacks - startStacks
		g_SharedData.IBM_UpdateOutbound("IBM_RunControl_StackString","Stacking: Completed online Ultra at z" . highZone . " generating " . generatedStacks . " stacks in " . Round(ElapsedTime/ 1000,2) . "s")
		g_IBM.Logger.AddMessage("Ultra{M=" . this.MelfManager.GetCurrentMelfEffect() . " z" . highZone . " Tar=" . targetStacks . "}," . generatedStacks . "," . ElapsedTime)
		if(g_SF.Memory.ReadHighestZone()<this.targetZone) ;If we'll jump from stack zone straight to reset zone things get a bit weird as the game behaves differently transitioning to the reset zone
			this.SetFormation() ;Standard call to reset trustRecent
    }

	UltraStackFarmSetup()
    {
		this.KEY_W.KeyPress_Bulk() ;Trying _Bulk() here, this is vulnerable but we are trying to get Melf deployed as fast as elvenly possible
		g_IBM.levelManager.LevelFormation("W", "min")
		StartTime := A_TickCount
        ElapsedTime := 0
		TimeOut:=3000 ;Must be short enough that failing to add a champion doesn't cause a delay - e.g. if Melf is to be levelled here, but Tatyana is also present and will complete the stack in reasonable time even without Melf
        g_SharedData.IBM_UpdateOutbound("LoopString","Setting stack farm formation")
        while (!g_SF.IsCurrentFormation(g_IBM.levelManager.GetFormation("W")) AND ElapsedTime < TimeOut) ;TODO: We might want to make a check that returns true if the formation is selected, either on field or in their bench seat, as this will fail if someone doesn't get placed after levelling due to the formation being under attack
        {
			this.KEY_W.KeyPress() ;Not using _Bulk here as the swap here is a failure mode
            g_IBM.levelManager.LevelFormation("W", "min",0) ;Should this be here? Needs to be time=0 so it doesn't eat all 5000ms loop ms
			g_IBM.IBM_Sleep(15)
            ElapsedTime := A_TickCount - StartTime
        }
		if (ElapsedTime >= TimeOut)
		{
			g_IBM.Logger.AddMessage("FAIL: UltraStackFarmSetup() did not set W formation within " . TimeOut . "ms")
			g_IBM.Logger.AddMessage(">DEBUG: Melf Level=[" . g_Heroes[59].ReadLevel() . "] Formation=" . this.DEBUG_FORMATION_STRING())
		}
    }

	;END ULTRA STACK TESTING

	StackNormal(maxOnlineStackTime := 300000) ;300s might look excessive, but I think it would take ~170s at x1 speed to gain 1122 stacks (11J to 1510 w/o Thunder Step)
    {
		; Melf stacking
        if (g_IBM_Settings["IBM_Online_Use_Melf"] AND this.PostponeStacking(g_SF.Memory.ReadCurrentZone()))
            return 0
		MEMORY_SB_STAT:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.StatHandler.BrivSteelbonesStacks
		ADDRESS_SB:=_MemoryManager.instance.getAddressFromOffsets(MEMORY_SB_STAT.BasePtr.BaseAddress,MEMORY_SB_STAT.FullOffsets*)
		TYPE_SB:=MEMORY_SB_STAT.ValueType
		startStacks:= stacks := _MemoryManager.instance.read(ADDRESS_SB,TYPE_SB)
		targetStacks:=this.GetTargetStacks(,true) ;Force recalculation of remaining haste stacks
        if (this.ShouldAvoidRestack(stacks, targetStacks))
        {
			return
		}
		this.SetFormation() ;Ensure the correct formation is set for the zone before we stop progress and try to stack
		StartTime := A_TickCount ;Start counting time from the point we go to stop autoprogress - SetFormation() is a normal part of zone completion
		this.ToggleAutoProgress( 0, false, true )
        if (g_IBM.LevelManager.Champions.HasKey(59) AND g_IBM.LevelManager.Champions[59].NeedsLevelling()) ;If we're levelling Melf in the stack zone (e.g. due to using Baldric), we need to do his initial levelup as fast as possible after the formation swap to try and stop it failing TODO: Having Melf hard-coded like this is cludgy but I don't see a way around it...
		{
			if (g_IBM.LevelManager.Champions[59].GetLevelsRequired() < 100)
				fastMelf:=2 ;Modifier press
			else
				fastMelf:=1 ;Normal press
		}
		else
			fastMelf:=0
		if (this.useFaridehUlt AND g_IBM.LevelManager.savedFormationChamps["W"].HasKey(33)) ;Set Farideh's level - currently ignoring BUD here as it can drop
			this.levelManager.OverrideLevelByIDRaiseToMin(33,"min",125)
		this.WaitForZoneCompleted() ;Complete the current zone
		this.OnlineStackFarmSetup(fastMelf, g_IBM.LevelManager.Champions[59].Key)
        ElapsedTime := 0
        g_SharedData.IBM_UpdateOutbound("LoopString","Stack Normal")
        g_SF.FallBackFromBossZone() ;Moved this out the loop, which might be a bad idea...
		if (this.useBrivBoost) ;Should this be moved before StackFarmSetup()? Or possibly into StartFarmSetup(this.useBrivboost) (as online only) - we want the first W press to occur before we start doing Other Stuff so the formation switch happens ASAP
			this.BrivBoost.Apply()
		g_IBM.levelManager.LevelFormation("W", "min") ;Ensures we're levelled, and applies any changes made based by Briv Boost if used
		maxOnlineStackTime/=g_SF.Memory.IBM_ReadBaseGameSpeed() ;Factor timescale into the timeout, leaving 30000ms @ x10
		fariUltUsed:=false
		precisionMode:=false
		precisionTrigger:=Floor(targetStacks * 0.90) ;At a steady-state stack rate of 240/s, for 600 stacks this is 60 => ~250ms - which is plenty of time to activate precision mode. Note that because attacks can get synced we can't get too tight with this
		currentZone:=g_SF.Memory.ReadCurrentZone() ;Used to report the stack zone, here as it is recorded before we toggle progress back on
		while (stacks < targetStacks AND ElapsedTime < maxOnlineStackTime )
        {
			if (this.useFaridehUlt AND !fariUltUsed AND g_SF.Memory.ReadActiveMonstersCount()>=100 AND this.BUDTracker.ReadBUD(0)<g_SF.Memory.IBM_ReadCurrentZoneMonsterHealthExponent())
			{
				g_Heroes[33].UseUltimate(,true) ;Using ExitOnceQueued so we don't stay waiting for the activation and potentially overstack	
				fariUltUsed:=true 
			}
			if (precisionMode)
			{
				Sleep, 0 ;Fast sleep
			}
			else
			{
				if (stacks > precisionTrigger) ;Once we have hit precisionTrigger stacks go critical and check faster to get maximum precision
				{
					Critical On
					g_InputManager.gameFocus() ;Set Game Focus so we don't have to do it when releasing from the stack (this will cause issues if the game loses focus in the last few hundred ms of stacking)
					precisionMode:=true
					;g_IBM.Logger.AddMessage("Precision Mode at:" . stacks . " stacks")
				}
				g_IBM.IBM_Sleep(15)
			}
			ElapsedTime := A_TickCount - StartTime
			stacks := _MemoryManager.instance.read(ADDRESS_SB,TYPE_SB)
        }
		this.KEY_autoProgress.KeyPress_Bulk() ;Enable autoprogress as fast as we can. If we're stuck the following will handle it. Using _Bulk for this reason-game focus is set when precision is turned on
		if (ElapsedTime >= maxOnlineStackTime)
        {
            Critical Off
			g_SF.RestartAdventure( "Normal@z" . currentZone . " took too long (" . ROUND(ElapsedTime/1000,1) . "s)") ;TODO for both this and StackNormal() - this seems a bit extreme?
            g_SF.SafetyCheck()
            g_PreviousZoneStartTime := A_TickCount
            return
        }
        g_PreviousZoneStartTime := A_TickCount
        runComplete:=g_SF.Memory.ReadHighestZone()>=this.targetZone ;If we'll jump from stack zone straight to reset zone things get a bit weird as the game behaves differently transitioning to the reset zone
		if (!runComplete)
		{
			;If we're at reset
			if (g_SF.Memory.ReadQuestRemaining() > 0) ;Irisiri - we can't use a WaitForZoneCompleted() return here in case the zone moved forward during the above checks. Progress SHOULD be stopped but...
			{
				g_SF.FallBackFromZone()
			}
			else
			{
				this.ToggleAutoProgress( 1, false, true )
			}
		}
		Critical Off
		generatedStacks:=stacks - startStacks
		g_SharedData.IBM_UpdateOutbound("IBM_RunControl_StackString","Stacking: Completed online at z" . currentZone . " generating " . generatedStacks . " stacks in " . Round(ElapsedTime/ 1000,2) . "s")
		g_IBM.Logger.AddMessage("Online{M=" . this.MelfManager.GetCurrentMelfEffect() . " z" . currentZone . " Tar=" . targetStacks . "}," . generatedStacks . "," . ElapsedTime)
		if (!runComplete)
			this.SetFormation() ;Standard call to reset trustRecent
    }

	WaitForZoneCompleted(maxTime := 3000)
    {
        this.SetFormation()
        StartTime := A_TickCount
        ElapsedTime := 0
        this.WaitForTransition()
        quest:=g_SF.Memory.ReadQuestRemaining()
        while (quest > 0 AND ElapsedTime < maxTime)
        {
            g_IBM.IBM_Sleep(10)
            this.SetFormation()
            quest:=g_SF.Memory.ReadQuestRemaining()
			ElapsedTime := A_TickCount - StartTime
        }
    }

	PostponeStacking(currentZone) ;Used to delay stacking whilst waiting for Melf's spawn-more buff
    {
        if (g_SF.Memory.ReadHasteStacks() < 50) ;Stack immediately if Briv can't jump anymore.
            return false
		if (currentZone > this.LastSafeStackZone) ; Stack immediately to prevent resetting before stacking.
			return false
		nextSpawnMoreRange := this.MelfManager.GetFirstMelfSpawnMoreRange(currentZone)
		if(nextSpawnMoreRange)
		{
			if (currentZone < nextSpawnMoreRange[1]) ;We're below the desired stack range, and (per the above check) one exists
			{
				return true
			}
			else if (this.zones[currentZone].stackZone==false) ;Not on a stack zone
            {
				return true
			}
			else if (!this.MelfManager.IsMelfEffectSpawnMore(currentZone)) ;Not spawning more
			{
				return true
			}
		}
		else ;No Spawn More available
		{
			if (this.zones[currentZone].stackZone == false) ;Even without spawn more, try to use a desired stackzone
				return true
		}
		return false
    }

	GetLastSafeStackZone()
    {
        lastZone:=this.targetZone - 1
        ; Move back one zone if the last zone before reset is a boss.
        if (Mod(lastZone, 5 ) == 0)
            lastZone--
        return lastZone - this.zonesPerJumpQ
    }

    ShouldAvoidRestack(stacks, targetStacks) 	; avoids attempts to stack again after stacking has been completed and level not reset yet.
    {
        if ( stacks >= targetStacks )
            return 1
        if (g_SF.Memory.ReadCurrentZone() == 1) ; likely modron has reset
            return 1
        if (g_SF.Memory.ReadCurrentZone() < g_IBM_Settings["IBM_Offline_Stack_Min"]) ; don't stack below min stack zone
            return 1
        return 0
    }

	StackRestart() ;TODO: Put rollback detection back into this?
    {
		startStacks:= lastStacks := stacks := g_SF.Memory.ReadSBStacks()
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
        while ( stacks < targetStacks AND retryAttempt <= maxRetries )
        {
			this.StackFailRetryAttempt++ ; per run
            retryAttempt++               ; pre stackfarm call
            this.StackFarmSetup()
            g_SF.CurrentZone := g_SF.Memory.ReadCurrentZone() ; record current zone before saving for bad progression checks
			if(this.ShouldWalk(g_SF.CurrentZone))
			{
				g_SF.KEY_GameStartFormation:=this.KEY_E ;TODO: Does this make sense for a blank? We could end up anywhere as we don't stop autoprogress
				g_SF.GameStartFormation:=g_IBM.levelManager.GetFormation("E")
			}
			else
			{
				g_SF.KEY_GameStartFormation:=this.KEY_Q ;TODO: Does this make sense for a blank? We could end up anywhere as we don't stop autoprogress
				g_SF.GameStartFormation:=g_IBM.levelManager.GetFormation("Q")
			}
            if (this.targetZone != "" AND g_SF.CurrentZone > this.targetZone)
            {
                g_SharedData.IBM_UpdateOutbound("LoopString","Attempted to offline stack after modron reset - verify settings")
                break
            }
			this.offlineSaveTime:=g_SF.CloseIC( "StackRestart" . (this.StackFailRetryAttempt > 1 ? (" - Warning: Retry #" . this.StackFailRetryAttempt - 1 . ". Check Stack Settings."): "") )
			g_SharedData.IBM_UpdateOutbound("LoopString","Stack Sleep: ")
            ElapsedTime:=0
			sleepStart:=A_TickCount ;Seperate to the save timer, this is the delay in restarting the game specifically
			while ( ElapsedTime < g_IBM_Settings["IBM_OffLine_Sleep_Time"] )
            {
                g_SharedData.IBM_UpdateOutbound("LoopString","Stack Sleep: " . g_IBM_Settings["IBM_OffLine_Sleep_Time"] - ElapsedTime)
                g_IBM.IBM_Sleep(15)
				ElapsedTime := A_TickCount - sleepStart
            }
			g_SF.SafetyCheck()
            stacks:=g_SF.Memory.ReadSBStacks()
            ;check if save reverted back to below stacking conditions
            if (g_SF.Memory.ReadCurrentZone() < g_IBM_Settings["IBM_Offline_Stack_Min"]) ;Irisiri - this might need to consider the offline fallback?
            {
                g_SharedData.IBM_UpdateOutbound("LoopString","Stack Sleep: Failed (zone < min)")
                Break  ; "Bad Save? Loaded below stack zone, see value."
            }
            ;g_SharedData.PreviousStacksFromOffline := stacks - lastStacks ;Doesn't appear to be used for anything
            lastStacks := stacks
			g_IBM.Logger.AddMessage("Offline:" . g_SF.Memory.ReadCurrentZone() . "," . stacks . ",Time:" . A_TickCount - this.offlineSaveTime . ",Attempt:" . retryAttempt . ",OfflineTime:" . g_SF.Memory.ReadOfflineTime() . ",Server:" . g_SF.Memory.IBM_GetWebRootFriendly())
			this.offlineSaveTime:=-1 ;Flags as not active
        }
        g_PreviousZoneStartTime := A_TickCount
		generatedStacks:=g_SF.Memory.ReadSBStacks() - startStacks
		totalTime:=A_TickCount-offlineStartTime
		if (retryAttempt > maxRetries+1) ;We're a bit screwed at this point, +1 as retryAttempt is really 'tryAttempt'
        {
			g_SharedData.IBM_UpdateOutbound("LoopString","Failed to generate target " . targetStacks . " stacks in " . maxRetries . " attempts. Verify settings")
			g_SharedData.IBM_UpdateOutbound("IBM_RunControl_StackString","FAIL: Attempted to stack offline at z" . g_SF.Memory.ReadCurrentZone() . " generating " . generatedStacks . " stacks in " . Round(totalTime/ 1000,2) . "s" . (retryAttempt>1 ? " using " . retryAttempt . " attempts" : ""))
        }
        else
		{
			g_SharedData.IBM_UpdateOutbound("IBM_RunControl_StackString","Stacking: Completed offline at z" . g_SF.Memory.ReadCurrentZone() . " generating " . generatedStacks . " stacks in " . Round(totalTime/ 1000,2) . "s" . (retryAttempt>1 ? " using " . retryAttempt . " attempts" : ""))
		}
    }

	StackFarmSetup()
    {
		if (!g_SF.KillCurrentBoss() ) ; Previously/Alternatively FallBackFromBossZone()
            g_SF.FallBackFromBossZone()
        this.KEY_W.KeyPress()
        this.ToggleAutoProgress(0,false,true)
		g_IBM.levelManager.LevelFormation("W", "min")
		this.WaitForTransition(this.KEY_W)
		StartTime := A_TickCount
        ElapsedTime := 0
		TimeOut:=5000
        g_SharedData.IBM_UpdateOutbound("LoopString","Setting stack farm formation")
        while (!g_SF.IsCurrentFormation(g_IBM.levelManager.GetFormation("W")) AND ElapsedTime < TimeOut)
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
	KillCurrentBoss(maxLoopTime:=25000 )
    {
        currentZone := this.Memory.ReadCurrentZone()
        if mod(currentZone, 5)
            return 1
        StartTime := A_TickCount
        ElapsedTime := 0
        g_SharedData.IBM_UpdateOutbound("LoopString","Killing boss before stacking")
        while ( !mod( this.Memory.ReadCurrentZone(), 5 ) AND ElapsedTime < maxLoopTime )
        {
            ElapsedTime := A_TickCount - StartTime
            this.SetFormation()
            if(!this.Memory.ReadQuestRemaining()) ; Quest complete, still on boss zone. Skip boss bag.
                this.ToggleAutoProgress(1,0,false)
            g_IBM.IBM_Sleep(50)
        }
        if(ElapsedTime >= maxLoopTime)
            return 0
        this.WaitForTransition()
        return 1
    }

	OnlineStackFarmSetup(fastMelf,levelKey) ;Cuts out checking for bosses, stopping and waiting for the transition, since we're already parked up on a completed zone
    {
        this.KEY_W.KeyPress()
		if (fastMelf==2)
		{
			g_IBM.LevelManager.SetModifierKey(true)
			levelKey.KeyPress_Bulk()
			g_IBM.LevelManager.SetModifierKey(false)
		}
		else if (fastMelf==1)
		{
			levelKey.KeyPress_Bulk()
		}
		StartTime := A_TickCount
        ElapsedTime := 0
		TimeOut:=3000 ;Must be short enough that failing to add a champion doesn't cause a delay - e.g. if Melf is to be levelled here, but Tatyana is also present and will complete the stack in reasonable time even without Melf
        g_SharedData.IBM_UpdateOutbound("LoopString","Setting stack farm formation")
        while (!g_SF.IsCurrentFormation(g_IBM.levelManager.GetFormation("W")) AND ElapsedTime < TimeOut) ;TODO: We might want to make a check that returns true if the formation is selected, either on field or in their bench seat, as this will fail if someone doesn't get placed after levelling due to the formation being under attack
        {
			this.KEY_W.KeyPress() ;Not using _Bulk here as the swap here is a failure mode
            g_IBM.levelManager.LevelFormation("W", "min",0) ;Should this be here? Needs to be time=0 so it doesn't eat all 5000ms loop ms
			g_IBM.IBM_Sleep(15)
            ElapsedTime := A_TickCount - StartTime
        }
		if (ElapsedTime >= TimeOut)
		{
			g_IBM.Logger.AddMessage("FAIL: OnlineStackFarmSetup() did not set W formation within " . TimeOut . "ms")
			g_IBM.Logger.AddMessage(">DEBUG: Melf Level=[" . g_Heroes[59].ReadLevel() . "] Formation=" . this.DEBUG_FORMATION_STRING() . " fastMelf=[" . fastMelf . "]")
		}
    }

	DEBUG_FORMATION_STRING() ;Returns the formation size and members as a string
	{
		size := g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.formation.slots.size.Read()
		if(size <= 0 OR size > 14) ; sanity check, 12 is the max number of concurrent champions possible.
			return "X:[]"
		formation:=":["
		champCount:=0
		loop, %size%
		{
			heroID := g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.formation.slots[A_index - 1].hero.def.ID.Read()
			if (heroID>0)
				champCount++
			else
				heroID:="_"
			formation.=heroID . ";"
		}
		formation:=champCount . formation . "]"
		return formation
	}

	;Override to use Sleep
	WaitForTransition(KEY:="", maxLoopTime:=5000) ;KEY is a IC_BrivMaster_InputManager_Key_Class object
    {
        if !g_SF.Memory.ReadTransitioning()
            return
        StartTime := A_TickCount
        g_SharedData.IBM_UpdateOutbound("LoopString","Waiting for transition...")
        if (KEY)
			g_InputManager.gameFocus() ;Set focus once and use _Bulk()
		while (g_SF.Memory.ReadTransitioning()==1 AND A_TickCount - StartTime < maxLoopTime)
        {
			If (KEY)
				KEY.KeyPress_Bulk()
			g_IBM.IBM_Sleep(15) ;Sleep as we don't want to go back multiple zones
        }
        return
    }


	SetFormationHighZone() ;Used when we don't want to check the current zone as we know it's complete - namely after the Casino when combining, when we will be jumping with the M value regardless of the formation swap - in which case we need to prepare to the next zone
	{
		isEZone:=this.zones[g_SF.Memory.ReadHighestZone()].jumpZone==false ;TODO: Any reason this doesn't use this.ShouldWalk()? Seems to be duplicating the Thellora recovery option from there in the bench/unbench code
		Critical On ;Here to handle the animation skip, maybe isn't needed for feat swap as a result?
		benchReturn:=this.BenchBrivConditions(isEZone) ;check to bench briv
		lastFormation:=g_SF.Memory.ReadMostRecentFormationFavorite() ;New Sep25 read, used in all cases as it is part of the bad formation check
        if (benchReturn AND lastFormation!=3) ;New Sep25 read. Formation 3 is E
        {
			this.KEY_E.KeyPress()
			;OutputDebug % A_TickCount . " z" . g_SF.Memory.ReadCurrentZone() . " SetFormation() - STD E - fastCheck=[" . fastCheck . "] trustRecent=[" . DEBUG_INITIAL_TRUST_RECENT . ">>" . trustRecent . "] lastFormation=[" . lastFormation . "]`n"
			If (benchReturn==2)
			{
				if (this.zones[g_SF.Memory.ReadHighestZone()].jumpZone) ;Only put Briv back in urgently if we need to jump right away. Note this does not have to consider featswap because we'll never enter this block with Briv in E, as we can't animation skip in that case
				{
					startTime:=A_TickCount
					while (g_SF.Memory.ReadFormationTransitionDir() == 4 and (A_TickCount-startTime)<5000)
					{
						g_IBM.IBM_Sleep(15)
					}
					this.KEY_Q.KeyPress_Bulk() ;_Bulk as follows the E.KeyPress()
				}
			}
			Critical Off
            return
        }
		else
			Critical Off
		;check to unbench briv
        if (this.UnBenchBrivConditions(isEZone) AND lastFormation!=1) ;Formation 1 is Q
        {
			this.KEY_Q.KeyPress()
			return
        }
	}

	SetFormation(fastCheck:=false) ;To be called with FastCheck during straightforward progression, e.g. not after stacking, falling back, other fun things
    {
		static trustRecent:=false ;Do we believe that the ReadMostRecentFormationFavorite() is respresentative? Needed as it changes even if the formation swap fails

		;DEBUG_INITIAL_TRUST_RECENT:=trustRecent ;DEBUG!

		if (!fastCheck)
			trustRecent:=false ;Reset to false for all normal calls
		isEZone:=this.ShouldWalk(g_SF.Memory.ReadCurrentZone()) ;TODO: Any reason this doesn't use this.ShouldWalk()? Seems to be duplicating the Thellora recovery option from there in the bench/unbench code
		Thread, NoTimers ;Here to handle the animation skip, maybe isn't needed for feat swap as a result?
		benchReturn:=this.BenchBrivConditions(isEZone) ;check to bench briv
		lastFormation:=g_SF.Memory.ReadMostRecentFormationFavorite() ;New Sep25 read, used in all cases as it is part of the bad formation check
        if (benchReturn AND lastFormation!=3) ;New Sep25 read. Formation 3 is E
        {
			this.KEY_E.KeyPress()
			If (benchReturn==2)
			{
				if (this.zones[g_SF.Memory.ReadHighestZone()].jumpZone) ;Only put Briv back in urgently if we need to jump right away. Note this does not have to consider featswap because we'll never enter this block with Briv in E, as we can't animation skip in that case
				{
					g_IBM.IBM_Sleep(15) ;Avoid swapping back instantly
					startTime:=A_TickCount
					while (g_SF.Memory.ReadFormationTransitionDir()==4 AND !g_Heroes[58].ReadBenched() AND (A_TickCount-startTime)<1000) ;Whilst we're in the transition and Briv is still on the field. Using .ReadBenched() as it's a simple read, whereas ReadFielded() has to loop the formation
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
			;OutputDebug % A_TickCount . "@z" . g_SF.Memory.ReadCurrentZone() . ": Swap to Q`n"
			this.KEY_Q.KeyPress()
			return
        }
		if (trustRecent AND fastCheck)
		{
			if !(lastFormation==1 OR lastFormation==3)
			{
				isEZone ? this.KEY_E.KeyPress() : this.KEY_Q.KeyPress()
			}
		}
		else
		{
			if !(g_SF.IsCurrentFormation(g_IBM.levelManager.GetFormation("Q")) OR g_SF.IsCurrentFormation(g_IBM.levelManager.GetFormation("E")))
			{
				isEZone ? this.KEY_E.KeyPress() : this.KEY_Q.KeyPress()
			}
			else
			{
				trustRecent:=true ;As we've checked we're on Q or E via formation read, we should be in normal progression
			}
		}
    }

	 ; True/False on whether Briv should be benched based on game conditions.
	 ;Irisiri - as part of drift checking, return changed as follows:
	 ;0 - as false before, do not bench
	 ;1 - as true before for most conditions, bench
	 ;2 - bench for animation override
    BenchBrivConditions(isEZone)
    {
		;Irisiri-Bench Briv if before Thellora's rush target so he doesn't do extra unplanned skips and run out of stacks (causing an early reset, and too few Thell stacks, causing a death spiral). Notes on the memory functions:
		;ReadTransitionOverrideSize() 	| should read 1 if briv jump animation override is loaded to , 0 otherwise - REMOVED in 627
		;ReadTransitionDirection() 		| 0 = Static (instant), 1 = Forward, 2 = Backward, 3=JumpDown, 4=FallDown
		;ReadFormationTransitionDir() 	| 0 = OnFromLeft, 1 = OnFromRight, 2 = OnFromTop, 3 = OffToLeft, 4 = OffToRight, 5 = OffToBottom
		;if (this.ShouldDoThelloraRecovery()) ;Irisiri - to stop Briv being used before Thell is done
		;	{
		;		return 1
		;	}
		;bench briv if jump animation override is added to list and it isn't a quick transition (reading ReadFormationTransitionDir makes sure QT isn't read too early). We can't do this for feat swap as every formation has Briv
		if (this.zonesPerJumpE == 1 AND g_SF.Memory.ReadTransitionDirection() == 1 AND g_SF.Memory.ReadFormationTransitionDir() == 4 )
            {
				return 2
			}
        ;bench briv not in a preferred briv jump zone
        if (isEZone)
			{
				return 1
			}
        return 0
    }

    ; True/False on whether Briv should be unbenched based on game conditions.
    UnBenchBrivConditions(isEZone)
    {
		; do not unbench Briv if before Thellora's rush target (as per the bench condition)
		;if (this.ShouldDoThelloraRecovery())
		;	{
		;	return false
		;	}
		; do not unbench briv if party is not on a perferred briv jump zone.
        if (isEZone)
			{
            return false
			}
		if (this.zonesPerJumpE > 1) ;Don't do transition-based checks when feat swapping
			return true ;Not a walk zone so go to Q
        ;if transition direction is "OnFromLeft"
		if (g_SF.Memory.ReadFormationTransitionDir() != 4)
		{
			return true
		}
        return false
    }

	/*
	ShouldDoThelloraRecovery() ;True if we should walk to Thellora's skip zone regardless of the zone's setting
	{
		return false ;Testing stack-calc based recovery as walk based can't work for feat swap TODO: Option for this? Maybe a max number of zones to walk?
		if (this.zonesPerJumpE > 1) ;If we're feat swapping we can't recover via walking. TODO: Should this actually check for Briv in E instead of using zonesPerJumpE as a proxy? Might be worth reading his presence in at the start of the run in Reset()
			return false
		currentZone:=g_SF.Memory.ReadCurrentZone()
		if (currentZone < this.thelloraTarget) ;If we're before Thellora's target area, check if we have enough stacks (ie because it's the first run and we have low Thellora charges but lots of stacks)
		{
			return g_SF.Memory.ReadHasteStacks()<this.zones[currentZone].stacksToFinish ;Do we have enough stacks anyway?
		}
		return false
	}
	*/

	ShouldWalk(currentZone)
	{
		return this.zones[currentZone].jumpZone==False ; Or this.ShouldDoThelloraRecovery()
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
				currentZone.nextZone:=zones[nextZoneNumber] ;Set the next zone
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
		If (g_SF.Memory.IBM_HeroHasAnyFeatsSavedInFormation(58, g_SF.Memory.GetSavedFormationSlotByFavorite(1)) or g_SF.Memory.IBM_HeroHasAnyFeatsSavedInFormation(58, g_SF.Memory.GetSavedFormationSlotByFavorite(3))) ;If there are feats saved in Q or E (which would overwrite any others in M)
		{
			thunderInQ:=g_SF.Memory.IBM_HeroHasFeatSavedInFormation(58, 2131 , g_SF.Memory.GetSavedFormationSlotByFavorite(1))
			thunderInE:=g_SF.Memory.IBM_HeroHasFeatSavedInFormation(58, 2131 , g_SF.Memory.GetSavedFormationSlotByFavorite(3))
			return (thunderInQ OR thunderInE)
		}
		else if (g_SF.Memory.IBM_HeroHasAnyFeatsSavedInFormation(58, g_SF.Memory.GetActiveModronFormationSaveSlot())) ;Briv has feats in M
			return g_SF.Memory.IBM_HeroHasFeatSavedInFormation(58, 2131 , g_SF.Memory.GetActiveModronFormationSaveSlot())
		else ;Non-feat swap might not have feats saved in formations at all
		{
			feats := g_SF.Memory.GetHeroFeats(58)
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
        g_PreviousZoneStartTime:=A_TickCount
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

class IC_BrivMaster_BUD_Tracker_Class ;Manages BUD calculations
{
	__New()
	{
		MEMORY_ACD:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].ActiveCampaignData
		this.minUltDPS:=LOG(MEMORY_ACD.minUltDPS.Read()) ;LOG10 as we are working in exponents
		this.ultBasedOnDPS:=MEMORY_ACD.ultBasedOnDPS.Read()
		this.ultFalloffExponent2:=MEMORY_ACD.ultFalloffExponent2.Read() ;The '2' is per the game source
		this.ultFalloffDelay:=MEMORY_ACD.ultFalloffDelay.Read()
		ultFalloffPeriod:=MEMORY_ACD.ultFalloffPeriod.Read()
		ultFalloffPeriodMod:=MEMORY_ACD.ultFalloffPeriodMod.Read()
		this.ultFalloff:=ultFalloffPeriod*ultFalloffPeriodMod ;These are only used multiplied like this, so no point storing the separate values
	}

	ReadBUD(realTimeOffset:=0)
	{
		MEMORY_ACD:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].ActiveCampaignData
		first8:=MEMORY_ACD.highestHitDamage.Read("Int64") ;Quad
        newObject := MEMORY_ACD.highestHitDamage.QuickClone()
        offsetIndex := newObject.FullOffsets.Count()
        newObject.FullOffsets[offsetIndex] := newObject.FullOffsets[offsetIndex] + 0x8
		last8:= newObject.Read("Int64")
		highestHitDamage:=g_SF.Memory.IBM_ConvQuadToExponent(first8,last8)
		highestHitCooldown:=MEMORY_ACD.highestHitCooldown.Read()
		timeSinceHighestHit:=MEMORY_ACD.timeSinceHighestHit.Read()
		gameTimeOffset:=realTimeOffset*g_SF.Memory.IBM_ReadBaseGameSpeed()
		HighestHitDecay:=1 / this.ultFalloffExponent2 ** Max(0, (Ceil(timeSinceHighestHit+gameTimeOffset) - this.ultFalloffDelay) / this.ultFalloff)
		HighestHitScaledDamage:=MAX(0,highestHitDamage + LOG(HighestHitDecay))
		BUD:=MAX(!this.ultBasedOnDPS ? HighestHitScaledDamage : HighestHitScaledDamage - LOG(highestHitCooldown),this.minUltDPS)
		return BUD
	}
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
			this.MainPID:=g_SF.PID
			this.MainHwnd:=g_SF.Hwnd
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
			g_SF.IBM_SuspendProcess(this.RelayPID,False)
			g_IBM.Logger.AddMessage("Relay PreRelease() state 5 - resuming")
		}
		else if (this.State==6) ;DEBUG: Relay is in a complete state. This might be possible during relay run recovery? TODO: This can be called when a second CloseIC() is called after the relay handover, e.g. because the run gets stuck
		{
			g_SF.IBM_SuspendProcess(this.RelayPID,False)
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
			g_SF.IBM_SuspendProcess(this.RelayPID,False)
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
			g_IBM.Logger.AddMessage("Relay Release() state [" . this.State . "] failed to detect state change")
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
		if (this.GetProcessName(this.HelperPID) == "AutoHotkey.exe") ;Kill the relay script
		{
			g_IBM.Logger.AddMessage("CleanUpOnFail() found Relay AHK script PID=[" . this.HelperPID . "] still running - killing")
			closeString:="ahk_pid " . this.HelperPID
			WinKill, %closeString%
		}
		WinGet, recoveryPID, PID, % "ahk_exe " . g_IBM_Settings["IBM_Game_Exe"] ;Check for IC processes
		if (recoveryPID)
		{
			g_IBM.Logger.AddMessage("CleanUpOnFail() recovery PID found=[" . recoveryPID . "]")
			g_SF.PID:=recoveryPID
			g_SF.IBM_SuspendProcess(g_SF.PID,False) ;Ensure the process is not stuck suspended
			g_SF.Hwnd:=WinExist("ahk_pid " . recoveryPID)
			g_SF.Memory.OpenProcessReader(recoveryPID) ;Open this PID specifically
			g_SF.ResetServerCall()
		}
		else ;Otherwise open as normal
		{
			g_IBM.Logger.AddMessage("CleanUpOnFail() no recovery PID found - calling g_SF.OpenIC()")
			g_SF.OpenIC()
		}
	}

	ProcessSwap()
	{
		logText:="ProcessSwap() changing PID=[" . g_SF.PID . "] and Hwnd=[" . g_SF.Hwnd . "] "
		g_SF.PID := this.RelayPID
		g_SF.Hwnd := this.RelayHwnd
		logText.="to PID=[" . g_SF.PID . "] and Hwnd=[" . g_SF.Hwnd . "]"
		g_IBM.Logger.AddMessage(logText)
		g_SF.Memory.OpenProcessReader(g_SF.PID)
		if (g_SF.WaitForGameReady(10000*g_IBM_Settings["IBM_OffLine_Timeout"])) ;Default is 5, so 50s
			g_IBM.Logger.AddMessage("ProcessSwap() completed switching process")
		else
			g_IBM.Logger.AddMessage("ProcessSwap() WaitForGameReady() call failed whilst switching process")
		g_SF.ResetServerCall()
		g_SharedData.IBM_UpdateOutbound("IBM_ProcessSwap",true) ;Allows the hub to react
	}

	GetProcessName(processID) ;To check without a window being present
	{
		if (hProcess := DllCall("OpenProcess", "uint", 0x0410, "int", 0, "uint", processID, "ptr"))
		{
			size := VarSetCapacity(buf, 0x0104 << 1, 0)
			if (DllCall("psapi\GetModuleFileNameEx", "ptr", hProcess, "ptr", 0, "ptr", &buf, "uint", size))
			{
				SplitPath, % StrGet(&buf), processExeName
				DllCall("CloseHandle", "ptr", hProcess)
				return processExeName
			}
			DllCall("CloseHandle", "ptr", hProcess)
		}
		return false
	}


	RelayCloseMain() ;Called from the Relay script via COM to close the main IC process during recovery
	{
		g_SF.CloseIC("Relay failed to halt at platform login",true) ;Close via PID
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
		heroIndex:=g_SF.Memory.GetHeroHandlerIndexByChampID(58)
		;size:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId.size.Read()
		size := g_SF.Memory.ReadHeroUpgradesSize(58)
		upgradeList:={}
		Loop, %size%
        {
			id:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId["value",A_Index-1].Id.Read()
            ;OutputDebug % "Calling g_SF.Memory.IBM_ReadHeroUpgradeRequiredLevelByIndex`n"
			level:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId[id].RequiredLevel.Read()
			effectString:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId[id].Def.baseEffectString.Read()
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
	minZone:=1
	maxZone:=2500
	zoneCap:=2500

	__New(zoneCap) ;Called with the zone cap to avoid duplicating it everywhere
	{
		this.zoneCap:=zoneCap
	}

	Reset(minZone,maxZone,lookahead) ;To be called once per run at the start, this deletes old patterns and handles possible changes of settings
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