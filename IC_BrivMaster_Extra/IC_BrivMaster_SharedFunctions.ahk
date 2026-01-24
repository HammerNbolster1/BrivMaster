;This file is intended for functions used across both the gem farm script and the hub. Currently meeting that goal is a WiP
#include %A_LineFile%\..\IC_BrivMaster_Memory.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\SH_KeyHelper.ahk ;Used for IC_BrivMaster_InputManager_Class

global g_PreviousZoneStartTime ;TODO: Why is this in here? It is used by CheckifStuck - move elsewhere if that function moves. Or possibly move it anyway...at least into the class constructor

class IC_BrivMaster_SharedFunctions_Class
{
	__new()
    {
        this.Memory:=New IC_BrivMaster_MemoryFunctions_Class(A_LineFile . "\..\Offsets\IC_Offsets.json")
		this.UserID:=""
		this.UserHash:=""
		this.InstanceID:=0
		this.steelbones:="" ;steelbones and sprint are used as some sort of cache so they can be acted on once memory reads are invalid I think TODO: Review
		this.sprint:=""
		this.PatronID:=0
    }

	LoadObjectFromAHKJSON(FileName,preserveBooleans:=false) ;If preserveBooleans is set 'true' and 'false' will be read as strings rather than being converted to -1 or 0, as AHK does not have a boolean type. Needed for game settings file TODO: Move JSON load/write somewhere the main script can use them too. Down with IE!
    {
        FileRead, oData, %FileName%
        data:=""
        try
        {
            if (preserveBooleans)
				data:=AHK_JSON_RAWBOOLEAN.Load(oData)
			else
				data:=AHK_JSON.Load(oData)
        }
        catch err
        {
            err.Message:=err.Message . "`tFile:`t" . FileName
            throw err
        }
        return data
    }

    WriteObjectToAHKJSON(FileName, ByRef object,preserveBooleans:=false)
    {
        if (preserveBooleans)
			objectJSON:=AHK_JSON_RAWBOOLEAN.Dump(object,,"`t")
		else
			objectJSON:=AHK_JSON.Dump(object,,"`t")
        if (!objectJSON)
            return
        FileDelete, %FileName%
        FileAppend, %objectJSON%, %FileName%
        return
    }

	GetProcessName(processID) ;To check without a window being present TODO: Used in multiple places, this might make sense for SharedFunctions as a result
	{
		if(hProcess:=DllCall("OpenProcess", "uint", 0x0410, "int", 0, "uint", processID, "ptr"))
		{
			size:=VarSetCapacity(buf, 0x0104 << 1, 0)
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

	ConvQuadToDouble(FirstEight, SecondEight) ;Takes input of first and second sets of eight byte int64s that make up a quad in memory. Obviously will not work if quad value exceeds double max
    {
        return (FirstEight + (2.0**63)) * (2.0**SecondEight)
    }

    IsCurrentFormation(testformation:="") ;Returns true if the formation array passed is the same as the formation currently on the game field. Always false on empty formation reads. Requires full formation.
    {
        if(!IsObject(testFormation))
            return false
        currentFormation := this.Memory.GetCurrentFormation()
        if(!IsObject(currentFormation))
            return false
        if(currentFormation.Count() != testformation.Count())
            return false
        loop, % currentFormation.Count()
            if(testformation[A_Index] != currentFormation[A_Index])
                return false
        return true
    }

	;A test if stuck on current area. After 35s, toggles autoprogress every 5s. After 45s, attempts falling back up to 2 times. After 65s, restarts level.
    CheckifStuck(isStuck:=false) ;TODO: The forced option being shoehorned into this seems out of place. Possibly due to the need to reset the static variables? Could make them farm object members instead?
    {
        static lastCheck:=0
        static fallBackTries:=0
        if (isStuck)
        {
            g_IBM.GameMaster.RestartAdventure("Game is stuck z[" . this.Memory.ReadCurrentZone() . "]")
            g_IBM.GameMaster.SafetyCheck()
            g_PreviousZoneStartTime := A_TickCount
            lastCheck:=0
            fallBackTries:=0
            return true
        }
		dtCurrentZoneTime := Round((A_TickCount - g_PreviousZoneStartTime) / 1000, 2)
		if (dtCurrentZoneTime <= 35) ;Irisiri - added fast exit for the standard case
			return false
        else if (dtCurrentZoneTime > 35 AND dtCurrentZoneTime <= 45 AND dtCurrentZoneTime - lastCheck > 5) ; first check - ensuring autoprogress enabled
        {
            g_IBM.RouteMaster.ToggleAutoProgress(1, true)
            if(dtCurrentZoneTime < 40)
                lastCheck:=dtCurrentZoneTime
        }
        if (dtCurrentZoneTime > 45 AND fallBackTries < 3 AND dtCurrentZoneTime - lastCheck > 15) ; second check - Fall back to previous zone and try to continue
        {
            ; reset memory values in case they missed an update.
            g_IBM.GameMaster.Hwnd:=WinExist("ahk_exe " . g_IBM_Settings["IBM_Game_Exe"]) ;TODO: This can screw things up if the there is more than one process open. At least align with .PID?
            this.Memory.OpenProcessReader()
            this.ResetServerCall()
            ; try a fall back
            g_IBM.RouteMaster.FallBackFromZone()
            g_IBM.RouteMaster.SetFormation() ;In the base script this just goes to Q, which might not be ideal, especially for feat swap
            g_IBM.RouteMaster.ToggleAutoProgress(1, true)
            lastCheck:=dtCurrentZoneTime
            fallBackTries++
        }
        if (dtCurrentZoneTime > 65)
        {
            g_IBM.GameMaster.RestartAdventure("Game is stuck z[" . this.Memory.ReadCurrentZone() . "]" )
            g_IBM.GameMaster.SafetyCheck()
            g_PreviousZoneStartTime:=A_TickCount
            lastCheck:=0
            fallBackTries:=0
            return true
        }
        return false
    }

    DoRushWait(stopProgress:=false) ;Wait for Thellora (ID=139) to activate her Rush ability. TODO: unknown what ReadRushTriggered() returns if she starts with 0 stacks or we have 0 favour (with the former being the case that might matter)
    {
        ElapsedTime:=0
		levelTypeChampions:=true ;Alternate levelling types to cover both without taking too long in each loop
		g_SharedData.UpdateOutbound("LoopString","Rush Wait")
		StartTime:=A_TickCount
		while(!(this.Memory.ReadCurrentZone() > 1 OR g_Heroes[139].ReadRushTriggered()) AND ElapsedTime < 8000)
        {
			if (stopProgress) ;If we are doing Elly's casino after the rush we need to stop ASAP so that 1 kill (probably via Melf) doesn't jump us an extra time, possibly on the wrong formation
			{
				if (this.Memory.ReadHighestZone() > 1)
				{
					g_IBM.RouteMaster.ToggleAutoProgress(0)
					stopProgress:=false ;No need to keep checking
				}
			}
			if (levelTypeChampions)
				g_IBM.levelManager.LevelWorklist() ;Level current worklist
			else
				g_IBM.levelManager.LevelClickDamage(0) ;Level click damage
            levelTypeChampions:=!levelTypeChampions
			ElapsedTime:=A_TickCount-StartTime
        }
    }

    SetUserCredentials() ;Removed creation of data to return for JSON export, as it never appeared to get used after output by ResetServerCall. Removed gem and chest data as those are fully handled by the hub side TODO: Is there any reason to keep this stuff in g_SF, rather than servercall? Seems like duplication
    {
        this.UserID:=this.Memory.ReadUserID()
		this.UserHash:=this.Memory.ReadUserHash()
		this.InstanceID:=this.Memory.ReadInstanceID()
        this.sprint:=this.Memory.ReadHasteStacks() ;TODO: Calling Haste 'Sprint' here is confusing; need to check throughout IC_Core if replacing it however (N.B. The reason for this naming is that the stat in the game is called 'BrivSprintStacks'). Possibly using that stat name in full would be clearer?
        this.steelbones:=this.Memory.ReadSBStacks()
    }

	;Removed saving of Servercall information to a JSON file, which never appeared to get used
	; sets the user information used in server calls such as user_id, hash, active modron, etc.
    ResetServerCall()
    {
        this.SetUserCredentials()
        g_ServerCall:=New IC_BrivMaster_ServerCall_Class(this.UserID,this.UserHash,this.InstanceID)
        version:=this.Memory.ReadBaseGameVersion()
        if (version != "")
            g_ServerCall.clientVersion := version
        this.GetWebRoot()
        g_ServerCall.networkID := this.Memory.ReadPlatform() ? this.Memory.ReadPlatform() : g_ServerCall.networkID
        g_ServerCall.activeModronID := this.Memory.ReadActiveGameInstance() ? this.Memory.ReadActiveGameInstance() : 1 ; 1, 2, 3 for modron cores 1, 2, 3
        g_ServerCall.activePatronID := this.PatronID ;this.Memory.ReadPatronID() == "" ? g_ServerCall.activePatronID : this.Memory.ReadPatronID() ; 0 = no patron
        g_ServerCall.UpdateDummyData()
    }

	WaitForModronReset(timeout:=60000)
    {
        StartTime:=A_TickCount
        ElapsedTime:=0
        g_SharedData.UpdateOutbound("LoopString","Modron Resetting...")
        this.SetUserCredentials()
		if (this.steelbones!="" AND this.steelbones>0 AND this.sprint!="") ;Only try and manually save if it hasn't already happened - (steelbones > 0)
			g_serverCall.CallPreventStackFail(this.sprint,this.steelbones,"WaitForModronReset()",true)
        while (this.Memory.ReadResetting() AND ElapsedTime < timeout)
        {
            g_IBM.IBM_Sleep(20)
            ElapsedTime:=A_TickCount - StartTime
        }
        g_SharedData.UpdateOutbound("LoopString", "Loading z1...")
		g_IBM.IBM_Sleep(20)
        while(!this.Memory.ReadUserIsInited() AND this.Memory.ReadCurrentZone()<1 AND ElapsedTime<timeout)
        {
            g_IBM.IBM_Sleep(20)
            ElapsedTime:=A_TickCount - StartTime
        }
        if (ElapsedTime>=timeout)
			return false
        return true
    }

	GetWebRoot()
    {
        tempWebRoot := this.Memory.ReadWebRoot()
        httpString := StrSplit(tempWebRoot,":")[1]
        isWebRootValid := httpString == "http" or httpString == "https"
        g_ServerCall.webroot := isWebRootValid ? tempWebRoot : g_ServerCall.webroot
    }
}

class IC_BrivMaster_SharedData_Class ;In the shared file as the SettingsPath static is used by the hub for the save/load location
{
	static SettingsPath := A_LineFile . "\..\IC_BrivMaster_Settings.json"

	__New()
	{
		this.BossesHitThisRun:=0
		this.TotalBossesHit:=0
        this.TotalRollBacks:=0
        this.BadAutoProgress:=0
		this.IBM_RestoreWindow_Enabled:=false
		this.IBM_RunControl_DisableOffline:=false
		this.IBM_RunControl_ForceOffline:=false
		this.IBM_ProcessSwap:=false
		this.IBM_RunControl_CycleString:=""
		this.IBM_RunControl_StatusString:=""
		this.IBM_RunControl_StackString:=""
		this.IBM_BuyChests:=false
		this.RunLogResetNumber:=0
		this.RunLog:=""
		this.LoopString:=""
		this.LastCloseReason:=""
	}

	Close() ;Taken from what was IC_BrivGemFarmRun_SharedData_Class in IC_BrivGemFarm_Run.ahk
    {
        if (g_SF.Memory.ReadUserIsInited()="") ; Invalid game state
            ExitApp
        g_IBM.RouteMaster.WaitForTransition()
        g_IBM.RouteMaster.FallBackFromZone()
        g_IBM.RouteMaster.ToggleAutoProgress(false, false, true)
        ExitApp
    }

	ShowGUI()
    {
        Gui, Show, NA
    }

	Init()
    {
        this.UpdateSettingsFromFile()
		this.IBM_OutboundDirty:=false ;Track if we've made changes to the data so the hub doesn't make unnecessary checks
    }

    UpdateSettingsFromFile() ;Load settings from the GUI settings file.
    {
        settings:=g_SF.LoadObjectFromAHKJSON(IC_BrivMaster_SharedData_Class.SettingsPath)
        if (!IsObject(settings))
            return false
		for k,v in settings ;Load all settings
			g_IBM_Settings[k]:=v
		g_IBM.RefreshGemFarmWindow()
    }

	UpdateOutbound(key,value) ;Update if the value has changed at mark the outbound data as dirty
	{
		if (this[key]!=value)
		{
			this[key]:=value
			this.IBM_OutboundDirty:=true
		}
	}

	ResetRunStats() ;Resets per-run stats from the main object (boss hits, rollbacks, bad autoprogression). This allows them to all be cleared in one go without spam setting the IBM_OutboundDirty flag
	{
		this.BossesHitThisRun:=0
		this.TotalBossesHit:=0
        this.TotalRollBacks:=0
        this.BadAutoProgress:=0
		this.IBM_OutboundDirty:=true
	}

	UpdateOutbound_Increment(key) ;Increment a value, used for things like boss hit tracking
	{
		if (this.HasKey(key))
			this[key]++
		else
			this[key]:=1
		this.IBM_OutboundDirty:=true
	}
}

class IC_BrivMaster_InputManager_Class ;A class for managing input related matters
{
	keyList:={} ;Indexed by key per the script (e.g. "F1","ClickDmg"), contains the mapped Key, lParam for SendMessage for down, and lParam for SendMessage for up

	__new() ;Currently it is up to code using this to add the necessary keys TODO: Pass the object containing the HWnd to be used byRef, so it can be used with g_IBM.GameMaster.Hwnd in main and g_SF.Hwnd in hub?
	{
		this.KeyMap:={}
		this.SCKeyMap:={}
		KeyHelper.BuildVirtualKeysMap(this.KeyMap, this.SCKeyMap) ;Note: KeyHelper is in SH_KeyHelper.ahk
		this.gameFocus()
	}

	addKey(key)
	{
		if (!this.keyList.HasKey(key))
			this.keyList[key]:=new IC_BrivMaster_InputManager_Key_Class(key,this.KeyMap,this.SCKeyMap)
	}

	getKey(key)
	{
		if (!this.keyList.HasKey(key))
			this.addkey(key)
		return this.keyList[key]
	}

	gameFocus() ;We need a way to detect IC losing focus, as that appears to be the only case that this needs to be re-called
	{
		hwnd:=g_IBM.GameMaster.Hwnd
		ControlFocus,, ahk_id %hwnd%
	}
}

class IC_BrivMaster_InputManager_Key_Class ;Represents a single key. Used by IC_BrivMaster_InputManager_Class
{
	__new(key,KeyMap,SCKeyMap)
	{
		this.key:=key
		this.mappedKey:=KeyMap[key]
		sc:=SCKeyMap[key] << 16
        this.lparamDown := Format("0x{:X}", 0x0 | sc)
		this.lparamUp := Format("0x{:X}", 0xC0000001 | sc)
		this.tag:="" ;Used for tracking arbitary infomation on the key, e.g. the associated seat for F-keys
	}

	Press() ;Hold a key and do not release
	{
        hwnd:=g_IBM.GameMaster.Hwnd
		mk:=this.mappedKey ;We have to copy the variables locally due to limitations of AHK :(
		lD:=this.lparamDown
        ControlFocus,, ahk_id %hwnd%
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
	}

	Release() ;Release a key
	{
        hwnd:=g_IBM.GameMaster.Hwnd
		mk:=this.mappedKey
		lU:=this.lparamUp
        ControlFocus,, ahk_id %hwnd% ;As above
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,2000
	}

	KeyPress() ;Press then release a key
	{
		startCritical:=A_IsCritical ;Store existing state of critical
		Critical, On
        hwnd:=g_IBM.GameMaster.Hwnd
        mk:=this.mappedKey
		lD:=this.lparamDown
		lU:=this.lparamUp
		ControlFocus,, ahk_id %hwnd% ;As above
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,2000
        if (!startCritical) ;Only turn critical off if wasn't on when we entered this function
			Critical, Off
	}

	Press_Bulk() ;The _Bulk versions do not set ControlFocus, and are intended for code that will send a lot of input together (e.g. levelling) and that code will be responsible for calling ControlFocus once
	{
        hwnd:=g_IBM.GameMaster.Hwnd
		mk:=this.mappedKey ;We have to copy the variables locally due to limitations of AHK :(
		lD:=this.lparamDown
    	SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
	}

	Release_Bulk() ;Release a key
	{
        hwnd:=g_IBM.GameMaster.Hwnd
		mk:=this.mappedKey
		lU:=this.lparamUp
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,2000
	}

	KeyPress_Bulk() ;Press then release a key
	{
        hwnd:=g_IBM.GameMaster.Hwnd
        mk:=this.mappedKey
		lD:=this.lparamDown
		lU:=this.lparamUp
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,2000
	}
}

class IC_BrivMaster_EllywickDealer_Class ;A class for managing Ellywick's card draws and her ultimate use. This is based heavily on ImpEGamer's RNGWaitingRoom addon
{
	;HeroID's used: Elly=83, DM=99
	static ULTIMATE_RESOLUTION_TIME:=300 ;Real world milliseconds. Normally seems to be 0 to 140ms

	CasinoTimer := ObjBindMethod(this, "Casino")
	GemCardsNeeded:={} ;These are pairs of base and melf mode values, eg {0:2,1:3} for 2 without melf and 3 with
	MaxRedraws:={}
	MinCards:={}
	Complete:=false
	Redraws:=0 ;Current redraws
	UsedUlt:=false ;Tracks Elly's ult being in progress, as her cards are only cleared when it ENDS, despite the visual
	StatusString:=" STATUS=" ;Used to return basic information on problems (eg DM fails)

	Start()
	{
		timerFunction:=this.CasinoTimer
		SetTimer, %timerFunction%, 20, 0
		this.Casino() ;Is this useful here?
	}

	Stop()
	{
		this.ClearTimers()
	}

	Reset()
	{
		this.ClearTimers() ;Timers must be stopped BEFORE we set any variables, as otherwise the timer functions could 'un-reset' them afterwards
		this.Complete:=false
		this.Redraws:=0
		this.UsedUlt:=false ;This assumes Reset() will only be called after an adventure resets
		this.MaxRedraws:=g_IBM_Settings["IBM_Casino_Redraws_Base"]
		this.GemCardsNeeded:=g_IBM_Settings["IBM_Casino_Target_Base"]
		this.MinCards:=g_IBM_Settings["IBM_Casino_MinCards_Base"]
		this.StatusString:=""
	}

	ClearTimers()
	{
		timerFunction:=this.CasinoTimer
		SetTimer, %timerFunction%, Off
	}

	Casino()
	{
		if (g_Heroes[83].SetupDotMHandlerIfNeeded()) ;Check the effect handler has been set up
			return ;Re-check on next timer tick
		if (g_SF.Memory.ReadResetting() OR g_SF.Memory.ReadCurrentZone() == "" OR this.GetNumCards() == "")
			return
		if (this.UsedUlt AND g_Heroes[83].ReadEllywickUltimateActive()!=1) ;Check for completed ultimate
			this.UsedUlt:=False
		if (this.Complete AND !this.UsedUlt) ;In flight re-roll checks. Order of lazy ANDs matters to avoid calling CanUseEllyWickUlt() every tick
		{
			if (this.RedrawsLeft() AND this.ShouldDrawMoreCards() AND this.ShouldRedraw() AND this.CanUseEllyWickUlt()) ;When we exit the waitroom early we might still need to do a re-roll
				this.UseEllywickUlt()
		}
		else if (this.ShouldDrawMoreCards())
		{
			if (this.RedrawsLeft()) ;Use ultimate if it's not on cooldown and there are redraws left
			{
				 if (!this.UsedUlt AND this.ShouldRedraw())
					this.UseEllywickUlt()
			}
			else if (this.MinCards == 0 OR (!this.UsedUlt AND this.GetNumCards()>=this.MinCards)) ;If we want to release at a certain number of cards we need to wait for the ult to resolve to be able to count correctly
				this.WaitRoomExit()
		}
		else
			this.WaitRoomExit()
	}

	WaitRoomExit() ;Seperate so we can put some status strings in here
	{
		this.Complete:=true
		this.Stop()
	}

	DrawsLeft()
	{
		return 5 - this.GetNumCards()
	}

	RedrawsLeft()
	{
		return this.MaxRedraws - this.Redraws
	}

	ShouldDrawMoreCards()
	{
		if (this.GetNumCards() < this.MinCards)
			return true
		return g_Heroes[83].GetNumGemCards() < this.GemCardsNeeded
	}

	ShouldRedraw()
	{
		numCards:=this.GetNumCards()
		if (numCards==5)
			return true
		else if (numCards==0)
			return false
		return this.DrawsLeft() < this.GemCardsNeeded - g_Heroes[83].GetNumGemCards()
	}

	GetNumCards() ;Not encapsulated yet as results used for error checking
	{
		size:=g_Heroes[83].EFFECT_HANDLER_CARDS.cardsInHand.size.Read()
		if (size=="" AND !g_Heroes[83].ReadBenched())
		{
			this.StatusString.="FAIL-GetNumCards() was empty & Elly(Level:" . g_Heroes[83].ReadLevel() . "):"
		}
		return size == "" ? 0 : size
	}

	UseEllywickUlt()
	{
		if (g_SF.Memory.ReadTransitioning()) ;Do not try using the ults during a transition - possible source of Weird Stuff
			return
		if (this.CanUseEllyWickUlt())
		{
			this.UsedUlt:=true ;Set here to block-double presses, until we can confirm it has / hasn't been used
			Critical On ;Champion levelling between reading the ultimate key and pressing it could cause the incorrect button to be pressed
			startTime:=A_TickCount
			elapsedTime:=0
			ultActivated:=false
			retryCount:=g_Heroes[83].UseUltimate()
			if (retryCount=="")
				this.StatusString.="FAIL-Elly ultButton returned empty:"
			Critical Off
			while (!ultActivated AND elapsedTime < IC_BrivMaster_EllywickDealer_Class.ULTIMATE_RESOLUTION_TIME)
			{
				g_IBM.IBM_Sleep(15)
				ultActivated:=g_Heroes[83].ReadEllywickUltimateActive() ;Specific read for Elly from her handler, seemed more reliable than the cooldown
				if (ultActivated=="") ;This should be 0 or 1, if we fail to get a value something has gone weird
					this.StatusString.="FAIL-Elly ultActivated was empty:"
				elapsedTime:=A_TickCount - startTime
			}
			If (ultActivated)
			{
				this.Redraws++
				this.UseDMUlt()
			}
			else
			{
				this.UsedUlt:=False
				this.StatusString.="FAIL-Elly Ult Attempted but failed to register:"
			}
		}
		else
		{
			if (this.CanUseDMUlt()) ;Somehow Elly's ult isn't ready by DM's is - try using it
			{
				this.StatusString.="FAIL-Elly Ult not available-DM available:"
				this.UseDMUlt(0)
			}
			else
			{
				this.StatusString.="FAIL-Elly(Level:" . g_Heroes[83].ReadLevel() . ") Ult not available-DM(Level:" . g_Heroes[99].ReadLevel() . ") Ult not available-Lowered Max Rerolls to " . this.Redraws . ":"
				this.MaxRedraws:=this.Redraws ;Lower max re-rolls so we move on; this Casino is busted
			}
		}
	}

	CanUseEllyWickUlt()
	{
		return !g_Heroes[83].ReadBenched() AND this.IsEllywickUltReady()
	}

	IsEllywickUltReady()
	{
		ultCooldown:=g_Heroes[83].ReadUltimateCooldown()
		if (ultCooldown=="")
			this.StatusString.="FAIL-IsEllywickUltReady() ultCooldown returned empty:"
		return ultCooldown <= 0
	}

	UseDMUlt(sleepTime:=30) ;30ms default sleep is for use after Elly's ult triggers, to let the game process it
	{
		if (this.CanUseDMUlt())
		{
			g_IBM.IBM_Sleep(sleepTime)
			Critical On ;Champion levelling between reading the ultimate key and pressing it could cause the incorrect button to be pressed
			startTime:=A_TickCount
			elapsedTime:=0
			ultActivated:=false
			retryCount:=g_Heroes[99].UseUltimate()
			if (retryCount=="")
				this.StatusString.="FAIL-DM ultButton returned empty:"
			Critical Off
			while (!ultActivated AND elapsedTime < IC_BrivMaster_EllywickDealer_Class.ULTIMATE_RESOLUTION_TIME)
			{
				g_IBM.IBM_Sleep(15)
				ultCooldown:=g_Heroes[99].ReadUltimateCooldown()
				if (ultCooldown=="")
					this.StatusString.="FAIL-DM ultCooldown returned empty:"
				ultActivated:=ultCooldown > 0
				elapsedTime:=A_TickCount - startTime
			}
		}
	}

	CanUseDMUlt()
	{
		return !g_Heroes[99].ReadBenched() AND this.IsDMUltReady()
	}

	IsDMUltReady()
	{
		ultCooldown:=g_Heroes[99].ReadUltimateCooldown()
		if (ultCooldown=="")
			this.StatusString.="FAIL-IsDMUltReady() ultCooldown returned empty:"
		return ultCooldown <= 0
	}
}

class IC_BrivMaster_EllywickDealer_NonFarm_Class extends IC_BrivMaster_EllywickDealer_Class
{
	__New(minCards,maxCards)
	{
		this.minCards := minCards ;These are arrays indexed by card type, so 1 is Knight, 2 Moon, 3 Gem, 4 Fates, 5 Flames
		this.maxCards := maxCards
		g_Heroes[83].Reset() ;Reset Elly to clear any previous handlers. This will also create the hero object if necessary
		g_Heroes[99].Reset() ;And DM
	}

	Casino()
	{
		if (g_Heroes[83].SetupDotMHandlerIfNeeded()) ;Check the effect handler has been set up
			return ;Re-check on next timer tick
		if (g_SF.Memory.ReadResetting() OR g_SF.Memory.ReadCurrentZone() == "" OR this.GetNumCards() == "")
			return
		if (this.UsedUlt AND g_Heroes[83].ReadEllywickUltimateActive()!=1) ;Check for completed ultimate
			{
				this.UsedUlt:=False
			}
		remaining := this.GetRemainingCardsToDraw()
		withinMax:=this.CheckWithinMax()
		if (remaining == 0 AND this.GetNumCards() == 5 AND withinMax) ;We're done
		{
			this.WaitedForEllywickThisRun := true
			g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Complete after " . this.Redraws . " redraws")
			this.Stop()
		}
		else if (this.DrawsLeft() < remaining or !withinMax) ;Need to re-roll
		{
			If (this.CanUseEllyWickUlt() AND !this.UsedUlt)
			{
				g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Using ultimate")
				this.UseEllywickUlt()
			}
			else
				g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Waiting for ultimate")
		}
		else
			g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Drawing Cards")
	}

	GetRemainingCardsToDraw() ;Check the minimums to determine if we need to draw more
	{
		num := 0
		for cardType, numCards in this.minCards
		   num += Max(0, numCards - g_Heroes[83].GetNumCardsOfType(cardType))
		return num
	}

	CheckWithinMax() ;Check the maximums have not been exceeded, this is a pass/fail
	{
		for cardType, maxCards in this.maxCards
		{
			if (g_Heroes[83].GetNumCardsOfType(cardType) > maxCards)
				return False
		}
		return True
	}

	Start() ;Overriden as we don't need Melf mode & won't have had the structured resets of a farm runn
	{
		timerFunction:=this.CasinoTimer
		SetTimer, %timerFunction%, 20, 0
		g_Heroes[83].SetupDotMHandlerIfNeeded() ;.Reset() is called by the constructor, and we create a new object every run (for some reason)
		this.Casino()
	}

	UseEllywickUlt() ;This version does not report errors, as there's no log to report them to
	{
		if (g_SF.Memory.ReadTransitioning()) ;Do not try using the ults during a transition - possible source of Weird Stuff
			return
		if (this.CanUseEllyWickUlt())
		{
			this.UsedUlt:=true ;Set here to block-double presses, until we can confirm it has / hasn't been used
			Critical On ;Champion levelling between reading the ultimate key and pressing it could cause the incorrect button to be pressed
			startTime:=A_TickCount
			elapsedTime:=0
			ultActivated:=false
			g_Heroes[83].UseUltimate()
			Critical Off
			while (!ultActivated AND elapsedTime < IC_BrivMaster_EllywickDealer_Class.ULTIMATE_RESOLUTION_TIME)
			{
				Sleep 30 ;g_IBM.IBM_Sleep() will not be available in the hub
				ultActivated:=g_Heroes[83].ReadEllywickUltimateActive() ;No check for empty in this version as we can't do much about it (no log)
				elapsedTime:=A_TickCount - startTime
			}
			If (ultActivated)
			{
				this.Redraws++
				this.UseDMUlt()
			}
			else
			{
				this.UsedUlt:=False
			}
		}
		else
		{
			if (this.CanUseDMUlt()) ;Elly's ult isn't ready by DM's is - try using it
				this.UseDMUlt(0)
		}
	}

	UseDMUlt(sleepTime:=30) ;30ms default sleep is for use after Elly's ult triggers, to let the game process it
	{
		if (this.CanUseDMUlt())
		{
			Sleep sleepTime ;BrivGemFarm will not be available here
			Critical On ;Champion levelling between reading the ultimate key and pressing it could cause the incorrect button to be pressed
			startTime:=A_TickCount
			elapsedTime:=0
			ultActivated:=false
			retryCount:=g_Heroes[99].UseUltimate()
			Critical Off
			while (!ultActivated AND elapsedTime < IC_BrivMaster_EllywickDealer_Class.ULTIMATE_RESOLUTION_TIME)
			{
				Sleep 30 ;BrivGemFarm will not be available here
				ultActivated:=g_Heroes[99].ReadUltimateCooldown() > 0 ;Not checking for empty in this version as nothing much we can do (no log)
				elapsedTime:=A_TickCount - startTime
			}
		}
	}
}

class IC_BrivMaster_Budget_Zlib_Class ;A class for applying z-lib compatible compression. Badly. This is aimed at strings of <100 characters
{
	__New() ;Pre-computes binary values for various things to improve run-time performance for compression. Note that this is not done the other way as the current intended use is only performance sensitive for compression - so avoid wasting memory for things that may never be used
	{
		BASE64_CHARACTERS:="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ;RFC 4648 S4, base64
		this.BASE64_TABLE:={} ;Binary key to character
		this.BASE64_REVERSE_TABLE:={} ;Character key to binary string
		Loop, Parse, BASE64_CHARACTERS
		{
			bin:=this.IntToBinaryString(A_Index-1,6)
			this.BASE64_TABLE[bin . ""]:=A_LoopField . "" ;Note: bin must be forced to a string, as whilst it would work as a number as long as processing used the same, the reverse table must have the leading zeros
			this.BASE64_REVERSE_TABLE[ASC(A_LoopField)]:=bin . "" ;This cannot use the character directly as object members are case-insensitive
		}
		this.HOFFMAN_CHARACTER_TABLE:={}
		loop 256
		{
			this.HOFFMAN_CHARACTER_TABLE[A_Index-1]:=this.CodeToBinaryString(A_Index-1)
		}
		this.LENGTH_TABLE:={}
		loop 256
		{
			this.LENGTH_TABLE[A_INDEX+2]:=this.GetLengthCode(A_INDEX+2) ;3 to 258
		}
		this.DISTANCE_TABLE:={}
		loop 64 ;As there are 32K distance values, only pre-calculate the short common ones. Others will be done as needed
		{
			this.DISTANCE_TABLE[A_INDEX]:=this.CalcDistanceCode(A_Index)
		}
	}

	;----------------------------------

	Deflate(inputString,minMatch:=3,maxMatch:=258) ;minMatch must be at least 3, and maxMatch must be at most 258 TODO: This is in dire need of some error handling
	{
		pos:=1
		inputLength:=StrLen(inputString)
		output:="" ;Note: Accumulating the existing output appears to be a tiny bit faster than not doing so and having more complex string operations
		outputBinary:="00011110" . "01011011" . "110" ;2 bytes of header (LSB first), 3 bits of block header
		while(pos<=inputLength)
		{
			if(inputLength-pos+1>=minMatch) ;If there are enough characters left for a minimum match. +1 is there because the character in the current position is included
			{
				match:=1
				distance:=0
				curLookahead:=minMatch
				while(match AND pos+curLookahead-1<=inputLength AND curLookahead<=maxMatch) ;-1 as the current character is included (i.e. SubStr(haystack,startPosition,3) takes 3 characters starting from position 1, so ends at startPosition+2
				{
					lookAhead:=SubStr(inputString,pos,curLookahead)
					match:=inStr(output,lookAhead,1,0) ;Look for an exact match, looking backwards (right to left). MUST be case-sensitive
					if(match AND pos-match<=32768)
					{
						distance:=pos-match
						lastFoundlookAhead:=lookAhead
						matchLength:=curLookahead ;We can use curLookahead instead of StrLen(lookAhead) as we checked in the while clause that there is enough remaining characters to fill the subStr
					}
					else ;Look for repeats, e.g. if the lookahead is abc, see if the previous characters were abc, if aaa, see if previous character was a
					{
						loop % curLookahead-1 ;An exact match would be covered above
						{
							endChunk:=SubStr(output,-A_Index+1) ;Start of 0 means return last character, -2 means return the last 3 characters
							if(this.StringRepeat(endChunk,curLookahead)==lookAhead)
							{
								match:=A_Index
								distance:=match
								matchLength:=curLookahead
								lastFoundlookAhead:=lookAhead
							}
						}
					}
					curLookahead++
				}
				if(distance) ;3+ char string exists in output buffer
				{
					output.=lastFoundlookAhead
					outputBinary.=this.LENGTH_TABLE[matchLength]
					outputBinary.=this.GetDistanceCode(distance)
					pos+=matchLength
					Continue
				}
			}
			char:=SubStr(inputString,pos,1)
			output.=char

			byteValue := NumGet(&inputUTF8, pos, "UChar")   ; 0..255
			code:=ASC(char)
			if(code>255)
				OutputDebug % char . " - " . code . "`n"

			outputBinary.=this.HOFFMAN_CHARACTER_TABLE[ASC(char)]
			pos++
		}
		outputBinary.="0000000" ;End of block, 256-256=0 as 7 bits, which we might as well hard-code
		while(MOD(StrLen(outputBinary),8)) ;Pad to byte boundry
			outputBinary.="0"
		outputBinary:=this.ReverseByteOrder(outputBinary) ;Reverse prior to adding Adler32
		adler32:=this.Adler32(inputString)
		Loop 32
			outputBinary.=((adler32 >> (32-A_Index)) & 1)
		outputBase64:=this.BinaryStringToBase64(outputBinary)
		return outputBase64
	}

	;----------------------------------

	Inflate(inputString)
	{
		bitString:=this.Base64ToBinaryString(inputString)
		BitStream:=new IC_BrivMaster_Budget_Zlib_Class.IC_BrivMaster_Budget_Zlib_Read_Bitstream_Class(bitString)
		CMF_CM:=BitStream.ReadAsLSB(4)
		CMF_CINFO:=BitStream.ReadAsLSB(4) ;Only used for the <=7 check
		FLG_FCHECK:=BitStream.ReadAsLSB(5)
		FLG_FDICT:=BitStream.ReadAsLSB(1)+0 ;Not used here (only valid input is 0) but part of the FCHECK checksum
		FLG_FLEVEL:=BitStream.ReadAsLSB(2) ;FLEVEL is not used in decompression, but is part of the FCHECK checksum
		HEADER:=CMF_CINFO . CMF_CM . FLG_FLEVEL . FLG_FDICT . FLG_FCHECK ;CMF*256 + FLG
		CMF_CM:=this.BinaryStringToInt(CMF_CM)
		CMF_CINFO:=this.BinaryStringToInt(CMF_CINFO)
		if(Mod(this.BinaryStringToInt(HEADER),31)) ;header checksum
		{
			return "" ;Need to throw error here
		}
		if(CMF_CM!=8)
		{
			return "" ;Need to throw error here
		}
		if(CMF_CINFO>7) ;Not used, but check valid per RFC
		{
			return "" ;Need to throw error here
		}
		if(FLG_FDICT!=0)
		{
			return "" ;Need to throw error here
		}
		;Build lookup tables. These are not pre-calculatea as the current use case for this class is performance-sensitive deflate, but insensitive inflate, so doing so would just waste memory
		LIT_LEN_TABLE:={} ;Table of binary strings to code value
		Loop 144 ;0-143	8 bits	codes	00110000 .. 10111111
			LIT_LEN_TABLE[this.IntToBinaryString(0x30 + A_Index-1,8) . ""] := A_Index-1
		Loop 112 ;144-255	9 bits	codes	110010000 .. 111111111
			LIT_LEN_TABLE[this.IntToBinaryString(0x190 + A_Index-1,9) . ""] := 143 + A_Index
		Loop 24 ;256-279	7 bits	codes	0000000 .. 0010111
			LIT_LEN_TABLE[this.IntToBinaryString(A_Index-1,7) . ""] := 256 + A_Index-1
		Loop 8 ;280-287	8 bits	codes	11000000 .. 11000111
			LIT_LEN_TABLE[this.IntToBinaryString(0xC0 + A_Index-1,8) . ""] := 280 + A_Index-1
		LEN_CODE_EXTRA:=[0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
		LEN_CODE_TABLE:={} ;TODO: Could this technique be applied to generating the deflate lookups?
		curLength:=3 ;Starts at 3
		loop 29 ;29 values, 257 through 285
		{
			extra:=LEN_CODE_EXTRA[A_Index]
			LEN_CODE_TABLE[A_Index+256,"L"]:=curLength
			LEN_CODE_TABLE[A_Index+256,"E"]:=extra
			curLength+=2 ** extra
		}
		LEN_CODE_TABLE[285,"L"]:=258 ;Exception for 285, as 284 could encode this length but it is given this unique code, presumably because it's expected to occur a lot
		DIST_CODE_EXTRA:=[0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13]
		DIST_CODE_TABLE:={}
		curDist:=1 ;Starts at 1
		Loop 30 ;Codes 0 through 29. 30 and 31 are unused
		{
			extra:=DIST_CODE_EXTRA[A_Index] ;No -1 as the hard-coded list is not 0-indexed
			DIST_CODE_TABLE[A_Index-1,"D"]:=curDist
			DIST_CODE_TABLE[A_Index-1,"E"]:=extra
			curDist+=2 ** extra
		}
		;Process blocks
		BFINAL:=0
		dataOutput:=""
		while(!BFINAL)
			{
			BFINAL:=BitStream.ReadAsLSB(1)+0 ;Convert straight to int since only single bit
			BTYPE:=BitStream.ReadAsLSB(2) ;LSB first to confuse us
			if(BTYPE!="01")
			{
				return "" ;Need to throw error here
			}
			loop
			{
				symbol:=BitStream.ReadAsMSB(7) ;Start with 7
				if(!LIT_LEN_TABLE.HasKey(symbol . ""))
				{
					symbol.=BitStream.ReadAsMSB(1)
					if(!LIT_LEN_TABLE.HasKey(symbol . ""))
					{
						symbol.=BitStream.ReadAsMSB(1)
						if(!LIT_LEN_TABLE.HasKey(symbol . ""))
						{
							return "" ;Need to throw error here
						}
					}
				}
				code:=LIT_LEN_TABLE[symbol . ""]
				if(code<256) ;Literal
					dataOutput.=Chr(code)
				else if(code==256) ;End of block
				{
					break ;Break the inner block loop
				}
				else if(code<=285) ;Length code
				{
					length:=LEN_CODE_TABLE[code].L
					extraBits:=LEN_CODE_TABLE[code].E
					if(extraBits)
					{
						extraBin:=BitStream.ReadAsLSB(extraBits)
						extraDec:=this.BinaryStringToInt(extraBin)
						length+=extraDec
					}
					distanceBinary:=BitStream.ReadAsMSB(5)
					distanceBase:=this.BinaryStringToInt(distanceBinary)
					distance:=DIST_CODE_TABLE[distanceBase].D
					extraBits:=DIST_CODE_TABLE[distanceBase].E
					if(extraBits)
					{
						extraBin:=BitStream.ReadAsLSB(extraBits)
						extraDec:=this.BinaryStringToInt(extraBin)
						distance+=extraDec
					}
					repeat:=SubStr(dataOutput,StrLen(dataOutput)-distance+1,length) ;If L>D this will return less than the requested length, handled below
					if(length>distance) ;If L>D repeat what we have to fill L
						dataOutput.=this.StringRepeat(repeat,length)
					else
						dataOutput.=repeat
				}
				else
				{
					return "" ;Need to throw error here
				}
			}
		}
		;Read to byte boundry
		BitStream.MoveToNextByte()
		ADLER32Bin:=BitStream.ReadAsLSB(8) . BitStream.ReadAsLSB(8) . BitStream.ReadAsLSB(8) . BitStream.ReadAsLSB(8) ;Since we're set up to read the LSB-first bitstream this has to be read byte-by-byte
		ADLER32Int:=this.BinaryStringToInt(ADLER32Bin)
		outputADLER32:=this.ADLER32(dataOutput)
		if(outputADLER32==ADLER32Int)
			return dataOutput
		else
		{
			return "" ;Need to throw error here
		}
	}

	;----------------------------------

	BinaryStringToBase64(string) ;Requires string to have a length that is a multiple of 8
	{
		pos:=1
		stringLength:=StrLen(string)
		while(pos<stringLength)
		{
			slice:=SubStr(string,pos,24) ;Take 24bits at a time
			sliceLen:=StrLen(slice)
			if(sliceLen==24) ;Standard case
			{
				loop, 4
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6) . ""] ;Concatenate with "" to force to string
				pos+=24
			}
			else if (sliceLen==16) ;16 bits, need to pad with 2 zeros to reach 18 and be divisible by 3, then add an = to replace the last 6-set
			{
				slice.="00"
				loop, 3
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6) . ""]
				accu.="="
				Break ;Since we're out of data
			}
			else if (sliceLen==8) ;8 bits, need to pad with 4 zeros to reach 12 and be divisible by 2, then add == to replace the last two 6-sets
			{
				slice.="0000"
				loop, 2
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6) . ""]
				accu.="=="
				Break ;Since we're out of data
			}
		}
		return accu
	}

	Base64ToBinaryString(base64String)
	{
		pos:=1
		stringLength:=StrLen(base64String)
		if(mod(stringLength,4))
		{
			OutputDebug % "Base64ToBinaryString(): Length [" . stringLength . "] is not a multiple of 4`n"
			return ""
		}
		accu:=""
		while(pos<stringLength)
		{
			slice:=SubStr(base64String,pos,4) ;4 characters at a time, to form 4x6=24bits=3bytes
			chars:=StrSplit(slice)
			if(chars[3]=="=" AND chars[4]=="=") ;Double pad, 1 byte of input + 4 added zeros, so take only the first 2 bits from the 2nd char
			{
				accu.=this.BASE64_REVERSE_TABLE[ASC(chars[1])] . subStr(this.BASE64_REVERSE_TABLE[ASC(chars[2])],1,2)
			}
			else if(chars[4]=="=") ;Single pad, 2 bytes of input + 2 added zeros, so take only the first 4 bits from the 3rd char
			{
				accu.=this.BASE64_REVERSE_TABLE[ASC(chars[1])] . this.BASE64_REVERSE_TABLE[ASC(chars[2])] . subStr(this.BASE64_REVERSE_TABLE[ASC(chars[3])],1,4)
			}
			else
			{
				accu.=this.BASE64_REVERSE_TABLE[ASC(chars[1])] . this.BASE64_REVERSE_TABLE[ASC(chars[2])] . this.BASE64_REVERSE_TABLE[ASC(chars[3])] . this.BASE64_REVERSE_TABLE[ASC(chars[4])]
			}
			pos+=4
		}
		return accu
	}

	StringRepeat(string,length) ;Repeats string until Length is reached, including partial repeats. Eg string=abc length=5 gives abcab
	{
		loop % Ceil(length/StrLen(string))
			output.=string
		return SubStr(output,1,length)
	}

	Adler32(data) ;Per RFC 1950
	{
		s1:=1
		s2:=0
		Loop Parse, data
		{
			byte:=Asc(A_LoopField)
			s1:=Mod(s1 + byte, 65521)
			s2:=Mod(s2 + s1, 65521)
		}
		return (s2 << 16) | s1
	}

	ReverseByteOrder(string) ;We assemble LSB-first as required for the hoffman encoding, but need to be MSB-first for the Base64 conversion. Requires string to have length that is a multiple of 8. Doing the 8 bits explictly seems fractionally faster than using a loop
	{
		pos:=1
		while(pos<StrLen(string))
		{
			accu.=SubStr(string,pos+7,1)
			accu.=SubStr(string,pos+6,1)
			accu.=SubStr(string,pos+5,1)
			accu.=SubStr(string,pos+4,1)
			accu.=SubStr(string,pos+3,1)
			accu.=SubStr(string,pos+2,1)
			accu.=SubStr(string,pos+1,1)
			accu.=SubStr(string,pos,1)
			pos+=8
		}
		return accu
	}

	GetDistanceCode(distance) ;Uses the lookup table for values up to 64, and calls the calculation of higher distances
	{
		if(distance<=64)
			return this.DISTANCE_TABLE[distance]
		else
			return this.CalcDistanceCode(distance)
	}

	CodeToBinaryString(code) ;Takes an ASCII character code, e.g. "97" for "a" and returns the fixed Hoffman-encouded binary representation as a LSB-first string. Used to pre-calculate the lookup table
	{
		if(code>=0 AND code<=143)
		{
			code+=0x30
			bits:=8
		}
		else if(code>=144 AND code<=255)
		{
			code+=0x100
			bits:=9
		}
		else
			MSGBOX % "Invalid character code"
		return this.IntToBinaryString(code,bits)
	}

	GetLengthCode(length) ;Used to pre-calculate the lookup table
	{
		if(length==3) ;Simple cases, no extra bits
			return "0000001"
		else if(length==4)
			return "0000010"
		else if(length==5)
			return "0000011"
		else if(length==6)
			return "0000100"
		else if(length==7)
			return "0000101"
		else if(length==8)
			return "0000110"
		else if(length==9)
			return "0000111"
		else if(length==10)
			return "0001000"
		else if(length<=12)
			return "0001001" . this.IntToBinaryStringLSB(length-11,1)
		else if(length<=14)
			return "0001010" . this.IntToBinaryStringLSB(length-13,1)
		else if(length<=16)
			return "0001011" . this.IntToBinaryStringLSB(length-15,1)
		else if(length<=18)
			return "0001100" . this.IntToBinaryStringLSB(length-17,1)
		else if(length<=22)
			return "0001101" . this.IntToBinaryStringLSB(length-19,2)
		else if(length<=26)
			return "0001110" . this.IntToBinaryStringLSB(length-23,2)
		else if(length<=30)
			return "0001111" . this.IntToBinaryStringLSB(length-27,2)
		else if(length<=34)
			return "0010000" . this.IntToBinaryStringLSB(length-31,2)
		else if(length<=42)
			return "0010001" . this.IntToBinaryStringLSB(length-35,3)
		else if(length<=50)
			return "0010010" . this.IntToBinaryStringLSB(length-43,3)
		else if(length<=58)
			return "0010011" . this.IntToBinaryStringLSB(length-51,3)
		else if(length<=66)
			return "0010100" . this.IntToBinaryStringLSB(length-59,3)
		else if(length<=82)
			return "0010101" . this.IntToBinaryStringLSB(length-67,4)
		else if(length<=98)
			return "0010110" . this.IntToBinaryStringLSB(length-83,4)
		else if(length<=114)
			return "0010111" . this.IntToBinaryStringLSB(length-99,4)
		else if(length<=130)
			return "11000000" . this.IntToBinaryStringLSB(length-115,4)
		else if(length<=162)
			return "11000001" . this.IntToBinaryStringLSB(length-131,5)
		else if(length<=194)
			return "11000010" . this.IntToBinaryStringLSB(length-163,5)
		else if(length<=226)
			return "11000011" . this.IntToBinaryStringLSB(length-195,5)
		else if(length<=257)
			return "11000100" . this.IntToBinaryStringLSB(length-227,5)
		else if(length==258)
			return "11000101"
	}

	CalcDistanceCode(distance)
	{
		if(distance<=4)
			return this.IntToBinaryString(distance-1,5)
		else if(distance<=6)
			return "00100" . this.IntToBinaryStringLSB(distance-5,1)
		else if(distance<=8)
			return "00101" . this.IntToBinaryStringLSB(distance-7,1)
		else if(distance<=12)
			return "00110" . this.IntToBinaryStringLSB(distance-9,2)
		else if(distance<=16)
			return "00111" . this.IntToBinaryStringLSB(distance-13,2)
		else if(distance<=24)
			return "01000" . this.IntToBinaryStringLSB(distance-17,3)
		else if(distance<=32)
			return "01001" . this.IntToBinaryStringLSB(distance-25,3)
		else if(distance<=48)
			return "01010" . this.IntToBinaryStringLSB(distance-33,4)
		else if(distance<=64)
			return "01011" . this.IntToBinaryStringLSB(distance-49,4)
		else if(distance<=96)
			return "01100" . this.IntToBinaryStringLSB(distance-65,5)
		else if(distance<=128)
			return "01101" . this.IntToBinaryStringLSB(distance-97,5)
		else if(distance<=192)
			return "01110" . this.IntToBinaryStringLSB(distance-129,6)
		else if(distance<=256)
			return "01111" . this.IntToBinaryStringLSB(distance-193,6)
		else if(distance<=384)
			return "10000" . this.IntToBinaryStringLSB(distance-257,7)
		else if(distance<=512)
			return "10001" . this.IntToBinaryStringLSB(distance-385,7)
		else if(distance<=768)
			return "10010" . this.IntToBinaryStringLSB(distance-513,8)
		else if(distance<=1024)
			return "10011" . this.IntToBinaryStringLSB(distance-769,8)
		else if(distance<=1536)
			return "10100" . this.IntToBinaryStringLSB(distance-1025,9)
		else if(distance<=2048)
			return "10101" . this.IntToBinaryStringLSB(distance-1537,9)
		else if(distance<=3072)
			return "10110" . this.IntToBinaryStringLSB(distance-2049,10)
		else if(distance<=4096)
			return "10111" . this.IntToBinaryStringLSB(distance-3073,10)
		else if(distance<=6144)
			return "11000" . this.IntToBinaryStringLSB(distance-4097,11)
		else if(distance<=8192)
			return "11001" . this.IntToBinaryStringLSB(distance-6145,11)
		else if(distance<=12288)
			return "11010" . this.IntToBinaryStringLSB(distance-8193,12)
		else if(distance<=16384)
			return "11011" . this.IntToBinaryStringLSB(distance-12289,12)
		else if(distance<=24576)
			return "11100" . this.IntToBinaryStringLSB(distance-16385,13)
		else if(distance<=32768)
			return "11101" . this.IntToBinaryStringLSB(distance-24577,13)
	}

	IntToBinaryString(code,bits) ;Takes an Int and returns a binary string
	{
		Loop % bits
			bin:=(code >> (A_Index-1)) & 1 . bin
		return bin
	}

	IntToBinaryStringLSB(code,bits) ;Takes an Int and returns a binary string, LSB first
	{
		Loop % bits
			bin:=bin . (code >> (A_Index-1)) & 1
		return bin
	}

	BinaryStringToInt(bitString) ;Takes a binary string and returns an int, MSB at the start of the string
	{
		bits:=StrLen(bitString)
		int:=0
		loop, Parse, bitString
			int+=A_LoopField * 2**(bits-A_Index)
		return int
	}

	BinaryStringToIntLSB(bitString) ;Takes a binary string and returns an int, LSB at the start of the string
	{
		int:=0
		loop, Parse, bitString
			int+=A_LoopField * 2**(A_Index-1)
		return int
	}

	class IC_BrivMaster_Budget_Zlib_Read_Bitstream_Class ;TODO: Could we do something like this to assemble the string in the appropriate order when deflating? I.e. fill a byte buffer LSB first, and then add that byte once full. Maybe not worth the effort over doing something properly in binary
	{
		__new(binaryString)
		{
			this.BinaryString:=binaryString
			this.BitIndex:=0 ;Bit we are currently on within the byte, 0 = LSB, 7-MSB
			this.ByteIndex:=0 ;Byte we are currently on
		}

		ReadAsMSB(numBits) ;Reads the given number of bits, starting from the LSB of each byte, forming an MSB-based value, e.g. the hoffman symbols in a stream
		{
			bits:=""
			loop %numBits%
				bits.=this.NextBit()
			return bits
		}

		ReadAsLSB(numBits) ;Reads the given number of bits, starting from the LSB of each byte, forming an LSB-based value, e.g. the BTYPE value
		{
			bits:=""
			loop %numBits%
				bits:=this.NextBit() . bits
			return bits
		}

		NextBit()
		{
			bit:=SubStr(this.BinaryString,this.ByteIndex*8+(8-this.BitIndex),1)
			if(this.BitIndex==7)
			{
				this.BitIndex:=0
				this.ByteIndex++
			}
			else
				this.BitIndex++
			return bit
		}

		MoveToNextByte() ;Moves to the start of the next byte
		{
			if(this.BitIndex) ;If bitIndex isn't already 0 (first bit), move forward to the start of the next byte
			{
				this.BitIndex:=0
				this.ByteIndex++
			}
		}
	}
}

/**
 * Lib: JSON.ahk
 *     JSON lib for AutoHotkey.
 * Version:
 *     v2.1.3 [updated 04/18/2016 (MM/DD/YYYY)]
 * License:
 *     WTFPL [http://wtfpl.net/]
 * Requirements:
 *     Latest version of AutoHotkey (v1.1+ or v2.0-a+)
 * Installation:
 *     Use #Include JSON.ahk or copy into a function library folder and then
 *     use #Include <JSON>
 * Links:
 *     GitHub:     - https://github.com/cocobelgica/AutoHotkey-JSON
 *     Forum Topic - http://goo.gl/r0zI8t
 *     Email:      - cocobelgica <at> gmail <dot> com
 */


/**
 * Class: JSON
 *     The JSON object contains methods for parsing JSON and converting values
 *     to JSON. Callable - NO; Instantiable - YES; Subclassable - YES;
 *     Nestable(via #Include) - NO.
 * Methods:
 *     Load() - see relevant documentation before method definition header
 *     Dump() - see relevant documentation before method definition header
 */
class AHK_JSON ;Irisiri - renamed as SH already has a JSON class powered by JavaScript
{
	/**
	 * Method: Load
	 *     Parses a JSON string into an AHK value
	 * Syntax:
	 *     value := JSON.Load( text [, reviver ] )
	 * Parameter(s):
	 *     value      [retval] - parsed value
	 *     text    [in, ByRef] - JSON formatted string
	 *     reviver   [in, opt] - function object, similar to JavaScript's
	 *                           JSON.parse() 'reviver' parameter
	 */
	class Load extends AHK_JSON.Functor
	{
		Call(self, ByRef text, reviver:="")
		{
			this.rev := IsObject(reviver) ? reviver : false
		; Object keys(and array indices) are temporarily stored in arrays so that
		; we can enumerate them in the order they appear in the document/text instead
		; of alphabetically. Skip if no reviver function is specified.
			this.keys := this.rev ? {} : false

			static quot := Chr(34), bashq := "\" . quot
			     , json_value := quot . "{[01234567890-tfn"
			     , json_value_or_array_closing := quot . "{[]01234567890-tfn"
			     , object_key_or_object_closing := quot . "}"

			key := ""
			is_key := false
			root := {}
			stack := [root]
			next := json_value
			pos := 0

			while ((ch := SubStr(text, ++pos, 1)) != "") {
				if InStr(" `t`r`n", ch)
					continue
				if !InStr(next, ch, 1)
					this.ParseError(next, text, pos)

				holder := stack[1]
				is_array := holder.IsArray

				if InStr(",:", ch) {
					next := (is_key := !is_array && ch == ",") ? quot : json_value

				} else if InStr("}]", ch) {
					ObjRemoveAt(stack, 1)
					next := stack[1]==root ? "" : stack[1].IsArray ? ",]" : ",}"

				} else {
					if InStr("{[", ch) {
					; Check if Array() is overridden and if its return value has
					; the 'IsArray' property. If so, Array() will be called normally,
					; otherwise, use a custom base object for arrays
						static json_array := Func("Array").IsBuiltIn || ![].IsArray ? {IsArray: true} : 0

					; sacrifice readability for minor(actually negligible) performance gain
						(ch == "{")
							? ( is_key := true
							  , value := {}
							  , next := object_key_or_object_closing )
						; ch == "["
							: ( value := json_array ? new json_array : []
							  , next := json_value_or_array_closing )

						ObjInsertAt(stack, 1, value)

						if (this.keys)
							this.keys[value] := []

					} else {
						if (ch == quot) {
							i := pos
							while (i := InStr(text, quot,, i+1)) {
								value := StrReplace(SubStr(text, pos+1, i-pos-1), "\\", "\u005c")

								static tail := A_AhkVersion<"2" ? 0 : -1
								if (SubStr(value, tail) != "\")
									break
							}

							if (!i)
								this.ParseError("'", text, pos)

							  value := StrReplace(value,  "\/",  "/")
							, value := StrReplace(value, bashq, quot)
							, value := StrReplace(value,  "\b", "`b")
							, value := StrReplace(value,  "\f", "`f")
							, value := StrReplace(value,  "\n", "`n")
							, value := StrReplace(value,  "\r", "`r")
							, value := StrReplace(value,  "\t", "`t")

							pos := i ; update pos

							i := 0
							while (i := InStr(value, "\",, i+1)) {
								if !(SubStr(value, i+1, 1) == "u")
									this.ParseError("\", text, pos - StrLen(SubStr(value, i+1)))

								uffff := Abs("0x" . SubStr(value, i+2, 4))
								if (A_IsUnicode || uffff < 0x100)
									value := SubStr(value, 1, i-1) . Chr(uffff) . SubStr(value, i+6)
							}

							if (is_key) {
								key := value, next := ":"
								continue
							}

						} else {
							value := SubStr(text, pos, i := RegExMatch(text, "[\]\},\s]|$",, pos)-pos)

							static number := "number", integer :="integer"
							if value is %number%
							{
								if value is %integer%
									value += 0
							}
							else if (value == "true" || value == "false")
								value := %value% + 0
							else if (value == "null")
								value := ""
							else
							; we can do more here to pinpoint the actual culprit
							; but that's just too much extra work.
								this.ParseError(next, text, pos, i)

							pos += i-1
						}

						next := holder==root ? "" : is_array ? ",]" : ",}"
					} ; If InStr("{[", ch) { ... } else

					is_array? key := ObjPush(holder, value) : holder[key] := value

					if (this.keys && this.keys.HasKey(holder))
						this.keys[holder].Push(key)
				}

			} ; while ( ... )

			return this.rev ? this.Walk(root, "") : root[""]
		}

		ParseError(expect, ByRef text, pos, len:=1)
		{
			static quot := Chr(34), qurly := quot . "}"

			line := StrSplit(SubStr(text, 1, pos), "`n", "`r").Length()
			col := pos - InStr(text, "`n",, -(StrLen(text)-pos+1))
			msg := Format("{1}`n`nLine:`t{2}`nCol:`t{3}`nChar:`t{4}"
			,     (expect == "")     ? "Extra data"
			    : (expect == "'")    ? "Unterminated string starting at"
			    : (expect == "\")    ? "Invalid \escape"
			    : (expect == ":")    ? "Expecting ':' delimiter"
			    : (expect == quot)   ? "Expecting object key enclosed in double quotes"
			    : (expect == qurly)  ? "Expecting object key enclosed in double quotes or object closing '}'"
			    : (expect == ",}")   ? "Expecting ',' delimiter or object closing '}'"
			    : (expect == ",]")   ? "Expecting ',' delimiter or array closing ']'"
			    : InStr(expect, "]") ? "Expecting JSON value or array closing ']'"
			    :                      "Expecting JSON value(string, number, true, false, null, object or array)"
			, line, col, pos)

			static offset := A_AhkVersion<"2" ? -3 : -4
			throw Exception(msg, offset, SubStr(text, pos, len))
		}

		Walk(holder, key)
		{
			value := holder[key]
			if IsObject(value) {
				for i, k in this.keys[value] {
					; check if ObjHasKey(value, k) ??
					v := this.Walk(value, k)
					if (v != AHK_JSON.Undefined)
						value[k] := v
					else
						ObjDelete(value, k)
				}
			}

			return this.rev.Call(holder, key, value)
		}
	}

	/**
	 * Method: Dump
	 *     Converts an AHK value into a JSON string
	 * Syntax:
	 *     str := JSON.Dump( value [, replacer, space ] )
	 * Parameter(s):
	 *     str        [retval] - JSON representation of an AHK value
	 *     value          [in] - any value(object, string, number)
	 *     replacer  [in, opt] - function object, similar to JavaScript's
	 *                           JSON.stringify() 'replacer' parameter
	 *     space     [in, opt] - similar to JavaScript's JSON.stringify()
	 *                           'space' parameter
	 */
	class Dump extends AHK_JSON.Functor
	{
		Call(self, value, replacer:="", space:="")
		{
			this.rep := IsObject(replacer) ? replacer : ""

			this.gap := ""
			if (space) {
				static integer := "integer"
				if space is %integer%
					Loop, % ((n := Abs(space))>10 ? 10 : n)
						this.gap .= " "
				else
					this.gap := SubStr(space, 1, 10)

				this.indent := "`n"
			}

			return this.Str({"": value}, "")
		}

		Str(holder, key)
		{
			value := holder[key]

			if (this.rep)
				value := this.rep.Call(holder, key, ObjHasKey(holder, key) ? value : JSON.Undefined)

			if IsObject(value) {
			; Check object type, skip serialization for other object types such as
			; ComObject, Func, BoundFunc, FileObject, RegExMatchObject, Property, etc.
				static type := A_AhkVersion<"2" ? "" : Func("Type")
				if (type ? type.Call(value) == "Object" : ObjGetCapacity(value) != "") {
					if (this.gap) {
						stepback := this.indent
						this.indent .= this.gap
					}

					is_array := value.IsArray
				; Array() is not overridden, rollback to old method of
				; identifying array-like objects. Due to the use of a for-loop
				; sparse arrays such as '[1,,3]' are detected as objects({}).
					if (!is_array) {
						for i in value
							is_array := i == A_Index
						until !is_array
					}

					str := ""
					if (is_array) {
						Loop, % value.Length() {
							if (this.gap)
								str .= this.indent

							v := this.Str(value, A_Index)
							str .= (v != "") ? v . "," : "null,"
						}
					} else {
						colon := this.gap ? ": " : ":"
						for k in value {
							v := this.Str(value, k)
							if (v != "") {
								if (this.gap)
									str .= this.indent

								str .= this.Quote(k) . colon . v . ","
							}
						}
					}

					if (str != "") {
						str := RTrim(str, ",")
						if (this.gap)
							str .= stepback
					}

					if (this.gap)
						this.indent := stepback

					return is_array ? "[" . str . "]" : "{" . str . "}"
				}

			} else ; is_number ? value : "value"
				return ObjGetCapacity([value], 1)=="" ? value : this.Quote(value)
		}

		Quote(string)
		{
			static quot := Chr(34), bashq := "\" . quot

			if (string != "") {
				  string := StrReplace(string,  "\",  "\\")
				; , string := StrReplace(string,  "/",  "\/") ; optional in ECMAScript
				, string := StrReplace(string, quot, bashq)
				, string := StrReplace(string, "`b",  "\b")
				, string := StrReplace(string, "`f",  "\f")
				, string := StrReplace(string, "`n",  "\n")
				, string := StrReplace(string, "`r",  "\r")
				, string := StrReplace(string, "`t",  "\t")

				static rx_escapable := A_AhkVersion<"2" ? "O)[^\x20-\x7e]" : "[^\x20-\x7e]"
				while RegExMatch(string, rx_escapable, m)
					string := StrReplace(string, m.Value, Format("\u{1:04x}", Ord(m.Value)))
			}

			return quot . string . quot
		}
	}

	/**
	 * Property: Undefined
	 *     Proxy for 'undefined' type
	 * Syntax:
	 *     undefined := JSON.Undefined
	 * Remarks:
	 *     For use with reviver and replacer functions since AutoHotkey does not
	 *     have an 'undefined' type. Returning blank("") or 0 won't work since these
	 *     can't be distnguished from actual JSON values. This leaves us with objects.
	 *     Replacer() - the caller may return a non-serializable AHK objects such as
	 *     ComObject, Func, BoundFunc, FileObject, RegExMatchObject, and Property to
	 *     mimic the behavior of returning 'undefined' in JavaScript but for the sake
	 *     of code readability and convenience, it's better to do 'return JSON.Undefined'.
	 *     Internally, the property returns a ComObject with the variant type of VT_EMPTY.
	 */
	Undefined[]
	{
		get {
			static empty := {}, vt_empty := ComObject(0, &empty, 1)
			return vt_empty
		}
	}

	class Functor
	{
		__Call(method, ByRef arg, args*)
		{
		; When casting to Call(), use a new instance of the "function object"
		; so as to avoid directly storing the properties(used across sub-methods)
		; into the "function object" itself.
			if IsObject(method)
				return (new this).Call(method, arg, args*)
			else if (method == "")
				return (new this).Call(arg, args*)
		}
	}
}

;Modifed AHK JSON Library for working with the game setting file - as JSON lacks a boolean type 'x'=false or 'y'=true ges turned into numeric values as standard, this version preserves them

/**
 * Modify by WarpRider, Member on https://www.autohotkey.com/boards
 *     ingnore the ahk internal vars true/false and the string null wil be not empty
 */

class AHK_JSON_RAWBOOLEAN extends AHK_JSON ;Irisiri - renamed as SH already has a JSON class powered by JavaScript TODO: Can we instead modify the base class to take rawboolean as as parameter?
{
	class Load extends AHK_JSON.Load
	{
		Call(self, ByRef text, reviver:="")
		{
			this.rev := IsObject(reviver) ? reviver : false
		; Object keys(and array indices) are temporarily stored in arrays so that
		; we can enumerate them in the order they appear in the document/text instead
		; of alphabetically. Skip if no reviver function is specified.
			this.keys := this.rev ? {} : false

			static quot := Chr(34), bashq := "\" . quot
			     , json_value := quot . "{[01234567890-tfn"
			     , json_value_or_array_closing := quot . "{[]01234567890-tfn"
			     , object_key_or_object_closing := quot . "}"

			key := ""
			is_key := false
			root := {}
			stack := [root]
			next := json_value
			pos := 0

			while ((ch := SubStr(text, ++pos, 1)) != "") {
				if InStr(" `t`r`n", ch)
					continue
				if !InStr(next, ch, 1)
					this.ParseError(next, text, pos)

				holder := stack[1]
				is_array := holder.IsArray

				if InStr(",:", ch) {
					next := (is_key := !is_array && ch == ",") ? quot : json_value

				} else if InStr("}]", ch) {
					ObjRemoveAt(stack, 1)
					next := stack[1]==root ? "" : stack[1].IsArray ? ",]" : ",}"

				} else {
					if InStr("{[", ch) {
					; Check if Array() is overridden and if its return value has
					; the 'IsArray' property. If so, Array() will be called normally,
					; otherwise, use a custom base object for arrays
						static json_array := Func("Array").IsBuiltIn || ![].IsArray ? {IsArray: true} : 0

					; sacrifice readability for minor(actually negligible) performance gain
						(ch == "{")
							? ( is_key := true
							  , value := {}
							  , next := object_key_or_object_closing )
						; ch == "["
							: ( value := json_array ? new json_array : []
							  , next := json_value_or_array_closing )

						ObjInsertAt(stack, 1, value)

						if (this.keys)
							this.keys[value] := []

					} else {
						if (ch == quot) {
							i := pos
							while (i := InStr(text, quot,, i+1)) {
								value := StrReplace(SubStr(text, pos+1, i-pos-1), "\\", "\u005c")

								static tail := A_AhkVersion<"2" ? 0 : -1
								if (SubStr(value, tail) != "\")
									break
							}

							if (!i)
								this.ParseError("'", text, pos)

							  value := StrReplace(value,  "\/",  "/")
							, value := StrReplace(value, bashq, quot)
							, value := StrReplace(value,  "\b", "`b")
							, value := StrReplace(value,  "\f", "`f")
							, value := StrReplace(value,  "\n", "`n")
							, value := StrReplace(value,  "\r", "`r")
							, value := StrReplace(value,  "\t", "`t")

							pos := i ; update pos

							i := 0
							while (i := InStr(value, "\",, i+1)) {
								if !(SubStr(value, i+1, 1) == "u")
									this.ParseError("\", text, pos - StrLen(SubStr(value, i+1)))

								uffff := Abs("0x" . SubStr(value, i+2, 4))
								if (A_IsUnicode || uffff < 0x100)
									value := SubStr(value, 1, i-1) . Chr(uffff) . SubStr(value, i+6)
							}

							if (is_key) {
								key := value, next := ":"
								continue
							}

						} else {
							value := SubStr(text, pos, i := RegExMatch(text, "[\]\},\s]|$",, pos)-pos)
							;MsgBox, "value=" %value%

							static number := "number", integer :="integer"
							if value is %number%
							{
								if value is %integer%
									value += 0
							}

							;WarpRider 31.01.2023: hier wird value auf true oder false geprüft und behandelt, nach AHK wird das dann 0 oder 1,
							;das ist aber falsch, da true/false für JSON keine boolschen Variablen sind, value muss unverändert übernommen werden
							else if (value == "true" || value == "false")
								value := value	;ORIGINAL: value := %value% + 0


							else if (value == "null")
								value := "null"									;WarpRider 31.01.2023: hier genauso, warum wird null nicht stur übernommen?
							else
							; we can do more here to pinpoint the actual culprit
							; but that's just too much extra work.
								this.ParseError(next, text, pos, i)

							pos += i-1
						}

						next := holder==root ? "" : is_array ? ",]" : ",}"
					} ; If InStr("{[", ch) { ... } else

					is_array? key := ObjPush(holder, value) : holder[key] := value

					if (this.keys && this.keys.HasKey(holder))
						this.keys[holder].Push(key)
				}

			} ; while ( ... )

			return this.rev ? this.Walk(root, "") : root[""]
		}
	}

	class Dump extends AHK_JSON.Dump
	{
		Str(holder, key)
		{
			value := holder[key]

			if (this.rep)
				value := this.rep.Call(holder, key, ObjHasKey(holder, key) ? value : JSON.Undefined)

			if IsObject(value) {
			; Check object type, skip serialization for other object types such as
			; ComObject, Func, BoundFunc, FileObject, RegExMatchObject, Property, etc.
				static type := A_AhkVersion<"2" ? "" : Func("Type")
				if (type ? type.Call(value) == "Object" : ObjGetCapacity(value) != "") {
					if (this.gap) {
						stepback := this.indent
						this.indent .= this.gap
					}

					is_array := value.IsArray
				; Array() is not overridden, rollback to old method of
				; identifying array-like objects. Due to the use of a for-loop
				; sparse arrays such as '[1,,3]' are detected as objects({}).
					if (!is_array) {
						for i in value
							is_array := i == A_Index
						until !is_array
					}

					str := ""
					if (is_array) {
						Loop, % value.Length() {
							if (this.gap)
								str .= this.indent

							v := this.Str(value, A_Index)
							str .= (v != "") ? v . "," : "null,"
						}
					} else {
						colon := this.gap ? ": " : ":"
						for k in value {
							v := this.Str(value, k)
							if (v != "") {
								if (this.gap)
									str .= this.indent

								str .= this.Quote(k) . colon . v . ","
							}
						}
					}

					if (str != "") {
						str := RTrim(str, ",")
						if (this.gap)
							str .= stepback
					}

					if (this.gap)
						this.indent := stepback

					return is_array ? "[" . str . "]" : "{" . str . "}"
				}

			}
			;WarpRider 31.01.2023: alle Werte hier, ausser Zahlen werden durch die Funktion Quote() mit " eingefasst,
			;das darf bei true,false,null eben nicht so sein, da true/false für JSON keine boolschen Variablen sind und null nicht leer werden
			else ; is_number ? value : "value"
			{
			;MsgBox, vor.Str.return.raw.value=%value%
			if (value == "true" || value == "false" || value == "null")
			  return value
			else
			  return ObjGetCapacity([value], 1)=="" ? value : this.Quote(value)
			}

		}
	}
}