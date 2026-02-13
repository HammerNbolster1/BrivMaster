;This file is intended for functions used across both the gem farm script and the hub. Currently meeting that goal is a WiP
#include %A_LineFile%\..\IC_BrivMaster_Memory.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\SH_KeyHelper.ahk ;Used for IC_BrivMaster_InputManager_Class

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

	LoadObjectFromAHKJSON(fileName,preserveBooleans:=false) ;If preserveBooleans is set 'true' and 'false' will be read as strings rather than being converted to -1 or 0, as AHK does not have a boolean type. Needed for game settings file TODO: Move JSON load/write somewhere the main script can use them too. Down with IE!
    {
        FileRead, oData, %fileName%
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
            err.Message:=err.Message . "`tFile:`t" . fileName
            throw err
        }
        return data
    }

    WriteObjectToAHKJSON(fileName, ByRef object,preserveBooleans:=false)
    {
        if (preserveBooleans)
			objectJSON:=AHK_JSON_RAWBOOLEAN.Dump(object,,"`t")
		else
			objectJSON:=AHK_JSON.Dump(object,,"`t")
        if (!objectJSON)
            return
        FileDelete, %fileName%
        FileAppend, %objectJSON%, %fileName%
        return
    }

	GetProcessName(processID) ;To check without a window being present
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
	
	/*
	ConvQuadToDouble(FirstEight, SecondEight) ;Takes input of first and second sets of eight byte int64s that make up a quad in memory. Obviously will not work if quad value exceeds double max TODO: Not currently used as only checking if gold=0 or not
    {
        return (FirstEight + (2.0**63)) * (2.0**SecondEight)
    }
	*/ 

    IsCurrentFormation(testformation:="") ;Returns true if the formation array passed is the same as the formation currently on the game field. Always false on empty formation reads. Requires full formation.
    {
        if(!IsObject(testFormation))
            return false
        currentFormation := this.Memory.GetCurrentFormation()
        if(!IsObject(currentFormation))
            return false
        if(currentFormation.Count()!=testformation.Count()) 
            return false
        loop, % currentFormation.Count()
            if(testformation[A_Index]!=currentFormation[A_Index])
                return false
        return true
    }
	
	IsCurrentFormationWithIgnore(testformation:="",ignoredHeroes:="") ;Returns true if the formation array passed is the same as the formation currently on the game field. Always false on empty formation reads. Requires full formation. TODO: This is a bit...temporary. Maybe make it more flexible
    {
        if(!IsObject(testFormation))
            return false
        currentFormation := this.Memory.GetCurrentFormation()
        if(!IsObject(currentFormation))
            return false
        loop, % currentFormation.Count()
            if(testformation[A_Index]!=currentFormation[A_Index] AND !ignoredHeroes[testformation[A_Index]])
                return false
        return true
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
        this.sprint:=g_Heroes[58].ReadHasteStacks() ;TODO: Calling Haste 'Sprint' here is confusing; need to check throughout IC_Core if replacing it however (N.B. The reason for this naming is that the stat in the game is called 'BrivSprintStacks'). Possibly using that stat name in full would be clearer?
        this.steelbones:=g_Heroes[58].ReadSBStacks()
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
		g_IBM.IBM_Sleep(100) ;20ms is not sufficent for this for all users. Was 50ms in BGF, but looks like the loading part of the reset takes >1s in reality, so using 100ms is a safe play without any performance concerns
        while(!this.Memory.ReadUserIsInited() AND this.Memory.ReadCurrentZone()<1 AND ElapsedTime<timeout)
        {
            g_IBM.IBM_Sleep(20)
            ElapsedTime:=A_TickCount - StartTime
        }
        if (ElapsedTime>=timeout)
			return false
        return true
    }
	
/* 
 WaitForModronReset(timeout:=60000)
    {
        StartTime:=A_TickCount
        ElapsedTime:=0
        g_SharedData.UpdateOutbound("LoopString","Modron Resetting...")
        g_IBM.Logger.AddMessage("WaitForModronReset() Modron Resetting...")
        this.SetUserCredentials()
        if (this.steelbones!="" AND this.steelbones>0 AND this.sprint!="") ;Only try and manually save if it hasn't already happened - (steelbones > 0)
            g_serverCall.CallPreventStackFail(this.sprint,this.steelbones,"WaitForModronReset()",true)
        while (this.Memory.ReadResetting() AND ElapsedTime < timeout)
        {
            g_IBM.IBM_Sleep(20)
            ElapsedTime:=A_TickCount - StartTime
        }
        g_SharedData.UpdateOutbound("LoopString", "Loading z1...")
        g_IBM.Logger.AddMessage("WaitForModronReset() Loading z1...")
        g_IBM.IBM_Sleep(20)
        while(!this.Memory.ReadUserIsInited() AND this.Memory.ReadCurrentZone()<1 AND ElapsedTime<timeout)
        {
            g_IBM.IBM_Sleep(20)
            ElapsedTime:=A_TickCount - StartTime
        }
        if (ElapsedTime>=timeout)
        {
            g_IBM.Logger.AddMessage("WaitForModronReset() Timeout Exit")
            return false
        }
        g_IBM.Logger.AddMessage("WaitForModronReset() Standard Exit")
        return true
    }
*/

	GetWebRoot()
    {
        tempWebRoot := this.Memory.ReadWebRoot()
        httpString := StrSplit(tempWebRoot,":")[1]
        isWebRootValid := httpString == "http" or httpString == "https"
        g_ServerCall.webroot := isWebRootValid ? tempWebRoot : g_ServerCall.webroot
    }
}

class IC_BrivMaster_SharedData_Class ;In the shared file as the SettingsPath static is used by the hub for the save/load location TODO: This seems like a lousy reason to load this into the hub, move settings path to SharedFunctions?
{
	static SettingsPath:=A_LineFile . "\..\IC_BrivMaster_Settings.json"

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
		for k,v in settings
		{
			if(k!="HUB") ;Do not load hub-only settings
				g_IBM_Settings[k]:=v
		}
		settings:=""
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
		SetTimer, %timerFunction%, 15, 0
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