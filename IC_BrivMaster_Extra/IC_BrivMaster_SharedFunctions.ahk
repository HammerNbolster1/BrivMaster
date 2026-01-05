;This file is intended for functions used across both the gem farm script and the hub. Currently meeting that goal is a WiP
#include %A_LineFile%\..\IC_BrivMaster_Memory.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\SH_KeyHelper.ahk ;Used for IC_BrivMaster_InputManager_Class

global g_PreviousZoneStartTime ;TODO: Why is this in here? It is used by CheckifStuck - move elsewhere if that function moves. Or possibly move it anyway...at least into the class constructor

class IC_BrivMaster_SharedFunctions_Class
{
	__new()
    {
        this.Memory:=New IC_BrivMaster_MemoryFunctions_Class(A_LineFile . "\..\..\IC_Core\MemoryRead\CurrentPointers.json")
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
        data := ""
        try
        {
            if (preserveBooleans)
				data:=AHK_JSON_RAWBOOLEAN.Load(oData)
			else
				data:=AHK_JSON.Load(oData)
        }
        catch err
        {
            err.Message := err.Message . "`nFile:`t" . FileName
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
    CheckifStuck(isStuck:=false)
    {
        static lastCheck := 0
        static fallBackTries := 0
        if (isStuck)
        {
            g_IBM.GameMaster.RestartAdventure("Game is stuck z[" . this.Memory.ReadCurrentZone() . "]")
            g_IBM.GameMaster.SafetyCheck()
            g_PreviousZoneStartTime := A_TickCount
            lastCheck := 0
            fallBackTries := 0
            return true
        }
		dtCurrentZoneTime := Round((A_TickCount - g_PreviousZoneStartTime) / 1000, 2)
		if (dtCurrentZoneTime <= 35) ;Irisiri - added fast exit for the standard case
			return false
        else if (dtCurrentZoneTime > 35 AND dtCurrentZoneTime <= 45 AND dtCurrentZoneTime - lastCheck > 5) ; first check - ensuring autoprogress enabled
        {
            g_IBM.RouteMaster.ToggleAutoProgress(1, true)
            if(dtCurrentZoneTime < 40)
                lastCheck := dtCurrentZoneTime
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
            g_IBM.GameMaster.RestartAdventure( "Game is stuck z[" . this.Memory.ReadCurrentZone() . "]" )
            g_IBM.GameMaster.SafetyCheck()
            g_PreviousZoneStartTime:=A_TickCount
            lastCheck := 0
            fallBackTries := 0
            return true
        }
        return false
    }
	
    DoRushWait(stopProgress:=false) ;Wait for Thellora (ID=139) to activate her Rush ability. TODO: unknown what ReadRushTriggered() returns if she starts with 0 stacks or we have 0 favour (with the former being the case that might matter)
    {
        ElapsedTime:=0
		levelTypeChampions:=true ;Alternate levelling types to cover both without taking too long in each loop
		g_SharedData.IBM_UpdateOutbound("LoopString","Rush Wait")
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

    SetUserCredentials() ;Removed creation of data to return for JSON export, as it never appeared to get used after output by ResetServerCall. Removed gem and chest data as those are fully handled by the hub side
    {
        this.UserID:=this.Memory.ReadUserID()
        this.UserHash:=this.Memory.ReadUserHash()
        this.InstanceID:=this.Memory.ReadInstanceID()
        this.sprint:=this.Memory.ReadHasteStacks() ;TODO: Calling Haste 'Sprint' here is confusing; need to check throughout IC_Core if replacing it however (N.B. The reason for this naming is that the stat in the game is called 'BrivSprintStacks')
        this.steelbones:=this.Memory.ReadSBStacks()
    }
	
	;Removed saving of Servercall information to a JSON file, which never appeared to get used
	; sets the user information used in server calls such as user_id, hash, active modron, etc.
    ResetServerCall()
    {
        this.SetUserCredentials()
        g_ServerCall := new IC_BrivMaster_ServerCall_Class( this.UserID, this.UserHash, this.InstanceID )
        version := this.Memory.ReadBaseGameVersion()
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
        StartTime := A_TickCount
        ElapsedTime := 0
        g_SharedData.IBM_UpdateOutbound("LoopString","Modron Resetting...")
        this.SetUserCredentials()
		if (this.steelbones != "" AND this.steelbones > 0 AND this.sprint != "" AND (this.sprint + FLOOR(this.steelbones * g_IBM.RouteMaster.stackConversionRate) <= 176046)) ;Only try and manually save if it hasn't already happened - (steelbones > 0)
        {
			g_IBM.Logger.AddMessage("Manual stack conversion: Converted Haste=[" . this.sprint + FLOOR(this.steelbones * g_IBM.RouteMaster.stackConversionRate) . "] from Haste=[" . this.sprint . "] and Steelbones=[" . this.steelbones . "] with stackConversionRate=[" . Round(g_IBM.RouteMaster.stackConversionRate,1) . "]")
			response:=g_serverCall.CallPreventStackFail(this.sprint + FLOOR(this.steelbones * g_IBM.RouteMaster.stackConversionRate), true)
		}	
        while (this.Memory.ReadResetting() AND ElapsedTime < timeout)
        {
            g_IBM.IBM_Sleep(20)
            ElapsedTime := A_TickCount - StartTime
        }
        g_SharedData.IBM_UpdateOutbound("LoopString", "Loading z1...")
        g_IBM.IBM_Sleep(20)
        while(!this.Memory.ReadUserIsInited() AND this.Memory.ReadCurrentZone() < 1 AND ElapsedTime < timeout)
        {
            g_IBM.IBM_Sleep(20)
            ElapsedTime := A_TickCount - StartTime
        }
        if (ElapsedTime >= timeout)
        {
            return false
        }
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

class IC_BrivMaster_InputManager_Class ;A class for managing input related matters 
{
	keyList:={} ;Indexed by key per the script (e.g. "F1","ClickDmg"), contains the mapped Key, lParam for SendMessage for down, and lParam for SendMessage for up

	__new() ;Currently it is up to code using this to add the necessary keys TODO: Pass the object containing the HWnd to be used byRef, so it can be used with g_IBM.GameMaster.Hwnd in main and g_SF.Hwnd in hub?
	{
		this.KeyMap:={}
		this.SCKeyMap:={}
		KeyHelper.BuildVirtualKeysMap(this.KeyMap, this.SCKeyMap) ;Note: KeyHelper is in SH_KeyHelper.ahk, which is an #include in SH_SharedFunctions.ahk.
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
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
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
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
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
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
	}

	KeyPress_Bulk() ;Press then release a key
	{
        hwnd:=g_IBM.GameMaster.Hwnd
        mk:=this.mappedKey
		lD:=this.lparamDown
		lU:=this.lparamUp
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
	}
}

class IC_BrivMaster_EllywickDealer_Class ;A class for managing Ellywick's card draws and her ultimate use. This is based heavily on ImpEGamer's RNGWaitingRoom addon
{
	;HeroID's used: Elly=83, DM=99
	static ULTIMATE_RESOLUTION_TIME:=300 ;Real world milliseconds. Normally seems to be 0 to 140ms

	CasinoTimer := ObjBindMethod(this, "Casino")
	GemCardsNeeded := {} ;These are pairs of base and melf mode values, eg {0:2,1:3} for 2 without melf and 3 with
	MaxRedraws := {}
	MinCards:={}
	GemCardsNeededInFlight:=0
	Complete := false
	Redraws := 0 ;Current redraws
	UsedUlt := false ;Tracks Elly's ult being in progress, as her cards are only cleared when it ENDS, despite the visual
	MelfMode:=false ;Is melf spawning more? Used to select the appropriate options
	StatusString:=" STATUS=" ;Used to return basic information on problems (eg DM fails)

	Start(setMelfMode:=false)
	{
		this.MelfMode:=setMelfMode
		timerFunction := this.CasinoTimer
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
		this.Complete := false
		this.Redraws := 0
		this.UsedUlt := false ;This assumes Reset() will only be called after an adventure resets
		this.MaxRedraws := {0:g_IBM_Settings["IBM_Casino_Redraws_Base"],1:g_IBM_Settings["IBM_Casino_Redraws_Melf"]}
		this.GemCardsNeeded := {0:g_IBM_Settings["IBM_Casino_Target_Base"],1:g_IBM_Settings["IBM_Casino_Target_Melf"]}
		this.MinCards:={0:g_IBM_Settings["IBM_Casino_MinCards_Base"],1:g_IBM_Settings["IBM_Casino_MinCards_Melf"]}
		this.GemCardsNeededInFlight:=g_IBM_Settings["IBM_Casino_Target_InFlight"]
		this.MelfMode:=false
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
			else if (g_Heroes[83].GetNumGemCards() < this.GemCardsNeededInFlight AND this.GetNumCards() == 5 AND this.CanUseEllyWickUlt()) ;Use ultimate to redraw cards if Ellywick doesn't have GemCardsNeededInFlight (due to maxRedraws being less than the maximum possible)
				this.UseEllywickUlt()
		}
		else if (this.ShouldDrawMoreCards())
		{
			if (this.RedrawsLeft()) ;Use ultimate if it's not on cooldown and there are redraws left
			{
				 if (!this.UsedUlt AND this.ShouldRedraw())
					this.UseEllywickUlt()
			}
			else if (this.GetMinCards() == 0 OR (!this.UsedUlt AND this.GetNumCards()>=this.GetMinCards())) ;If we want to release at a certain number of cards we need to wait for the ult to resolve to be able to count correctly
				this.WaitRoomExit()
		}
		else
			this.WaitRoomExit()
	}

	WaitRoomExit() ;Seperate so we can put some status strings in here
	{
		this.Complete:=true
		if (g_Heroes[83].GetNumGemCards() >= this.GemCardsNeededInFlight) ;If we've reached our in-flight re-roll target in the waitroom there is no reason to keep the timer running
			this.Stop()
	}

	GetMinCards()
	{
		return this.MinCards[this.melfMode]
	}

	DrawsLeft()
	{
		return 5 - this.GetNumCards()
	}

	RedrawsLeft()
	{
		return this.MaxRedraws[this.melfMode] - this.Redraws
	}

	ShouldDrawMoreCards()
	{
		if (this.GetNumCards() < this.GetMinCards())
			return true
		return g_Heroes[83].GetNumGemCards() < this.GemCardsNeeded[this.melfMode]
	}

	ShouldRedraw()
	{
		numCards := this.GetNumCards()
		if (numCards == 5)
			return true
		else if (numCards == 0)
			return false
		return this.DrawsLeft() < this.GemCardsNeeded[this.melfMode] - g_Heroes[83].GetNumGemCards()
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
				this.MaxRedraws[this.melfMode]:=this.Redraws ;Lower max re-rolls so we move on; this Casino is busted
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

class IC_BrivMaster_SharedData_Class
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
        if (g_SF.Memory.ReadCurrentZone()=="") ; Invalid game state
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
	
	ReloadSettings(ReloadSettingsFunc) ;Unused by BM, but might be relevant for addons TODO: Review
    {
        reloadFunc := Func(ReloadSettingsFunc)
        reloadFunc.Call()
    }

	IBM_Init()
    {
        this.IBM_UpdateSettingsFromFile()
		this.IBM_OutboundDirty:=false ;Track if we've made changes to the data so the hub doesn't make unnecessary checks
    }

    IBM_UpdateSettingsFromFile(fileName := "") ;Load settings from the GUI settings file.
    {
        if (fileName == "")
            fileName := IC_BrivMaster_SharedData_Class.SettingsPath
        settings:=g_SF.LoadObjectFromAHKJSON(fileName)
        if (!IsObject(settings))
            return false
		for k,v in settings ;Load all settings
			g_IBM_Settings[k]:=v
		if(g_IBM) ;If the gem farm exists (as it will not when this is called from the hub without the farm running) TODO: Why try to read the settings in that case?
			g_IBM.RefreshGemFarmWindow()
    }
	
	IBM_UpdateOutbound(key,value) ;Update if the value has changed at mark the outbound data as dirty
	{
		if (this[key]!=value)
		{
			this[key]:=value
			this.IBM_OutboundDirty:=true
		}
	}
	
	IBM_ResetRunStats() ;Resets per-run stats from the main object (boss hits, rollbacks, bad autoprogression). This allows them to all be cleared in one go without spam setting the IBM_OutboundDirty flag 
	{
		this.BossesHitThisRun:=0
		this.TotalBossesHit:=0
        this.TotalRollBacks:=0
        this.BadAutoProgress:=0
		this.IBM_OutboundDirty:=true
	}
	
	IBM_UpdateOutbound_Increment(key) ;Increment a value, used for things like boss hit tracking
	{
		if (this.HasKey(key))
			this[key]++
		else
		{
			this[key]:=1
		}
		this.IBM_OutboundDirty:=true
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