class IC_BrivMaster_DianaCheese_Class ;A class for cheesing Diana's Electrum drops
{
	__new()
	{
		this.SetCapacity("TZData", 172)
        DllCall( "RtlFillMemory", "Ptr",this.GetAddress("TZData"), "Ptr",172, "Char",0 ) ; Zero fill memory
        this.ReadCNETimeZone(this.GetAddress("TZData"))
	}
	
	InWindow()
	{
		serverTime:=this.GetCNETime() 
		return serverTime > 11.95 AND serverTime < 12.5 ;11:57 to 12:30. Reset is at 12:00 CNE time (Pacific local time)
	}

	GetCNETime() ;Returns hours with minutes as a fraction, e.g. 8.5 = 08:30, 23.95 = 23:57
	{
		; Get current UTC system time
		VarSetCapacity(SYSTEMTIME, 16, 0)
		DllCall("GetSystemTime", "Ptr", &SYSTEMTIME)
		;Convert UTC to PST/PDT, accounting for DST
		VarSetCapacity(LocalTime, 16, 0)
		Result := DllCall("SystemTimeToTzSpecificLocalTime", "Ptr", this.GetAddress("TZData"), "Ptr", &SYSTEMTIME, "Ptr", &LocalTime)
		if (!Result) {
			return ""
		}
		; Extract fields from LocalTime
		Hour := NumGet(LocalTime, 8, "UShort")
		Minute := NumGet(LocalTime, 10, "UShort")
		return Hour + Minute/60
	}

	ReadCNETimeZone(TIME_ZONE_INFORMATION) ;Gets time data for CNE's Pacific standard time location. It's okay for this to error with message boxes as it's a one-off at startup TODO: This needs to be built into an organised pre-flight check
	{
		; Read Pacific Standard Time data from registry (Windows 11 format)
		RegRead, TZIHex, HKEY_LOCAL_MACHINE, SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\Pacific Standard Time, TZI
		if ErrorLevel {
			MsgBox % "Diana Cheese Setup: Failed to read TZI registry key."
			return
		}
		RegRead, StandardName, HKEY_LOCAL_MACHINE, SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\Pacific Standard Time, Std
		if ErrorLevel {
			MsgBox % "Diana Cheese Setup: Failed to read Std registry key."
			return
		}
		RegRead, DaylightName, HKEY_LOCAL_MACHINE, SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\Pacific Standard Time, Dlt
		if ErrorLevel {
			MsgBox % "Diana Cheese Setup: Failed to read Dlt registry key."
			return
		}
		; Parse TZI hex string
		Bias := this.HexToInt(SubStr(TZIHex, 1, 8))
		StandardBias := this.HexToInt(SubStr(TZIHex, 9, 8))
		DaylightBias := this.HexToInt(SubStr(TZIHex, 17, 8))
		; Parse StandardDate (bytes 13-28, hex 25-56, SYSTEMTIME: 8 USHORTs)
		VarSetCapacity(StandardDate, 16, 0)
		NumPut(this.HexToUShort(SubStr(TZIHex, 25, 4)), StandardDate, 0, "UShort")  ; wYear
		NumPut(this.HexToUShort(SubStr(TZIHex, 29, 4)), StandardDate, 2, "UShort")  ; wMonth
		NumPut(this.HexToUShort(SubStr(TZIHex, 33, 4)), StandardDate, 4, "UShort")  ; wDayOfWeek
		NumPut(this.HexToUShort(SubStr(TZIHex, 37, 4)), StandardDate, 6, "UShort")  ; wDay
		NumPut(this.HexToUShort(SubStr(TZIHex, 41, 4)), StandardDate, 8, "UShort")  ; wHour
		NumPut(this.HexToUShort(SubStr(TZIHex, 45, 4)), StandardDate, 10, "UShort") ; wMinute
		NumPut(this.HexToUShort(SubStr(TZIHex, 49, 4)), StandardDate, 12, "UShort") ; wSecond
		NumPut(this.HexToUShort(SubStr(TZIHex, 53, 4)), StandardDate, 14, "UShort") ; wMilliseconds
		; Parse DaylightDate (bytes 29-44, hex 57-88, SYSTEMTIME: 8 USHORTs)
		VarSetCapacity(DaylightDate, 16, 0)
		NumPut(this.HexToUShort(SubStr(TZIHex, 57, 4)), DaylightDate, 0, "UShort")  ; wYear
		NumPut(this.HexToUShort(SubStr(TZIHex, 61, 4)), DaylightDate, 2, "UShort")  ; wMonth
		NumPut(this.HexToUShort(SubStr(TZIHex, 65, 4)), DaylightDate, 4, "UShort")  ; wDayOfWeek
		NumPut(this.HexToUShort(SubStr(TZIHex, 69, 4)), DaylightDate, 6, "UShort")  ; wDay
		NumPut(this.HexToUShort(SubStr(TZIHex, 73, 4)), DaylightDate, 8, "UShort")  ; wHour
		NumPut(this.HexToUShort(SubStr(TZIHex, 77, 4)), DaylightDate, 10, "UShort") ; wMinute
		NumPut(this.HexToUShort(SubStr(TZIHex, 81, 4)), DaylightDate, 12, "UShort") ; wSecond
		NumPut(this.HexToUShort(SubStr(TZIHex, 85, 4)), DaylightDate, 14, "UShort") ; wMilliseconds
		; Populate TIME_ZONE_INFORMATION
		NumPut(Bias, TIME_ZONE_INFORMATION + 0, 0, "Int")          ; Bias
		StrPut(StandardName, TIME_ZONE_INFORMATION + 4, 64, "UTF-16")
		DllCall("RtlMoveMemory", "Ptr", TIME_ZONE_INFORMATION + 68, "Ptr", &StandardDate, "UInt", 16)
		NumPut(StandardBias, TIME_ZONE_INFORMATION + 0, 84, "Int")  ; StandardBias
		StrPut(DaylightName, TIME_ZONE_INFORMATION + 88, 64, "UTF-16")
		DllCall("RtlMoveMemory", "Ptr", TIME_ZONE_INFORMATION + 152, "Ptr", &DaylightDate, "UInt", 16)
		NumPut(DaylightBias, TIME_ZONE_INFORMATION + 0, 168, "Int")  ; DaylightBias
	}

	ReverseHexBytes(hex)
	{
		len:=StrLen(hex)
		result:=""
		Loop, % len // 2 		; Process two chars (one byte) at a time, from end to start
		{
			pos:=len - (2 * A_Index) + 1
			result.=SubStr(hex, pos, 2)
		}
		return result
	}

	HexToInt(hex)
	{
		hex := this.ReverseHexBytes(hex) ; Reverse byte order (little-endian)
		val :="0x" . hex
		val+=0 ; Convert to unsigned integer and ensure numeric output
		if (val > 0x7FFFFFFF) ; Convert to signed 32-bit integer
			val := val - 0x100000000
		return val
	}


	HexToUShort(hex)
	{
		hex:=this.ReverseHexBytes(hex) ; Reverse byte order (little-endian)
		hex:="0x" . hex ; Convert to unsigned short and ensure numeric output
		return hex + 0
	}
}

class IC_BrivMaster_Logger_Class ;A class for recording run logs
{
	__New(logDir)
	{
		FormatTime, formattedDateTime,, yyyyMMddTHHmmss ;Can't include : in a filename so using the less human friendly version here
		if (!FileExist(logDir)) ;Create the log subdirectory if not present
			FileCreateDir, %logDir%
		this.logBase:=LogDir . "\RunLog_" . formattedDateTime ;A separate variable so other logs can use a matching start time, e.g. RunLog_20250101T000000.csv from this class and RunLog_20250101T000000_Relay.csv
		this.logPath:=this.logBase . ".csv" ;The path and name for the main log specifically
		reset:=g_SF.Memory.ReadResetsTotal()
		if (reset!="") ;If we can read the current reset use that, otherwise set to -1 for invalid
			g_SharedData.IBM_UpdateOutbound("RunLogResetNumber",reset)
		else
			g_SharedData.IBM_UpdateOutbound("RunLogResetNumber",-1)
		g_SharedData.IBM_UpdateOutbound("RunLog",{})
		this.LogEntries:={}
		this.OutputHeader()
	}

	NewRun()
	{
		startTime:=A_TickCount ;So it doesn't change between entries
		if (this.LogEntries.HasKey("Run")) ;There will be no entry for the first run
		{
			this.LogEntries.Run.End:=startTime
			if (this.LogEntries.Run.LastZone > g_IBM.RouteMaster.targetZone) ;We don't get anything from bosses jumped after our reset, so clamp
				this.LogEntries.Run.LastZone:=g_IBM.RouteMaster.targetZone
			else if (this.LogEntries.Run.LastZone < g_IBM.RouteMaster.targetZone) ;If we didn't make it to reset
				this.LogEntries.Run.Fail:=true
			g_SharedData.IBM_UpdateOutbound("RunLogResetNumber",-1) ;Invalid whilst updating
			g_SharedData.IBM_UpdateOutbound("RunLog",AHK_JSON.Dump(this.LogEntries))
			g_SharedData.IBM_UpdateOutbound("RunLogResetNumber",this.LogEntries.Run.ResetNumber)
			;Output log
			loadTime:=this.LogEntries.Run.ActiveStart - this.LogEntries.Run.Start
			resetTime:=this.LogEntries.Run.End - this.LogEntries.Run.ResetReached
			runString:=this.LogEntries.Run.ResetNumber . "," . this.LogEntries.Run.StartRealTime . "," . this.LogEntries.Run.Start . "," ;Reset #,Start Time,Start Tick
			runString.=this.LogEntries.Run.End - this.LogEntries.Run.Start . "," . this.LogEntries.Run.ResetReached - this.LogEntries.Run.ActiveStart . "," . loadTime + resetTime . "," ;Total,Active,Wait
			runString.=loadTime . "," . resetTime . "," . this.LogEntries.Run.Cycle . "," ;Load,Reset,Cycle
			runString.=this.LogEntries.Run.Fail . "," . this.LogEntries.Run.LastZone . "," . g_SF.Memory.ReadChestCountByID(282)  ;Fail,LastZone,Electrum
			messageString:=""
			for _,v in this.LogEntries.Messages
				messageString.=v . ","
			FileAppend, % runString . "," . messageString . "`n", % this.logPath
		}
		;Reset for new
		this.LogEntries.Messages:={}
		this.LogEntries.Thellora:={}
		this.LogEntries.Run:={}
		this.LogEntries.Run.Start:=startTime
		FormatTime, formattedDateTime,, yyyy-MM-ddTHH:mm:ss
		this.LogEntries.Run.StartRealTime:=formattedDateTime
		this.LogEntries.Run.ResetNumber:=g_SF.Memory.ReadResetsTotal()
		this.LogEntries.Run.GHActive:=g_SF.Memory.IBM_IsBuffActive("Potion of the Gem Hunter") ;Does this break in non-English clients?
		this.LogEntries.Run.LastZone:=0
		this.LogEntries.Run.Fail:=false
		this.LogEntries.Run.Cycle:=""
	}
	
	OutputHeader()
	{
		FileAppend, % "Reset #,Start Time,Start Tick,Total,Active,Wait,Load,Reset,Cycle,Fail,LastZone,Electrum`n", % this.logPath
	}
	
	ForceFail() ;The zone-based check does not capture runs that reach the target, but fail to reset, causing us to have Weird Stuff going on with no reported fails
	{
		if (this.LogEntries.HasKey("Run"))
			this.LogEntries.Run.Fail:=true
	}

	SetRunCycle(cycleNumber) ;The routeMaster won't be .Reset() until after the log starts, so need to add the cylce number once available
	{
		if (this.LogEntries.HasKey("Run"))
			this.LogEntries.Run.Cycle:=cycleNumber
	}
	
	SetActiveStartTime() ;Called when z1 is Active
	{
		if (this.LogEntries.HasKey("Run"))
			this.LogEntries.Run.ActiveStart:=A_TickCount
	}

	AddMessage(message)
	{
		if (this.LogEntries.HasKey("Run"))
			this.LogEntries.Messages.Push(A_TickCount - this.LogEntries.Run.Start . "," . message)
		else
			this.LogEntries.Messages.Push(A_TickCount . "(Abs)," . message)
	}

	AddThelloraCompensationMessage(message,jumps) ;Avoid spamming this every time it is applied - only when the jump value changes
	{
		if (!this.LogEntries.Thellora.LastJumps OR (this.LogEntries.Thellora.LastJumps And this.LogEntries.Thellora.LastJumps!=jumps))
		{
			this.LogEntries.Thellora.LastJumps:=jumps
			this.AddMessage(message . jumps)
		}
	}

	ResetReached()
	{
		if (this.LogEntries.HasKey("Run"))
		{
			if (!this.LogEntries.Run.ResetReached) ;This will be called multiple times, only record the first entry TODO: This can cause problems with Relay restarts, as the old client can pass the reset after saving?
				this.LogEntries.Run.ResetReached:=A_TickCount
			currentZone:=g_SF.Memory.ReadCurrentZone() ;Record the end zone if still a valid read
			if (currentZone)
				this.UpdateZone(currentZone)
		}
	}

	UpdateZone(zone)
	{
		if (this.LogEntries.HasKey("Run"))
		{
			if (zone > this.LogEntries.Run.LastZone)
				this.LogEntries.Run.LastZone:=zone
		}
		this.AddMessage("z" . zone . " intent: " . (g_IBM.routeMaster.ShouldWalk(zone) ? "E" : "Q") . " to z" . g_IBM.routeMaster.zones[zone].nextZone.z) ;Uncomment for debugging
	}
}

class IC_BrivMaster_InputManager_Class ;A class for managing input related matters
{
	keyList:={} ;Indexed by key per the script (e.g. "F1","ClickDmg"), contains the mapped Key, lParam for SendMessage for down, and lParam for SendMessage for up

	__new() ;Currently it is up to code using this to add the necessary keys
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
		hwnd:=g_SF.Hwnd
		ControlFocus,, ahk_id %hwnd%
	}
	
	ReleaseAll() ;Sends a key-up for all used keys. Added to due to apparent input issues
	{
		this.gameFocus()
		c:=0
		for _,key in this.keyList
		{
			key.Release_Bulk()
			c++
		}
		g_IBM.Logger.AddMessage("ReleaseAll() sent key-up for [" . c . "] keys") ;As the default may change we want to be informed
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
        hwnd:=g_SF.Hwnd
		mk:=this.mappedKey ;We have to copy the variables locally due to limitations of AHK :(
		lD:=this.lparamDown
        ControlFocus,, ahk_id %hwnd%
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
	}

	Release() ;Release a key
	{
        hwnd:=g_SF.Hwnd
		mk:=this.mappedKey
		lU:=this.lparamUp
        ControlFocus,, ahk_id %hwnd% ;As above
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
	}

	KeyPress() ;Press then release a key
	{
		startCritical:=A_IsCritical ;Store existing state of critical
		Critical, On
        hwnd:=g_SF.Hwnd
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
        hwnd:=g_SF.Hwnd
		mk:=this.mappedKey ;We have to copy the variables locally due to limitations of AHK :(
		lD:=this.lparamDown
    	SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
	}

	Release_Bulk() ;Release a key
	{
        hwnd:=g_SF.Hwnd
		mk:=this.mappedKey
		lU:=this.lparamUp
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
	}

	KeyPress_Bulk() ;Press then release a key
	{
        hwnd:=g_SF.Hwnd
        mk:=this.mappedKey
		lD:=this.lparamDown
		lU:=this.lparamUp
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
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
        settings:=g_SF.LoadObjectFromJSON(fileName)
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