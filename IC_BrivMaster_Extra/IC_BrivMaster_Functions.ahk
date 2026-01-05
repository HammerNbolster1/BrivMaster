;This file is intended for functions used in the gem farm script, but not the hub

class IC_BrivMaster_DialogSwatter_Class ;A class for swatting dialogs that appears at game start
{
	__New()
    {
        this.Timer:=ObjBindMethod(this, "Swat")
		this.KEY_ESC:=g_InputManager.getKey("Esc")
    }

    Start()
    {
		timerFunction:=this.Timer
		SetTimer, %timerFunction%, 100, 0
		this.StartTime:=A_TickCount
    }

    Stop()
    {
        timerFunction:=this.Timer
		SetTimer, %timerFunction%, Off
    }

    Swat()
    {
        if (g_SF.Memory.ReadWelcomeBackActive())
			this.KEY_ESC.KeyPress() ;.KeyPress() applies critical itself
		else if (A_TickCount > this.StartTime + 3000) ;3s should be enough to get the swat done
			this.Stop() ;Stop the timer since we don't have anything to swat
    }
}

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


