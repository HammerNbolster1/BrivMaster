;This file is intended for functions used in the gem farm script, but not the hub

class IC_BrivMaster_EllywickCasino_Class ;A class to manage the whole casino, without the use of a timer
{
	static TIMEOUT_BASE:=100000  ;Allow 10s at x10 speed, 8s at x12.5
	
	__New(combining)
	{
		if(combining)
		{
			this.ghostLevelling:=g_IBM_Settings["IBM_Level_Options_Ghost"] ;Ghost levelling applies only to combining, where Briv is present for all of the Casino
			this.levelFormation:="min"
		}
		else
		{
			this.ghostLevelling:=false
			this.levelFormation:="z1"
		}
		this.Reset()
	}

	Reset()
	{
		this.Complete:=false
		this.Redraws:=0
		this.UsedUlt:=false ;This assumes Reset() will only be called after an adventure reset
		this.MaxRedraws:=g_IBM_Settings["IBM_Casino_Redraws_Base"] ;Maximum redraws allowed (1, or 2 with DM)
		this.GemCardsNeeded:=g_IBM_Settings["IBM_Casino_Target_Base"] ;Target gem cards
		this.MinCards:=g_IBM_Settings["IBM_Casino_MinCards_Base"] ;Minimum cards before exiting, used to try and avoid saving with a partial hand when hitting a boss shortly after the Casino
		this.lockedFrontColumnChamps:={}
	}

	Casino(lockedFrontColumnChamps) ;lockedFrontColumnChamps are a list of champions from the front row whose levelling has been locked (set to 0)
	{
		this.lockedFrontColumnChamps:=lockedFrontColumnChamps ;Store in the object so external .UnlockHeroes() calls don't need to re-pass
		frontColumnLevellingAllowed:=lockedFrontColumnChamps.Count()==0 ;If there are no locked champions there's no need to check for unlocking them
		if (!g_Heroes[83].ReadBenched()) ;TODO: Could possibly check level as well?
        {
			ghostLevellingAllowed:=!this.ghostLevelling ;TODO: Fix these variable names...
			MEMORY_MELEE_ADDRESS:=g_SF.Memory.ResolvePointers(g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numAttackingMonstersReached)
			MEMORY_MELEE_TYPE:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numAttackingMonstersReached.ValueType
			MEMORY_RANGE_ADDRESS:=g_SF.Memory.ResolvePointers(g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numRangedAttackingMonsters)
			MEMORY_RANGE_TYPE:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.formation.numRangedAttackingMonsters.ValueType
			g_Heroes[83].InitDoMTHandler() ;TODO: We should perhaps check if this actually worked?
			timeout:=IC_BrivMaster_EllywickCasino_Class.TIMEOUT_BASE/g_SF.Memory.IBM_ReadBaseGameSpeed()
            ElapsedTime:=0
            StartTime:=A_TickCount
			while (ElapsedTime<timeout)
            {
				;Start Casino card logic
				if (g_SF.Memory.ReadResetting() OR g_SF.Memory.ReadCurrentZone()=="") ;Abort the loop if we hit a reset or the memory reads fail
					break
				if (this.UsedUlt AND !g_Heroes[83].ReadEllywickUltimateActive()) ;Check for completed ultimate
					this.UsedUlt:=false
				if (this.ShouldDrawMoreCards())
				{
					if (this.MaxRedraws-this.Redraws > 0) ;Use ultimate if it's not on cooldown and there are redraws left
					{
						 if (!this.UsedUlt AND this.ShouldRedraw())
							this.UseEllywickUlt()
					}
					else if (this.MinCards==0 OR (!this.UsedUlt AND g_Heroes[83].ReadNumCards()>=this.MinCards)) ;If we want to release at a certain number of cards we need to wait for the ult to resolve to be able to count correctly
						break
				}
				else
					break
				;End Casino card logic TODO: We might need to check if we are within 1 card of a full hand, meeting the gem target, or re-rolling and skip the later part of the loop to ensure responsiveness		
				g_IBM.levelManager.LevelWorklist()
				g_IBM.levelManager.LevelClickDamage()
				if (!frontColumnLevellingAllowed) ;Check if we can allow this, the aim is to level whilst the formation is engauged so the champion is NOT placed, saving time without interfering with Briv
				{
					if (_MemoryManager.instance.Read(MEMORY_MELEE_ADDRESS,MEMORY_MELEE_TYPE) + _MemoryManager.instance.Read(MEMORY_RANGE_ADDRESS,MEMORY_RANGE_TYPE)>2) ;TODO: Investigate these thresholds
					{
						this.UnlockHeroes(lockedFrontColumnChamps,this.levelFormation)
						frontColumnLevellingAllowed:=True
					}
				}
				if (!ghostLevellingAllowed AND (frontColumnLevellingAllowed OR g_SF.Memory.IsCurrentFormationFull())) ;Either front row levelling is allowed (we've dealt with that champ, or doesn't care about the front row), or the formation is full so we can level away
				{
					g_IBM.levelManager.LevelFormation("A",this.levelFormation,,,[33]) ;Suppress Farideh, so that her levelling can be blocked during online stacking during recovery
					ghostLevellingAllowed:=true
				}
				g_IBM.IBM_Sleep(10)
				ElapsedTime:=A_TickCount-StartTime
            }
			ElapsedTime:=A_TickCount-StartTime ;As we will break out of the loop normally
			g_IBM.Logger.AddMessage("Casino{z" . g_SF.Memory.ReadCurrentZone() . " T=" . ElapsedTime . " R=" . this.Redraws . " M=" . g_IBM.RouteMaster.MelfManager.GetCurrentMelfEffect() .  " SB=" . g_Heroes[58].ReadSBStacks() . "}")
			return !frontColumnLevellingAllowed ;Returns true if we still need to unlock champions. Done like this so for featswap we can get autoprogress toggled on ASAP
		}
		else
		{
			g_IBM.Logger.AddMessage("No Elly{z" . g_SF.Memory.ReadCurrentZone() . "}")
			return !frontColumnLevellingAllowed
		}
	}
	
	UnlockHeroes(levelFormation:="") ;Separated as this must be called either during the Casino, or if Elly is MIA. TODO: Store lockedFrontColumnChamps in the object so the follow-up calls don't have to re-pass? Reset in, well, Reset()
	{
		if (this.lockedFrontColumnChamps.Count()>0)
		{
			for _,v in this.lockedFrontColumnChamps
			{
				g_IBM.LevelManager.ResetLevelByID(v)
			}
			if (levelFormation)
				g_IBM.LevelManager.LevelFormation("M",levelFormation) ;Re-create job. This could do without being a duplicate of the call in FirstZone (things will go weird when we change one and forget to change the other)
		}
	}
	
	ShouldDrawMoreCards()
	{
		if (g_Heroes[83].ReadNumCards()<this.MinCards)
			return true
		return g_Heroes[83].GetNumGemCards() < this.GemCardsNeeded
	}
	
	ShouldRedraw()
	{
		numCards:=g_Heroes[83].ReadNumCards()
		if (numCards==5)
			return true
		else if (numCards==0)
			return false
		return (5-numCards) < (this.GemCardsNeeded - g_Heroes[83].GetNumGemCards())
	}
	
	UseEllywickUlt()
	{
		if (g_SF.Memory.ReadTransitioning()) ;Do not try using the ults during a transition - possible source of Weird Stuff
			return
		if (g_Heroes[83].CanUseUltimate())
		{
			this.UsedUlt:=true ;Set here to block double presses, until we can confirm it has / hasn't been used
			retryCount:=g_Heroes[83].UseUltimate(50) ;50 'retries' is 5 actual attempts due to the way UseUltimate counts. +1 is a queue wait
			if (retryCount=="" OR retryCount>50) ;Failed to find key, or failed to register
			{
				g_IBM.Logger.AddMessage("Casino Elly(Level=[" . g_Heroes[83].ReadLevel() . "] Benched=[" . g_Heroes[83].ReadBenched() . "]) failed to activate with retryCount=[" . retryCount . "]")
				this.UsedUlt:=false
			}
			else
			{
				this.Redraws++
				this.UseDMUlt()
			}
		}
		else
		{
			if (g_Heroes[99].CanUseUltimate()) ;Somehow Elly's ult isn't ready by DM's is - try using it
				this.UseDMUlt(0) ;No timeout since Elly's ult is not in progress (this.UsedUlt is false) and has not just been attempted
			else ;Lower max re-rolls so we move on; this Casino is busted
			{
				g_IBM.Logger.AddMessage("Casino Elly(Level=[" . g_Heroes[83].ReadLevel() . "] Benched=[" . g_Heroes[83].ReadBenched() . "]) Ult not available and DM(Level=[" . g_Heroes[99].ReadLevel() . "] Benched=[" . g_Heroes[83].ReadBenched() . "]) Ult not available - lowered max rerolls to [" . this.Redraws . "]")
				this.MaxRedraws:=this.Redraws 
			}
		}
	}

	UseDMUlt(sleepTime:=30) ;30ms default sleep is for use after Elly's ult triggers, to let the game process it
	{
		if (g_Heroes[99].CanUseUltimate())
		{
			g_IBM.IBM_Sleep(sleepTime)
			retryCount:=g_Heroes[99].UseUltimate(50)
			if (retryCount=="" OR retryCount>50) ;Failed to find key, or failed to register
			{
				g_IBM.Logger.AddMessage("Casino DM(Level=[" . g_Heroes[99].ReadLevel() . "] Benched=[" . g_Heroes[99].ReadBenched() . "]) failed to activate with retryCount=[" . retryCount . "]")
			}
		}
	}
}

class IC_BrivMaster_Logger_Class ;A class for recording run logs
{
	__New(logDir)
	{
		FormatTime, formattedDateTime,, % g_IBM_Settings["IBM_Format_Date_File"] ;Can't include : in a filename so using the less human friendly version here
		if (!FileExist(logDir)) ;Create the log subdirectory if not present
			FileCreateDir, %logDir%
		this.logBase:=logDir . "\RunLog_" . formattedDateTime ;A separate variable so other logs can use a matching start time, e.g. RunLog_20250101T000000.csv from this class and RunLog_20250101T000000_Relay.csv
		this.miniLogPath:=logDir . "\MiniLog.json" ;Needs to be set in all cases as the minilog can be turned on whilst running TODO: Should we allow this?
		this.logPath:=this.logBase . ".csv" ;The path and name for the main log specifically
		reset:=g_SF.Memory.ReadResetsTotal()
		if (reset!="") ;If we can read the current reset use that, otherwise set to -1 for invalid
			g_SharedData.UpdateOutbound("RunLogResetNumber",reset)
		else
			g_SharedData.UpdateOutbound("RunLogResetNumber",-1)
		g_SharedData.UpdateOutbound("RunLog",{})
		this.LogEntries:={}
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
			g_SharedData.UpdateOutbound("RunLogResetNumber",-1) ;Invalid whilst updating
			logEntryJSON:=AHK_JSON.Dump(this.LogEntries.Run)
			g_SharedData.UpdateOutbound("RunLog",logEntryJSON)
			g_SharedData.UpdateOutbound("RunLogResetNumber",this.LogEntries.Run.ResetNumber)
			;Output log
			loadTime:=this.LogEntries.Run.ActiveStart - this.LogEntries.Run.Start
			resetTime:=this.LogEntries.Run.End - this.LogEntries.Run.ResetReached
			runString:=this.LogEntries.Run.ResetNumber . "," . this.LogEntries.Run.StartRealTime . "," . this.LogEntries.Run.Start . "," ;Reset #,Start Time,Start Tick
			runString.=this.LogEntries.Run.End - this.LogEntries.Run.Start . "," . this.LogEntries.Run.ResetReached - this.LogEntries.Run.ActiveStart . "," . loadTime + resetTime . "," ;Total,Active,Wait
			runString.=loadTime . "," . resetTime . "," . this.LogEntries.Run.Cycle . "," ;Load,Reset,Cycle
			runString.=this.LogEntries.Run.Fail . "," . this.LogEntries.Run.LastZone . "," . g_SF.Memory.ReadChestCountByID(282) ;Fail,LastZone,Electrum. Note the electrum is for interest due to Diana's bug (or more generally the scavenger bug), not really related to farming
			if(g_IBM_Settings["IBM_Logger_MiniLog"])
			{
				try
				{
					if(FileExist(this.miniLogPath))
						FileDelete % this.miniLogPath
					FileAppend, %logEntryJSON%, % this.miniLogPath
				}
				catch err
					this.AddMessage("Minilog output failed: " . err.Message)
			}
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
		FormatTime, formattedDateTime,,% g_IBM_Settings["IBM_Format_Date_Display"]
		this.LogEntries.Run.StartRealTime:=formattedDateTime
		this.LogEntries.Run.ResetNumber:=g_SF.Memory.ReadResetsTotal()
		this.LogEntries.Run.GHActive:=g_SF.Memory.IBM_IsBuffActive("Potion of the Gem Hunter") ;Does this break in non-English clients?
		this.LogEntries.Run.LastZone:=0
		this.LogEntries.Run.Fail:=false
		this.LogEntries.Run.Cycle:=""
	}

	OutputHeader()
	{
		FileAppend, % "Reset #,Start Time,Start Tick,Total,Active,Wait,Load,Reset,Cycle,Fail,LastZone,Electrum," . g_IBM.RouteMaster.GetStrategyString() . "`n", % this.logPath
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
		if(g_IBM_Settings["IBM_Logger_ZoneLog"])
			this.AddMessage("z" . zone . " intent: " . (g_IBM.routeMaster.ShouldWalk(zone) ? "E" : "Q") . " to z" . g_IBM.routeMaster.zones[zone].nextZone.z)
	}
}

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

