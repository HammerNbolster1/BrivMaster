;This file is intended for functions used in the gem farm script, but not the hub

class IC_BrivMaster_Budget_Zlib_Class ;A class for applying z-lib compression. Badly. This is aimed at strings of <100 characters
{
	__New() ;Pre-computes binary values for various things to improve run-time performance
	{
		BASE64_CHARACTERS:="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ;RFC 4648 S4, base64
		this.BASE64_TABLE:={}
		Loop, Parse, BASE64_CHARACTERS
		{
			this.BASE64_TABLE[this.IntToBinaryString(A_Index-1,6)]:=A_LoopField ;Note: The key gets converted to a decimal number, e.g. "000100" becomes 100 base-10, but the same happens when looking up using a string of 1's and 0's so the lookup still works out. The alternative would be to force to string at both ends, which is likely more internal operations
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

	Deflate(inputString,minMatch:=3,maxMatch:=258) ;inputString must fit into a single 32K block. minMatch must be at least 3, and maxMatch must be at most 258
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

	BinaryStringToBase64(string) ;Requires string to have a length that is a multiple of 8
	{
		pos:=1
		while(pos<StrLen(string))
		{
			slice:=SubStr(string,pos,24) ;Take 24bits at a time
			sliceLen:=StrLen(slice)
			if(sliceLen==24) ;Standard case
			{
				loop, 4
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6)]
				pos+=24
			}
			else if (sliceLen==16) ;16 bits, need to pad with 2 zeros to reach 18 and be divisible by 3, then add an = to replace the last 6-set
			{
				slice.="00"
				loop, 3
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6)]
				accu.="="
				Break ;Since we're out of data
			}
			else if (sliceLen==8) ;8 bits, need to pad with 4 zeros to reach 12 and be divisible by 2, then add == to replace the last two 6-sets
			{
				slice.="0000"
				loop, 2
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6)]
				accu.="=="
				Break ;Since we're out of data
			}
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
			return "00011000" . this.IntToBinaryStringLSB(length-115,4)
		else if(length<=162)
			return "00011001" . this.IntToBinaryStringLSB(length-131,5)
		else if(length<=194)
			return "00011010" . this.IntToBinaryStringLSB(length-163,5)
		else if(length<=226)
			return "00011011" . this.IntToBinaryStringLSB(length-195,5)
		else if(length<=257)
			return "00011100" . this.IntToBinaryStringLSB(length-227,5)
		else if(length==258)
			return "00011101"
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
}

class IC_BrivMaster_Logger_Class ;A class for recording run logs
{
	__New(logDir)
	{
		FormatTime, formattedDateTime,, % g_IBM_Settings["IBM_Format_Date_File"] ;Can't include : in a filename so using the less human friendly version here
		if (!FileExist(logDir)) ;Create the log subdirectory if not present
			FileCreateDir, %logDir%
		this.logBase:=LogDir . "\RunLog_" . formattedDateTime ;A separate variable so other logs can use a matching start time, e.g. RunLog_20250101T000000.csv from this class and RunLog_20250101T000000_Relay.csv
		this.logPath:=this.logBase . ".csv" ;The path and name for the main log specifically
		reset:=g_SF.Memory.ReadResetsTotal()
		if (reset!="") ;If we can read the current reset use that, otherwise set to -1 for invalid
			g_SharedData.UpdateOutbound("RunLogResetNumber",reset)
		else
			g_SharedData.UpdateOutbound("RunLogResetNumber",-1)
		g_SharedData.UpdateOutbound("RunLog",{})
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
			g_SharedData.UpdateOutbound("RunLogResetNumber",-1) ;Invalid whilst updating
			g_SharedData.UpdateOutbound("RunLog",AHK_JSON.Dump(this.LogEntries))
			g_SharedData.UpdateOutbound("RunLogResetNumber",this.LogEntries.Run.ResetNumber)
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
		;this.AddMessage("z" . zone . " intent: " . (g_IBM.routeMaster.ShouldWalk(zone) ? "E" : "Q") . " to z" . g_IBM.routeMaster.zones[zone].nextZone.z) ;Uncomment for debugging
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

