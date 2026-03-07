;This file is intended for classes used across both the gem farm script and the hub
#include %A_LineFile%\..\IC_BrivMaster_Memory.ahk

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

	LoadObjectFromAHKJSON(fileName,preserveBooleans:=false) ;If preserveBooleans is set 'true' and 'false' will be read as strings rather than being converted to -1 or 0, as AHK does not have a boolean type. Needed for game settings file
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
		this.gameFocus()
	}

	addKey(key)
	{
		if (!this.keyList.HasKey(key))
			this.keyList[key]:=new IC_BrivMaster_InputManager_Class.IC_BrivMaster_InputManager_Key_Class(key)
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
	
	class IC_BrivMaster_InputManager_Key_Class ;Represents a single key. Used by IC_BrivMaster_InputManager_Class
	{
		__new(key)
		{
			this.key:=key
			if(g_IBM_Settings["IBM_Scan_Codes"].HasKey(key)) ;Following key logic taken from SH_KeyHelper.ahk
			{
				formattedSC:=Format("sc{:X}", g_IBM_Settings["IBM_Scan_Codes",key])    ;Reformat for use in GetKeyVK (sc + hex. e.g. scC0)
				vk:=GetKeyVK(formattedSC)            ;Get virtual key value (dec)
				this.mappedKey:=Format("0x{:X}", vk) ;convert virtual key to hex code 
				sc:=g_IBM_Settings["IBM_Scan_Codes",key] << 16
				this.lparamDown:=Format("0x{:X}", 0x0 | sc)
				this.lparamUp:=Format("0x{:X}", 0xC0000001 | sc)
			}
			else
				g_IBM.Logger.AddMessage("InputManager: No scancode for key=[" . key . "]")
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
}

class IC_BrivMaster_ServerCall_Class extends IBM_ServerCall_Class
{
	userID:=0 ;TODO: Review the population and updating of all of these class variables
    userHash:=""
    instanceID:=0
    networkID:=11
    clientVersion:=999
    activeModronID:=1
    activePatronID:=0
    dummyData:=""
    webRoot:="http://ps22.idlechampions.com/~idledragons/" ;Default
    timeoutVal:=60000

	__New(userID:=0, userHash:=0, instanceID:=0)
    {
        this.userID:=userID
        this.userHash:=userHash
        this.instanceID:=instanceID
        this.shinies:=0
        this.md5Module:=DllCall("LoadLibrary", "Str", "advapi32.dll", "Ptr")
        return this
    }

	CallPreventStackFail(sprint, steelbones, message,launchScript:=false) ;This function should be called after checking sprint & steelbones are valid - i.e. move 0 if no value is present. TODO: Can maybe bring this in too?
    {
        stacks:=sprint + FLOOR(steelbones * g_IBM.RouteMaster.stackConversionRate)
		g_IBM.Logger.AddMessage("Servercall Save via: "  . message . " Converted Haste=[" . stacks . "] from Haste=[" . sprint . "] and Steelbones=[" . steelbones . "] with stackConversionRate=[" . Round(g_IBM.RouteMaster.stackConversionRate,1) . "]")
		jsonString:="{""stats"":{""briv_steelbones_stacks"":0,""briv_sprint_stacks"":" . stacks . "}}"
		boundaryHeader:=this.GetBoundryHeader()
		save:=this.GetSaveFromJSON(jsonString,boundaryHeader)
		if(launchScript) ;Do server call from new script to prevent hanging script due to network issues.
        {
            webRoot:=this.webRoot
            scriptLocation:=A_LineFile . "\..\IC_BrivMaster_SaveStacks.ahk"
            Run, %A_AhkPath% "%scriptLocation%" "%webRoot%" "%save%" "%boundaryHeader%"
        }
        else
        {
            try
                response:=this.ServerCallSave(save,boundaryHeader)
            catch
                g_IBM.Logger.AddMessage("Failed to save Briv stacks")			
        }
		return response ;Note this will only be meaningful for the synchronous version of the call
    }
	
    ServerCallSave(saveBody,boundaryHeader,retryNum:=0) ; Special server call specifically for use with saves. saveBody must be encoded before using this call.
    {
        response:=""
        WR:=ComObjCreate( "WinHttp.WinHttpRequest.5.1" )
        ; https://learn.microsoft.com/en-us/windows/win32/winhttp/iwinhttprequest-settimeouts defaults: 0 (DNS Resolve), 60000 (connection timeout. 60s), 30000 (send timeout), 60000 (receive timeout)
        WR.SetTimeouts( "0", "15000", "7500", "30000" )
        Try 
		{
            WR.Open("POST",this.webroot . "post.php?call=saveuserdetails&", true)
            WR.SetRequestHeader("Accept-Encoding", "identity")
			WR.SetRequestHeader("Content-Type", "multipart/form-data; boundary=""" . boundaryHeader . """")
            WR.SetRequestHeader("User-Agent", "BestHTTP")
            WR.Send(saveBody)
            WR.WaitForResponse(-1)
            data:=WR.ResponseText
            if(data) ;Don't try to JSON.Load the string if empty TODO: Review this codepath, does it retry in the no response case? I don't think so...
			{
				try
				{
					response:=AHK_JSON.Load(data)
					if(!(response.switch_play_server=="")) ;NOT(response.switch_play_server=="") => switch_play_server exists. TODO: Should this retry when the server was valid? Probably not given the nature of this call - if the server is down we probably want to return to the run
					{
						retryNum++
						this.WebRoot:=response.switch_play_server
						if(retryNum<=3)
						{						
							WR:=""
							return this.ServerCallSave(saveBody,boundaryHeader,retryNum) 
						}
					}
				}
			}
        }
		WR:=""
        return response
    }

    __Delete() ;Free library after use
    {
        DllCall("FreeLibrary", "Ptr", this.md5Module)
    }

    MD5Save(stringVal) ;Creates a salted md5 checksum for a save string. Modified from https://www.autohotkey.com/boards/viewtopic.php?f=6&t=21
    {
        stringVal:=stringVal . "somethingpoliticallycorrect"
        VarSetCapacity(MD5_CTX, 104, 0)
		DllCall("advapi32\MD5Init", "Ptr", &MD5_CTX)
        DllCall("advapi32\MD5Update", "Ptr", &MD5_CTX, "AStr", stringVal, "UInt", StrLen(stringVal))
        DllCall("advapi32\MD5Final", "Ptr", &MD5_CTX)
        loop, 16
            o.=Format("{:02" (case ? "X" : "x") "}", NumGet(MD5_CTX, 87 + A_Index, "UChar"))
        StringLower, o,o
        return o
    }

    GetSaveFromJSON(jsonString,boundaryHeader,timeStamp:="0") ;Converts user's data into form data that can be submitted for a save
    {
		userData:=g_zlib.Deflate(jsonString)
		checksum:=this.MD5Save(jsonString)
		Random, r1, 0, 65535
		Random, r2, 0, 65535
		boundrySuffix:=Format("{:04X}", r2) . Format("{:04X}", r1) ;Random is limited to signed int32, so instead of faffing about with that just glue two 16-bit values together
        mimicSave:="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""call""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 15`r`n`r`n"
        mimicSave.="saveuserdetails`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""language_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 1`r`n`r`n"
        mimicSave.="1`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""user_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  StrLen(this.userID)  "`r`n`r`n"
        mimicSave.=this.userID . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""hash""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 32`r`n`r`n"
        mimicSave.=this.userHash . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""details_compressed""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  (StrLen(userData))  "`r`n`r`n"
        mimicSave.=userData . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""checksum""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 32`r`n`r`n"
        mimicSave.=checksum . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""timestamp""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  StrLen(timeStamp)  "`r`n`r`n"
        mimicSave.=timeStamp . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""request_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 1`r`n`r`n"
        mimicSave.="1`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""network_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: " StrLen(this.networkID)  "`r`n`r`n"
        mimicSave.=this.networkID . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""mobile_client_version""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  StrLen(this.clientVersion)  "`r`n`r`n"
        mimicSave.=this.clientVersion . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""instance_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  StrLen(this.instanceID)  "`r`n`r`n"
        mimicSave.=this.instanceID . "`r`n"
        mimicSave.="--" . boundaryHeader . "--`r`n"
        return mimicSave
    }
	
	GetBoundryHeader()
	{
		Random, r1, 0, 65535
		Random, r2, 0, 65535
		return "BestHTTP_HTTPMultiPartForm_" . Format("{:04X}", r2) . Format("{:04X}", r1) ;Random is limited to signed int32, so instead of faffing about with that just glue two 16-bit values together
	}

    UpdateDummyData()
    {
        this.dummyData:="&language_id=1&timestamp=0&request_id=0&network_id=" . this.networkID . "&mobile_client_version=" . this.clientVersion . "&offline_v2_build=1"
    }

    ;============================================================
    ;Various server call functions that should be pretty obvious.
    ;============================================================
    ;Except this one, it is used internally and shouldn't be called directly.
    ServerCall(callName, parameters, timeout:="", retryNum:=0) 
    {
        response:=""
        URLtoCall:=this.webRoot . "post.php?call=" . callName . parameters
        timeout:=timeout ? timeout : this.timeoutVal
        WR:=ComObjCreate("WinHttp.WinHttpRequest.5.1")
        WR.SetTimeouts(0,45000,30000,timeout) ;https://learn.microsoft.com/en-us/windows/win32/winhttp/iwinhttprequest-settimeouts defaults: 0 (DNS Resolve), 60000 (connection timeout. 60s), 30000 (send timeout), 60000 (receive timeout)
        Try
		{
            WR.Open("POST",URLtoCall,true)
            WR.SetRequestHeader("Content-Type","application/x-www-form-urlencoded")
            WR.Send()
            WR.WaitForResponse(-1)
            data:=WR.ResponseText
            Try
            {
                response:=AHK_JSON.Load(data)
                if(!(response.switch_play_server==""))
                {
                    retryNum++
                    this.WebRoot:=response.switch_play_server
                    if(retryNum<=3) 
                        return this.ServerCall(callName,parameters,timeout,retryNum)
                }
            }
        }
		WR:=""
        return response
    }

	/* Only used by IsOnWorldMap which we've commented out
    CallUserDetails() ; Pulls user details from the server and returns it in a json parsed object
    {
        getUserParams := this.dummyData . "&include_free_play_objectives=true&instance_key=1&user_id=" . this.userID . "&hash=" . this.userHash
        userDetails := this.ServerCall( "getuserdetails", getUserParams )
        return userDetails
    }
	*/

    CallLoadAdventure(adventureToLoad) ;Starts a new adventure and returns the response
    {
        patronTier:=this.activePatronID ? 1 : 0
        advParams:=this.dummyData . "&patron_tier=" . patronTier . "&user_id=" . this.userID . "&hash=" . this.userHash . "&instance_id=" . this.instanceID 
            . "&game_instance_id=" . this.activeModronID . "&adventure_id=" . adventureToLoad . "&patron_id=" . this.activePatronID
        return this.ServerCall("setcurrentobjective", advParams)
    }

    CallEndAdventure() ;Calling this loses everything earned during the adventure, should only be used when stuck
    {
        advParams:=this.dummyData "&user_id=" this.userID "&hash=" this.userHash "&instance_id=" this.instanceID "&game_instance_id=" this.activeModronID
        return this.ServerCall("softreset",advParams)
    }

    CallBuyChests(chestID,chests,chestType:="") ;Buys <chests> number of <chestID> chests. Automatically uses Patron purchase call for patron chests. TODO: Either add Elminster patron, or strip this back to just basic chests
    {
        if (chests>250)
            chests:=250
        else if (chests<1)
            return
        if(chestType=="eventV2")
        {
            chestParams := this.dummyData "&user_id=" this.userID "&hash=" this.userHash "&instance_id=" this.instanceID "&chest_type_id=" chestID "&count=" chests "&spend_event_v2_tokens=1"
            return this.ServerCall("buysoftcurrencychest",chestParams)
        }
        else if(chestID!=152 AND chestID!=153 AND chestID!=219  AND chestID!=311)
        {
            chestParams:=this.dummyData "&user_id=" this.userID "&hash=" this.userHash "&instance_id=" this.instanceID "&chest_type_id=" chestID "&count=" chests
            return this.ServerCall("buysoftcurrencychest",chestParams)
        }
        else
        {
            switch chestID
            {
                case 152:
                    itemID := 1
                    patronID := 1
                case 153:
                    itemID := 23
                    patronID := 2
                case 219:
                    itemID := 45
                    patronID := 3
                case 311:
                    itemID := 76
                    patronID := 4
                Default:
                    return ""
            }
            chestParams:=this.dummyData "&user_id=" this.userID "&hash=" this.userHash "&instance_id=" this.instanceID "&patron_id=" patronID "&shop_item_id=" itemID
            return this.ServerCall( "purchasepatronshopitem", chestParams )
        }
    }

    CallOpenChests(chestID, chests) ;Open <chests> number of <chestID> chest.
    {
        if (chests>1000)
            chests:=1000
        else if (chests<1)
            return
        chestParams:="&gold_per_second=0&checksum=4c5f019b6fc6eefa4d47d21cfaf1bc68&user_id=" this.userID "&hash=" this.userHash 
            . "&instance_id=" this.instanceID "&chest_type_id=" chestid "&game_instance_id=" this.activeModronID "&count=" chests
        return this.ServerCall("opengenericchest",chestParams,60000)
    }

	/* ;Not actually used, we just seem to assume LoadAdventure will work after sending the end adventure call
    ;A method to check if the party is on the world map. Necessary state to use callLoadAdventure()
    IsOnWorldMap()
    {
        currentAdventure := 0
        userDetails := this.CallUserDetails()
        if ( !IsObject( userDetails ) )
            return "Failed to fetch or build user details."
        for k, v in userDetails.details.game_instances
        {
            if (v.game_instance_id == this.activeInstanceID) 
            {
                currentAdventure := v.current_adventure_id
            }
        }
        if ( currentAdventure == -1 )
            return 1
        else
            return 0
    }
	*/
    
    CallGetPlayServer() ;Get the loadbalanced Play Server
    {
        return this.ServerCall("getPlayServerForDefinitions", this.dummyData)
    }

    UpdatePlayServer() ;TODO: Consider how this interacts with the webroot memory read
    {
		oldWebRoot:=this.webRoot
		this.webRoot:="http://ps23.idlechampions.com/~idledragons/" ;Assume ps23 will always be available (avoiding using master) TODO: Why do we call ps23 and not the current server to check this?
		response:=this.CallGetPlayServer()
		if (response!="" AND response.play_server!="")
			this.webRoot:=response.play_server
		else
			this.webRoot:=oldWebRoot
		;Note: A repeat this.CallGetPlayServer() call and logic was removed, it might have been for debugging...or might have served an actual purpose
    }
}

class IBM_ServerCall_Class ;Simple generic servercall class, this is SH_ServerCalls without the proxy settings
{
    __New()
    {
        return this
    }

    BasicServerCall(url, timeout:=60000) 
    {
        response:=""
        WR:=ComObjCreate("WinHttp.WinHttpRequest.5.1")
        WR.SetTimeouts( 0, 45000, 30000, timeout)
        Try
		{
            WR.Open("GET", Url, true)
            WR.SetRequestHeader( "Content-Type","application/x-www-form-urlencoded" )
            WR.SetRequestHeader( "Accept","application/json" )
            WR.Send()
            WR.WaitForResponse(-1)
            data:=WR.ResponseText
            Try
            {
                response:=AHK_JSON.Load(data) ;We could potentially handle an empty return / exception from AKH_JSON.Load() rather than have BasicServerCallRaw(), but that might mean something JSON-adjacent gets processed when it shouldn't
            }
        }
        catch exception
		{
			WR:=""
			return exception
		}
        WR:=""
		return response
    }
	
	BasicServerCallRaw(url, timeout:=60000) ;Does not parse as JSON
    {
        data:=""
        WR:=ComObjCreate("WinHttp.WinHttpRequest.5.1")
        WR.SetTimeouts( 0, 45000, 30000, timeout)
        Try
		{
            WR.Open("GET", Url, true)
            WR.SetRequestHeader( "Content-Type","application/x-www-form-urlencoded" )
            WR.SetRequestHeader( "Accept","application/json" )
            WR.Send()
            WR.WaitForResponse(-1)
            data:=WR.ResponseText
        }
        catch exception
		{
			WR:=""
			return exception
		}
        WR:=""
		return data
    }
}

class IBM_Theme
{
    __new()
	{
		this.defaultFontSize:=8
		this.Theme:=g_IBM_Settings["IBM_Theme_Current"]
	}
	
    UseThemeTextColor(guiName, textType:="DefaultText", weight:=400) ;Sets the color/weight for subsequent text based on the theme
    {
        textColor:=Format("{:#x}", this.Theme[textType])
        Gui, %guiName%:Font, % "c" . textColor . " w" . weight . " s" . this.defaultFontSize
    }
	
	GetThemeTextColour(textType:="DefaultText") ;Returns the colour value, including the 'c' prefix, for a theme colour. Needed when changing text colour dynamically
    {
        return "c" . Format("{:#x}", this.Theme[textType])
    }

	GetThemeBackgroundColor()
    {
        return Format("{:#x}", this.Theme["WindowColor"]) ;No 'c' prefix here
    }

	GetThemeListViewBackgroundColor()
    {
		return Format("{:#x}", this.Theme["TableBackgroundColor"]) ;No 'c' prefix here
    }

    ; Sets the window title bar to dark if theme is a dark theme. GUI must be shown before calling.
    UseThemeTitleBar(guiName)
    {
        if(this.Theme.DarkMode)
        {
            if (A_OSVersion>="10.0.17763" AND SubStr(A_OSVersion, 1, 3)="10.")
            {
                attr:=19
                if (A_OSVersion>="10.0.18985")
                    attr:=20
                Gui, %guiName%: +hwndGuiID
                DllCall("dwmapi\DwmSetWindowAttribute", "ptr", GuiID, "int", attr, "int*", true, "int", 4)             
                Gui, %guiName%:Hide
                Gui, %guiName%:Show
            }
        }
    }
}