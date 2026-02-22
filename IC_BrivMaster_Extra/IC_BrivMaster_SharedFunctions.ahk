;This file is intended for classes used across both the gem farm script and the hub. Currently meeting that goal is a WiP
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

