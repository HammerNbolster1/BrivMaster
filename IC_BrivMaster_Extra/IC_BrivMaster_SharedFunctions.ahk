;This file is intended for functions used across both the gem farm script and the hub. Currently meeting that goal is a WiP
#include %A_LineFile%\..\IC_BrivMaster_Memory.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\SH_SharedFunctions.ahk

global g_PreviousZoneStartTime ;TODO: Why is this in here? It is used by CheckifStuck - move elsewhere if that function moves. Or possibly move it anyway...

class IC_BrivMaster_SharedFunctions_Class extends SH_SharedFunctions ;TODO: This has been separated from IC Core, but check what is going on in SH_SharedFunctions that might be redundant / undesireable? Does have some unneeded functions and includes
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