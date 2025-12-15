#include %A_LineFile%\..\..\IC_Core\IC_SharedFunctions_Class.ahk
#include %A_LineFile%\..\IC_BrivMaster_Memory.ahk

class IC_BrivMaster_SharedFunctions_Class extends IC_SharedFunctions_Class
{
	;TODO: Determine if these class variables are needed
	steelbones := ""
    sprint := ""
    PatronID := 0
	
	__new()
    {
        this.Memory := New IC_BrivMaster_MemoryFunctions_Class(A_LineFile . "\..\..\IC_Core\MemoryRead\CurrentPointers.json")
    }
	
	;Overriden to allow a string to be passed to OpenIC() to aid debugging, and to avoid using recursion
	;Reopens Idle Champions if it is closed. Calls RecoverFromGameClose after opening IC. Returns true if window still exists.
    SafetyCheck()
    {
        if (Not WinExist( "ahk_exe " . g_IBM_Settings["IBM_Game_Exe"]))
        {
            openResult:=this.OpenIC("Called from SafetyCheck()")
			while(openResult==-1) ;OpenIC returns -1 when it times out
            {
				this.CloseIC("Failed to start Idle Champions")
				openResult:=this.OpenIC("Called from SafetyCheck() loop")
            }
            if(this.Memory.ReadResetting() AND this.Memory.ReadCurrentZone() <= 1 AND this.Memory.ReadCurrentObjID() == "")
                this.WorldMapRestart()
            this.RecoverFromGameClose()
            this.BadSaveTest()
            return false
        }
        else if ( this.Memory.ReadCurrentZone() == "" )  ; game loaded but can't read zone? failed to load proper on last load? (Tests if game started without script starting it)
        {
            g_IBM.Logger.AddMessage("SafetyCheck() Resetting process reader - old PID=[" . g_SF.PID . "] and Hwnd=[" . g_SF.Hwnd . "] ")
			gameExe := g_IBM_Settings["IBM_Game_Exe"]
			this.Hwnd := WinExist("ahk_exe " . gameExe)
            Process, Exist, %gameExe%
            this.PID := ErrorLevel
            this.Memory.OpenProcessReader()
            this.ResetServerCall()
			g_IBM.Logger.AddMessage("SafetyCheck() Reset process reader - new PID=[" . g_SF.PID . "] and Hwnd=[" . g_SF.Hwnd . "] ")
        }
        return true
    }
	
	BadSaveTest() ;TODO: Given this is 4 lines of code, is there a need for it to be a separate function?
    {
        if(g_IBM.currentZone != "" and g_IBM.currentZone - 1 > g_SF.Memory.ReadCurrentZone())
            g_SharedData.IBM_UpdateOutbound_Increment("TotalRollBacks")
        else if (g_IBM.currentZone != "" and g_IBM.currentZone < g_SF.Memory.ReadCurrentZone())
			g_SharedData.IBM_UpdateOutbound_Increment("BadAutoProgress")
    }
	
	;A test if stuck on current area. After 35s, toggles autoprogress every 5s. After 45s, attempts falling back up to 2 times. After 65s, restarts level.
    CheckifStuck(isStuck:=false)
    {
        static lastCheck := 0
        static fallBackTries := 0
        if (isStuck)
        {
            this.RestartAdventure("Game is stuck z[" . this.Memory.ReadCurrentZone() . "]")
            this.SafetyCheck()
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
            this.Hwnd := WinExist("ahk_exe " . g_IBM_Settings["IBM_Game_Exe"])
            this.Memory.OpenProcessReader()
            this.ResetServerCall()
            ; try a fall back
            this.FallBackFromZone()
            g_IBM.RouteMaster.SetFormation() ;In the base script this just goes to Q, which might not be ideal, especially for feat swap
            g_IBM.RouteMaster.ToggleAutoProgress(1, true)
            lastCheck := dtCurrentZoneTime
            fallBackTries++
        }
        if (dtCurrentZoneTime > 65)
        {
            this.RestartAdventure( "Game is stuck z[" . this.Memory.ReadCurrentZone() . "]" )
            this.SafetyCheck()
            g_PreviousZoneStartTime := A_TickCount
            lastCheck := 0
            fallBackTries := 0
            return true
        }
        return false
    }
	
	;Overridden to add logging for debugging problems and to handle effects that change Briv's stack conversion
	;Uses server calls to test for being on world map, and if so, start an adventure (CurrentObjID). If force is declared, will use server calls to stop/start adventure.
	RestartAdventure( reason := "" )
    {
		g_SharedData.IBM_UpdateOutbound("LoopString","ServerCall: Restarting adventure")
		g_IBM.Logger.AddMessage("Forced Restart (Reason:" . reason . " at:z" . this.Memory.ReadCurrentZone() . " with haste:" . this.Memory.ReadHasteStacks() . ")")
		this.CloseIC(reason)
		g_SharedData.IBM_UpdateOutbound("LoopString","ServerCall: Checking stack conversion")
		if (this.steelbones != "")
			convertedSteelbones:=FLOOR(this.steelbones * g_IBM.RouteMaster.stackConversionRate) ;Handle Thunder Step
		if (this.sprint != "" AND this.steelbones != "" AND (this.sprint + convertedSteelbones)<=176046)
		{
			g_IBM.Logger.AddMessage("Servercall Save (Haste:" . this.sprint . " Steelbones[Raw:" . this.steelbones . " Converted:" . convertedSteelbones . "] for a total of:" . this.sprint + convertedSteelbones . ")")
			response := g_serverCall.CallPreventStackFail(this.sprint + convertedSteelbones)
		}
		else if (this.sprint != "" AND this.steelbones != "")
		{
			g_IBM.Logger.AddMessage("Servercall Save (Haste:" . this.sprint . " raw Steelbones:" . this.steelbones . " which should convert to:" . convertedSteelbones . ")")
			response := g_serverCall.CallPreventStackFail(this.sprint + convertedSteelbones)
			g_SharedData.IBM_UpdateOutbound("LoopString","ServerCall: Restarting with >176k stacks, some stacks lost")
		}
		else
		{
			g_IBM.Logger.AddMessage("Servercall Save Not Required (Haste:" . this.sprint . " raw Steelbones:" . this.steelbones . " which should convert to:" . convertedSteelbones . ")")
			g_SharedData.IBM_UpdateOutbound("LoopString","ServerCall: Restarting adventure (no manual stack conv.)")
		}
		response:=g_ServerCall.CallEndAdventure()
		response:=g_ServerCall.CallLoadAdventure( this.CurrentAdventure )
		g_IBM.TriggerStart:=true
    }
	
	RecoverFromGameClose()
    {
        StartTime:=A_TickCount
        ElapsedTime:=0
        timeout:=10000 ;TODO: Does this make sense? Should it use the timeout factor?
        currentZone:=this.Memory.ReadCurrentZone()
		if(currentZone==1) ;TODO: What happens if the zone read is invalid, i.e. -1 or ""? The formation lookup will fail...which is probably appropriate
			return
		gameStartFormation:=g_IBM.RouteMaster.GetStandardFormation(currentZone)
		KEY:=g_IBM.RouteMaster.GetStandardFormationKey(currentZone)
        ElapsedTime:=0
		isCurrentFormation:=this.IsCurrentFormation(gameStartFormation)
        while(!isCurrentFormation AND ElapsedTime < timeout AND !this.Memory.ReadNumAttackingMonstersReached())
        {
			KEY.KeyPress() ;Note: Inputs in this function are covered by Critical being turned on previously via WaitForGameReady() calling WaitForFinalStatUpdates()
            g_IBM.IBM_Sleep(15) ;Fast as we do want to mash this to get it in before an enemy spawns
			isCurrentFormation:=this.IsCurrentFormation(gameStartFormation)
			ElapsedTime := A_TickCount - StartTime
        }
		timeout*=2 ;Double the timeout
        while(!isCurrentFormation AND (this.Memory.ReadNumAttackingMonstersReached() OR this.Memory.ReadNumRangedAttackingMonsters()) AND (ElapsedTime < timeout))
        {
            ElapsedTime := A_TickCount - StartTime
            this.FallBackFromZone()
            KEY.KeyPress()
            g_IBM.RouteMaster.ToggleAutoProgress(1, true)
            isCurrentFormation:=this.IsCurrentFormation(gameStartFormation)
        }
		Critical Off ;Turned On previously via WaitForGameReady() calling WaitForFinalStatUpdates()
        g_SharedData.IBM_UpdateOutbound("LoopString","Loading game finished")
    }
	
	IBM_SuspendProcess(PID,doSuspend:=True)
	{
		h:=DllCall("OpenProcess","uInt",0x1F0FFF,"Int",0,"Int",PID)
		If (!h)
			Return -1
		If (doSuspend)
			DllCall("ntdll.dll\NtSuspendProcess","Int",h)
		Else
			DllCall("ntdll.dll\NtResumeProcess","Int",h)
		DllCall("CloseHandle","Int",h)
	}
	
	IBM_WaitForUserLogin() ;Waits for the user platform login, then suspends the IC process until a defined time has past since the game closed
	{
		;Wait for user login, and ensure enough time has elapsed to trigger offline progress
		targetTime:=g_IBM_Settings["IBM_OffLine_Delay_Time"] ;Amount of time we'd like to elapse before passing platform login
		if (this.Memory.IBM_ReadIsGameUserLoaded()!=1 AND (A_TickCount - g_IBM.routeMaster.offlineSaveTime < targetTime))
		{
			g_SharedData.IBM_UpdateOutbound("LoopString","Waiting for platform login...")
			ElapsedTime:=A_TickCount - g_IBM.routeMaster.offlineSaveTime
			Critical On ;We need to catch the platform login completing before the game progresses to the userdata request
			while (this.Memory.IBM_ReadIsGameUserLoaded()!=1 AND ElapsedTime < targetTime) ;Wait for user loaded or we run out of time, then stop IC
			{
				Sleep 0 ;Need to be fast to catch this
				ElapsedTime:=A_TickCount - g_IBM.routeMaster.offlineSaveTime
			}
			Critical Off
			ElapsedTime:=A_TickCount - g_IBM.routeMaster.offlineSaveTime
			if (ElapsedTime >= targetTime) ;Don't suspend if we ran out of time waiting
			{
				return
			}
			this.IBM_SuspendProcess(g_SF.PID,True) 
			ElapsedTime:=A_TickCount - g_IBM.routeMaster.offlineSaveTime
			While (ElapsedTime < targetTime)
			{
				g_IBM.IBM_Sleep(15)
				ElapsedTime:=A_TickCount - g_IBM.routeMaster.offlineSaveTime
			}
			this.IBM_SuspendProcess(g_SF.PID,False)
		}
	}
	
	;Overridden to better order the sleeps vs the checks
	; Waits for the game to be in a ready state
    WaitForGameReady( timeout := 90000)
    {
        ;g_IBM.routeMaster.DebugTick("WaitForGameReady() start")
		if (!g_IBM.routeMaster.HybridBlankOffline AND g_IBM.routeMaster.offlineSaveTime>=0) ;If this is set by stack restart
			this.IBM_WaitForUserLogin()
		timeoutTimerStart := A_TickCount
        ElapsedTime:=0
		; wait for game to start
        g_SharedData.IBM_UpdateOutbound("LoopString","Waiting for game started...")
        gameStarted := 0
		lastInput:=-250 ;Input limiter for the escape key presses
		while( ElapsedTime < timeout AND !gameStarted)
        {	
            if (A_TickCount > lastInput+250 AND this.Memory.ReadIsSplashVideoActive())
			{
				g_IBM.KEY_ESC.KeyPress()
				lastInput:=A_TickCount
				g_IBM.IBM_Sleep(15) ;Short sleep as we've spent time on input already
			}
			else ;Longer sleep if not sending input
				g_IBM.IBM_Sleep(45)
			gameStarted := this.Memory.ReadGameStarted()
            ElapsedTime := A_TickCount - timeoutTimerStart
        }
		g_IBM.RefreshImportCheck() ;The game has started so version memory reads should be available
        ; check if game has offline progress to calculate
        offlineTime := this.Memory.ReadOfflineTime()
		if(gameStarted AND offlineTime <= 0 AND offlineTime != "")
        {
			return true ; No offline progress to calculate, game started
		}
        ; wait for offline progress to finish
        g_SharedData.IBM_UpdateOutbound("LoopString","Waiting for offline progress...")
        offlineDone := 0
		while( ElapsedTime < timeout AND !offlineDone)
        {
            g_IBM.IBM_Sleep(50)
            offlineDone := this.Memory.ReadOfflineDone()
			ElapsedTime := A_TickCount - timeoutTimerStart
        }
        ; finished before timeout
        if(offlineDone)
        {
			this.WaitForFinalStatUpdates()
			g_PreviousZoneStartTime := A_TickCount
            return true
        }
        this.CloseIC( "WaitForGameReady-Failed to finish in " . Floor(timeout/ 1000) . "s." )
        return false
    }
	
	;Override to send formation switch
	; Waits until stats are finished updating from offline progress calculations.
    WaitForFinalStatUpdates()
    {
		;g_IBM.routeMaster.DebugTick("WaitForFinalStatUpdates() start")
		g_SharedData.IBM_UpdateOutbound("LoopString","Waiting for offline progress (Area Active)...")
        ElapsedTime := 0
        ; Starts as 1, turns to 0, back to 1 when active again.
        StartTime := A_TickCount
        while(this.Memory.ReadAreaActive() AND ElapsedTime < 5000) ;This was 1736ms, which it seems can be exceeded causing things to go wierd, better to wait here a little longer
        {
            ElapsedTime := A_TickCount - StartTime
            g_IBM.IBM_Sleep(15)
        }
		;g_IBM.routeMaster.DebugTick("WaitForFinalStatUpdates() Area Active")
		formationActive:=False
		Critical On ;From here to the zone becoming active timing is important to maximise our chances of getting to the proper formation before something spawns and blocks us. This is not turned off by this function intentionally
		while(!this.Memory.ReadAreaActive() AND ElapsedTime < 7000) ;2000ms beyond the initial loop
        {
            if (!formationActive)
			{
				g_IBM.IBM_Sleep(15) ;Only sleep whilst the formation is inactive, we want to react as fast as possible once the area is active
				if (!this.Memory.IBM_IsCurrentFormationEmpty()) ;IRISIRI - Once champions start being placed we will try sending input. Was trying to make this mode responsive once the zone becomes available but that seems too early to be useful
				{
					formationActive:=True
				}
			}
			ElapsedTime := A_TickCount - StartTime
        }
		currentZone:=this.Memory.ReadCurrentZone()
		if(currentZone>1) ;Do not try to change formation if the current zone is either not valid (waste of time) or 1 (where it will override M and cause issues)
			g_IBM.RouteMaster.GetStandardFormationKey(currentZone).KeyPress()
    }

	;Override to use sleep, not sure why this spins the wheels in loops like this, but the base script does it a LOT
	FallBackFromZone(maxLoopTime:=5000)
    {
        StartTime:=A_TickCount
        ElapsedTime:=0
        while(this.Memory.ReadCurrentZone() == -1 AND ElapsedTime < maxLoopTime)
        {
            CurrentZone := this.Memory.ReadCurrentZone()
			g_IBM.IBM_Sleep(15)
			ElapsedTime := A_TickCount - StartTime
        }
        CurrentZone := this.Memory.ReadCurrentZone()
        StartTime := A_TickCount
        ElapsedTime:=0
        g_SharedData.IBM_UpdateOutbound("LoopString","Falling back from zone...")
        while(!this.Memory.ReadTransitioning() AND ElapsedTime < maxLoopTime)
        {
            g_IBM.RouteMaster.KEY_LEFT.KeyPress()
			g_IBM.IBM_Sleep(15) ;Sleep for this one as we don't want to go back multiple zones
			ElapsedTime := A_TickCount - StartTime
        }
        g_IBM.RouteMaster.WaitForTransition()
    }
	
	; Wait for Thellora (ID=139) to activate her Rush ability.
    DoRushWait(stopProgress:=false) ;Note: unknown what ReadRushTriggered() returns if she starts with 0 stacks or we have 0 favour (with the former being the case that might matter)
    {
        StartTime := A_TickCount
        ElapsedTime := 0
		levelTypeChampions:=true ;Alternate levelling types to cover both without taking too long in each loop
		g_SharedData.IBM_UpdateOutbound("LoopString","Rush Wait")
		while (!(this.Memory.ReadCurrentZone() > 1 OR g_Heroes[139].ReadRushTriggered()) AND ElapsedTime < 8000)
        {
			if (stopProgress) ;If we are doing Elly's casino after the rush we need to stop ASAP so that 1 kill (probably via Melf) doesn't jump us an extra time, possibly on the wrong formation
			{
				if (this.Memory.ReadHighestZone() > 1)
				{
					g_IBM.RouteMaster.ToggleAutoProgress(0)
					stopProgress:=false ;No need to keep checking, and allows for levelling
				}
			}
			if (levelTypeChampions)
				g_IBM.levelManager.LevelWorklist() ;Level current worklist
			else
				g_IBM.levelManager.LevelClickDamage(0) ;Level click damage
            levelTypeChampions:=!levelTypeChampions
			ElapsedTime := A_TickCount - StartTime
        }
        g_PreviousZoneStartTime := A_TickCount
    }
	
	;Overriding to:
	;1) launch with higher process priority (note that realtime requires things to be run as admin)
	;2) lower the timeout on opening the game
	;3) Address the loop Sleep applying after a sucessful load
	; Attemps to open IC. Game should be closed before running this function or multiple copies could open
    OpenIC(message:="")
    {
		waitForReadyTimeout:=10000*g_IBM_Settings["IBM_OffLine_Timeout"] ;Default is 5, so 50s
		timeoutVal := 5000*g_IBM_Settings["IBM_OffLine_Timeout"] + waitForReadyTimeout ;Default is 5, so 25s + the 50s above=75s
        loadingDone := false
        g_SharedData.IBM_UpdateOutbound("LoopString","Starting Game" . (message ? " " . message : ""))
		g_IBM.Logger.AddMessage("Starting Game" . (message ? " " . message : ""))
        WinGet, savedActive,, A ;Changed to the handle, multiple windows could have the same name
        this.SavedActiveWindow := savedActive
        StartTime := A_TickCount
        while ( !loadingZone AND ElapsedTime < timeoutVal )
        {
			this.Hwnd := 0
            ElapsedTime := A_TickCount - StartTime
            if(ElapsedTime < timeoutVal)
			{
				this.OpenProcessAndSetPID(timeoutVal - ElapsedTime)
				Process, Priority, % this.PID, Realtime
			}
            ElapsedTime := A_TickCount - StartTime
            if(ElapsedTime < timeoutVal)
            {
				this.SetLastActiveWindowWhileWaitingForGameExe(timeoutVal - ElapsedTime) ;Fixed typo
			}
            this.ActivateLastWindow()
            this.Memory.OpenProcessReader()
            ElapsedTime := A_TickCount - StartTime
            if(ElapsedTime < timeoutVal)
                loadingZone := this.WaitForGameReady(waitForReadyTimeout) ;Override the default 90000ms timeout as that seems execessive NOTE: WaitForGameReady will turn Critical On via WaitForFinalStatUpdates
            if(loadingZone)
                this.ResetServerCall()
			else
				g_IBM.IBM_Sleep(50) ;Moved this to an Else, otherwise it delays code progression when loading is sucessful
            ElapsedTime := A_TickCount - StartTime
        }
        if(ElapsedTime >= timeoutVal)
        {
			Critical Off ;Potential edge case where loadingZone was set to true but we ran out time whilst exiting the loop
			return -1 ; took too long to open
		}
        else
        {
			g_IBM.routeMaster.ResetCycleCount() ;Whatever the reason, we've gone offline and therefore don't need to restart the game again
			g_IBM.DialogSwatter_Start()
			return 0
		}
    }
	
	;Override to fix the typo in the name, and to use the Hwnd instead of window name
	;Saves this.SavedActiveWindow as the last window and waits for the game exe to load its window.
    SetLastActiveWindowWhileWaitingForGameExe(timeoutLeft := 32000)
    {
        StartTime := A_TickCount
        ; Process exists, wait for the window:
        while(!(this.Hwnd:=WinExist( "ahk_exe " . g_IBM_Settings["IBM_Game_Exe"])) AND ElapsedTime < timeoutLeft)
        {
            WinGet, savedActive,, A ;Changed to the handle, multiple windows could have the same name
            this.SavedActiveWindow := savedActive
            ElapsedTime := A_TickCount - StartTime
            g_IBM.IBM_Sleep(50)
        }
    }
	
	;Removed creation of data to return for JSON export, as it never appeared to get used after output by ResetServerCall. Removed gem and chest data as those are fully handled by the hub side
    SetUserCredentials()
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
	
	WaitForModronReset(timeout := 60000)
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
	
	;Copied unaltered from BrivGemFarm
	GetWebRoot()
    {
        tempWebRoot := this.Memory.ReadWebRoot()
        httpString := StrSplit(tempWebRoot,":")[1]
        isWebRootValid := httpString == "http" or httpString == "https"
        g_ServerCall.webroot := isWebRootValid ? tempWebRoot : g_ServerCall.webroot
    }
	
	;Override to use IBM option
	ActivateLastWindow()
    {
        if (!g_SharedData.IBM_RestoreWindow_Enabled)
            return
        g_IBM.IBM_Sleep(80)
        hwnd := this.Hwnd
        WinActivate, ahk_id %hwnd% ; Idle Champions likes to be activated before it can be deactivated Irisiri: Testing if this is true
        savedActive:="ahk_id " . this.SavedActiveWindow
		WinActivate, %savedActive%
    }
	
    OpenProcessAndSetPID(timeoutLeft:=32000) ;Runs ICs and sets this.PID
    {
        this.PID:=0
        processWaitingTimeout:=10000 ;10s
        ElapsedTime:=0
        StartTime:=A_TickCount
        while (!this.PID AND ElapsedTime < timeoutLeft )
        {
            g_SharedData.IBM_UpdateOutbound("LoopString","Opening IC...")
            programLoc := g_IBM_Settings["IBM_Game_Launch"]
            try
            {
                if (g_IBM_Settings["IBM_Game_Hide_Launcher"])
					Run, %programLoc%,,Hide, openPID
				else
					Run, %programLoc%,,,openPID
            }
            catch
            {
                MsgBox, 48, Unable to launch game, `nVerify the game location is set properly by enabling the Game Location Settings addon, clicking Change Game Location on the Briv Gem Farm tab, and ensuring the launch command is set properly.
                ExitApp
            }
			g_IBM.IBM_Sleep(15)
			if (this.GetProcessName(openPID)==g_IBM_Settings["IBM_Game_Exe"]) ;If we launch the game .exe directly (e.g. Steam) the Run PID will be the game, but for things like EGS it will not so we need to find it
				this.PID:=openPID
			else
			{
				; Add 10s (default) to ElapsedTime so each exe waiting loop will take at least 10s before trying to run a new instance of the game
				timeoutForPID:=ElapsedTime + processWaitingTimeout 
				exeName:=g_IBM_Settings["IBM_Game_Exe"]
				while(!this.PID AND ElapsedTime < timeoutForPID)
				{
					g_IBM.IBM_Sleep(50)
					Process, Exist, %exeName%
					this.PID:=ErrorLevel
					ElapsedTime:=A_TickCount - StartTime
				}
				ElapsedTime:=A_TickCount - StartTime
				g_IBM.IBM_Sleep(50)
			}
        }
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
	
    CloseIC(string:="",usePID:=false)
    {
		g_SharedData.IBM_UpdateOutbound("LastCloseReason",string)
        this.ResetServerCall() ;Check that server call object is updated before closing IC in case any server calls need to be made by the script before the game restarts TODO: Consider the scenarios where this matters that might follow from this function
        if (string!="")
            string:=": " . string
        g_SharedData.IBM_UpdateOutbound("LoopString","Closing IC" . string)
        if (usePID)
			sendMessageString := "ahk_pid " . this.PID
		else
			sendMessageString := "ahk_exe " . g_IBM_Settings["IBM_Game_Exe"]
		timeout:=2000*g_IBM_Settings["IBM_OffLine_Timeout"] ;Default is 5, so 10s
		if WinExist(sendMessageString)
			SendMessage, 0x112, 0xF060,,, %sendMessageString%,,,, %timeout% ; WinClose
		saveCompleteTime:=-1 ;Unset
		;The memory reads through the usual game instance structure become invalid before the actual saveHandler object is gone, potentially resulting in us detecting a save early and killing the game before it is done - most likely to impact slow systems. Reading the handler directly prevents that
		ADDRESS_DIRTY:=_MemoryManager.instance.getAddressFromOffsets(g_SF.Memory.GameManager.game.gameInstances[0].isDirty.BasePtr.BaseAddress,g_SF.Memory.GameManager.game.gameInstances[0].isDirty.FullOffsets*) 
		TYPE_DIRTY:=g_SF.Memory.GameManager.game.gameInstances[0].isDirty.ValueType
		ADDRESS_CURRENT_SAVE:=_MemoryManager.instance.getAddressFromOffsets(g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.SaveHandler.currentSave.BasePtr.BaseAddress,g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.SaveHandler.currentSave.FullOffsets*)
		TYPE_CURRENT_SAVE:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.SaveHandler.currentSave.ValueType
		StartTime:=A_TickCount
		while (WinExist(sendMessageString) AND A_TickCount - StartTime < timeout)
        {
            g_IBM.IBM_Sleep(15)
			if (saveCompleteTime==-1 AND saveStatus:=this.CloseIC_SaveCheck(ADDRESS_DIRTY,TYPE_DIRTY,ADDRESS_CURRENT_SAVE,TYPE_CURRENT_SAVE)) ;If saveStatus==2 then the game appears to have closed and we did not confirm the saved actually happened, but there's no value in doing a full wait when there is nothing to check so it is treated the same - either it saved and we missed it, or it won't ever save and there's no point waiting
			{
				saveCompleteTime:=A_TickCount
				g_IBM.Logger.AddMessage("CloseIC() Standard Loop "  . (saveStatus==1 ? "Save" : "Reads Invalid") . " - saveCompleteTime=[" . saveCompleteTime . "] Timeout=[" . A_TickCount - StartTime . "/" . timeout . "]")
				g_IBM.routeMaster.CheckRelayRelease()
				StartTime:=A_TickCount ;Reset timeout after save, there's no longer a reason not to close the game by force in the following loop
				timeout:=500*g_IBM_Settings["IBM_OffLine_Timeout"] ;Default is 5, so 2.5s
			}
        }
		timeout:=2000*g_IBM_Settings["IBM_OffLine_Timeout"] ;Reset to standard
        StartTime:=A_TickCount
		NextCloseAttempt:=A_TickCount ;Throttle input whilst continuing to check rapidly for game save and window closure
		while (WinExist(sendMessageString) AND A_TickCount - StartTime < timeout) ; Outright murder
		{
			if (saveCompleteTime==-1 AND saveStatus:=this.CloseIC_SaveCheck(ADDRESS_DIRTY,TYPE_DIRTY,ADDRESS_CURRENT_SAVE,TYPE_CURRENT_SAVE))
			{
				saveCompleteTime:=A_TickCount
				g_IBM.routeMaster.CheckRelayRelease()
				g_IBM.Logger.AddMessage("CloseIC() TerminateProgress Loop " . (saveStatus==1 ? "Save" : "Reads Invalid") . " - saveCompleteTime=[" . saveCompleteTime . "] Timeout=[" . A_TickCount - StartTime . "/" . timeout . "]")
			}
			if (A_TickCount >= NextCloseAttempt) 
			{
				hProcess := DllCall("Kernel32.dll\OpenProcess", "UInt", 0x0001, "Int", false, "UInt", g_SF.PID, "Ptr")
				if(hProcess)
				{
					g_IBM.Logger.AddMessage("CloseIC() failed to close cleanly: sending TerminateProcess saveCompleteTime=[" . saveCompleteTime . "] Timeout=[" . A_TickCount - StartTime . "/" . timeout . "]")
					DllCall("Kernel32.dll\TerminateProcess", "Ptr", hProcess, "UInt", 0)
					DllCall("Kernel32.dll\CloseHandle", "Ptr", hProcess)
				}
				else
				{
					g_IBM.Logger.AddMessage("CloseIC() failed to close cleanly: failed to get process handle for TerminateProcess saveCompleteTime=[" . saveCompleteTime . "] Timeout=[" . A_TickCount - StartTime . "/" . timeout . "]")
					Break ;If we can't get the handle for the process trying again isn't going to help
				}
				NextCloseAttempt:=A_TickCount+500
			}
			g_IBM.IBM_Sleep(15)
		}
		if (saveCompleteTime==-1) ;Failed to detect, going to have to go with current time
			saveCompleteTime:=A_TickCount
        return saveCompleteTime
    }
	
	CloseIC_SaveCheck(ADDRESS_DIRTY,TYPE_DIRTY,ADDRESS_CURRENT_SAVE,TYPE_CURRENT_SAVE) ;Returns 2 if either of memory reads are invalid, 1 if the game is active and has saved and 0 otherwise
	{
		dirty:=_MemoryManager.instance.read(ADDRESS_DIRTY,TYPE_DIRTY)
		currentSave:=_MemoryManager.instance.read(ADDRESS_CURRENT_SAVE,TYPE_CURRENT_SAVE)
		if(dirty=="" OR currentSave=="") ;Memory reads are gone, so game has proceeded to close. This also seems to happen if the relay fails to stop the game and the current copy has the 'Instance invalid' error
			return 2
		else if (dirty==0 AND currentSave==0) ;Save complete. Dirty appears to get set to 0 before the save instance in some cases, so best to check both
			return 1
		return 0
	}
	
	IBM_CNETimeStampToDate(timeStamp) ;Takes a timestamp in seconds-since-day-0 format and converts it to a date for AHK use
	{
		unixTime:=timeStamp-62135596800 ;Difference between day 1 (01Jan0001) and unix time (AHK doesn't support dates before 1601 so we can't just set converted:=1)
		converted:=1970
		converted+=unixTime,s
		return converted
	}
}