class IC_BrivMaster_SharedFunctions_Class extends IC_BrivSharedFunctions_Class
{
	static BASE_64_CHARACTERS := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" ;RFC 4648 S5 URL-safe, aka base64url
	
	;Overriden to allow a string to be passed to OpenIC() to aid debugging
	;Reopens Idle Champions if it is closed. Calls RecoverFromGameClose after opening IC. Returns true if window still exists.
    SafetyCheck()
    {
        ; TODO: Base case check in case safety check never succeeds in opening the game.
        if (Not WinExist( "ahk_exe " . g_userSettings[ "ExeName"] ))
        {
            if(this.OpenIC("Called from SafetyCheck()") == -1)
            {
                this.CloseIC("Failed to start Idle Champions")
                this.SafetyCheck()
            }
            if(this.Memory.ReadResetting() AND this.Memory.ReadCurrentZone() <= 1 AND this.Memory.ReadCurrentObjID() == "")
                this.WorldMapRestart()
            this.RecoverFromGameClose(this.GameStartFormation)
            this.BadSaveTest()
            return false
        }
         ; game loaded but can't read zone? failed to load proper on last load? (Tests if game started without script starting it)
        else if ( this.Memory.ReadCurrentZone() == "" )
        {
            g_BrivGemFarm.Logger.AddMessage("SafetyCheck() Resetting process reader - old PID=[" . g_SF.PID . "] and Hwnd=[" . g_SF.Hwnd . "] ")
			this.Hwnd := WinExist( "ahk_exe " . g_userSettings[ "ExeName"] )
            existingProcessID := g_userSettings[ "ExeName"]
            Process, Exist, %existingProcessID%
            this.PID := ErrorLevel
            this.Memory.OpenProcessReader()
            this.ResetServerCall()
			g_BrivGemFarm.Logger.AddMessage("SafetyCheck() Reset process reader - new PID=[" . g_SF.PID . "] and Hwnd=[" . g_SF.Hwnd . "] ")
        }
        return true
    }
	
	;Overridden for memory usage check and formation set on fallback
	;A test if stuck on current area. After 35s, toggles autoprogress every 5s. After 45s, attempts falling back up to 2 times. After 65s, restarts level.
    CheckifStuck(isStuck:=false)
    {
        static lastCheck := 0
        static fallBackTries := 0
        dtCurrentZoneTime := Round((A_TickCount - g_PreviousZoneStartTime) / 1000, 2)
        if (isStuck)
        {
            this.RestartAdventure( "Game is stuck z[" . this.Memory.ReadCurrentZone() . "]")
            this.SafetyCheck()
            g_PreviousZoneStartTime := A_TickCount
            lastCheck := 0
            fallBackTries := 0
            return true
        }
		if (dtCurrentZoneTime <= 35) ;Irisiri - added fast exit for the standard case
			return false
        else if (dtCurrentZoneTime > 35 AND dtCurrentZoneTime <= 45 AND dtCurrentZoneTime - lastCheck > 5) ; first check - ensuring autoprogress enabled
        {
            g_BrivGemFarm.RouteMaster.ToggleAutoProgress(1, true)
            if(dtCurrentZoneTime < 40)
                lastCheck := dtCurrentZoneTime
        }
        if (dtCurrentZoneTime > 45 AND fallBackTries < 3 AND dtCurrentZoneTime - lastCheck > 15) ; second check - Fall back to previous zone and try to continue
        {
            ; reset memory values in case they missed an update.
            this.Hwnd := WinExist( "ahk_exe " . g_userSettings[ "ExeName"] )
            this.Memory.OpenProcessReader()
            this.ResetServerCall()
            ; try a fall back
            this.FallBackFromZone()
            g_BrivGemFarm.RouteMaster.SetFormation() ;In the base script this just goes to Q, which might not be ideal, especially for feat swap
            g_BrivGemFarm.RouteMaster.ToggleAutoProgress(1, true)
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
	
	;Used to monitor IC's memory usage
	/*
    IBM_GetWorkingSetPrivateSize(processID)
	{
	   static SYSTEM_INFORMATION_CLASS := 0x5
	   if (DllCall("Ntdll\NtQuerySystemInformation", "UInt", SYSTEM_INFORMATION_CLASS, "Ptr", 0, "UInt", 0, "UInt*", Size, "Int") != 0)
	   {
		  VarSetCapacity(SYSTEM_PROCESS_INFORMATION, Size), Offset := 0
		  if (DllCall("Ntdll\NtQuerySystemInformation", "UInt", SYSTEM_INFORMATION_CLASS, "Ptr", &SYSTEM_PROCESS_INFORMATION, "UInt", Size, "UInt*", 0, "Int") = 0)
		  {
			 Loop
			 {
				WorkingSetPrivateSize := NumGet(SYSTEM_PROCESS_INFORMATION, Offset + 8, "Int64")
				UniqueProcessId := NumGet(SYSTEM_PROCESS_INFORMATION, Offset + 56 + 3 * A_PtrSize, "Ptr")
				if (UniqueProcessId = processID)
				   return WorkingSetPrivateSize
				NextEntryOffset := NumGet(SYSTEM_PROCESS_INFORMATION, Offset, "UInt")
				Offset += NextEntryOffset
			 } Until !NextEntryOffset
		  }
	   }
	}
	*/
	/*
	IBM_GetWorkingSetPrivateSize(processID) ;This version is not the private set - looks like commit charge? Which might be the relevant one for memory crashes
	{
		static PMC_EX, size := NumPut(VarSetCapacity(PMC_EX, 8 + A_PtrSize * 9, 0), PMC_EX, "uint")
		if (hProcess := DllCall("OpenProcess", "uint", 0x1000, "int", 0, "uint", processID))
		{
			if !(DllCall("GetProcessMemoryInfo", "ptr", hProcess, "ptr", &PMC_EX, "uint", size))
				if !(DllCall("psapi\GetProcessMemoryInfo", "ptr", hProcess, "ptr", &PMC_EX, "uint", size))
					return (ErrorLevel := 2) & 0, DllCall("CloseHandle", "ptr", hProcess)
			DllCall("CloseHandle", "ptr", hProcess)
			return NumGet(PMC_EX, 8 + A_PtrSize * 8, "uptr")
		}
		return (ErrorLevel := 1) & 0
	}
	*/
	
	;Overridden to add logging for debugging problems and to handle effects that change Briv's stack conversion
	;Uses server calls to test for being on world map, and if so, start an adventure (CurrentObjID). If force is declared, will use server calls to stop/start adventure.
	RestartAdventure( reason := "" )
    {
		g_SharedData.LoopString := "ServerCall: Restarting adventure"
		g_BrivGemFarm.Logger.AddMessage("Forced Restart (Reason:" . reason . " at:z" . this.Memory.ReadCurrentZone() . " with haste:" . this.Memory.ReadHasteStacks() . ")")
		this.CloseIC(reason)
		if (this.steelbones != "")
			convertedSteelbones:=FLOOR(this.steelbones * g_BrivGemFarm.RouteMaster.stackConversionRate) ;Handle Thunder Step
		if (this.sprint != "" AND this.steelbones != "" AND (this.sprint + convertedSteelbones) < 190000)
		{
			g_BrivGemFarm.Logger.AddMessage("Servercall Save (Haste:" . this.sprint . " Steelbones[Raw:" . this.steelbones . " Converted:" . convertedSteelbones . "] for a total of:" . this.sprint + convertedSteelbones . ")")
			response := g_serverCall.CallPreventStackFail(this.sprint + convertedSteelbones)
		}
		else if (this.sprint != "" AND this.steelbones != "")
		{
			g_BrivGemFarm.Logger.AddMessage("Servercall Save (Haste:" . this.sprint . " raw Steelbones:" . this.steelbones . " which should convert to:" . convertedSteelbones . ")")
			response := g_serverCall.CallPreventStackFail(this.sprint + convertedSteelbones)
			g_SharedData.LoopString := "ServerCall: Restarting with >190k stacks, some stacks lost."
		}
		else
		{
			g_BrivGemFarm.Logger.AddMessage("Servercall Save Not Required (Haste:" . this.sprint . " raw Steelbones:" . this.steelbones . " which should convert to:" . convertedSteelbones . ")")
			g_SharedData.LoopString := "ServerCall: Restarting adventure (no manual stack conv.)"
		}
		response:=g_ServerCall.CallEndAdventure()
		response:=g_ServerCall.CallLoadAdventure( this.CurrentAdventure )
		g_BrivGemFarm.TriggerStart:=true
    }
	
	;Override to remove swap to E when feat swapping. TODO: Why did this swap to E anyway? Just using a normal SetFormation
	;This is called when trying to stack, if for some reason we're trying to stack on a boss zone A) things have gone weird (fallback maybe?) and B) We should complete on the expected formation to stay on-route. If that jumps us into the Modron reset that's a route setup issue (although perhaps we should check for it)
	KillCurrentBoss( maxLoopTime := 25000 )
    {
        CurrentZone := this.Memory.ReadCurrentZone()
        if mod( CurrentZone, 5 )
            return 1
        StartTime := A_TickCount
        ElapsedTime := 0
        counter := 0
        sleepTime := 67
        g_SharedData.LoopString := "Killing boss before stacking."
        while ( !mod( this.Memory.ReadCurrentZone(), 5 ) AND ElapsedTime < maxLoopTime )
        {
            ElapsedTime := A_TickCount - StartTime
            g_BrivGemFarm.routeMaster.SetFormation()
            if(!this.Memory.ReadQuestRemaining()) ; Quest complete, still on boss zone. Skip boss bag.
                g_BrivGemFarm.RouteMaster.ToggleAutoProgress(1,0,false)
            Sleep, %sleepTime%
        }
        if(ElapsedTime >= maxLoopTime)
            return 0
        this.WaitForTransition()
        return 1
    }

	;Override to make it MASH KEYS FASTER, in an attempt to avoid fallbacks more reliably
	RecoverFromGameClose(formationFavouriteKey:="Q") ;Normally called from g_SF.SafetyCheck() with g_SF.GameStartFormation as parameter
    {
        StartTime := A_TickCount
        ElapsedTime := 0
        timeout := 10000
        if(this.Memory.ReadCurrentZone() == 1)
			return
        spam := "{" . formationFavouriteKey  . "}" ;TODO: Move to InputManager
        formationFavorite := g_BrivGemFarm.levelManager.GetFormation(formationFavouriteKey)
        ElapsedTime := 0
		isCurrentFormation:=this.IsCurrentFormation(formationFavorite)
        while(!isCurrentFormation AND ElapsedTime < timeout AND !this.Memory.ReadNumAttackingMonstersReached())
        {
			this.DirectedInput(,, spam )
            Sleep IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST ;Using _FAST not _INPUT as we do want to mash this to get it in before an enemy spawns
			isCurrentFormation:=this.IsCurrentFormation(formationFavorite)
			ElapsedTime := A_TickCount - StartTime
        }
        while(!isCurrentFormation AND (this.Memory.ReadNumAttackingMonstersReached() OR this.Memory.ReadNumRangedAttackingMonsters()) AND (ElapsedTime < (2 * timeout)))
        {
            ElapsedTime := A_TickCount - StartTime
            this.FallBackFromZone()
            this.DirectedInput(,, spam )
            g_BrivGemFarm.RouteMaster.ToggleAutoProgress(1, true)
            isCurrentFormation := this.IsCurrentFormation(formationFavorite)
        }
		Critical Off ;Turned On previously via WaitForGameReady() calling WaitForFinalStatUpdates()
        g_SharedData.LoopString := "Loading game finished."
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
		targetTime:=g_BrivUserSettingsFromAddons["IBM_OffLine_Delay_Time"] ;Amount of time we'd like to elapse before passing platform login
		if (g_SF.Memory.IBM_ReadIsGameUserLoaded()!=1 AND (A_TickCount - g_BrivGemFarm.routeMaster.offlineSaveTime < targetTime))
		{
			g_SharedData.LoopString := "Waiting for platform login..."
			;g_BrivGemFarm.routeMaster.DebugTick("IBM_WaitForUserLogin() wait for platform login")
			ElapsedTime:=A_TickCount - g_BrivGemFarm.routeMaster.offlineSaveTime
			Critical On ;We need to catch the platform login completing before the game progresses to the userdata request
			While (g_SF.Memory.IBM_ReadIsGameUserLoaded()!=1 AND ElapsedTime < targetTime) ;Wait for user loaded or we run out of time, then stop IC
			{
				Sleep 0 ;Need to be fast to catch this
				ElapsedTime:=A_TickCount - g_BrivGemFarm.routeMaster.offlineSaveTime
			}
			Critical Off
			;g_BrivGemFarm.routeMaster.DebugTick("IBM_WaitForUserLogin() platform login done - suspending process")
			ElapsedTime:=A_TickCount - g_BrivGemFarm.routeMaster.offlineSaveTime
			if (ElapsedTime >= targetTime) ;Don't suspend if we ran out of time waiting
			{
				;g_BrivGemFarm.routeMaster.DebugTick("IBM_WaitForUserLogin() time ran out whilst waiting for user load")
				return
			}
			this.IBM_SuspendProcess(g_SF.PID,True) 
			;g_BrivGemFarm.routeMaster.DebugTick("IBM_WaitForUserLogin() suspended process - waiting for target time")
			ElapsedTime:=A_TickCount - g_BrivGemFarm.routeMaster.offlineSaveTime
			While (ElapsedTime < targetTime)
			{
				;g_BrivGemFarm.routeMaster.DebugTick("IBM_WaitForUserLogin() waiting - elapsed:" . ElapsedTime . " target:" . targetTime)
				Sleep IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
				ElapsedTime:=A_TickCount - g_BrivGemFarm.routeMaster.offlineSaveTime
			}
			;g_BrivGemFarm.routeMaster.DebugTick("IBM_WaitForUserLogin() reactivating process")
			this.IBM_SuspendProcess(g_SF.PID,False)
		}
		else
		{
			;g_BrivGemFarm.routeMaster.DebugTick("IBM_WaitForUserLogin() not waiting for platform login")
		}
	}
	
	;Overridden to better order the sleeps vs the checks
	; Waits for the game to be in a ready state
    WaitForGameReady( timeout := 90000)
    {
        ;g_BrivGemFarm.routeMaster.DebugTick("WaitForGameReady() start")
		if (!g_BrivGemFarm.routeMaster.HybridBlankOffline AND g_BrivGemFarm.routeMaster.offlineSaveTime>=0) ;If this is set by stack restart
			this.IBM_WaitForUserLogin()
		timeoutTimerStart := A_TickCount
        ElapsedTime := 0	
        ; wait for game to start
        g_SharedData.LoopString := "Waiting for game started..."
        gameStarted := 0
		lastInput:=-250 ;Input limiter for the escape key presses
		while( ElapsedTime < timeout AND !gameStarted)
        {	
            if (A_TickCount > lastInput+250 AND this.Memory.IBM_IsSplashVideoActive())
			{
				g_BrivGemFarm.KEY_ESC.KeyPress()
				lastInput:=A_TickCount
				sleep 15 ;Short sleep as we've spent time on input already
			}
			else ;Longer sleep if not sending input
				Sleep 45
			gameStarted := this.Memory.ReadGameStarted()
            ElapsedTime := A_TickCount - timeoutTimerStart
        }
        ; check if game has offline progress to calculate
        offlineTime := this.Memory.ReadOfflineTime()
		if(gameStarted AND offlineTime <= 0 AND offlineTime != "")
        {
			return true ; No offline progress to calculate, game started
		}
        ; wait for offline progress to finish
        g_SharedData.LoopString := "Waiting for offline progress..."
        offlineDone := 0
		while( ElapsedTime < timeout AND !offlineDone)
        {
            Sleep 100
            offlineDone := this.Memory.ReadOfflineDone()
			ElapsedTime := A_TickCount - timeoutTimerStart
        }
        ; finished before timeout
        if(offlineDone)
        {
			this.WaitForFinalStatUpdates(this.GameStartFormation)
			g_PreviousZoneStartTime := A_TickCount
            return true
        }
        this.CloseIC( "WaitForGameReady-Failed to finish in " . Floor(timeout/ 1000) . "s." )
        return false
    }
	
	;Override to send formation switch
	; Waits until stats are finished updating from offline progress calculations.
    WaitForFinalStatUpdates(startFormationKey:="q")
    {
		;g_BrivGemFarm.routeMaster.DebugTick("WaitForFinalStatUpdates() start")
		g_SharedData.LoopString := "Waiting for offline progress (Area Active)..."
        ElapsedTime := 0
        ; Starts as 1, turns to 0, back to 1 when active again.
        StartTime := A_TickCount
        while(this.Memory.ReadAreaActive() AND ElapsedTime < 5000) ;This was 1736ms, which it seems can be exceeded causing things to go wierd, better to wait here a little longer
        {
            ElapsedTime := A_TickCount - StartTime
            Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
        }
		;g_BrivGemFarm.routeMaster.DebugTick("WaitForFinalStatUpdates() Area Active")
		formationActive:=False
        KEY:=g_BrivGemFarm.inputManager.getKey(startFormationKey)
		Critical On ;From here to the zone becoming active timing is important to maximise our chances of getting to the proper formation before something spawns and blocks us. This is not turned off by this function intentionally
		while(!this.Memory.ReadAreaActive() AND ElapsedTime < 7000) ;2000ms beyond the initial loop
        {
            if (!formationActive)
			{
				Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST ;Only sleep whilst the formation is inactive, we want to react as fast as possible once the area is active
				if (!this.Memory.IBM_IsCurrentFormationEmpty()) ;IRISIRI - Once champions start being placed we will try sending input. Was trying to make this mode responsive once the zone becomes available but that seems too early to be useful
				{
					formationActive:=True
				}
			}
			ElapsedTime := A_TickCount - StartTime
        }
		KEY.KeyPress()
    }

	;Override to use sleep, not sure why this spins the wheels in loops like this, but the base script does it a LOT
	FallBackFromZone(maxLoopTime:=5000)
    {
        fellBack:=0
        StartTime:=A_TickCount
        ElapsedTime:=0
        while(this.Memory.ReadCurrentZone() == -1 AND ElapsedTime < maxLoopTime)
        {
            CurrentZone := this.Memory.ReadCurrentZone()
			Sleep IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
			ElapsedTime := A_TickCount - StartTime
        }
        CurrentZone := this.Memory.ReadCurrentZone()
        StartTime := A_TickCount
        ElapsedTime:=0
        g_SharedData.LoopString := "Falling back from zone.."
        while(!this.Memory.ReadTransitioning() AND ElapsedTime < maxLoopTime)
        {
            this.DirectedInput(,, "{Left}" )
			Sleep IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_INPUT ;INPUT for this one as we don't want to go back multiple zones
			ElapsedTime := A_TickCount - StartTime
        }
        this.WaitForTransition()
        ;ElapsedTime := A_TickCount - StartTime
        fellBack := 1 ;Irisiri - this makes the return meaningless, and doesn't match the description above. Needs to check if it timed out at each stage?
        return fellBack
    }
	
	;Override to use IBM levelling and progress management
	; Wait for Thellora to activate her Rush ability.
    DoRushWait(stopProgress:=false) ;Note: unknown what IBM_ThelloraTriggered returns if she starts with 0 stacks or we have 0 favour (with the former being the case that might matter)
    {
		;OutputDebug % A_TickCount ":DoRushWait() - start`n"
        StartTime := A_TickCount
        ElapsedTime := 0
		levelTypeChampions:=true ;Alternate levelling types to cover both without taking too long in each loop
		g_SharedData.LoopString := "Rush Wait"
		while (!(this.Memory.ReadCurrentZone() > 1 OR this.Memory.IBM_ThelloraTriggered()) AND ElapsedTime < 8000)
        {
			;OutputDebug % A_TickCount ":DoRushWait() - loop`n"
			if (stopProgress) ;If we are doing Elly's casino after the rush we need to stop ASAP so that 1 kill (probably via Melf) doesn't jump us an extra time, possibly on the wrong formation
			{
				if (this.Memory.ReadHighestZone() > 1)
				{
					g_BrivGemFarm.RouteMaster.ToggleAutoProgress(0)
					stopProgress:=false ;No need to keep checking, and allows for levelling
				}
			}
			if (levelTypeChampions)
				g_BrivGemFarm.levelManager.LevelWorklist() ;Level current worklist
			else
				g_BrivGemFarm.levelManager.LevelClickDamage(0) ;Level click damage
            levelTypeChampions:=!levelTypeChampions
			ElapsedTime := A_TickCount - StartTime
        }
        g_PreviousZoneStartTime := A_TickCount
		;OutputDebug % A_TickCount ":DoRushWait() - end`n"
    }
	
	;Overriding to:
	;1) launch with higher process priority (note that realtime requires things to be run as admin)
	;2) lower the timeout on opening the game
	;3) Address the loop Sleep applying after a sucessful load
	; Attemps to open IC. Game should be closed before running this function or multiple copies could open
    OpenIC(message:="")
    {
		waitForReadyTimeout:=45000 ;TODO: Make this a setting - varies by system and platform (Steam tends to be faster than EGS)
		timeoutVal := 32000 + waitForReadyTimeout
        loadingDone := false
        g_SharedData.LoopString := "Starting Game" . (message ? " " . message : "")
		g_BrivGemFarm.Logger.AddMessage("Starting Game" . (message ? " " . message : ""))
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
				Process, Priority, % this.PID, Realtime ;Irisiri
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
				Sleep, 62 ;Moved this to an Else, otherwise it delays code progression when loading is sucessful. This is also oddly specific
            ElapsedTime := A_TickCount - StartTime
        }
        if(ElapsedTime >= timeoutVal)
        {
			Critical Off ;Potential edge case where loadingZone was set to true but we ran out time whilst exiting the loop
			return -1 ; took too long to open
		}
        else
        {
			g_BrivGemFarm.routeMaster.ResetCycleCount() ;Whatever the reason, we've gone offline and therefore don't need to restart the game again
			g_BrivGemFarm.DialogSwatter_Start()
			return 0
		}
    }
	
	;Override to fix the typo in the name, and to use the Hwnd instead of window name
	;Saves this.SavedActiveWindow as the last window and waits for the game exe to load its window.
    SetLastActiveWindowWhileWaitingForGameExe(timeoutLeft := 32000)
    {
        StartTime := A_TickCount
        ; Process exists, wait for the window:
        while(!(this.Hwnd := WinExist( "ahk_exe " . g_userSettings[ "ExeName"] )) AND ElapsedTime < timeoutLeft)
        {
            WinGet, savedActive,, A ;Changed to the handle, multiple windows could have the same name
            this.SavedActiveWindow := savedActive
            ElapsedTime := A_TickCount - StartTime
            Sleep, 62
        }
    }
	
	;Override to use IBM option
	ActivateLastWindow()
    {
        if (!g_SharedData.IBM_RestoreWindow_Enabled)
            return
        Sleep, 100 ; extra wait for window to load
        hwnd := this.Hwnd
        WinActivate, ahk_id %hwnd% ; Idle Champions likes to be activated before it can be deactivated Irisiri: Testing if this is true
        savedActive:="ahk_id " . this.SavedActiveWindow
		WinActivate, %savedActive%
    }
	
	;Overridden to handle Claim Daily Platinum - done here for blank restarts as we don't want to delay restarting the game - TODO: Except I moved Daily Platinum to the hub...
	; Runs the process and set this.PID once it is found running. 
    OpenProcessAndSetPID(timeoutLeft := 32000)
    {
        this.PID := 0
        processWaitingTimeout := 10000 ;10s
        ElapsedTime := 0
        StartTime := A_TickCount
        while (!this.PID AND ElapsedTime < timeoutLeft )
        {
            g_SharedData.LoopString := "Opening IC.."
            programLoc := g_UserSettings[ "InstallPath" ]
            try
            {
                Run, %programLoc% ;TODO: Take the PID from this if the EXE matches the game one; no need for the loop. Consider if later timers might be impacted, however
            }
            catch
            {
                MsgBox, 48, Unable to launch game, `nVerify the game location is set properly by enabling the Game Location Settings addon, clicking Change Game Location on the Briv Gem Farm tab, and ensuring the launch command is set properly.
                ExitApp
            }
			Sleep, 15
            ; Add 10s (default) to ElapsedTime so each exe waiting loop will take at least 10s before trying to run a new instance of hte game
            timeoutForPID := ElapsedTime + processWaitingTimeout 
            while(!this.PID AND ElapsedTime < timeoutForPID AND ElapsedTime < timeoutLeft)
            {
                existingProcessID := g_userSettings[ "ExeName"]
                Process, Exist, %existingProcessID%
                this.PID := ErrorLevel
                Sleep, 50
                ElapsedTime := A_TickCount - StartTime
            }
            ElapsedTime := A_TickCount - StartTime
            Sleep, 50
        }
    }
	
	;Overridden to reduce check loop sleep time
	;A function that closes IC. If IC takes longer than 60 seconds to save and close then the script will force it closed.
    CloseIC( string := "",usePID:=false)
    {
		g_SharedData.LastCloseReason := string
        ; check that server call object is updated before closing IC in case any server calls need to be made
        ; by the script before the game restarts
        this.ResetServerCall()
        if ( string != "" )
            string := ": " . string
        g_SharedData.LoopString := "Closing IC" . string
        if (usePID)
			sendMessageString := "ahk_pid " . this.PID ;TODO: When using PID we need to fall back to closing by exe name at some point, which will obviously ruin a relay, but it's possible we end up with the game and this.PID not being aligned
		else
			sendMessageString := "ahk_exe " . g_userSettings[ "ExeName"]
		if WinExist(sendMessageString)
            SendMessage, 0x112, 0xF060,,, %sendMessageString%,,,, 10000 ; WinClose
		StartTime := A_TickCount
		saveCompleteTime := -1 ;Unset
		while ( WinExist( sendMessageString) AND A_TickCount - StartTime < 5000 )
        {
            Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST ;Reduced from 200ms, we want to get into chest opening and the sleep timer with maximum consistency
			if (saveCompleteTime==-1 AND !g_SF.Memory.IBM_ReadIsInstanceDirty())
			{
				saveCompleteTime :=A_TickCount
				g_BrivGemFarm.routeMaster.CheckRelayRelease()
			}
        }
        StartTime := A_TickCount
		while ( WinExist( sendMessageString ) AND A_TickCount - StartTime < 5000 ) ; Kill after 5 seconds.
        {
			if (saveCompleteTime==-1 AND !g_SF.Memory.IBM_ReadIsInstanceDirty())
			{
				saveCompleteTime :=A_TickCount
				g_BrivGemFarm.routeMaster.CheckRelayRelease()
			}
			g_BrivGemFarm.Logger.AddMessage("IC failed to close cleanly: sending WinKill")
			WinKill, sendMessageString
			sleep 200 ;Let WinKill do its thing
		}
		if WinExist( sendMessageString )
		{
			hProcess := DllCall("Kernel32.dll\OpenProcess", "UInt", 0x0001, "Int", false, "UInt", g_SF.PID, "Ptr")
			if(hProcess)
			{
				DllCall("Kernel32.dll\TerminateProcess", "Ptr", hProcess, "UInt", 0)
				DllCall("Kernel32.dll\CloseHandle", "Ptr", hProcess)
				g_BrivGemFarm.Logger.AddMessage("IC failed to close cleanly: sending TerminateProcess")
			} else
				g_BrivGemFarm.Logger.AddMessage("IC failed to close cleanly: failed to get process handle for TerminateProcess")
		}
		if (saveCompleteTime==-1) ;Failed to detect, going to have to go with current time
		{
			saveCompleteTime:=A_TickCount
		}
        return saveCompleteTime
    }

	/*
	IBM_DeepClone(obj) ;Clone an object, including its first level children. Was used by the levelManager
	{
		nobj := obj.Clone()
		for k,v in nobj
			if IsObject(v)
				nobj[k] := this.IBM_DeepClone(v)
		return nobj
	}
	*/

	InjectAddon()
    {
        splitStr := StrSplit(A_LineFile, "\")
        addonDirLoc := splitStr[(splitStr.Count()-1)]
        addonLoc := "#include *i %A_LineFile%\..\..\" . addonDirLoc . "\IC_BrivMaster_Addon.ahk`n"
        FileAppend, %addonLoc%, %g_BrivFarmModLoc%
    }

	IBM_ConvertBinaryArrayToBase64(value) ;Converts an array of 0/1 values to base 64. Note this is NOT proper base64url as we've no interest in making it byte compatible
	{
		charIndex:=1
		chars:=[]
		;OutputDebug % value.Length() . "`n"
		loop, % value.Length()
		{
			if (!chars.HasKey(charIndex))
				chars[charIndex]:=[]
			chars[charIndex].Push(value[A_Index])
			;OutputDebug % "Loop:" . charIndex . " " . chars[charIndex].Length() . "`n"
			if (chars[charIndex].Length()==6)
				charIndex++
		}
		while (chars[charIndex].Length() < 6) ;Pad the last character to 6 bits, otherwise 11 would convert to dec 3, as would 000011
			chars[charIndex].Push(0)
		accu:=""
		loop, % chars.Length()
		{
			accu.=SubStr(IC_BrivMaster_SharedFunctions_Class.BASE_64_CHARACTERS,this.IBM_BinaryArrayToDec(chars[A_INDEX])+1,1) ;1 for 1-index array
		}
		return accu
	}

	IBM_BinaryArrayToDec(value)
	{
		charPos:=0
		accu:=0
		while value.Length() >= 1
		{
			accu+=value.Pop()*(2**charPos)
			charPos++
		}
		return accu
	}

	IBM_ConvertBase64ToBinaryArray(value) ;Converts a base-64 value to a binary array, limited to the specified size Note this is NOT proper base64url as we've no interest in making it byte compatible. The result will always be a multiple of 6 bits TODO: Should we allow a size limit here (eg IBM_ConvertBase64ToBinaryArray(value,maxsize) )
	{
		length:=StrLen(value)
		accu:=[]
		loop, parse, value
		{
			base:=(InStr(IC_BrivMaster_SharedFunctions_Class.BASE_64_CHARACTERS,A_LoopField,true)-1) ;InStr must be set to case-sensitive
			accu.Push((base & 0x20)>0,(base & 0x10)>0,(base & 0x08)>0,(base & 0x04)>0,(base & 0x02)>0,(base & 0x01)>0)
		}
		return accu
	}
	
	IBM_CNETimeStampToDate(timeStamp) ;Takes a timestamp in seconds-since-day-0 format and converts it to a date for AHK use
	{
		unixTime:=timeStamp-62135596800 ;Difference between day 1 (01Jan0001) and unix time (AHK doesn't support dates before 1601 so we can't just set converted:=1)
		converted:=1970
		converted+=unixTime,s
		return converted
	}
}