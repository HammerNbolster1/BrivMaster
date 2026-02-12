#include %A_LineFile%\..\..\..\SharedFunctions\MemoryRead\SH__MemoryManager.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\MemoryRead\SH_MemoryPointer.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\MemoryRead\SH_StaticMemoryPointer.ahk

class IC_BrivMaster_EngineSettings_Class extends SH_StaticMemoryPointer ;EngineSettings class contains IC's EngineSettings class structure. Useful for finding webroot for doing server calls.
{
    Refresh()
    {
        if (_MemoryManager.is64bit=="") ;Don't build offsets if no client is available to check variable types.
            return
        baseAddress:=_MemoryManager.baseAddress["mono-2.0-bdwgc.dll"]+this.ModuleOffset
        if (this.BasePtr.BaseAddress!=baseAddress)
        {
            this.BasePtr.BaseAddress:=baseAddress
            this.Is64Bit:=_MemoryManager.is64bit
            if (this.UnityGameEngine=="")
            {
                this.UnityGameEngine:={}
                this.UnityGameEngine.Core:={}
                this.UnityGameEngine.Core.EngineSettings:=new GameObjectStructure(this.StructureOffsets)
                this.UnityGameEngine.Core.EngineSettings.BasePtr:=new SH_BasePtr(this.BasePtr.BaseAddress, this.ModuleOffset, this.StructureOffsets)
                this.UnityGameEngine.Core.EngineSettings.Is64Bit:=_MemoryManager.is64Bit
                #include *i %A_LineFile%\..\Offsets\IC_EngineSettings_Import.ahk
                return
            }
            this.UnityGameEngine.Core.EngineSettings.BasePtr:=new SH_BasePtr(this.BasePtr.BaseAddress, this.ModuleOffset, this.StructureOffsets, "EngineSettings")
            this.ResetBasePtr(this.UnityGameEngine.Core.EngineSettings)
        }
    }
}

class IC_BrivMaster_GameSettings_Class extends SH_StaticMemoryPointer ;GameSettings class contains IC's GameSettings class structure. Useful for finding details for doing server calls
{
    Refresh()
    {
        if (_MemoryManager.is64bit == "") ;Don't build offsets if no client is available to check variable types.
            return
        baseAddress:=_MemoryManager.baseAddress["mono-2.0-bdwgc.dll"]+this.ModuleOffset
        if (this.BasePtr.BaseAddress!=baseAddress)
        {
            this.BasePtr.BaseAddress:=baseAddress
            this.Is64Bit:=_MemoryManager.is64bit
            if (this.CrusadersGame=="")
            {
                this.CrusadersGame:={}
                this.CrusadersGame.GameSettings:=new GameObjectStructure(this.StructureOffsets)
                this.CrusadersGame.GameSettings.BasePtr:=new SH_BasePtr(this.BasePtr.BaseAddress, this.ModuleOffset, this.StructureOffsets)
                this.CrusadersGame.GameSettings.Is64Bit:=_MemoryManager.is64Bit
                #include *i %A_LineFile%\..\Offsets\IC_GameSettings_Import.ahk
                return
            }
            this.CrusadersGame.GameSettings.BasePtr := new SH_BasePtr(this.BasePtr.BaseAddress, this.ModuleOffset, this.StructureOffsets, "GameSettings")
            this.ResetBasePtr(this.CrusadersGame.GameSettings)
        }
    }
}

class IC_BrivMaster_IdleGameManager_Class extends SH_MemoryPointer ;GameManager class contains the in game data structure layout
{
    Refresh()
    {
        baseAddress:=_MemoryManager.baseAddress["mono-2.0-bdwgc.dll"]+this.ModuleOffset
        if (_MemoryManager.is64bit == "") ;Don't build offsets if no client is available to check variable types. TODO: This is really being used as a 'is attached to process' flag, which works because wer'e only using 64 bit, but should probably be it's own thing - see 64bit purge
            return
        if (this.BasePtr.BaseAddress!=baseAddress)
        {
            this.BasePtr.BaseAddress:=baseAddress
            this.Is64Bit:=_MemoryManager.is64bit
            ; Note: Using example Offsets 0xCB0,0 from CE, 0 is a mod (+) and disappears leaving just 0xCB0
            ; this.StructureOffsets[1] += 0x10
            if (this.IdleGameManager=="")
            {
                this.IdleGameManager:=New GameObjectStructure(this.StructureOffsets)
                this.IdleGameManager.BasePtr:=new SH_BasePtr(this.BasePtr.BaseAddress, this.ModuleOffset, this.StructureOffsets, "IdleGameManager")
                this.IdleGameManager.Is64Bit:=_MemoryManager.is64bit
                #include *i %A_LineFile%\..\Offsets\IC_IdleGameManager_Import.ahk ;Build offsets for class using imported AHK files.
                ; DEBUG: Enable this line to be able to view the variable name of the GameObject. (e.g. this.game would have a GSOName variable that says "game" )
                ; this.game.SetNames()
                return
            }
            ; Objects exist, update memory addresses only
            ; Note: Once imports have been built, IdleGameManager is no longer used for GameObjects. Structure builds from this -> this.game, NOT this.IdleGameManager.game
            this.IdleGameManager.BasePtr:=new SH_BasePtr(this.BasePtr.BaseAddress, this.ModuleOffset, this.StructureOffsets)
            this.ResetBasePtr(this.IdleGameManager)
        }
    }
}

class IC_BrivMaster_MemoryFunctions_Class
{
	__new(fileLoc:="IC_Offsets.json")
	{
        FileRead, offsetData, %fileLoc%
        if(offsetData=="")
        {
            MsgBox 16, Briv Master, % "Pointer data not found. Please review the BM Game tab of the settings."
            if(ObjGetBase(g_IBM).__Class:="IC_BrivMaster_GemFarm_Class") ;Exit from the gem farm, but not the hub - or we won't be able to select any pointers!
				ExitApp
        }
        currentPointers:=AHK_JSON.Load(offsetData)
		this.Versions:={} ;All the verison information is stored in the pointer JSON file, so load
		this.Versions.Import_Revision:=currentPointers["Import_Revision"]
		this.Versions.Import_Version_Major:=currentPointers["Import_Version_Major"]
		this.Versions.Import_Version_Minor:=currentPointers["Import_Version_Minor"]
		this.Versions.Platform:=currentPointers["Platform"]
		this.Versions.Pointer_Revision:=currentPointers["Pointer_Revision"]
		this.Versions.Pointer_Version_Major:=currentPointers["Pointer_Version_Major"]
		this.Versions.Pointer_Version_Minor:=currentPointers["Pointer_Version_Minor"]
        _MemoryManager.exeName:=g_IBM_Settings["IBM_Game_Exe"]
        _MemoryManager.Refresh()
        this.Is64bit:=_MemoryManager.Is64Bit ;TODO: We need to remove 32 bit support in general
        this.GameManager:=new IC_BrivMaster_IdleGameManager_Class(currentPointers.IdleGameManager.moduleAddress, currentPointers.IdleGameManager.moduleOffset)
        this.GameSettings:=new IC_BrivMaster_GameSettings_Class(currentPointers.GameSettings.moduleAddress, currentPointers.GameSettings.staticOffset, currentPointers.GameSettings.moduleOffset)
        this.EngineSettings:=new IC_BrivMaster_EngineSettings_Class(currentPointers.EngineSettings.moduleAddress, currentPointers.EngineSettings.staticOffset, currentPointers.EngineSettings.moduleOffset)
		this.FavoriteFormations:={} ;Irisiri - used for formation caching by the looks of it
		this.LastFormationSavesVersion:={} ;Irisiri- used for formation caching by the looks of it
		this.SlotFormations:={} ;Irisiri - used for formation caching by the looks of it
    }
	
	OpenProcessReader(pid:="") ;If supplied with a PID will have the memory manager load that instead of using the window, via IBM override
    {
        _MemoryManager.exeName:=g_IBM_Settings["IBM_Game_Exe"]
        isExeRead:=_MemoryManager.Refresh(,pid)
        if(isExeRead==-1)
            return
        if(_MemoryManager.handle=="")
            MsgBox, , , Could not read from exe. Try running as Admin. , 7
        this.Is64Bit:=_MemoryManager.is64Bit
		this.GameManager.Refresh()
        this.GameSettings.Refresh()
        this.EngineSettings.Refresh()
    }

 	GetImportsVersion()
	{
        return this.Versions.Import_Version_Major . this.Versions.Import_Version_Minor . " " . this.Versions.Import_Revision ;'639 A', '639.1 B'
    }

	ReadBaseGameVersion()
	{
        return this.GameSettings.MobileClientVersion.Read()
    }

	ReadGameStarted()
	{
        return this.GameManager.game.gameStarted.Read()
    }

	ReadResetting()
	{
        return this.GameManager.game.gameInstances[0].ResetHandler.Resetting.Read()
    }

	ReadTransitioning()
	{
        return this.GameManager.game.gameInstances[0].Controller.areaTransitioner.IsTransitioning_k__BackingField.Read()
    }

    ReadTransitionDirection() ;0 = static (instant), 1 = right, 2 = left, 3 = JumpDown, 4 = FallDown (new)
	{
        return this.GameManager.game.gameInstances[0].Controller.areaTransitioner.transitionDirection.Read()
    }

    ReadFormationTransitionDir() ;0 = OnFromLeft, 1 = OnFromRight, 2 = OnFromTop, 3 = OffToLeft, 4 = OffToRight, 5 = OffToBottom (new)
	{
        return this.GameManager.game.gameInstances[0].Controller.formation.transitionDir.Read()
    }

	ReadAreaActive()
	{
        return this.GameManager.game.gameInstances[0].Controller.area.Active.Read()
    }

	ReadUserIsInited()
	{
        return this.GameManager.game.gameInstances[0].Controller.userData.inited.Read()
    }

	ReadIsSplashVideoActive()
	{
        return this.GameManager.game.loadingScreen.SplashScreen.IsActive_k__BackingField.Read()
    }

	ReadClickLevel()
	{
        return this.GameManager.game.gameInstances[0].ClickLevel.Read()
    }

    ReadUserID()
	{
        ; return this.GameManager.game.gameUser.ID.Read() ; alternative, not in imports currently
        return this.GameSettings.UserID.Read()
    }

    ReadUserHash()
	{
        ; return this.GameManager.game.gameUser.Hash.Read() ; Alternative, not in imports currently
        return this.GameSettings.Hash.Read()
    }

    ReadInstanceID()
	{
        return this.GameSettings._instance.instanceID.Read()
    }

	ReadWebRoot()
	{
        return this.EngineSettings.WebRoot.Read()
    }

    ReadPlatform()
	{
        return this.GameSettings.Platform.Read()
    }

	ReadGems()
	{
        return this.GameManager.game.gameInstances[0].Controller.userData.redRubies.Read()
    }

	ReadCurrentObjID()
	{
        return this.GameManager.game.gameInstances[0].ActiveCampaignData.currentObjective.ID.Read()
    }

	ReadQuestRemaining()
	{
        return this.GameManager.game.gameInstances[0].ActiveCampaignData.currentArea.QuestRemaining.Read()
    }

	ReadCurrentZone()
	{
        return this.GameManager.game.gameInstances[0].ActiveCampaignData.currentAreaID.Read()
    }

    ReadHighestZone()
	{
        return this.GameManager.game.gameInstances[0].ActiveCampaignData.highestAvailableAreaID.Read()
    }

	ReadActiveGameInstance() ;TODO: Appears to duplicate IBM_GetActiveGameInstanceID via a different import, both are used currently
	{
        return this.GameManager.game.gameInstances[0].Controller.userData.ActiveUserGameInstance.Read()
    }


    GetActiveModronFormation() ;Returns the formation array of the formation used in the currently active modron.
	{
        formation:=""
        formationSaveSlot:=this.GetActiveModronFormationSaveSlot()
        if(formationSaveSlot >= 0)
            formation := this.GetFormationSaveBySlot(formationSaveSlot) ;Get the formation using the index (slot)
        return formation
    }

	GetActiveModronFormationSaveSlot()
	{
        favorite:="M" ; (M)odron
        version:= this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2.__version.Read()
        if(this.FavoriteFormations[favorite]!="" AND version==this.LastFormationSavesVersion[favorite])
            return this.FavoriteFormations[favorite]
        ; Find the Campaign ID (e.g. 1 is Sword Cost, 2 is Tomb, 1400001 is Sword Coast with Zariel Patron, etc.)
        ; Find the SaveID associated to the Campaign ID
        ; Find the index (slot) of the formation with the correct SaveID
        formationSaveID:=this.GetModronFormationsSaveIDByFormationCampaignID(this.ReadFormationCampaignID())
        formationSavesSize:=this.ReadFormationSavesSize()
        if(formationSavesSize<=0 OR formationSavesSize>500) ; sanity check, should be < 51 saves per map.
            return ""
        formationSaveSlot := -1
        loop, %formationSavesSize%
        {
            if (this.ReadFormationSaveIDBySlot(A_Index - 1) == formationSaveID)
            {
                formationSaveSlot := A_Index - 1
                Break
            }
        }
        return formationSaveSlot
    }

    GetModronFormationsSaveIDByFormationCampaignID(formationCampaignID) ;Uses FormationCampaignID to search the modron for the SaveID of the formation the active modron is using
	{
        modronSavesSlot:=this.GetCurrentModronSaveSlot() ;Find which modron core is being used
        return this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves[modronSavesSlot].FormationSaves[formationCampaignID].Read() ;Find SaveID for given formationCampaignID
    }

    GetCurrentModronSaveSlot() ;Finds the index of the current modron in ModronHandlers
	{
        activeGameInstance:=this.ReadActiveGameInstance()
        modronSavesSize:=this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves.size.Read()
        if(modronSavesSize <= 0 OR modronSavesSize > 20) ; sanity check, should be < 5 as of 2023-09-03
            return ""
        loop, %modronSavesSize%
            if (this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves[A_Index - 1].InstanceID.Read()==activeGameInstance)
                return A_Index - 1
    }

    GetModronResetArea() ;Finds the Modron Reset area for the current instance's core
	{
        return this.GetCoreTargetAreaByInstance(this.ReadActiveGameInstance())
    }

	GetCoreTargetAreaByInstance(InstanceID:=1)
	{
        saveSize:=this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves.size.Read() ;reads memory for the number of cores
        if(saveSize <= 0 OR saveSize > 50000) ; sanity check, should be a positive integer and less than 2005 as that is max allowed area as of 2023-09-03 Irisiri - unclear why the reset zone would be relevant here, number of cores possibly?
            return ""
        loop, %saveSize%  ;cycle through saved formations to find save slot of Favorite
            if (this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves[A_Index - 1].InstanceID.Read()==InstanceID)
                return this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves[A_Index - 1].targetArea.Read()
        return -1
    }

	ReadModronAutoFormation()
	{
        return this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves[this.GetCurrentModronSaveSlot()].TogglePreferences[0].Read()
    }

	ReadModronAutoReset()
	{
        return this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves[this.GetCurrentModronSaveSlot()].TogglePreferences[1].Read()
    }

	ReadModronAutoBuffs()
	{
        return this.GameManager.game.gameInstances[0].Controller.userData.ModronHandler.modronSaves[this.GetCurrentModronSaveSlot()].TogglePreferences[2].Read()
    }

	ReadNumAttackingMonstersReached()
	{
        return this.GameManager.game.gameInstances[0].Controller.formation.numAttackingMonstersReached.Read()
    }

	ReadNumRangedAttackingMonsters()
	{
        return this.GameManager.game.gameInstances[0].Controller.formation.numRangedAttackingMonsters.Read()
    }
	
	ReadActiveMonstersCount()
	{
        return this.GameManager.game.gameInstances[0].Controller.area.activeMonsters.size.Read()
    }

    ReadFormationCampaignID() ;Reads the FormationCampaignID for the FormationSaves index passed in
	{
        return this.GameManager.game.gameInstances[0].FormationSaveHandler.FormationCampaignID.Read()
    }

    ReadFormationSaveIDBySlot(slot:=0) ;Reads the SaveID for the FormationSaves index passed in
	{
        return this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[slot].SaveID.Read()
    }

	ReadOfflineTime()
	{
        return this.GameManager.game.gameInstances[0].OfflineHandler.OfflineTimeRequested_k__BackingField.Read()
    }

	ReadOfflineDone()
	{
        handlerState:=this.GameManager.game.gameInstances[0].OfflineHandler.CurrentState_k__BackingField.Read()
        stopReason:=this.GameManager.game.gameInstances[0].OfflineHandler.CurrentStopReason_k__BackingField.Read()
        return handlerState==0 AND stopReason != "" ; handlerstate is "inactive" and stopReason is not null
    }

	ReadResetsTotal()
	{
        return this.GameManager.game.gameInstances[0].Controller.userData.StatHandler.Resets.Read()
    }

	ReadResetsCount()
	{
        return this.GameManager.game.gameInstances[0].ResetsSinceLastManual.Read()
    }

	ReadAutoProgressToggled()
	{
        return this.GameManager.game.gameInstances[0].Screen.uiController.topBar.objectiveProgressBox.areaBar.autoProgressButton.toggled.Read()
    }

	ReadWelcomeBackActive()
	{
        return this.GameManager.game.gameInstances[0].Screen.uiController.notificationManager.notificationDisplay.welcomeBackNotification.Active.Read()
    }

    GetFormationSaveBySlot(slot := 0, ignoreEmptySlots := 0) ;Read the champions saved in a given formation save slot. returns an array of champ ID with -1 representing an empty formation slot. When parameter ignoreEmptySlots is set to 1 or greater, empty slots (memory read value == -1) will not be added to the array.
	{
        currentVersion:=this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[slot].Formation.__version.Read()
        if(currentVersion != "" AND currentVersion==this.LastFormationSavesVersion["slot" . slot] AND this.SlotFormations["slot" . slot] != "")
        {
            if(!ignoreEmptySlots)
                return this.SlotFormations["slot" . slot].Clone()
            else if (currentVersion != "" AND currentVersion == this.LastFormationSavesVersion["slot" . slot . "1"] AND this.SlotFormations["slot" . slot . "1"] != "")
                return this.SlotFormations["slot" . slot . "1"].Clone()
            Formation:={}
            for indexVal,champID2 in this.SlotFormations["slot" . slot]
                if(champID2 != -1)
                    Formation.Push(champID2)
            return this.SlotFormations["slot" . slot . "1"]:=Formation.Clone()
        }
        Formation := {}
        _size := this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[slot].Formation.size.Read()
        if(_size <= 0 OR _size > 20) ; sanity check
            return ""
        loop, %_size%
        {
            champID := this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[slot].Formation[A_Index - 1].Read()
            if (!ignoreEmptySlots or champID != -1)
                Formation.Push( champID )
        }
        this.LastFormationSavesVersion["slot" . slot] := currentVersion
        this.SlotFormations["slot" . slot] := Formation.Clone()
        return Formation.Clone()
    }

    GetSavedFormationSlotByFavorite(favorite:=1) ;Looks for a saved formation matching a favorite. Returns "" on failure. Favorite, 0 = not a favorite, 1 = save slot 1 (Q), 2 = save slot 2 (W), 3 = save slot 3 (E). O(n) for potentially large list, try to limit use
	{
        formationSavesSize := this.ReadFormationSavesSize() ;Reads memory for the number of saved formations
        if(formationSavesSize <= 0 OR formationSavesSize > 500) ; sanity check, should be less than 51 as of 2023-09-03
            return ""
        formationSaveSlot := ""
        loop, %formationSavesSize% ;cycle through saved formations to find save slot of Favorite
            if (this.ReadFormationFavoriteIDBySlot(A_Index - 1)==favorite)
                return A_Index - 1
        return ""
    }

	ReadMostRecentFormationFavorite() ;Note this is the most recent requested - it DOES update if the formation swap fails, so is not reliable
	{
        return this.GameManager.game.gameInstances[0].FormationSaveHandler.mostRecentFormation.Favorite.Read()
    }

    GetFormationByFavorite(favorite:=0)  ;Returns the formation stored at the favorite value passed in.
	{
        version:= this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2.__version.Read()
        if(this.FavoriteFormations[favorite] != "" AND version == this.LastFormationSavesVersion[favorite])
            return this.FavoriteFormations[favorite]
        slot:=this.GetSavedFormationSlotByFavorite(favorite)
        formation := this.GetFormationSaveBySlot(slot)
        this.FavoriteFormations[favorite] := formation.Clone()
        this.LastFormationSavesVersion[favorite] := version
        return formation
    }


    GetCurrentFormation() ;Returns an array containing the current formation. Note: Slots with no hero are converted from 0 to -1 to match other formation saves
	{
        size := this.GameManager.game.gameInstances[0].Controller.formation.slots.size.Read()
        if(size <= 0 OR size > 14) ; sanity check, 12 is the max number of concurrent champions possible.
            return ""
        formation := Array()
        loop, %size%
        {
            heroID := this.ReadChampIDBySlot(A_Index - 1)
            formation.Push( heroID > 0 ? heroID : -1)
        }
        return formation
    }

	ReadChampIDBySlot(slot := 0)
	{
        return this.GameManager.game.gameInstances[0].Controller.formation.slots[slot].hero.def.ID.Read()
    }


    ReadFormationSavesSize() ;Read the number of saved formations for the active campaign
	{
        return this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2.size.Read()
    }

    ReadFormationFavoriteIDBySlot(slot:=0) ;reads if a formation save is a favorite 0 = not a favorite, 1 = favorite slot 1 (q), 2 = 2 (w), 3 = 3 (e)
	{
        return this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[slot].Favorite.Read()
    }

    ReadChestCountByID(chestID) ;Chests are stored in a dictionary under the "entries". It functions like a 32-Bit list but the ID is every 4th value. Item[0] = ID, item[1] = MAX, Item[2] = ID, Item[3] = count. They are each 4 bytes, not a pointer
	{
        return this.GameManager.game.gameInstances[0].Controller.userData.ChestHandler.chestCounts[chestID].Read()
    }

    ReadPatronID()
	{
        patronIDDef:=this.GameManager.game.gameInstances[0].PatronHandler.ActivePatron_k__BackingField.Read()
        if (patronIDDef==0 OR patronIDDef=="")
            return patronIDDef
        patronID:=this.GameManager.game.gameInstances[0].PatronHandler.ActivePatron_k__BackingField.ID.Read()
        if(patronID < 0 OR patronID > 100) ; Ignore clearly bad memory reads.
            patronID:=""
        return patronID
    }

	HeroHasFeatSavedInFormation(heroID:=58, featID:=2131, formationSlot:=1)
	{
        size:=this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[formationSlot].Feats[heroID].List.size.Read()
        if(size=="")
            return ""
        if(size<=0 OR size>10) ; sanity check
            return false
        Loop, %size%
            if (featID == this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[formationSlot].Feats[heroID].List[A_Index - 1].Read())
                return true
        return false
    }

	HeroHasAnyFeatsSavedInFormation(heroID := 58, formationSlot := 1)
	{
        size:=this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[formationSlot].Feats[heroID].List.size.Read()
        if(size == "")
            return ""
        if(size <= 0 OR size > 10) ; sanity check
            return false
        return true
    }

    GetHeroFeats(heroID)
	{
        if (heroID < 1)
            return ""
        size:=this.GameManager.game.gameInstances[0].Controller.userData.FeatHandler.heroFeatSlots[heroID].List.size.Read()
        if (size < 0 OR size > 10) ;Sanity check, should be < 4 but set to 10 in case of future feat num increase.
            return ""
        featList:=[]
        Loop, %size%
            featList.Push(this.GameManager.game.gameInstances[0].Controller.userData.FeatHandler.heroFeatSlots[heroID].List[A_Index - 1].ID.Read())
        return featList
    }

	IBM_GetWebRootFriendly() ;Handle failures for user-facing reads (mainly the log). WebRoot uses the EngineSettings pointer that moves a lot. TODO: Why is this in memory? Should probably be functions or shared functions
	{
		webRoot:=this.ReadWebRoot()
		if(!webRoot)
			webRoot:="Unable to read WebRoot"
		return webRoot
	}

	IBM_ReadGameVersionMinor() ;If the game is 636.2, return '.2'. This can be, and often is, empty
	{
		return this.GameSettings.VersionPostFix.Read()
    }

	IBM_IsBuffActive(buffName) ;Is a Gem Hunter potion active
	{
		buffSize:=this.GameManager.game.gameInstances[0].BuffHandler.activeBuffs.size.Read()
		if (buffSize < 0 OR size > 1000)
			return false
		loop %buffSize%
		{
			if (this.GameManager.game.gameInstances[0].BuffHandler.activeBuffs[A_Index-1].Name.Read()==buffName) ;TODO: Find out if this gets localised; might need to use the effect name (although that would collide with anything else that gave +50% gems)
				return true
		}
		return false
	}

	IBM_ReadBaseGameSpeed() ;Reads the game speed without the area transition multipier Diana applies, e.g. x10 will flick between x10 and x50 constantly - this will always return x10
	{
		areaTransMulti:=this.GameManager.game.gameInstances[0].areaTransitionTimeScaleMultiplier.Read()
        if (!areaTransMulti)
			areaTransMulti:=1 ;So we don't divide by zero
		return this.GameManager.TimeScale.Read() / areaTransMulti
	}

	IBM_ReadCurrentZoneMonsterHealthExponent() ;Returns 85.90308999 for 8e85 for example
	{
		MEMORY_HEALTH:=g_SF.Memory.GameManager.game.gameInstances[0].ActiveCampaignData.currentArea.Health
		first8:=MEMORY_HEALTH.Read("Int64") ;Quad
        newObject := MEMORY_HEALTH.QuickClone()
        offsetIndex := newObject.FullOffsets.Count()
        newObject.FullOffsets[offsetIndex] := newObject.FullOffsets[offsetIndex] + 0x8
		last8:= newObject.Read("Int64")
		return this.IBM_ConvQuadToExponent(first8,last8)
	}

	IBM_ConvQuadToExponent(FirstEight,SecondEight) ;Converts a quad to an exponent, e.g. 8e85 to 85.90308999. Necessary as AHK can't do Doubles let alone Quads TODO: Should this be in Memory or Shared functions?
    {
        f := log( FirstEight + ( 2.0 ** 63 ) )
        decimated := ( log( 2 ) * SecondEight / log( 10 ) ) + f
        if(decimated <= 4)
            return round((FirstEight + (2.0**63)) * (2.0**SecondEight), 2)
        exponent:=floor(decimated)
        significand:=round( 10 ** (decimated-exponent), 2 )
        return exponent + log(significand)
    }

	IBM_GetCurrentCampaignFavourExponent() ;Process the double directly to avoid AHK limits, or trying to manage it as a string
	{
		static indexCache:=""
		currencyID:=this.GameManager.game.gameInstances[0].ActiveCampaignData.AdventureDef._campaignDef.ResetCurrencyID.Read()
		if (currencyID=="")
			return
		RESET_DEFS:=this.GameManager.game.gameInstances[0].Controller.userData.ResetCurrencyHandler.ResetCurrencyDefs
		if(!indexCache OR RESET_DEFS[indexCache].ID.Read()!=currencyID) ;If there's no cached index, or the cached index no longer points to the right ID
		{
			indexCache:="" ;Reset as invalid
			size:=RESET_DEFS.size.Read()
			if(size<0 OR size>500)
				return
			loop, %size%
			{
				if(RESET_DEFS[A_Index-1].ID.Read()==currencyID)
				{
					indexCache:=A_Index-1
					break
				}
			}
		}
		full8bytes:=RESET_DEFS[indexCache].CurrentAmount.Read("Int64")+0
		sign:=(full8bytes & 0x8000000000000000) >> 63
		signMulti:=sign ? -1:1
		exponent:=((full8bytes & 0x7FF0000000000000) >> 52) - 1023 ;For IEEE 754 double
		mantissa:=(full8bytes & 0x000FFFFFFFFFFFFF) / 0x000FFFFFFFFFFFFF
		favourExp:=exponent * LOG(2) + LOG(signMulti*(1+mantissa)) ;As an exponent, e.g. 306.6 for 10^306.6=4e306
		return floor(favourExp)
	}

	IBM_ReadAreaMonsterDamageMultiplier()
    {
        return g_SF.Memory.GameManager.game.gameInstances[0].ActiveCampaignData.currentArea.AreaDef.MonsterDamageMultiplier.Read()
    }

	IBM_ReadCampaignMonsterDamageMultiplier()
    {
        return this.GameManager.game.gameInstances[0].ActiveCampaignData.currentRules.MonsterDamageModifier.Read()
    }

	IBM_ReadMonsterBaseDPS()
    {
        return this.GameManager.game.gameInstances[0].ActiveCampaignData.currentRules.monsterbaseStats.BaseDPS.Read()
    }

	IBM_ReadDPSGrowthCurve()
    {
        size:=this.GameManager.game.gameInstances[0].ActiveCampaignData.currentRules.monsterbaseStats.DPSGrowthRateCurve.size.Read()
		data:={}
		loop %size%
		{
			curvePoint:={}
			curvePoint.level:=this.GameManager.game.gameInstances[0].ActiveCampaignData.currentRules.monsterbaseStats.DPSGrowthRateCurve["key",A_Index-1].Read()
			curvePoint.value:=this.GameManager.game.gameInstances[0].ActiveCampaignData.currentRules.monsterbaseStats.DPSGrowthRateCurve[curvePoint.level].Read()
			data.Push(curvePoint)
		}
		return data
    }

	IBM_ReadGoldFirst8BytesBySeat(seat) ;Reads the first 8 bytes of the gold quad
    {
        return this.GameManager.game.gameInstances[0].Screen.uiController.bottomBar.heroPanel.activeBoxes[seat-1].lastGold.Read("Int64")
    }

	/*
    IBM_ReadGoldSecond8BytesBySeat(seat) ;Reads the second 8 bytes of the gold quad. 2026-01-25 - not in use as we're only checking for gold=0 or not, for which the exponent is not necessary
    {
        newObject := this.GameManager.game.gameInstances[0].Screen.uiController.bottomBar.heroPanel.activeBoxes[seat-1].lastGold.QuickClone()
        goldOffsetIndex := newObject.FullOffsets.Count()
        newObject.FullOffsets[goldOffsetIndex] := newObject.FullOffsets[goldOffsetIndex] + 0x8
        return newObject.Read("Int64")
    }
	*/

	IBM_IsCurrentFormationEmpty() ;True if the current formation contains 0 champions
    {
        size := this.GameManager.game.gameInstances[0].Controller.formation.slots.size.Read()
        if(size <= 0 OR size > 14) ; sanity check, 12 is the max number of concurrent champions possible TODO: If 12 is max why is this 14? (was based on g_SF.Memory.GetCurrentFormation() )
            return true ;Assumed that an invalid read means the formation is empty
        loop, %size%
        {
            heroID := this.GameManager.game.gameInstances[0].Controller.formation.slots[A_index - 1].hero.def.ID.Read()
			if (heroID>0)
				return false
        }
		return true
    }

	IBM_IsCurrentFormationFull()
    {
        size:=this.GameManager.game.gameInstances[0].Controller.formation.slots.size.Read()
		loop %size%
        {
            if (this.GameManager.game.gameInstances[0].Controller.formation.slots[A_index - 1].hero.def.ID.Read()=="")
				return false
        }
        return true
    }

	IBM_ClickDamageLevelAmount() ;This is the base amount set per levelling seletion, e.g. always 1/10/25/100
	{
		return this.GameManager.game.gameInstances[0].Screen.uiController.bottomBar.heroPanel.clickDamageBox.levelUpAmount.Read()
	}

	IBM_GetFrontColumnSize() ;Used when we want to block champions from being levelled in the front formation slots so they do not share attacks with Briv
	{
		size:=this.GameManager.game.gameInstances[0].Controller.formation.slots.size.Read()
        frontCount:=0
        loop, %size%
        {
			if (this.GameManager.game.gameInstances[0].Controller.formation.slots[A_index - 1].SlotDef.Column.Read()==0) ;TODO: Might be a problem if there is a Xaryxis-like escort at the front of a formation in the future, read slot validity first?
				frontCount++
        }
		return frontCount
	}

	IBM_ReadIsInstanceDirty() ;Dirty = unsaved data
	{
		return this.GameManager.game.gameInstances[0].isDirty.Read()
	}

	IBM_ReadCurrentSave() ;Pointer to the current save, 0 if there isn't one active, so we can test if it's 0 or not. Non-zero whilst the game is saving
	{
		return this.GameManager.game.gameInstances[0].Controller.userData.SaveHandler.currentSave.Read()
	}

	IBM_ReadIsGameUserLoaded()
	{
		return this.GameManager.game.gameUser.Loaded.Read()
	}

    IBM_ReadClickLevelUpAllowed()
    {
        value:=this.GameManager.game.gameInstances[0].Screen.uiController.bottomBar.heroPanel.clickDamageBox.maxLevelUpAllowed.Read()
        return value=="" ? 1 : value ;TODO: Why does this default to 1 not 0?
    }

	IBM_ReadLastSave()
	{
		return this.GameManager.game.gameInstances[0].Controller.userData.SaveHandler.lastUserDataSaveTime.Read()
	}

	IBM_GetCurrentFormationChampions() ;Returns the champions in the formation, without positioning data, eg data[58]==true
    {
        size:=this.GameManager.game.gameInstances[0].Controller.formation.slots.size.Read()
        if(size<=0 OR size>14) ; sanity check, 12 is the max number of concurrent champions possible.
            return ""
        champList:=[]
        loop, %size%
        {
            heroID:=this.GameManager.game.gameInstances[0].Controller.formation.slots[A_index - 1].hero.def.ID.Read()
            if (heroID > 0)
				champList[heroID]:=true
        }
        return champList
    }

	IBM_GetFormationFieldFamiliarCountBySlot(slot)
	{
		familiarCount:=0
		size:=this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[slot].Familiars["Clicks"].List.size.Read()
		if(size < 0 OR size > 10) ; sanity check, should be < 6 but set to 10 in case of future game field familiar increase.
			return ""
		loop %size%
		{
			if(this.GameManager.game.gameInstances[0].FormationSaveHandler.formationSavesV2[slot].Familiars["Clicks"].List[A_Index - 1].Read()>=0) ;Negative numbers are used to store gaps in familiar layout, e.g. -3,13,-2 means '3 empty spaces, familiar ID 13, 2 empty spaces'
				familiarCount++
		}
		return familiarCount
	}

	IBM_GetActiveGameInstanceID() ;This is the instance ID 1 to 4, NOT the ID if the instance in the gameInstances collection
	{
		return this.GameManager.game.gameInstances[0].InstanceUserData_k__BackingField.InstanceId.Read()
	}
}