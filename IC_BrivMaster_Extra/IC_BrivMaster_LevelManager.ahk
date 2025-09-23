class IC_BrivMaster_LevelManager_Class ;A class for managing champion levelling
{
	Champions:={} ;Stores the level data for each champion in the formation . By champID; seat, levelling key (eg {F5}) min and max. Seat is included so we can surpress by seat if wanted
	levelingDone:={} ;Records if levelling type is completely done, so we don't go through all the checks when we're already done for the run, key by formation, then for z1,min,max, eg levelingDone["Q","min"]==true
	maxKeyPresses:=4
	failedConversionMode:=false
	savedFormations:={} ;Formations as per standard memory reads
	savedFormationChamps:={} ;Champions in each formation, eg savedFormationChamps["E",58]==true -> Briv is in E
	currentWorkList:="" ;Current IC_BrivMaster_LevelManager_WorkList_Class object

	LevelFormation(formationIndex, mode:="min", allowedTime:=10000, forcePriority:=false, surpressByID:="", waitForGold:=false)
	{
		if (this.levelingDone[formationIndex,mode]) ;This formation is done for the given mode
			return
		this.CreateWorklist(formationIndex,mode,surpressByID,waitForGold)
		;OutputDebug % A_TickCount . ":LevelFormation.CreateWorklist() complete`n"
		this.LevelWorklist(allowedTime,forcePriority,waitForGold)
	}

	LevelWorklist(allowedTime:=0,forcePriority:=false,waitForGold:=false) ;Default allowedTime is here, as if this is being called directly we're likely looking for single 'taps' whilst monitoring other things
	{
		if (!IsObject(this.currentWorkList)) ;We've called this without using LevelFormation() first
			return
		startTime:=A_TickCount
		runTime:=0
		while (runTime<=allowedTime) ;Note that as we've set runTime to 0, an AllowedTime of 0 will run once
		{
			;OutputDebug % A_TickCount . ":LevelWorklist Loop Done()=[" . this.currentWorkList.Done() . "] ForcePrio=[" . forcePriority . "] IsPriorityDone()=[" . this.currentWorkList.IsPriorityDone() . "]`n"
			if (this.currentWorkList.Done() OR (forcePriority AND this.currentWorkList.IsPriorityDone())) ;Nothing to do
				break
			;OutputDebug % A_TickCount . ":LevelWorklist Calling Level()`n"
			this.currentWorkList.Level(this.maxKeyPresses,waitForGold,forcePriority)
			runTime:=A_TickCount-startTime
		}
	}

	CreateWorklist(formationIndex,mode,surpressByID,waitForGold) 	;Formation - "Q", if not supplied uses current, mode - "min", supressByID - array of champion IDs to not include in levelling
	{
		if(formationIndex=="") ;Get current
			championIDs:=g_SF.Memory.IBM_GetCurrentFormationChampions()
		else
			championIDs:=this.savedFormationChamps[formationIndex]
		pendingChampCounter:=0 ;Count how many champions still need levelling in the current Mode
		this.currentWorkList:=new IC_BrivMaster_LevelManager_WorkList_Class(this,mode)
		for champID,_ in championIDs
		{
			pendingChampCounter+=this.currentWorkList.AddChamp(champID,surpressByID,waitForGold)
		}
		if(pendingChampCounter==0 AND !(formationIndex==""))
			this.levelingDone[formationIndex,mode]:=true
		;OutputDebug % A_TickCount . ":CreateWorklist() end Size=[" . this.currentWorkList.Champs.Count() . "] pendingChampCounter=[" . pendingChampCounter . "]`n"
	}

	GetClickDamageTargetLevel() ;TODO: This needs a way to factor in the increased level curve beyond z2000
	{
		if (g_SF.Memory.ReadCurrentZone()==1) ;On z1 we want to level to meet Thellora's rush target
			return this.clickDamageTargetRush
		else
			return Min(this.clickDamageTargetFinal,g_SF.Memory.ReadHighestZone()+g_BrivGemFarm.routeMaster.zonesPerJumpQ*2) ;Return the lowest of the reset zone click damage requirement, and one jump from the next landing zone to ensure we don't overlevel, but never have to wait for levelling
	}

	LevelClickDamage(timeout:=500) ;Default 500ms should be good for a min of 3 upgrades, being 300 levels on x100, which should be enough even going 300 zones with Thellora
    {
		startTime:=A_TickCount
		clickTarget:=this.GetClickDamageTargetLevel()
		while (g_SF.Memory.IBM_ReadClickLevel() < clickTarget AND g_SF.Memory.IBM_ReadClickLevelUpAllowed() >= 100 AND A_TickCount - startTime < timeout) ;TODO: This probably needs a check for click damage being below area base mob HP, and allowing levelling in less than 100 levels at a time if so
		{
			;OutputDebug % A_TickCount . ":Levelling Click Damage`n"
			this.KEY_ClickDmg.KeyPress() ;No value in trying to build this to be able to use _Bulk() as it will mostly only be one press at a time
			Sleep IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_INPUT ;Sleep here seems to be necessary to prevent overlevelling
		}
    }

	SetupFailedConversion()
	{
		if (!this.failedConversionMode)
		{
			this.OverrideMinToSoftCap()
			this.failedConversionMode:=true
			this.ResetLevellingDone()
		}
	}

	OverrideMinToSoftCap() ;Overrides all min levels to the champion's softcap, for failed conversion recovery
	{
		for champID,_ in this.Champions
		{
			this.Champions[champID].SetSoftCap()
		}
	}

	;Note that this is only done once, to change a target level the script would have to be restarted
	__New(combine) ;Process all the formations
	{
		levelSettings:=g_BrivUserSettingsFromAddons["IBM_LevelManager_Levels",combine]
		this.ExtractFormation(g_SF.Memory.GetSavedFormationSlotByFavorite(1),"Q")
		this.ExtractFormation(g_SF.Memory.GetSavedFormationSlotByFavorite(2),"W")
		this.ExtractFormation(g_SF.Memory.GetSavedFormationSlotByFavorite(3),"E")
		this.ExtractFormation(g_SF.Memory.GetActiveModronFormationSaveSlot(),"M")
		this.BuildHeroIndexTable() ;Must be before ProcessFormation() TODO: Replace with Sep25 base memory functions table
		this.ProcessFormation(levelSettings)
		this.ResetLevellingDone()
		this.maxKeyPresses:=g_BrivUserSettingsFromAddons["IBM_LevelManager_Input_Max"]
		this.KEY_ClickDmg:=g_BrivGemFarm.inputManager.getKey("ClickDmg")
		this.ExtactFrontColumn()
		this.failedConversionMode:=false
		this.KEY_Modifier:=g_BrivGemFarm.inputManager.getKey(g_BrivUserSettingsFromAddons["IBM_Level_Options_Mod_Key"]=="Ctrl" ? "LCtrl" : g_BrivUserSettingsFromAddons["IBM_Level_Options_Mod_Key"]) ;Modifer to hold - the game uses LeftControl in the keybindings, as much as it doesn't seem to make a lick of difference
		this.modifierLevelUpAmount:=g_BrivUserSettingsFromAddons["IBM_Level_Options_Mod_Value"] ;How many levels applying the modifier key will give per keypress
	}

	BuildHeroIndexTable() ;Builds a table of all used champion IDs with the associated HeroHandler indicies (due to some being skipped it isn't just champID-1)
	{
		size:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.HeroHandler.heroes.size.Read()
		this.HeroIndexTable:={}
		loop %size%
		{
			champID:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.HeroHandler.heroes[A_Index-1].def.ID.Read()
			if (this.savedFormationChamps["A"].HasKey(champID))
			{
				this.HeroIndexTable[champID]:=A_Index-1
			}
		}
	}

	ResetLevellingDone()
	{
		this.levelingDone["Q"]:={"min":false,"z1":false}
		this.levelingDone["W"]:={"min":false,"z1":false}
		this.levelingDone["E"]:={"min":false,"z1":false}
		this.levelingDone["M"]:={"min":false,"z1":false}
		this.levelingDone["A"]:={"min":false,"z1":false}
	}

	IsChampInFormation(champID, index)
	{
		return this.savedFormationChamps[index,champID]
	}

	IsChampInAnyFormation(champID, index) ;index can be multiple, eg "QE" would return true if champID is in either Q or E
	{
		return (inStr(index,"Q") AND this.savedFormationChamps["Q",champID]) OR (inStr(index,"W") AND this.savedFormationChamps["W",champID]) OR (inStr(index,"E") AND this.savedFormationChamps["E",champID]) OR (inStr(index,"M") AND this.savedFormationChamps["M",champID])
	}

	ExtractFormation(slot,index) ;Extracts both the usual formation and the champ list in one go
    {
        this.savedFormations[index]:={}
		this.savedFormationChamps[index]:={}
        size := g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].FormationSaveHandler.formationSavesV2[slot].Formation.size.Read()
        if(size <= 0 OR size > 500) ; sanity check, should be less than 51 as of 2023-09-03
            return ""
        loop, %size%
        {
			champID := g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].FormationSaveHandler.formationSavesV2[slot].Formation[A_Index - 1].Read()
			this.savedFormations[index].Push(champID)
			if (champID != -1)
            {
                this.savedFormationChamps[index,champID]:=true
				this.savedFormationChamps["A",champID]:=true ;"A" is a meta-list of all champions in use
            }
        }
    }

	GetFormation(index) ;Returns saved formation by key, eg "Q", the modron formation "M"
	{
		return this.savedFormations[index]
	}

	ExtactFrontColumn() ;Returns the champions in the front row of the formation, used to suppress levelling so Briv takes all the hits
	{
		frontSize:=g_SF.Memory.IBM_GetFrontColumnSize()
		this.frontColumnChampionsM:={}
		loop %frontSize%
		{
			this.frontColumnChampionsM.Push(this.savedFormations["M"][A_Index])
		}
	}

	GetFrontColumnNoBriv() ;Returns the champions in the front row of the formation, used to suppress levelling so Briv takes all the hits. TODO: This needs to exclude Hew w/avoidance feat
	{
		frontNoBriv:={}
		for _,v in this.frontColumnChampionsM
		{
			if (v!=58)
				frontNoBriv.Push(v)
		}
		return frontNoBriv
	}

	GetFrontColumn() ;Returns the champions in the front row of the formation, used to suppress levelling so Briv takes all the hits. TODO: This needs to exclude Hew w/avoidance feat
	{
		return this.frontColumnChampionsM
	}

	OverrideLevelByID(ChampID, mode, level) ;Updates the current data (only!)
	{
		if (this.Champions.hasKey(ChampID))
			this.Champions[ChampID].OverrideLevel(mode,level)
	}

	ResetLevelByID(ChampID) ;Reset a champion's level to reflect the master settings
	{
		if (this.Champions.hasKey(ChampID))
		{
			this.Champions[ChampID].Reset()
			this.ResetLevellingDone()
		}
	}

	OverrideLevelByIDRaiseToMin(ChampID, mode, level) ;Updates the current data (only) - raises the champions target level to level if lower, otherwise do nothing
	{
		if (this.Champions.hasKey(ChampID))
		{
			if (this.Champions[ChampID].Current[mode] < level) ;TODO: Encapsulate
			{
				this.Champions[ChampID].Current[mode]:=level ;TODO: Encapsulate
				this.ResetLevellingDone() ;As we might need to do further levelling after raising
			}
		}
	}

	OverrideLevelByIDLowerToMax(ChampID, mode, level) ;Updates the current data (only) - lowers the champions target level to level if higher, otherwise do nothing
	{
		if (this.Champions.hasKey(ChampID))
		{
			if (this.Champions[ChampID].Current[mode] > level)
				this.Champions[ChampID].Current[mode]:=level
		}
	}

	RaisePriorityForFrontRow(ChampID) ;Updates the current data (only) - adjusts champion levelling priority for z1 front row - sets to 1/100 if <=0
	{
		if (this.Champions.hasKey(ChampID))
			this.Champions[ChampID].RaisePriorityForFrontRow()
	}


	Reset()
	{
		this.ResetLevellingDone()
		for _,Champion in this.Champions ;Reset each champion
			Champion.Reset()
		this.failedConversionMode:=false
		this.clickDamageTargetFinal:=g_BrivGemFarm.routeMaster.targetZone ;These need a curve for post-z2000 HP. Done in Reset() as __New() is current called before the routemaster is set up
		if (g_BrivGemFarm.routeMaster.combining)
			this.clickDamageTargetRush:=g_BrivGemFarm.routeMaster.ThelloraTarget ;Only needs to be high enough for the Thellora target as we will stop there are do the Casino
		else
			this.clickDamageTargetRush:=g_BrivGemFarm.routeMaster.ThelloraTarget + g_BrivGemFarm.routeMaster.zonesPerJumpQ*2 ;Include 2 jumps
	}

	ProcessFormation(levelSettings)
	{
		for champID,_ in this.savedFormationChamps["A"]
		{
			curChamp:=new IC_BrivMaster_Champion_Class(this,champID,levelSettings)
			this.Champions[champID]:=curChamp
		}
	}
	
	SetModifierKey(useModifier)
	{
		if (useModifier)
			this.KEY_Modifier.Press_Bulk()
		else
			this.KEY_Modifier.Release_Bulk()
		startTime:=A_TickCount
		;TODO: This uses champ 1 as should always be present. It might make sense to swap to click damage though?
		while (g_SF.Memory.IBM_LevellingOverRideActive(1)!=useModifier AND A_TickCount - startTime < 100) ;Allow 100ms for the keypress to apply at maximum to avoid getting stuck. On a fast PC it only took AHK tick (15ms) extra when needed
		{
			Sleep 0 ;Should probably be >0
		}
	}
}

class IC_BrivMaster_Champion_Class ;Represents a champion, along with mostly levelling related properties
{
	__New(levelManager,champID,levelSettings)
	{
		this.ID:=champID
		this.HeroIndex:=levelManager.HeroIndexTable[champID]
		this.Seat:=g_SF.Memory.IBM_ReadChampSeatByIndex(this.HeroIndex)
		this.Key:=g_BrivGemFarm.inputManager.getKey("F" . this.Seat) ;So we don't have to re-calc this constantly
		this.Key.Tag:=this.Seat ;Use the tag to track the seat. TODO: If levelling is encapsulated properly this might not be needed
		this.lastUpgradeLevel:=g_SF.Memory.IBM_GetLastUpgradeLevelByIndex(this.HeroIndex)
		this.Master:={}
		if (levelSettings.hasKey(champID))
		{
			champData:=levelSettings[champID]
			if champData.hasKey("min")
				this.Master.Min:=champData["min"]
			else
			{
				level:=g_BrivUserSettingsFromAddons["IBM_LevelManager_Defaults_Min"]
				this.Master.Min:=(level == "" or !level) ? 0 : 1
			}
			if champData.hasKey("z1")
				this.Master.z1:=champData["z1"]
			else
			{
				level:=g_BrivUserSettingsFromAddons["IBM_LevelManager_Defaults_Min"]
				this.Master.z1:=(level == "" or !level) ? 0 : 1
			}
			if champData.hasKey("z1c")
				this.Master.z1c:=champData["z1c"]
			else
				this.Master.z1c:=false
			if champData.hasKey("prio")
				this.Master.priority:=champData["prio"]
			else
				this.Master.priority:=0
			if champData.hasKey("priolimit")
				this.Master.priorityLimit:=champData["priolimit"]
			else
				this.Master.priorityLimit:=""
		}
		else ;No data, apply defaults
		{
			level:=g_BrivUserSettingsFromAddons["IBM_LevelManager_Defaults_Min"]
			this.Master.Min:=(level == "" or !level) ? 0 : 1
			this.Master.z1:=0 ;This is a champion with no settings at all - do not level them in z1, as that is intended to be a vaguely controlled enviroment
			this.Master.z1c:=false
			this.Master.priority:=0
			this.Master.priorityLimit:=""
		}
		this.Master.PendingLevels:=0
		this.Current:=this.Master.Clone() ;A copy of the master data to allow manipulation at runtime whilst allowing us to reset to default each run
	}

	GetTargetLevel(mode:="min")
	{
		if(mode=="z1")
			return this.Current.z1
		else if (mode=="min")
			return this.Current.min
		else
			return 0
	}

	NeedsLevelling(mode:="min")
	{
		this.Current.Level:=g_SF.Memory.IBM_ReadChampLvlByIndex(this.HeroIndex)
		if (this.Current.Level=="")
			this.Current.Level:=0 ;Or the memory reads were not ready. Need to check something like ReadHeroIsOwned() returns a value maybe? That should possibly be done long before we get this far though
		if(mode=="z1")
			return this.Current.Level < this.Current.z1
		else if (mode=="min")
			return this.Current.Level < this.Current.min
		else
			return 0
	}

	GetPriority(mode:="min",includePending:=true)
	{
		if(mode=="z1") ;Priority settings apply to z1 only
		{
			;this.Current.Level:=g_SF.Memory.IBM_ReadChampLvlByIndex(this.HeroIndex) ;TODO: Decide if we should check this here? It's checked before we add to the levelling Worklist, and after each levelling attempt
			;if (this.Current.Level=="")
			;this.Current.Level:=0 ;Or the memory reads were not ready. Need to check something like ReadHeroIsOwned() returns a value maybe? That should possibly be done long before we get this far though
			expectedLevel:=includePending ? this.Current.Level + this.Current.PendingLevels : this.Current.Level
			if (this.Current.PriorityLimit AND expectedLevel>=this.Current.PriorityLimit)
				return 0
			else
				return this.Current.Priority
		}
		else
			return 0
	}

	CheckZ1cAllowed(mode:="min") ;checks for zone 1 completed conditions
	{
		if(mode=="z1" AND this.Current.z1c)
			return g_SF.Memory.ReadCurrentZone()>1 OR g_SF.Memory.ReadQuestRemaining()==0 ;allow levelling if the zone is complete on z1
		else
			return true
	}

	GetLevelsRequired(mode:="min") ;Always includes pending. Does not refresh this.Current.Level TODO: If we do some fancy memory manager for levelling or champions, we could maybe add the re-check
	{
		if(mode=="z1")
			return Max(this.Current.z1 - (this.Current.Level + this.Current.PendingLevels),0)
		else if (mode=="min")
			return Max(this.Current.min - (this.Current.Level + this.Current.PendingLevels),0)
		else
			return 0
	}

	Reset()
	{
		this.Current:=this.Master.Clone()
	}

	SetSoftCap() ;Sets the champions current min level to softcap
	{
		this.Current.Min:=this.lastUpgradeLevel
	}

	OverrideLevel(mode,level)
	{
		this.Current[mode]:=level
	}

	RaisePriorityForFrontRow()
	{
		if (this.Current.Priority<=0)
		{
			this.Current.Priority:=1
			this.Current.PriorityLimit:=100
		}
	}
}

class IC_BrivMaster_LevelManager_WorkList_Class ;A class to manage the processing of a levelling job
{
	__New(levelManager,mode)
	{
		this.champs:={}
		this.parent:=levelManager
		this.mode:=mode
		this.minPriority:=0 ;Minimum priority of added champions
		this.maxPriority:=0 ;Maximum priority of added champions
	}

	Level(maxKeyPresses,byRef waitForGold, forcePriority) ;WaitforGold is ByRef as we only want to call it on the first iteration
	{
		keyList100:=[]
		keyList10:=[] ;Note this is the modifier list, which might no longer be x10...
		this.GetKeyList(maxKeyPresses,keyList100,keyList10,forcePriority)
		;OutputDebug % A_TickCount . ":levelManager.Level(), x100 count=[" . keyList100.Count() . "] x10 count=[" . keyList10.Count() . "]`n"
		if (keyList100.Count()==0 AND keyList10.Count()==0) ;Due to z1c restrictions, it is possible that .Done() is false but there is nothing to do this iteration
			return
		g_BrivGemFarm.inputManager.gameFocus() ;This might be a bit early when waitforgold is needed. Possibly checking adventure gold, then calling gameFocus(), then checking hero gold might be better for the first run
		if (waitForGold) ;Wait for gold if requested, for start-of-run calls only
		{
			waitForGold:=!this.WaitForFirstGold(keyList100.Count()>0 ? keyList100[1].tag : keyList10[1].tag)
		}
		for _, key in keyList100
			key.KeyPress_Bulk()
		if (keyList10.Count()>0) ;Check this one so modifier key is not used unnecessarily
		{
			this.parent.SetModifierKey(true)
			for _, key in keyList10
				key.KeyPress_Bulk()
			this.parent.SetModifierKey(false)
		}
		this.UpdateLevels()
	}

    WaitForFirstGold(checkSeat)
    {
        StartTime := A_TickCount
		gold:=g_SF.ConvQuadToDouble(g_SF.Memory.IBM_ReadGoldFirst8BytesBySeat(checkSeat), g_SF.Memory.IBM_ReadGoldSecond8BytesBySeat(checkSeat))
		;OutputDebug % A_TickCount . " waiting for gold - seat [" . gold . "] adventure [" . g_SF.ConvQuadToDouble(g_SF.Memory.ReadGoldFirst8Bytes(), g_SF.Memory.ReadGoldSecond8Bytes()) . "]`n"
		while ((gold==0 OR gold=="") AND A_TickCount - StartTime < 10000 ) ;Note that the seat gold value will be null whilst the new run gets set up by the game
        {
			Sleep 0
			gold:=g_SF.ConvQuadToDouble(g_SF.Memory.IBM_ReadGoldFirst8BytesBySeat(checkSeat), g_SF.Memory.IBM_ReadGoldSecond8BytesBySeat(checkSeat))
        }
		;OutputDebug % A_TickCount . ":Waited for first gold - seat [" . gold . "] adventure [" . g_SF.ConvQuadToDouble(g_SF.Memory.ReadGoldFirst8Bytes(), g_SF.Memory.ReadGoldSecond8Bytes()) . "]`n"
		return gold>0
    }

	GetKeyList(maxKeyPresses,byRef keyList100,byRef keyList10,forcePriority) ;Populates the 2 provided keylists. If forcePriority=true then all priority > 1 champions will be added regardless of maxKeyPresses. TODO: Can this continue to count up champions with prio > 0 to allow us to do the IsPriorityDone check here instead of as a separate loop? Note that keyList10 may now be x10 or x25
	{
		curPriority:=this.maxPriority
		while (curPriority>=this.minPriority AND (keyList100.Count() < maxKeyPresses OR (forcePriority AND curPriority>0))) ;Iterate over all used priority levels from highest to lowest
		{
			champList:=this.GetChampsAtPriority(curPriority) ;Get a list of champions at this priority with the number of levels required
			while (champList.Count() > 0 AND (keyList100.Count() < maxKeyPresses OR (forcePriority AND curPriority>0)))
			{
				for champID,_ in champList.Clone() ;Clone so we can remove from the real list
				{
					Champion:=this.Champs[champID]
					levelsRequired:=Champion.GetLevelsRequired(this.mode)
					if (levelsRequired >= 200) ;Add and don't remove as we need more than 1 press
					{
						keyList100.push(Champion.Key)
						Champion.Current.PendingLevels+=100 ;TODO: Should this be encapsulated?
						if (Champion.GetPriority(this.mode,true)!=curPriority) ;The pending levels may take us over the priorityLimit and change the champion's priority
						{
							champList.Remove(champID)
						}
						if (keyList100.Count() >= maxKeyPresses AND (!forcePriority OR curPriority<=0))
							Break ;Breaks out of the For loop. The while loop will handle itself
					}
					else if (levelsRequired >= 100) ;Add and remove since we're <200
					{
						keyList100.push(Champion.Key)
						Champion.Current.PendingLevels+=100 ;TODO: Should this be encapsulated?
						champList.Remove(champID)
						if (keyList100.Count() >= maxKeyPresses AND (!forcePriority OR curPriority<=0))
							Break ;Breaks out of the For loop. The while loop will handle itself
					}
					else
					{
						champList.Remove(champID)
					}
				}
			}
			curPriority--
		}
		k100Count:=keyList100.Count()+1 ;The modifier key cycle required for x10/x25 levelling is considered to be a key press as well
		curPriority:=this.maxPriority
		while (curPriority>=this.minPriority AND (keyList10.Count()+k100Count < maxKeyPresses OR (forcePriority AND curPriority>0))) ;Iterate over all used priority levels from highest to lowest
		{
			champList:=this.GetChampsAtPriority(curPriority) ;Get a list of champions at this priority with the number of levels required
			while (champList.Count() > 0 AND (keyList10.Count()+k100Count < maxKeyPresses OR (forcePriority AND curPriority>0)))
			{
				for champID,_ in champList.Clone() ;Clone so we can remove from the real list
				{
					Champion:=this.Champs[champID]
					levelsRequired:=Champion.GetLevelsRequired(this.mode)
					if (levelsRequired < 100) ;Due to the z1c condition being dynamic a champion can be read in to GetChampsAtPriority() for x10 after having being ignored for x100
					{
						if (levelsRequired > this.parent.modifierLevelUpAmount) ;Add and don't remove as we need more than 1 press
						{
							keyList10.push(Champion.Key)
							Champion.Current.PendingLevels+=this.parent.modifierLevelUpAmount
							if (Champion.GetPriority(this.mode,true)!=curPriority) ;The pending levels may take us over the priorityLimit and change the champion's priority
							{
								champList.Remove(champID)
							}
							;OutputDebug % ">" . champID . ":LevelsRequired:" . levelsRequired[champID] . ","
							if (keyList10.Count()+k100Count >= maxKeyPresses AND (!forcePriority OR curPriority<=0))
								Break ;Breaks out of the For loop. The while loop will handle itself
						}
						else if (levelsRequired > 0) ;Add and remove since we're <=10
						{
							keyList10.push(Champion.Key)
							Champion.Current.PendingLevels+=this.parent.modifierLevelUpAmount
							;OutputDebug % "=" . champID . ":LevelsRequired:" . levelsRequired[champID] . ","
							champList.Remove(champID)
							if (keyList10.Count()+k100Count >= maxKeyPresses AND (!forcePriority OR curPriority<=0))
								Break ;Breaks out of the For loop. The while loop will handle itself
						}
						else
							champList.Remove(champID)
					}
					else
						champList.Remove(champID)
				}
			}
			curPriority--
		}
	}

	GetChampsAtPriority(curPriority)
	{
		champList:={}
		for champID,Champ in this.champs
		{
			if ((Champ.GetPriority(this.mode,true)==curPriority) AND Champ.CheckZ1cAllowed(this.mode))
				champList[champID]:=true
		}
		return champList
	}

	UpdateLevels()
	{
		for champID,_ in this.Champs.Clone() ;Clone because we will be removing entries from the main list
		{
			this.Champs[champID].Current.PendingLevels:=0
			if (!this.Champs[champID].NeedsLevelling(this.Mode))
				this.Champs.Delete(champID)
		}
	}

	IsPriorityDone()
	{
		for _,Champion in this.Champs
		{
			if ((Champion.GetPriority(this.mode,false) > 0) AND Champion.CheckZ1cAllowed(this.mode)) ;GetPriority() does not include pending levels here
				return false
		}
		return true
	}

	Done()
	{
		return this.Champs.Count()==0
	}

	AddChamp(champID,surpressByID,startOfRun)
	{
		if (!this.parent.Champions.hasKey(champID)) ;No data
			return 0
		Champion:=this.parent.Champions[champID]
		if(Champion.NeedsLevelling(this.mode))
		{
			if (startOfRun OR champID==g_SF.Memory.IBM_SelectedChampIDBySeat(Champion.Seat)) ;We can't check for selection at the very start as the game will still be loading in, so have to assume M has loaded as it should. Expecting WaitForGold to be passed as startOfRun here
			{
				if (!this.ValueIsInList(surpressByID,champID))
				{
					this.Champs[champID]:=Champion
					this.UpdatePriorityMinMax(Champion.GetPriority(this.mode,false)) ;No point including pending levels as there won't be any
				}
			}
			return 1
		}
		else
			
		return 0
	}

	ValueIsInList(simpleArray, findValue) ;true if the value is in the given simple Array
	{
		for _,v in simpleArray
		{
			if (v==findValue)
				return true
		}
		return false
	}

	UpdatePriorityMinMax(current)
	{
		if (current<this.minPriority)
			this.minPriority:=current
		if (current>this.maxPriority)
			this.maxPriority:=current
	}
}