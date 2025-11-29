;No #include required as already in IC_SharedFunctions_Class.ahk

class IC_BrivMaster_MemoryFunctions_Class extends IC_MemoryFunctions_Class
{
	IBM_GetWebRootString() ;Handle read failures - it uses the EngineSettings pointer that moves a lot
	{
		webRoot:=g_SF.Memory.ReadWebRoot()
		if(!webRoot)
			webRoot:="Unable to read WebRoot"
	}
	
	IBM_ReadGameVersionMinor() ;If the game is 636.2, return '.2'. This can the empty
	{
		return this.GameSettings.VersionPostFix.Read() 
    }
	
	IBM_IsBuffActive(buffName) ;Is a Gem Hunter potion active
	{
		buffSize:=this.GameManager.game.gameInstances[this.GameInstance].BuffHandler.activeBuffs.size.Read()
		if (buffSize < 0 OR size > 1000)
			return false 
		loop %buffSize%
		{
			if (this.GameManager.game.gameInstances[this.GameInstance].BuffHandler.activeBuffs[A_Index-1].Name.Read()==buffName) ;TODO: Find out if this gets localised; might need to use the effect name (although that would collide with anything else that gave +50% gems)
				return true
		}
		return false
	}

	;Overridden to remove cruft and to take a PID instead of just finding it via a window name, in case there are multiple windows
	OpenProcessReader(pid:="") ;If supplied with a PID will have the memory manager load that instead of using the window, via IBM override
    {
        global g_UserSettings
        _MemoryManager.exeName := g_IBM_Settings["IBM_Game_Exe"]
        isExeRead := _MemoryManager.Refresh(,pid)
        if(isExeRead == -1)
            return
        if(_MemoryManager.handle == "")
            MsgBox, , , Could not read from exe. Try running as Admin. , 7
        this.Is64Bit := _MemoryManager.is64Bit
		this.GameManager.Refresh()
        this.GameSettings.Refresh()
        this.EngineSettings.Refresh()
    }
	
	IBM_IsChampInCurrentFormation(champID) ;g_SF.IsChampInFormation() loops through the results of a loop, which is...not ideal
	{
        FORMATION_SLOTS:=this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots
		size:=FORMATION_SLOTS.size.Read()
        if(size <= 0 OR size > 14) ; sanity check, 12 is the max number of concurrent champions possible.
            return ""
        loop, %size%
        {
            if (champID==FORMATION_SLOTS[A_index - 1].hero.def.ID.Read())
				return true
        }
        return false
    }
	
	IBM_ReadBaseGameSpeed() ;Reads the game speed without the area transition multipier Diana applies, e.g. x10 will flick between x10 and x50 constantly - this will always return x10
	{
		areaTransMulti:=this.GameManager.game.gameInstances[this.GameInstance].areaTransitionTimeScaleMultiplier.Read()
        if (!areaTransMulti)
			areaTransMulti:=1 ;So we don't divide by zero
		return this.GameManager.TimeScale.Read() / areaTransMulti
	}
	
	IBM_ReadCurrentZoneMonsterHealthExponent() ;Returns 85.90308999 for 8e85 for example
	{
		MEMORY_HEALTH:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].ActiveCampaignData.currentArea.Health
		first8:=MEMORY_HEALTH.Read("Int64") ;Quad
        newObject := MEMORY_HEALTH.QuickClone()
        offsetIndex := newObject.FullOffsets.Count()
        newObject.FullOffsets[offsetIndex] := newObject.FullOffsets[offsetIndex] + 0x8
		last8:= newObject.Read("Int64")
		return this.IBM_ConvQuadToExponent(first8,last8)
	}
	
	IBM_ConvQuadToExponent(FirstEight,SecondEight) ;Converts a quad to an exponent, e.g. 8e85 to 85.90308999. Necessary as AHK can't do Doubles let alone Quads
    {
        f := log( FirstEight + ( 2.0 ** 63 ) )
        decimated := ( log( 2 ) * SecondEight / log( 10 ) ) + f
        if(decimated <= 4)
            return round((FirstEight + (2.0**63)) * (2.0**SecondEight), 2)
        exponent:=floor(decimated)
        significand:=round( 10 ** (decimated-exponent), 2 )
        return exponent + log(significand)
    }

	IBM_ReadChampSeatByIndex(heroIndex:=0) ;Avoids the hero index lookup
    {
        return this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[heroIndex].def.SeatID.Read()
    }

	IBM_GetLastUpgradeLevelByIndex(heroIndex) ;Loop upgrades until the upgrade with the highest level is found.
	{
		size:=this.GameManager.game.gameInstances[this.GameInstance].Controller.UserData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId.size.Read()
		if (size < 1 || size > 1000)
			return 0
		maxUpgradeLevel:=0
		Loop, %size% ;Loop and save the highest level requirement
		{
			requiredLevel := this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[heroIndex].upgradeHandler.upgradesByUpgradeId["value",A_Index-1].RequiredLevel.Read()
			if (requiredLevel != 9999) ;This check taken from IC_BrivGemFarm_Levelup; I assume this is the value for 'not available'
				maxUpgradeLevel := Max(requiredLevel, maxUpgradeLevel)
		}
		return maxUpgradeLevel
	}

	/*
	IBM_GetTotalBrivSkipZones() ;Uses direct reads instead of a handler. This one is probably a bad idea...but is also not being used
	{
		;Briv jumps a base amount + a chance for another zone. At an exact jump that chance is normally 1, so 9 amount + 1 = 10 Zones, aka 9J
		;Accurate Acrobatics does reduce the chance to 0, so 12 + 0 = 12 Zones, aka 11J (given iLevels for 11.9998)
		EK_HANDLER:=this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[this.GetHeroHandlerIndexByChampID(58)].effects.effectKeysByHashedKeyName
		EK_HANDLER_SIZE := EK_HANDLER.size.Read()
		EllyUltActive:=""
		loop, %EK_HANDLER_SIZE%
		{
			PARENT_HANDLER:=EK_HANDLER["value", A_Index - 1].List[0].parentEffectKeyHandler
			if ("briv_unnatural_haste" == PARENT_HANDLER.def.Key.Read())
			{
				brivSkipAmount:=PARENT_HANDLER.activeEffectHandlers[0].areaSkipAmount.Read() ;TODO: Should resolve offsets for activeEffectHandlers[0] once and re-use within the call instead of having it all twice
				brivSkipChance:=PARENT_HANDLER.activeEffectHandlers[0].areaSkipChance.Read()
				break
			}
		}
		if (brivSkipAmount="" OR brivSkipChance="")
			return ""
		return brivSkipAmount + Round(brivSkipChance)
	}
	*/

	/* Replaced by IBM_UseUltimate
	IBM_GetUltimateHotkey(champID)
	{
		ULTIMATEITEMS_LIST:=this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.ultimatesBar.ultimateItems
        ULTIMATE_HOTKEY:=""
		loop, % ULTIMATEITEMS_LIST.size.Read()
        {
            if (champID == ULTIMATEITEMS_LIST[A_Index-1].hero.def.ID.Read())
			{
				ULTIMATE_HOTKEY:=ULTIMATEITEMS_LIST[A_Index-1].HotKey.Read()
				break
			}
        }
		return ULTIMATE_HOTKEY
	}
	*/

	IBM_GetUltimateCooldown(champID)
	{
		ULTIMATEITEMS_LIST:=this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.ultimatesBar.ultimateItems
        ULTIMATE_CD:=""
		loop, % ULTIMATEITEMS_LIST.size.Read()
        {
            if (champID == ULTIMATEITEMS_LIST[A_Index-1].hero.def.ID.Read())
			{
				ULTIMATE_CD:=ULTIMATEITEMS_LIST[A_Index-1].ultimateAttack.internalCooldownTimer.Read()
				break
			}
        }
		return ULTIMATE_CD
	}
	
	IBM_UseUltimate(champID,maxRetries:=50,exitOnceQueued:=false) ;Uses an ultimate, retrying up to the given number of times if the cooldown doesn't change. If exitOnceQueued is true the function will return as soon as the ultimate is queued - which may mean it never activates if something changes in the game state (area change most likely)
	{
		ULTIMATEITEMS_LIST:=this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.ultimatesBar.ultimateItems
        ULTIMATE_HOTKEY:=""
		ADDRESS_ULTIMATEITEMS_LIST:=_MemoryManager.instance.getAddressFromOffsets(ULTIMATEITEMS_LIST.BasePtr.BaseAddress,ULTIMATEITEMS_LIST.FullOffsets*)
		ADDRESS_ULTIMATEITEMS_ITEMS:=_MemoryManager.instance.getAddressFromOffsets(ADDRESS_ULTIMATEITEMS_LIST,0x10)
		HEROID_OFFSET:=[ULTIMATEITEMS_LIST.hero.Offset[1],ULTIMATEITEMS_LIST.hero.def.Offset[1],ULTIMATEITEMS_LIST.hero.def.ID.Offset[1]] ;TODO: A lot of this never changes; should be prepared once only. Some kind of ultimate handler object?
		HEROID_TYPE:=ULTIMATEITEMS_LIST.hero.def.ID.ValueType
		
		loop, % _MemoryManager.instance.read(ADDRESS_ULTIMATEITEMS_LIST,"Int",0x18)
        {
            ADDRESS_ULTIMATEITEMS_ITEM:=_MemoryManager.instance.getAddressFromOffsets(ADDRESS_ULTIMATEITEMS_ITEMS,0x20 + (A_Index-1) * 0x8)
			if (champID == _MemoryManager.instance.read(ADDRESS_ULTIMATEITEMS_ITEM,HEROID_TYPE,HEROID_OFFSET*))
			{
				ULTIMATE_HOTKEY:=_MemoryManager.instance.read(ADDRESS_ULTIMATEITEMS_ITEM,ULTIMATEITEMS_LIST.HotKey.ValueType,ULTIMATEITEMS_LIST.HotKey.Offset*)
				break
			}
        }
		if (ULTIMATE_HOTKEY=="") ;Return empty
			return
		ULTIMATE_KEY:=g_IBM.inputManager.getKey(ULTIMATE_HOTKEY) ;TODO: Maybe the input manager should be passed as an argument to this function? Or if moved to an object it could just be passed over once at setup of that
		ULTIMATE_KEY.KeyPress()
		retryCount:=0
		ULTIMATEATTACK:=ULTIMATEITEMS_LIST.ultimateAttack
		ADDRESS_ULTIMATEATTACK:=_MemoryManager.instance.getAddressFromOffsets(ADDRESS_ULTIMATEITEMS_ITEM, ULTIMATEATTACK.Offset*)
		while (_MemoryManager.instance.read(ADDRESS_ULTIMATEATTACK,ULTIMATEATTACK.internalCooldownTimer.ValueType,ULTIMATEATTACK.internalCooldownTimer.Offset*)<=0 AND retryCount < maxRetries)
		{
			if (_MemoryManager.instance.read(ADDRESS_ULTIMATEATTACK,ULTIMATEATTACK.queued.ValueType,ULTIMATEATTACK.queued.Offset*)) ;If the ultimate is queued, just wait on it
			{
				retryCount++ ;Counting this as 1/10th of a retry to avoid having to have some duplicate timeout in case the queued attack gets stuck forever
				if (exitOnceQueued)
					return retryCount
				g_IBM.IBM_Sleep(15)
			}
			else
			{
				ULTIMATE_KEY.KeyPress()
				retryCount+=10
				Sleep 0
			}
		}
		return retryCount
	}

	IBM_GetEllywickUltimateActive() ;Direct read, slower than using an ActiveEffectKeyHandler, but this is the only thing read from CotFeywild - the rest is in DoMThings which is separate
	{
		EK_HANDLER:=this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[this.GetHeroHandlerIndexByChampID(83)].effects.effectKeysByHashedKeyName
		EK_HANDLER_SIZE := EK_HANDLER.size.Read()
		EllyUltActive:=""
		loop, %EK_HANDLER_SIZE%
		{
			PARENT_HANDLER:=EK_HANDLER["value", A_Index - 1].List[0].parentEffectKeyHandler
			if ("ellywick_call_of_the_feywild" == PARENT_HANDLER.def.Key.Read())
			{
				EllyUltActive:=PARENT_HANDLER.activeEffectHandlers[0].IsUltimateActive.Read()
				break
			}
		}
		return EllyUltActive
	}

	;Overridden to remove weird fallbacks to reads with no imports
	ReadChampLvlByID(ChampID:= 0)
    {
		return this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[this.GetHeroHandlerIndexByChampID(ChampID)].level.Read()
    }

	IBM_ReadChampLvlByIndex(heroIndex:=0)
    {
        return this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[heroIndex].level.Read()
    }

	IBM_ThelloraTriggered() ;Has Thellora rushed yet this run?
	{
		return this.GameManager.game.gameInstances[this.GameInstance].StatHandler.ServerStats["thellora_plateaus_of_unicorn_run_has_triggered"].Read()==1
	}

	IBM_GetThelloraAreaCharges() ;How many zones does Thellora have stored? TODO: A little encapsulation wouldn't kill us, these keys are going to get scattered all over
	{
		return this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.StatHandler.ServerStats["thellora_plateaus_of_unicorn_run_areas"].Read()
	}

	IBM_GetThelloraRushTarget() ;Gets the base favour exponent which Thellora uses to cap her rush amount. Note this is much slower than using an ActiveEffectKeyHandler that is already set up for her, but much faster than having to set one up first
	{
		ThelloraRushTarget:=""
		EK_HANDLERS:=this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[this.GetHeroHandlerIndexByChampID(139)].effects.effectKeysByHashedKeyName
		EK_HANDLERS_SIZE := EK_HANDLERS.size.Read()
		loop, %EK_HANDLERS_SIZE%
		{
			EK_PARENT_HANDLER:=EK_HANDLERS["value", A_Index - 1].List[0].parentEffectKeyHandler
			if ("thellora_plateaus_of_unicorn_run" == EK_PARENT_HANDLER.def.Key.Read())
			{
				ThelloraRushTarget:=EK_PARENT_HANDLER.activeEffectHandlers[0].baseFavorExponent.Read()
				break
			}
		}
		return ThelloraRushTarget
	}

	IBM_ReadAreaMonsterDamageMultiplier()
    {
        return g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].ActiveCampaignData.currentArea.AreaDef.MonsterDamageMultiplier.Read()
    }

	IBM_ReadCampaignMonsterDamageMultiplier()
    {
        return this.GameManager.game.gameInstances[this.GameInstance].ActiveCampaignData.currentRules.MonsterDamageModifier.Read()
    }

	IBM_ReadMonsterBaseDPS()
    {
        return this.GameManager.game.gameInstances[this.GameInstance].ActiveCampaignData.currentRules.monsterbaseStats.BaseDPS.Read()
    }

	IBM_ReadDPSGrowthCurve()
    {
        size:=this.GameManager.game.gameInstances[this.GameInstance].ActiveCampaignData.currentRules.monsterbaseStats.DPSGrowthRateCurve.size.Read()
		data:={}
		loop %size%
		{
			curvePoint:={}
			curvePoint.level:=this.GameManager.game.gameInstances[this.GameInstance].ActiveCampaignData.currentRules.monsterbaseStats.DPSGrowthRateCurve["key",A_Index-1].Read()
			curvePoint.value:=this.GameManager.game.gameInstances[this.GameInstance].ActiveCampaignData.currentRules.monsterbaseStats.DPSGrowthRateCurve[curvePoint.level].Read()
			data.Push(curvePoint)
		}
		return data
    }

	IBM_GetMaxHealthByID(champID)
    {
        return this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[this.GetHeroHandlerIndexByChampID(champID)].lastMaxHealth.Read()
    }

	IBM_GetOverwhelmByID(champID)
    {
        return this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.HeroHandler.heroes[this.GetHeroHandlerIndexByChampID(champID)].overwhelm.Read()
    }

	IBM_ReadGoldFirst8BytesBySeat(seat)
    {
        return this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.bottomBar.heroPanel.activeBoxes[seat-1].lastGold.Read("Int64")
    }

    ;reads the last 8 bytes of the quad value of gold
    IBM_ReadGoldSecond8BytesBySeat(seat)
    {
        newObject := this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.bottomBar.heroPanel.activeBoxes[seat-1].lastGold.QuickClone()
        goldOffsetIndex := newObject.FullOffsets.Count()
        newObject.FullOffsets[goldOffsetIndex] := newObject.FullOffsets[goldOffsetIndex] + 0x8
        return newObject.Read("Int64")
    }

	IBM_IsSplashVideoActive() ;True if the loading screen videos are playing
	{
		return this.GameManager.game.loadingScreen.SplashScreen.IsActive_k__BackingField.Read()==1
	}

	IBM_IsCurrentFormationEmpty() ;True if the current formation contains 0 champions
    {
        size := this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots.size.Read()
        if(size <= 0 OR size > 14) ; sanity check, 12 is the max number of concurrent champions possible TODO: If 12 is max why is this 14? (was based on g_SF.Memory.GetCurrentFormation() )
            return true ;Assumed that an invalid read means the formation is empty
        loop, %size%
        {
            heroID := this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots[A_index - 1].hero.def.ID.Read()
			if (heroID>0)
				return false
        }
		return true
    }

	IBM_IsCurrentFormationFull()
    {
        size := this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots.size.Read()
		loop %size%
        {
            if (this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots[A_index - 1].hero.def.ID.Read()="")
				return false
        }
        return true
    }

	IBM_LevellingOverRideActive(seat) ;Is a modifier key combination being used to adjust levelling? This reads the specified seat although they should the same. TODO: Clickdamage would probably be a smarter way of doing this
	{
		this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.bottomBar.heroPanel.activeBoxes[seat-1].levelUpInfoHandler.OverrideLevelUpAmount.Read()
	}

	IBM_GetFrontColumnSize() ;Used when we want to block champions from being levelled in the front formation slots so they do not share attacks with Briv
	{
		size := this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots.size.Read()
        frontCount:=0
        loop, %size%
        {
			if (this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots[A_index - 1].SlotDef.Column.Read()==0) ;TODO: Might be a problem if there is a Xaryxis-like escort at the front of a formation in the future, read slot validity first?
				frontCount++
        }
		return frontCount
	}

	IBM_HeroHasFeatSavedInFormation(heroID, featID, formationSlot)
	{
		size := this.GameManager.game.gameInstances[this.GameInstance].FormationSaveHandler.formationSavesV2[formationSlot].Feats[heroID].List.size.Read()
		if(size <= 0 OR size > 10) ; sanity check, should be < 6 but set to 10 in case of future game field familiar increase.
			return false
		Loop, %size%
		{
			value := this.GameManager.game.gameInstances[this.GameInstance].FormationSaveHandler.formationSavesV2[formationSlot].Feats[heroID].List[A_Index - 1].Read()
			if (value==featID)
				return true
		}
		return false
	}

	IBM_HeroHasAnyFeatsSavedInFormation(heroID, formationSlot)
	{
		size := this.GameManager.game.gameInstances[this.GameInstance].FormationSaveHandler.formationSavesV2[formationSlot].Feats[heroID].List.size.Read()
		if(size <= 0 OR size > 10) ; sanity check, should be < 6 but set to 10 in case of future game field familiar increase.
			return false
		return true
	}

	IBM_ReadIsInstanceDirty() ;Dirty = unsaved data
	{
		return this.GameManager.game.gameInstances[this.GameInstance].isDirty.Read()
	}

	IBM_ReadIsGameUserLoaded()
	{
		return this.GameManager.game.gameUser.Loaded.Read()
	}

	IBM_ReadClickLevel()
    {
        return this.GameManager.game.gameInstances[this.GameInstance].ClickLevel.Read()
    }

    IBM_ReadClickLevelUpAllowed()
    {
        value := this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.bottomBar.heroPanel.clickDamageBox.maxLevelUpAllowed.Read()
        return value == "" ? 1 : value
    }

	IBM_ReadLastSave()
	{
		return this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.SaveHandler.lastUserDataSaveTime.Read()
	}

	IBM_GetCurrentFormationChampions() ;Returns the champions in the formation, without positioning data, eg data[58]==true
    {
        size := this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots.size.Read()
        if(size <= 0 OR size > 14) ; sanity check, 12 is the max number of concurrent champions possible.
            return ""
        champList := []
        loop, %size%
        {
            heroID := this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots[A_index - 1].hero.def.ID.Read()
            if (heroID > 0)
				champList[heroID]:=true
        }
        return champList
    }

	IBM_SelectedChampIDBySeat(seat)
	{
		return this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.bottomBar.heroPanel.activeBoxes[seat - 1].hero.def.ID.read()
	}
}