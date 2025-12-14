;No #include required as already in IC_SharedFunctions_Class.ahk

class IC_BrivMaster_MemoryFunctions_Class extends IC_MemoryFunctions_Class
{
	IBM_GetWebRootFriendly() ;Handle failures for user-facing reads (mainly the log). WebRoot uses the EngineSettings pointer that moves a lot
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

	/*
	IBM_GetTotalBrivSkipZones() ;Uses direct reads instead of a handler. This one is probably a bad idea...but is also not being used. Note memory reads for this have been removed
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
            if (this.GameManager.game.gameInstances[this.GameInstance].Controller.formation.slots[A_index - 1].hero.def.ID.Read()=="")
				return false
        }
        return true
    }

	IBM_ClickDamageLevelAmount() ;This is the base amount set per levelling seletion, e.g. always 1/10/25/100
	{
		return this.GameManager.game.gameInstances[this.GameInstance].Screen.uiController.bottomBar.heroPanel.clickDamageBox.levelUpAmount.Read()
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
	
	IBM_ReadCurrentSave() ;Pointer to the current save, 0 if there isn't one active, so we can test if it's 0 or not. Non-zero whilst the game is saving
	{
		return this.GameManager.game.gameInstances[this.GameInstance].Controller.userData.SaveHandler.currentSave.Read()
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
	
	IBM_GetFormationFieldFamiliarCountBySlot(slot)
	{
		familiarCount:=0
		size:=this.GameManager.game.gameInstances[this.GameInstance].FormationSaveHandler.formationSavesV2[slot].Familiars["Clicks"].List.size.Read()
		if(size < 0 OR size > 10) ; sanity check, should be < 6 but set to 10 in case of future game field familiar increase.
			return ""
		loop %size%
		{
			if(this.GameManager.game.gameInstances[this.GameInstance].FormationSaveHandler.formationSavesV2[slot].Familiars["Clicks"].List[A_Index - 1].Read()>=0) ;Negative numbers are used to store gaps in familiar layout, e.g. -3,13,-2 means '3 empty spaces, familiar ID 13, 2 empty spaces'
				familiarCount++
		}
		return familiarCount
	}
}