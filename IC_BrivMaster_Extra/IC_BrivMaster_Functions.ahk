class IC_BrivMaster_Logger_Class ;A class for recording run logs
{
	__New(logPath)
	{
		this.logPath:=logPath
		reset:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.StatHandler.Resets.Read() ;TODO: Move this to .Memory and use that both here and in Melf Manager?
		if (reset!="") ;If we can read the current reset use that, otherwise set to -1 for invalid
			g_SharedData.RunLogResetNumber:=reset
		else
			g_SharedData.RunLogResetNumber:=-1
		g_SharedData.RunLog:={}
		this.LogEntries:={}
		this.OutputHeader()
	}

	NewRun()
	{
		startTime:=A_TickCount ;So it doesn't change between entries
		if (this.LogEntries.HasKey("Run")) ;There will be no entry for the first run
		{
			this.LogEntries.Run.End:=startTime
			if (this.LogEntries.Run.LastZone > g_BrivGemFarm.RouteMaster.targetZone) ;We don't get anything from bosses jumped after our reset, so clamp
				this.LogEntries.Run.LastZone:=g_BrivGemFarm.RouteMaster.targetZone
			else if (this.LogEntries.Run.LastZone < g_BrivGemFarm.RouteMaster.targetZone) ;If we didn't make it to reset
				this.LogEntries.Run.Fail:=true
			g_SharedData.RunLogResetNumber:=-1 ;Invalid whilst updating
			g_SharedData.RunLog:=AHK_JSON.Dump(this.LogEntries)
			g_SharedData.RunLogResetNumber:=this.LogEntries.Run.ResetNumber
			;Output log
			runString:=this.LogEntries.Run.ResetNumber . "," . this.LogEntries.Run.StartRealTime . "," . this.LogEntries.Run.Start . "," . this.LogEntries.Run.End - this.LogEntries.Run.Start . "," . this.LogEntries.Run.ResetReached - this.LogEntries.Run.Start . "," . this.LogEntries.Run.End - this.LogEntries.Run.ResetReached . "," . this.LogEntries.Run.Cycle . "," . this.LogEntries.Run.Fail . "," . this.LogEntries.Run.LastZone
			messageString:=""
			for _,v in this.LogEntries.Messages
				messageString.=v . ","
			FileAppend, % runString . "," . g_SF.Memory.ReadChestCountByID(282) . "," . messageString . "`n", % this.logPath
		}
		;Reset for new
		this.LogEntries.Messages:={}
		this.LogEntries.Thellora:={}
		this.LogEntries.Run:={}
		this.LogEntries.Run.Start:=startTime
		FormatTime, formattedDateTime,, yyyy-MM-ddTHH:mm:ss
		this.LogEntries.Run.StartRealTime:=formattedDateTime
		this.LogEntries.Run.ResetNumber:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.StatHandler.Resets.Read() ;TODO: Move this to .Memory and use that both here and in Melf Manager?
		this.LogEntries.Run.GHActive:=g_SF.Memory.IBM_IsBuffActive("Potion of the Gem Hunter") ;Does this break in non-English clients?
		this.LogEntries.Run.LastZone:=0
		this.LogEntries.Run.Fail:=false
		this.LogEntries.Run.Cycle:=""
	}
	
	SetRunCycle(cycleNumber) ;The routeMaster won't be .Reset() until after the log starts, so need to add the cylce number once available
	{
		if (this.LogEntries.HasKey("Run"))
			this.LogEntries.Run.Cycle:=cycleNumber
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
	}

	OutputHeader()
	{
		FileAppend, % "Reset #,Start Time,Start Tick,Total,Active,Reset,Cycle,Fail,LastZone,Electrum`n", % this.logPath
	}

}

class IC_BrivMaster_InputManager_Class ;A class for managing input related matters
{
	keyList:={} ;Indexed by key per the script (e.g. "F1","ClickDmg"), contains the mapped Key, lParam for SendMessage for down, and lParam for SendMessage for up

	__new() ;Currently it is up to code using this to add the necessary keys
	{
		this.gameFocus()
	}

	addKey(key)
	{
		if (!this.keyList.HasKey(key))
			this.keyList[key]:=new IC_BrivMaster_InputManager_Key_Class(key)
	}

	getKey(key)
	{
		if (!this.keyList.HasKey(key))
			this.addkey(key)
		return this.keyList[key]
	}

	gameFocus() ;We need a way to detect IC losing focus, as that appears to be the only case that this needs to be re-called
	{
		hwnd:=g_SF.Hwnd
		ControlFocus,, ahk_id %hwnd%
	}
}

class IC_BrivMaster_InputManager_Key_Class ;Represents a single key. Used by IC_BrivMaster_InputManager_Class
{
	__new(key)
	{
		this.key:=key ;TODO: Is this useful?
		this.mappedKey:=g_KeyMap[key]
		sc:=g_SCKeyMap[key] << 16
        this.lparamDown := Format("0x{:X}", 0x0 | sc)
		this.lparamUp := Format("0x{:X}", 0xC0000001 | sc)
		this.tag:="" ;Used for tracking arbitary infomation on the key, e.g. the associated seat for F-keys
	}

	Press() ;Hold a key and do not release
	{
        hwnd:=g_SF.Hwnd
		mk:=this.mappedKey ;We have to copy the variables locally due to limitations of AHK :(
		lD:=this.lparamDown
        ControlFocus,, ahk_id %hwnd% ;TODO: is this strictly necessary for every input event? It is needed to some extent - the script clearly fails to sent inputs without it, but it seems this is only after focus is removed from the game window
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
	}

	Release() ;Release a key
	{
        hwnd:=g_SF.Hwnd
		mk:=this.mappedKey
		lU:=this.lparamUp
        ControlFocus,, ahk_id %hwnd% ;As above
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
	}

	KeyPress() ;Press then release a key
	{
		startCritical:=A_IsCritical ;Store existing state of critical
		Critical, On
        hwnd:=g_SF.Hwnd
        mk:=this.mappedKey
		lD:=this.lparamDown
		lU:=this.lparamUp
		ControlFocus,, ahk_id %hwnd% ;As above
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
        if (!startCritical) ;Only turn critical off if wasn't on when we entered this function
			Critical, Off
	}

	Press_Bulk() ;The _Bulk versions do not set ControlFocus, and are intended for code that will send a lot of input together (e.g. levelling) and that code will be responsible for calling ControlFocus once
	{
        hwnd:=g_SF.Hwnd
		mk:=this.mappedKey ;We have to copy the variables locally due to limitations of AHK :(
		lD:=this.lparamDown
    	SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
	}

	Release_Bulk() ;Release a key
	{
        hwnd:=g_SF.Hwnd
		mk:=this.mappedKey
		lU:=this.lparamUp
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
	}

	KeyPress_Bulk() ;Press then release a key
	{
        hwnd:=g_SF.Hwnd
        mk:=this.mappedKey
		lD:=this.lparamDown
		lU:=this.lparamUp
		SendMessage, 0x0100, %mk%, %lD%,, ahk_id %hwnd%,,,,1000
		SendMessage, 0x0101, %mk%, %lU%,, ahk_id %hwnd%,,,,1000
	}
}

class IC_BrivMaster_EllywickDealer_Class ;A class for managing Ellywick's card draws and her ultimate use. This is based heavily on ImpEGamer's RNGWaitingRoom addon
{
		static HERO_ID_DM:=99
		static HERO_ID_ELLY:=83
		static ULTIMATE_RESOLUTION_TIME:=300 ;Real world milliseconds. Normally seems to be 0 to 140ms

		CasinoTimer := ObjBindMethod(this, "Casino")
        GemCardsNeeded := {} ;These are pairs of base and melf mode values, eg {0:2,1:3} for 2 without melf and 3 with
        MaxRedraws := {}
		MinCards:={}
		GemCardsNeededInFlight:=0
        Complete := false
        Redraws := 0 ;Current redraws
        UsedUlt := false ;Tracks Elly's ult being in progress, as her cards are only cleared when it ENDS, despite the visual
        MelfMode:=false ;Is melf spawning more? Used to select the appropriate options
		StatusString:=" STATUS=" ;Used to return basic information on problems (eg DM fails)
		EFFECT_HANDLER_CARDS:="" ;Deck of Many Things effect handler cards object, dereferrenced from main memory functions for performance
		EFFECT_KEY_DOMT:="ellywick_deck_of_many_things"

		;DEBUG THINGS
		DEBUG:=false
		DEBUG_LOG_STRING:=""

		__new()
		{
			this.HERO_INDEX_ELLY:=g_SF.Memory.GetHeroHandlerIndexByChampID(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY) ;Only needs to be done once TODO: Use LevelManager lookup
		}

		DEBUG_ELLY_STATUS_CSV(commentString:="")
		{
			this.DEBUG_LOG_STRING.=A_NOW . "," . A_TickCount . "," . commentString . ","
			this.DEBUG_LOG_STRING.="Elly:" . g_SF.Memory.IBM_GetUltimateCooldown(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY) . ","
			this.DEBUG_LOG_STRING.="DM:" . g_SF.Memory.IBM_GetUltimateCooldown(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM) . ","
			this.DEBUG_LOG_STRING.="Redraws:" . this.Redraws . "/" . this.MaxRedraws[this.melfMode] . ","
			this.DEBUG_LOG_STRING.="Transitioning:" . g_SF.Memory.ReadTransitioning() . ","
			this.DEBUG_LOG_STRING.="Hand:"
			loop, % this.EFFECT_HANDLER_CARDS.size.Read()
			{
				if (3==this.EFFECT_HANDLER_CARDS[A_index - 1].CardType.Read())
					this.DEBUG_LOG_STRING.="G"
				else
					this.DEBUG_LOG_STRING.="x"
			}
			this.DEBUG_LOG_STRING.=",MelfMode:" . this.melfMode . ","
			this.DEBUG_LOG_STRING.="EllyUltViaRead:" . g_SF.Memory.IBM_GetEllywickUltimateActive() . ","
			this.DEBUG_LOG_STRING.="z" . g_SF.Memory.ReadCurrentZone() . ","
			this.DEBUG_LOG_STRING.=this.DEBUG_FORMATION_STRING() . ","
			this.DEBUG_LOG_STRING.="DM Level:" . g_SF.Memory.ReadChampLvlByID(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM) . "`n"
		}

		DEBUG_FORMATION_STRING()
		{
			size := g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.formation.slots.size.Read()
			if(size <= 0 OR size > 14) ; sanity check, 12 is the max number of concurrent champions possible.
				return "X:[]"
			formation:=":["
			champCount:=0
			loop, %size%
			{
				heroID := g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.formation.slots[A_index - 1].hero.def.ID.Read()
				if (heroID>0)
					champCount++
				else
					heroID:=-1
				formation.=heroID . ";"
			}
			formation:=champCount . formation . "]"
			return formation
		}

		;END DEBUG THINGS

		Start(setMelfMode:=false)
        {
            this.MelfMode:=setMelfMode
			timerFunction := this.CasinoTimer
			this.InitHandler()
            SetTimer, %timerFunction%, 20, 0
			this.Casino() ;Is this useful here?

        }

		InitHandler()
		{
			EK_HANDLER:=g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.HeroHandler.heroes[this.HERO_INDEX_ELLY].effects.effectKeysByHashedKeyName
			EK_HANDLER_SIZE := EK_HANDLER.size.Read()
			loop, %EK_HANDLER_SIZE%
			{
				PARENT_HANDLER:=EK_HANDLER["value", A_Index - 1].List[0].parentEffectKeyHandler
				if (this.EFFECT_KEY_DOMT == PARENT_HANDLER.def.Key.Read())
				{
					this.EFFECT_HANDLER_CARDS:=PARENT_HANDLER.activeEffectHandlers._items.Clone()
					break
				}
			}
			if (this.EFFECT_HANDLER_CARDS)
				this.EFFECT_HANDLER_CARDS.IBM_ReBase() ;Breaks the links with the main memory management structure. This will mean it could (and usually will) become invalid on reset or restart. Doesn't work from the hub but isn't neccesary there as we don't really care about performance
		}

        Stop() ;Note - do not clear EFFECT_HANDLER_CARDS here as it might be needed for Flames-based stack zones
        {
            this.ClearTimers()
        }

        Reset()
        {
			this.ClearTimers() ;Timers must be stopped BEFORE we set any variables, as otherwise the timer functions could 'un-reset' them afterwards
            this.Complete := false
            this.Redraws := 0
			this.UsedUlt := false ;This assumes Reset() will only be called after an adventure resets
			this.MaxRedraws := {0:g_BrivUserSettingsFromAddons["IBM_Casino_Redraws_Base"],1:g_BrivUserSettingsFromAddons["IBM_Casino_Redraws_Melf"]}
            this.GemCardsNeeded := {0:g_BrivUserSettingsFromAddons["IBM_Casino_Target_Base"],1:g_BrivUserSettingsFromAddons["IBM_Casino_Target_Melf"]}
            this.MinCards:={0:g_BrivUserSettingsFromAddons["IBM_Casino_MinCards_Base"],1:g_BrivUserSettingsFromAddons["IBM_Casino_MinCards_Melf"]}
			this.GemCardsNeededInFlight:=g_BrivUserSettingsFromAddons["IBM_Casino_Target_InFlight"]
			this.MelfMode:=false
			this.StatusString:=""
			this.EFFECT_HANDLER_CARDS:="" ;We can't get this yet as Elly's handler won't be available until she is levelled
        }

		ClearTimers()
		{
		    timerFunction := this.CasinoTimer
            SetTimer, %timerFunction%, Off
		}

        Casino()
        {
			if (this.DEBUG)
				this.DEBUG_ELLY_STATUS_CSV("Casino() Timer Handler")
			if (!this.EFFECT_HANDLER_CARDS) ;Check the effect handler has been set up
			{
				this.InitHandler()
				return ;Re-check on next timer tick
			}
			if (g_SF.Memory.ReadResetting() OR g_SF.Memory.ReadCurrentZone() == "" OR this.GetNumCards() == "")
                return
			if (this.UsedUlt AND g_SF.Memory.IBM_GetEllywickUltimateActive()!=1) ;Check for completed ultimate
			{
				this.UsedUlt:=False
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Casino() cleared ultimate block")
			}
            if (this.Complete AND !this.UsedUlt) ;In flight re-roll checks. Order of lazy ANDs matters to avoid calling CanUseEllyWickUlt() every tick
            {
                if (this.RedrawsLeft() AND this.ShouldDrawMoreCards() AND this.ShouldRedraw() AND this.CanUseEllyWickUlt()) ;When we exit the waitroom early we might still need to do a re-roll
					this.UseEllywickUlt()
				else if (this.GetNumGemCards() < this.GemCardsNeededInFlight AND this.GetNumCards() == 5 AND this.CanUseEllyWickUlt()) ; Use ultimate to redraw cards if Ellywick doesn't have GemCardsNeededInFlight (due to maxRedraws being less than the maximum possible)
                    this.UseEllywickUlt()
            }
            else if (this.ShouldDrawMoreCards())
            {
                if (this.RedrawsLeft()) ;Use ultimate if it's not on cooldown and there are redraws left
                {
                     if (!this.UsedUlt AND this.ShouldRedraw())
                        this.UseEllywickUlt()
                }
                else if (this.GetMinCards() == 0 OR (!this.UsedUlt AND this.GetNumCards()>=this.GetMinCards())) ;If we want to release at a certain number of cards we need to wait for the ult to resolve to be able to count correctly
                    this.WaitRoomExit()
            }
            else
				this.WaitRoomExit()
        }

		WaitRoomExit() ;Seperate so we can put some status strings in here
		{
			this.Complete:=true
			if (this.GetNumGemCards() >= this.GemCardsNeededInFlight) ;If we've reached our in-flight re-roll target in the waitroom there is no reason to keep the timer running
				this.Stop()
		}

		GetMinCards()
		{
			return this.MinCards[this.melfMode]
		}

        DrawsLeft()
        {
			return 5 - this.GetNumCards()
        }

        RedrawsLeft()
        {
			return this.MaxRedraws[this.melfMode] - this.Redraws
        }

        ShouldDrawMoreCards()
        {
            if (this.GetNumCards() < this.GetMinCards())
                return true
            return this.GetNumGemCards() < this.GemCardsNeeded[this.melfMode]
        }

        ShouldRedraw()
        {
            numCards := this.GetNumCards()
            if (numCards == 5)
                return true
            else if (numCards == 0)
                return false
            return this.DrawsLeft() < this.GemCardsNeeded[this.melfMode] - this.GetNumGemCards()
        }

        GetNumCards()
        {
            size := this.EFFECT_HANDLER_CARDS.cardsInHand.size.Read()
            if (size == "" AND this.IsEllyWickOnTheField())
            {
				this.StatusString.="FAIL-GetNumCards() was empty & Elly(Level:" . g_SF.Memory.ReadChampLvlByID(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY) . "):"
            }
            return size == "" ? 0 : size
        }

        GetNumGemCards()
        {
            return this.GetNumCardsOfType(3)
        }

        GetNumCardsOfType(cardType:=3) ;3 is Gem, 5 is Flames
        {
            numCards := 0
			loop, % this.EFFECT_HANDLER_CARDS.cardsInHand.size.Read()
			{
				if (cardType==this.EFFECT_HANDLER_CARDS.cardsInHand[A_index - 1].CardType.Read())
					numCards++
			}
            return numCards
        }

        UseEllywickUlt()
        {
			if (this.DEBUG)
				this.DEBUG_ELLY_STATUS_CSV("UseEllywickUlt() start")
			if (g_SF.Memory.ReadTransitioning()) ;Do not try using the ults during a transition - possible source of Weird Stuff
			{
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("UseEllywickUlt() aborted due to transition")
				return
			}
			if (this.CanUseEllyWickUlt())
            {
                if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Can use ult")
				this.UsedUlt := true ;Set here to block-double presses, until we can confirm it has / hasn't been used
				Critical On ;Champion levelling between reading the ultimate key and pressing it could cause the incorrect button to be pressed
				startTime:=A_TickCount
				elapsedTime:=0
				ultActivated:=false
				retryCount:=g_SF.Memory.IBM_UseUltimate(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY)
				if (retryCount=="")
					this.StatusString.="FAIL-Elly ultButton returned empty:"
				Critical Off
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Post-input; pre-loop")
				while (!ultActivated AND elapsedTime < IC_BrivMaster_EllywickDealer_Class.ULTIMATE_RESOLUTION_TIME)
				{
					Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
					if (this.DEBUG)
						this.DEBUG_ELLY_STATUS_CSV("ultActivated loop post-sleep " . elapsedTime . "ms")
					ultActivated:=g_SF.Memory.IBM_GetEllywickUltimateActive() ;Specific read for Elly from her handler, seemed more reliable than the cooldown
					if (ultActivated=="") ;This should be 0 or 1, if we fail to get a value something has gone weird
						this.StatusString.="FAIL-Elly ultActivated was empty:"
					elapsedTime:=A_TickCount - startTime
				}
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Exited loop after " . elapsedTime)
                If (ultActivated)
				{
					this.Redraws++
					if (this.DEBUG)
						this.DEBUG_ELLY_STATUS_CSV("Redraws incremented-calling DM")
					this.UseDMUlt()
				}
				else
				{
					this.UsedUlt:=False
					this.StatusString.="FAIL-Elly Ult Attempted but failed to register:"
					if (this.DEBUG)
						this.DEBUG_ELLY_STATUS_CSV("Ult was attempted but failed to register")
				}
            }
			else
			{
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Can't use ult")
				if (this.CanUseDMUlt()) ;Somehow Elly's ult isn't ready by DM's is - try using it
				{
					this.StatusString.="FAIL-Elly Ult not available-DM available:"
					if (this.DEBUG)
						this.DEBUG_ELLY_STATUS_CSV("Can't use ult-DM available-calling")
					this.UseDMUlt(0)
				}
				else
				{
					this.StatusString.="FAIL-Elly(Level:" . g_SF.Memory.ReadChampLvlByID(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY) . ") Ult not available-DM(Level:" . g_SF.Memory.ReadChampLvlByID(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM) . ") Ult not available-Lowered Max Rerolls to " . this.Redraws . ":"
					if (this.DEBUG)
						this.DEBUG_ELLY_STATUS_CSV("Can't use ult-DM not available-lowering max reroll from:" . this.MaxRedraws[this.melfMode] . " to: " . this.Redraws)
					this.MaxRedraws[this.melfMode]:=this.Redraws ;Lower max re-rolls so we move on; this Casino is busted
				}
			}
        }

        CanUseEllyWickUlt()
        {
            return this.IsEllyWickOnTheField() AND this.IsEllywickUltReady()
        }

		IsEllyWickOnTheField()
        {
            return g_SF.Memory.IBM_IsChampInCurrentFormation(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY)
        }

		IsEllywickUltReady()
        {
			ultCooldown:=g_SF.Memory.IBM_GetUltimateCooldown(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY)
			if (ultCooldown=="")
				this.StatusString.="FAIL-IsEllywickUltReady() ultCooldown returned empty:"
			return ultCooldown <= 0
        }

        UseDMUlt(sleepTime:=30) ;30ms default sleep is for use after Elly's ult triggers, to let the game process it
        {
			if (this.DEBUG)
				this.DEBUG_ELLY_STATUS_CSV("UseDMUlt() start")
			if (this.CanUseDMUlt())
            {
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Can use DM ult; sleeping for " . sleepTime . "ms")
				Sleep sleepTime
				Critical On ;Champion levelling between reading the ultimate key and pressing it could cause the incorrect button to be pressed
				startTime:=A_TickCount
				elapsedTime:=0
				ultActivated:=false
				retryCount:=g_SF.Memory.IBM_UseUltimate(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM)
				if (retryCount=="")
					this.StatusString.="FAIL-DM ultButton returned empty:"
				Critical Off
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Post-input; pre-loop")
				while (!ultActivated AND elapsedTime < IC_BrivMaster_EllywickDealer_Class.ULTIMATE_RESOLUTION_TIME)
				{
					Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
					if (this.DEBUG)
						this.DEBUG_ELLY_STATUS_CSV("ultActivated loop post-sleep " . elapsedTime . "ms")
					ultCooldown:=g_SF.Memory.IBM_GetUltimateCooldown(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM)
					if (ultCooldown=="")
						this.StatusString.="FAIL-DM ultCooldown returned empty:"
					ultActivated:=ultCooldown > 0
					elapsedTime:=A_TickCount - startTime
				}
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Exited loop after " . elapsedTime . "ms")
            }
			else
			{
				if (this.DEBUG)
					this.DEBUG_ELLY_STATUS_CSV("Can't use DM ult")
			}
        }

        CanUseDMUlt()
        {
            return this.IsDMOnTheField() AND this.IsDMUltReady()
        }

		IsDMOnTheField()
        {
            return g_SF.Memory.IBM_IsChampInCurrentFormation(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM)
        }

        IsDMUltReady()
        {
            ultCooldown:=g_SF.Memory.IBM_GetUltimateCooldown(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM)
			if (ultCooldown=="")
				this.StatusString.="FAIL-IsDMUltReady() ultCooldown returned empty:"
			return ultCooldown <= 0
        }

    }

class IC_BrivMaster_EllywickDealer_NonFarm_Class extends IC_BrivMaster_EllywickDealer_Class
{
	__New(minCards,maxCards)
	{
		this.minCards := minCards ;These are arrays indexed by card type, so 1 is Knight, 2 Moon, 3 Gem, 4 Fates, 5 Flames
		this.maxCards := maxCards
		this.inputManager:=new IC_BrivMaster_InputManager_Class()
		this.HERO_INDEX_ELLY:=g_SF.Memory.GetHeroHandlerIndexByChampID(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY) ;Only needs to be done once
	}

	Casino()
	{
		if (!this.EFFECT_HANDLER_CARDS) ;Check the effect handler has been set up
		{
			this.InitHandler()
			return ;Re-check on next timer tick
		}
		if (g_SF.Memory.ReadResetting() || g_SF.Memory.ReadCurrentZone() == "" || this.GetNumCards() == "")
			return
		if (this.UsedUlt AND g_SF.Memory.IBM_GetEllywickUltimateActive()!=1) ;Check for completed ultimate
			{
				this.UsedUlt:=False
			}
		remaining := this.GetRemainingCardsToDraw()
		withinMax:=this.CheckWithinMax()
		if (remaining == 0 AND this.GetNumCards() == 5 AND withinMax) ;We're done
		{
			this.WaitedForEllywickThisRun := true
			g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Complete after " . this.Redraws . " redraws")
			this.Stop()
		}
		else if (this.DrawsLeft() < remaining or !withinMax) ;Need to re-roll
		{
			If (this.CanUseEllyWickUlt() AND !this.UsedUlt)
			{
				g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Using ultimate")
				this.UseEllywickUlt()
			}
			else
				g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Waiting for ultimate")
		}
		else
			g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Drawing Cards")
	}

	GetRemainingCardsToDraw() ;Check the minimums to determine if we need to draw more
	{
		num := 0
		for cardType, numCards in this.minCards
		   num += Max(0, numCards - this.GetNumCardsOfType(cardType))
		return num
	}

	CheckWithinMax() ;Check the maximums have not been exceeded, this is a pass/fail
	{
		for cardType, maxCards in this.maxCards
		{
			if (this.GetNumCardsOfType(cardType) > maxCards)
				return False
		}
		return True
	}

	Start() ;Overriden as we might need to get the process when launching this way
	{
		g_SF.Hwnd := WinExist("ahk_exe " . g_UserSettings[ "ExeName"])
		existingProcessID := g_UserSettings[ "ExeName"]
		Process, Exist, %existingProcessID%
		g_SF.PID := ErrorLevel
		g_SF.Memory.OpenProcessReader()
		timerFunction := this.CasinoTimer
		SetTimer, %timerFunction%, 20, 0
		this.InitHandler()
		this.Casino()

	}

	UseEllywickUlt()
        {
			if (g_SF.Memory.ReadTransitioning()) ;Do not try using the ults during a transition - possible source of Weird Stuff
			{
				return
			}
			if (this.CanUseEllyWickUlt())
            {
				this.UsedUlt := true ;Set here to block-double presses, until we can confirm it has / hasn't been used
				Critical On ;Champion levelling between reading the ultimate key and pressing it could cause the incorrect button to be pressed
				ultButton := g_SF.GetUltimateButtonByChampID(IC_BrivMaster_EllywickDealer_Class.HERO_ID_ELLY)
				ultKey:=this.inputManager.getKey(ultButton)
				startTime:=A_TickCount
				elapsedTime:=0
				ultActivated:=false
				ultKey.KeyPress()
				Critical Off
				while (!ultActivated AND elapsedTime < IC_BrivMaster_EllywickDealer_Class.ULTIMATE_RESOLUTION_TIME)
				{
					Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
					ultActivated:=g_SF.Memory.IBM_GetEllywickUltimateActive() ;No check for empty in this version as we can't do much about it (no log)
					elapsedTime:=A_TickCount - startTime
				}
                If (ultActivated)
				{
					this.Redraws++
					this.UseDMUlt()
				}
				else
				{
					this.UsedUlt:=False
				}
            }
			else
			{
				if (this.CanUseDMUlt()) ;Somehow Elly's ult isn't ready by DM's is - try using it
				{
					this.UseDMUlt(0)
				}
			}
        }
		
		UseDMUlt(sleepTime:=30) ;30ms default sleep is for use after Elly's ult triggers, to let the game process it
        {
			if (this.CanUseDMUlt())
            {
				Sleep sleepTime
				Critical On ;Champion levelling between reading the ultimate key and pressing it could cause the incorrect button to be pressed
				ultButton := g_SF.GetUltimateButtonByChampID(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM)
				ultKey:=this.inputManager.getKey(ultButton)
				startTime:=A_TickCount
				elapsedTime:=0
				ultActivated:=false
				ultKey.KeyPress()
				Critical Off
				while (!ultActivated AND elapsedTime < IC_BrivMaster_EllywickDealer_Class.ULTIMATE_RESOLUTION_TIME)
				{
					Sleep, IC_BrivMaster_BrivGemFarm_Class.IRI_LOOP_WAIT_FAST
					ultActivated:=g_SF.Memory.IBM_GetUltimateCooldown(IC_BrivMaster_EllywickDealer_Class.HERO_ID_DM) > 0 ;Not checking for empty in this version as nothing much we can do (no log)
					elapsedTime:=A_TickCount - startTime
				}
            }
        }

}

/**
 * Lib: JSON.ahk
 *     JSON lib for AutoHotkey.
 * Version:
 *     v2.1.3 [updated 04/18/2016 (MM/DD/YYYY)]
 * License:
 *     WTFPL [http://wtfpl.net/]
 * Requirements:
 *     Latest version of AutoHotkey (v1.1+ or v2.0-a+)
 * Installation:
 *     Use #Include JSON.ahk or copy into a function library folder and then
 *     use #Include <JSON>
 * Links:
 *     GitHub:     - https://github.com/cocobelgica/AutoHotkey-JSON
 *     Forum Topic - http://goo.gl/r0zI8t
 *     Email:      - cocobelgica <at> gmail <dot> com
 */


/**
 * Class: JSON
 *     The JSON object contains methods for parsing JSON and converting values
 *     to JSON. Callable - NO; Instantiable - YES; Subclassable - YES;
 *     Nestable(via #Include) - NO.
 * Methods:
 *     Load() - see relevant documentation before method definition header
 *     Dump() - see relevant documentation before method definition header
 */
class AHK_JSON ;Irisiri - renamed as SH already has a JSON class powered by JavaScript
{
	/**
	 * Method: Load
	 *     Parses a JSON string into an AHK value
	 * Syntax:
	 *     value := JSON.Load( text [, reviver ] )
	 * Parameter(s):
	 *     value      [retval] - parsed value
	 *     text    [in, ByRef] - JSON formatted string
	 *     reviver   [in, opt] - function object, similar to JavaScript's
	 *                           JSON.parse() 'reviver' parameter
	 */
	class Load extends AHK_JSON.Functor
	{
		Call(self, ByRef text, reviver:="")
		{
			this.rev := IsObject(reviver) ? reviver : false
		; Object keys(and array indices) are temporarily stored in arrays so that
		; we can enumerate them in the order they appear in the document/text instead
		; of alphabetically. Skip if no reviver function is specified.
			this.keys := this.rev ? {} : false

			static quot := Chr(34), bashq := "\" . quot
			     , json_value := quot . "{[01234567890-tfn"
			     , json_value_or_array_closing := quot . "{[]01234567890-tfn"
			     , object_key_or_object_closing := quot . "}"

			key := ""
			is_key := false
			root := {}
			stack := [root]
			next := json_value
			pos := 0

			while ((ch := SubStr(text, ++pos, 1)) != "") {
				if InStr(" `t`r`n", ch)
					continue
				if !InStr(next, ch, 1)
					this.ParseError(next, text, pos)

				holder := stack[1]
				is_array := holder.IsArray

				if InStr(",:", ch) {
					next := (is_key := !is_array && ch == ",") ? quot : json_value

				} else if InStr("}]", ch) {
					ObjRemoveAt(stack, 1)
					next := stack[1]==root ? "" : stack[1].IsArray ? ",]" : ",}"

				} else {
					if InStr("{[", ch) {
					; Check if Array() is overridden and if its return value has
					; the 'IsArray' property. If so, Array() will be called normally,
					; otherwise, use a custom base object for arrays
						static json_array := Func("Array").IsBuiltIn || ![].IsArray ? {IsArray: true} : 0

					; sacrifice readability for minor(actually negligible) performance gain
						(ch == "{")
							? ( is_key := true
							  , value := {}
							  , next := object_key_or_object_closing )
						; ch == "["
							: ( value := json_array ? new json_array : []
							  , next := json_value_or_array_closing )

						ObjInsertAt(stack, 1, value)

						if (this.keys)
							this.keys[value] := []

					} else {
						if (ch == quot) {
							i := pos
							while (i := InStr(text, quot,, i+1)) {
								value := StrReplace(SubStr(text, pos+1, i-pos-1), "\\", "\u005c")

								static tail := A_AhkVersion<"2" ? 0 : -1
								if (SubStr(value, tail) != "\")
									break
							}

							if (!i)
								this.ParseError("'", text, pos)

							  value := StrReplace(value,  "\/",  "/")
							, value := StrReplace(value, bashq, quot)
							, value := StrReplace(value,  "\b", "`b")
							, value := StrReplace(value,  "\f", "`f")
							, value := StrReplace(value,  "\n", "`n")
							, value := StrReplace(value,  "\r", "`r")
							, value := StrReplace(value,  "\t", "`t")

							pos := i ; update pos

							i := 0
							while (i := InStr(value, "\",, i+1)) {
								if !(SubStr(value, i+1, 1) == "u")
									this.ParseError("\", text, pos - StrLen(SubStr(value, i+1)))

								uffff := Abs("0x" . SubStr(value, i+2, 4))
								if (A_IsUnicode || uffff < 0x100)
									value := SubStr(value, 1, i-1) . Chr(uffff) . SubStr(value, i+6)
							}

							if (is_key) {
								key := value, next := ":"
								continue
							}

						} else {
							value := SubStr(text, pos, i := RegExMatch(text, "[\]\},\s]|$",, pos)-pos)

							static number := "number", integer :="integer"
							if value is %number%
							{
								if value is %integer%
									value += 0
							}
							else if (value == "true" || value == "false")
								value := %value% + 0
							else if (value == "null")
								value := ""
							else
							; we can do more here to pinpoint the actual culprit
							; but that's just too much extra work.
								this.ParseError(next, text, pos, i)

							pos += i-1
						}

						next := holder==root ? "" : is_array ? ",]" : ",}"
					} ; If InStr("{[", ch) { ... } else

					is_array? key := ObjPush(holder, value) : holder[key] := value

					if (this.keys && this.keys.HasKey(holder))
						this.keys[holder].Push(key)
				}

			} ; while ( ... )

			return this.rev ? this.Walk(root, "") : root[""]
		}

		ParseError(expect, ByRef text, pos, len:=1)
		{
			static quot := Chr(34), qurly := quot . "}"

			line := StrSplit(SubStr(text, 1, pos), "`n", "`r").Length()
			col := pos - InStr(text, "`n",, -(StrLen(text)-pos+1))
			msg := Format("{1}`n`nLine:`t{2}`nCol:`t{3}`nChar:`t{4}"
			,     (expect == "")     ? "Extra data"
			    : (expect == "'")    ? "Unterminated string starting at"
			    : (expect == "\")    ? "Invalid \escape"
			    : (expect == ":")    ? "Expecting ':' delimiter"
			    : (expect == quot)   ? "Expecting object key enclosed in double quotes"
			    : (expect == qurly)  ? "Expecting object key enclosed in double quotes or object closing '}'"
			    : (expect == ",}")   ? "Expecting ',' delimiter or object closing '}'"
			    : (expect == ",]")   ? "Expecting ',' delimiter or array closing ']'"
			    : InStr(expect, "]") ? "Expecting JSON value or array closing ']'"
			    :                      "Expecting JSON value(string, number, true, false, null, object or array)"
			, line, col, pos)

			static offset := A_AhkVersion<"2" ? -3 : -4
			throw Exception(msg, offset, SubStr(text, pos, len))
		}

		Walk(holder, key)
		{
			value := holder[key]
			if IsObject(value) {
				for i, k in this.keys[value] {
					; check if ObjHasKey(value, k) ??
					v := this.Walk(value, k)
					if (v != AHK_JSON.Undefined)
						value[k] := v
					else
						ObjDelete(value, k)
				}
			}

			return this.rev.Call(holder, key, value)
		}
	}

	/**
	 * Method: Dump
	 *     Converts an AHK value into a JSON string
	 * Syntax:
	 *     str := JSON.Dump( value [, replacer, space ] )
	 * Parameter(s):
	 *     str        [retval] - JSON representation of an AHK value
	 *     value          [in] - any value(object, string, number)
	 *     replacer  [in, opt] - function object, similar to JavaScript's
	 *                           JSON.stringify() 'replacer' parameter
	 *     space     [in, opt] - similar to JavaScript's JSON.stringify()
	 *                           'space' parameter
	 */
	class Dump extends AHK_JSON.Functor
	{
		Call(self, value, replacer:="", space:="")
		{
			this.rep := IsObject(replacer) ? replacer : ""

			this.gap := ""
			if (space) {
				static integer := "integer"
				if space is %integer%
					Loop, % ((n := Abs(space))>10 ? 10 : n)
						this.gap .= " "
				else
					this.gap := SubStr(space, 1, 10)

				this.indent := "`n"
			}

			return this.Str({"": value}, "")
		}

		Str(holder, key)
		{
			value := holder[key]

			if (this.rep)
				value := this.rep.Call(holder, key, ObjHasKey(holder, key) ? value : JSON.Undefined)

			if IsObject(value) {
			; Check object type, skip serialization for other object types such as
			; ComObject, Func, BoundFunc, FileObject, RegExMatchObject, Property, etc.
				static type := A_AhkVersion<"2" ? "" : Func("Type")
				if (type ? type.Call(value) == "Object" : ObjGetCapacity(value) != "") {
					if (this.gap) {
						stepback := this.indent
						this.indent .= this.gap
					}

					is_array := value.IsArray
				; Array() is not overridden, rollback to old method of
				; identifying array-like objects. Due to the use of a for-loop
				; sparse arrays such as '[1,,3]' are detected as objects({}).
					if (!is_array) {
						for i in value
							is_array := i == A_Index
						until !is_array
					}

					str := ""
					if (is_array) {
						Loop, % value.Length() {
							if (this.gap)
								str .= this.indent

							v := this.Str(value, A_Index)
							str .= (v != "") ? v . "," : "null,"
						}
					} else {
						colon := this.gap ? ": " : ":"
						for k in value {
							v := this.Str(value, k)
							if (v != "") {
								if (this.gap)
									str .= this.indent

								str .= this.Quote(k) . colon . v . ","
							}
						}
					}

					if (str != "") {
						str := RTrim(str, ",")
						if (this.gap)
							str .= stepback
					}

					if (this.gap)
						this.indent := stepback

					return is_array ? "[" . str . "]" : "{" . str . "}"
				}

			} else ; is_number ? value : "value"
				return ObjGetCapacity([value], 1)=="" ? value : this.Quote(value)
		}

		Quote(string)
		{
			static quot := Chr(34), bashq := "\" . quot

			if (string != "") {
				  string := StrReplace(string,  "\",  "\\")
				; , string := StrReplace(string,  "/",  "\/") ; optional in ECMAScript
				, string := StrReplace(string, quot, bashq)
				, string := StrReplace(string, "`b",  "\b")
				, string := StrReplace(string, "`f",  "\f")
				, string := StrReplace(string, "`n",  "\n")
				, string := StrReplace(string, "`r",  "\r")
				, string := StrReplace(string, "`t",  "\t")

				static rx_escapable := A_AhkVersion<"2" ? "O)[^\x20-\x7e]" : "[^\x20-\x7e]"
				while RegExMatch(string, rx_escapable, m)
					string := StrReplace(string, m.Value, Format("\u{1:04x}", Ord(m.Value)))
			}

			return quot . string . quot
		}
	}

	/**
	 * Property: Undefined
	 *     Proxy for 'undefined' type
	 * Syntax:
	 *     undefined := JSON.Undefined
	 * Remarks:
	 *     For use with reviver and replacer functions since AutoHotkey does not
	 *     have an 'undefined' type. Returning blank("") or 0 won't work since these
	 *     can't be distnguished from actual JSON values. This leaves us with objects.
	 *     Replacer() - the caller may return a non-serializable AHK objects such as
	 *     ComObject, Func, BoundFunc, FileObject, RegExMatchObject, and Property to
	 *     mimic the behavior of returning 'undefined' in JavaScript but for the sake
	 *     of code readability and convenience, it's better to do 'return JSON.Undefined'.
	 *     Internally, the property returns a ComObject with the variant type of VT_EMPTY.
	 */
	Undefined[]
	{
		get {
			static empty := {}, vt_empty := ComObject(0, &empty, 1)
			return vt_empty
		}
	}

	class Functor
	{
		__Call(method, ByRef arg, args*)
		{
		; When casting to Call(), use a new instance of the "function object"
		; so as to avoid directly storing the properties(used across sub-methods)
		; into the "function object" itself.
			if IsObject(method)
				return (new this).Call(method, arg, args*)
			else if (method == "")
				return (new this).Call(arg, args*)
		}
	}
}