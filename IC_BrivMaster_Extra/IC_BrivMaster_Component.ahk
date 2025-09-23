#include *i %A_LineFile%\..\IC_BrivMaster_Functions.ahk
#include *i %A_LineFile%\..\IC_BrivMaster_Overrides.ahk
#include *i %A_LineFile%\..\IC_BrivMaster_Overrides_SF.ahk
#include *i %A_LineFile%\..\IC_BrivMaster_Overrides_GF.ahk

; Naming convention in Script Hub is that simple global variables should start with ``g_`` to make it easy to know that a global variable is what is being used.
IC_BrivMaster_SharedFunctions_Class.InjectAddon()
global g_IriBrivMaster := new IC_IriBrivMaster_Component
global g_IriBrivMaster_GUI := new IC_IriBrivMaster_GUI
SH_UpdateClass.UpdateClassFunctions(GameObjectStructure, IC_BrivMaster_GameObjectStructure) ;Required so that the Ellywick tool can work in the same way as the main script. TODO: Might not be needed if Aug25 SH update is applied and has built-in methods for this
SH_UpdateClass.UpdateClassFunctions(g_SF.Memory, IC_BrivMaster_MemoryFunctions_Class) ;Make memory overrides available as well
g_IriBrivMaster.Init()


Class IC_IriBrivMaster_Component
{
	Settings := ""
	TimerFunction := ObjBindMethod(this, "UpdateStatus")
	ChestSnatcherTimer := ObjBindMethod(this, "ChestSnatcher")
	SharedRunData:=""
	CONSTANT_serverRateOpen:=1000 ;For chests
	CONSTANT_serverRateBuy:=250
	ServerCallFailCount:=0 ;Track the number of failed calls, so we can refresh the user data / servercall, but avoid doing so because one call happened to fail (e.g. at 20:00 UK the new game day starting tends to result in fails)
	MemoryReadFailCount:=0 ;Separate tracker for memory reads, as these are expected to fail during resets etc (TODO: We could combine and just add different numbers, e.g. 5 for a call fail or 1 for a memory read fail?)
	CONSTANT_goldCost:=500
	CONSTANT_silverCost:=50

	Init()
    {
        ; Read settings
		g_SF.Memory.GetChampIDToIndexMap() ;This is normally in the effect key handler, which is unhelpful for us, so having to call manually. TODO: Put somewhere sensible
		g_IriBrivMaster_GUI.Init()
        this.LoadSettings()
		this.ResetStats() ;Before we initiate the timers
		g_BrivFarmAddonStartFunctions.Push(ObjBindMethod(this, "Start"))
        g_BrivFarmAddonStopFunctions.Push(ObjBindMethod(this, "Stop"))
		this.NextDailyClaimCheck:=A_TickCount + 300000 ;Wait 5min before making the first check, avoid spamming calls whilst testing things
		this.ServerCallFailCount:=0
		this.MemoryReadFailCount:=0
		this.ChestSnatcher_Messages:={}
		this.GameSettingFileLocation:=""
		this.NextGameSettingsCheck:=A_TickCount + 60000 ;Wait 1min, as we'll likely be starting the script right away which will check for us
		this.CurrentGems:=0 ;Gem/Chest data used over multiple elements of this class
		this.Chests:={}
		this.Chests.CurrentSilver:=0
		this.Chests.CurrentGold:=0
		this.Chests.OpenedSilver:=0
		this.Chests.OpenedGold:=0
		this.Chests.OpenedSilver:=0
		this.Chests.OpenedGold:=0
    }

	; Returns an object with default values for all settings.
    GetNewSettings()
    {
        settings := {}
        settings.IBM_Chest_UseSmart := true
        settings.IBM_Chests_TimePercent := 90
        settings.IBM_Offline_Stack_Zone:=350
		settings.IBM_OffLine_Flames_Use := false
        settings.IBM_OffLine_Flames_Zones := [g_BrivUserSettings[ "StackZone" ],g_BrivUserSettings[ "StackZone" ],g_BrivUserSettings[ "StackZone" ],g_BrivUserSettings[ "StackZone" ],g_BrivUserSettings[ "StackZone" ]]
		settings.IBM_Route_Combine := 0
		settings.IBM_Route_Combine_Boss_Avoidance := 1
		settings.IBM_DailyRewardClaim_Enable := false
        settings.IBM_LevelManager_Levels[0,58,"z1"]:=200
        settings.IBM_LevelManager_Levels[0,58,"min"]:=200
        settings.IBM_LevelManager_Levels[1,58,"z1"]:=200
        settings.IBM_LevelManager_Levels[1,58,"min"]:=200
		settings.IBM_Route_Zones_Jump:=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
		settings.IBM_Route_Zones_Stack:=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
		settings.IBM_Online_Use_Melf:=false
		settings.IBM_Online_Melf_Min:=349
		settings.IBM_Online_Melf_Max:=800
		settings.IBM_Online_Ultra_Enabled:=false
		settings.IBM_LevelManager_Defaults_Min:=false ;False is the lower option
		settings.IBM_LevelManager_Input_Max:=5
		settings.IBM_LevelManager_Boost_Use:=false
		settings.IBM_LevelManager_Boost_Multi:=8
		settings.IBM_Route_BrivJump_Q:=4
		settings.IBM_Route_BrivJump_E:=0
		settings.IBM_Route_BrivJump_M:=4
		settings.IBM_Casino_Target_Melf:=3
		settings.IBM_Casino_Redraws_Melf:=1
		settings.IBM_Casino_MinCards_Melf:=0
		settings.IBM_Casino_Target_Base:=3
		settings.IBM_Casino_Redraws_Base:=1
		settings.IBM_Casino_MinCards_Base:=0
		settings.IBM_Casino_Target_InFlight:=1
		settings.IBM_OffLine_Delay_Time:=15000
		settings.IBM_Level_Options_Mod_Key:="Shift"
		settings.IBM_Level_Options_Mod_Value:=10
		settings.IBM_Route_Offline_Restore_Window:=true
		settings.IBM_OffLine_Freq:=1
		settings.IBM_OffLine_Blank:=0
		settings.IBM_OffLine_Blank_Relay:=0
		settings.IBM_OffLine_Blank_Relay_Zones:=400
		settings.IBM_Level_Options_Limit_Tatyana:=false
		settings.IBM_Level_Options_Suppress_Front:=true
		settings.IBM_Level_Options_Ghost:=true
		settings.IBM_ChestSnatcher_Options_Min_Gold:=500
		settings.IBM_ChestSnatcher_Options_Min_Silver:=500
		settings.IBM_ChestSnatcher_Options_Min_Buy:=250
		settings.IBM_ChestSnatcher_Options_Estimate_Gold:=10
		settings.IBM_ChestSnatcher_Options_Estimate_Silver:=3
		settings.IBM_Game_Settings_Option_Profile:=1
		settings.IBM_Game_Settings_Option_Set:={1:{Name:"Profile 1",Framerate:600,Particles:0,HRes:1920,VRes:1080,Fullscreen:false,CapFPSinBG:false,SaveFeats:false,ConsolePortraits:false,NarrowHero:true,AllHero:true,Swap25100:false},2:{Name:"Profile 2",Framerate:600,Particles:0,HRes:1920,VRes:1080,Fullscreen:false,CapFPSinBG:false,SaveFeats:false,ConsolePortraits:false,NarrowHero:true,AllHero:true,Swap25100:false}}
        return settings
    }

	SaveSettings()
    {
        settings := this.Settings
        g_SF.WriteObjectToJSON(IC_BrivMaster_SharedData_Class.SettingsPath, settings)
        ; Apply settings to BrivGemFarm
		if (ComObjType(this.SharedRunData,"IID") or this.RefreshComObject())
		{
			this.SharedRunData.IBM_UpdateSettingsFromFile()
		}
    }

	RefreshComObject()
	{
		try ; avoid thrown errors when comobject is not available.
		{
			this.SharedRunData := ComObjActive(g_BrivFarm.GemFarmGUID)
		}
		catch
		{
			this.SharedRunData:=""
			return false
		}
		return true
	}

	Start()
    {
        g_SF.ResetServerCall() ;The process reader should have been set up by the main the script, but it doesn't instantiate the g_ServerCall object
		fncToCallOnTimer := this.TimerFunction
        SetTimer, %fncToCallOnTimer%, 500, 0
		this.SharedRunData:="" ;Reset this on start
		if (this.RefreshComObject())
        {
			this.SharedRunData.IBM_SmartChests_Time:=0 ;Cancel any orders open as the hub starts
        }
		fncToCallOnTimer := this.ChestSnatcherTimer
        SetTimer, %fncToCallOnTimer%, 450, 0
		this.ChestSnatcher_AddMessage("General","Awaiting first order")
		this.SoftResetStats() ;Soft reset so we don't discard totals etc but also don't pick up a part run
		this.UpdateStatus(true) ;Force an update so the flags don't get left on the default if that happens to match the script default
		this.GameSettingsCheck()
    }

    Stop()
    {
        fncToCallOnTimer := this.TimerFunction
        SetTimer, %fncToCallOnTimer%, Off
		fncToCallOnTimer := this.ChestSnatcherTimer
        SetTimer, %fncToCallOnTimer%, Off
		g_IriBrivMaster_GUI.ResetStatusText()
    }

	SoftResetStats()
	{
		this.Stats.StartUpStage:=0
		this.Stats.LastRun:=-1
		
		this.Stats.StacksSB:=""
		this.Stats.StacksHaste:=""
		
		this.Stats.PreviousRunEndTime:=""
		this.Stats.PreviousZoneStartTime:=""
		this.Stats.LastZone:=""
	}
	
	ResetStats()
	{
		this.Stats:={}

		this.SoftResetStats()

		this.Stats.Total:={}
		this.Stats.Total.Fast:=""
		this.Stats.Total.Slow:=""
		this.Stats.Total.TotalTime:=0
		this.Stats.Active:={}
		this.Stats.Active.Fast:=""
		this.Stats.Active.Slow:=""
		this.Stats.Active.TotalTime:=0
		this.Stats.Reset:={}
		this.Stats.Reset.Fast:=""
		this.Stats.Reset.Slow:=""
		this.Stats.Reset.TotalTime:=0
		
		this.Stats.FailTotalTime:=0 ;We could add the rest to this?

		this.Stats.TotalRuns:=0
		this.Stats.FailRuns:=0
		this.Stats.BossKills:=0
		this.Stats.StartTime:=""

		this.Stats.Chests:={}
		this.Stats.Chests.SilverStart:=""
		this.Stats.Chests.GoldStart:=""
		this.Stats.StartGems:=""

		this.Stats.GHActive:=-1 ;-1=not set, 0=Seen only inactive, 1=Seen only active, 2=Seen both
		
		GuiControl, ICScriptHub:, IBM_Stats_Group, % "Run Stats"
		GuiControl, -Redraw, IBM_Stats_Run_LV
		Gui, ICScriptHub:Default
		Gui, ListView, IBM_Stats_Run_LV
		LV_Modify(1,,"Total","--.--","--.--","--.--","--.--")
		LV_Modify(2,,"Active","--.--","--.--","--.--","--.--")
		LV_Modify(3,,"Reset","--.--","--.--","--.--","--.--")
		LV_ModifyCol(1,"AutoHdr")
		LV_ModifyCol(2,"AutoHdr")
		LV_ModifyCol(3,"AutoHdr")
		LV_ModifyCol(4,"AutoHdr")
		LV_ModifyCol(5,"AutoHdr")
		GuiControl, +Redraw, IBM_Stats_Run_LV
		GuiControl, ICScriptHub:, IBM_Stats_Total_Runs, 0
		GuiControl, ICScriptHub:, IBM_Stats_Total_Time, 0s (0h)
		GuiControl, ICScriptHub:, IBM_Stats_Fail_Runs, 0
		GuiControl, ICScriptHub:, IBM_Stats_Fail_Time, 0s
		GuiControl, ICScriptHub:, IBM_Stats_Chests, Gold: - / - / - Silver: - / - / -
		
		GuiControl, ICScriptHub:, IBM_Stats_BPH, BPH: --.-- 
		GuiControl, ICScriptHub:, IBM_Stats_GPH, GPH: --.--
		GuiControl, ICScriptHub:, IBM_Stats_TotalGems, 0
		GuiControl, ICScriptHub:, IBM_Stats_GPB, -.-
		GuiControl, ICScriptHub:, IBM_Stats_Gem_Bonus, -.-`%
		GuiControl, ICScriptHub:, IBM_Stats_BSC_Reward, --.-- (Bosses: --.--, Gems: --.--)
		GuiControl, ICScriptHub:, IBM_Stats_Total_Reward, --.-- (Bosses: --.--, Gems: --.--)
		GuiControl, ICScriptHub:+cBlack, IBM_Stats_Gem_Hunter
		GuiControl, ICScriptHub:MoveDraw,IBM_Stats_Gem_Hunter ;Required to update the colour as we don't change the text
		
		GuiControl, ICScriptHub:, IBM_Stats_Current_Area_Run_Time, - / -
		GuiControl, ICScriptHub:, IBM_Stats_Loop, -
		GuiControl, ICScriptHub:, IBM_Stats_Current_Briv, - / -
		GuiControl, ICScriptHub:, IBM_Stats_Last_Close, -
		GuiControl, ICScriptHub:, IBM_Stats_Boss_Hits, - / -
        GuiControl, ICScriptHub:, IBM_Stats_Rollbacks, 0
        GuiControl, ICScriptHub:, IBM_Stats_Bad_Auto, 0
	}

	UpdateStats()
	{
		static CONSTANT_baseGPB:=9.02 ;TODO: Read blessings so this correctly reflects those? Seems a bit of a waste of CPU cycles admittedly...just do it at startup? Would then have to deal with the memory read not being available at that point
		static CONSTANT_silversPerBoss:=0.05361
		static CONSTANT_goldPerBoss:=0.00423
		static CONSTANT_BSCPerSilver:=0.10356
		static CONSTANT_BSCPerGold:=1.00522
		static CONSTANT_BountiesPerGold:=167.81838
		static CONSTANT_BountiesPerEventPack:=7500
		static CONSTANT_TotalRewardPerEventPack:=18.63041867

		;Run stats
		if (this.SharedRunData.RunLogResetNumber!=-1) ;-1 means unset by main script, or in the process of updating
		{
			if (this.SharedRunData.RunLogResetNumber!=this.Stats.LastRun)
			{
				if (this.Stats.StartUpStage==0) ;At startup we need to detect a run, then the run changing to the next, which will be the 1st full run of the script, and read gems/chests/etc at that point, but not report until that 1st full run completes; i.e. RunLogResetNumber changes to the 2nd full run
				{
					this.Stats.LastRun:=this.SharedRunData.RunLogResetNumber
					this.Stats.StartUpStage:=1
					GuiControl, ICScriptHub:, IBM_Stats_Group, % "Run Stats (Waiting for first full run to start)"
					LogData:=AHK_JSON.Load(this.SharedRunData.RunLog)
					this.Stats.PreviousRunEndTime:=LogData.Run.End ;Include this so it is available for run timing
				}
				else if (this.Stats.StartUpStage==1) ;The run number has changed to a 2nd real number - this is the start of 1st run we are timing
				{
					this.Stats.LastRun:=this.SharedRunData.RunLogResetNumber
					this.Stats.StartUpStage:=2
					GuiControl, ICScriptHub:, IBM_Stats_Group, % "Run Stats (Waiting for first full run to complete)"
					silvers:=g_SF.Memory.ReadChestCountByID(1)
					if(silvers!="")
					{
						this.Chests.CurrentSilver:=silvers
						this.Stats.Chests.SilverStart:=silvers
					}
					golds:=g_SF.Memory.ReadChestCountByID(2)
					if(golds!="")
					{
						this.Chests.CurrentGold:=golds
						this.Stats.Chests.GoldStart:=golds
					}
					this.Chests.PurchasedSilver:=0	;Reset here as this is the point we are measuring from
					this.Chests.OpenedSilver:=0
					this.Chests.PurchasedGold:=0
					this.Chests.OpenedGold:=0
					gems:=g_SF.Memory.ReadGems()
					if(gems!="")
					{
						this.CurrentGems:=gems
						this.Stats.StartGems:=gems
					}
					LogData:=AHK_JSON.Load(this.SharedRunData.RunLog)
					this.Stats.PreviousRunEndTime:=LogData.Run.End ;Include this so it is available for run timing
				}
				else
				{
					LogData:=AHK_JSON.Load(this.SharedRunData.RunLog)
					this.Stats.LastRun:=LogData.Run.ResetNumber

					totalDuration:=LogData.Run.End - LogData.Run.Start
					activeTime:=LogData.Run.ResetReached - LogData.Run.Start
					resetTime:=LogData.Run.End - LogData.Run.ResetReached
					this.StatsUpdateFastSlow(this.Stats.Total,totalDuration)
					if LogData.Run.HasKey("ResetReached") ;Failed runs may not have a reset value 
					{
						this.StatsUpdateFastSlow(this.Stats.Active,activeTime)
						this.StatsUpdateFastSlow(this.Stats.Reset,resetTime)
					}
					this.Stats.TotalRuns++
					this.Stats.PreviousRunEndTime:=LogData.Run.End
					Gui, ICScriptHub:Default
					Gui, ListView, IBM_Stats_Run_LV
					GuiControl, -Redraw, IBM_Stats_Run_LV
					LV_Modify(1,,"Total",ROUND(totalDuration/1000,2),ROUND((this.Stats.Total.TotalTime/this.Stats.TotalRuns)/1000,2),ROUND(this.Stats.Total.Fast/1000,2),ROUND(this.Stats.Total.Slow/1000,2))
					LV_Modify(2,,"Active",ROUND(activeTime/1000,2),ROUND((this.Stats.Active.TotalTime/this.Stats.TotalRuns)/1000,2),ROUND(this.Stats.Active.Fast/1000,2),ROUND(this.Stats.Active.Slow/1000,2))
					LV_Modify(3,,"Reset",ROUND(resetTime/1000,2),ROUND((this.Stats.Reset.TotalTime/this.Stats.TotalRuns)/1000,2),ROUND(this.Stats.Reset.Fast/1000,2),ROUND(this.Stats.Reset.Slow/1000,2))
					LV_ModifyCol(2,"AutoHdr")
					LV_ModifyCol(3,"AutoHdr")
					LV_ModifyCol(4,"AutoHdr")
					LV_ModifyCol(5,"AutoHdr")
					GuiControl, +Redraw, IBM_Stats_Run_LV
					if (LogData.Run.Fail) ;Failed runs (i.e. ones that did not reach the reset zone)
					{
						this.Stats.FailRuns++
						this.Stats.FailTotalTime+=totalDuration
					}
					if (this.Stats.StartTime=="") ;First run
					{
						this.Stats.StartTime:=LogData.Run.Start
					}
					totalTime:=LogData.Run.End - this.Stats.StartTime
					GuiControl, ICScriptHub:, IBM_Stats_Total_Runs, % this.Stats.TotalRuns
					GuiControl, ICScriptHub:, IBM_Stats_Total_Time, % ROUND(totalTime/1000,2) . "s (" . ROUND(totalTime/3600000,2) . "h)"
					GuiControl, ICScriptHub:, IBM_Stats_Fail_Runs, % this.Stats.FailRuns
					GuiControl, ICScriptHub:, IBM_Stats_Fail_Time, % ROUND(this.Stats.FailTotalTime/1000,2) . "s"

					silvers:=g_SF.Memory.ReadChestCountByID(1)
					if(silvers!="")
					{
						this.Chests.CurrentSilver:=silvers
					}
					golds:=g_SF.Memory.ReadChestCountByID(2)
					if(golds!="")
					{
						this.Chests.CurrentGold:=golds
					}
					silverString:=this.Chests.CurrentSilver - this.Stats.Chests.SilverStart + this.Chests.OpenedSilver - this.Chests.PurchasedSilver . " / " . this.Chests.PurchasedSilver . " / " . this.Chests.OpenedSilver ; Start + Purchased + Dropped - Opened
					goldString:=this.Chests.CurrentGold - this.Stats.Chests.GoldStart + this.Chests.OpenedGold - this.Chests.PurchasedGold . " / " . this.Chests.PurchasedGold . " / " . this.Chests.OpenedGold
					GuiControl, ICScriptHub:, IBM_Stats_Chests, % "Silver: " . silverString . " Gold: " . goldString

					this.Stats.BossKills+=FLOOR(LogData.Run.LastZone / 5)
					bph:=(this.Stats.BossKills / totalTime) * 3600000
					GuiControl, ICScriptHub:, IBM_Stats_BPH, % "BPH: " . ROUND(bph,2) ;Includes the prefix so it can be properly centered
					gems:=g_SF.Memory.ReadGems()
					if(gems!="")
					{
						this.CurrentGems:=gems
					}
					gemsTotal:=this.CurrentGems - this.Stats.StartGems + this.Chests.PurchasedGold*this.CONSTANT_goldCost + this.Chests.PurchasedSilver*this.CONSTANT_silverCost
					gph:=(gemsTotal / totalTime) * 3600000
					GuiControl, ICScriptHub:, IBM_Stats_GPH, % "GPH: " . ROUND(gph,2) ;Includes the prefix so it can be properly centered
					GuiControl, ICScriptHub:, IBM_Stats_TotalGems, % gemsTotal
					;Track GH status
					if (this.Stats.GHActive!=2) ;If already set to 2 the current value no longer matters; we've seen both states
					{
						if (LogData.Run.GHActive)
						{
							if (this.Stats.GHActive==-1) ;Not set yet - set to active
								this.Stats.GHActive:=1
							else if (this.Stats.GHActive==0) ;Been seen inactive - mark as both states
								this.Stats.GHActive:=2
						}
						else
						{
							if (this.Stats.GHActive==-1) ;Not set yet - set to inactive
								this.Stats.GHActive:=0
							else if (this.Stats.GHActive==1) ;Been seen active - mark as both states
								this.Stats.GHActive:=2
						}
					}
					gemMulti:=this.Stats.GHActive>0 ? 1.5 : 1 ;Mixed is processed as active TODO: Is it worth dealing with it dropping off? Doesn't seem like something we need to track
					rawGPB:=gph/bph
					GuiControl, ICScriptHub:, IBM_Stats_GPB, % ROUND(rawGPB/gemMulti,1)
					gemBonus:=(rawGPB/CONSTANT_baseGPB)/gemMulti
					GuiControl, ICScriptHub:, IBM_Stats_Gem_Bonus, % ROUND((gemBonus-1)*100,1) . "%" ;Best expressed as a percentage
					silverChestIncome:=bph*CONSTANT_silversPerBoss
					goldChestIncomeDrops:=bph*CONSTANT_goldPerBoss
					goldChestIncomeGems:=gph/this.CONSTANT_goldCost
					BSCIncomeDrops:=silverChestIncome*CONSTANT_BSCPerSilver + goldChestIncomeDrops * CONSTANT_BSCPerGold
					BSCIncomeGems:=goldChestIncomeGems * CONSTANT_BSCPerGold
					BountyIncomeDrops:=((goldChestIncomeDrops * CONSTANT_BountiesPerGold)/CONSTANT_BountiesPerEventPack)*CONSTANT_TotalRewardPerEventPack
					BountyIncomeGems:=((goldChestIncomeGems * CONSTANT_BountiesPerGold)/CONSTANT_BountiesPerEventPack)*CONSTANT_TotalRewardPerEventPack
					GuiControl, ICScriptHub:, IBM_Stats_BSC_Reward, % ROUND(BSCIncomeDrops+BSCIncomeGems,1) . " (Bosses: " . ROUND(BSCIncomeDrops,1) . ", Gems: " . Round(BSCIncomeGems,1) . ")"
					GuiControl, ICScriptHub:, IBM_Stats_Total_Reward, % ROUND(BSCIncomeDrops+BSCIncomeGems+BountyIncomeDrops+BountyIncomeGems,1) . " (Bosses: " . ROUND(BSCIncomeDrops+BountyIncomeDrops,1) . ", Gems: " . Round(BSCIncomeGems+BountyIncomeGems,1) . ")"
					if (this.Stats.GHActive==0)
						GH_colour:="cRed"
					else if (this.Stats.GHActive==1)
						GH_colour:="cGreen"
					else if (this.Stats.GHActive==2)
						GH_colour:="cFFC000" ;Amber
					else
						GH_colour:="c000000"
					GuiControl, ICScriptHub:+%GH_colour%, IBM_Stats_Gem_Hunter
					GuiControl, ICScriptHub:MoveDraw,IBM_Stats_Gem_Hunter ;Required to update the colour as we don't change the text
					FormatTime, formattedDateTime,, yyyy-MM-ddTHH:mm:ss
					GuiControl, ICScriptHub:, IBM_Stats_Group, % "Run Stats (" . formattedDateTime . ")"
				}
			}
		}
		;Current stats - Run time
        if (this.Stats.PreviousRunEndTime)
			runTime:=A_TickCount - this.Stats.PreviousRunEndTime
		else
			runTime:="-.-"
		;Current stats - Area time
		areaTime:="-.-"
		currentZone:=g_SF.Memory.ReadCurrentZone()
		if (currentZone!="")
		{
			if (this.Stats.LastZone=="" OR currentZone==1) ;Start of run or reset
			{
				if (this.Stats.PreviousRunEndTime) ;If there is an end time for the previous run, use that as a starting point
				{
					this.Stats.PreviousZoneStartTime:=this.Stats.PreviousRunEndTime
					areaTime:=A_TickCount - this.Stats.PreviousRunEndTime
				}
				else
				{
					this.Stats.PreviousZoneStartTime:=A_TickCount
					areaTime:=0 ;Since we just set PreviousZoneStartTime:=A_TickCount
				}
				this.Stats.LastZone:=currentZone
			}
			else if (currentZone > 1 AND currentZone != this.Stats.LastZone) ;New zone
			{
				this.Stats.PreviousZoneStartTime:=A_TickCount
				this.Stats.LastZone:=currentZone
				areaTime:=0 ;Since we just set PreviousZoneStartTime:=A_TickCount
			}
			else
			{
				areaTime:=A_TickCount - this.Stats.PreviousZoneStartTime
			}
		}
		else
			this.MemoryReadFailCount++ ;The zone read is used as the trigger to refresh memory if needed, as it's done every time and should be available outside of a few moments during reset TODO: That isn't so true during offlines (even BrivMaster's ones)
		;Current stats - Steelbones stacks
  		stacks:=g_SF.Memory.ReadSBStacks()
        if (stacks=="") ;If the memory read isn't current
        {
            if (this.Stats.StacksSB=="")
				message_SB:="-"
			else
				message_SB := this.Stats.StacksSB . " [last]"
        }
		else
		{
            this.Stats.StacksSB:=stacks
			message_SB:=this.Stats.StacksSB
        }
		;Current stats - Haste stacks
        stacks:=g_SF.Memory.ReadHasteStacks()
		if (stacks=="") ;If the memory read isn't current
        {
            if (this.Stats.StacksHaste=="")
				message_Haste:="-"
			else
				message_Haste := this.Stats.StacksHaste . " [last]"
        }
		else
		{
            this.Stats.StacksHaste:=stacks
			message_Haste:=this.Stats.StacksHaste
        }
        GuiControl, ICScriptHub:, IBM_Stats_Current_Area_Run_Time, % ROUND(areaTime/1000,1) . " / " . Round(runTime/1000,1)
		GuiControl, ICScriptHub:, IBM_Stats_Loop, % this.SharedRunData.LoopString
		GuiControl, ICScriptHub:, IBM_Stats_Current_Briv, % message_SB . " / " . message_Haste
		GuiControl, ICScriptHub:, IBM_Stats_Last_Close, % this.SharedRunData.LastCloseReason
		;Gem farm stats
		GuiControl, ICScriptHub:, IBM_Stats_Boss_Hits, % this.SharedRunData.BossesHitThisRun . " / " . this.SharedRunData.TotalBossesHit
        GuiControl, ICScriptHub:, IBM_Stats_Rollbacks, % this.SharedRunData.TotalRollBacks
        GuiControl, ICScriptHub:, IBM_Stats_Bad_Auto, % this.SharedRunData.BadAutoProgress
	}
	
	StatsUpdateFastSlow(Stat,statTime) ;Helper for the slow/fast/total stat for each category
	{
		if (!Stat.Slow OR statTime > Stat.Slow)
			Stat.Slow:=statTime
		if (!Stat.Fast OR statTime < Stat.Fast)
			Stat.Fast:=statTime
		Stat.TotalTime+=statTime
	}

	GameSettingsCheck(change:=false) ;Checks settings but does not change them
	{
		this.NextGameSettingsCheck:=A_TickCount + 3600000 ;Hourly check
		checkTime:="(" . A_Hour . ":" . A_Min . ")"
		if (!this.GameSettingFileLocation)
		{
			this.GetSettingsFileLocation(checkTime)
			if (!this.GameSettingFileLocation) ;We tried and we failed
				return
		}
		profile:=this.settings.IBM_Game_Settings_Option_Profile
		gameSettings:=this.LoadObjectFromAHKJSON(this.GameSettingFileLocation)
		changeCount:=0
		this.SettingCheck(gameSettings,"TargetFramerate","Framerate",false,changeCount,change) ;TODO: Just use the CNE names for all the simple ones and loop this?!
		this.SettingCheck(gameSettings,"PercentOfParticlesSpawned","Particles",false,changeCount,change)
		this.SettingCheck(gameSettings,"resolution_x","HRes",false,changeCount,change)
		this.SettingCheck(gameSettings,"resolution_y","VRes",false,changeCount,change)
		this.SettingCheck(gameSettings,"resolution_fullscreen","Fullscreen",true,changeCount,change)
		this.SettingCheck(gameSettings,"ReduceFramerateWhenNotInFocus","CapFPSinBG",true,changeCount,change)
		this.SettingCheck(gameSettings,"FormationSaveIncludeFeatsCheck","SaveFeats",true,changeCount,change)
		this.SettingCheck(gameSettings,"UseConsolePortraits","ConsolePortraits",true,changeCount,change)
		this.SettingCheck(gameSettings,"NarrowHeroBoxes","NarrowHero",true,changeCount,change)
		this.SettingCheck(gameSettings,"ShowAllHeroBoxes","AllHero",true,changeCount,change)
		this.SettingCheck(gameSettings,"HotKeys","Swap25100",false,changeCount,change)
		if (changeCount)
		{
			if (change)
			{
				if (this.IsGameClosed())
				{
					this.WriteObjectToAHKJSON(this.GameSettingFileLocation,gameSettings)
					g_IriBrivMaster_GUI.GameSettings_Status(checkTime . " IC and " . this.settings.IBM_Game_Settings_Option_Set[profile,"Name"] . " aligned with " . (changeCount==1 ? "1 change" : changeCount . " changes"),"cGreen")
				}
				else
				{
					MsgBox,,Game Running,Game settings cannot be changed whilst Idle Champions is running
					g_IriBrivMaster_GUI.GameSettings_Status(checkTime . " IC and " . this.settings.IBM_Game_Settings_Option_Set[profile,"Name"] . " have " . changeCount . (changeCount==1 ? " difference" : " differences"),"cFFC000")
				}

			}
			else
			{
				g_IriBrivMaster_GUI.GameSettings_Status(checkTime . " IC and " . this.settings.IBM_Game_Settings_Option_Set[profile,"Name"] . " have " . changeCount . (changeCount==1 ? " difference" : " differences"),"cFFC000")
			}
		}
		else
		{
			g_IriBrivMaster_GUI.GameSettings_Status(checkTime . " IC and " . this.settings.IBM_Game_Settings_Option_Set[profile,"Name"] . " match","cGreen")
		}
	}

	SettingCheck(gameSettings, CNEName, IBMName,isBoolean, byRef changeCount,change:=false)
	{
		if (IBMName=="Swap25100") ;Special case for the hotkey swap
		{
			if (this.settings.IBM_Game_Settings_Option_Set[this.settings.IBM_Game_Settings_Option_Profile,IBMName]) ;If not using this option we don't care what the user has set them to, so only check in this case
			{
				level25:=gameSettings[CNEName,"hero_level_25"] ;This should be a single-element array ["LeftControl"]
				if !(level25.Count()==1 AND level25[1]=="LeftControl")
				{
					changeCount++
					if (change)
						gameSettings[CNEName,"hero_level_25"]:=["LeftControl"]
				}
				level100:=gameSettings[CNEName,"hero_level_100"] ;This should be a two-element array ["LeftShift","LeftControl"]
				if !(level100.Count()==2 AND ((level100[1]=="LeftShift" AND level100[2]=="LeftControl") OR (level100[1]=="LeftControl" AND level100[2]=="LeftShift"))) ;TODO: Shift,Control is how the game saves it, determine if Control,Shift is actually valid?
				{
					changeCount++
					if (change)
						gameSettings[CNEName,"hero_level_100"]:=["LeftShift","LeftControl"]
				}
			}
			return
		}
		if (isBoolean)
			targetValue:=this.settings.IBM_Game_Settings_Option_Set[this.settings.IBM_Game_Settings_Option_Profile,IBMName]==1 ? "true" : "false"
		else
			targetValue:=this.settings.IBM_Game_Settings_Option_Set[this.settings.IBM_Game_Settings_Option_Profile,IBMName]
		if gameSettings[CNEName]!=targetValue
		{
			changeCount++
			if (change)
				gameSettings[CNEName]:=targetValue
		}
	}

	LoadObjectFromAHKJSON( FileName )
    {
        FileRead, oData, %FileName%
        data := ""
        try
        {
            data := AHK_JSON_RAWBOOLEAN.Load( oData )
        }
        catch err
        {
            err.Message := err.Message . "`nFile:`t" . FileName
            throw err
        }
        return data
    }

    WriteObjectToAHKJSON( FileName, ByRef object )
    {
        objectJSON := AHK_JSON_RAWBOOLEAN.Dump( object,,"`t")
        if (!objectJSON)
            return
        FileDelete, %FileName%
        FileAppend, %objectJSON%, %FileName%
        return
    }

	GetSettingsFileLocation(checkTime)
	{
		installPath := g_UserSettings["InstallPath"] ;Contains filename. For Steam this will be C:\Idle Champions\Idle Champions.exe or so, which allows us to get the path. Other platforms will require a memory read
		SplitPath, installPath, exeName, settingsFileLoc
		settingsFileLoc.="\IdleDragons_Data\StreamingAssets\localSettings.json"
		if (exeName==g_userSettings["ExeName"] AND FileExist(settingsFileLoc)) ;The ExeName check avoids a file system check with no chance of working
		{
			this.GameSettingFileLocation:=settingsFileLoc
		}
		else
		{
			if (this.IsGameClosed()) ;Can't get the memory read
			{
				g_IriBrivMaster_GUI.GameSettings_Status(checkTime . " Unable to read path from memory whilst game is closed","cFF0000")
				return
			}
			webRequestLogLoc:=g_SF.Memory.GetWebRequestLogLocation()
			if (!InStr(webRequestLogLoc, "webRequestLog"))
			{
				g_IriBrivMaster_GUI.GameSettings_Status(checkTime . " Unable to read webRequestLog location","cFF0000")
				return
			}
			settingsFileLoc := StrReplace(webRequestLogLoc, "downloaded_files\webRequestLog.txt", "localSettings.json")
			if (FileExist(settingsFileLoc))
			{
				this.GameSettingFileLocation:=settingsFileLoc
			}
		}
	}

	IsGameClosed()
	{
		return !WinExist("ahk_exe " . g_userSettings[ "ExeName"])
	}

	ChestSnatcher() ;Run on a timer to process chest purchase orders
	{
		if (ComObjType(this.SharedRunData,"IID") or this.RefreshComObject())
		{
			time:=this.SharedRunData.IBM_SmartChests_Time ;This is the number of milliseconds to budget opening. If negative, it ignores save checks, ie +2500 = open for 2500ms after a save, -10000 ms = open for 10000ms without waiting
			If (time!=0) ;Check daily rewards or Open chests
			{
				;OutputDebug % "ChestSnatcher() order placed. NextDailyClaimCheck=[" . this.NextDailyClaimCheck . "] which is=[" . ROUND((this.NextDailyClaimCheck - A_TickCount)/60000,1) . "]min from now`n"
				if (this.settings.IBM_DailyRewardClaim_Enable AND A_TickCount >= this.NextDailyClaimCheck)
				{
					this.ChestSnatcher_ClaimDailyRewards(time)
				}
				else if (g_BrivUserSettings[ "OpenGolds" ] OR g_BrivUserSettings[ "OpenSilvers" ])
				{
					this.ChestSnatcher_Process(time)
				}
				else
					this.SharedRunData.IBM_SmartChests_Time:=0 ;Cancel the order
				;g_SF.TotalGems:=g_SF.Memory.ReadGems() ;Read current gems each time an order is placed, this is something of a proxy for 'every run' - shouldn't be needed as stats updates at the start of each run, and that's where the game will be showing a correct number
			}
			Else ;Buy chests - these can be done at any time
			{
				gems:=this.CurrentGems - g_BrivUserSettings["MinGemCount"]
				amountG:=Min(Floor(gems / this.CONSTANT_goldCost) , this.CONSTANT_serverRateBuy )
				amountS:=Min(Floor(gems / this.CONSTANT_silverCost), this.CONSTANT_serverRateBuy )
				if (g_BrivUserSettings["BuyGolds"] AND amountG >= this.settings.IBM_ChestSnatcher_Options_Min_Buy)
				{
					this.ChestSnatcher_AddMessage("Buy","No open order, buying " . amountG . " Gold")
					this.ChestSnatcher_BuyChests(2, amountG )
				}
				else if (g_BrivUserSettings["BuySilvers"] AND amountS >= this.settings.IBM_ChestSnatcher_Options_Min_Buy)
				{
					this.ChestSnatcher_AddMessage("Buy","No open order, buying " . amountS . " Silver")
					this.ChestSnatcher_BuyChests(1, amountS )
				}
			}
		}
		else
			this.ChestSnatcher_AddMessage("General","Unable to connect, Refreshing")
	}

	ChestSnatcher_AddMessage(action,comment)
	{
		message:={}
		FormatTime, formattedTime,, HH:mm:ss
		message["Time"]:=formattedTime
		message["Action"]:=action
		message["Comment"]:=comment
		this.ChestSnatcher_Messages.Push(message)
		if (this.ChestSnatcher_Messages.Count()>20)
			this.ChestSnatcher_Messages.RemoveAt(1)
	}

	ChestSnatcher_ClaimDailyRewards(timeAllowed) ;We don't need the time here, but use the positive/negative to determine if we need to check for save timer
	{
		if (timeAllowed>0) ;Positive time allowances require a save check
		{
			lastSaveEpoch:=this.ChestSnatcher_ReadLastSave() ;Reads in seconds since 01Jan0001
			If (lastSaveEpoch=="")
				return
			lastSave:=this.ChestSnatcher_CNETimeStampToDate(lastSaveEpoch)
			secondsElapsed:=A_NOW
			secondsElapsed-=lastSave,s
			runOpen:=(secondsElapsed<2)
		}
		else
		{
			runOpen:=true
		}
		if(runOpen)
		{
			serverString:="&user_id=" . g_SF.Memory.ReadUserID() . "&hash=" . g_SF.Memory.ReadUserHash() . "&instance_id=" . g_SF.Memory.ReadInstanceID() . "&language_id=1&timestamp=0&request_id=0&network_id=" . g_SF.Memory.ReadPlatform() . "&mobile_client_version=" . g_SF.Memory.ReadBaseGameVersion() . "&instance_key=1&offline_v2_build=1&localization_aware=true"
			extraParams := "&is_boost=0" . serverString
			response := g_ServerCall.ServerCall("claimdailyloginreward",extraParams)
			if (IsObject(response) AND response.success)
			{
				nextClaim_Seconds:=response.daily_login_details.next_claim_seconds
				if (response.daily_login_details.premium_active)
				{
					extraParams := "&is_boost=1" . serverString
					response := g_ServerCall.ServerCall("claimdailyloginreward",extraParams)
					if (IsObject(response) AND response.success)
					{
						nextClaim_Seconds:=response.daily_login_details.next_claim_seconds
						this.ChestSnatcher_AddMessage("Claim", "Claimed standard and premium daily rewards")
					}
					else ;Standard worked, premium failed despite being available?
						this.ChestSnatcher_AddMessage("Claim", "Claimed standard daily reward and failed to claim available premium reward")
				}
				else
				{
					this.ChestSnatcher_AddMessage("Claim", "Claimed standard daily reward")
				}
				if (!nextClaim_Seconds) ;If we somehow didn't get a value for the next time (despite success on the call), wait 5min before calling again
					nextClaim_Seconds:=300
				this.NextDailyClaimCheck:=A_TickCount + MIN(28800000,nextClaim_Seconds * 1000) ;8 hours, or the next reset TODO: What happens when this rolls over?
			}
			else
			{
				this.NextDailyClaimCheck:=A_TickCount + 60000 ;Wait 1min before trying again
				this.ChestSnatcher_AddMessage("Claim","Failed to claim daily rewards")
				this.ServerCallFailCount++
			}
		}
	}

	ChestSnatcher_GetBaseServerString()
	{
		return "&user_id=" . g_SF.Memory.ReadUserID() . "&hash=" . g_SF.Memory.ReadUserHash() . "&instance_id=" . g_SF.Memory.ReadInstanceID() . "&language_id=1&timestamp=0&request_id=0&network_id=" . g_SF.Memory.ReadPlatform() . "&mobile_client_version=" . g_SF.Memory.ReadBaseGameVersion() . "&instance_key=1&offline_v2_build=1&localization_aware=true"
	}

	ChestSnatcher_BuyChests(chestID := 1, numChests := 100)
    {
		if (numChests > 0)
		{
			;this.RefreshUserData() ;Will make no change if the game is closed
			callTime:=A_TickCount
			response := g_ServerCall.CallBuyChests( chestID, numChests )
			serverCallTime:=A_TickCount-callTime
			if (response.okay AND response.success)
			{
				If (chestID==1)
				{
					this.Chests.PurchasedSilver+=numChests
					this.Chests.CurrentSilver:=response.chest_count
					this.ChestSnatcher_AddMessage("Buy","Bought " . numChests " Silver in " . serverCallTime . "ms")
				}
				else If (ChestID==2)
				{
					this.Chests.PurchasedGold+=numChests
					this.Chests.CurrentGold:=response.chest_count
					this.ChestSnatcher_AddMessage("Buy","Bought " . numChests " Gold in " . serverCallTime . "ms")
				}
				this.CurrentGems:=response.currency_remaining
			}
			else
			{
				this.ChestSnatcher_AddMessage("Buy","Chest purchase failed")
				this.ServerCallFailCount++
			}
		}
    }

	ChestSnatcher_Process(timeAllowed:=2500)
	{
		if (timeAllowed>0) ;Positive time allowances require a save check
		{
			lastSaveEpoch:=this.ChestSnatcher_ReadLastSave() ;Reads in seconds since 01Jan0001
			If (lastSaveEpoch=="")
				return
			lastSave:=this.ChestSnatcher_CNETimeStampToDate(lastSaveEpoch)
			secondsElapsed:=A_NOW
			secondsElapsed-=lastSave,s
			runOpen:=(secondsElapsed<2)
		}
		else
		{
			runOpen:=true
			timeAllowed:=ABS(timeAllowed)
		}
		If (runOpen) ;Due to the 1s resolution of the timer it will be easily detected by the timer running every 600ms
		{
			this.SharedRunData.IBM_SmartChests_Time:=0 ;Prevent repeats in the same run
			amountG := Min(this.Chests.CurrentGold, this.CONSTANT_serverRateOpen)
			amountS := Min(this.Chests.CurrentSilver, this.CONSTANT_serverRateOpen)
			If (g_BrivUserSettings["OpenGolds"] AND this.Chests.CurrentGold >= this.settings.IBM_ChestSnatcher_Options_Min_Gold)
			{
				this.ChestSnatcher_OpenChests(2,timeAllowed, amountG)
			}
			else If (g_BrivUserSettings["OpenSilvers"] AND this.Chests.CurrentSilver >= this.settings.IBM_ChestSnatcher_Options_Min_Silver) ;Only open one type at a time, so only check this if we've not got enough golds
			{
				this.ChestSnatcher_OpenChests(1,timeAllowed, amountS)
			}
			Else
				this.ChestSnatcher_AddMessage("Open","Not enough chests to process open order")
		}
	}

	RefreshUserData()
    {
        if(WinExist("ahk_exe " . g_userSettings[ "ExeName"])) ; only update server when the game is open
        {
            g_SF.Memory.OpenProcessReader()
            g_SF.ResetServerCall()
			this.ServerCallFailCount:=0 ;Reset
			this.MemoryReadFailCount:=0
			if (ComObjType(this.SharedRunData,"IID") or this.RefreshComObject())
				this.SharedRunData.IBM_ProcessSwap:=false
        }
    }

	ChestSnatcher_ReadLastSave() ;TODO: Why isn't this in .Memory? Need to apply the IBM Memory Overrides to the hub
	{
		return g_SF.Memory.GameManager.game.gameInstances[g_SF.Memory.GameInstance].Controller.userData.SaveHandler.lastUserDataSaveTime.Read()
	}

	ChestSnatcher_CNETimeStampToDate(timeStamp) ;Takes a timestamp in seconds-since-day-0 format and converts it to a date for AHK use
	{
		unixTime:=timeStamp-62135596800 ;Difference between day 1 (01Jan0001) and unix time (AHK doesn't support dates before 1601 so we can't just set converted:=1)
		converted:=1970
		converted+=unixTime,s
		return converted
	}

	ChestSnatcher_OpenChests(chestID:=1,remainingTime:=0,numChests:=250)
    {
		timePerChest:=chestID==2 ? this.settings.IBM_ChestSnatcher_Options_Estimate_Gold : this.settings.IBM_ChestSnatcher_Options_Estimate_Silver ;This script can get interupted by critical in the main one, so we can't time things here unfortunately and have to go with hard-coded
		chestName:=chestID==2 ? "Gold" : "Silver"
		if (remainingTime < numChests * timePerChest)
            numChests := Floor(remainingTime / timePerChest)
        if (numChests < 25) ;Doing a servercall for <10 chests is a waste, and will not completed in the time the simple linear model expects (eg 1 chest cannot complete in 10ms because the server call won't even make it to the server in that time) TODO: Make this a setting. Have upped to 25 for now
            return
		;this.RefreshUserData() ;Will make no change if the game is closed
        callTime:=A_TickCount
		this.ChestSnatcher_AddMessage("Open","Opening " . chestName . " order for " . ROUND(remainingTime,0) . "ms")
		chestResults := g_ServerCall.CallOpenChests( chestID, numChests )
		serverCallTime:=A_TickCount-callTime
        if (!chestResults.success)
		{
			if (!chestResults.failure_reason)
			{
				this.ChestSnatcher_AddMessage("Open","Failed attempting to open " . numChests . " " . chestName " - no reason reported")
				this.ServerCallFailCount++
			}
			else if (chestResults.failure_reason=="Outdated instance id")
			{
				this.ChestSnatcher_AddMessage("Open","Failed attempting to open " . numChests . " " . chestName " - Old ID - Refreshing")
				this.RefreshUserData()
			}
			else
			{
				this.ChestSnatcher_AddMessage("Open","Failed attempting to open " . numChests . " " . chestName " - " . chestResults.failure_reason)
				this.ServerCallFailCount++
			}
			return
		}
 		if (chestID==1)
		{
			this.Chests.OpenedSilver+=numChests
			this.Chests.CurrentSilver:=chestResults.chests_remaining
			this.ChestSnatcher_AddMessage("Open","Opened " . numChests " Silver in " . serverCallTime . "ms")

		}
		else if (chestID==2)
		{
			this.Chests.OpenedGold+=numChests
		    this.Chests.CurrentGold:=chestResults.chests_remaining
			this.ChestSnatcher_AddMessage("Open","Opened " . numChests " Gold in " . serverCallTime . "ms")
		}
    }

	SetControl_RestoreWindow() ;Toggles
	{
		if (ComObjType(this.SharedRunData,"IID") or this.RefreshComObject())
            this.SharedRunData.IBM_RestoreWindow_Enabled:=!this.SharedRunData.IBM_RestoreWindow_Enabled
		else
			Msgbox % "Failed to update script."
	}

	ParseRouteImportString(routeString)
	{
		RegExMatch(routeString,"{([A-Za-z0-9-_]+),.*}",routeMatches)
		if (strlen(routeMatches1)>0)
		{
			this.settings.IBM_Route_Zones_Jump:=IC_BrivMaster_SharedFunctions_Class.IBM_ConvertBase64ToBinaryArray(routeMatches1)
			while (this.settings.IBM_Route_Zones_Jump.Length() > 50) ;The input will represent a multiple of 6 bits
				this.settings.IBM_Route_Zones_Jump.Pop()
			g_IriBrivMaster_GUI.RefreshRouteJumpBoxes()
		}
		RegExMatch(routeString,"{.*,([A-Za-z0-9-_]+)}",routeMatches)
		if (strlen(routeMatches1)>0)
		{
			this.settings.IBM_Route_Zones_Stack:=IC_BrivMaster_SharedFunctions_Class.IBM_ConvertBase64ToBinaryArray(routeMatches1)
			while (this.settings.IBM_Route_Zones_Stack.Length() > 50) ;The input will represent a multiple of 6 bits
				this.settings.IBM_Route_Zones_Stack.Pop()
			g_IriBrivMaster_GUI.RefreshRouteStackBoxes()
		}
	}

	GetRouteExportString()
	{
		return "{" . IC_BrivMaster_SharedFunctions_Class.IBM_ConvertBinaryArrayToBase64(this.settings.IBM_Route_Zones_Jump) . "," . IC_BrivMaster_SharedFunctions_Class.IBM_ConvertBinaryArrayToBase64(this.settings.IBM_Route_Zones_Stack) . "}"
	}

	SetControl_OfflineStacking()
	{
		if (ComObjType(this.SharedRunData,"IID") or this.RefreshComObject())
            this.SharedRunData.IBM_RunControl_DisableOffline:=!this.SharedRunData.IBM_RunControl_DisableOffline ;Toggle
		else
			Msgbox % "Failed to update script."
	}

	SetControl_QueueOffline()
	{
		If (ComObjType(this.SharedRunData,"IID") OR this.RefreshComObject())
			this.SharedRunData.IBM_RunControl_ForceOffline:=!this.SharedRunData.IBM_RunControl_ForceOffline
		else
			Msgbox % "Failed to update script."
	}

	UpdateStatus(force:=false) ;Run by timer to update the GUI. TODO: Some kind of 'Dirty' flag in the object might be a smarter way to handle avoiding unnecessary GUI updates? Also TODO: Since these are all simple toggles, a single function in the GUI could handle them with the control name being passed?
    {
		comValid:=ComObjType(this.SharedRunData,"IID") OR this.RefreshComObject()
		if ((comValid AND this.SharedRunData.IBM_ProcessSwap) OR this.ServerCallFailCount>2 OR this.MemoryReadFailCount>10) ;Irisiri - check we are still attached to the process
		{
			this.RefreshUserData()
		}
		if (force) ;Set to empty so that an update occurs regardless of true/false state in main script
		{
			this.STATUS_RunControlOffline:=""
			this.STATUS_RunControlForce:=""
			this.STATUS_RestoreWindow:=""
			this.STATUS_Stack_String:=""
			this.STATUS_Stack_String:=""
		}
		if (comValid)
        {
			if (this.STATUS_RunControlOffline!=this.SharedRunData.IBM_RunControl_DisableOffline)
			{
				this.STATUS_RunControlOffline:=this.SharedRunData.IBM_RunControl_DisableOffline
				g_IriBrivMaster_GUI.UpdateRunControlDisable(this.STATUS_RunControlOffline)

			}

			if (this.STATUS_RunControlForce!=this.SharedRunData.IBM_RunControl_ForceOffline)
			{
				this.STATUS_RunControlForce:=this.SharedRunData.IBM_RunControl_ForceOffline
				g_IriBrivMaster_GUI.UpdateRunControlForce(this.STATUS_RunControlForce)
			}

			if (this.STATUS_RestoreWindow!=this.SharedRunData.IBM_RestoreWindow_Enabled)
			{
				this.STATUS_RestoreWindow:=this.SharedRunData.IBM_RestoreWindow_Enabled
				g_IriBrivMaster_GUI.UpdateRestoreWindow(this.SharedRunData.IBM_RestoreWindow_Enabled)
			}

			if (this.CYCLE_Message_String!=this.SharedRunData.IBM_RunControl_CycleString OR this.STATUS_Message_String!=this.SharedRunData.IBM_RunControl_StatusString OR this.STATUS_Stack_String!=this.SharedRunData.IBM_RunControl_StackString)
			{
				this.CYCLE_Message_String:=this.SharedRunData.IBM_RunControl_CycleString
				this.STATUS_Message_String:=this.SharedRunData.IBM_RunControl_StatusString
				this.STATUS_Stack_String:=this.SharedRunData.IBM_RunControl_StackString
				g_IriBrivMaster_GUI.UpdateRunStatus(this.CYCLE_Message_String,this.STATUS_Message_String,this.STATUS_Stack_String)
			}
			this.UpdateStats()
        }
        else
        {
            g_IriBrivMaster_GUI.ResetStatusText()
        }
		g_IriBrivMaster_GUI.IBM_ChestsSnatcher_Status_Update()
		if (A_TickCount>=this.NextGameSettingsCheck)
			this.GameSettingsCheck()
    }

	LoadSettings()
    {
        needSave := false
        default := this.GetNewSettings()
        this.Settings := settings := g_SF.LoadObjectFromJSON(IC_BrivMaster_SharedData_Class.SettingsPath)
        if (!IsObject(settings))
        {
            this.Settings := settings := default
            needSave := true
        }
        else
        {
            ; Delete extra settings
            for k, v in settings
            {
                if (!default.HasKey(k))
                {
                    settings.Delete(k)
                    needSave := true
                }
            }
            ; Add missing settings
            for k, v in default
            {
                if (!settings.HasKey(k) || settings[k] == "")
                {
                    settings[k] := default[k]
                    needSave := true
                }
            }
        }
        if (needSave)
            this.SaveSettings()
        ; Set the state of GUI buttons with saved settings.
        g_IriBrivMaster_GUI.UpdateGUISettings(settings)
    }

    UpdateSetting(setting, value)
    {
        this.Settings[setting] := value
    }

    UpdateLevelSettings(levelData)
    {
        this.Settings["IBM_LevelManager_Levels",this.Settings["IBM_Route_Combine"]] := levelData
    }

	UpdateRouteSetting(setting,toggleZone)
	{
		this.Settings[setting][toggleZone]:=!this.Settings[setting][toggleZone]
	}

	IBM_GetGUIFormationData() ;Generates formation data for the level manager GUI
	{
		championData:={} ;This will be per seat, then champID with a list of formations containing, eg championData[1][58] being [1,3,4] if Briv is in Q/E/M but not W
        slots:=["Q","W","E"]
		loop 3
		{
			formation:=g_SF.Memory.GetFormationByFavorite(A_Index)
			this.IBM_GetGUIFormationData_ProcessFormation(championData,slots[A_Index],formation)
		}
		formation:=g_SF.Memory.GetActiveModronFormation()
		this.IBM_GetGUIFormationData_ProcessFormation(championData,"M",formation)
		return championData
	}

	IBM_GetGUIFormationData_ProcessFormation(championData,index,formation)
	{
		for _, champId in formation
		{
			if champID>0
			{
				seat:=g_SF.Memory.ReadChampSeatByID(champID)
                if !(championData.hasKey(seat) and championData[seat].hasKey(champID)) ;Create entry for this champ
                {
                    championData[seat,champID,"Name"]:=g_SF.Memory.ReadChampNameByID(champID) ;We need to create the array if it doesn't yet exist
                    championData[seat,champID,"Q"]:=false
                    championData[seat,champID,"W"]:=false
                    championData[seat,champID,"E"]:=false
                    championData[seat,champID,"M"]:=false
                }
                championData[seat,champID,index]:=true
			}
		}
	}

	IBM_Elly_StartNonGemFarm()
    {
        this.Elly_NonGemFarm := new IC_BrivMaster_EllywickDealer_NonFarm_Class(this.IBM_Elly_GetNonGemFarmCards("Min"),this.IBM_Elly_GetNonGemFarmCards("Max"))
        this.Elly_NonGemFarm.Start()
		g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Started")
    }

    IBM_Elly_StopNonGemFarm()
    {
        this.Elly_NonGemFarm.Stop()
        this.Elly_NonGemFarm := ""
		g_IriBrivMaster_GUI.SetEllyNonGemFarmStatus("Stopped")
    }

    IBM_Elly_GetNonGemFarmCards(capType:="Min")
    {
        cards := []
        Loop 5
        {
            GuiControlGet, cap, ICScriptHub:, IBM_NonGemFarm_Elly_%capType%_%A_Index% ;Eg IBM_NonGemFarm_Elly_Min_1
            cards.Push(cap)
        }
        return cards
    }

}

;Modifed AHK JSON Library for working with the game setting file - as JSON lacks a boolean type 'x'=false or 'y'=true ges turned into numeric values as standard

/**
 * Modify by WarpRider, Member on https://www.autohotkey.com/boards
 *     ingnore the ahk internal vars true/false and the string null wil be not empty
 */

class AHK_JSON_RAWBOOLEAN extends AHK_JSON ;Irisiri - renamed as SH already has a JSON class powered by JavaScript
{
	class Load extends AHK_JSON.Load
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
							;MsgBox, "value=" %value%

							static number := "number", integer :="integer"
							if value is %number%
							{
								if value is %integer%
									value += 0
							}

							;WarpRider 31.01.2023: hier wird value auf true oder false geprüft und behandelt, nach AHK wird das dann 0 oder 1,
							;das ist aber falsch, da true/false für JSON keine boolschen Variablen sind, value muss unverändert übernommen werden
							else if (value == "true" || value == "false")
								value := value	;ORIGINAL: value := %value% + 0


							else if (value == "null")
								value := "null"									;WarpRider 31.01.2023: hier genauso, warum wird null nicht stur übernommen?
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
	}

	class Dump extends AHK_JSON.Dump
	{
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

			}
			;WarpRider 31.01.2023: alle Werte hier, ausser Zahlen werden durch die Funktion Quote() mit " eingefasst,
			;das darf bei true,false,null eben nicht so sein, da true/false für JSON keine boolschen Variablen sind und null nicht leer werden
			else ; is_number ? value : "value"
			{
			;MsgBox, vor.Str.return.raw.value=%value%
			if (value == "true" || value == "false" || value == "null")
			  return value
			else
			  return ObjGetCapacity([value], 1)=="" ? value : this.Quote(value)
			}

		}
	}
}