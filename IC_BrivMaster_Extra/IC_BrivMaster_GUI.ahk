GUIFunctions.AddTab("Briv Master")
GUIFunctions.AddTab("BM Game")
GUIFunctions.AddTab("BM Route")
GUIFunctions.AddTab("BM Levels")

class IC_IriBrivMaster_GUI
{
	static IBM_COLOUR_FORMATION_IN:="c00B050"
	static IBM_COLOUR_FORMATION_OUT:="cE0E0E0"
	static IBM_COLOUR_ROUTE_NO:="cD5D5D5"
	static IBM_COLOUR_ROUTE_YES_JUMP:="c00F000"
	static IBM_COLOUR_ROUTE_YES_STACK:="cF00000"
	static IBM_SYMBOL_ROUTE_JUMP:="▲" ;This file (only) has to be saved as a UTF-8-BOM file to make these symbols work
	static IBM_SYMBOL_ROUTE_STACK:="≡"
	static IBM_SYMBOL_CONTROL_ACTIVE:="●"
	static IBM_SYMBOL_UI_DOWN:="▼"
	static IBM_SYMBOL_UI_CONFIG:="⚙"
	static IBM_SYMBOL_UI_LEFT:="◀"
	static IBM_SYMBOL_UI_CLEAR:="○"
	static IBM_SYMBOL_UI_LIGHT:="⬤"
	
	levelDataSet:={}
	controlLock:=false

	Init()
	{
		global ;Required for GUI control variables
		GUIFunctions.UseThemeTextColor()
		GuiControl, ICScriptHub: +gIBM_Launch_Override, LaunchClickButton ;Override the main launch button to use IBM settings
		groupWidth:=480
		g_TabControlWidth:=520 ;Widen script hub
		;MAIN TAB
		Gui, ICScriptHub:Tab, Briv Master
		;Buttons for starting, saving etc
		buttonWidth:=25
		buttonSpacing:=15
		firstButtonOffset:=(groupWidth/2) - (buttonWidth*5+buttonSpacing*4)/2 ;Place the center of the set of buttons in the centre of the group box area
		buttonStatusWidth:=Max(firstButtonOffset-10, 50)
		Gui, ICScriptHub:Add, Text, xm+5 y+10 r2 w%buttonStatusWidth% vIBM_MainButtons_Status 0x200
		Gui, ICScriptHub:Add, Picture, xm+%firstButtonOffset% yp+0 h-1 w%buttonWidth% gIBM_MainButtons_Start vIBM_MainButtons_Start, %g_PlayButton%
		Gui, ICScriptHub:Add, Picture, x+%buttonSpacing% h-1 w%buttonWidth% gIBM_MainButtons_Stop vIBM_MainButtons_Stop, %g_StopButton%
		Gui, ICScriptHub:Add, Picture, x+%buttonSpacing% h-1 w%buttonWidth% gIBM_MainButtons_Connect vIBM_MainButtons_Connect, %g_ConnectButton%
		Gui, ICScriptHub:Add, Picture, x+%buttonSpacing% h-1 w%buttonWidth% gIBM_MainButtons_Save vIBM_MainButtons_Save, %g_SaveButton%
		Gui, ICScriptHub:Add, Picture, x+%buttonSpacing% h-1 w%buttonWidth% gIBM_MainButtons_Reset vIBM_MainButtons_Reset, %A_LineFile%\..\Resources\Reset-100x100.png
		GUIFunctions.AddToolTip("IBM_MainButtons_Start", "Start Gem Farm")
        GUIFunctions.AddToolTip("IBM_MainButtons_Stop", "Stop Gem Farm")
        GUIFunctions.AddToolTip("IBM_MainButtons_Connect", "Reconnect to Gem Farm script")
        GUIFunctions.AddToolTip("IBM_MainButtons_Save", "Save Briv Master settings from all tabs")
		GUIFunctions.AddToolTip("IBM_MainButtons_Reset", "Reset stats")
		;Cycle
		Gui, ICScriptHub:Add, Groupbox, xm+385 yp-8 w100 h38
		Gui, ICScriptHub:Add, Text, xp+5 yp+12 w90 0x200 h18 Center vIBM_RunControl_Cycle, % "Cycle -/-"
		;Run control
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+5 w%groupWidth% h141 vIBM_RunControl_Group, Run Control
		Gui, ICScriptHub:Font, w400
		;>Group for offline control options
		Gui, ICScriptHub:Add, Groupbox, xs+8 ys+10 w290 h41
		;>Pause offline
		Gui, ICScriptHub:Add, Text, xp+10 yp+13 w80 0x200 h20, Offline stacking ;0x200 centres vertically
		Gui, ICScriptHub:Add, Button, x+5 w50 vIBM_RunControl_Offline_Toggle gIBM_RunControl_Offline_Toggle, Pause
		Gui, ICScriptHub:Add, Text, x+5 w10 0x200 h20 vIBM_RunControl_Offline_StatusPause, % IC_IriBrivMaster_GUI.IBM_SYMBOL_CONTROL_ACTIVE
		;>Queue offline
		Gui, ICScriptHub:Add, Text, x+10 yp+0 w40 0x200 h20, Queue ;0x200 centres vertically
		Gui, ICScriptHub:Add, Button, x+5 w50 vIBM_RunControl_Offline_Queue_Toggle gIBM_RunControl_Offline_Queue_Toggle, Force
		Gui, ICScriptHub:Add, Text, x+5 w10 0x200 h20 vIBM_RunControl_Offline_StatusQueue, % IC_IriBrivMaster_GUI.IBM_SYMBOL_CONTROL_ACTIVE
		;>Restore Window
		Gui, ICScriptHub:Add, Groupbox, xs+305 ys+10 w166 h41
		Gui, ICScriptHub:Add, Text, xp+10 yp+13 w80 0x200 h20, Restore Window ;0x200 centres vertically
		Gui, ICScriptHub:Add, Button, x+5 w50 vIBM_RunControl_RestoreWindow_Toggle gIBM_RunControl_RestoreWindow_Toggle, Enable
		Gui, ICScriptHub:Add, Text, x+5 w10 0x200 h20 vIBM_RunControl_RestoreWindow_Status, % IC_IriBrivMaster_GUI.IBM_SYMBOL_CONTROL_ACTIVE
		;>RunControl status
		Gui, ICScriptHub:Add, Text, xs+10 yp+35 w460 vIBM_RunControl_Status, -
		Gui, ICScriptHub:Add, Text, xs+10 yp+15 w460 vIBM_RunControl_Stack, -
		Gui, ICScriptHub:Add, Text, xs+10 yp+15, Stage:
		Gui, ICScriptHub:Add, Text, x+3 w400 vIBM_Stats_Loop, -
		Gui, ICScriptHub:Add, Text, xs+10 yp+15, Last Close:
		Gui, ICScriptHub:Add, Text, x+3 w400 vIBM_Stats_Last_Close, -
		Gui, ICScriptHub:Add, Text, xs+10 yp+15, Current Area / Run (s):
		Gui, ICScriptHub:Add, Text, x+3 w80 vIBM_Stats_Current_Area_Run_Time, -
		Gui, ICScriptHub:Add, Text, xs+230 yp+0, Current SB / Haste Stacks:
		Gui, ICScriptHub:Add, Text, x+3 w80 vIBM_Stats_Current_Briv, -
		;Stats
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+8 w%groupWidth% h220 vIBM_Stats_Group, Run Stats
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, ListView , +cBlack xs+10 ys+20 w220 0x2000 LV0x10000 vIBM_Stats_Run_LV Count3 R3 LV0x10 NoSort NoSortHdr, Time|Last|Mean|Fast|Slow ;0x2000 is remove H scroll bar, LV0x10000 is double-buffering to stop flickering, LV0x10 prevents re-ordering of columns
		GuiControl, -Redraw, IBM_Stats_Run_LV
		Gui, ICScriptHub:Default
		Gui, ListView, IBM_Stats_Run_LV
		LV_Add(,"Total","--.--","--.--","--.--","--.--")
		LV_Add(,"Active","--.--","--.--","--.--","--.--")
		LV_Add(,"Wait","--.--","--.--","--.--","--.--")
		LV_ModifyCol(1,"AutoHdr")
		LV_ModifyCol(2,"AutoHdr")
		LV_ModifyCol(3,"AutoHdr")
		LV_ModifyCol(4,"AutoHdr")
		LV_ModifyCol(5,"AutoHdr")
		GuiControl, +Redraw, IBM_Stats_Run_LV
		GuiControlGet, statsLVEndPos, ICScriptHub:Pos, IBM_Stats_Run_LV
		highlightY:=statsLVEndPosY+statsLVEndPosH+15
		Gui, ICScriptHub:Add, Text, xs+250 ys+20 w85, Total Runs:
		Gui, ICScriptHub:Add, Text, x+3 w140 vIBM_Stats_Total_Runs, -
		Gui, ICScriptHub:Add, Text, xs+250 y+4 w85, Total Time:
		Gui, ICScriptHub:Add, Text, x+3 w140 vIBM_Stats_Total_Time, -
		Gui, ICScriptHub:Add, Text, xs+250 y+4 w85, Failed Runs:
		Gui, ICScriptHub:Add, Text, x+3 w140 vIBM_Stats_Fail_Runs, -
		Gui, ICScriptHub:Add, Text, xs+250 y+4 w85, Failed Run Time:
		Gui, ICScriptHub:Add, Text, x+3 w140 vIBM_Stats_Fail_Time, -
		highlightWidth:=FLOOR((groupWidth-21)/2)
		GUIFunctions.UseThemeTextColor("SpecialTextColor1", 700)
		Gui, ICScriptHub:Add, Text, xs+10 y%highlightY% w%highlightWidth% Center vIBM_Stats_BPH, BPH
		GUIFunctions.UseThemeTextColor("SpecialTextColor2", 700)
		Gui, ICScriptHub:Add, Text, x+1 w100 w%highlightWidth% Center vIBM_Stats_GPH, GPH
		GUIFunctions.UseThemeTextColor()
		Gui, ICScriptHub:Add, Text, xs+10 y+10, Total Gems:
		Gui, ICScriptHub:Add, Text, x+3 w80 vIBM_Stats_TotalGems,
		Gui, ICScriptHub:Add, Text, x+10, Gem Hunter:
		Gui, ICScriptHub:Add, Text, x+3 vIBM_Stats_Gem_Hunter, % IC_IriBrivMaster_GUI.IBM_SYMBOL_CONTROL_ACTIVE
		GUIFunctions.AddToolTip("IBM_Stats_Gem_Hunter", "Gem Hunter potion status for the recorded runs. Green means it was active for all runs, amber for some runs and red for none")
		Gui, ICScriptHub:Add, Text, x+10, GPB:
		Gui, ICScriptHub:Add, Text, x+3 w30 vIBM_Stats_GPB, -
		Gui, ICScriptHub:Add, Text, x+10, Bonus:
		Gui, ICScriptHub:Add, Text, x+3 w50 vIBM_Stats_Gem_Bonus, -
		Gui, ICScriptHub:Add, Text, xs+10 y+5, Chests (Dropped/Bought/Opened)
		Gui, ICScriptHub:Add, Text, x+3 w295 vIBM_Stats_Chests, Gold: - / - / - Silver: - / - / -
		Gui, ICScriptHub:Add, Text, xs+10 y+5 w75, BSC iLevels/h:
		Gui, ICScriptHub:Add, Text, x+3 w350 vIBM_Stats_BSC_Reward, -
		Gui, ICScriptHub:Add, Text, xs+10 y+0 w75, Total iLevels/h:
		Gui, ICScriptHub:Add, Text, x+3 w350 vIBM_Stats_Total_Reward, -
		Gui, ICScriptHub:Add, Text, xs+10 y+5 , Boss Levels Hit (This Run / Total):
		Gui, ICScriptHub:Add, Text, x+3 w50 vIBM_Stats_Boss_Hits, -
		Gui, ICScriptHub:Add, Text, x+10, Rollbacks:
		Gui, ICScriptHub:Add, Text, x+3 w20 vIBM_Stats_Rollbacks, -
		Gui, ICScriptHub:Add, Text, x+10, Bad Autoprogressions:
		Gui, ICScriptHub:Add, Text, x+3 w20 vIBM_Stats_Bad_Auto, -
		;Chests
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+12 w%groupWidth% h50 vIBM_Chest_Group, Chests && Daily Platinum
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, ListView , +cBlack xs+10 ys+20 w410 0x2000 LV0x10000 LV0x10 vIBM_ChestsSnatcher_Status Count10 -Hdr R1, Time|Action|Result ;0x2000 is remove H scroll bar, LV0x10000 is double-buffering to stop flickering
		GuiControl, -Redraw, IBM_ChestsSnatcher_Status
		Gui, ICScriptHub:Default
		Gui, ListView, IBM_ChestsSnatcher_Status
		LV_ModifyCol(1,50)
		LV_ModifyCol(2,50)
		GuiControl, +Redraw, IBM_ChestsSnatcher_Status
		;>Chest Log window
		Gui, IBM_ChestSnatcher_Log:New , , Chest & Daily Platinum Log
		Gui, IBM_ChestSnatcher_Log:Margin, 0,0
		Gui, IBM_ChestSnatcher_Log:-Resize -MaximizeBox -Caption +HwndLog_Hwnd
		this.IBM_ChestSnatcher_Log_Hwnd:=Log_Hwnd ;Save handle to the log window
		Gui, IBM_ChestSnatcher_Log:Add, ListView , w410 0x2000 LV0x10000 vIBM_ChestsSnatcher_Status_Expanded Count20 R20, Time|Action|Result ;0x2000 is remove H scroll bar, LV0x10000 is double-buffering to stop flickering
		GuiControl, -Redraw, vIBM_ChestsSnatcher_Status_Expanded
		Gui, IBM_ChestSnatcher_Log:Default
		Gui, ListView, vIBM_ChestsSnatcher_Status_Expanded
		LV_ModifyCol(1,50)
		LV_ModifyCol(2,50)
		GuiControl, +Redraw, vIBM_ChestsSnatcher_Status_Expanded
		Gui, ICScriptHub:Default
		;>Chest buttons
		Gui, ICScriptHub:Add, Button, x+5 w20 vIBM_ChestsSnatcher_Status_Resize gIBM_ChestsSnatcher_Status_Resize, % IC_IriBrivMaster_GUI.IBM_SYMBOL_UI_DOWN
		Gui, ICScriptHub:Add, Button, x+5 w20 vIBM_ChestsSnatcher_Options gIBM_ChestsSnatcher_Options, % IC_IriBrivMaster_GUI.IBM_SYMBOL_UI_CONFIG
		;>Chest options
		Gui, IBM_ChestSnatcher_Options:New , , Chest Options ;Note this window uses an Accept button to accept changes, so that the script does not execute based on partial entry (e.g. with poor timing it could buy 12 chests whilst you were typing 123 into the box)
		Gui, IBM_ChestSnatcher_Options:-Resize -MaximizeBox +HwndOpt_Hwnd
		this.IBM_ChestSnatcher_Opt_Hwnd:=Opt_Hwnd ;Save handle to the options window
		Gui, IBM_ChestSnatcher_Options:Add, Edit, xm+10 w50 Number Limit3 vIBM_ChestSnatcher_Options_Min_Buy
		Gui, IBM_ChestSnatcher_Options:Add, Text, x+10 w170 h18 0x200, Gold to buy per call (0 to disable)
		Gui, IBM_ChestSnatcher_Options:Add, Edit, xm+10 w50 Number Limit4 vIBM_ChestSnatcher_Options_Open_Gold
		Gui, IBM_ChestSnatcher_Options:Add, Text, x+10 w170 h18 0x200, Gold to open per call (0 to disable)
		Gui, IBM_ChestSnatcher_Options:Add, Edit, xm+10 w50 Number Limit4 vIBM_ChestSnatcher_Options_Open_Silver
		Gui, IBM_ChestSnatcher_Options:Add, Text, x+10 w170 h18 0x200, Silver to open per call (0 to disable)
		Gui, IBM_ChestSnatcher_Options:Add, Edit, xm+10 w50 Number Limit8 vIBM_ChestSnatcher_Options_Min_Gem
		Gui, IBM_ChestSnatcher_Options:Add, Text, x+10 w170 h18 0x200, Reserve Gems
		Gui, IBM_ChestSnatcher_Options:Add, Edit, xm+10 w50 Number Limit8 vIBM_ChestSnatcher_Options_Min_Gold
		Gui, IBM_ChestSnatcher_Options:Add, Text, x+10 w170 h18 0x200, Reserve Gold
		Gui, IBM_ChestSnatcher_Options:Add, Edit, xm+10 w50 Number Limit8 vIBM_ChestSnatcher_Options_Min_Silver
		Gui, IBM_ChestSnatcher_Options:Add, Text, x+10 w170 h18 0x200, Reserve Silver
		Gui, IBM_ChestSnatcher_Options:Add, CheckBox, xm+10 h18 0x200 vIBM_ChestSnatcher_Options_Claim, Claim Daily Rewards
		gui, IBM_ChestSnatcher_Options:Add, Button, xm+100 w50 gIBM_ChestSnatcher_Options_OK_Button, Accept
		;Game Settings
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+12 w%groupWidth% h50 vIBM_Game_Settings_Group, % "Game Settings" ;Group has a variable so we can check its location for the
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+20 h18 0x200, Profile:
		Gui, ICScriptHub:Add, Radio, x+5 h18 vIBM_Game_Settings_Profile_1 gIBM_Game_Settings_Profile, Profile 1 ;TODO: Disable wrapping on these, as it seems that can happen here?
		Gui, ICScriptHub:Add, Radio, x+0 h18 vIBM_Game_Settings_Profile_2 gIBM_Game_Settings_Profile, Profile 2
		Gui, ICScriptHub:Add, Text, x+10 h18 w220 0x200 vIBM_Game_Settings_Status, Not checked
		Gui, ICScriptHub:Add, Button, xs+398 yp+0 w47 vIBM_Game_Settings_Fix gIBM_Game_Settings_Fix, Set Now
		Gui, ICScriptHub:Add, Button, x+5 w20 vIBM_Game_Settings_Options gIBM_Game_Settings_Options, % IC_IriBrivMaster_GUI.IBM_SYMBOL_UI_CONFIG
		;>Game Settings Options Window
		Gui, ICScriptHub:Font, w700
		Gui, IBM_Game_Settings_Options:New , , Game Settings
		Gui, ICScriptHub:Font, w400
		Gui, IBM_Game_Settings_Options:-Resize -MaximizeBox +HwndOpt_Hwnd
		this.IBM_Game_Settings_Opt_Hwnd:=Opt_Hwnd ;Save handle to the options window
		Gui, IBM_Game_Settings_Options:Add, Text, xm+0 w80 h18 0x200 Center, Profile 1
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w80 h18 0x200 Center, Option
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w80 h18 0x200 Center, Profile 2

		Gui, IBM_Game_Settings_Options:Add, Edit, xm+0 w80 Limit12 vIBM_Game_Settings_Option_Name_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w80 h18 0x200 Center, Name
		Gui, IBM_Game_Settings_Options:Add, Edit, x+3 w80 Limit12 vIBM_Game_Settings_Option_Name_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, Edit, xm+0 w80 vIBM_Game_Settings_Option_Framerate_1 Limit4 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w80 h18 0x200 Center, Framerate
		Gui, IBM_Game_Settings_Options:Add, Edit, x+3 w80 Limit4 vIBM_Game_Settings_Option_Framerate_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, Edit, xm+0 w80 Limit3 vIBM_Game_Settings_Option_Particles_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w80 h18 0x200 Center, % "% Particles"
		Gui, IBM_Game_Settings_Options:Add, Edit, x+3 w80 Limit3 vIBM_Game_Settings_Option_Particles_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, Edit, xm+0 w80 Limit4 vIBM_Game_Settings_Option_HRes_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w80 h18 0x200 Center, H. Resolution
		Gui, IBM_Game_Settings_Options:Add, Edit, x+3 w80 Limit4 vIBM_Game_Settings_Option_HRes_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, Edit, xm+0 w80 Limit4 vIBM_Game_Settings_Option_VRes_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w80 h18 0x200 Center, V. Resolution
		Gui, IBM_Game_Settings_Options:Add, Edit, x+3 w80 Limit4 vIBM_Game_Settings_Option_VRes_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, CheckBox, xm+32 w28 vIBM_Game_Settings_Option_Fullscreen_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w120 h18 0x200 Center, Fullscreen
		Gui, IBM_Game_Settings_Options:Add, CheckBox, x+16 w28 vIBM_Game_Settings_Option_Fullscreen_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, CheckBox, xm+32 w28 vIBM_Game_Settings_Option_CapFPSinBG_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w120 h18 0x200 Center, Cap FPS in BG
		Gui, IBM_Game_Settings_Options:Add, CheckBox, x+16 w28 vIBM_Game_Settings_Option_CapFPSinBG_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, CheckBox, xm+32 w28 vIBM_Game_Settings_Option_SaveFeats_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w120 h18 0x200 Center, Save Feats
		Gui, IBM_Game_Settings_Options:Add, CheckBox, x+16 w28 vIBM_Game_Settings_Option_SaveFeats_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, Text, xm+29 w34 h18 0x200, 100
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w120 h18 0x200 Center, Level Amount
		Gui, IBM_Game_Settings_Options:Add, Text, x+10 w28 h18 0x200, 100

		Gui, IBM_Game_Settings_Options:Add, CheckBox, xm+32 w28 vIBM_Game_Settings_Option_ConsolePortraits_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w120 h18 0x200 Center, Console Portraits
		Gui, IBM_Game_Settings_Options:Add, CheckBox, x+16 w28 vIBM_Game_Settings_Option_ConsolePortraits_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, CheckBox, xm+32 w28 vIBM_Game_Settings_Option_NarrowHero_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w120 h18 0x200 Center, Narrow Hero Boxes
		Gui, IBM_Game_Settings_Options:Add, CheckBox, x+16 w28 vIBM_Game_Settings_Option_NarrowHero_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, CheckBox, xm+32 w28 vIBM_Game_Settings_Option_AllHero_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w120 h18 0x200 Center, Show All Bench Seats
		Gui, IBM_Game_Settings_Options:Add, CheckBox, x+16 w28 vIBM_Game_Settings_Option_AllHero_2 gIBM_Game_Settings_Option_Change

		Gui, IBM_Game_Settings_Options:Add, CheckBox, xm+32 w28 vIBM_Game_Settings_Option_Swap25100_1 gIBM_Game_Settings_Option_Change
		Gui, IBM_Game_Settings_Options:Add, Text, x+3 w120 h18 0x200 Center, Swap x25 and x100
		Gui, IBM_Game_Settings_Options:Add, CheckBox, x+16 w28 vIBM_Game_Settings_Option_Swap25100_2 gIBM_Game_Settings_Option_Change
		;Ellywick non-gemfarming Tool
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+12 w%groupWidth% h75, % "Ellywick Non-Gemfarm Re-roll Tool"
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, w36 xs+58 ys+20 Center, Knight
		Gui, ICScriptHub:Add, Text, w36 x+3 Center, Moon
		Gui, ICScriptHub:Add, Text, w36 x+3 Center, Gem
		Gui, ICScriptHub:Add, Text, w36 x+3 Center, Fates
		Gui, ICScriptHub:Add, Text,  w36 x+3 Center, Flames
		Gui, ICScriptHub:Add, Button, x+20 yp-3 w50 vIBM_NonGemFarm_Elly_Start gIBM_NonGemFarm_Elly_Start, Start
		Gui, ICScriptHub:Add, Button, x+5 w50 vIBM_NonGemFarm_Elly_Stop gIBM_NonGemFarm_Elly_Stop, Stop
		Gui, ICScriptHub:Add, Text, w40 xs+10 y+5 h18 0x200, Min:Max
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+10 Number Limit1 vIBM_NonGemFarm_Elly_Min_1
		Gui, ICScriptHub:Add, Text, w5 x+0 h18 0x200 Center, :
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+0 Number Limit1 vIBM_NonGemFarm_Elly_Max_1
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+10 Number Limit1 vIBM_NonGemFarm_Elly_Min_2
		Gui, ICScriptHub:Add, Text, w5 x+0 h18 0x200 Center, :
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+0 Number Limit1 vIBM_NonGemFarm_Elly_Max_2
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+10 Number Limit1 vIBM_NonGemFarm_Elly_Min_3
		Gui, ICScriptHub:Add, Text, w5 x+0 h18 0x200 Center, :
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+0 Number Limit1 vIBM_NonGemFarm_Elly_Max_3
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+10 Number Limit1 vIBM_NonGemFarm_Elly_Min_4
		Gui, ICScriptHub:Add, Text, w5 x+0 h18 0x200 Center, :
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+0 Number Limit1 vIBM_NonGemFarm_Elly_Max_4
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+10 Number Limit1 vIBM_NonGemFarm_Elly_Min_5
		Gui, ICScriptHub:Add, Text, w5 x+0 h18 0x200 Center, :
		Gui, ICScriptHub:Add, Edit, +cBlack  w12 x+0 Number Limit1 vIBM_NonGemFarm_Elly_Max_5
		Gui, ICScriptHub:Add, Text, x+25 w130 vIBM_NonGemFarm_Elly_Status,

		;GAME TAB
		Gui, ICScriptHub:Tab, BM Game
		;Game location
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+10 w%groupWidth% h128, Game Location
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, w55 xs+5 ys+20 h18 0x200, Executable:
		Gui, ICScriptHub:Add, Edit, +cBlack  w40 x+10 w170 vIBM_Game_Exe gIBM_Game_Location_Settings
		GUIFunctions.AddToolTip("IBM_Game_Exe", "The game executable file name, normally IdleDragons.exe")
		Gui, ICScriptHub:Add, CheckBox, x+10 h18 0x200 vIBM_Game_Hide_Launcher gIBM_Game_Location_Settings, Hide launcher
		GUIFunctions.AddToolTip("IBM_Game_Hide_Launcher", "Select this option to hide the window created by the launch command. Useful when using an alternative launcher and do not want to see the window it creates. Do not use when launching the game directly")
		Gui, ICScriptHub:Add, Button, xs+400 yp+0 w70 vIBM_Game_Copy_From_Game gIBM_Game_Copy_From_Game, Copy from IC
		Gui, ICScriptHub:Add, Text, w55 xs+5 y+5 h18 0x200, Location:
		Gui, ICScriptHub:Add, Edit, +cBlack  w40 x+10 w402 r2 vIBM_Game_Path gIBM_Game_Location_Settings
		GUIFunctions.AddToolTip("IBM_Game_Path", "The game install location")
		Gui, ICScriptHub:Add, Text, w55 r2 xs+5 y+5 h18, Launch Command:
		Gui, ICScriptHub:Add, Edit, +cBlack  w40 x+10 w402 r2 vIBM_Game_Launch gIBM_Game_Location_Settings
		GUIFunctions.AddToolTip("IBM_Game_Launch", "The launch command for the game. This is seperated to allow the use of different launchers")
		;Script Window Options
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+9 w%groupWidth% h50, % "Window Options"
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+20 h18 0x200, Screen Position (x,y):
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+2 Number Limit4 vIBM_Window_X gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, Text, x+2 h18 0x200, ,
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+2 Number Limit4 vIBM_Window_Y gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, CheckBox, x+15 h18 0x200 vIBM_Window_Hide gIBM_Generic_Setting_Int, Hide
		Gui, ICScriptHub:Add, CheckBox, x+15 h18 0x200 vIBM_Window_Dark_Icon gIBM_Generic_Setting_Int, Dark Icon
		;Offsets
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+13 w%groupWidth% h106, Offsets
		Gui, ICScriptHub:Font, w400
		gameMajor:=g_SF.Memory.ReadBaseGameVersion() ;Major version, e.g. 636.3 will return 636
		gameMinor:=g_SF.Memory.IBM_ReadGameVersionMinor() ;If the game is 636.3, return .3, 637 will return empty as it has no minor version
		gameVersion:=gameMajor ? gameMajor . gameMinor : "<Not found>"
		Gui, ICScriptHub:Add, Text, w200 xs+5 ys+15 h18 0x200 vIBM_Offsets_Text_Game, % "Game Version: " . gameVersion
		Gui, ICScriptHub:Add, Text, w200 xs+220 yp+0 h18 0x200 vIBM_Offsets_Text_Platform, % "Platform: " . g_IriBrivMaster.GetPlatformString()
				
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Text, w45 xs+5 y+2 h18 0x200, % "Pointers" 
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, w150 x+10 h18 0x200 vIBM_Offsets_Text_Pointers_Current, % "Current: " . g_IriBrivMaster.GetPointersVersion()
		Gui, ICScriptHub:Add, Text, w120 x+10 h18 0x200 vIBM_Offsets_Text_Pointers_GitHub, % "GitHub: <Not checked>"
		Gui, ICScriptHub:Add, Button, xs+400 yp+0 w70 vIBM_Offsets_Check_Now gIBM_Offsets_Check_Now, Check now
		
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Text, w45 xs+5 y+2 h18 0x200, % "Imports" 
		Gui, ICScriptHub:Font, w400
		
		currentImports:=g_SF.Memory.GetImportsVersion()
		comparison:=g_IriBrivMaster.VersionComparison(gameVersion,currentImports)
		if(comparison.GT)
			colour:="cRed"
		else
			colour:="cBlack"
		Gui, ICScriptHub:Add, Text, w150 x+10 %colour% h18 0x200 vIBM_Offsets_Text_Imports_Current, % "Current: " . currentImports
		Gui, ICScriptHub:Add, Text, w120 x+10 h18 0x200 vIBM_Offsets_Text_Imports_GitHub, % "GitHub: <Not checked>"		
		Gui, ICScriptHub:Add, Button, xs+400 yp+0 w70 vIBM_Offsets_Download gIBM_Offsets_Download, Download
		
		Gui, ICScriptHub:Add, CheckBox, xs+5 yp+25 w120 h18 0x200 Center vIBM_Offsets_Check gIBM_Generic_Hub_Setting_Int, Check automatically
		GUIFunctions.AddToolTip("IBM_Offsets_Check", "Check this option to automatically check for updates to Script Hub and enabled addons when Script Hub starts")
		Gui, ICScriptHub:Add, CheckBox, x+10 w120 h18 0x200 Center vIBM_Offsets_Lock_Pointers gIBM_Generic_Hub_Setting_Int, Update imports only
		GUIFunctions.AddToolTip("IBM_Offsets_Lock_Pointers", "Check this option to only apply new imports when downloading. Use this if you have tweaked the pointers yourself")
		;Server
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+7 w%groupWidth% h43, Server
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, w400 xs+5 ys+15 h18 0x200 vIBM_Server_Text_PS, % "Play Server: " . g_IriBrivMaster.GetPlayServerFriendly()
		Gui, ICScriptHub:Add, Button, xs+415 yp+0 w55 vIBM_Server_Check gIBM_Server_Check, Refresh
		;Versions - core, static
		Gui, ICScriptHub:Font, w700
		sideBarWidth:=90
		mainWidth:=groupWidth-sideBarWidth-5
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+9 w%mainWidth% h63, Core Versions
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+15 w10 h18 0x200 vIBM_Version_Status_SH, % IC_IriBrivMaster_GUI.IBM_SYMBOL_UI_LIGHT
		Gui, ICScriptHub:Add, Text, x+5 w305 h18 0x200 vIBM_Version_Text_SH, % "Script Hub: " . GetScriptHubVersion() ;This is a script-level function. Will be replaced with check data if checking is enabled
		readMeLocation:=A_ScriptDir . "\ReadMe.md"
		if(FileExist(readMeLocation))
			Gui, ICScriptHub:Add, Link, xs+335 yp+0 w45 h18 0x200 vIBM_Version_Readme_SH, % "<a href=""" . readMeLocation . """>Readme</a>"
		Gui, ICScriptHub:Add, Text, w350 xs+5 yp+20 h18 0x200, % "AHK Version: " . A_AhkVersion
		
		;Versions - check options sidebar
		sideBarOffset:=mainWidth+10
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+%sideBarOffset% ys+0 w%sideBarWidth% h63, Version Check
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, CheckBox, xs+12 yp+15 w65 h18 0x200 vIBM_Version_Check gIBM_Generic_Hub_Setting_Int, On load
		GUIFunctions.AddToolTip("IBM_Version_Check", "Check this option to automatically check for updates to Script Hub and enabled addons when Script Hub starts")
		Gui, ICScriptHub:Add, Button, xs+10 yp+20 w70 vIBM_Version_Check_Now gIBM_Version_Check_Now, Check now
		
		;Versions - addons, dynamic based on number of enabled addons. Addons cannot be enabled / disabled without restarting script hub, so this can be done at set up only, and having other items follow is therefore possible
		Gui, ICScriptHub:Font, w700
		AddonGroupHeight:=AddonManagement.EnabledAddons.Count() * 20 + 25
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+9 w%groupWidth% h%AddonGroupHeight% vIBM_Version_Groupbox, Addon Versions ;Height must be adjusted later
		Gui, ICScriptHub:Font, w400
		currentIndex:=1
		GuiControlGet, AddonGroupPos, ICScriptHub:Pos, IBM_Version_Groupbox
		yPos:=AddonGroupPosY + 16
		for _,v in AddonManagement.Addons ;AddonManagement.EnabledAddons does not contain the directory for the Readme check
        {
			if(v.Enabled)
			{
				Gui, ICScriptHub:Add, Text, xs+10 y%yPos% w10 h18 0x200 vIBM_Version_Status_Addon_%currentIndex%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_UI_LIGHT
				Gui, ICScriptHub:Add, Text, w350 x+5 y%yPos% h18 0x200 vIBM_Version_Text_Addon_%currentIndex%, % v.Name ": " . v.Version ;Will be updated later
				readMeLocation:=A_ScriptDir . "\Addons\" . v.Dir . "\ReadMe.md"
				if(FileExist(readMeLocation))
					Gui, ICScriptHub:Add, Link, xs+430 yp+0 w45 h18 0x200 vIBM_Version_Readme_Addon_%currentIndex%, % "<a href=""" . readMeLocation . """>Readme</a>"
				yPos+=20
				currentIndex++
			}
        }

		;ROUTE TAB
		Gui, ICScriptHub:Tab, BM Route
		;Combine
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+10 w%groupWidth% h50 vIBM_z1Group, Starting Strategy
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, CheckBox, xs+10 ys+20 h18 vIBM_Route_Combine gIBM_Route_Combine, Combine Thellora and Briv
		GUIFunctions.AddToolTip("IBM_Route_Combine","Combining Thellora and Briv causes them to jump together from zone 1, otherwise only Thellora will jump from zone 1")
		Gui, ICScriptHub:Add, CheckBox, x+20 h18 vIBM_Route_Combine_Boss_Avoidance gIBM_Generic_Setting_Int, Avoid Bosses
		GUIFunctions.AddToolTip("IBM_Route_Combine_Boss_Avoidance","When this option is selected the script will check if Thellora will combine onto a boss, and break the combine if doing so will cause her to land on a non-boss zone instead. If using this mode with Feat Swapping and an M jump greater than the E jump, an additional jump's worth of stacks are generated in the prior run if possible")
		;Route settings for jump/stacking zones
		sideBarWidth:=72
		mainWidth:=groupWidth-sideBarWidth-5
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+12 w%mainWidth% h270 vIBM_Route_Group, Route
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+20 h18 0x200, % "Select zones to jump with the Q formation ("
		textColour:=IC_IriBrivMaster_GUI.IBM_COLOUR_ROUTE_YES_JUMP
		Gui, ICScriptHub:Add, Text, x+0 h18 0x200 %textColour%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_ROUTE_JUMP
		Gui, ICScriptHub:Add, Text, x+0 h18 0x200, % ") and to perform online stacking ("
		textColour:=IC_IriBrivMaster_GUI.IBM_COLOUR_ROUTE_YES_STACK
		Gui, ICScriptHub:Add, Text, x+0 h18 0x200 %textColour%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_ROUTE_STACK
		Gui, ICScriptHub:Add, Text, x+0 h18 0x200, % ")"
		this.CreateRouteBoxes(40)
		this.RefreshRouteJumpBoxes()
		this.RefreshRouteStackBoxes()
		Gui, ICScriptHub:Add, Button, w185 xs+10 y+3 vIBM_Route_Import_Button gIBM_Route_Import_Button, Import
		Gui, ICScriptHub:Add, Button, w185 x+5  vIBM_Route_Export_Button gIBM_Route_Export_Button, Export
		GUIFunctions.UseThemeTextColor() ;The route grid appears to mess with themes (due to the use of GUI Font?), so reset here
		;>Jump sidebar
		sideBarOffset:=mainWidth+10
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+%sideBarOffset% ys+0 w%sideBarWidth% h270, Briv Jumps
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+15 ys+20 h18 0x200 w10, Q:
		Gui, ICScriptHub:Add, Edit, +cBlack  w20 x+3 Number Limit2 vIBM_Route_BrivJump_Q gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, Text, xs+15 y+10 h18 0x200 w10, E:
		Gui, ICScriptHub:Add, Edit, +cBlack  w20 x+3 Number Limit2 vIBM_Route_BrivJump_E gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, Text, xs+15 y+10 h18 0x200 w10, M:
		Gui, ICScriptHub:Add, Edit, +cBlack  w20 x+3 Number Limit2 vIBM_Route_BrivJump_M gIBM_Generic_Setting_Int
		GUIFunctions.AddToolTip("IBM_Route_BrivJump_Q", "The number of additional zones Briv jumps using the Q formation")
		GUIFunctions.AddToolTip("IBM_Route_BrivJump_E", "The number of additional zones Briv jumps using the E formation when feat swapping. Ignored if Briv is not saved in E")
		GUIFunctions.AddToolTip("IBM_Route_BrivJump_M", "The number of additional zones Briv jumps using the M (Modron) formation when feat swapping. Used when combining to determine the initial jump.")
		;Due to the sidebar we need to get the Y location of the buttons at the bottom of the jump/stack box
		GuiControlGet, RouteEndPosition, ICScriptHub:Pos, IBM_Route_Import_Button
		nextY:=RouteEndPositionY+RouteEndPositionH+9
		;Offline stacking zones (with Flames-based options)
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y%nextY% w%groupWidth% h101, Stacking Zones
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+20 h18 0x200, Offline:
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 yp+0 Number Limit4 vIBM_Offline_Stack_Zone gIBM_Generic_Setting_Int
		GUIFunctions.AddToolTip("IBM_Offline_Stack_Zone","Offline stacking will be performed on or after this zone during normal operation. When flames-based stacking is enabled this will be used for 0 flames cards")
		Gui, ICScriptHub:Add, Text, x+10 h18 0x200, Minimum stack zone:
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 yp+0 Number Limit4 vIBM_OffLine_Stack_Min gIBM_Generic_Setting_Int
		GUIFunctions.AddToolTip("IBM_OffLine_Stack_Min","The minimum zone Briv can farm stacks on; that is the lowest zone that the W formation does not kill enemies. Used for recovery")
		Gui, ICScriptHub:Add, CheckBox, xs+10 y+5 h18 0x200 vIBM_OffLine_Flames_Use gIBM_OffLine_Flames_Use, Flames-based:
		GUIFunctions.AddToolTip("IBM_OffLine_Flames_Use", "Ellywick's Flames cards increase the damage enemies deal, reducing the stacks Briv gains during offline stacking. This option allows this to be accounted for. Spending the time calibrating your stack zone for the rare instances of 3 or more cards is unlikely to be worthwhile; set them to a lower zone so that Briv does not die. Remember that the Gem feat makes the 5-card value unnecessary")
		Gui, ICScriptHub:Add, Text, x+15 h18 0x200, 1
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 Number Limit4 vIBM_OffLine_Flames_Zone_Edit_1 gIBM_OffLine_Flames_Zone_Any_Edit Disabled
		Gui, ICScriptHub:Add, Text, x+9 h18 0x200, 2
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 Number Limit4 vIBM_OffLine_Flames_Zone_Edit_2 gIBM_OffLine_Flames_Zone_Any_Edit Disabled
		Gui, ICScriptHub:Add, Text, x+9 h18 0x200, 3
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 Number Limit4 vIBM_OffLine_Flames_Zone_Edit_3 gIBM_OffLine_Flames_Zone_Any_Edit Disabled
		Gui, ICScriptHub:Add, Text, x+9 h18 0x200, 4
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 Number Limit4 vIBM_OffLine_Flames_Zone_Edit_4 gIBM_OffLine_Flames_Zone_Any_Edit Disabled
		Gui, ICScriptHub:Add, Text, x+9 h18 0x200, 5
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 Number Limit4 vIBM_OffLine_Flames_Zone_Edit_5 gIBM_OffLine_Flames_Zone_Any_Edit Disabled
		Gui, ICScriptHub:Add, CheckBox, xs+10 y+5 h18 0x200 vIBM_Online_Melf_Use gIBM_Online_Melf_Use, Online Stack with Melf:
		GUIFunctions.AddToolTip("IBM_Online_Melf_Use","When enabled online stacking will be performed when Melf's increased spawn count effect is active, within the range specified")
		Gui, ICScriptHub:Add, Text, x+10 h18 0x200, Min
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 Number Limit4 vIBM_Online_Melf_Min gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, Text, x+10 h18 0x200, Max
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 Number Limit4 vIBM_Online_Melf_Max gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, CheckBox, x+10 h18 0x200 vIBM_Online_Ultra_Enabled gIBM_Generic_Setting_Int, Ultra Stack
		GUIFunctions.AddToolTip("IBM_Online_Ultra_Enabled", "With sufficient BUD, attempts to swap to the W formation when exiting the zone prior to the stack zone and then exit the stack zone using Briv's ultimate. This requires specific stack zone settings")
		;Offline config
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+12 w%groupWidth% h99, Offline Settings
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+20 h18 0x200, Platform login:
		Gui, ICScriptHub:Add, Edit, +cBlack  w40 x+3 Number Limit5 vIBM_OffLine_Delay_Time gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, Text, x+3 h18 0x200, ms
		GUIFunctions.AddToolTip("IBM_OffLine_Delay_Time", "The time to wait during an offline restart between the previous instance of the game saving, and the new one completing platform login. Set this high enough to consistently trigger stacking, but no higher")
		Gui, ICScriptHub:Add, Text, x+15 h18 0x200, Restart sleep:
		Gui, ICScriptHub:Add, Edit, +cBlack  w35 x+3 Number Limit4 vIBM_OffLine_Sleep_Time gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, Text, x+3 h18 0x200, ms
		GUIFunctions.AddToolTip("IBM_OffLine_Sleep_Time", "The time to wait between the game closing and launching a new copy. This should only be increased from 0 if the lack of delay causes platform issues")
		Gui, ICScriptHub:Add, Text, x+15 h18 0x200, Timeout factor:
		Gui, ICScriptHub:Add, Edit, +cBlack  w20 x+3 Number Limit2 vIBM_OffLine_Timeout gIBM_Generic_Setting_Int
		GUIFunctions.AddToolTip("IBM_OffLine_Timeout", "Controls the time allowed for the game to start and close. The start time is 10s times this value, and the initial close time is 2s times this value")
		Gui, ICScriptHub:Add, Text, xs+10 y+5 h18 0x200, Offline every:
		Gui, ICScriptHub:Add, Edit, +cBlack  w25 x+3 Number Limit3 vIBM_OffLine_Freq_Edit gIBM_OffLine_Freq_Edit
		Gui, ICScriptHub:Add, Text, x+5 h18 0x200, runs
		GUIFunctions.AddToolTip("IBM_OffLine_Freq_Edit", "Often referred to as FORT (Force Offline Run Threshold)")
		Gui, ICScriptHub:Add, CheckBox, x+15 h20 0x200 vIBM_Route_Offline_Restore_Window gIBM_Generic_Setting_Int, Restore window
		GUIFunctions.AddToolTip("IBM_Route_Offline_Restore_Window", "Sets the default Restore Window option to be used when the script starts")
		Gui, ICScriptHub:Add, CheckBox, xs+10 y+5 h18 0x200 vIBM_OffLine_Blank gIBM_OffLine_Blank, Blank restarts
		GUIFunctions.AddToolTip("IBM_OffLine_Blank", "Blank offline runs do not attempt to stack, and will online stack if needed along with a restart of the game. Use this to clear memory bloat in the game when offline stacking is slower overall than online")
		Gui, ICScriptHub:Add, CheckBox, x+10 h18 0x200 vIBM_OffLine_Blank_Relay gIBM_OffLine_Blank, Relay restarts
		GUIFunctions.AddToolTip("IBM_OffLine_Blank_Relay", "Relay blank restarts launch a new instance of the game prior to closing the current one. Not compatible with the Epic Games Launcher")
		Gui, ICScriptHub:Add, Text, x+10 h18 0x200, Relay start offset:
		Gui, ICScriptHub:Add, Edit, +cBlack  w25 x+3 Number Limit4 vIBM_OffLine_Blank_Relay_Zones gIBM_OffLine_Blank
		GUIFunctions.AddToolTip("IBM_OffLine_Blank_Relay_Zones", "The number of zones prior to the Offline zone that the relay will start. If stacking with Melf and the online stacking zone is within the Relay window, this will be be offset from that stacking zone instead. In any case the relay will not start until after Thellora's landing zone")
		;Ellywick Casino
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+7 w%groupWidth% h50, % "Ellywick's Casino"
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+20 h18 0x200, Target Gem cards:
		Gui, ICScriptHub:Add, Edit, +cBlack  w15 x+2 Number Limit1 vIBM_Casino_Target_Base gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, Text, x+10 h18 0x200, Maximum redraws:
		Gui, ICScriptHub:Add, Edit, +cBlack  w15 x+3 Number Limit1 vIBM_Casino_Redraws_Base gIBM_Generic_Setting_Int
		Gui, ICScriptHub:Add, Text, x+10 h18 0x200, Minimum cards:
		Gui, ICScriptHub:Add, Edit, +cBlack  w15 x+3 Number Limit1 vIBM_Casino_MinCards_Base gIBM_Generic_Setting_Int

		;LEVELS TAB
		Gui, ICScriptHub:Tab, BM Levels
		;Levelling Options
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+7 w%groupWidth% h102, Levelling Options
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+20 h18 0x200, Max sequential keys
		Gui, ICScriptHub:Add, Edit, +cBlack  w40 x+5 Number w20 Limit2 vIBM_LevelManager_Input_Max gIBM_LevelManager_Input_Max
		GUIFunctions.AddToolTip("IBM_LevelManager_Input_Max", "The maximum number of key presses to be send to the game in a batch during levelling. Minimum of 2.")
		Gui, ICScriptHub:Add, Text, x+10 h18 0x200, Modifier key
		Gui, ICScriptHub:Add, DropDownList, x+5 w45 vIBM_Level_Options_Mod_Key gIBM_Generic_Setting_String, Shift|Ctrl|Alt
		GUIFunctions.AddToolTip("IBM_Level_Options_Mod_Key", "The modifier keybind to use for levelling less than 100 levels at a time. Set all champions to multiples of 100 levels if you do not wish to use this feature")
		Gui, ICScriptHub:Add, Text, x+5 h18 0x200, for x
		Gui, ICScriptHub:Add, DropDownList, x+1 w35 vIBM_Level_Options_Mod_Value gIBM_Generic_Setting_Int, 10|25
		GUIFunctions.AddToolTip("IBM_Level_Options_Mod_Value", "The levelling amount associated with the key selected. This must match the in-game keybind")
		Gui, ICScriptHub:Add, CheckBox, xs+10 y+8 h18 0x200 vIBM_Level_Options_BrivBoost_Use gIBM_Level_Options_BrivBoost_Use, Briv Level Boost
		GUIFunctions.AddToolTip("IBM_Level_Options_BrivBoost_Use", "When enabled will increase Briv's level during online stacking. Use when Briv's normal level is insufficent for later stack zones")
		Gui, ICScriptHub:Add, Text, x+15 h18 0x200, Safety Factor
		Gui, ICScriptHub:Add, Edit, +cBlack  w20 x+1 Number Limit2 vIBM_LevelManager_Boost_Multi gIBM_Generic_Setting_Int
		GUIFunctions.AddToolTip("IBM_LevelManager_Boost_Multi", "This is how many times greater Briv's HP should be than the incoming damage of 100 enemies. Useful range 8 (fast stacking) to 12 (slower stacking)")
		Gui, ICScriptHub:Add, CheckBox, x+10 h18 0x200 vIBM_Level_Diana_Cheese gIBM_Generic_Setting_Int, Dynamic Diana
		GUIFunctions.AddToolTip("IBM_Level_Diana_Cheese", "Diana can give excess chests after the daily reset. This option will raise her level to 200 for Electrum Chest Scavenger from 3 minutes before the daily reset to 30 minutes after. Her level in the main options should be left at 100")
		Gui, ICScriptHub:Add, CheckBox, x+10 h18 0x200 vIBM_Level_Recovery_Softcap gIBM_Generic_Setting_Int, Recovery Levelling
		GUIFunctions.AddToolTip("IBM_Level_Recovery_Softcap", "With this option selected, champions will be levelled to their last update when reaching a boss zone in stack conversion recovery, that is when Briv has no stacks and the minimum stack zone has yet to be reached. This can aid killing armoured bosses, but will raise the minimum zone required to gain online stacks")
		Gui, ICScriptHub:Add, CheckBox, xs+10 y+8 h18 0x200 vIBM_Level_Options_Limit_Tatyana gIBM_Generic_Setting_Int, Smart Tatyana in Casino
		GUIFunctions.AddToolTip("IBM_Level_Options_Limit_Tatyana", "Only level Tatyana at the start of a run if Melf's Spawn More effect is not active in the Casino zone. To use this option her Start level should be set to 0")
		Gui, ICScriptHub:Add, CheckBox, x+10 h18 0x200 vIBM_Level_Options_Suppress_Front gIBM_Generic_Setting_Int, Surpress Front Row
		GUIFunctions.AddToolTip("IBM_Level_Options_Suppress_Front", "Do not level champions other than Briv in the front row. Used to maximise Briv's stack gain in the Casino")
		Gui, ICScriptHub:Add, CheckBox, x+10 h18 0x200 vIBM_Level_Options_Ghost gIBM_Generic_Setting_Int, Ghost Level
		GUIFunctions.AddToolTip("IBM_Level_Options_Ghost", "During the Casino, level champions that are not part of the formation so long as they will not be placed, either due to all slots being full or only slots at the front being available and the formation being under attack. This option makes it more likely all speed effects will be ready for the first normal zone. Only applied when combining")
		;Level manager - headings
		Gui, ICScriptHub:Font, w700
		Gui, ICScriptHub:Add, Groupbox, Section xm+5 y+12 w%groupWidth% h70 vIBM_LevelManager, Level Manager
		Gui, ICScriptHub:Font, w400
		Gui, ICScriptHub:Add, Text, xs+10 ys+20 h20 w15 Left 0x200, S
		Gui, ICScriptHub:Add, Text, x+1 h20 w90 Left 0x200, Champion
		Gui, ICScriptHub:Add, Text, w40 h20 x+1 0x200 vIBM_LevelRow_H_z1, Start
		GUIFunctions.AddToolTip("IBM_LevelRow_H_z1", "Levels used for the first zone")
		Gui, ICScriptHub:Add, Text, w55 h20 x+1 0x200 vIBM_LevelRow_H_Priority, Priority
		GUIFunctions.AddToolTip("IBM_LevelRow_H_Priority", "Levelling priority for the first zone. Options with levels beside them will use the selected priority only until that level is reached, at which point it will be treated as 0")
		Gui, ICScriptHub:Add, Text, w40 h20 x+1 0x200, Normal
		Gui, ICScriptHub:Add, Text, w83 x+5 0x200 h20 Center 0x200, Formations
		Gui, ICScriptHub:Add, Text, w62 x+5 0x200 h20 Center 0x200, Feats
		Gui, ICScriptHub:Add, Button, w50 x+10 vIBM_LevelManager_Refresh gIBM_LevelManager_Refresh, Refresh
		;Level manager - create the maximum of 40 rows (4 formations x 10 champions), we will hide what we don't need when populating TODO: Decide if we really need 40 here, it's a complete solution...but also pointlessly overkill. 12 is relatively high (as of 23Aug25)
		this.LevelRow_Priority_Value:=[5,4,3,2,1,0,-1,-2,-3,-4,-5,5,4,3,2,1,5,4,3,2,1]
		this.LevelRow_Priority_Limit:=["","","","","","","","","","","",100,100,100,100,100,200,200,200,200,200]
		loop 40
			this.CreateLevelRow(A_Index)
		this.RefreshLevelRows() ;Also resizes things
	}

	RefreshRouteJumpBoxes()
	{
		loop, 50
		{
			textColour:=g_IriBrivMaster.Settings["IBM_Route_Zones_Jump",A_Index] ? IC_IriBrivMaster_GUI.IBM_COLOUR_ROUTE_YES_JUMP : IC_IriBrivMaster_GUI.IBM_COLOUR_ROUTE_NO
			GuiControl, ICScriptHub: +%textColour%, IBM_Route_J_%A_Index%
			GuiControl, , IBM_Route_J_%A_Index%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_ROUTE_JUMP
		}
	}

	RefreshRouteStackBoxes()
	{
		loop, 50
		{
			textColour:=g_IriBrivMaster.Settings["IBM_Route_Zones_Stack",A_Index] ? IC_IriBrivMaster_GUI.IBM_COLOUR_ROUTE_YES_STACK : IC_IriBrivMaster_GUI.IBM_COLOUR_ROUTE_NO
			GuiControl, ICScriptHub: +%textColour%, IBM_Route_S_%A_Index%
			GuiControl, , IBM_Route_S_%A_Index%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_ROUTE_STACK
		}
	}

	CreateRouteBoxes(sectionOffsetY) ;sectionOffsetY is the number of pixels from the top of the current section to start the grid
	{
		global
		rowSpacing:=40
		counter:=1
		loop, 5
		{
			rowOffset:=rowSpacing*(A_Index-1)+sectionOffsetY
			Gui, ICScriptHub:Font, s9
			Gui, ICScriptHub:Add, Text, w35 xs+10 ys+%rowOffset% h35 Center 0x1000 vIBM_Route_%counter%_Zone, %counter%
			Gui, ICScriptHub:Font, s16
			textColour:=IC_IriBrivMaster_GUI.IBM_COLOUR_ROUTE_NO ;Default colour
			Gui, ICScriptHub:Add, Text, %textColour% xp+2 yp+14  Center gIBM_Route_J_Click vIBM_Route_J_%counter%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_ROUTE_JUMP
			Gui, ICScriptHub:Add, Text, %textColour% xp+18 yp+0  Center gIBM_Route_S_Click vIBM_Route_S_%counter%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_ROUTE_STACK
			counter++
			loop, 9
			{
				Gui, ICScriptHub:Font, s9
				Gui, ICScriptHub:Add, Text, w35 x+6 ys+%rowOffset% h35 Center 0x1000 vIBM_Route_%counter%_Zone, %counter%
				Gui, ICScriptHub:Font, s16
				textColour:=IC_IriBrivMaster_GUI.IBM_COLOUR_ROUTE_NO ;Default colour
				Gui, ICScriptHub:Add, Text, %textColour% xp+2 yp+14  Center gIBM_Route_J_Click vIBM_Route_J_%counter%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_ROUTE_JUMP
				Gui, ICScriptHub:Add, Text, %textColour% xp+18 yp+0  Center gIBM_Route_S_Click vIBM_Route_S_%counter%, % IC_IriBrivMaster_GUI.IBM_SYMBOL_ROUTE_STACK
				counter++
			}
		}
		Gui, ICScriptHub:Font
	}

	UpdateGUISettings(data)
    {
        this.controlLock:=true ;Prevent control g-labels messing things up whilst populating. This is particularly import when one label processes multiple controls, as it can read values out of yet-to-be-populated controls and thus blank that setting
		;Stacking Zone group
		GuiControl, ICScriptHub:, IBM_OffLine_Stack_Zone_Edit, % data.IBM_Offline_Stack_Zone
		GuiControl, ICScriptHub:, IBM_OffLine_Stack_Min_Edit, % data.IBM_Offline_Stack_Min
		GuiControl, ICScriptHub:, IBM_OffLine_Flames_Use, % data.IBM_OffLine_Flames_Use
		Loop, 5
		{
			GuiControl, ICScriptHub:, IBM_OffLine_Flames_Zone_Edit_%A_Index%, % data.IBM_OffLine_Flames_Zones[A_Index]
		}
		IBM_OffLine_Flames_Enable_Edit(data.IBM_OffLine_Flames_Use) ;And for the flames zone boxes
		GuiControl, ICScriptHub:, IBM_Online_Melf_Use, % data.IBM_Online_Use_Melf
		GuiControl, ICScriptHub:, IBM_Online_Melf_Min_Edit, % data.IBM_Online_Melf_Min
		GuiControl, ICScriptHub:, IBM_Online_Melf_Max_Edit, % data.IBM_Online_Melf_Max
		IBM_Online_Melf_Enable(data.IBM_Online_Use_Melf)
		GuiControl, ICScriptHub:, IBM_Online_Ultra_Enabled, % data.IBM_Online_Ultra_Enabled
		;Briv jumps
		GuiControl, ICScriptHub:, IBM_Route_BrivJump_Q_Edit, % data.IBM_Route_BrivJump_Q
		GuiControl, ICScriptHub:, IBM_Route_BrivJump_E_Edit, % data.IBM_Route_BrivJump_E
		GuiControl, ICScriptHub:, IBM_Route_BrivJump_M_Edit, % data.IBM_Route_BrivJump_M
		;RouteMaster tab
		GuiControl, ICScriptHub:, IBM_Route_Combine, % data.IBM_Route_Combine
		IBM_Combine_Enable(data.IBM_Route_Combine)
		GuiControl, ICScriptHub:, IBM_Route_Combine_Boss_Avoidance, % data.IBM_Route_Combine_Boss_Avoidance
		;Levelling options
		GuiControl, ICScriptHub:, IBM_Level_Options_Input_Max, % data.IBM_LevelManager_Input_Max
		GuiControl, ICScriptHub:, IBM_Level_Options_BrivBoost_Use, % data.IBM_LevelManager_Boost_Use
		GuiControl, ICScriptHub:, IBM_Level_Options_BrivBoost_Multi, % data.IBM_LevelManager_Boost_Multi
		IBM_Level_Options_BrivBoost_Enable(data.IBM_LevelManager_Boost_Use)
		GuiControl, ICScriptHub:, IBM_Level_Options_Limit_Tatyana, % data.IBM_Level_Options_Limit_Tatyana
		GuiControl, ICScriptHub:, IBM_Level_Options_Suppress_Front, % data.IBM_Level_Options_Suppress_Front
		GuiControl, ICScriptHub:, IBM_Level_Options_Ghost, % data.IBM_Level_Options_Ghost
		GuiControl, ICScriptHub:ChooseString, IBM_Level_Options_Mod_Key, % data.IBM_Level_Options_Mod_Key
		GuiControl, ICScriptHub:ChooseString, IBM_Level_Options_Mod_Value, % data.IBM_Level_Options_Mod_Value
		GuiControl, ICScriptHub:, IBM_Level_Diana_Cheese, % data.IBM_Level_Diana_Cheese
		GuiControl, ICScriptHub:, IBM_Level_Recovery_Softcap, % data.IBM_Level_Recovery_Softcap
		;Chests
		this.UpdateChestSnatcherOptions(data)
		;Game settings
		profile:=data.IBM_Game_Settings_Option_Profile
		GuiControl, ICScriptHub:, IBM_Game_Settings_Profile_1, % profile==1
		GuiControl, ICScriptHub:, IBM_Game_Settings_Profile_2, % !(profile==1)
		this.GameSettings_Update(data)
		;Offsets
		GuiControl, ICScriptHub:, IBM_Offsets_Check, % data.IBM_Offsets_Check
		GuiControl, ICScriptHub:, IBM_Offsets_Lock_Pointers, % data.IBM_Offsets_Lock_Pointers
		;Versions
		GuiControl, ICScriptHub:, IBM_Version_Check, % data.IBM_Version_Check
		;Levelling
		this.RefreshLevelRows()
		this.RefreshRouteJumpBoxes()
		this.RefreshRouteStackBoxes()
		;Ellywick's Casino
		GuiControl, ICScriptHub:, IBM_Casino_Target_Base, % data.IBM_Casino_Target_Base
		GuiControl, ICScriptHub:, IBM_Casino_Redraws_Base, % data.IBM_Casino_Redraws_Base
		GuiControl, ICScriptHub:, IBM_Casino_MinCards_Base, % data.IBM_Casino_MinCards_Base
		;Window
		GuiControl, ICScriptHub:, IBM_Window_X, % data.IBM_Window_X
		GuiControl, ICScriptHub:, IBM_Window_Y, % data.IBM_Window_Y
		GuiControl, ICScriptHub:, IBM_Window_Hide, % data.IBM_Window_Hide
		GuiControl, ICScriptHub:, IBM_Window_Dark_Icon, % data.IBM_Window_Dark_Icon
		;Offline
		GuiControl, ICScriptHub:, IBM_OffLine_Delay_Time_Edit, % data.IBM_OffLine_Delay_Time
		GuiControl, ICScriptHub:, IBM_OffLine_Sleep_Time_Edit, % data.IBM_OffLine_Sleep_Time
		GuiControl, ICScriptHub:, IBM_OffLine_Freq_Edit, % data.IBM_OffLine_Freq
		GuiControl, ICScriptHub:, IBM_OffLine_Blank, % data.IBM_OffLine_Blank
		GuiControl, ICScriptHub:, IBM_OffLine_Blank_Relay, % data.IBM_OffLine_Blank_Relay
		GuiControl, ICScriptHub:, IBM_OffLine_Blank_Relay_Zones, % data.IBM_OffLine_Blank_Relay_Zones
		IBM_Offline_Blank_EnableControls(data.IBM_OffLine_Blank,data.IBM_OffLine_Blank_Relay)
		GuiControl, ICScriptHub:, IBM_OffLine_Timeout_Edit, % data.IBM_OffLine_Timeout
		;Run control
		GuiControl, ICScriptHub:, IBM_RunControl_RestoreWindow_Default, % data.IBM_Route_Offline_Restore_Window
		;Game Location
		GuiControl, ICScriptHub:, IBM_Game_Exe, % data.IBM_Game_Exe
		GuiControl, ICScriptHub:, IBM_Game_Path, % data.IBM_Game_Path
		GuiControl, ICScriptHub:, IBM_Game_Launch, % data.IBM_Game_Launch
		GuiControl, ICScriptHub:, IBM_Game_Hide_Launcher, % data.IBM_Game_Hide_Launcher
		this.UpdateNonGemFarmEllySettings(data.IBM_Ellywick_NonGemFarm_Cards)
		this.controlLock:=false
    }

	UpdateChestSnatcherOptions(data) ;ChestSnatcher options in a separate function so that the window can be updated when opened to overwrite unaccepted changes
	{
		GuiControl, IBM_ChestSnatcher_Options:, IBM_ChestSnatcher_Options_Claim, % data.IBM_DailyRewardClaim_Enable
		GuiControl, IBM_ChestSnatcher_Options:, IBM_ChestSnatcher_Options_Min_Gem, % data.IBM_ChestSnatcher_Options_Min_Gem
		GuiControl, IBM_ChestSnatcher_Options:, IBM_ChestSnatcher_Options_Min_Gold, % data.IBM_ChestSnatcher_Options_Min_Gold
		GuiControl, IBM_ChestSnatcher_Options:, IBM_ChestSnatcher_Options_Min_Silver, % data.IBM_ChestSnatcher_Options_Min_Silver
		GuiControl, IBM_ChestSnatcher_Options:, IBM_ChestSnatcher_Options_Min_Buy, % data.IBM_ChestSnatcher_Options_Min_Buy
		GuiControl, IBM_ChestSnatcher_Options:, IBM_ChestSnatcher_Options_Open_Gold, % data.IBM_ChestSnatcher_Options_Open_Gold
		GuiControl, IBM_ChestSnatcher_Options:, IBM_ChestSnatcher_Options_Open_Silver, % data.IBM_ChestSnatcher_Options_Open_Silver
	}

	CreateLevelRow(index)
	{
		global
		Gui, ICScriptHub:Add, Text, xs+10 y+5 h20 w15 Left 0x200 Hidden vIBM_LevelRow_%index%_Seat, %seat%
		Gui, ICScriptHub:Add, Text, x+1 h20 w90 Left 0x200 Hidden vIBM_LevelRow_%index%_Name, % data["Name"]
		Gui, ICScriptHub:Add, Edit, +cBlack  w40 x+1 Number Limit4 Hidden vIBM_LevelRow_%index%_z1
		Gui, ICScriptHub:Add, DropDownList, w55 x+1 Hidden AltSubmit hwndIBM_LevelRow_DLL_%index% vIBM_LevelRow_%index%_Priority, 5|4|3|2|1|0||-1|-2|-3|-4|-5|5↓100|4↓100|3↓100|2↓100|1↓100|5↓200|4↓200|3↓200|2↓200|1↓200
		DDLindex:=IBM_LevelRow_DLL_%index%
		DDLHeight:=17.5*this.GetDPIScale()
		PostMessage, 0x0153, -1, %DDLHeight%,, ahk_id %DDLindex% ;Set height (since H200 or R4 is setting height of dropdown list)
		Gui, ICScriptHub:Add, Edit, +cBlack  w40 x+1 Number Limit4 Hidden vIBM_LevelRow_%index%_min
		Gui, ICScriptHub:Font, Bold
		Gui, ICScriptHub:Add, Text, w20 x+5 h20 Center Hidden 0x200 0x1000 vIBM_LevelRow_%index%_Q, Q
		Gui, ICScriptHub:Add, Text, w20 x+1 h20 Center Hidden 0x200 0x1000 vIBM_LevelRow_%index%_W, W
		Gui, ICScriptHub:Add, Text, w20 x+1 h20 Center Hidden 0x200 0x1000 vIBM_LevelRow_%index%_E, E
		Gui, ICScriptHub:Add, Text, w20 x+1 h20 Center Hidden 0x200 0x1000 vIBM_LevelRow_%index%_M, M
		Gui, ICScriptHub:Font, Normal
		Gui, ICScriptHub:Add, Text, x+5 w20 h20 0x200 CENTER Hidden vIBM_LevelRow_%index%_Feats_Selected
		Gui, ICScriptHub:Add, Button, x+1 w20 h20 Hidden vIBM_LevelRow_%index%_Feats_Set gIBM_LevelRow_Feats_Set, % IC_IriBrivMaster_GUI.IBM_SYMBOL_UI_LEFT
		Gui, ICScriptHub:Add, Button, x+1 w20 h20 Hidden vIBM_LevelRow_%index%_Feats_Clear gIBM_LevelRow_Feats_Clear, % IC_IriBrivMaster_GUI.IBM_SYMBOL_UI_CLEAR
	}

	RefreshLevelRows()
	{
		this.levelDataSet:=g_IriBrivMaster.IBM_GetGUIFormationData() ;Gets formation data, without levels
		If IsObject(this.levelDataSet)
		{
			this.LoadCurrentLevels()
			index:=1
			for seat, seatMembers in this.levelDataSet
			{
				for champID, champData in seatMembers
				{
					lastY:=this.RefreshLevelRow(index,seat,champData)
					index++
				}
			}
			while (index<=40) ;Hide remaining rows
			{
				this.HideLevelRow(index)
				index++
			}
			;Resize group
			if (lastY) ;If the game isn't running this will not be set
			{
				GuiControlGet, initialSize, ICScriptHub:Pos, IBM_LevelManager
				updatedHeight:=lastY-initialSizeY+10
				GuiControl, ICScriptHub:Move, IBM_LevelManager, h%updatedHeight%
			}
		}
	}

	RefreshLevelRow(index,seat,data) ;Single row
	{
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_Seat, %seat%
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_Seat
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_Name, % data["Name"]
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_Name
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_z1, % data["z1"]
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_z1
		testString:=data["prio"] . (data["priolimit"] ?  "↓" . data["priolimit"] : "")
		GuiControl, ICScriptHub:ChooseString, IBM_LevelRow_%index%_Priority, %testString%
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_Priority
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_min, % data["min"]
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_min
		textColour:=data["Q"] ? IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_IN : IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_OUT
		GuiControl, ICScriptHub: +%textColour%, IBM_LevelRow_%index%_Q
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_Q
		textColour:=data["W"] ? IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_IN : IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_OUT
		GuiControl, ICScriptHub: +%textColour%, IBM_LevelRow_%index%_W
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_W
		textColour:=data["E"] ? IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_IN : IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_OUT
		GuiControl, ICScriptHub: +%textColour%, IBM_LevelRow_%index%_E
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_E
		textColour:=data["M"] ? IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_IN : IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_OUT
		GuiControl, ICScriptHub: +%textColour%, IBM_LevelRow_%index%_M
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_M
		featCount:=data["Feat_List"] ? data["Feat_List"].Count() : 0
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_Feats_Selected, % featCount . (data["Feat_Exclusive"] ? "" : "+")
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_Feats_Selected
		GUIFunctions.AddToolTip("IBM_LevelRow_" . index . "_Feats_Selected", this.GetFeatTooltip(data))
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_Feats_Set
		GuiControl, ICScriptHub:Show, IBM_LevelRow_%index%_Feats_Clear
		GuiControlGet, placement, ICScriptHub:Pos, IBM_LevelRow_%index%_z1
		return placementY+placementH ;Return the botton of the edit box controls, used to size things
	}

	GetFeatTooltip(data)
	{
		featTooltip:=""
		if(data["Feat_List"] AND data["Feat_List"].Count()>0)
		{
			for id,name in data["Feat_List"]
			{
				featTooltip.=name . " (" . id ")`n"
			}
		}
		return featTooltip
	}

	HideLevelRow(index) ;Single row
	{
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_Seat, ""
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_Seat
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_Name, ""
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_Name
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_z1, 0
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_z1
		GuiControl, ChooseString, IBM_LevelRow_%index%_Priority, 0
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_Priority
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_min, 0
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_min
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_max, 0
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_max
		textColour:=IC_IriBrivMaster_GUI.IBM_COLOUR_FORMATION_OUT
		GuiControl, ICScriptHub: +%textColour%, IBM_LevelRow_%index%_Q
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_Q
		GuiControl, ICScriptHub: +%textColour%, IBM_LevelRow_%index%_W
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_W
		GuiControl, ICScriptHub: +%textColour%, IBM_LevelRow_%index%_E
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_E
		GuiControl, ICScriptHub: +%textColour%, IBM_LevelRow_%index%_M
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_M
		GuiControl, ICScriptHub:, IBM_LevelRow_%index%_Feats_Selected, ""
		g_MouseToolTips.Remove(GUIFunctions.GetToolTipTarget("IBM_LevelRow_" . index . "_Feats_Selected")) ;Remove tooltip
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_Feats_Selected
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_Feats_Set
		GuiControl, ICScriptHub:Hide, IBM_LevelRow_%index%_Feats_Clear
	}

	GetLevelRowData() ;Extracts set levels
	{
		currentLevels:=[]
		If IsObject(this.levelDataSet) ;Do not refresh here, as the values entered will be based on the data displayed from the last refresh
		{
			index:=1 ;TODO: Switch this to using champData["ListIndex"]
			for seat, seatMembers in this.levelDataSet
			{
				for champID, champData in seatMembers
				{
					GuiControlGet, value,, IBM_LevelRow_%index%_z1
					if (value)
						currentLevels[champID,"z1"]:=value
					GuiControlGet, value,, IBM_LevelRow_%index%_Priority
					if (value)
						currentLevels[champID,"prio"]:=this.LevelRow_Priority_Value[value]
						currentLevels[champID,"priolimit"]:=this.LevelRow_Priority_Limit[value]
					GuiControlGet, value,, IBM_LevelRow_%index%_min
					if (value)
						currentLevels[champID,"min"]:=value
					currentLevels[champID,"Feat_List"]:=champData["Feat_List"]
					currentLevels[champID,"Feat_Exclusive"]:=champData["Feat_Exclusive"]
					index++
				}
			}
		}
		return currentLevels
	}

	LoadCurrentLevels() ;Loads currently saved levels into the main level data set
	{
		;Levels are saved per stategy, then by champ ID
		savedLevels:=g_IBM_Settings["IBM_LevelManager_Levels"]
		for seat, seatMembers in this.levelDataSet
		{
			for champID, champData in seatMembers
			{
				if savedLevels.hasKey(champID)
				{
					champData["z1"]:=savedLevels[champID,"z1"]
					champData["min"]:=savedLevels[champID,"min"]
					champData["prio"]:=savedLevels[champID,"prio"]
					champData["priolimit"]:=savedLevels[champID,"priolimit"]
					champData["Feat_List"]:=savedLevels[champID,"Feat_List"]
					champData["Feat_Exclusive"]:=savedLevels[champID,"Feat_Exclusive"]
				}
				else
				{
					champData["z1"]:=""
					champData["min"]:=""
					champData["prio"]:=0
					champData["priolimit"]:=""
					champData["Feat_List"]:=""
					champData["Feat_Exclusive"]:=false
				}
			}
		}
	}

	ResetStatusText()
	{
		GuiControl, ICScriptHub: +cBlack, IBM_RunControl_Offline_StatusPause
		GuiControl, ICScriptHub: +cBlack, IBM_RunControl_Offline_StatusQueue
		GuiControl, ICScriptHub: +cBlack, IBM_RunControl_RestoreWindow_Status
		GuiControl, ICScriptHub:Text, IBM_RunControl_Status, Unable to read data from main script
	}

	UpdateRestoreWindow(isEnabled,allowMoveDraw:=true)
	{
		If (isEnabled)
		{
			GuiControl, ICScriptHub:+cGreen, IBM_RunControl_RestoreWindow_Status
			GuiControl, ICScriptHub:Text, IBM_RunControl_RestoreWindow_Toggle, Disable
		}
		else
		{
			GuiControl, ICScriptHub:+cRed, IBM_RunControl_RestoreWindow_Status
			GuiControl, ICScriptHub:Text, IBM_RunControl_RestoreWindow_Toggle, Enable
		}
		GuiControl, ICScriptHub:Enable, IBM_RunControl_RestoreWindow_Toggle
		if(allowMoveDraw)
			GuiControl, ICScriptHub:MoveDraw,IBM_RunControl_RestoreWindow_Status
	}

	UpdateRunControlDisable(disableOffline,allowMoveDraw:=true) ;Offline stacking Pause/Resume
	{
		If (disableOffline)
		{
			GuiControl, ICScriptHub:+cRed, IBM_RunControl_Offline_StatusPause ;Note disabled is 'red' here because offline stacking is normally switched on
			GuiControl, ICScriptHub:Text, IBM_RunControl_Offline_Toggle, Resume
		}
		else
		{
			GuiControl, ICScriptHub:+cGreen, IBM_RunControl_Offline_StatusPause
			GuiControl, ICScriptHub:Text, IBM_RunControl_Offline_Toggle, Pause
		}
		GuiControl, ICScriptHub:Enable, IBM_RunControl_Offline_Toggle
		if(allowMoveDraw)
			GuiControl, ICScriptHub:MoveDraw,IBM_RunControl_Offline_StatusPause 
	}

	UpdateRunControlForce(queueOffline,allowMoveDraw:=true) ;Force Queue
	{
		If (queueOffline)
		{
			GuiControl, ICScriptHub:+cGreen, IBM_RunControl_Offline_StatusQueue
			GuiControl, ICScriptHub:Text, IBM_RunControl_Offline_Queue_Toggle, Cancel
		}
		else
		{
			GuiControl, ICScriptHub:+cRed, IBM_RunControl_Offline_StatusQueue
			GuiControl, ICScriptHub:Text, IBM_RunControl_Offline_Queue_Toggle, Queue
		}
		GuiControl, ICScriptHub:Enable, IBM_RunControl_Offline_Queue_Toggle
		if(allowMoveDraw)
			GuiControl, ICScriptHub:MoveDraw,IBM_RunControl_Offline_StatusQueue
	}

	UpdateRunStatus(cycleString,statusString,stackString)
	{
		GuiControl, ICScriptHub:Text, IBM_RunControl_Cycle, % cycleString
		GuiControl, ICScriptHub:Text, IBM_RunControl_Status, % statusString
		GuiControl, ICScriptHub:Text, IBM_RunControl_Stack, % stackString
	}

	SetEllyNonGemFarmStatus(statusString)
	{
		GuiControl, ICScriptHub:Text, IBM_NonGemFarm_Elly_Status, % statusString
	}

	IBM_ChestsSnatcher_Status_Update(forceLog:=false)
	{
		curMessage:=g_IriBrivMaster.ChestSnatcher_Messages[g_IriBrivMaster.ChestSnatcher_Messages.maxIndex()]
		;Single-item window
		Gui, ICScriptHub:Default
		Gui, ListView, IBM_ChestsSnatcher_Status
		GuiControl, -Redraw, IBM_ChestsSnatcher_Status
		LV_Delete(1)
		LV_Add(,curMessage.time,curMessage.action,curMessage.comment)
		GuiControl, +Redraw, IBM_ChestsSnatcher_Status
		;Multi-item window
		if (WinExist("ahk_id " . g_IriBrivMaster_GUI.IBM_ChestSnatcher_Log_Hwnd) OR forceLog)
		{
			Gui, IBM_ChestSnatcher_Log:Default
			Gui, ListView, IBM_ChestsSnatcher_Status_Expanded
			GuiControl, -Redraw, IBM_ChestsSnatcher_Status_Expanded
			count:=g_IriBrivMaster.ChestSnatcher_Messages.count()
			LV_Delete()
			loop %count%
			{
				curMessage:=g_IriBrivMaster.ChestSnatcher_Messages[count-A_Index+1]
				LV_Add(,curMessage.time,curMessage.action,curMessage.comment)
			}
			GuiControl, +Redraw, IBM_ChestsSnatcher_Status_Expanded
			Gui, ICScriptHub:Default
		}
	}

	GameSettings_Update(data)
	{
		loop 2 ;Two profiles
		{
			profileIndex:=A_Index
			GuiControl, ICScriptHub:Text, IBM_Game_Settings_Profile_%profileIndex%, % data.IBM_Game_Settings_Option_Set[profileIndex].Name
			for setting,value in data.IBM_Game_Settings_Option_Set[profileIndex]
			{
				GuiControl, IBM_Game_Settings_Options:, IBM_Game_Settings_Option_%setting%_%profileIndex%, %value%
			}
		}
	}

	GameSettings_Status(statusText, colour)
	{
		GuiControl, ICScriptHub: +%colour%, IBM_Game_Settings_Status
		GuiControl, ICScriptHub:Text, IBM_Game_Settings_Status, %statusText%
		;GuiControl, ICScriptHub:MoveDraw,IBM_Game_Settings_Status
	}

	GetDPIScale()
	{
		hdc := DllCall("GetDC", "ptr", 0)
		dpi := DllCall("GetDeviceCaps", "ptr", hdc, "int", 88) ; LOGPIXELSY
		DllCall("ReleaseDC", "ptr", 0, "ptr", hdc)
		return dpi / 96
	}

	ReadNonGemFarmEllySettings()
	{
		cardOptions:=[]
		loop, 5
		{
			GuiControlGet, value,, IBM_NonGemFarm_Elly_Min_%A_Index%
			cardOptions.Push(value+0)
			GuiControlGet, value,, IBM_NonGemFarm_Elly_Max_%A_Index%
			cardOptions.Push(value+0)
		}
		return cardOptions
	}

	UpdateNonGemFarmEllySettings(cardOptions)
	{
		index:=1
		loop, 5
		{
			GuiControl, ICScriptHub:, IBM_NonGemFarm_Elly_Min_%A_Index%, % cardOptions[index]
			index++
			GuiControl, ICScriptHub:, IBM_NonGemFarm_Elly_Max_%A_Index%, % cardOptions[index]
			index++
		}
	}
}

IBM_Server_Check()
{
	GuiControl, ICScriptHub:Text, IBM_Server_Text_PS, % "Play Server: " . g_IriBrivMaster.GetPlayServerFriendly()
}

IBM_Version_Check()
{
	GuiControlGet, value,, IBM_Version_Check
	g_IriBrivMaster.UpdateSetting("IBM_Version_Check",value+0)
}

IBM_Version_Check_Now()
{
	g_IriBrivMaster.RunVersionCheck()
}

IBM_LevelRow_Feats_Set()
{
	RegExMatch(A_GuiControl,"IBM_LevelRow_(\d{1,2})_Feats_Set",row)
	for _, seatMembers in g_IriBrivMaster_GUI.levelDataSet
	{
		for champID, champData in seatMembers
		{
			if(champData["ListIndex"]==row1)
			{
				g_IBM_Settings.IBM_LevelManager_Levels:=g_IriBrivMaster_GUI.GetLevelRowData() ;Makes sure new champions are in the data set before we attempt to make changes
				HERO_FEATS:=g_SF.Memory.GameManager.game.gameInstances[0].Controller.userData.FeatHandler.heroFeatSlots[champID].List
				size:=HERO_FEATS.size.Read()
				currentFeats:={}
				messageFeats:=""
				Loop, %size%
				{
					id:=HERO_FEATS[A_Index - 1].ID.Read()
					name:=HERO_FEATS[A_Index - 1].Name.Read()
					if(id) ;heroFeatSlots always has the 4 slots
					{
						currentFeats[id]:=name
						messageFeats.=name . " (" . id . ")`n"
					}
				}
				if (currentFeats.Count()>0)
				{
					message:="Selecting the following feats as required for " . champData["Name"] . ":`n" . messageFeats . message.="`nMake this selection exclusive?"
					Msgbox, 35, Feat Guard, %message% ;3 is Yes/No/Cancel, + 32 for Question icon
					IfMsgBox Yes
					{
						savedLevels[ChampID,"Feat_Exclusive"]:=true
					}
					IfMsgBox No
					{
						savedLevels[ChampID,"Feat_Exclusive"]:=false
					}
					IfMsgBox Cancel
					{
						return
					}
					savedLevels[ChampID,"Feat_List"]:=currentFeats
					g_IriBrivMaster_GUI.RefreshLevelRows()
					return
				}
				else
				{
					message:="No feats are currently equipped on " . champData["Name"] . "`nMake this selection exclusive?"
					savedLevels[ChampID,"Feat_List"]:=""
					Msgbox, 33, Feat Guard, %message% ;1 is OK/Cancel, + 32 for Question icon
					IfMsgBox OK
					{
						savedLevels[ChampID,"Feat_Exclusive"]:=true
						g_IriBrivMaster_GUI.RefreshLevelRows()
						return
					}
					savedLevels[ChampID,"Feat_Exclusive"]:=false
					g_IriBrivMaster_GUI.RefreshLevelRows()
					return
				}
			}
		}
	}
}

IBM_Offsets_Download()
{
	g_IriBrivMaster.DownloadOffsets()
}

IBM_Offsets_Check_Now()
{
	g_IriBrivMaster.CheckOffsetVersions()
}

IBM_Offsets_Check()
{
	GuiControlGet, value,, IBM_Offsets_Check
	g_IriBrivMaster.UpdateSetting("IBM_Offsets_Check",value+0)
}

IBM_Offsets_Lock_Pointers()
{
	GuiControlGet, value,, IBM_Offsets_Lock_Pointers
	g_IriBrivMaster.UpdateSetting("IBM_Offsets_Lock_Pointers",value+0)
}

IBM_LevelRow_Feats_Clear()
{
	RegExMatch(A_GuiControl,"IBM_LevelRow_(\d{1,2})_Feats_Clear",row)
	for _, seatMembers in g_IriBrivMaster_GUI.levelDataSet
	{
		for champID, champData in seatMembers
		{
			if(champData["ListIndex"]==row1)
			{
				g_IBM_Settings.IBM_LevelManager_Levels:=g_IriBrivMaster_GUI.GetLevelRowData() ;Makes sure new champions are in the data set before we attempt to make changes
				savedLevels:=g_IBM_Settings["IBM_LevelManager_Levels"]
				savedLevels[champID,"Feat_List"]:=""
				savedLevels[champID,"Feat_Exclusive"]:=false
			}
		}
	}
	g_IriBrivMaster_GUI.RefreshLevelRows()
}


IBM_Window_Settings()
{
	if (g_IriBrivMaster_GUI.controlLock)
		return
	GuiControlGet, value,, IBM_Window_X
	g_IriBrivMaster.UpdateSetting("IBM_Window_X",value+0)
	GuiControlGet, value,, IBM_Window_Y
	g_IriBrivMaster.UpdateSetting("IBM_Window_Y",value+0)
	GuiControlGet, value,, IBM_Window_Hide
	g_IriBrivMaster.UpdateSetting("IBM_Window_Hide",value+0)
	GuiControlGet, value,, IBM_Window_Dark_Icon
	g_IriBrivMaster.UpdateSetting("IBM_Window_Dark_Icon",value+0)
}

IBM_MainButtons_Start() {
    g_IriBrivMaster.Run_Clicked()
}

IBM_MainButtons_Stop() {
    g_IriBrivMaster.Stop_Clicked()
}

IBM_MainButtons_Connect() {
    g_IriBrivMaster.Connect_Clicked()
}

IBM_OffLine_Timeout_Edit()
{
	GuiControlGet, value,, IBM_OffLine_Timeout_Edit
	g_IriBrivMaster.UpdateSetting("IBM_OffLine_Timeout",value+0)
}

IBM_Launch_Override() ;To allow us to use IBM game location settings TODO: The game launch routine should probably not be in the GUI file. Also duplication with farm script side
{
	programLoc:=g_IBM_Settings.IBM_Game_Launch
    try
    {
		if (g_IBM_Settings.IBM_Game_Hide_Launcher)
			Run, %programLoc%,,Hide, openPID
		else
			Run, %programLoc%,,,openPID
    }
    catch
    {
        MsgBox, 48, % "Unable to launch game, `nVerify the game location is set properly in the Briv Master settings. If you do not wish to use Briv Master's location settings please disable the addon"
    }
	if (g_SF.GetProcessName(openPID)==g_IBM_Settings.IBM_Game_Exe) ;If we launch the game .exe directly (e.g. Steam) the Run PID will be the game, but for things like EGS it will not so we need to find it
		g_SF.PID:=openPID
    else
	{
		Process, Exist, % g_IBM_Settings.IBM_Game_Exe
		g_SF.PID:=ErrorLevel
	}
	Process, Priority, % g_SF.PID, Realtime ;Raises IC's priority
}

IBM_Game_Copy_From_Game() ;Copy game location settings from the running game. Note that using WinGet ProcessPath will return odd values for some mounted devices
{
	GuiControlGet, currentExe,, vIBM_Game_Exe
	useExe:="IdleDragons.exe" ;Standard .exe name
	hWnd:=WinExist("ahk_exe " . useExe)
	if(!hWnd)
	{
		useExe:=currentExe
		hWnd:=WinExist("ahk_exe " . useExe)
	}
	if(hWnd) ;A game window exists
	{
		location:=IBM_Game_Copy_From_Game_Location_Helper(useExe) . "\" ;Trailing \ is removed
		if (g_SF.Memory.ReadPlatform()==21) ;21 is the EGS platform code
			launch:="explorer.exe ""com.epicgames.launcher://apps/7e508f543b05465abe3a935960eb70ac%3A48353a502e72433298f25827e03dbff0%3A40cb42e38c0b4a14a1bb133eb3291572?action=launch&silent=true"""
		else
		{
			launch:=location . useExe
		}
		GuiControl, ICScriptHub:, IBM_Game_Exe, % useExe
		GuiControl, ICScriptHub:, IBM_Game_Path, % location
		GuiControl, ICScriptHub:, IBM_Game_Launch, % launch
		IBM_Game_Location_Settings()
	}
	else
		Msgbox 16, Briv Master, Idle Champions not found. If you have changed the executable file name please enter it into the Executable field and try again ;16 is Stop/Error, +0 for just OK
}

IBM_Game_Copy_From_Game_Location_Helper(processName) ;TODO: Should this be in the GUI file?
{
	for gameProcess in ComObjGet("winmgmts:").ExecQuery("Select * from Win32_Process where Name='" . processName . "'")  ;Notepad++ UDF langauge file can't copy with the quoted single quote for some reason
	{
		command:=gameProcess.CommandLine ;For sensible platforms, this will be C:\Games\IdleDragons.exe. EGS is not sensible, and so it will be "C:/Games/IdleDragons.exe" -STUFF". Those forward slashes are not typos...
		SplitPath command, outFile, outPath
		if (outFile!=processName)
		{
			exeLocation:=InStr(command,processName)
			cleanString:=SubStr(command,1,exeLocation + StrLen(processName)-1) ;-1 as InStr returns the location of the 1st character
			cleanString:=StrReplace(cleanString,"""") ;Remove quotes
			cleanString:=StrReplace(cleanString,"/","\") ;Fix slashes
			SplitPath cleanString,, outPath
		}
		return outPath
	}
}

IBM_Game_Location_Settings() ;TODO: Should be able to use the generic Int for this, and the g-label will trigger when IBM_Game_Copy_From_Game() makes changes?
{
	if (g_IriBrivMaster_GUI.controlLock)
		return
	GuiControlGet, value,, IBM_Game_Exe
	g_IriBrivMaster.UpdateSetting("IBM_Game_Exe",value)
	GuiControlGet, value,, IBM_Game_Path
	g_IriBrivMaster.UpdateSetting("IBM_Game_Path",value)
	GuiControlGet, value,, IBM_Game_Launch
	g_IriBrivMaster.UpdateSetting("IBM_Game_Launch",value)
	GuiControlGet, value,, IBM_Game_Hide_Launcher
	g_IriBrivMaster.UpdateSetting("IBM_Game_Hide_Launcher",value+0)
}

IBM_Game_Settings_Profile()
{
	GuiControlGet, value,, IBM_Game_Settings_Profile_1
	profile:=value ? 1 : 2
	g_IBM_Settings.HUB.IBM_Game_Settings_Option_Profile:=profile
	g_IriBrivMaster.GameSettingsCheck()
}

IBM_Game_Settings_Fix()
{
	GuiControlGet, value,, IBM_Game_Settings_Profile_1
	profile:=value ? 1 : 2
	g_IriBrivMaster.GameSettingsCheck(true)
}

IBM_Game_Settings_Options()
{
	if WinExist("ahk_id " . g_IriBrivMaster_GUI.IBM_Game_Settings_Opt_Hwnd)
	{
		Gui, IBM_Game_Settings_Options:Hide
	}
	else
	{
		GuiControlGet, GameSettings, Hwnd, IBM_Game_Settings_Group
		WinGetPos, GameOptX, GameOptY,GameOptW,GameOptH, % "ahk_id " . GameSettings
		Gui, IBM_Game_Settings_Options:Show, Hide ;Creates the window so we can read the size
		DetectHiddenWindows, On
		WinGetPos, OptionsX,OptionsY,OptionsW,OptionsH, % "ahk_id " . g_IriBrivMaster_GUI.IBM_Game_Settings_Opt_Hwnd
		DetectHiddenWindows, Off
		targetX:=GameOptX + (GameOptW - OptionsW)//2
		targetY:=GameOptY + GameOptH + 1
		Gui, IBM_Game_Settings_Options:Show, X%targetX% Y%targetY%
	}
}

IBM_Game_Settings_Option_Change() ;This just updates everything, since we shouldn't be screwing around in the game settings options screen constantly TODO: Should this refresh the profile names in the main window? Probably should...
{
	if (g_IriBrivMaster_GUI.controlLock)
		return
	loop 2 ;Two profiles
	{
		profileIndex:=A_Index
		for setting,_ in g_IBM_Settings.HUB.IBM_Game_Settings_Option_Set[profileIndex]
		{
			GuiControlGet, value,, IBM_Game_Settings_Option_%setting%_%profileIndex%
			if value is integer ;Mixed types between the name (string) and values (int/bool)
				value:=value+0
			g_IBM_Settings.HUB.IBM_Game_Settings_Option_Set[profileIndex,setting]:=value
		}
	}
}

IBM_ChestsSnatcher_Status_Resize()
{
	if WinExist("ahk_id " . g_IriBrivMaster_GUI.IBM_ChestSnatcher_Log_Hwnd)
	{
		Gui, IBM_ChestSnatcher_Log:Hide
	}
	else
	{
		g_IriBrivMaster_GUI.IBM_ChestsSnatcher_Status_Update(true) ;Update the list before showing it
		GuiControlGet, StatusList, Hwnd, IBM_ChestsSnatcher_Status
		WinGetPos, StatusListX, StatusListY,,StatusListH, % "ahk_id " . StatusList
		targetX:=StatusListX
		targetY:=StatusListY+StatusListH+1
		Gui, IBM_ChestSnatcher_Log:Show, X%targetX% Y%targetY%
	}
}

IBM_ChestsSnatcher_Options()
{
	if WinExist("ahk_id " . g_IriBrivMaster_GUI.IBM_ChestSnatcher_Opt_Hwnd)
	{
		Gui, IBM_ChestSnatcher_Options:Hide
	}
	else
	{
		g_IriBrivMaster_GUI.UpdateChestSnatcherOptions()
		GuiControlGet, StatusList, Hwnd, IBM_ChestsSnatcher_Status
		WinGetPos, StatusListX, StatusListY,StatusListW,StatusListH, % "ahk_id " . StatusList
		targetX:=StatusListX+StatusListW//2
		targetY:=StatusListY+StatusListH+1
		Gui, IBM_ChestSnatcher_Options:Show, X%targetX% Y%targetY%
	}
}

IBM_ChestSnatcher_Options_OK_Button() ;Applies all the the ChestSnatcher options
{
	GuiControlGet, value,, IBM_ChestSnatcher_Options_Min_Buy
	if (value>g_IriBrivMaster.CONSTANT_serverRateBuy) ;Can't buy more than 250 chests at a time
		value:=g_IriBrivMaster.CONSTANT_serverRateBuy
	g_IBM_Settings.HUB.IBM_ChestSnatcher_Options_Min_Buy:=value
	GuiControlGet, value,, IBM_ChestSnatcher_Options_Open_Gold
	if (value>g_IriBrivMaster.CONSTANT_serverRateOpen) ;Can't open more than 1000 at a time
		value:=g_IriBrivMaster.CONSTANT_serverRateOpen
	g_IBM_Settings.HUB.IBM_ChestSnatcher_Options_Open_Gold:=value
	GuiControlGet, value,, IBM_ChestSnatcher_Options_Open_Silver
	if (value>g_IriBrivMaster.CONSTANT_serverRateOpen) ;Can't open more than 1000 at a time
		value:=g_IriBrivMaster.CONSTANT_serverRateOpen
	g_IBM_Settings.HUB.IBM_ChestSnatcher_Options_Open_Silver:=value
	GuiControlGet, value,, IBM_ChestSnatcher_Options_Min_Gem
	g_IBM_Settings.HUB.IBM_ChestSnatcher_Options_Min_Gem:=value
	GuiControlGet, value,, IBM_ChestSnatcher_Options_Min_Gold
	g_IBM_Settings.HUB.IBM_ChestSnatcher_Options_Min_Gold:=value
	GuiControlGet, value,, IBM_ChestSnatcher_Options_Min_Silver
	g_IBM_Settings.HUB.IBM_ChestSnatcher_Options_Min_Silver:=value
	GuiControlGet, value,, IBM_ChestSnatcher_Options_Claim
	g_IBM_Settings.HUB.IBM_DailyRewardClaim_Enable:=value
	Gui, IBM_ChestSnatcher_Options:Hide
}

IBM_OffLine_Blank() ;Handle the cascade of enabled/disabled options for Blank->Relay
{
	GuiControlGet, blankOn,, IBM_OffLine_Blank
	g_IriBrivMaster.UpdateSetting("IBM_OffLine_Blank",blankOn+0)
	GuiControlGet, relayOn,, IBM_OffLine_Blank_Relay
	g_IriBrivMaster.UpdateSetting("IBM_OffLine_Blank_Relay",relayOn+0)
	GuiControlGet, value,, IBM_OffLine_Blank_Relay_Zones
	g_IriBrivMaster.UpdateSetting("IBM_OffLine_Blank_Relay_Zones",value+0)
	IBM_Offline_Blank_EnableControls(blankOn,relayOn)
}

IBM_Offline_Blank_EnableControls(relay, relayZones)
{
	if (relay)
		GuiControl, ICScriptHub:Enable, IBM_OffLine_Blank_Relay
	else
		GuiControl, ICScriptHub:Disable, IBM_OffLine_Blank_Relay
	if (relay AND relayZones)
		GuiControl, ICScriptHub:Enable, IBM_OffLine_Blank_Relay_Zones
	else
		GuiControl, ICScriptHub:Disable, IBM_OffLine_Blank_Relay_Zones
}

IBM_OffLine_Freq_Edit()
{
	GuiControlGet, value,, IBM_OffLine_Freq_Edit
	if (value<1)
		value:=1
	g_IriBrivMaster.UpdateSetting("IBM_OffLine_Freq",value)
}

IBM_NonGemFarm_Elly_Start()
{
	g_IriBrivMaster.IBM_Elly_StartNonGemFarm()
}

IBM_NonGemFarm_Elly_Stop()
{
	g_IriBrivMaster.IBM_Elly_StopNonGemFarm()
}

IBM_LevelManager_Input_Max()
{
	GuiControlGet, value,, IBM_LevelManager_Input_Max
	if (value<2)
		value:=2 ;Due to potential use of modifier keys this must be at least 2
	g_IriBrivMaster.UpdateSetting("IBM_LevelManager_Input_Max",value)
}

IBM_Level_Options_BrivBoost_Use()
{
	GuiControlGet, value,, IBM_Level_Options_BrivBoost_Use
	IBM_Level_Options_BrivBoost_Enable(value)
	g_IriBrivMaster.UpdateSetting("IBM_LevelManager_Boost_Use",value)
}

IBM_Level_Options_BrivBoost_Enable(enableControl)
{
	if (enableControl)
	{
		GuiControl, ICScriptHub:Enable, IBM_LevelManager_Boost_Multi
	}
	else
	{
		GuiControl, ICScriptHub:Disable, IBM_LevelManager_Boost_Multi
	}
}

IBM_Online_Melf_Use()
{
	GuiControlGet, value,, IBM_Online_Melf_Use
	IBM_Online_Melf_Enable(value)
	g_IriBrivMaster.UpdateSetting("IBM_Online_Use_Melf",value)
}

IBM_Online_Melf_Enable(enableControl)
{
	if (enableControl)
	{
		GuiControl, ICScriptHub:Enable, IBM_Online_Melf_Min_Edit
		GuiControl, ICScriptHub:Enable, IBM_Online_Melf_Max_Edit
	}
	else
	{
		GuiControl, ICScriptHub:Disable, IBM_Online_Melf_Min_Edit
		GuiControl, ICScriptHub:Disable, IBM_Online_Melf_Max_Edit
	}
}

IBM_Route_Import_Button()
{
	InputBox routeString, Route Import,,,,100,,,,, ;InputBox, OutputVar , Title, Prompt, Hide, Width, Height, X, Y, Locale, Timeout, Default
	g_IriBrivMaster.ParseRouteImportString(routeString)
}

IBM_Route_Export_Button()
{
	InputBox _, Route Export,,,,100,,,,, % g_IriBrivMaster.GetRouteExportString() ;InputBox, OutputVar , Title, Prompt, Hide, Width, Height, X, Y, Locale, Timeout, Default
}

IBM_Route_J_Click()
{
	RegExMatch(A_GuiControl,"_(\d+)$",index)
	g_IriBrivMaster.UpdateRouteSetting("IBM_Route_Zones_Jump",index1) ;index1 is the first submatch...AHK is cursed
	g_IriBrivMaster_GUI.RefreshRouteJumpBoxes()
}

IBM_Route_S_Click()
{
	RegExMatch(A_GuiControl,"_(\d+)$",index)
	g_IriBrivMaster.UpdateRouteSetting("IBM_Route_Zones_Stack",index1) ;index1 is the first submatch...AHK is still cursed
	g_IriBrivMaster_GUI.RefreshRouteStackBoxes()
}

IBM_RunControl_Offline_Toggle()
{
	GuiControl, ICScriptHub:Disable, IBM_RunControl_Offline_Pause
	g_IriBrivMaster.SetControl_OfflineStacking(true)
}

IBM_RunControl_Offline_Queue_Toggle()
{
	GuiControl, ICScriptHub:Disable, IBM_RunControl_Offline_Queue
	g_IriBrivMaster.SetControl_QueueOffline(true)
}

IBM_RunControl_RestoreWindow_Toggle()
{
	GuiControl, ICScriptHub:Disable, IBM_RunControl_RestoreWindow_Toggle
	g_IriBrivMaster.SetControl_RestoreWindow()
}

IBM_LevelManager_Refresh() ;UI refresh button
{
	g_IriBrivMaster_GUI.RefreshLevelRows()
}

IBM_Route_Combine()
{
	GuiControlGet, value,, IBM_Route_Combine
	g_IriBrivMaster.UpdateSetting("IBM_Route_Combine",value)
	IBM_Combine_Enable(value)
}

IBM_Combine_Enable(enableControl)
{
	if (enableControl)
		GuiControl, ICScriptHub:Enable, IBM_Route_Combine_Boss_Avoidance
	else
		GuiControl, ICScriptHub:Disable, IBM_Route_Combine_Boss_Avoidance
}

IBM_OffLine_Flames_Use()
	{
	GuiControlGet, value,, IBM_OffLine_Flames_Use
	g_IriBrivMaster.UpdateSetting("IBM_OffLine_Flames_Use",value)
	IBM_OffLine_Flames_Enable_Edit(value)
}

IBM_OffLine_Flames_Enable_Edit(enableControl)
{
	if (enableControl)
	{
		loop, 5
			GuiControl, ICScriptHub:Enable, IBM_OffLine_Flames_Zone_Edit_%A_Index%
	}
	else
	{
		loop, 5
			GuiControl, ICScriptHub:Disable, IBM_OffLine_Flames_Zone_Edit_%A_Index%
	}
}


IBM_MainButtons_Reset()
{
	GuiControl, ICScriptHub: Disable, IBM_MainButtons_Reset
	g_IriBrivMaster.ResetStats()
	g_IriBrivMaster.UpdateStatus() ;NOT UpdateStats(), as that assumes we've already checked the COM object is valid
	GuiControl, ICScriptHub: Enable, IBM_MainButtons_Reset
}

IBM_MainButtons_Save()
{
	Gui, ICScriptHub:Submit, NoHide
	GuiControl, ICScriptHub: Disable, IBM_MainButtons_Save
	flamesZones:=[]
	loop, 5
		{
			GuiControlGet, curZone,, IBM_OffLine_Flames_Zone_Edit_%A_Index%
			flamesZones[A_Index]:=curZone+0
		}
	g_IriBrivMaster.UpdateSetting("IBM_OffLine_Flames_Zones",flamesZones)
	g_IriBrivMaster.UpdateSetting("IBM_Ellywick_NonGemFarm_Cards",g_IriBrivMaster_GUI.ReadNonGemFarmEllySettings())
	;Level Manager
	if (g_IriBrivMaster_GUI.levelDataSet.Length() > 0) ;Only save if we have some formations loaded (prevents overwritting dates with nothing because we didn't read these in whilst saving other things), and check if we've actually made changes
	{
		savedData:=g_IriBrivMaster.GetLevelSettings()
		haveAdded:=false
		addedString:=""
		heroList:={} ;Used to check for removals later to avoid excessive loops around the seat->hero structure
		for _, seatMembers in g_IriBrivMaster_GUI.levelDataSet
		{
			for heroID, heroData in seatMembers
			{
				heroList[heroID]:=heroData.Name
				if(!savedData.hasKey(heroID))
				{
					haveAdded:=true
					addedString.=heroData.Name . " (" . heroID . ")`n"
				}
			}
		}
		haveRemoved:=false
		removedString:=""
		for heroID,heroData in savedData
		{
			if(!heroList.hasKey(heroID))
			{
				haveRemoved:=true
				heroName:=g_Heroes[heroID].ReadName() ;The name of the hero is not in current data, and might not be available at all
				removedString.=(heroName ? heroName : "Unable to retrieve champion name") . " (" . heroID . ")`n"
			}
		}
		if(haveAdded OR haveRemoved)
		{
			saveMsg:="The following champion changes have been made in the Level Manager:`n"
			if(haveAdded)
				saveMsg.="`nAdded:`n" . addedString . "`n"
			if(haveRemoved)
				saveMsg.="`nRemoved:`n" . removedString . "`n"
			saveMsg.="`nSave Level Manager changes?"
			Msgbox, 36, Briv Master Level Manager, %saveMsg% ;4 is Yes/No, + 32 for Question icon
			ifMsgBox Yes
				g_IriBrivMaster.UpdateLevelSettings(g_IriBrivMaster_GUI.GetLevelRowData())
		}
		else
			g_IriBrivMaster.UpdateLevelSettings(g_IriBrivMaster_GUI.GetLevelRowData())
	}
	;Done with levels
	g_IriBrivMaster.SaveSettings()
	GuiControl, ICScriptHub: Enable, IBM_MainButtons_Save
}