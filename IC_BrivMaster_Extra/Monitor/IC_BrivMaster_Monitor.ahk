#Requires AutoHotkey 1.1.37+ <1.2
#SingleInstance Force
#NoEnv
SetBatchLines, -1
ListLines Off

#include %A_LineFile%\..\..\Lib\IC_BrivMaster_JSON.ahk

global g_Settings:={}
global g_settingFile:=A_LineFile . "\..\IC_BrivMaster_Monitor_Settings.json"
if(FileExist(g_settingFile))
{
    FileRead, rawSettings, %g_settingFile%
    g_Settings:=AHK_JSON.Load(rawSettings)
}
else
{
    g_Settings:={}
    g_Settings.Rows:=6
    g_Settings.Freq:=2000
    g_Settings.Amber:=40
    g_Settings.Red:=60
    g_Settings.Dark:=false
    newSettings:=AHK_JSON.Dump(g_Settings)
    FileAppend, %newSettings%, %g_settingFile%
}

ICON_STANDARD:=A_LineFile . "\..\..\Resources\IBM_L.ico"
try
{
    Menu Tray, Icon, %ICON_STANDARD%
}
logFile:=A_LineFile . "\..\..\Logs\MiniLog.json"
if(!FileExist(logFile))
{
    MsgBox, 16, Briv Master Monitor, Unable to find MiniLog to monitor
    ExitApp
}

Gui, IBM_Monitor:New, ,Briv Master Monitor
Gui, IBM_Monitor:+HwndwindowHandle
global g_hWnd:=windowHandle
if(g_Settings.Dark)
{
    Gui, Font, s9 cWhite
    Gui, IBM_Monitor:Color, 303030
    if (A_OSVersion >= "10.0.17763" && SubStr(A_OSVersion, 1, 3) = "10.") ;Window title logic taken from ScriptHub
    {
        attr := 19
        if (A_OSVersion >= "10.0.18985")
        {
            attr := 20
        }
        DllCall("dwmapi\DwmSetWindowAttribute", "ptr", g_hWnd, "int", attr, "int*", true, "int", 4)
    }
}
else
{
    Gui, Font, s9
}
Gui, IBM_Monitor:Add, Text, xm+0 h20 0x200, Time since last update (s):
Gui, IBM_Monitor:Add, Text, x+5 w30 h20 0x200 vIBM_Monitor_Last_Update,-
Gui, IBM_Monitor:Add, Button, xm+259 yp+0 vIBM_Monitor_Settings gIBM_Monitor_Settings, ⚙ ;Symbol requires this file to saved as UTF-8 with BOM
rowCount:=g_Settings.Rows
Gui, IBM_Monitor:Add, ListView,xm+0 y+3 w282 0x2000 LV0x10000 vIBM_Monitor_LV Count%rowCount% R%rowCount% LV0x10 NoSort NoSortHdr, BPH|Total|Active|Wait|Cycle|Fail ;0x2000 is remove H scroll bar, LV0x10000 is double-buffering to stop flickering, LV0x10 prevents re-ordering of columns
GuiControl, -Redraw, IBM_Monitor_LV
Gui, ListView, IBM_Monitor_LV
LV_ModifyCol(1,60)
LV_ModifyCol(2,50)
LV_ModifyCol(3,50)
LV_ModifyCol(4,50)
LV_ModifyCol(5,40)
LV_ModifyCol(6,30)
if(g_Settings.Dark)
{
    GuiControl, IBM_Monitor: +Background303030, IBM_Monitor_LV
    GuiControl, IBM_Monitor: +cWhite, IBM_Monitor_LV
}
GuiControl, +Redraw, IBM_Monitor_LV
GuiControlGet, statsLVEndPos, ICScriptHub:Pos, IBM_Monitor_LV

GUI, IBM_Monitor_Settings:New, ,Settings
Gui, IBM_Monitor_Settings:-Resize -MaximizeBox +HwndwindowHandle
global g_hWnd_Settings:=windowHandle
if(g_Settings.Dark)
{
    Gui, Font, s9 cWhite
    Gui, IBM_Monitor_Settings:Color, 303030
    if (A_OSVersion >= "10.0.17763" && SubStr(A_OSVersion, 1, 3) = "10.") ;Window title logic taken from ScriptHub
    {
        attr:=19
        if (A_OSVersion >= "10.0.18985")
        {
            attr:=20
        }
        DllCall("dwmapi\DwmSetWindowAttribute", "ptr", g_hWnd_Settings, "int", attr, "int*", true, "int", 4)
    }
}
else
{
    Gui, Font, s9
}
Gui, IBM_Monitor_Settings:Add, Edit, cBlack xm+0 h20 w45 Number Limit2 0x200 vIBM_Monitor_Settings_Rows
Gui, IBM_Monitor_Settings:Add, Text, x+5 h20 w180 0x200, Runs to display (requires restart)
Gui, IBM_Monitor_Settings:Add, Edit, cBlack xm+0 h20 w45 Number Limit6 0x200 vIBM_Monitor_Settings_Freq
Gui, IBM_Monitor_Settings:Add, Text, x+5 h20 0x200, Update frequency (ms)
Gui, IBM_Monitor_Settings:Add, Edit, cBlack xm+0 h20 w45 Number Limit3 0x200 vIBM_Monitor_Settings_Amber
Gui, IBM_Monitor_Settings:Add, Text, x+5 h20 0x200, Amber alert level threshold (s)
Gui, IBM_Monitor_Settings:Add, Edit, cBlack xm+0 h20 w45 Number Limit3 0x200 vIBM_Monitor_Settings_Red
Gui, IBM_Monitor_Settings:Add, Text, x+5 h20 0x200, Red alert level threshold (s)
Gui, IBM_Monitor_Settings:Add, Checkbox, xm+0 h20 0x200 vIBM_Monitor_Settings_Dark, Dark mode (requires restart)
Gui, IBM_Monitor_Settings:Add, Button, xm+90 w50 vIBM_Monitor_Settings_OK gIBM_Monitor_Settings_OK, OK
Gui, IBM_Monitor:Show

A64 := (A_PtrSize = 8 ? 4 : 0) ;Alignment for pointers in 64-bit environment
cbSize := 4 + A64 + A_PtrSize + 4 + 4 + 4 + A64
VarSetCapacity(FLASHWINFO_ON, cbSize, 0) ;FLASHWINFO structure for turning taskbar flashes on. As we fill with zeros there is no need to add the later parts of the structure that will all be 0 in our case
Addr:=&FLASHWINFO_ON
Addr:=NumPut(cbSize,    Addr + 0, 0,   "UInt")
Addr:=NumPut(g_hWnd,    Addr + 0, A64, "Ptr")
Addr:=NumPut(0x0000000E,Addr + 0, 0,   "UInt") ;0x0000000E is 0x0000000C | 0x00000002
;Addr:=NumPut(0,         Addr + 0, 0,   "UInt")
;Addr:=NumPut(0,         Addr + 0, 0,   "Uint")

VarSetCapacity(FLASHWINFO_OFF, cbSize, 0) ;FLASHWINFO structure for turning taskbar flashes off
Addr:=&FLASHWINFO_OFF
Addr:=NumPut(cbSize,    Addr + 0, 0,   "UInt")
Addr:=NumPut(g_hWnd,    Addr + 0, A64, "Ptr")
;Addr:=NumPut(0,Addr + 0, 0,   "UInt")
;Addr:=NumPut(0,         Addr + 0, 0,   "UInt")
;Addr:=NumPut(0,         Addr + 0, 0,   "Uint")

lastLogModify:=0
Loop
{
    FileGetTime, currentLogModify, %logFile%, M
    if(currentLogModify!=lastLogModify)
    {
        lastLogModify:=currentLogModify
        FileRead, logContent, %logfile%
        if(StrLen(logContent)>0) ;Ignore empty - I believe this can happen if the file is read between being created and being populated
        {
            
            runData:=AHK_JSON.Load(logContent)
            duration:=runData.End-runData.Start
            totalDuration:=ROUND(duration / 1000,2) ;Convert to seconds
            if(runData.ActiveStart) ;With no activestart the run is incomplete, possibly the first run
            {
                loadTime:=runData.ActiveStart - runData.Start
                resetTime:=runData.End - runData.ResetReached
                waitTime:=ROUND((loadTime+resetTime) / 1000,2) ;Convert to seconds TODO: Why not just total - active for wait time?
                activeTime:=ROUND((runData.ResetReached-runData.ActiveStart) / 1000,2)
                bosses:=Floor(runData.LastZone / 5)
                runsPerHour:=3600000/duration
                bph:=ROUND(bosses*runsPerHour,2)
            }
            else
            {
                waitTime:="-"
                activeTime:="-"
                bph:="Partial"
            }
            failString:=runData.Fail ? "Fail" : "-"
            Gui, IBM_Monitor:Default ;Needed due to the options GUI
            Gui, ListView, IBM_Monitor_LV
            GuiControl, -Redraw, IBM_Monitor_LV
            if(LV_GetCount()>=rowCount)
            {
                LV_Delete(LV_GetCount())
            }
            LV_Insert(1,"",bph,totalDuration,activeTime,waitTime,runData.Cycle,failString)
            GuiControl, +Redraw, IBM_Monitor_LV
        }
    }
    timeSinceLastModify:=""
    timeSinceLastModify-=currentLogModify, s
    AlertColour(timeSinceLastModify,&FLASHWINFO_ON,&FLASHWINFO_OFF)
    GuiControl,IBM_Monitor:, IBM_Monitor_Last_Update,%timeSinceLastModify%
    Sleep g_Settings.Freq
}

IBM_Monitor_Settings()
{
    GuiControl, IBM_Monitor_Settings:, IBM_Monitor_Settings_Rows, % g_Settings.Rows
    GuiControl, IBM_Monitor_Settings:, IBM_Monitor_Settings_Freq, % g_Settings.Freq
    GuiControl, IBM_Monitor_Settings:, IBM_Monitor_Settings_Amber, % g_Settings.Amber
    GuiControl, IBM_Monitor_Settings:, IBM_Monitor_Settings_Red, % g_Settings.Red
    GuiControl, IBM_Monitor_Settings:, IBM_Monitor_Settings_Dark, % g_Settings.Dark
    Gui, IBM_Monitor_Settings:Show
}

IBM_Monitor_Settings_OK()
{
    Gui, IBM_Monitor_Settings:Submit
    GuiControlGet, tempRows, , IBM_Monitor_Settings_Rows
    if(tempRows<1)
        tempRows:=1
    GuiControlGet, tempFreq, , IBM_Monitor_Settings_Freq
    if(tempFreq<100)
        tempFreq:=100
    GuiControlGet, tempAmber, , IBM_Monitor_Settings_Amber
    if(tempAmber<1)
        tempAmber:=1
    GuiControlGet, tempRed, , IBM_Monitor_Settings_Red
    if(tempRed<=tempAmber) ;The red threshold must be above the amber one
        tempRed:=tempAmber+1
     GuiControlGet, tempDark, , IBM_Monitor_Settings_Dark
    reloadNeeded:=tempRows!=g_Settings.Rows OR tempDark!=g_Settings.Dark
    g_Settings.Rows:=tempRows
    g_Settings.Freq:=tempFreq
    g_Settings.Amber:=tempAmber
    g_Settings.Red:=tempRed
    g_Settings.Dark:=tempDark
    settingsJSON:=AHK_JSON.Dump(g_Settings)
    FileDelete, %g_settingFile%
    FileAppend, %settingsJSON%, %g_settingFile%
    if(reloadNeeded)
        Reload
}

AlertColour(timeSinceLastModify,FLASHWINFO_ON,FLASHWINFO_OFF)
{
    static lastState:=0 ;0=Good 1=Alert 2=Action
    if(timeSinceLastModify>g_Settings.Red)
        state:=2
    else if(timeSinceLastModify>g_Settings.Amber)
        state:=1
    else
        state:=0
    if(state!=lastState)
    {
        if(state==2)
        {
            GuiControl, IBM_Monitor:+cRed, IBM_Monitor_Last_Update
            DllCall("User32.dll\FlashWindowEx", "Ptr", FLASHWINFO_ON, "UInt")
        }
        else if(state==1)
        {
            GuiControl, IBM_Monitor:+cFFA000, IBM_Monitor_Last_Update
            DllCall("User32.dll\FlashWindowEx", "Ptr", FLASHWINFO_OFF, "UInt")
        }
        else
        {
            if(g_Settings.Dark)
                GuiControl, IBM_Monitor:+cWhite, IBM_Monitor_Last_Update
            else
                GuiControl, IBM_Monitor:+cBlack, IBM_Monitor_Last_Update
            DllCall("User32.dll\FlashWindowEx", "Ptr", FLASHWINFO_OFF, "UInt")
        }
        lastState:=state
    }
}