#Requires AutoHotkey 1.1.37+ <1.2
#SingleInstance Force
#NoEnv
SetBatchLines, -1
ListLines Off

#include %A_LineFile%\..\Lib\IC_BrivMaster_JSON.ahk

rowCount:=6
ICON_STANDARD:=A_LineFile . "\..\Resources\IBM_L.ico"
try
{
    Menu Tray, Icon, %ICON_STANDARD%
}
logFile:=A_LineFile . "\..\Logs\MiniLog.json"
if(!FileExist(logFile))
{
    MsgBox, 16, Briv Master Monitor, Unable to find MiniLog to monitor
    ExitApp
}

Gui, IBM_Monitor:New, ,Briv Master Monitor
Gui, IBM_Monitor:+HwndwindowHandle
global g_hWnd:=windowHandle
Gui, Font, s9
Gui, IBM_Monitor:Add, Text, xm+0, Time since last update (s):
Gui, IBM_Monitor:Add, Text, x+5 w30 vIBM_Monitor_Last_Update,-

Gui, IBM_Monitor:Add, ListView , +cBlack xm+0 y+5 w280 0x2000 LV0x10000 vIBM_Monitor_LV Count%rowCount% R%rowCount% LV0x10 NoSort NoSortHdr, BPH|Total|Active|Wait|Cycle|Fail ;0x2000 is remove H scroll bar, LV0x10000 is double-buffering to stop flickering, LV0x10 prevents re-ordering of columns
GuiControl, -Redraw, IBM_Monitor_LV
Gui, ListView, IBM_Monitor_LV
LV_ModifyCol(1,60)
LV_ModifyCol(2,50)
LV_ModifyCol(3,50)
LV_ModifyCol(4,50)
LV_ModifyCol(5,40)
LV_ModifyCol(6,30)
GuiControl, +Redraw, IBM_Monitor_LV
GuiControlGet, statsLVEndPos, ICScriptHub:Pos, IBM_Monitor_LV
Gui, IBM_Monitor:Show

sleepTime:=1000 ;ms
lastLogModify:=0

Loop
{
    FileGetTime, currentLogModify, %logFile%, M
    if(currentLogModify!=lastLogModify)
    {
        lastLogModify:=currentLogModify
        FileRead, logContent, %logfile%
        runData:=AHK_JSON.Load(logContent)
        duration:=runData.End-runData.Start
        totalDuration:=ROUND(duration / 1000,2) ;Convert to seconds
        loadTime:=runData.ActiveStart - runData.Start
		resetTime:=runData.End - runData.ResetReached
        waitTime:=ROUND((loadTime+resetTime) / 1000,2) ;Convert to seconds TODO: Why not just total - active for wait time?
        activeTime:=ROUND((runData.ResetReached-runData.ActiveStart) / 1000,2)
		bosses:=Floor(runData.LastZone / 5)
        runsPerHour:=3600000/duration
        bph:=ROUND(bosses*runsPerHour,2)
        failString:=runData.Fail ? "Fail" : "-"
        Gui, ListView, IBM_Monitor_LV
		GuiControl, -Redraw, IBM_Monitor_LV
		if(LV_GetCount()>=rowCount)
		{
			LV_Delete(LV_GetCount())
		}
		LV_Insert(1,"",bph,totalDuration,activeTime,waitTime,runData.Cycle,failString)
		GuiControl, +Redraw, IBM_Monitor_LV
    }
    timeSinceLastModify:=""
    timeSinceLastModify-=currentLogModify, s
    AlertColour(timeSinceLastModify)
    GuiControl,IBM_Monitor:, IBM_Monitor_Last_Update,%timeSinceLastModify%
    Sleep sleepTime
}

AlertColour(timeSinceLastModify)
{
    static lastState:=0 ;0=Good 1=Alert 2=Action
	static THRESHOLD_RED:=50
    static THRESHOLD_AMBER:=35
    if(timeSinceLastModify>THRESHOLD_RED)
        state:=2
    else if(timeSinceLastModify>THRESHOLD_AMBER)
        state:=1
    else
        state:=0
    if(state!=lastState)
    {
        if(state==2)
        {
            GuiControl, IBM_Monitor:+cRed, IBM_Monitor_Last_Update
            FlashWindowEx(g_hWnd, 0x0000000E) ;0x0000000E is 0x0000000C | 0x00000002
        }
        else if(state==1)
        {
            GuiControl, IBM_Monitor:+cFFA000, IBM_Monitor_Last_Update
            FlashWindowEx(g_hWnd, 0)
        }
        else
        {
            GuiControl, IBM_Monitor:+cBlack, IBM_Monitor_Last_Update
            FlashWindowEx(g_hWnd, 0)
        }
        lastState:=state
    }
}

FlashWindowEx(hWnd:=0, dwFlags:=0, uCount:=0, dwTimeout:=0) ;Wrapper for the DLL call due to the FLASHWINFO structure being non-trivial, taken from https://www.autohotkey.com/board/topic/92043-problems-with-flashwindowex/
{
   Static A64 := (A_PtrSize = 8 ? 4 : 0) ;Alignment for pointers in 64-bit environment
   Static cbSize := 4 + A64 + A_PtrSize + 4 + 4 + 4 + A64
   VarSetCapacity(FLASHWINFO, cbSize, 0) ;FLASHWINFO structure
   Addr:=&FLASHWINFO
   Addr:=NumPut(cbSize,    Addr + 0, 0,   "UInt")
   Addr:=NumPut(hWnd,      Addr + 0, A64, "Ptr")
   Addr:=NumPut(dwFlags,   Addr + 0, 0,   "UInt")
   Addr:=NumPut(uCount,    Addr + 0, 0,   "UInt")
   Addr:=NumPut(dwTimeout, Addr + 0, 0,   "Uint")
   DllCall("User32.dll\FlashWindowEx", "Ptr", &FLASHWINFO, "UInt")
}