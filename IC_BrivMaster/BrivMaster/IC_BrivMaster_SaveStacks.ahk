#SingleInstance force
#NoTrayIcon
#Persistent
#NoEnv
ListLines Off

#include %A_LineFile%\..\..\Lib\IC_BrivMaster_JSON.ahk
global g_webRoot:=A_Args[1] ? A_Args[1] : "http://ps22.idlechampions.com/~idledragons/" ;TODO: Just pass this to the function instead of making it a global?
ServerCallSave(A_Args[2],A_Args[3])
ExitApp

ServerCallSave(saveBody,boundaryHeader,retryNum:=0)
{
	response:=""
	WR:=ComObjCreate("WinHttp.WinHttpRequest.5.1")
	WR.SetTimeouts("0","15000","7500","30000")
	Try 
	{
		WR.Open("POST", g_webRoot . "post.php?call=saveuserdetails&", true)
		WR.SetRequestHeader("Accept-Encoding", "identity")
		WR.SetRequestHeader("Content-Type", "multipart/form-data; boundary=""" . boundaryHeader . """")
		WR.SetRequestHeader("User-Agent", "BestHTTP")
		WR.Send(saveBody)
		WR.WaitForResponse(-1)
		data:=WR.ResponseText
		Try
		{
			response:=AHK_JSON.Load(data)
			if(!(response.switch_play_server==""))
			{
				retryNum++
				g_webRoot:=response.switch_play_server
				if(retryNum<=3) 
					ServerCallSave(saveBody,boundaryHeader,retryNum) 
			}
		}
	}
}