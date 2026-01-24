#SingleInstance force
#NoTrayIcon
#Persistent
#NoEnv ; Avoids checking empty variables to see if they are environment variables.
ListLines Off

#include %A_LineFile%\..\..\..\SharedFunctions\json.ahk ;TODO: Move to AHK JSON, which means having that in a separate file somewhere
global g_webRoot:=A_Args[1] ? A_Args[1] : "http://ps22.idlechampions.com/~idledragons/" ;TODO: Just pass this to the function instead of making it a global?
ServerCallSave(A_Args[2],A_Args[3])
ExitApp

ServerCallSave(saveBody,boundaryHeader,retryNum:=0) ; Special server call specifically for use with saves. saveBody must be encoded before using this call.
{
	response:=""
	WR:=ComObjCreate( "WinHttp.WinHttpRequest.5.1" )
	; https://learn.microsoft.com/en-us/windows/win32/winhttp/iwinhttprequest-settimeouts defaults: 0 (DNS Resolve), 60000 (connection timeout. 60s), 30000 (send timeout), 60000 (receive timeout)
	WR.SetTimeouts( "0", "15000", "7500", "30000" )
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
			response:=JSON.parse(data)
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