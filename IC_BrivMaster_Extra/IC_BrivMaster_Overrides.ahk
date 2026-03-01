#include %A_LineFile%\..\..\..\ServerCalls\IC_ServerCalls_Class.ahk ;For IC_ServerCalls_Class override

class IC_BrivMaster_ServerCall_Class extends IC_ServerCalls_Class ;TODO: IC_ServerCalls_Class extends SH_ServerCalls which includes the Javascript-based json.ahk
{
    CallPreventStackFail(sprint, steelbones, message,launchScript:=false) ;This function should be called after checking sprint & steelbones are valid - i.e. move 0 if no value is present. TODO: Can maybe bring this in too?
    {
        stacks:=sprint + FLOOR(steelbones * g_IBM.RouteMaster.stackConversionRate)
		g_IBM.Logger.AddMessage("Servercall Save via: "  . message . " Converted Haste=[" . stacks . "] from Haste=[" . sprint . "] and Steelbones=[" . steelbones . "] with stackConversionRate=[" . Round(g_IBM.RouteMaster.stackConversionRate,1) . "]")
		jsonString:="{""stats"":{""briv_steelbones_stacks"":0,""briv_sprint_stacks"":" . stacks . "}}"
		boundaryHeader:=this.GetBoundryHeader()
		save:=this.GetSaveFromJSON(jsonString,boundaryHeader)
		if(launchScript) ;Do server call from new script to prevent hanging script due to network issues.
        {
            webRoot:=this.webRoot
            scriptLocation:=A_LineFile . "\..\IC_BrivMaster_SaveStacks.ahk"
            Run, %A_AhkPath% "%scriptLocation%" "%webRoot%" "%save%" "%boundaryHeader%"
        }
        else
        {
            try
                response:=this.ServerCallSave(save,boundaryHeader)
            catch
                g_IBM.Logger.AddMessage("Failed to save Briv stacks")			
        }
		return response ;Note this will only be meaningful for the synchronous version of the call
    }
	
    ServerCallSave(saveBody,boundaryHeader,retryNum:=0) ; Special server call specifically for use with saves. saveBody must be encoded before using this call.
    {
        response:=""
        WR:=ComObjCreate( "WinHttp.WinHttpRequest.5.1" )
        ; https://learn.microsoft.com/en-us/windows/win32/winhttp/iwinhttprequest-settimeouts defaults: 0 (DNS Resolve), 60000 (connection timeout. 60s), 30000 (send timeout), 60000 (receive timeout)
        WR.SetTimeouts( "0", "15000", "7500", "30000" )
        Try 
		{
            WR.Open("POST",this.webroot . "post.php?call=saveuserdetails&", true)
            WR.SetRequestHeader("Accept-Encoding", "identity")
			WR.SetRequestHeader("Content-Type", "multipart/form-data; boundary=""" . boundaryHeader . """")
            WR.SetRequestHeader("User-Agent", "BestHTTP")
            WR.Send(saveBody)
            WR.WaitForResponse(-1)
            data:=WR.ResponseText
            if(data) ;Don't try to JSON.Load the string if empty TODO: Review this codepath, does it retry in the no response case? I don't think so...
			{
				try
				{
					response:=AHK_JSON.Load(data)
					if(!(response.switch_play_server=="")) ;NOT(response.switch_play_server=="") => switch_play_server exists. TODO: Should this retry when the server was valid? Probably not given the nature of this call - if the server is down we probably want to return to the run
					{
						retryNum++
						this.WebRoot:=response.switch_play_server
						if(retryNum<=3)
						{						
							WR:=""
							return this.ServerCallSave(saveBody,boundaryHeader,retryNum) 
						}
					}
				}
			}
        }
		WR:=""
        return response
    }

	__New(userID:=0, userHash:=0, instanceID:=0 )
    {
        this.userID := userID
        this.userHash := userHash
        this.instanceID := instanceID
        this.shinies := 0
        this.md5Module:=DllCall("LoadLibrary", "Str", "advapi32.dll", "Ptr")
        return this ;TODO: Does this achieve anything?
    }

    __Delete() ;Free library after use
    {
        DllCall("FreeLibrary", "Ptr", this.md5Module)
    }

    MD5Save(stringVal) ;Creates a salted md5 checksum for a save string. Modified from https://www.autohotkey.com/boards/viewtopic.php?f=6&t=21
    {
        stringVal:=stringVal . "somethingpoliticallycorrect"
        VarSetCapacity(MD5_CTX, 104, 0)
		DllCall("advapi32\MD5Init", "Ptr", &MD5_CTX)
        DllCall("advapi32\MD5Update", "Ptr", &MD5_CTX, "AStr", stringVal, "UInt", StrLen(stringVal))
        DllCall("advapi32\MD5Final", "Ptr", &MD5_CTX)
        loop, 16
            o.=Format("{:02" (case ? "X" : "x") "}", NumGet(MD5_CTX, 87 + A_Index, "UChar"))
        StringLower, o,o
        return o
    }

    GetSaveFromJSON(jsonString,boundaryHeader,timeStamp:="0") ;Converts user's data into form data that can be submitted for a save
    {
		userData:=g_zlib.Deflate(jsonString)
		checksum:=this.MD5Save(jsonString)
		Random, r1, 0, 65535
		Random, r2, 0, 65535
		boundrySuffix:=Format("{:04X}", r2) . Format("{:04X}", r1) ;Random is limited to signed int32, so instead of faffing about with that just glue two 16-bit values together
        mimicSave:="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""call""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 15`r`n`r`n"
        mimicSave.="saveuserdetails`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""language_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 1`r`n`r`n"
        mimicSave.="1`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""user_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  StrLen(this.userID)  "`r`n`r`n"
        mimicSave.=this.userID . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""hash""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 32`r`n`r`n"
        mimicSave.=this.userHash . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""details_compressed""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  (StrLen(userData))  "`r`n`r`n"
        mimicSave.=userData . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""checksum""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 32`r`n`r`n"
        mimicSave.=checksum . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""timestamp""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  StrLen(timeStamp)  "`r`n`r`n"
        mimicSave.=timeStamp . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""request_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: 1`r`n`r`n"
        mimicSave.="1`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""network_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: " StrLen(this.networkID)  "`r`n`r`n"
        mimicSave.=this.networkID . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""mobile_client_version""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  StrLen(this.clientVersion)  "`r`n`r`n"
        mimicSave.=this.clientVersion . "`r`n"
        mimicSave.="--" . boundaryHeader . "`r`n"
        mimicSave.="Content-Disposition: form-data; name=""instance_id""`r`n"
        mimicSave.="Content-Type: text/plain; charset=utf-8`r`n"
        mimicSave.="Content-Length: "  StrLen(this.instanceID)  "`r`n`r`n"
        mimicSave.=this.instanceID . "`r`n"
        mimicSave.="--" . boundaryHeader . "--`r`n"
        return mimicSave
    }
	
	GetBoundryHeader()
	{
		Random, r1, 0, 65535
		Random, r2, 0, 65535
		return "BestHTTP_HTTPMultiPartForm_" . Format("{:04X}", r2) . Format("{:04X}", r1) ;Random is limited to signed int32, so instead of faffing about with that just glue two 16-bit values together
	}
}