#SingleInstance force
#NoTrayIcon
#Persistent
#NoEnv ; Avoids checking empty variables to see if they are environment variables.
ListLines Off

#include %A_LineFile%\..\..\..\ServerCalls\IC_ServerCalls_Class.ahk
serverCall:=new IC_ServerCalls_Class
serverCall.webRoot := A_Args[1] ? A_Args[1] : "http://ps22.idlechampions.com/~idledragons/"
serverCall.ServerCallSave(A_Args[2])
ExitApp