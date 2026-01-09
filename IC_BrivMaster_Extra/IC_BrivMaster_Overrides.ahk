class IC_BrivMaster_ServerCall_Class extends IC_ServerCalls_Class
{
	; Copied from an older (mid-2025) verison of BrivGemFarmPerformance, as we're not using the separate servercalls script for everything
	; forces an attempt for the server to remember stacks
    CallPreventStackFail(stacks, launchScript:=false) ;TODO: This could possibly do the Thunder Step multiplier directly? If so needs removing from both the RestartAdventure() and WaitForModronReset() calls
    {
        response:= ""
        stacks:= g_SaveHelper.GetEstimatedStackValue(stacks)
        userData:= g_SaveHelper.GetCompressedDataFromBrivStacks(stacks)
        checksum:= g_SaveHelper.GetSaveCheckSumFromBrivStacks(stacks)
        save:=g_SaveHelper.GetSave(userData, checksum, this.userID, this.userHash, this.networkID, this.clientVersion, this.instanceID)
        if (launchScript) ; do server call from new script to prevent hanging script due to network issues.
        {
            webRoot := this.webRoot
            scriptLocation := A_LineFile . "\..\IC_BrivMaster_SaveStacks.ahk"
            Run, %A_AhkPath% "%scriptLocation%" "%webRoot%" "%save%"
        }
        else
        {
            try
            {
                response:=this.ServerCallSave(save)
            }
            catch, ErrMsg
            {
                g_SharedData.LoopString := "Failed to save Briv stacks" ;TODO: Log this instead
            }
        }
        return response
    }
}

class IBM_Memory_Manager extends _MemoryManager
{

	;Override to add option to take a PID to use instead of finding any process with the .exe name
    Refresh(moduleName:="mono-2.0-bdwgc.dll", pid:="")
    {
        this.isInstantiated := false
        ;Open a process with sufficient access to read and write memory addresses (this is required before you can use the other functions)
        ;You only need to do this once. But if the process closes/restarts, then you will need to perform this step again. Refer to the notes section below.
        ;Also, if the target process is running as admin, then the script will also require admin rights!
        ;Note: The program identifier can be any AHK windowTitle i.e.ahk_exe, ahk_class, ahk_pid, or simply the window title.
        ;handle is an optional variable in which the opened handle is stored.
        if (pid)
			processLookup:="AHK_PID " . pid
		else
			processLookup:="AHK_EXE " . this._exeName
		this.instance:=New _ClassMemory(processLookup, "", handle)
        this.handle:=handle
        if IsObject(this.instance)
        {
            this.isInstantiated := true
        }
        else
        {
            this.baseAddress[moduleName1] := -1
            return False
        }
        this.baseAddress[moduleName]:=this.instance.getModuleBaseAddress(moduleName)
        return true
    }
}

class IC_BrivMaster_GameObjectStructure_Add
{
	IBM_ReBase(baseItem:="") ;Propogate a new base address through all child objects. Call without argument for base item
	{
		if (IsObject(baseItem)) ;Child object
		{
			this.BasePtr := baseItem.BasePtr
			this.FullOffsets := baseItem.FullOffsets.Clone()
			this.FullOffsets.Push(this.Offset*)
		}
		else ;The base item we called from
		{
			this.BasePtr:= new SH_BasePtr(_MemoryManager.instance.getAddressFromOffsets(this.BasePtr.BaseAddress,this.FullOffsets*))
			this.Is64Bit := _MemoryManager.is64Bit
			this.FullOffsets := Array()          ; Full list of offsets required to get from base pointer to this object
			this.FullOffsetsHexString := ""      ; Same as above but in readable hex string format. (Enable commented lines assigning this value to use for debugging)
			this.BaseAddressPtr := ""            ; The name of the pointer class that created this object.
			this.Offset := 0x0                   ; The offset from last object to this object.
			;TODO: Is forcing IsAddedIndex below appropriate? I think it is so that we can ReBase collection members without the next read just overwriting it
			this.IsAddedIndex := false           ; __Get lookups on non-existent keys will create key objects with this value being true. Prevents cloning non-existent values.
		}
		for k,v in this ;Recurse children
        {
			if(IsObject(v) AND ObjGetBase(v).__Class == "GameObjectStructure" AND v.FullOffsets != "" AND k != "BasePtr")
            {
                if(v.IsAddedIndex) ;Remove created objects
					this.Delete(k)
				else
					v.IBM_ReBase(this)
            }
        }

	}
}