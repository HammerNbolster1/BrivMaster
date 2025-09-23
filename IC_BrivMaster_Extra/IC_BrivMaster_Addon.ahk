; This file is what is executed as a mod to BrivGemFarm when it starts.

; The #include here is required for injection
#include %A_LineFile%\..\IC_BrivMaster_Overrides_GF.ahk
#include %A_LineFile%\..\IC_BrivMaster_Overrides_SF.ahk
#include %A_LineFile%\..\IC_BrivMaster_Overrides.ahk
#include %A_LineFile%\..\IC_BrivMaster_Functions.ahk
#include %A_LineFile%\..\IC_BrivMaster_LevelManager.ahk
#include %A_LineFile%\..\IC_BrivMaster_RouteMaster.ahk
#include %A_LineFile%\..\..\..\SharedFunctions\SH_UpdateClass.ahk
SH_UpdateClass.UpdateClassFunctions(g_BrivGemFarm, IC_BrivMaster_BrivGemFarm_Class)
SH_UpdateClass.UpdateClassFunctions(g_SF, IC_BrivMaster_SharedFunctions_Class)
SH_UpdateClass.UpdateClassFunctions(g_SharedData, IC_BrivMaster_SharedData_Class)
SH_UpdateClass.UpdateClassFunctions(g_SF.Memory, IC_BrivMaster_MemoryFunctions_Class)
SH_UpdateClass.UpdateClassFunctions(GameObjectStructure, IC_BrivMaster_GameObjectStructure)
SH_UpdateClass.UpdateClassFunctions(_MemoryManager, IBM_Memory_Manager)


g_SharedData.IBM_Init()