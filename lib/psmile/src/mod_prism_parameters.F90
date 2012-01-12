MODULE mod_prism_parameters
!
! -- clim.h   18-08-95   Version 2.0   Author: Laurent Terray
!    ******
!             26-10-99   Version 2.4   Jean Latour (F.S.E.) MPI-2 support
!@
! -- mod_clim_proto.f90  12-06-02  Version 3.0   A. Caubel 
! --                     20-05-03  added PRISM_Recvd/Sent   S.Legutke
     
!    ******************
!
!@  Contents : variables related to the CLIM library
!@  --------
!@
!
  USE mod_prism_kinds
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Debug    = 3
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Ok	 = 0 

!-----Ports status

  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_NotDef      = -2 
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Out         = 14 
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_In          = 15
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_InOut       = 2 
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Recvd       = 3 
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Sent        = 4 
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_LocTrans    = 5
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_ToRest      = 6
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Output      = 7
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_SentOut     = 8
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_ToRestOut   = 9
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_FromRest    = 10
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Input       = 11
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_RecvOut     = 12
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_FromRestOut = 13

!-----Coupler Comm Status

  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_NONE        = 100
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_COMM_READY  = 101
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_COMM_WAIT   = 102
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_PUT         = 103
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_GET         = 104

!-----Field status

  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_exported  = 1
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_ignored   = 2
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_input     = 3
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_output    = 4
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_expout    = 5
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_ignout    = 6
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_auxilary  = 7

!----Field local transformation

  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_instant = 1
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_average = 2
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_accumul = 3
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_min     = 4
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_max     = 5

!----Field conserv option

  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_cnone   = 0
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_cglobal = 1
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_cglbpos = 2
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_cbasbal = 4
  INTEGER (kind=ip_intwp_p), PARAMETER :: ip_cbaspos = 5

!-----Parallel distribution

  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Strategy = 1 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Segments = 2 

  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Serial   = 0 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Apple    = 1 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Box      = 2 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Orange   = 3 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Points   = 4 

  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Offset   = 2 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Length   = 3 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_SizeX    = 3 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_SizeY    = 4 
  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_LdX      = 5 

!-----Datatypes

  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Real   = 4  
  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_Double = 8 

!-----Quit parameters
!
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_ContPvm = 0 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_StopPvm = 1 
!
!-----Error Codes
!
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_MaxCodes  = -22 
!
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Ok	 = 0 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_FastExit  = -1 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_BadName   = -2 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_BadPort   = -3 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_BadType   = -4 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_DoubleDef= -5 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_NotStep   = -6 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_IncStep   = -7 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_IncSize   = -8 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_NotClim   = -9 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_TimeOut   = -10 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Pvm       = -11 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_FirstCall = -12 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_PbRoute   = -13 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Group     = -14 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_BadTaskId = -15 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_NoTask    = -16 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_InitBuff  = -17 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Pack      = -18 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Unpack    = -19 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Down      = -20 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_PvmExit   = -21 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: CLIM_Mpi       = -22 
!  INTEGER (kind=ip_intwp_p), PARAMETER :: PRISM_NotFreq  = -23 
!

END MODULE mod_prism_parameters

