!> @mainpage Oasis3-MCT Version 3.0: April, 2015
!> This provides documentation of the Oasis3-MCT implementation for developers.
!> A separate Oasis3-MCT User Guide is available for Oasis users.

!> Provides top level OASIS interfaces to the user community.

MODULE mod_oasis

! !USES:
  USE mod_oasis_kinds  ,ONLY: ip_single_p
  USE mod_oasis_kinds  ,ONLY: ip_double_p
  USE mod_oasis_kinds  ,ONLY: ip_realwp_p
  USE mod_oasis_kinds  ,ONLY: ll_single
  USE mod_oasis_kinds  ,ONLY: ip_i2_p
  USE mod_oasis_kinds  ,ONLY: ip_i4_p
  USE mod_oasis_kinds  ,ONLY: ip_i8_p
  USE mod_oasis_kinds  ,ONLY: ip_intwp_p

  USE mod_oasis_parameters
  USE mod_oasis_namcouple

  USE mod_oasis_method ,ONLY: oasis_init_comp    
  USE mod_oasis_method ,ONLY: oasis_terminate
  USE mod_oasis_method ,ONLY: oasis_enddef        

  USE mod_oasis_part   ,ONLY: oasis_def_partition 
  
  USE mod_oasis_var    ,ONLY: oasis_def_var      
  
  USE mod_oasis_getput_interface ,ONLY: oasis_get 
  USE mod_oasis_getput_interface ,ONLY: oasis_put 
  
  USE mod_oasis_grid   ,ONLY: oasis_start_grids_writing 
  USE mod_oasis_grid   ,ONLY: oasis_write_grid 
  USE mod_oasis_grid   ,ONLY: oasis_write_angle
  USE mod_oasis_grid   ,ONLY: oasis_write_corner        
  USE mod_oasis_grid   ,ONLY: oasis_write_mask          
  USE mod_oasis_grid   ,ONLY: oasis_write_area          
  USE mod_oasis_grid   ,ONLY: oasis_terminate_grids_writing 

  USE mod_oasis_auxiliary_routines ,ONLY: oasis_get_localcomm
  USE mod_oasis_auxiliary_routines ,ONLY: oasis_set_couplcomm
  USE mod_oasis_auxiliary_routines ,ONLY: oasis_create_couplcomm
  USE mod_oasis_auxiliary_routines ,ONLY: oasis_get_intracomm
  USE mod_oasis_auxiliary_routines ,ONLY: oasis_get_intercomm 
  USE mod_oasis_auxiliary_routines ,ONLY: oasis_set_debug     
  USE mod_oasis_auxiliary_routines ,ONLY: oasis_get_debug     
  USE mod_oasis_auxiliary_routines, ONLY: oasis_get_ncpl
  USE mod_oasis_auxiliary_routines, ONLY: oasis_get_freqs
  USE mod_oasis_auxiliary_routines, ONLY: oasis_put_inquire
  
  USE mod_oasis_sys    ,ONLY: oasis_abort       

  IMPLICIT NONE

!===============================================================================

END MODULE mod_oasis
