!> Provides top level OASIS interfaces to the user community using prism names
!> for backwards compatibility.

module mod_prism

! !USES:
   use mod_oasis_kinds  ,only: ip_single_p
   use mod_oasis_kinds  ,only: ip_double_p
   use mod_oasis_kinds  ,only: ip_realwp_p
   use mod_oasis_kinds  ,only: ll_single
   use mod_oasis_kinds  ,only: ip_i2_p
   use mod_oasis_kinds  ,only: ip_i4_p
   use mod_oasis_kinds  ,only: ip_i8_p
   use mod_oasis_kinds  ,only: ip_intwp_p

   use mod_oasis_parameters
   USE mod_oasis_namcouple

   use mod_oasis_method ,only: prism_init_comp_proto     => oasis_init_comp
   use mod_oasis_method ,only: prism_terminate_proto     => oasis_terminate
   use mod_oasis_method ,only: prism_enddef_proto        => oasis_enddef

   use mod_oasis_part   ,only: prism_def_partition_proto => oasis_def_partition 

   use mod_oasis_var    ,only: prism_def_var_proto       => oasis_def_var

   use mod_oasis_getput_interface ,only: prism_get_proto => oasis_get
   use mod_oasis_getput_interface ,only: prism_put_proto => oasis_put

   use mod_oasis_grid   ,only: prism_start_grids_writing => oasis_start_grids_writing
   use mod_oasis_grid   ,only: prism_write_grid          => oasis_write_grid 
   USE mod_oasis_grid   ,only: prism_write_angle         => oasis_write_angle
   use mod_oasis_grid   ,only: prism_write_corner        => oasis_write_corner 
   use mod_oasis_grid   ,only: prism_write_mask          => oasis_write_mask
   use mod_oasis_grid   ,only: prism_write_area          => oasis_write_area
   use mod_oasis_grid   ,only: prism_terminate_grids_writing => oasis_terminate_grids_writing

   use mod_oasis_auxiliary_routines ,only: prism_get_localcomm_proto => oasis_get_localcomm
   use mod_oasis_auxiliary_routines ,only: prism_set_couplcomm       => oasis_set_couplcomm
   use mod_oasis_auxiliary_routines ,only: prism_create_couplcomm    => oasis_create_couplcomm
   use mod_oasis_auxiliary_routines ,only: prism_get_intracomm       => oasis_get_intracomm
   use mod_oasis_auxiliary_routines ,only: prism_get_intercomm       => oasis_get_intercomm 
   use mod_oasis_auxiliary_routines, only: prism_set_debug           => oasis_set_debug 
   use mod_oasis_auxiliary_routines, only: prism_get_debug           => oasis_get_debug
   use mod_oasis_auxiliary_routines, only: prism_get_ncpl            => oasis_get_ncpl
   use mod_oasis_auxiliary_routines, only: prism_get_freqs           => oasis_get_freqs
   use mod_oasis_auxiliary_routines, only: prism_put_inquire         => oasis_put_inquire

   use mod_oasis_sys    ,only: prism_abort_proto         => oasis_abort

   implicit none

!===============================================================================

end module mod_prism
