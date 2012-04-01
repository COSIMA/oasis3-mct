module mod_oasis

! !USES:
   use mod_prism_kinds  ,only: ip_single_p
   use mod_prism_kinds  ,only: ip_double_p
   use mod_prism_kinds  ,only: ip_realwp_p
   use mod_prism_kinds  ,only: ll_single
   use mod_prism_kinds  ,only: ip_i2_p
   use mod_prism_kinds  ,only: ip_i4_p
   use mod_prism_kinds  ,only: ip_i8_p
   use mod_prism_kinds  ,only: ip_intwp_p

   use mod_prism_parameters

   use mod_prism_method ,only: oasis_init_comp     => prism_method_init
   use mod_prism_method ,only: oasis_terminate     => prism_method_terminate
   use mod_prism_method ,only: oasis_get_localcomm => prism_method_getlocalcomm
   use mod_prism_method ,only: oasis_get_intracomm => prism_method_get_intracomm
   use mod_prism_method ,only: oasis_get_intercomm => prism_method_get_intercomm
   use mod_prism_method ,only: oasis_set_debug     => prism_method_setdebug
   use mod_prism_method ,only: oasis_get_debug     => prism_method_getdebug
   use mod_prism_method ,only: oasis_enddef        => prism_method_enddef

   use mod_prism_part   ,only: oasis_def_partition => prism_part_def

   use mod_prism_var    ,only: oasis_def_var       => prism_var_def

   use mod_prism_getput_interface ,only: oasis_get => prism_get_proto
   use mod_prism_getput_interface ,only: oasis_put => prism_put_proto

   use mod_prism_grid   ,only: oasis_start_grids_writing => prism_grid_start_grids_writing
   use mod_prism_grid   ,only: oasis_write_grid          => prism_grid_write_grid
   use mod_prism_grid   ,only: oasis_write_corner        => prism_grid_write_corner
   use mod_prism_grid   ,only: oasis_write_mask          => prism_grid_write_mask
   use mod_prism_grid   ,only: oasis_write_area          => prism_grid_write_area
   use mod_prism_grid   ,only: oasis_terminate_grids_writing => prism_grid_terminate_grids_writing

   use mod_prism_sys    ,only: oasis_abort       => prism_sys_abort

   implicit none

!===============================================================================

end module mod_oasis
