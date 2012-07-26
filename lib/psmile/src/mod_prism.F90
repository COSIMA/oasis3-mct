module mod_prism

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

   use mod_prism_method ,only: prism_init_comp_proto     => prism_method_init
   use mod_prism_method ,only: prism_terminate_proto     => prism_method_terminate
   use mod_prism_method ,only: prism_get_localcomm_proto => prism_method_getlocalcomm
   use mod_prism_method ,only: prism_set_couplcomm       => prism_method_setcouplcomm
   use mod_prism_method ,only: prism_create_couplcomm    => prism_method_createcouplcomm
   use mod_prism_method ,only: prism_get_intracomm       => prism_method_get_intracomm
   use mod_prism_method ,only: prism_get_intercomm       => prism_method_get_intercomm
   use mod_prism_method, only: prism_set_debug           => prism_method_setdebug
   use mod_prism_method, only: prism_get_debug           => prism_method_getdebug
   use mod_prism_method ,only: prism_enddef_proto        => prism_method_enddef

   use mod_prism_part   ,only: prism_def_partition_proto => prism_part_def

   use mod_prism_var    ,only: prism_def_var_proto       => prism_var_def

   use mod_prism_getput_interface ,only: prism_get_proto
   use mod_prism_getput_interface ,only: prism_put_proto

   use mod_prism_grid   ,only: prism_start_grids_writing => prism_grid_start_grids_writing
   use mod_prism_grid   ,only: prism_write_grid          => prism_grid_write_grid
   use mod_prism_grid   ,only: prism_write_corner        => prism_grid_write_corner
   use mod_prism_grid   ,only: prism_write_mask          => prism_grid_write_mask
   use mod_prism_grid   ,only: prism_write_area          => prism_grid_write_area
   use mod_prism_grid   ,only: prism_terminate_grids_writing => prism_grid_terminate_grids_writing

   use mod_prism_sys    ,only: prism_abort_proto         => prism_sys_abort

   implicit none

!===============================================================================

end module mod_prism
