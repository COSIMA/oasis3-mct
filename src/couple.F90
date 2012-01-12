    PROGRAM couple

    USE mod_prism
    USE mod_prism_data

    IMPLICIT NONE

    character(len=16)        :: cdnam
    integer(kind=ip_intwp_p) :: ierr, ib_b
    integer(kind=ip_intwp_p) :: mynummod
    integer(kind=ip_intwp_p) :: comm_world
    integer(kind=ip_intwp_p) :: comm_local
    character(len=*),parameter :: subname = 'program_couple'


! ---------------------------- Poema verses ----------------------------

    mynummod = 0
    cdnam = 'oasis'

    call prism_init_comp_proto(mynummod,cdnam,ierr)

    comm_world = mpi_comm_global
    comm_local = mpi_comm_local

    write(nulprt,*) subname,' comms ',comm_world,comm_local

    call prism_enddef_proto (ierr)
  
    call prism_terminate_proto(ierr)

    END Program couple
