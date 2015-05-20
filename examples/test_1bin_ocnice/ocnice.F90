!------------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!=============================================================================
!
PROGRAM ocnice

  ! Use for netCDF library
  USE netcdf
  ! Use for OASIS communication library
  USE mod_oasis
  USE mod_flddiag

  IMPLICIT NONE

  INCLUDE 'mpif.h'

  ! -------------------------------------------------------------------------------
  ! This is a sample program for coupling submodels in a single exectuable.
  ! In this case, there is a single oasis component (ocnice) and each submodel
  ! (ocn, ice, and io) is running concurrently on a unique set of ocnice tasks.
  ! Within the ocn, there are two submodels running sequentially (phys and dyna)
  ! There is 2-way coupling between the phys and dyna in the ocean model, and between
  ! the ocean and ice models.  There is 1-way coupling to the io task from the
  ! ocean and ice models.
  ! -------------------------------------------------------------------------------

  ! Component stuff
  CHARACTER(len=*),PARAMETER :: comp_name = 'ocnice'
  INTEGER             :: comp_id                   ! component identification
  LOGICAL             :: ocn_pe, ice_pe, io_pe     ! flag whether task is ocn, ice, or io
  CHARACTER(len=8)    :: submodel

  ! MPI stuff
  INTEGER             :: mype, npes ! rank and  number of pe
  INTEGER             :: localComm  ! ocnice MPI communicator
  INTEGER             :: mpiocn, mpiice, mpiio  ! local submodel communicators
  INTEGER             :: ocnpe_start, ocnpe_end
  INTEGER             :: icepe_start, icepe_end
  INTEGER             :: iope_start , iope_end

  ! Timestepping stuff
  INTEGER             :: ib
  INTEGER, PARAMETER  :: ib_max  = 4      ! number of time steps
  INTEGER, PARAMETER  :: delta_t = 3600   ! time step
  INTEGER             :: time             ! Time used in oasis_put/get 

  ! Variable ids (need to be saved between init and run phases)
  ! In real application, these would be saved in a separate ocn, ice, and io module
  ! Not all these variables are used, this just provides a full matrix.
  ! io does not send, it just receives in this example.

  INTEGER :: &
             vid_p1 , vid_p2 , vid_p3 , vid_p4 , vid_p5 , &    ! ocn phys vars to send
             vid_p1d, vid_p2d, vid_p3d, vid_p4d, vid_p5d, &    ! ocn phys vars recved by dyna
             vid_p1i, vid_p2i, vid_p3i, vid_p4i, vid_p5i, &    ! ocn phys vars recved by ice
             vid_p1o, vid_p2o, vid_p3o, vid_p4o, vid_p5o, &    ! ocn phys vars recved by io
             vid_d1 , vid_d2 , vid_d3 , vid_d4 , vid_d5 , &    ! ocn dyna vars to send
             vid_d1p, vid_d2p, vid_d3p, vid_d4p, vid_d5p, &    ! ocn dyna vars recved by phys
             vid_d1i, vid_d2i, vid_d3i, vid_d4i, vid_d5i, &    ! ocn dyna vars recved by ice
             vid_d1o, vid_d2o, vid_d3o, vid_d4o, vid_d5o, &    ! ocn dyna vars recved by io
             vid_i1 , vid_i2 , vid_i3 , vid_i4 , vid_i5 , &    ! ice vars to send
             vid_i1p, vid_i2p, vid_i3p, vid_i4p, vid_i5p, &    ! ice vars recved by phys
             vid_i1d, vid_i2d, vid_i3d, vid_i4d, vid_i5d, &    ! ice vars recved by dyna
             vid_i1o, vid_i2o, vid_i3o, vid_i4o, vid_i5o       ! ice vars recved by io

  REAL*8, allocatable :: &
             fld_p1 (:,:), fld_p2 (:,:), fld_p3 (:,:), fld_p4 (:,:), fld_p5 (:,:), &    ! ocn phys vars to send
             fld_p1d(:,:), fld_p2d(:,:), fld_p3d(:,:), fld_p4d(:,:), fld_p5d(:,:), &    ! ocn phys vars recved by dyna
             fld_p1i(:,:), fld_p2i(:,:), fld_p3i(:,:), fld_p4i(:,:), fld_p5i(:,:), &    ! ocn phys vars recved by ice
             fld_p1o(:,:), fld_p2o(:,:), fld_p3o(:,:), fld_p4o(:,:), fld_p5o(:,:), &    ! ocn phys vars recved by io
             fld_d1 (:,:), fld_d2 (:,:), fld_d3 (:,:), fld_d4 (:,:), fld_d5 (:,:), &    ! ocn dyna vars to send
             fld_d1p(:,:), fld_d2p(:,:), fld_d3p(:,:), fld_d4p(:,:), fld_d5p(:,:), &    ! ocn dyna vars recved by phys
             fld_d1i(:,:), fld_d2i(:,:), fld_d3i(:,:), fld_d4i(:,:), fld_d5i(:,:), &    ! ocn dyna vars recved by ice
             fld_d1o(:,:), fld_d2o(:,:), fld_d3o(:,:), fld_d4o(:,:), fld_d5o(:,:), &    ! ocn dyna vars recved by io
             fld_i1 (:,:), fld_i2 (:,:), fld_i3 (:,:), fld_i4 (:,:), fld_i5 (:,:), &    ! ice vars to send
             fld_i1p(:,:), fld_i2p(:,:), fld_i3p(:,:), fld_i4p(:,:), fld_i5p(:,:), &    ! ice vars recved by phys
             fld_i1d(:,:), fld_i2d(:,:), fld_i3d(:,:), fld_i4d(:,:), fld_i5d(:,:), &    ! ice vars recved by dyna
             fld_i1o(:,:), fld_i2o(:,:), fld_i3o(:,:), fld_i4o(:,:), fld_i5o(:,:)       ! ice vars recved by io

  ! Other stuff
  INTEGER             :: ierror, w_unit, icpl
  CHARACTER(len=32)   :: filename
  INTEGER             :: FILE_Debug=1 ! to manage writing in the debug file w_unit
  REAL*8, parameter :: spval = -1.0e36
  CHARACTER(len=*),PARAMETER :: subname = '(ocnice) '

  !!!!!!!!!!!!!!!!! OASIS_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     call OASIS_init_comp  (comp_id, comp_name, ierror)
     call OASIS_get_localcomm  ( localComm, ierror )

  !--- Get MPI size and rank ---

     call MPI_Comm_Size ( localComm, npes, ierror )
     call MPI_Comm_Rank ( localComm, mype, ierror )

  !--- model output file for each task ---

     w_unit = 100 + mype
     write(filename,'(a,I3.3)') trim(comp_name)//'.out_',mype
     open(w_unit, file=trim(filename), form='formatted')

  !!!!!!!!!!!!!!!!! DEFINE model layout!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     ocn_pe = .false.
     ice_pe = .false.
     io_pe  = .false.

  !--- arbitrarily assign ocn, io, and ice tasks ---

     ocnpe_start = 0
     ocnpe_end   = npes/2
     icepe_start = min(npes-1,npes/2+1)
     icepe_end   = max(npes-2,icepe_start)
     iope_start  = npes - 1
     iope_end    = npes - 1

     WRITE(w_unit,*) subname,'ocnpe_start :',ocnpe_start
     WRITE(w_unit,*) subname,'ocnpe_end :',ocnpe_end
     WRITE(w_unit,*) subname,'icepe_start :',icepe_start
     WRITE(w_unit,*) subname,'icepe_end :',icepe_end
     WRITE(w_unit,*) subname,'iope_start :',iope_start
     WRITE(w_unit,*) subname,'iope_end :',iope_end


     submodel = ' '
     if (mype >= ocnpe_start .and. mype <= ocnpe_end) then
        submodel = trim(submodel)//'+ocn'
        ocn_pe = .true.
     endif
     if (mype >= icepe_start .and. mype <= icepe_end) then
        submodel = trim(submodel)//'+ice'
        ice_pe = .true.
     endif
     if (mype >= iope_start .and. mype <= iope_end) then
        submodel = trim(submodel)//'+io'
        io_pe = .true.
     endif

     write(w_unit,*) subname,' local comm info: comp_name = ',trim(comp_name),comp_id
     write(w_unit,*) subname,' local comm info: mype      = ',mype,npes
     write(w_unit,*) subname,' decomp info: submodels     = ',trim(submodel)
     write(w_unit,*) subname,' decomp info: ocn, ice, io  = ',ocn_pe, ice_pe, io_pe

     if (iope_end /= iope_start) then
        write(w_unit,*) subname,' ERROR: io submodel must be run on 1 task'
        stop
     endif

  !--- create submodel mpicomms ---

     icpl = MPI_UNDEFINED
     if (ocn_pe) icpl = 1
     call MPI_COMM_SPLIT(localcomm, icpl, 1, mpiocn, ierror)

     icpl = MPI_UNDEFINED
     if (ice_pe) icpl = 1
     call MPI_COMM_SPLIT(localcomm, icpl, 1, mpiice, ierror)

     icpl = MPI_UNDEFINED
     if (io_pe) icpl = 1
     call MPI_COMM_SPLIT(localcomm, icpl, 1, mpiio , ierror)

  !!!!!!!!!!!!!!!!! Run submodels !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     if (ocn_pe) call ocn_init()
     if (ice_pe) call ice_init()
     if (io_pe)  call io_init()

     call OASIS_enddef(ierror)

     time = 0
     do ib = 1, ib_max

        if (ocn_pe) call ocn_run(time)
        if (ice_pe) call ice_run(time)
        if (io_pe)  call io_run(time)

        time = time + delta_t

     enddo

     call OASIS_terminate(ierror)

  !!!!!!!!!!!!!!!!!! END of PROGRAM OCNICE !!!!!!!!!!!!!!!!!!!!!!


  !---------------------------------------------------------------------------
  CONTAINS
  !---------------------------------------------------------------------------

  SUBROUTINE ocn_init()

  implicit none

  !--- local ---
  INTEGER :: il_size, dpe, dpes
  INTEGER :: var_nodims(2)
  CHARACTER(len=8) :: decomp
  INTEGER, allocatable :: il_paral(:)
  INTEGER :: part_phys, part_dyna
  INTEGER :: var_shape_phys(4)
  INTEGER :: var_shape_dyna(4)
  INTEGER :: nlon, nlat
  CHARACTER(len=32) :: filename
  CHARACTER(len=*),PARAMETER :: subname = '(ocn_init) '

  !--- initialize grid and decomp and call OASIS_def_partition
  !--- one grid (lmdz), two decomps (phys, dyna)

     filename = 'grid_lmdz.nc'
     call read_dimgrid(nlon,nlat,filename,w_unit,FILE_Debug)
     write(w_unit,*) subname,'ocn grid: ',nlon,nlat

  !--- phys decomp ---

     decomp   = 'apple'
     il_size = 3
     allocate(il_paral(il_size))
     dpe  = mype - ocnpe_start
     dpes = ocnpe_end - ocnpe_start + 1
     call decomp_def (il_paral,il_size,nlon,nlat,dpe,dpes,w_unit)
     var_shape_phys(:) = 1
     var_shape_phys(2) = il_paral(3)

     call OASIS_def_partition  (part_phys, il_paral, ierror, name='partocn_phys')
     deallocate(il_paral)

  !--- dyna decomp ---

     decomp   = 'box'
     il_size = 5
     allocate(il_paral(il_size))
     dpe  = mype - ocnpe_start
     dpes = ocnpe_end - ocnpe_start + 1
     call decomp_def (il_paral,il_size,nlon,nlat,dpe,dpes,w_unit)
     var_shape_dyna(:) = 1
     var_shape_dyna(2) = il_paral(3)
     var_shape_dyna(4) = il_paral(4)

     call OASIS_def_partition  (part_dyna, il_paral, ierror, name='partocn_dyna')
     deallocate(il_paral)

  !--- define variables ---

     var_nodims(1) = 2    ! Rank of the field array is 2
     var_nodims(2) = 1    ! Bundles always 1 for OASIS3
     call OASIS_def_var  (vid_p1 , 'OCN_P1' , part_phys, var_nodims, OASIS_Out, var_shape_phys, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p2 , 'OCN_P2' , part_phys, var_nodims, OASIS_Out, var_shape_phys, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p3 , 'OCN_P3' , part_phys, var_nodims, OASIS_Out, var_shape_phys, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p4 , 'OCN_P4' , part_phys, var_nodims, OASIS_Out, var_shape_phys, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p5 , 'OCN_P5' , part_phys, var_nodims, OASIS_Out, var_shape_phys, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d1p, 'OCN_D1P', part_phys, var_nodims, OASIS_In , var_shape_phys, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d2p, 'OCN_D2P', part_phys, var_nodims, OASIS_In , var_shape_phys, OASIS_Real, ierror)
     call OASIS_def_var  (vid_i1p, 'ICE_I1P', part_phys, var_nodims, OASIS_In , var_shape_phys, OASIS_Real, ierror)
     call OASIS_def_var  (vid_i2p, 'ICE_I2P', part_phys, var_nodims, OASIS_In , var_shape_phys, OASIS_Real, ierror)

     call OASIS_def_var  (vid_d1 , 'OCN_D1' , part_dyna, var_nodims, OASIS_Out, var_shape_dyna, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d2 , 'OCN_D2' , part_dyna, var_nodims, OASIS_Out, var_shape_dyna, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d3 , 'OCN_D3' , part_dyna, var_nodims, OASIS_Out, var_shape_dyna, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d4 , 'OCN_D4' , part_dyna, var_nodims, OASIS_Out, var_shape_dyna, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d5 , 'OCN_D5' , part_dyna, var_nodims, OASIS_Out, var_shape_dyna, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p1d, 'OCN_P1D', part_dyna, var_nodims, OASIS_In , var_shape_dyna, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p2d, 'OCN_P2D', part_dyna, var_nodims, OASIS_In , var_shape_dyna, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p3d, 'OCN_P3D', part_dyna, var_nodims, OASIS_In , var_shape_dyna, OASIS_Real, ierror)

  !--- allocate arrays for variables ---

     allocate(fld_p1 (var_shape_phys(2),var_shape_phys(4))); fld_p1  = spval
     allocate(fld_p2 (var_shape_phys(2),var_shape_phys(4))); fld_p2  = spval
     allocate(fld_p3 (var_shape_phys(2),var_shape_phys(4))); fld_p3  = spval
     allocate(fld_p4 (var_shape_phys(2),var_shape_phys(4))); fld_p4  = spval
     allocate(fld_p5 (var_shape_phys(2),var_shape_phys(4))); fld_p5  = spval
     allocate(fld_d1p(var_shape_phys(2),var_shape_phys(4))); fld_d1p = spval
     allocate(fld_d2p(var_shape_phys(2),var_shape_phys(4))); fld_d2p = spval
     allocate(fld_i1p(var_shape_phys(2),var_shape_phys(4))); fld_i1p = spval
     allocate(fld_i2p(var_shape_phys(2),var_shape_phys(4))); fld_i2p = spval

     allocate(fld_d1 (var_shape_dyna(2),var_shape_dyna(4))); fld_d1  = spval
     allocate(fld_d2 (var_shape_dyna(2),var_shape_dyna(4))); fld_d2  = spval
     allocate(fld_d3 (var_shape_dyna(2),var_shape_dyna(4))); fld_d3  = spval
     allocate(fld_d4 (var_shape_dyna(2),var_shape_dyna(4))); fld_d4  = spval
     allocate(fld_d5 (var_shape_dyna(2),var_shape_dyna(4))); fld_d5  = spval
     allocate(fld_p1d(var_shape_dyna(2),var_shape_dyna(4))); fld_p1d = spval
     allocate(fld_p2d(var_shape_dyna(2),var_shape_dyna(4))); fld_p2d = spval
     allocate(fld_p3d(var_shape_dyna(2),var_shape_dyna(4))); fld_p3d = spval

  END SUBROUTINE ocn_init

  !---------------------------------------------------------------------------
  SUBROUTINE ocn_run(time)

  implicit none

  INTEGER, intent(in) :: time

  !--- local ---
  REAL*8 :: fmin, fmax, fsum
  CHARACTER(len=*),PARAMETER :: subname = '(ocn_run) '

  !!!!!! PHYSICS PART !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     write(w_unit,*) subname,' physics time = ',time

  !--- physics get ---

     call OASIS_get(vid_d1p, time, fld_d1p, ierror)
     call OASIS_get(vid_d2p, time, fld_d2p, ierror)
     call OASIS_get(vid_i1p, time, fld_i1p, ierror)
     call OASIS_get(vid_i2p, time, fld_i2p, ierror)

  !---- advance physics ---

     call flddiag(fld_d1p, fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_d1p local  = ',minval(fld_d1p),maxval(fld_d1p),sum(fld_d1p)
     write(w_unit,*) subname,' fld_d1p global = ',fmin,fmax,fsum
     call flddiag(fld_d2p, fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_d2p local  = ',minval(fld_d2p),maxval(fld_d2p),sum(fld_d2p)
     write(w_unit,*) subname,' fld_d2p global = ',fmin,fmax,fsum
     call flddiag(fld_i1p, fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_i1p local  = ',minval(fld_i1p),maxval(fld_i1p),sum(fld_i1p)
     write(w_unit,*) subname,' fld_i1p global = ',fmin,fmax,fsum
     call flddiag(fld_i2p, fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_i2p local  = ',minval(fld_i2p),maxval(fld_i2p),sum(fld_i2p)
     write(w_unit,*) subname,' fld_i2p global = ',fmin,fmax,fsum

     fld_p1 = mype * 100. + 1.0 + float(time)/1.0e6
     fld_p2 = mype * 100. + 2.0 + float(time)/1.0e6
     fld_p3 = mype * 100. + 3.0 + float(time)/1.0e6
     fld_p4 = mype * 100. + 4.0 + float(time)/1.0e6
     fld_p5 = mype * 100. + 5.0 + float(time)/1.0e6

     call flddiag(fld_p1 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_p1  local  = ',minval(fld_p1 ),maxval(fld_p1 ),sum(fld_p1 )
     write(w_unit,*) subname,' fld_p1  global = ',fmin,fmax,fsum
     call flddiag(fld_p2 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_p2  local  = ',minval(fld_p2 ),maxval(fld_p2 ),sum(fld_p2 )
     write(w_unit,*) subname,' fld_p2  global = ',fmin,fmax,fsum
     call flddiag(fld_p3 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_p3  local  = ',minval(fld_p3 ),maxval(fld_p3 ),sum(fld_p3 )
     write(w_unit,*) subname,' fld_p3  global = ',fmin,fmax,fsum
     call flddiag(fld_p4 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_p4  local  = ',minval(fld_p4 ),maxval(fld_p4 ),sum(fld_p4 )
     write(w_unit,*) subname,' fld_p4  global = ',fmin,fmax,fsum
     call flddiag(fld_p5 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_p5  local  = ',minval(fld_p5 ),maxval(fld_p5 ),sum(fld_p5 )
     write(w_unit,*) subname,' fld_p5  global = ',fmin,fmax,fsum

  !---- physics put ---

     call OASIS_put(vid_p1 , time, fld_p1 , ierror)
     call OASIS_put(vid_p2 , time, fld_p2 , ierror)
     call OASIS_put(vid_p3 , time, fld_p3 , ierror)
     call OASIS_put(vid_p4 , time, fld_p4 , ierror)
     call OASIS_put(vid_p5 , time, fld_p5 , ierror)

  !!!!!! DYNAMICS PART !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     write(w_unit,*) subname,' dynamics time = ',time

  !--- dynamics get ---

     call OASIS_get(vid_p1d, time, fld_p1d, ierror)
     call OASIS_get(vid_p2d, time, fld_p2d, ierror)
     call OASIS_get(vid_p3d, time, fld_p3d, ierror)

  !---- advance dyanmics ---

     call flddiag(fld_p1d, fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_p1d local  = ',minval(fld_p1d),maxval(fld_p1d),sum(fld_p1d)
     write(w_unit,*) subname,' fld_p1d global = ',fmin,fmax,fsum
     call flddiag(fld_p2d, fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_p2d local  = ',minval(fld_p2d),maxval(fld_p2d),sum(fld_p2d)
     write(w_unit,*) subname,' fld_p2d global = ',fmin,fmax,fsum
     call flddiag(fld_p3d, fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_p3d local  = ',minval(fld_p3d),maxval(fld_p3d),sum(fld_p3d)
     write(w_unit,*) subname,' fld_p3d global = ',fmin,fmax,fsum

     fld_d1 = mype * 200. + 1.0 + float(time)/1.0e6
     fld_d2 = mype * 200. + 2.0 + float(time)/1.0e6
     fld_d3 = mype * 200. + 3.0 + float(time)/1.0e6
     fld_d4 = mype * 200. + 4.0 + float(time)/1.0e6
     fld_d5 = mype * 200. + 5.0 + float(time)/1.0e6

     call flddiag(fld_d1 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_d1  local  = ',minval(fld_d1 ),maxval(fld_d1 ),sum(fld_d1 )
     write(w_unit,*) subname,' fld_d1  global = ',fmin,fmax,fsum
     call flddiag(fld_d2 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_d2  local  = ',minval(fld_d2 ),maxval(fld_d2 ),sum(fld_d2 )
     write(w_unit,*) subname,' fld_d2  global = ',fmin,fmax,fsum
     call flddiag(fld_d3 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_d3  local  = ',minval(fld_d3 ),maxval(fld_d3 ),sum(fld_d3 )
     write(w_unit,*) subname,' fld_d3  global = ',fmin,fmax,fsum
     call flddiag(fld_d4 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_d4  local  = ',minval(fld_d4 ),maxval(fld_d4 ),sum(fld_d4 )
     write(w_unit,*) subname,' fld_d4  global = ',fmin,fmax,fsum
     call flddiag(fld_d5 , fmin, fmax, fsum, mpiocn)
     write(w_unit,*) subname,' fld_d5  local  = ',minval(fld_d5 ),maxval(fld_d5 ),sum(fld_d5 )
     write(w_unit,*) subname,' fld_d5  global = ',fmin,fmax,fsum

  !---- dynamics put ---

     call OASIS_put(vid_d1 , time, fld_d1 , ierror)
     call OASIS_put(vid_d2 , time, fld_d2 , ierror)
     call OASIS_put(vid_d3 , time, fld_d3 , ierror)
     call OASIS_put(vid_d4 , time, fld_d4 , ierror)
     call OASIS_put(vid_d5 , time, fld_d5 , ierror)

  END SUBROUTINE ocn_run
  !---------------------------------------------------------------------------

  SUBROUTINE ice_init()

  implicit none

  !--- local ---
  INTEGER :: il_size, dpe, dpes
  INTEGER :: var_nodims(2)
  CHARACTER(len=8) :: decomp
  INTEGER, allocatable :: il_paral(:)
  INTEGER :: part_ice
  INTEGER :: var_shape_ice(4)
  INTEGER :: nlon, nlat
  CHARACTER(len=32) :: filename
  CHARACTER(len=*),PARAMETER :: subname = '(ice_init) '

  !--- initialize grid and decomp and call OASIS_def_partition ---
  !--- one grid (lmd2), one decomp (ice) ---

     filename = 'grid_lmd2.nc'
     call read_dimgrid(nlon,nlat,filename,w_unit,FILE_Debug)
     write(w_unit,*) subname,'ice grid: ',nlon,nlat

  !--- ice decomp ---

     decomp   = 'apple'
     il_size = 3
     allocate(il_paral(il_size))
     dpe  = mype - icepe_start
     dpes = icepe_end - icepe_start + 1
     call decomp_def (il_paral,il_size,nlon,nlat,dpe,dpes,w_unit)
     var_shape_ice(:) = 1
     var_shape_ice(2) = il_paral(3)

     call OASIS_def_partition  (part_ice, il_paral, ierror, name='partice_ice')
     deallocate(il_paral)

  !--- define variables ---

     var_nodims(1) = 2    ! Rank of the field array is 2
     var_nodims(2) = 1    ! Bundles always 1 for OASIS3
     call OASIS_def_var  (vid_i1 , 'ICE_I1' , part_ice, var_nodims, OASIS_Out, var_shape_ice, OASIS_Real, ierror)
     call OASIS_def_var  (vid_i2 , 'ICE_I2' , part_ice, var_nodims, OASIS_Out, var_shape_ice, OASIS_Real, ierror)
     call OASIS_def_var  (vid_i3 , 'ICE_I3' , part_ice, var_nodims, OASIS_Out, var_shape_ice, OASIS_Real, ierror)
     call OASIS_def_var  (vid_i4 , 'ICE_I4' , part_ice, var_nodims, OASIS_Out, var_shape_ice, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p3i, 'OCN_P3I', part_ice, var_nodims, OASIS_In , var_shape_ice, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p4i, 'OCN_P4I', part_ice, var_nodims, OASIS_In , var_shape_ice, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d3i, 'OCN_D3I', part_ice, var_nodims, OASIS_In , var_shape_ice, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d4i, 'OCN_D4I', part_ice, var_nodims, OASIS_In , var_shape_ice, OASIS_Real, ierror)

  !--- allocate arrays for variables ---

     allocate(fld_i1 (var_shape_ice(2),var_shape_ice(4))); fld_i1  = spval
     allocate(fld_i2 (var_shape_ice(2),var_shape_ice(4))); fld_i2  = spval
     allocate(fld_i3 (var_shape_ice(2),var_shape_ice(4))); fld_i3  = spval
     allocate(fld_i4 (var_shape_ice(2),var_shape_ice(4))); fld_i4  = spval
     allocate(fld_p3i(var_shape_ice(2),var_shape_ice(4))); fld_p3i = spval
     allocate(fld_p4i(var_shape_ice(2),var_shape_ice(4))); fld_p4i = spval
     allocate(fld_d3i(var_shape_ice(2),var_shape_ice(4))); fld_d3i = spval
     allocate(fld_d4i(var_shape_ice(2),var_shape_ice(4))); fld_d4i = spval


  END SUBROUTINE ice_init

  !---------------------------------------------------------------------------

  SUBROUTINE ice_run(time)

  implicit none

  INTEGER, intent(in) :: time

  !--- local ---
  REAL*8 :: fmin, fmax, fsum
  CHARACTER(len=*),PARAMETER :: subname = '(ice_run) '

  !!!!!! ICE PART !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     write(w_unit,*) subname,' ice time = ',time

  !--- ice get ---

     call OASIS_get(vid_p3i, time, fld_p3i, ierror)
     call OASIS_get(vid_p4i, time, fld_p4i, ierror)
     call OASIS_get(vid_d3i, time, fld_d3i, ierror)
     call OASIS_get(vid_d4i, time, fld_d4i, ierror)

  !---- advance ice ---

     call flddiag(fld_p3i, fmin, fmax, fsum, mpiice)
     write(w_unit,*) subname,' fld_p3i local  = ',minval(fld_p3i),maxval(fld_p3i),sum(fld_p3i)
     write(w_unit,*) subname,' fld_p3i global = ',fmin,fmax,fsum
     call flddiag(fld_p4i, fmin, fmax, fsum, mpiice)
     write(w_unit,*) subname,' fld_p4i local  = ',minval(fld_p4i),maxval(fld_p4i),sum(fld_p4i)
     write(w_unit,*) subname,' fld_p4i global = ',fmin,fmax,fsum
     call flddiag(fld_d3i, fmin, fmax, fsum, mpiice)
     write(w_unit,*) subname,' fld_d3i local  = ',minval(fld_d3i),maxval(fld_d3i),sum(fld_d3i)
     write(w_unit,*) subname,' fld_d3i global = ',fmin,fmax,fsum
     call flddiag(fld_d4i, fmin, fmax, fsum, mpiice)
     write(w_unit,*) subname,' fld_d4i local  = ',minval(fld_d4i),maxval(fld_d4i),sum(fld_d4i)
     write(w_unit,*) subname,' fld_d4i global = ',fmin,fmax,fsum

     fld_i1 = mype * 300. + 1.0 + float(time)/1.0e6
     fld_i2 = mype * 300. + 2.0 + float(time)/1.0e6
     fld_i3 = mype * 300. + 3.0 + float(time)/1.0e6
     fld_i4 = mype * 300. + 4.0 + float(time)/1.0e6

     call flddiag(fld_i1 , fmin, fmax, fsum, mpiice)
     write(w_unit,*) subname,' fld_i1  local  = ',minval(fld_i1 ),maxval(fld_i1 ),sum(fld_i1 )
     write(w_unit,*) subname,' fld_i1  global = ',fmin,fmax,fsum
     call flddiag(fld_i2 , fmin, fmax, fsum, mpiice)
     write(w_unit,*) subname,' fld_i2  local  = ',minval(fld_i2 ),maxval(fld_i2 ),sum(fld_i2 )
     write(w_unit,*) subname,' fld_i2  global = ',fmin,fmax,fsum
     call flddiag(fld_i3 , fmin, fmax, fsum, mpiice)
     write(w_unit,*) subname,' fld_i3  local  = ',minval(fld_i3 ),maxval(fld_i3 ),sum(fld_i3 )
     write(w_unit,*) subname,' fld_i3  global = ',fmin,fmax,fsum
     call flddiag(fld_i4 , fmin, fmax, fsum, mpiice)
     write(w_unit,*) subname,' fld_i4  local  = ',minval(fld_i4 ),maxval(fld_i4 ),sum(fld_i4 )
     write(w_unit,*) subname,' fld_i4  global = ',fmin,fmax,fsum

  !---- ice put ---

     call OASIS_put(vid_i1 , time, fld_i1 , ierror)
     call OASIS_put(vid_i2 , time, fld_i2 , ierror)
     call OASIS_put(vid_i3 , time, fld_i3 , ierror)
     call OASIS_put(vid_i4 , time, fld_i4 , ierror)


  END SUBROUTINE ice_run

  !---------------------------------------------------------------------------

  SUBROUTINE io_init()

  implicit none

  !--- local ---
  INTEGER :: il_size, dpe, dpes
  INTEGER :: var_nodims(2)
  CHARACTER(len=8) :: decomp
  INTEGER, allocatable :: il_paral(:)
  INTEGER :: part_ocn, part_ice
  INTEGER :: var_shape_ocn(4)
  INTEGER :: var_shape_ice(4)
  INTEGER :: nlon, nlat
  CHARACTER(len=32) :: filename
  CHARACTER(len=*),PARAMETER :: subname = '(io_init) '

  !--- initialize grid and decomp and call OASIS_def_partition
  !--- two grids (lmdz, lmd2), two decomps (ocn, ice)
  !--- this assumes decomps on 1 task only

  !--- ocn grid/decomp ---

     filename = 'grid_lmdz.nc'
     call read_dimgrid(nlon,nlat,filename,w_unit,FILE_Debug)
     write(w_unit,*) subname,'io ocn grid: ',nlon,nlat

     decomp   = 'serial'
     il_size = 3
     allocate(il_paral(il_size))
     il_paral(:) = 0
     il_paral(3) = nlon*nlat
     var_shape_ocn(:) = 1
     var_shape_ocn(2) = nlon*nlat

     call OASIS_def_partition  (part_ocn, il_paral, ierror, name='partio_ocn')
     deallocate(il_paral)

  !--- ice grid/decomp ---

     filename = 'grid_lmd2.nc'
     call read_dimgrid(nlon,nlat,filename,w_unit,FILE_Debug)
     write(w_unit,*) subname,'io ice grid: ',nlon,nlat

     decomp   = 'serial'
     il_size = 3
     allocate(il_paral(il_size))
     il_paral(:) = 0
     il_paral(3) = nlon*nlat
     var_shape_ice(:) = 1
     var_shape_ice(2) = nlon*nlat

     call OASIS_def_partition  (part_ice, il_paral, ierror, name='partio_ice')
     deallocate(il_paral)

  !--- define variables ---

     var_nodims(1) = 2    ! Rank of the field array is 2
     var_nodims(2) = 1    ! Bundles always 1 for OASIS3
     call OASIS_def_var  (vid_p4o, 'OCN_P4O', part_ocn, var_nodims, OASIS_In , var_shape_ocn, OASIS_Real, ierror)
     call OASIS_def_var  (vid_p5o, 'OCN_P5O', part_ocn, var_nodims, OASIS_In , var_shape_ocn, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d4o, 'OCN_D4O', part_ocn, var_nodims, OASIS_In , var_shape_ocn, OASIS_Real, ierror)
     call OASIS_def_var  (vid_d5o, 'OCN_D5O', part_ocn, var_nodims, OASIS_In , var_shape_ocn, OASIS_Real, ierror)

     call OASIS_def_var  (vid_i3o, 'ICE_I3O', part_ice, var_nodims, OASIS_In , var_shape_ice, OASIS_Real, ierror)
     call OASIS_def_var  (vid_i4o, 'ICE_I4O', part_ice, var_nodims, OASIS_In , var_shape_ice, OASIS_Real, ierror)

  !--- allocate arrays for variables ---

     allocate(fld_p4o(var_shape_ocn(2),var_shape_ocn(4))); fld_p4o = spval
     allocate(fld_p5o(var_shape_ocn(2),var_shape_ocn(4))); fld_p5o = spval
     allocate(fld_d4o(var_shape_ocn(2),var_shape_ocn(4))); fld_d4o = spval
     allocate(fld_d5o(var_shape_ocn(2),var_shape_ocn(4))); fld_d5o = spval

     allocate(fld_i3o(var_shape_ice(2),var_shape_ice(4))); fld_i3o = spval
     allocate(fld_i4o(var_shape_ice(2),var_shape_ice(4))); fld_i4o = spval

  END SUBROUTINE io_init

  !---------------------------------------------------------------------------

  SUBROUTINE io_run(time)

  implicit none

  INTEGER, intent(in) :: time

  !--- local ---
  REAL*8 :: fmin, fmax, fsum
  CHARACTER(len=*),PARAMETER :: subname = '(io_run) '

  !!!!!! IO PART !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     write(w_unit,*) subname,' io time = ',time

  !--- io get ---

     call OASIS_get(vid_p4o, time, fld_p4o, ierror)
     call OASIS_get(vid_p5o, time, fld_p5o, ierror)
     call OASIS_get(vid_d4o, time, fld_d4o, ierror)
     call OASIS_get(vid_d5o, time, fld_d5o, ierror)
     call OASIS_get(vid_i3o, time, fld_i3o, ierror)
     call OASIS_get(vid_i4o, time, fld_i4o, ierror)


     call flddiag(fld_p4o, fmin, fmax, fsum, mpiio)
     write(w_unit,*) subname,' fld_p4o local  = ',minval(fld_p4o),maxval(fld_p4o),sum(fld_p4o)
     write(w_unit,*) subname,' fld_p4o global = ',fmin,fmax,fsum
     call flddiag(fld_p5o, fmin, fmax, fsum, mpiio)
     write(w_unit,*) subname,' fld_p5o local  = ',minval(fld_p5o),maxval(fld_p5o),sum(fld_p5o)
     write(w_unit,*) subname,' fld_p5o global = ',fmin,fmax,fsum
     call flddiag(fld_d4o, fmin, fmax, fsum, mpiio)
     write(w_unit,*) subname,' fld_d4o local  = ',minval(fld_d4o),maxval(fld_d4o),sum(fld_d4o)
     write(w_unit,*) subname,' fld_d4o global = ',fmin,fmax,fsum
     call flddiag(fld_d5o, fmin, fmax, fsum, mpiio)
     write(w_unit,*) subname,' fld_d5o local  = ',minval(fld_d5o),maxval(fld_d5o),sum(fld_d5o)
     write(w_unit,*) subname,' fld_d5o global = ',fmin,fmax,fsum
     call flddiag(fld_i3o, fmin, fmax, fsum, mpiio)
     write(w_unit,*) subname,' fld_i3o local  = ',minval(fld_i3o),maxval(fld_i3o),sum(fld_i3o)
     write(w_unit,*) subname,' fld_i3o global = ',fmin,fmax,fsum
     call flddiag(fld_i4o, fmin, fmax, fsum, mpiio)
     write(w_unit,*) subname,' fld_i4o local  = ',minval(fld_i4o),maxval(fld_i4o),sum(fld_i4o)
     write(w_unit,*) subname,' fld_i4o global = ',fmin,fmax,fsum


  END SUBROUTINE io_run

  !---------------------------------------------------------------------------

END PROGRAM ocnice




