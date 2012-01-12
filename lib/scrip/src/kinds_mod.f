!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This module defines the F90 kind parameter for common data types.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: kinds_mod.f 818 2006-03-10 17:18:31Z valcke $
!
!     Copyright (c) 1997, 1998 the Regents of the University of 
!       California.
!
!     This software and ancillary information (herein called software) 
!     called SCRIP is made available under the terms described here.  
!     The software has been approved for release with associated 
!     LA-CC Number 98-45.
!
!     Unless otherwise indicated, this software has been authored
!     by an employee or employees of the University of California,
!     operator of the Los Alamos National Laboratory under Contract
!     No. W-7405-ENG-36 with the U.S. Department of Energy.  The U.S.
!     Government has rights to use, reproduce, and distribute this
!     software.  The public may copy and use this software without
!     charge, provided that this Notice and any statement of authorship
!     are reproduced on all copies.  Neither the Government nor the
!     University makes any warranty, express or implied, or assumes
!     any liability or responsibility for the use of this software.
!
!     If software is modified to produce derivative works, such modified
!     software should be clearly marked, so as not to confuse it with 
!     the version available from Los Alamos National Laboratory.
!
!***********************************************************************

      module kinds_mod
      USE mod_kinds_oasis

!-----------------------------------------------------------------------

      implicit none
      save

!-----------------------------------------------------------------------

      integer(kind=ip_intwp_p), parameter :: char_len  = 80
      integer(kind=ip_intwp_p), parameter :: int_kind  = ip_intwp_p
      integer(kind=ip_intwp_p), parameter :: log_kind  = kind(.true.)
      integer(kind=ip_intwp_p), parameter :: real_kind = ip_realwp_p
      integer(kind=ip_intwp_p), parameter :: dbl_kind  = ip_realwp_p
!-----------------------------------------------------------------------

      end module kinds_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
