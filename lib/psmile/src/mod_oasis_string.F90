
!> Character string manipulation methods

!> These methods work generally on character strings, but also, more particularly
!> on lists.  A list is a character string that contains substrings separated by
!> a delimeter.  That delimeter can be set by the user but the default is ":".
!> Colon delimeted lists are used in OASIS and MCT mainly to instantiate a list
!> of fields, such as "temperature:humidity:zonal_velocity:meridiona_velocity".

#define NEW_LGI_METHOD2a
!!#define NEW_LGI_METHOD2b
!===============================================================================
!BOP ===========================================================================
!
! !MODULE: mod_oasis_string -- string and list methods
!
! !DESCRIPTION:
!    General string and specific list method.  A list is a single string
!    that is delimited by a character forming multiple fields, ie,
!    character(len=*) :: mylist = "t:s:u1:v1:u2:v2:taux:tauy"
!    The delimiter is called listDel in this module, is default ":",
!    but can be set by a call to oasis_string_listSetDel.
!
!
! !INTERFACE: ------------------------------------------------------------------

module mod_oasis_string

! !USES:

   use mod_oasis_kinds
   use mod_oasis_parameters
   use mod_oasis_data
   use mod_oasis_sys
   use mod_oasis_timer

   implicit none
   private

! !PUBLIC TYPES:

   ! no public types

! !PUBLIC MEMBER FUNCTIONS:

   public :: oasis_string_countChar       ! Count number of char in string, fn
   public :: oasis_string_toUpper         ! Convert string to upper-case
   public :: oasis_string_toLower         ! Convert string to lower-case
   public :: oasis_string_getParentDir    ! For a pathname get the parent directory name
   public :: oasis_string_lastIndex       ! Index of last substr in str
   public :: oasis_string_endIndex        ! Index of end of substr in str
   public :: oasis_string_leftAlign       ! remove leading white space
   public :: oasis_string_alphanum        ! remove all non alpha-numeric characters
   public :: oasis_string_betweenTags     ! get the substring between the two tags
   public :: oasis_string_parseCFtunit    ! parse CF time units
   public :: oasis_string_clean           ! Set string to all white space

   public :: oasis_string_listIsValid     ! test for a valid "list"
   public :: oasis_string_listGetNum      ! Get number of fields in list, fn
   public :: oasis_string_listGetIndex    ! Get index of field
   public :: oasis_string_listGetIndexF   ! function version of listGetIndex
   public :: oasis_string_listGetName     ! get k-th field name
   public :: oasis_string_listIntersect   ! get intersection of two field lists
   public :: oasis_string_listUnion       ! get union of two field lists
   public :: oasis_string_listMerge       ! merge two lists to form third
   public :: oasis_string_listAppend      ! append list at end of another
   public :: oasis_string_listPrepend     ! prepend list in front of another
   public :: oasis_string_listSetDel      ! Set field delimeter in lists
   public :: oasis_string_listGetDel      ! Get field delimeter in lists

   public :: oasis_string_setAbort        ! set local abort flag
   public :: oasis_string_setDebug        ! set local debug flag

! !PUBLIC DATA MEMBERS:

   ! no public data members

!EOP

   character(len=1)    ,save :: listDel  = ":"    ! note single exec implications
   character(len=2)    ,save :: listDel2 = "::"   ! note single exec implications
   logical             ,save :: doabort  = .true.
   integer(ip_i4_p),save :: debug    = 0

!===============================================================================
contains
!===============================================================================

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_countChar -- Count number of occurances of a character
!
! !DESCRIPTION:
!>  Count number of occurances of a single character in a string
!     \newline
!     n = shr\_string\_countChar(string,character)
!
!
! !INTERFACE: ------------------------------------------------------------------

integer function oasis_string_countChar(str,char,rc)


   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)    ,intent(in)           :: str   !< string to search
   character(1)    ,intent(in)           :: char  !< char to search for
   integer(ip_i4_p),intent(out),optional :: rc    !< return code

!EOP

   !----- local -----
   integer(ip_i4_p) :: count    ! counts occurances of char
   integer(ip_i4_p) :: n        ! generic index

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_countChar) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   count = 0
   do n = 1, len_trim(str)
      if (str(n:n) == char) count = count + 1
   end do
   oasis_string_countChar = count

   if (present(rc)) rc = 0

   call oasis_debug_exit(subname)

end function oasis_string_countChar

!===============================================================================
!BOP ===========================================================================
! !IROUTINE: oasis_string_toUpper -- Convert string to upper case
!
! !DESCRIPTION:
!>    Convert the input string to upper-case.
!     Use achar and iachar intrinsics to ensure use of ascii collating sequence.
!
!
! !INTERFACE: ------------------------------------------------------------------

function oasis_string_toUpper(str)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(len=*), intent(in) :: str      !< input string to convert to upper case
   character(len=len(str))      :: oasis_string_toUpper  !< output converted string

   !----- local -----
   integer(ip_i4_p) :: i             ! Index
   integer(ip_i4_p) :: aseq          ! ascii collating sequence
   integer(ip_i4_p) :: LowerToUpper  ! integer to convert case
   character(len=1) :: ctmp          ! Character temporary

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_toUpper) "

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   LowerToUpper = iachar("A") - iachar("a")

   do i = 1, len(str)
      ctmp = str(i:i)
      aseq = iachar(ctmp)
      if ( aseq >= iachar("a") .and. aseq <= iachar("z") ) &
           ctmp = achar(aseq + LowertoUpper)
      oasis_string_toUpper(i:i) = ctmp
   end do

   call oasis_debug_exit(subname)

end function oasis_string_toUpper

!===============================================================================
!BOP ===========================================================================
! !IROUTINE: oasis_string_toLower -- Convert string to lower case
!
! !DESCRIPTION:
!>    Convert the input string to lower-case.
!     Use achar and iachar intrinsics to ensure use of ascii collating sequence.
!
!
! !INTERFACE: ------------------------------------------------------------------
function oasis_string_toLower(str)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(len=*), intent(in) :: str      !< input string to convert to lower case
   character(len=len(str))      :: oasis_string_toLower  !< output converted string

   !----- local -----
   integer(ip_i4_p) :: i            ! Index
   integer(ip_i4_p) :: aseq         ! ascii collating sequence
   integer(ip_i4_p) :: UpperToLower ! integer to convert case
   character(len=1) :: ctmp         ! Character temporary

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_toLower) "

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   UpperToLower = iachar("a") - iachar("A")

   do i = 1, len(str)
      ctmp = str(i:i)
      aseq = iachar(ctmp)
      if ( aseq >= iachar("A") .and. aseq <= iachar("Z") ) &
           ctmp = achar(aseq + UpperToLower)
      oasis_string_toLower(i:i) = ctmp
   end do

   call oasis_debug_exit(subname)

end function oasis_string_toLower

!===============================================================================
!BOP ===========================================================================
! !IROUTINE: oasis_string_getParentDir -- For pathname get the parent directory name
!
! !DESCRIPTION:
!>   Get the parent directory pathname.
!
!
! !INTERFACE: ------------------------------------------------------------------

function oasis_string_getParentDir(str)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(len=*), intent(in) :: str      !< input string
   character(len=len(str))      :: oasis_string_getParentDir !< return directory path

   !----- local -----
   integer(ip_i4_p) :: i       ! Index
   integer(ip_i4_p) :: nlen    ! Length of string

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_getParentDir) "

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   nlen = len_trim(str)
   if ( str(nlen:nlen) == "/" ) nlen = nlen - 1
   i = index( str(1:nlen), "/", back=.true. )
   if ( i == 0 )then
      oasis_string_getParentDir = str
   else
      oasis_string_getParentDir = str(1:i-1)
   end if
   
   call oasis_debug_exit(subname)

end function oasis_string_getParentDir

!===============================================================================
!BOP ===========================================================================
!
!
! !IROUTINE: oasis_string_lastIndex -- Get index of last substr within string
!
! !DESCRIPTION:
!> Get the index of the last occurance of a substring within a string
!     \newline
!     n = shr\_string\_lastIndex(string,substring)
!
!
! !INTERFACE: ------------------------------------------------------------------

integer function oasis_string_lastIndex(string,substr,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)    ,intent(in)           :: string !< input string to search
   character(*)    ,intent(in)           :: substr !< sub-string to search for
   integer(ip_i4_p),intent(out),optional :: rc     !< return code

!EOP

   !--- local ---

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_lastIndex) "

!-------------------------------------------------------------------------------
! Note: 
! - "new" F90 back option to index function makes this home-grown solution obsolete
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   oasis_string_lastIndex = index(string,substr,.true.)

   if (present(rc)) rc = 0

   call oasis_debug_exit(subname)

end function oasis_string_lastIndex

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_endIndex -- Get the ending index of substr within string
!
! !DESCRIPTION:
!>  Get the ending index of the first occurance of a substring within string
!     \newline
!     n = shr\_string\_endIndex(string,substring)
!
!
! !INTERFACE: ------------------------------------------------------------------

integer function oasis_string_endIndex(string,substr,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)    ,intent(in)           :: string !< string to search
   character(*)    ,intent(in)           :: substr !< sub-string to search for
   integer(ip_i4_p),intent(out),optional :: rc     !< return code

!EOP

   !--- local ---
   integer(ip_i4_p)   :: i       ! generic index

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_endIndex) "

!-------------------------------------------------------------------------------
! Notes:
! * returns zero if substring not found, uses len_trim() intrinsic
! * very similar to: i = index(str,substr,back=.true.) 
! * do we need this function?
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   i = index(trim(string),trim(substr))
   if ( i == 0 ) then
      oasis_string_endIndex = 0  ! substr is not in string
   else
      oasis_string_endIndex = i + len_trim(substr) - 1
   end if

!  -------------------------------------------------------------------
!  i = index(trim(string),trim(substr),back=.true.)
!  if (i == len(string)+1) i = 0
!  oasis_string_endIndex = i
!  -------------------------------------------------------------------

   if (present(rc)) rc = 0

   call oasis_debug_exit(subname)

end function oasis_string_endIndex

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_leftAlign -- remove leading white space
!
! !DESCRIPTION:
!>    Remove leading white space
!     \newline
!     call shr\_string\_leftAlign(string)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_leftAlign(str,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)    ,intent(inout)          :: str  !< input and returned string
   integer(ip_i4_p),intent(out)  ,optional :: rc   !< return code

!EOP

   !----- local ----
   integer(ip_i4_p) :: rCode ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_leftAlign) "

!-------------------------------------------------------------------------------
! note: 
! * ?? this routine isn't needed, use the intrisic adjustL instead ??
!-------------------------------------------------------------------------------

!  -------------------------------------------------------------------
!  --- used this until I discovered the intrinsic function below
!  do while (len_trim(str) > 0 ) 
!     if (str(1:1) /= ' ') exit
!     str = str(2:len_trim(str))
!  end do
!  rCode = 0
!  !! (len_trim(str) == 0 ) rCode = 1  ! ?? appropriate ??
!  -------------------------------------------------------------------

   call oasis_debug_enter(subname)

   str = adjustL(str)
   if (present(rc)) rc = 0

   call oasis_debug_exit(subname)

end subroutine oasis_string_leftAlign

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_alphanum -- remove non alpha numeric characters
!
! !DESCRIPTION:
!>   Remove all non alpha numeric characters from string
!     \newline
!     call shr\_string\_alphanum(string)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_alphanum(str,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)    ,intent(inout)          :: str  !< input and output string
   integer(ip_i4_p),intent(out)  ,optional :: rc   !< return code

!EOP

   !----- local ----
   integer(ip_i4_p) :: rCode  ! return code
   integer(ip_i4_p) :: n,icnt ! counters

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_alphaNum) "

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   icnt = 0
   do n=1,len_trim(str)
     if ((str(n:n) >= 'a' .and. str(n:n) <= 'z') .or.  &
         (str(n:n) >= 'A' .and. str(n:n) <= 'Z') .or.  &
         (str(n:n) >= '0' .and. str(n:n) <= '9')) then
       icnt = icnt + 1
       str(icnt:icnt) = str(n:n)
     endif
   enddo
   do n=icnt+1,len(str)
     str(n:n) = ' '
   enddo

   if (present(rc)) rc = 0

   call oasis_debug_exit(subname)

end subroutine oasis_string_alphanum

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_betweenTags -- Get the substring between the two tags.
!
! !DESCRIPTION:
!>   Get the substring found between the start and end strings.
!    \newline
!    call shr\_string\_betweenTags(string,startTag,endTag,substring,rc)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_betweenTags(string,startTag,endTag,substr,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)        ,intent(in)  :: string      !< input string to search
   character(*)        ,intent(in)  :: startTag    !< start string
   character(*)        ,intent(in)  :: endTag      !< end string
   character(*)        ,intent(out) :: substr      !< output sub-string between tags
   integer(ip_i4_p),intent(out),optional :: rc !< return code

!EOP

   !--- local ---
   integer(ip_i4_p)   :: iStart  ! substring start index
   integer(ip_i4_p)   :: iEnd    ! substring end   index
   integer(ip_i4_p)   :: rCode   ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_betweenTags) "

!-------------------------------------------------------------------------------
! Notes:
! * assumes the leading/trailing white space is not part of start & end tags
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   iStart = oasis_string_endIndex(string,trim(adjustL(startTag))) ! end of start tag
   iEnd   =               index(string,trim(adjustL(endTag  ))) ! start of end tag

   rCode = 0
   substr = ""

   if (iStart < 1) then
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) subname,estr,"can't find start tag in string"
       WRITE(nulprt,*) subname,estr,"start tag = ",TRIM(startTag)
       WRITE(nulprt,*) subname,estr,"string    = ",TRIM(string)
       CALL oasis_flush(nulprt)
       rCode = 1
   else if (iEnd < 1) then
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) subname,estr,"can't find end tag in string"
       WRITE(nulprt,*) subname,estr,"end   tag = ",TRIM(  endTag)
       WRITE(nulprt,*) subname,estr,"string    = ",TRIM(string)
       CALL oasis_flush(nulprt)
       rCode = 2
   else if ( iEnd <= iStart) then
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) subname,estr,"start tag not before end tag"
       WRITE(nulprt,*) subname,estr,"start tag = ",TRIM(startTag)
       WRITE(nulprt,*) subname,estr,"end   tag = ",TRIM(  endTag)
       WRITE(nulprt,*) subname,estr,"string    = ",TRIM(string)
       CALL oasis_flush(nulprt)
       rCode = 3
   else if ( iStart+1 == iEnd ) then
      substr = ""
      WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
      WRITE(nulprt,*) subname,wstr,"zero-length substring found in ",TRIM(string)
      CALL oasis_flush(nulprt)
   else
      substr = string(iStart+1:iEnd-1)
      IF (LEN_TRIM(substr) == 0) THEN
          WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
          WRITE(nulprt,*) subname,wstr,"white-space substring found in ",TRIM(string)
          CALL oasis_flush(nulprt)
     ENDIF
   end if

   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end subroutine oasis_string_betweenTags

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_parseCFtunit -- Parse CF time unit
!
! !DESCRIPTION:
!>  Parse CF time unit into a delta string name and a base time in yyyymmdd
!  and seconds (nearest integer actually).
!     \newline
!     call shr\_string\_parseCFtunit(string,substring)
!     \newline
!  Input string is like "days since 0001-06-15 15:20:45.5 -6:00"
!    - recognizes "days", "hours", "minutes", "seconds"
!    - must have at least yyyy-mm-dd, hh:mm:ss.s is optional
!    - expects a "since" in the string
!    - ignores time zone part
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_parseCFtunit(string,unit,bdate,bsec,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)    ,intent(in)           :: string !< string to search
   character(*)    ,intent(out)          :: unit   !< delta time unit
   integer(ip_i4_p),intent(out)          :: bdate  !< base date yyyymmdd
   real(ip_r8_p)   ,intent(out)          :: bsec   !< base seconds
   integer(ip_i4_p),intent(out),optional :: rc     !< return code

!EOP

   !--- local ---
   integer(ip_i4_p)   :: i,i1,i2          ! generic index
   character(ic_long) :: tbase            ! baseline time
   character(ic_long) :: lstr             ! local string
   integer(ip_i4_p)   :: yr,mo,da,hr,min  ! time stuff
   real(ip_r8_p)      :: sec              ! time stuff

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_parseCFtunit) "

!-------------------------------------------------------------------------------
! Notes:
! o assume length of CF-1.0 time attribute char string  < ic_long
!   This is a reasonable assumption.
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   unit = 'none'
   bdate = 0
   bsec = 0.0_ip_r8_p

   i = oasis_string_lastIndex(string,'days ')
   if (i > 0) unit = 'days'
   i = oasis_string_lastIndex(string,'hours ')
   if (i > 0) unit = 'hours'
   i = oasis_string_lastIndex(string,'minutes ')
   if (i > 0) unit = 'minutes'
   i = oasis_string_lastIndex(string,'seconds ')
   if (i > 0) unit = 'seconds'

   if (trim(unit) == 'none') then
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) subname,estr,'time unit unknown'
       CALL oasis_flush(nulprt)
       CALL oasis_string_abort(subName//' time unit unknown')
   endif

   i = oasis_string_lastIndex(string,' since ')
   if (i < 1) then
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) subname,estr,'since does not appear in unit attribute for time '
       CALL oasis_flush(nulprt)
       CALL oasis_string_abort(subName//' no since in attr name')
   endif
   tbase = trim(string(i+6:))
   call oasis_string_leftAlign(tbase)

   if (debug > 0 .and. nulprt > 0) then
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) TRIM(subName)//' '//'unit '//TRIM(unit)
       WRITE(nulprt,*) TRIM(subName)//' '//'tbase '//TRIM(tbase)
       CALL oasis_flush(nulprt)
   endif

   yr=0; mo=0; da=0; hr=0; min=0; sec=0
   i1 = 1

   i2 = index(tbase,'-') - 1
   lstr = tbase(i1:i2)
   read(lstr,*,ERR=200,END=200) yr
   tbase = tbase(i2+2:)
   call oasis_string_leftAlign(tbase)

   i2 = index(tbase,'-') - 1
   lstr = tbase(i1:i2)
   read(lstr,*,ERR=200,END=200) mo
   tbase = tbase(i2+2:)
   call oasis_string_leftAlign(tbase)

   i2 = index(tbase,' ') - 1
   lstr = tbase(i1:i2)
   read(lstr,*,ERR=200,END=200) da
   tbase = tbase(i2+2:)
   call oasis_string_leftAlign(tbase)

   i2 = index(tbase,':') - 1
   lstr = tbase(i1:i2)
   read(lstr,*,ERR=200,END=100) hr
   tbase = tbase(i2+2:)
   call oasis_string_leftAlign(tbase)

   i2 = index(tbase,':') - 1
   lstr = tbase(i1:i2)
   read(lstr,*,ERR=200,END=100) min
   tbase = tbase(i2+2:)
   call oasis_string_leftAlign(tbase)

   i2 = index(tbase,' ') - 1
   lstr = tbase(i1:i2)
   read(lstr,*,ERR=200,END=100) sec

100  continue

   IF (debug > 0 ) THEN
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) TRIM(subName),'ymdhms:',yr,mo,da,hr,min,sec
       CALL oasis_flush(nulprt)
   ENDIF

   bdate = abs(yr)*10000 + mo*100 + da
   if (yr < 0) bdate = -bdate
   bsec = real(hr*3600 + min*60,ip_r8_p) + sec

   if (present(rc)) rc = 0

   call oasis_debug_exit(subname)
   return

200  continue
   WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
   write(nulprt,*) subname,estr,'200 on char num read '
   CALL oasis_flush(nulprt)
   call oasis_string_abort(subName//estr//'on char num read')
   call oasis_debug_exit(subname)

end subroutine oasis_string_parseCFtunit

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_clean -- Clean a string, set it to "blank"
!
! !DESCRIPTION:
!>    Clean a string, set it to blank
!     \newline
!     call shr\_string\_clean(string,rc)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_clean(string,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)             ,intent(inout) :: string  !< string
   integer(ip_i4_p),optional,intent(out)   :: rc      !< return code

!EOP

   !----- local -----
   integer(ip_i4_p)   :: n       ! counter
   integer(ip_i4_p)   :: rCode   ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_clean) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   rCode = 0
   string = '       '
   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end subroutine oasis_string_clean

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listIsValid -- determine whether string is a valid list
!
! !DESCRIPTION:
!>    Determine whether string is a valid list
!     \newline
!     logical_var = shr\_string\_listIsValid(list,rc)
!
!
! !INTERFACE: ------------------------------------------------------------------

logical function oasis_string_listIsValid(list,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)             ,intent(in)  :: list    !< list/string
   integer(ip_i4_p),optional,intent(out) :: rc      !< return code

!EOP

   !----- local -----
   integer  (ip_i4_p) :: nChar   ! lenth of list
   integer  (ip_i4_p) :: rCode   ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listIsValid) "

!-------------------------------------------------------------------------------
! check that the list conforms to the list format
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   rCode = 0
   oasis_string_listIsValid = .true.

   nChar = len_trim(list)
   if (nChar < 1) then                           ! list is an empty string
      rCode = 1
   else if (    list(1:1)     == listDel  ) then ! first char is delimiter
      rCode = 2 
   else if (list(nChar:nChar) == listDel  ) then ! last  char is delimiter
      rCode = 3
   else if (index(trim(list)," " )     > 0) then ! white-space in a field name
      rCode = 4
   else if (index(trim(list),listDel2) > 0) then ! found zero length field
      rCode = 5
   end if
   
   if (rCode /= 0) then
      oasis_string_listIsValid = .false.
      WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
      write(nulprt,*) subname,wstr,"invalid list = ",trim(list)
      CALL oasis_flush(nulprt)
   endif

   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end function oasis_string_listIsValid

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listGetName -- Get name of k-th field in list
!
! !DESCRIPTION:
!>    Get name of k-th field in list
!     \newline
!     call shr\_string\_listGetName(list,k,name,rc)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listGetName(list,k,name,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)             ,intent(in)  :: list    !< input list
   integer(ip_i4_p)         ,intent(in)  :: k       !< index of field
   character(*)             ,intent(out) :: name    !< k-th name in list
   integer(ip_i4_p),optional,intent(out) :: rc      !< return code

!EOP

   !----- local -----
   integer(ip_i4_p)   :: i,j,n   ! generic indecies
   integer(ip_i4_p)   :: kFlds   ! number of fields in list
   integer(ip_i4_p)   :: i0,i1   ! name = list(i0:i1)
   integer(ip_i4_p)   :: rCode   ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listGetName) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   rCode = 0

   !--- check that this is a valid list ---
   if (.not. oasis_string_listIsValid(list,rCode) ) then
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
      write(nulprt,*) subname,estr,"invalid list = ",trim(list)
      CALL oasis_flush(nulprt)
      call oasis_string_abort(subName//estr//"invalid list = "//trim(list))
   end if

   !--- check that this is a valid index ---
   kFlds = oasis_string_listGetNum(list)
   if (k<1 .or. kFlds<k) then
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) subname,estr,"invalid index = ",k
       WRITE(nulprt,*) subname,estr,"         list = ",TRIM(list)
       CALL oasis_flush(nulprt)
       CALL oasis_string_abort(subName//estr//"invalid index")
   end if

   !--- start with whole list, then remove fields before and after desired field ---
   i0 = 1
   i1 = len_trim(list)

   !--- remove field names before desired field ---
   do n=2,k
      i = index(list(i0:i1),listDel)
      i0 = i0 + i
   end do

   !--- remove field names after desired field ---
   if ( k < kFlds ) then
      i = index(list(i0:i1),listDel)
      i1 = i0 + i - 2
   end if

   !--- copy result into output variable ---
   name = list(i0:i1)//"   "

   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end subroutine oasis_string_listGetName

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listIntersect -- Get intersection of two field lists
!
! !DESCRIPTION:
!>    Get intersection of two fields lists, write into third list
!     \newline
!     call shr\_string\_listIntersect(list1,list2,listout)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listIntersect(list1,list2,listout,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)             ,intent(in)  :: list1   !< input list 1
   character(*)             ,intent(in)  :: list2   !< input list 2
   character(*)             ,intent(out) :: listout !< output list
   integer(ip_i4_p),optional,intent(out) :: rc      !< return code

!EOP

   !----- local -----
   integer(ip_i4_p)   :: nf,n1,n2 ! counters
   character(ic_med)  :: name     ! field name
   integer(ip_i4_p)   :: rCode    ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listIntersect) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   rCode = 0

   nf = oasis_string_listGetNum(list1)
   call oasis_string_clean(listout)
   do n1 = 1,nf
     call oasis_string_listGetName(list1,n1,name,rCode)
     n2 = oasis_string_listGetIndexF(list2,name)
     if (n2 > 0) then
       call oasis_string_listAppend(listout,name)
     endif
   enddo

   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end subroutine oasis_string_listIntersect

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listUnion -- Get union of two field lists
!
! !DESCRIPTION:
!>    Get union of two fields lists, write into third list
!     \newline
!     call shr\_string\_listUnion(list1,list2,listout)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listUnion(list1,list2,listout,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)             ,intent(in)  :: list1   !< input list 1
   character(*)             ,intent(in)  :: list2   !< input list 2
   character(*)             ,intent(out) :: listout !< output list 3
   integer(ip_i4_p),optional,intent(out) :: rc      !< return code

!EOP

   !----- local -----
   integer(ip_i4_p)  :: nf,n1,n2 ! counters
   character(ic_med) :: name     ! field name
   integer(ip_i4_p)  :: rCode    ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listUnion) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   rCode = 0

   call oasis_string_clean(listout)

   nf = oasis_string_listGetNum(list1)
   do n1 = 1,nf
     call oasis_string_listGetName(list1,n1,name,rCode)
     n2 = oasis_string_listGetIndexF(listout,name)
     if (n2 < 1) then
       call oasis_string_listAppend(listout,name)
     endif
   enddo

   nf = oasis_string_listGetNum(list2)
   do n1 = 1,nf
     call oasis_string_listGetName(list2,n1,name,rCode)
     n2 = oasis_string_listGetIndexF(listout,name)
     if (n2 < 1) then
       call oasis_string_listAppend(listout,name)
     endif
   enddo

   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end subroutine oasis_string_listUnion

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listMerge -- Merge lists two list to third
!
! !DESCRIPTION:
!>    Merge two lists into a third list
!     \newline
!     call shr\_string\_listMerge(list1,list2,listout)
!     call shr\_string\_listMerge(list1,list2,list1)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listMerge(list1,list2,listout,rc)

   implicit none
! !INPUT/OUTPUT PARAMETERS:

   character(*)             ,intent(in)  :: list1   !< input list 1
   character(*)             ,intent(in)  :: list2   !< input list 2
   character(*)             ,intent(out) :: listout !< output list
   integer(ip_i4_p),optional,intent(out) :: rc      !< return code

!EOP

   !----- local -----
   character(ic_xl) :: l1,l2   ! local char strings
   integer(ip_i4_p) :: rCode   ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listMerge) "

!-------------------------------------------------------------------------------
! Notes:
! - no input or output string should be longer than ic_xl
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   rCode = 0

   !--- make sure temp strings are large enough ---
   if ( (len(l1) < len_trim(list1)) .or. (len(l2) < len_trim(list2))) then
      call oasis_string_abort(subName//estr//"temp string not large enough")
   end if

   call oasis_string_clean(l1)
   call oasis_string_clean(l2)
   call oasis_string_clean(listout)
   l1 = trim(list1)
   l2 = trim(list2)
   call oasis_string_leftAlign(l1,rCode)
   call oasis_string_leftAlign(l2,rCode)
   if (len_trim(l1)+len_trim(l2)+1 > len(listout)) &
      call oasis_string_abort(subName//estr//"output list string not large enough")
   if (len_trim(l1) == 0) then
     listout = trim(l2)
   else
     listout = trim(l1)//":"//trim(l2)
   endif

   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end subroutine oasis_string_listMerge

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listAppend -- Append one list to another
!
! !DESCRIPTION:
!>    Append one list to another
!     \newline
!     call shr\_string\_listAppend(list,listadd)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listAppend(list,listadd,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)             ,intent(inout) :: list    !< input and output list
   character(*)             ,intent(in)    :: listadd !< list to append
   integer(ip_i4_p),optional,intent(out)   :: rc      !< return code

!EOP

   !----- local -----
   character(ic_xl) :: l1      ! local string
   integer(ip_i4_p) :: rCode   ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listAppend) "

!-------------------------------------------------------------------------------
! Notes:
! - no input or output string should be longer than ic_xl
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   rCode = 0

   !--- make sure temp string is large enough ---
   if (len(l1) < len_trim(listAdd)) then
      call oasis_string_abort(subName//estr//'temp string not large enough')
   end if

   call oasis_string_clean(l1)
   l1 = trim(listadd)
   call oasis_string_leftAlign(l1,rCode)
   if (len_trim(list)+len_trim(l1)+1 > len(list)) &
      call oasis_string_abort(subName//estr//'output list string not large enough')
   if (len_trim(list) == 0) then
     list = trim(l1)
   else
     list = trim(list)//":"//trim(l1)
   endif

   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end subroutine oasis_string_listAppend

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listPrepend -- Prepend one list to another
!
! !DESCRIPTION:
!>    Prepend one list to another
!     \newline
!     call shr\_string\_listPrepend(listadd,list)
!     \newline
!     results in listadd:list
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listPrepend(listadd,list,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)             ,intent(in)    :: listadd ! input and output list
   character(*)             ,intent(inout) :: list    ! list to prepend
   integer(ip_i4_p),optional,intent(out)   :: rc      ! return code

!EOP

   !----- local -----
   character(ic_xl) :: l1      ! local string
   integer(ip_i4_p) :: rCode   ! return code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listPrepend) "

!-------------------------------------------------------------------------------
! Notes:
! - no input or output string should be longer than ic_xl
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   rCode = 0

   !--- make sure temp string is large enough ---
   if (len(l1) < len_trim(listAdd)) then
      call oasis_string_abort(subName//estr//'temp string not large enough')
   end if

   call oasis_string_clean(l1)
   l1 = trim(listadd)
   call oasis_string_leftAlign(l1,rCode)
   call oasis_string_leftAlign(list,rCode)
   if (len_trim(list)+len_trim(l1)+1 > len(list)) &
      call oasis_string_abort(subName//estr//"output list string not large enough")
   if (len_trim(l1) == 0) then
     list = trim(list)
   else
     list = trim(l1)//":"//trim(list)
   endif

   if (present(rc)) rc = rCode

   call oasis_debug_exit(subname)

end subroutine oasis_string_listPrepend

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listGetIndexF -- Get index of field in string
!
! !DESCRIPTION:
!>    Get the index of a field in a list
!     \newline
!     k = shr\_string\_listGetIndex(str,"taux")
!
!
! !INTERFACE: ------------------------------------------------------------------

integer function oasis_string_listGetIndexF(string,fldStr)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in) :: string   !< input string
   character(*),intent(in) :: fldStr   !< name of field

!EOP

   !----- local -----
   integer(ip_i4_p)    :: k        ! local index variable
   integer(ip_i4_p)    :: rc       ! error code

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listGetIndexF) "

!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call oasis_string_listGetIndex(string,fldStr,k,print=.false.,rc=rc)
   oasis_string_listGetIndexF = k

   call oasis_debug_exit(subname)

end function oasis_string_listGetIndexF

#if (defined NEW_LGI_METHOD2a || defined NEW_LGI_METHOD2b)
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listGetIndex -- Get index of field in string
!
! !DESCRIPTION:
!>    Get the index of a field in a string
!     \newline
!     call shr\_string\_listGetIndex(str,"taux",k,rc)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listGetIndex(string,fldStr,kFld,print,rc)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*)    ,intent(in)           :: string  !< input list
   character(*)    ,intent(in)           :: fldStr  !< name of field
   integer(ip_i4_p),intent(out)          :: kFld    !< index of field in list
   logical         ,intent(in) ,optional :: print   !< print switch
   integer(ip_i4_p),intent(out),optional :: rc      !< return code

!EOP

   !----- local -----
   integer(ip_i4_p)   :: n,n1,n2          ! index for colon position
   integer(ip_i4_p)   :: lens             ! length of string
   logical            :: found            ! T => field found in fieldNames
   logical            :: lprint           ! local print flag

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listGetIndex) "

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)
!   call oasis_timer_start('tcx_slgi0')

!   call oasis_timer_start('tcx_slgia')
   if (present(rc)) rc = 0

   kfld   = 0
   found  = .false.

   lprint = .false.
   if (present(print)) lprint = print

   !--- confirm proper size of input data ---
   if (len_trim(fldStr) < 1) then
       IF (lprint) THEN
           WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
           WRITE(nulprt,*) subname,estr,"input field name has 0 length"
           CALL oasis_flush(nulprt)
       ENDIF
       CALL oasis_string_abort(subName//"invalid field name")
   end if

!   call oasis_timer_stop('tcx_slgia')
!   call oasis_timer_start('tcx_slgib')

   lens = len_trim(string)

!   write(nulprt,*) subname,' tcx1 ',string
!   write(nulprt,*) subname,' tcx2 ',fldStr

   n = index(string,listDel,back=.false.)
!   write(nulprt,*) subname,' tcx3 ',n
!   call oasis_timer_start('tcx_slgib')
!   call oasis_timer_start('tcx_slgic')
   if (n <= 0) then  ! single field only
!      call oasis_timer_start('tcx_slgic1')
      if (trim(fldStr) == string(1:lens)) then
         found = .true.
         kFld = 1
      endif
!      call oasis_timer_stop('tcx_slgic1')
!      write(nulprt,*) subname,' tcx4a ',found,kfld
   elseif (n > 0) then
      !--- check first string ---
!      call oasis_timer_start('tcx_slgic2')
      if (trim(fldStr) == string(1:n-1)) then
         found = .true.
         kFld = 1
      endif          
!      write(nulprt,*) subname,' tcx4b ',found,kfld
!      call oasis_timer_stop('tcx_slgic2')
      !--- check last string ---
      if (.not.found) then
!         call oasis_timer_start('tcx_slgic3')
         n = index(string,listDel,back=.true.)
         if (trim(fldStr) == string(n+1:lens)) then
            found = .true.
            kFld = oasis_string_listGetNum(string)
         endif
!         call oasis_timer_stop('tcx_slgic3')
!         write(nulprt,*) subname,' tcx4c ',found,kfld
      endif
      !--- check other strings ---
      if (.not.found) then
!         call oasis_timer_start('tcx_slgic4')
         n = index(string,':'//trim(fldStr)//':',back=.false.)
!         write(nulprt,*) subname,' tcx5a ',n
         if (n > 0) then
            found = .true.
#if defined NEW_LGI_METHOD2a
            if (n <= lens) then
#endif
#if defined NEW_LGI_METHOD2b
            if (n <= lens/2) then
#endif
!               call oasis_timer_start('tcx_slgic4a')
               n1 = 0
               kFld = 1
               do while (n1 < n) 
                  kFld = kFld + 1
                  n2 = index(string(n1+1:lens),listDel,back=.false.)
                  n1 = n1 + n2
!                  write(nulprt,*) subname,' tcx5b ',kfld,n2,n1,n
               enddo
!               call oasis_timer_stop('tcx_slgic4a')
            else
!               call oasis_timer_start('tcx_slgic4b')
               n1 = lens+1
               kFld = oasis_string_listGetNum(string) + 1
!               call oasis_timer_stop('tcx_slgic4b')
!               call oasis_timer_start('tcx_slgic4c')
               do while (n1 > n) 
                  kFld = kFld - 1
                  n2 = index(string(1:n1-1),listDel,back=.true.)
                  n1 = n2
!                  write(nulprt,*) subname,' tcx5c ',kfld,n2,n1,n
               enddo
!               call oasis_timer_stop('tcx_slgic4c')
            endif
         endif
!         write(nulprt,*) subname,' tcx4d ',found,kfld
!         call oasis_timer_stop('tcx_slgic4')
      endif
   endif

!   call oasis_timer_stop('tcx_slgic')

!   call oasis_timer_start('tcx_slgid')

   !--- not finding a field is not a fatal error ---
   if (.not. found) then
      IF (lprint) THEN
          WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
          WRITE(nulprt,*) subname,"FYI: field ",TRIM(fldStr)," not found in list ",TRIM(string)
          CALL oasis_flush(nulprt)
      ENDIF
      if (present(rc)) rc = 1
   end if

!   call oasis_timer_stop('tcx_slgid')
!   call oasis_timer_stop('tcx_slgi0')
   call oasis_debug_exit(subname)

end subroutine oasis_string_listGetIndex
#endif
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listGetNum -- get number of fields in a string list
!
! !DESCRIPTION:
!> return number of fields in string list
!
!
! !INTERFACE: ------------------------------------------------------------------

integer function oasis_string_listGetNum(str)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in) :: str   !< input list

!EOP

   !----- local -----
   integer(ip_i4_p) :: count    ! counts occurances of char

   !----- formats -----
   character(*),parameter :: subName =   "(oasis_string_listGetNum) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   oasis_string_listGetNum = 0

   if (len_trim(str) > 0) then
      count = oasis_string_countChar(str,listDel)
      oasis_string_listGetNum = count + 1
   endif

   call oasis_debug_exit(subname)

end function oasis_string_listGetNum

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listSetDel -- Set list delimeter character
!
! !DESCRIPTION:
!>    Set field delimeter character in lists
!     \newline
!     call shr\_string\_listSetDel(":")
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listSetDel(cflag)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(len=1),intent(in) :: cflag  !< field delimeter

!EOP

   !--- formats ---
   character(*),parameter :: subName =   "(oasis_string_listSetDel) "

!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   IF (debug > 0) THEN
       WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
       WRITE(nulprt,*) subname,' changing listDel from '//TRIM(listDel)//' to '//TRIM(cflag)
       CALL oasis_flush(nulprt)
   ENDIF
   listDel = trim(cflag)
   listDel2 = listDel//listDel

   call oasis_debug_exit(subname)

end subroutine oasis_string_listSetDel

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_listGetDel -- Get list delimeter character
!
! !DESCRIPTION:
!>    Get field delimeter character in lists
!     \newline
!     call shr\_string\_listGetDel(del)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_listGetDel(del)

  implicit none

! !INPUT/OUTPUT PARAMETERS:

  character(*),intent(out) :: del  !< field delimeter

!EOP

   !--- formats ---
   character(*),parameter :: subName =   "(oasis_string_listGetDel) "

!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   del = trim(listDel)

   call oasis_debug_exit(subname)

end subroutine oasis_string_listGetDel

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_setAbort -- Set local oasis_string abort flag
!
! !DESCRIPTION:
!>    Set local oasis_string abort flag, true = abort, false = print and continue
!     \newline
!     call shr\_string\_setAbort(.false.)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_setAbort(flag)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

  logical,intent(in) :: flag   !< abort flag

!EOP

   !--- formats ---
   character(*),parameter :: subName =   "(oasis_string_setAbort) "

!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   if (debug > 0) then
      if (flag) then
          WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
          WRITE(nulprt,*) subname,' setting abort to true'
          CALL oasis_flush(nulprt)
      else
          WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
          WRITE(nulprt,*) subname,' setting abort to false'
          CALL oasis_flush(nulprt)
      endif
   endif

   doabort = flag

   call oasis_debug_exit(subname)

end subroutine oasis_string_setAbort

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_string_setDebug -- Set local oasis_string debug level
!
! !DESCRIPTION:
!>    Set local oasis_string debug level, 0 = production
!     \newline
!     call shr\_string\_setDebug(2)
!
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_string_setDebug(iFlag)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(ip_i4_p),intent(in) :: iFlag !< requested debug level

!EOP

   !--- local ---

   !--- formats ---
   character(*),parameter :: subName =   "(oasis_string_setDebug) "

!-------------------------------------------------------------------------------
! NTOE: write statement can be expensive if called many times.
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

!   if (OASIS_debug > 0) write(nulprt,*) subname,' changing debug level from ',debug,' to ',iflag
   debug = iFlag

   call oasis_debug_exit(subname)

end subroutine oasis_string_setDebug

!===============================================================================
!===============================================================================

!> Supports aborts in the string module

subroutine oasis_string_abort(string)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),optional,intent(in) :: string  !< error string

!EOP

   !--- local ---
   character(ic_xl) :: lstring
   character(*),parameter :: subName =   "(oasis_string_abort)"

!-------------------------------------------------------------------------------
! NOTE:
! - no input or output string should be longer than ic_xl
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lstring = ''
   if (present(string)) lstring = string

   if (doabort) then
      WRITE(nulprt,*) subname,estr,'abort for ',TRIM(lstring)
      call oasis_abort()
   else
      write(nulprt,*) subname,wstr,'no abort for '//trim(lstring)
      CALL oasis_flush(nulprt)
   endif

   call oasis_debug_exit(subname)

end subroutine oasis_string_abort

!===============================================================================
!===============================================================================

end module mod_oasis_string
