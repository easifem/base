! DISPMODULE, A FORTRAN 95 MODULE FOR PRETTY-PRINTING MATRICES.
! Version number 1.02 6-Sept-2008
!
! NOTE: THE MAIN MODULE, DISPMODULE, IS LATER IN THIS FILE.
!
! The usage documentation for DISPMODULE is in a separate document, that exists
! in several formats:
!
!   dispmodule_userman.doc   Word 2003 doc file
!   dispmodule_userman.pdf   PDF file
!   dispmodule_userman.html  HTML file
!   dispmodule_userman.txt   Text file
!
! Copyright (c) 2008, Kristj�n J�nasson, Dept. of Computer Science, University of
! Iceland (jonasson@hi.is). This software is free. For details see the file README.
!
! NOTE 1: STYLES
!   Styles can be LEFT, ABOVE, PAD, UNDERLINE or NUMBER. Padding is by default done with hyphen
!   characters (e.g. ---title---), but can be changed for example to asterisks with style='*PAD'.
!   Underlining is also with hypens and can also be changed, e.g. with style='*UNDERLINE'. Lower
!   or mixed case is acceptable: style='above' or style='Above'. It is also possible to specify
!   both NUMBER and one of the other styles, with e.g. style='ABOVE & NUMBER'.
!
! NOTE 2: DOUBLE PRECISION
!   The double precision functions and subroutines above (the sections marked DOUBLE PRECISION
!   PROECDURES and DOUBLE PRECISION COMPLEX PROECEDURES) are copies of the sections marked SINGLE
!   PRECISION PROCEDURES and SINGLE PRECISION COMPLEX PROCEDURES, with the kind parameter sngl
!   changed to dble, the procedure name suffixes _sngl and _cplx changed to _dble and _cpld, and
!   single changed to double (only appears in comments). The add-on module DISP_R16MOD is another
!   copy of these procedures (for quad precision).

MODULE DISPMODULE
USE GlobalData, ONLY: Real64, Real32, I4B, LGT
USE DISPMODULE_UTIL
USE DISP_I8MOD
USE DISP_I4MOD
USE DISP_I2MOD
USE DISP_I1MOD
USE DISP_R16MOD
USE DISP_R8MOD
USE DISP_R4MOD
USE DISP_L1MOD
USE DISP_CHARMOD
IMPLICIT NONE
PRIVATE
PUBLIC DISP
  !! Main routine of package, "pretty-prints" vectors and matrices
PUBLIC DISP_SET
  !! Subroutine to change default settings for DISP
PUBLIC DISP_GET
  !! Obtain current default settings
PUBLIC DISP_SET_FACTORY
  !! Call (without parameters) to restore original default settings
PUBLIC TOSTRING
  !! Convert numbers to strings
PUBLIC TOSTRING_SET
  !! Change settings for tostring
PUBLIC TOSTRING_SET_FACTORY
  !! Restore original default settings for tostring
PUBLIC DISP_SETTINGS
  !! Derived type with settings
PUBLIC ASTERISK_UNIT
  !! Constant to specify displaying on asterisk unit (normally the screen)
PUBLIC PUTSTR_UNIT
  !! Constant to specify the use of subroutines putstr and putnl to display
PUBLIC NULL_UNIT
  !! Constant to specify discarding of all displayed output

INTERFACE DISP_SET
  MODULE PROCEDURE disp_set, disp_set_ds
end interface

INTEGER, PARAMETER :: ASTERISK_UNIT = -3
  !! ASTERIC FILE
INTEGER, PARAMETER :: PUTSTR_UNIT   = -2
  !!
INTEGER, PARAMETER :: NULL_UNIT     = -1
  !!
INTEGER, PARAMETER :: dint = I4B !kind(0)
  !! default integer
INTEGER, PARAMETER :: sngl = Real32 !kind(0.0)
  !! single precision (default real)
INTEGER, PARAMETER :: dble = Real64 !kind(0d0)
  !! double precision
INTEGER, PARAMETER :: dlog = LGT !kind(.false.)
  !! default logical

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine disp_set(advance, digmax, matsep, orient, sep, style, unit, zeroas)
  ! Change display settings according to individual parameters
  character(*), optional, intent(in) :: advance, sep, matsep, orient, style, zeroas
  integer, optional, intent(in) :: digmax, unit
  if (present(advance))    DEFSET%advance = upper(advance)
  if (present(sep))        DEFSET%sep = sep
  if (present(sep))        DEFSET%seplen = min(9, len(sep))
  if (present(zeroas))     DEFSET%zeroas = zeroas
  if (present(zeroas))     DEFSET%zaslen = min(9, len(zeroas))
  if (present(matsep))     DEFSET%matsep = matsep
  if (present(matsep))     DEFSET%matseplen = min(9, len(matsep))
  if (present(orient))     DEFSET%orient = upper(orient)
  if (present(style))      DEFSET%style = style
  if (present(digmax))     DEFSET%digmax = digmax
  if (present(unit))       DEFSET%unit = unit
  call check_settings
end subroutine disp_set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine disp_set_factory()
  ! Change display settings to the original default
  DEFSET = FACTORY_SETTINGS
end subroutine disp_set_factory

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine avoid_compiler_warnings
  ! Routine that exists only to avoid compiler warnings (due to compiler bugs)
  type(boxlist), pointer :: boxl_dummy1 => null(), boxl_dummy2 => null()
  type(boxnode), pointer :: boxn_dummy1 => null(), boxn_dummy2 => null()
  type(tostring_settings), pointer :: ts1 => null(), ts2 => null()
  ts1 => ts2
  ts2 => ts1
  boxl_dummy2 => boxl_dummy1
  boxl_dummy1 => boxl_dummy2
  boxn_dummy2 => boxn_dummy1
  boxn_dummy1 => boxn_dummy2
end subroutine avoid_compiler_warnings

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine tostring_set(sep, rfmt, ifmt, trimb, trimz)
  character(*), optional, intent(in) :: sep, rfmt, ifmt, trimb, trimz
  if (present(sep))    tosset%sep    = upper(sep)
  if (present(sep))    tosset%seplen = min(9, len(sep))
  if (present(rfmt))   tosset%rfmt   = upper(rfmt)
  if (present(ifmt))   tosset%ifmt   = upper(ifmt)
  if (present(trimb))  tosset%trimb  = upper(trimb)
  if (present(trimz))  tosset%trimz  = upper(trimz)
  call tostring_check_settings
end subroutine tostring_set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine tostring_set_factory()
  logical dummy
  dummy = .false.
  if (dummy) call avoid_compiler_warnings
  tosset = tosfac
end subroutine tostring_set_factory

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine disp_set_ds(settings)
  ! Change display settings according to the structure "settings"
  type(disp_settings), intent(in) :: settings
  DEFSET = settings
  call check_settings
end subroutine disp_set_ds

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function disp_get() result(defs)
  ! Return current display settings
  type(disp_settings) :: defs
  defs = DEFSET
end function disp_get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DISPMODULE
