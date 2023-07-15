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
USE GlobalData, ONLY: REAL64, REAL32, I4B, LGT
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
END INTERFACE

INTEGER, PARAMETER :: ASTERISK_UNIT = -3
  !! ASTERIC FILE
INTEGER, PARAMETER :: PUTSTR_UNIT = -2
  !!
INTEGER, PARAMETER :: NULL_UNIT = -1
  !!
INTEGER, PARAMETER :: dint = I4B !kind(0)
  !! default integer
INTEGER, PARAMETER :: sngl = REAL32 !kind(0.0)
  !! single precision (default real)
INTEGER, PARAMETER :: dble = REAL64 !kind(0d0)
  !! double precision
INTEGER, PARAMETER :: dlog = LGT !kind(.false.)
  !! default logical

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE disp_set(advance, digmax, matsep, orient, sep, style, unit, zeroas)
  ! Change display settings according to individual parameters
  character(*), optional, intent(in) :: advance, sep, matsep, orient, style, zeroas
  INTEGER, OPTIONAL, INTENT(in) :: digmax, unit
  IF (PRESENT(advance)) DEFSET%advance = upper(advance)
  IF (PRESENT(sep)) DEFSET%sep = sep
  IF (PRESENT(sep)) DEFSET%seplen = MIN(9, LEN(sep))
  IF (PRESENT(zeroas)) DEFSET%zeroas = zeroas
  IF (PRESENT(zeroas)) DEFSET%zaslen = MIN(9, LEN(zeroas))
  IF (PRESENT(matsep)) DEFSET%matsep = matsep
  IF (PRESENT(matsep)) DEFSET%matseplen = MIN(9, LEN(matsep))
  IF (PRESENT(orient)) DEFSET%orient = upper(orient)
  IF (PRESENT(style)) DEFSET%style = style
  IF (PRESENT(digmax)) DEFSET%digmax = digmax
  IF (PRESENT(unit)) DEFSET%unit = unit
  CALL check_settings
END SUBROUTINE disp_set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE disp_set_factory()
  ! Change display settings to the original default
  DEFSET = FACTORY_SETTINGS
END SUBROUTINE disp_set_factory

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE avoid_compiler_warnings
  ! Routine that exists only to avoid compiler warnings (due to compiler bugs)
  TYPE(boxlist), POINTER :: boxl_dummy1 => NULL(), boxl_dummy2 => NULL()
  TYPE(boxnode), POINTER :: boxn_dummy1 => NULL(), boxn_dummy2 => NULL()
  TYPE(tostring_settings), POINTER :: ts1 => NULL(), ts2 => NULL()
  ts1 => ts2
  ts2 => ts1
  boxl_dummy2 => boxl_dummy1
  boxl_dummy1 => boxl_dummy2
  boxn_dummy2 => boxn_dummy1
  boxn_dummy1 => boxn_dummy2
END SUBROUTINE avoid_compiler_warnings

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE tostring_set(sep, rfmt, ifmt, trimb, trimz)
  CHARACTER(*), OPTIONAL, INTENT(in) :: sep, rfmt, ifmt, trimb, trimz
  IF (PRESENT(sep)) tosset%sep = upper(sep)
  IF (PRESENT(sep)) tosset%seplen = MIN(9, LEN(sep))
  IF (PRESENT(rfmt)) tosset%rfmt = upper(rfmt)
  IF (PRESENT(ifmt)) tosset%ifmt = upper(ifmt)
  IF (PRESENT(trimb)) tosset%trimb = upper(trimb)
  IF (PRESENT(trimz)) tosset%trimz = upper(trimz)
  CALL tostring_check_settings
END SUBROUTINE tostring_set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE tostring_set_factory()
  LOGICAL dummy
  dummy = .FALSE.
  IF (dummy) CALL avoid_compiler_warnings
  tosset = tosfac
END SUBROUTINE tostring_set_factory

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE disp_set_ds(settings)
  ! Change display settings according to the structure "settings"
  TYPE(disp_settings), INTENT(in) :: settings
  DEFSET = settings
  CALL check_settings
END SUBROUTINE disp_set_ds

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION disp_get() RESULT(defs)
  ! Return current display settings
  TYPE(disp_settings) :: defs
  defs = DEFSET
END FUNCTION disp_get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DISPMODULE
