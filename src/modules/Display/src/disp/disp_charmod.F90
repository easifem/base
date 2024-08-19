! Add-on module to DISPMODULE to display selected_real_kind(25) reals
! (these are probably 16 bytes and possibly snglruple precision)
!
! This module is obtained by copying the section SINGLE PRECSION PROCEDURES from
! dispmodule.F90, replacing sngl with sngl, single withe snglruple (only appears
! in comments) and cplx with cplx, adding a DECLARATIONS section, and defining
! the constant sngl as selected_real_kind(25).
!
! Copyright (c) 2008, Kristj�n J�nasson, Dept. of Computer Science, University of
! Iceland (jonasson@hi.is). This software is free. For details see the file README.

MODULE DISP_CHARMOD
USE DISPMODULE_UTIL
USE GlobalData, ONLY: REAL32
PRIVATE
PUBLIC DISP

INTERFACE DISP
  MODULE PROCEDURE disp_ts_dchr, disp_v_dchr, disp_tv_dchr, disp_m_dchr, disp_tm_dchr
END INTERFACE DISP

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine disp_v_dchr(x, fmt, advance, lbound, sep, style, trim, unit, orient)
  ! Default character vector without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim, orient
  CHARACTER(*), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
CALL disp_tv_dchr('', x, fmt, advance, lbound, sep, style, trim, unit, orient)
END SUBROUTINE disp_v_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE disp_m_dchr(x, fmt, advance, lbound, sep, style, trim, unit)
  ! Default character matrix without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim
  CHARACTER(*), INTENT(in) :: x(:, :)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
  CALL disp_tm_dchr('', x, fmt, advance, lbound, sep, style, trim, unit)
END SUBROUTINE disp_m_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE disp_ts_dchr(title, x, fmt, advance, sep, style, trim, unit)
  ! Default character scalar with title
CHARACTER(*), INTENT(in), OPTIONAL :: title, x, fmt, advance, sep, style, trim
  CHARACTER(0) empty(1, 0)
  INTEGER, INTENT(in), OPTIONAL :: unit
  empty = ''
  IF (PRESENT(title) .AND. PRESENT(x)) THEN
    call disp_nonopt_dchr(title, x, fmt, advance, sep=sep, style=style, trim=trim, unit=unit)
  ELSEIF (PRESENT(x)) THEN
    call disp_nonopt_dchr('', x, fmt, advance, sep=sep, style='left', trim=trim, unit=unit)
  ELSEIF (PRESENT(title)) THEN
    call disp_nonopt_dchr('', title, fmt, advance, sep=sep, style='left', trim=trim, unit=unit)
  ELSE
    call disp_tm_dchr('', empty, fmt, advance, sep=sep, style=style, trim=trim, unit=unit)
  END IF
END SUBROUTINE disp_ts_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE disp_nonopt_dchr(title, x, fmt, advance, sep, style, trim, unit)
  ! This routine exists to circumvent bug in gfortran, that made it not possible to change scalar strings
  ! to matrices with reshape in calls of disp_tm_dchr. This intermediate routine provides work-around.
  CHARACTER(*), INTENT(in) :: title, x, fmt, advance, sep, style, trim
  OPTIONAL fmt, advance, sep, style, trim
  INTEGER, INTENT(in), OPTIONAL :: unit
  CHARACTER(LEN(x)) :: xm(1, 1)
  xm(1, 1) = x
  call disp_tm_dchr(title, xm, fmt, advance, sep=sep, style=style, trim=trim, unit=unit)
END SUBROUTINE disp_nonopt_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine disp_tv_dchr(title, x, fmt, advance, lbound, sep, style, trim, unit, orient)
  ! Default character vector with title
  CHARACTER(*), INTENT(in) :: title, x(:)
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim, orient
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
  TYPE(settings) :: SE
  call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient)
  IF (SE%row) THEN
    CALL disp_dchr(title, RESHAPE(x, (/1, SIZE(x)/)), SE)
  ELSE
    CALL disp_dchr(title, RESHAPE(x, (/SIZE(x), 1/)), SE)
  END IF
END SUBROUTINE disp_tv_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine disp_tm_dchr(title, x, fmt, advance, lbound, sep, style, trim, unit)
  ! Default character matrix with title
  CHARACTER(*), INTENT(in) :: title ! The title to use for the matrix
  CHARACTER(*), INTENT(in) :: x(:, :) ! The matrix to be written
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt ! Format edit descriptor to use for each matrix element (e.g.'A4')
  INTEGER, INTENT(in), OPTIONAL :: unit ! Unit to display on
  CHARACTER(*), INTENT(in), OPTIONAL :: advance ! 'No' to print next matrix to right of current, otherewise 'Yes'
  CHARACTER(*), INTENT(in), OPTIONAL :: sep ! Separator between matrix columns (e.g. ", ")
  CHARACTER(*), INTENT(in), OPTIONAL :: style ! Style(s): see NOTE 1 below
  CHARACTER(*), INTENT(in), OPTIONAL :: trim ! 'Auto' (the default) to trim if fmt absent, 'no' for no
  !                                                ! trimming, 'yes' for trimming
  INTEGER, INTENT(in), OPTIONAL :: LBOUND(:) ! Lower bounds of x
  !
  TYPE(settings) :: SE
CALL get_SE(SE, title, SHAPE(x), fmt, advance, lbound, sep, style, trim, unit)
  CALL disp_dchr(title, x, SE)
END SUBROUTINE disp_tm_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE disp_dchr(title, x, SE)
  ! Default character item to box
  CHARACTER(*), INTENT(in) :: title, x(:, :)
  TYPE(settings), INTENT(INOUT) :: SE
  CHARACTER(13) :: edesc
  CHARACTER, POINTER :: boxp(:, :)
  INTEGER :: m, n, j, lin1, wleft, lx, w
  INTEGER, DIMENSION(SIZE(x, 2)) :: wid, nbl, n1, n2, widp
  m = SIZE(x, 1)
  n = SIZE(x, 2)
  lx = LEN(x)
  w = SE%w
  IF (w <= 0) THEN
    w = lx
    IF (w < 0) THEN
      edesc = '(A__________)'
      WRITE (edesc(3:12), '(SS,I10)') w
      SE%ed = edesc
    END IF
  END IF
  IF (SE%trm .AND. SIZE(x) > 0) THEN
    n1 = MINVAL(MOD(VERIFY(x, ' ') - w - 1, w + 1), 1) + w + 1
    n2 = MAXVAL(VERIFY(x, ' ', back=.TRUE.), 1)
    wid = n2 - n1 + 1
    nbl = w - wid
  ELSE
    n1 = 1
    n2 = w
    wid = w
    nbl = 0
  END IF
  IF (ALL(wid == 0)) n = 0
  SE%w = w
  CALL preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
  DO j = 1, n
    IF (SE%trm) THEN
      call copytobox(x(:,j)(n1(j):n2(j)), lin1, wid(j), widp(j), nbl(j), boxp,  wleft)
    ELSE
      if (widp(j) > lx) call copyseptobox(repeat(' ', widp(j)-lx), m, lin1, boxp,  wleft)
      CALL copytobox(x(:, j), lin1, lx, lx, 0, boxp, wleft)
    END IF
    IF (j < n) CALL copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp, wleft)
  END DO
  CALL finishbox(title, SE, boxp)
END SUBROUTINE disp_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DISP_CHARMOD
