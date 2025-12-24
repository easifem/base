MODULE DISP_L1MOD

! Add-on module to DISPMODULE to display 1-byte logical items
! (assuming that these have kind = 1)
!
! This module is obtained by copying the section DEFAULT LOGICAL PROCEDURES from
! dispmodule.F90, replacing dlog with log1 and 'default logical' with '1-byte
! logical' (only appears in comments), and adding the DECLARATIONS section below.
!
! Copyright (c) 2008, Kristj�n J�nasson, Dept. of Computer Science, University of
! Iceland (jonasson@hi.is). This software is free. For details see the file README.

USE dispmodule_util
USE GlobalData, ONLY: LGT
PUBLIC DISP
PUBLIC TOSTRING

PRIVATE

INTERFACE Display
    module procedure disp_s_log1, disp_ts_log1, disp_v_log1, disp_tv_log1, disp_m_log1, disp_tm_log1
END INTERFACE

INTERFACE disp
    module procedure disp_s_log1, disp_ts_log1, disp_v_log1, disp_tv_log1, disp_m_log1, disp_tm_log1
END INTERFACE

INTERFACE tostring
    module procedure tostring_log1, tostring_f_log1, tostring_s_log1, tostring_sf_log1
END INTERFACE

INTEGER, PARAMETER :: log1 = LGT ! hopefully logical(1) is byte

CONTAINS

! ********************************************** 1-BYTE LOGICAL PROCEDURES *************************************************
SUBROUTINE disp_s_log1(x, fmt, advance, sep, trim, unit)
  ! 1-byte logical scalar without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, trim
  LOGICAL(log1), INTENT(in) :: x
  INTEGER, INTENT(in), OPTIONAL :: unit
  CALL disp_ts_log1('', x, fmt, advance, sep, 'left', trim, unit)
END SUBROUTINE disp_s_log1

  subroutine disp_v_log1(x, fmt, advance, lbound, sep, style, trim, unit, orient)
  ! 1-byte logical vector without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim, orient
  LOGICAL(log1), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
CALL disp_tv_log1('', x, fmt, advance, lbound, sep, style, trim, unit, orient)
END SUBROUTINE disp_v_log1

SUBROUTINE disp_m_log1(x, fmt, advance, lbound, sep, style, trim, unit)
  ! 1-byte logical matrix without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim
  LOGICAL(log1), INTENT(in) :: x(:, :)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
  CALL disp_tm_log1('', x, fmt, advance, lbound, sep, style, trim, unit)
END SUBROUTINE disp_m_log1

SUBROUTINE disp_ts_log1(title, x, fmt, advance, sep, style, trim, unit)
  ! 1-byte logical scalar with title
  CHARACTER(*), INTENT(in) :: title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim
  LOGICAL(log1), INTENT(in) :: x
  INTEGER, INTENT(in), OPTIONAL :: unit
    call disp_tm_log1(title, reshape((/x/), (/1, 1/)), fmt, advance, sep=sep, style=style, trim=trim, unit=unit)
END SUBROUTINE disp_ts_log1

  subroutine disp_tv_log1(title, x, fmt, advance, lbound, sep, style, trim, unit, orient)
  ! 1-byte logical vector with title
  CHARACTER(*), INTENT(in) :: title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim, orient
  LOGICAL(log1), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
  TYPE(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient)
  IF (SE%row) THEN
    CALL disp_log1(title, RESHAPE(x, (/1, SIZE(x)/)), SE)
  ELSE
    CALL disp_log1(title, RESHAPE(x, (/SIZE(x), 1/)), SE)
  END IF
END SUBROUTINE disp_tv_log1

  subroutine disp_tm_log1(title, x, fmt, advance, lbound, sep, style, trim, unit)
  ! 1-byte logical matrix with title
  CHARACTER(*), INTENT(in) :: title ! The title to use for the matrix
  LOGICAL(log1), INTENT(in) :: x(:, :) ! The matrix to be written
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt ! Format edit descriptor to use for each matrix element (e.g. 'L1')
  INTEGER, INTENT(in), OPTIONAL :: unit ! Unit to display on
  CHARACTER(*), INTENT(in), OPTIONAL :: advance ! 'No' to print next matrix to right of current, otherewise 'Yes'
  CHARACTER(*), INTENT(in), OPTIONAL :: sep ! Separator between matrix columns (e.g. ", ")
  CHARACTER(*), INTENT(in), OPTIONAL :: style ! Style(s): See NOTE 1 below
  CHARACTER(*), INTENT(in), OPTIONAL :: trim ! 'Auto' (the default) to trim if fmt absent, 'no' for no trimming,
  !                                                ! 'yes' for trimming
  INTEGER, INTENT(in), OPTIONAL :: LBOUND(:) ! Lower bounds of x
  TYPE(settings) :: SE
  !
CALL get_SE(SE, title, SHAPE(x), fmt, advance, lbound, sep, style, trim, unit)
  CALL disp_log1(title, x, SE)
END SUBROUTINE disp_tm_log1

SUBROUTINE disp_log1(title, x, SE)
  ! Write 1-byte logical to box or unit
  CHARACTER(*), INTENT(in) :: title
  LOGICAL(log1), INTENT(in) :: x(:, :)
  TYPE(settings), INTENT(INOUT) :: SE
  INTEGER wid(SIZE(x, 2)), nbl(SIZE(x, 2))
  IF (SE%w <= 0 .OR. SE%trm) THEN
    SE%ed = '(L1)'
    IF (SIZE(x) == 0) THEN
      wid = 0
    ELSE
      wid = 1
    END IF
    SE%w = 1
    nbl = SE%w - wid
  ELSE
    wid = SE%w
    nbl = 0
  END IF
  CALL tobox_log1(title, x, SE, wid, nbl)
END SUBROUTINE disp_log1

SUBROUTINE tobox_log1(title, x, SE, wid, nbl)
  CHARACTER(*), INTENT(in) :: title
  LOGICAL(log1), INTENT(in) :: x(:, :)
  TYPE(settings), INTENT(INOUT) :: SE
  INTEGER, INTENT(INOUT) :: wid(:)
  INTEGER, INTENT(INOUT) :: nbl(:)
  CHARACTER(SE%w) :: s(SIZE(x, 1))
  INTEGER :: m, n, lin1, i, j, wleft, widp(SIZE(wid))
  CHARACTER, POINTER :: boxp(:, :)
  m = SIZE(x, 1)
  n = SIZE(x, 2)
  CALL preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
  DO j = 1, n
    IF (m > 0) WRITE (s, SE%ed) (x(i, j), i=1, m)
    CALL copytobox(s, lin1, wid(j), widp(j), nbl(j), boxp, wleft)
    IF (j < n) CALL copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp, wleft)
  END DO
  CALL finishbox(title, SE, boxp)
END SUBROUTINE tobox_log1

! ********** 1-BYTE LOGICAL TOSTRING PROCEDURES *********
FUNCTION tostring_s_log1(x) RESULT(st)
  LOGICAL(log1), INTENT(in) :: x
  CHARACTER(1) :: st
  st = tostring_f_log1((/x/), 'L1')
END FUNCTION tostring_s_log1

FUNCTION tostring_sf_log1(x, fmt) RESULT(st)
  LOGICAL(log1), INTENT(in) :: x
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(len_f_log1((/x/), fmt)) :: st
  st = tostring_f_log1((/x/), fmt)
END FUNCTION tostring_sf_log1

FUNCTION tostring_log1(x) RESULT(st)
  LOGICAL(log1), INTENT(in) :: x(:)
  CHARACTER(1 + (SIZE(x) - 1)*(1 + tosset0%seplen)) :: st
  st = tostring_f_log1(x, 'L1')
END FUNCTION tostring_log1

FUNCTION tostring_f_log1(x, fmt) RESULT(st)
  LOGICAL(log1), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(len_f_log1(x, fmt)) :: st
  CHARACTER(widthmax_log1(fmt)) :: sa(SIZE(x))
  INTEGER :: w, d
  LOGICAL :: gedit
  CHARACTER(nnblk(fmt) + 2) :: fmt1
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w <= 0) THEN; st = errormsg; RETURN; END IF
  WRITE (sa, fmt1) x
  IF (tosset0%trimb == 'YES') sa = ADJUSTL(sa)
  CALL tostring_get(sa, st)
END FUNCTION tostring_f_log1

PURE FUNCTION len_f_log1(x, fmt) RESULT(wtot)
  LOGICAL(log1), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  INTEGER :: wtot, w, d
  LOGICAL :: gedit
  CHARACTER(nnblk(fmt) + 2) :: fmt1
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w <= 0) THEN; wtot = LEN(errormsg); RETURN; END IF
  IF (tosset0%trimb == 'YES') wtot = SIZE(x)
  IF (tosset0%trimb == 'NO') wtot = w * SIZE(x)
  wtot = wtot + (SIZE(x) - 1) * (tosset0%seplen)
END FUNCTION len_f_log1

PURE FUNCTION widthmax_log1(fmt) RESULT(w)
  CHARACTER(*), INTENT(in) :: fmt
  INTEGER w, d
  LOGICAL gedit
  CHARACTER(nnblk(fmt) + 5) :: fmt1
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w <= 0) w = 1
END FUNCTION widthmax_log1

END MODULE DISP_L1MOD
