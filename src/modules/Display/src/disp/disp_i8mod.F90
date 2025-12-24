MODULE DISP_I8MOD

! Add-on module to DISPMODULE to display 8-byte integers
! (assuming that these are obtained with selected_int_kind(18))
!
! This module is obtained by copying the section DEFAULT INTEGER PROCEDURES from
! from dispmodule.F90, replacing dint with byt8 and 'default integer' with 8-byte
! integer (only appears in comments), and adding the DECLARATIONS section below.
!
! Copyright (c) 2008, Kristj�n J�nasson, Dept. of Computer Science, University of
! Iceland (jonasson@hi.is). This software is free. For details see the file README.

USE DISPMODULE_UTIL
USE GlobalData, ONLY: INT64

PUBLIC DISP
PUBLIC TOSTRING

PRIVATE

INTERFACE disp
    module procedure disp_s_byt8, disp_ts_byt8, disp_v_byt8, disp_tv_byt8, disp_m_byt8, disp_tm_byt8
END INTERFACE

INTERFACE tostring
    module procedure tostring_byt8, tostring_f_byt8, tostring_s_byt8, tostring_sf_byt8
END INTERFACE

INTEGER, PARAMETER :: byt8 = INT64

CONTAINS

! ******************************** 8-BYTE INTEGER PROCEDURES *******************************
SUBROUTINE disp_s_byt8(x, fmt, advance, sep, trim, unit, zeroas)
  ! 8-byte integer scalar without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, trim, zeroas
  INTEGER(byt8), INTENT(in) :: x
  INTEGER, INTENT(in), OPTIONAL :: unit
  CALL disp_ts_byt8('', x, fmt, advance, sep, 'left', trim, unit, zeroas)
END SUBROUTINE disp_s_byt8

  subroutine disp_v_byt8(x, fmt, advance, lbound, sep, style, trim, unit, orient, zeroas)
  ! 8-byte integer vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
  INTEGER(byt8), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
    call disp_tv_byt8('', x, fmt, advance, lbound, sep, style, trim, unit, orient, zeroas)
END SUBROUTINE disp_v_byt8

  subroutine disp_m_byt8(x, fmt, advance, lbound, sep, style, trim, unit, zeroas)
  ! 8-byte integer matrix without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim, zeroas
  INTEGER(byt8), INTENT(in) :: x(:, :)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
CALL disp_tm_byt8('', x, fmt, advance, lbound, sep, style, trim, unit, zeroas)
END SUBROUTINE disp_m_byt8

  subroutine disp_ts_byt8(title, x, fmt, advance, sep, style, trim, unit, zeroas)
  ! 8-byte integer scalar with title
  CHARACTER(*), INTENT(in) :: title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim, zeroas
  INTEGER(byt8), INTENT(in) :: x
  INTEGER, INTENT(in), OPTIONAL :: unit
    call disp_tm_byt8(title, reshape((/x/), (/1, 1/)), fmt, advance, sep=sep, style=style, trim=trim, unit=unit, &
                    zeroas=zeroas)
END SUBROUTINE disp_ts_byt8

  subroutine disp_tv_byt8(title, x, fmt, advance, lbound, sep, style, trim, unit, orient, zeroas)
  ! 8-byte integer vector with title
  CHARACTER(*), INTENT(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
  INTEGER(byt8), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:)
  TYPE(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient, zeroas)
  IF (SE%row) THEN
    CALL disp_byt8(title, RESHAPE(x, (/1, SIZE(x)/)), SE)
  ELSE
    CALL disp_byt8(title, RESHAPE(x, (/SIZE(x), 1/)), SE)
  END IF
END SUBROUTINE disp_tv_byt8

  subroutine disp_tm_byt8(title, x, fmt, advance, lbound, sep, style, trim, unit, zeroas)
  ! 8-byte integer matrix with title
  CHARACTER(*), INTENT(in) :: title ! The title to use for the matrix
  INTEGER(byt8), INTENT(in) :: x(:, :) ! The matrix to be written
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt ! Format edit descriptor to use for each matrix element (e.g.'I4')
  INTEGER, INTENT(in), OPTIONAL :: unit ! Unit to display on
  CHARACTER(*), INTENT(in), OPTIONAL :: advance ! 'No' to print next matrix to right of current, otherewise 'Yes'
  CHARACTER(*), INTENT(in), OPTIONAL :: sep ! Separator between matrix columns (e.g. ", ")
  CHARACTER(*), INTENT(in), OPTIONAL :: zeroas ! Zeros are replaced by this string
  CHARACTER(*), INTENT(in), OPTIONAL :: style ! Style(s): See NOTE 1 below
  CHARACTER(*), INTENT(in), OPTIONAL :: trim ! 'Auto' (the default) to trim if fmt absent, 'no' for no trimming,
  !                                                ! trimming, 'yes' for trimming
  INTEGER, INTENT(in), OPTIONAL :: LBOUND(:) ! Lower bounds of x
  TYPE(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, zeroas=zeroas)
  CALL disp_byt8(title, x, SE)
END SUBROUTINE disp_tm_byt8

SUBROUTINE disp_byt8(title, x, SE)
  ! 8-byte integer item
  CHARACTER(*), INTENT(in) :: title
  INTEGER(byt8), INTENT(in) :: x(:, :)
  TYPE(settings), INTENT(INOUT) :: SE
  INTEGER wid(SIZE(x, 2)), nbl(SIZE(x, 2))
  CALL find_editdesc_byt8(x, SE, wid, nbl) ! determine also SE%w
  CALL tobox_byt8(title, x, SE, wid, nbl)
END SUBROUTINE disp_byt8

SUBROUTINE tobox_byt8(title, x, SE, wid, nbl)
  ! Write 8-byte integer matrix to box
  CHARACTER(*), INTENT(in) :: title
  INTEGER(byt8), INTENT(in) :: x(:, :)
  TYPE(settings), INTENT(INOUT) :: SE
  INTEGER, INTENT(INOUT) :: wid(:)
  INTEGER, INTENT(INOUT) :: nbl(:)
  CHARACTER(SE%w) :: s(SIZE(x, 1))
  INTEGER :: lin1, j, wleft, m, n, widp(SIZE(wid))
  CHARACTER, POINTER :: boxp(:, :)
  m = SIZE(x, 1)
  n = SIZE(x, 2)
  CALL preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
  DO j = 1, n
    IF (m > 0) WRITE (s, SE%ed) x(:, j)
  IF (SE%lzas > 0) CALL replace_zeronaninf(s, SE%zas(1:SE%lzas), x(:, j) == 0)
    CALL copytobox(s, lin1, wid(j), widp(j), nbl(j), boxp, wleft)
    IF (j < n) CALL copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp, wleft)
  END DO
  CALL finishbox(title, SE, boxp)
END SUBROUTINE tobox_byt8

SUBROUTINE find_editdesc_byt8(x, SE, wid, nbl)
  ! Determine SE%ed, SE%w (unless specified) and wid
  INTEGER(byt8), INTENT(in) :: x(:, :)
  TYPE(settings), INTENT(INOUT) :: SE
  INTEGER, INTENT(out) :: wid(SIZE(x, 2)), nbl(SIZE(x, 2))
  !
  INTEGER(byt8) xmaxv(SIZE(x, 2)), xminv(SIZE(x, 2)), xp, xm
  LOGICAL xzero(SIZE(x, 2)), xallz(SIZE(x, 2))
  CHARACTER(22) s
  INTEGER ww
  !
  IF (SE%w == 0) THEN
    xp = MAXVAL(x)
    xm = MINVAL(x)
    WRITE (s, '(SS,I0)') xp; ww = LEN_TRIM(s)
    WRITE (s, '(SS,I0)') xm; ww = MAX(ww, LEN_TRIM(s))
    SE%w = MAX(SE%lzas, ww)
    CALL replace_w(SE%ed, ww)
  ELSEIF (SE%w < 0) THEN ! obtain max-width of x
    IF (SIZE(x) == 0) THEN
      SE%ed = '()'
      SE%w = 0
      wid = 0
      RETURN
    END IF
    xp = MAXVAL(x)
    xm = MINVAL(x)
    WRITE (s, '(SS,I0)') xp; ww = LEN_TRIM(s)
    WRITE (s, '(SS,I0)') xm; ww = MAX(ww, LEN_TRIM(s))
    ww = MAX(SE%lzas, ww)
    SE%ed = '(SS,Ixx)'
    WRITE (SE%ed(6:7), '(SS,I2)') ww
    SE%w = ww
  END IF
  IF (SE%trm) THEN
    xmaxv = MAXVAL(x, 1) ! max in each column
    xminv = MINVAL(x, 1) ! min
    xzero = ANY(x == 0_BYT8, 1) ! true where column has some zeros
    xallz = ALL(x == 0_BYT8, 1) ! true where column has only zeros
    CALL getwid_byt8(xmaxv, xminv, xzero, xallz, SE, wid, nbl)
  ELSE
    wid = SE%w
    nbl = 0
  END IF
END SUBROUTINE find_editdesc_byt8

SUBROUTINE getwid_byt8(xmaxv, xminv, xzero, xallz, SE, wid, nbl)
  INTEGER(byt8), INTENT(in) :: xmaxv(:), xminv(:)
  LOGICAL, INTENT(in) :: xzero(:), xallz(:) ! True for columns with some/all zeros
  TYPE(settings), INTENT(in) :: SE ! Settings
  INTEGER, INTENT(out) :: wid(:) ! Widths of columns
  INTEGER, INTENT(out) :: nbl(:) ! n of blanks to peel from left (w-wid)
  CHARACTER(SE%w) :: stmax(SIZE(xmaxv)), stmin(SIZE(xmaxv))
  INTEGER w
  w = SE%w
  WRITE (stmax, SE%ed) xmaxv
  WRITE (stmin, SE%ed) xminv
  nbl = MOD(VERIFY(stmin, ' ') + w, w + 1) ! loc. of first nonblank
  nbl = MIN(nbl, MOD(VERIFY(stmax, ' ') + w, w + 1))
  wid = w - nbl
  IF (SE%lzas > 0) THEN
    wid = MERGE(SE%lzas, wid, xallz)
    wid = MAX(wid, MERGE(SE%lzas, 0, xzero))
    nbl = w - wid
  END IF
END SUBROUTINE getwid_byt8

! ********* 8-BYTE INTEGER TOSTRING PROCEDURES *********
FUNCTION tostring_s_byt8(x) RESULT(st)
  ! Scalar to string
  INTEGER(byt8), INTENT(in) :: x
  CHARACTER(len_f_byt8((/x/), tosset0%ifmt)) :: st
  st = tostring_f_byt8((/x/), tosset0%ifmt)
END FUNCTION tostring_s_byt8

FUNCTION tostring_sf_byt8(x, fmt) RESULT(st)
  ! Scalar with specified format to string
  INTEGER(byt8), INTENT(in) :: x
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(len_f_byt8((/x/), fmt)) :: st
  st = tostring_f_byt8((/x/), fmt)
END FUNCTION tostring_sf_byt8

FUNCTION tostring_byt8(x) RESULT(st)
  ! Vector to string
  INTEGER(byt8), INTENT(in) :: x(:)
  CHARACTER(len_f_byt8(x, tosset0%ifmt)) :: st
  st = tostring_f_byt8(x, tosset0%ifmt)
END FUNCTION tostring_byt8

FUNCTION tostring_f_byt8(x, fmt) RESULT(st)
  ! Vector with specified format to string
  INTEGER(byt8), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(len_f_byt8(x, fmt)) :: st
  CHARACTER(widthmax_byt8(x, fmt)) :: sa(SIZE(x))
  INTEGER :: w, d
  LOGICAL :: gedit
  CHARACTER(nnblk(fmt) + 5) :: fmt1
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w < 0) THEN; st = errormsg; RETURN; END IF
  WRITE (sa, fmt1) x
  IF (tosset0%trimb == 'YES' .OR. w == 0) sa = ADJUSTL(sa)
  CALL tostring_get(sa, st)
END FUNCTION tostring_f_byt8

PURE FUNCTION len_f_byt8(x, fmt) RESULT(wtot)
  ! Total width of tostring representation of x
  INTEGER(byt8), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(widthmax_byt8(x, fmt)) :: sa(SIZE(x))
  INTEGER :: wtot, w, d
  LOGICAL :: gedit
  CHARACTER(nnblk(fmt) + 5) :: fmt1
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w < 0) THEN; wtot = LEN(errormsg); RETURN; END IF
  WRITE (sa, fmt1) x
  IF (tosset0%trimb == 'YES' .OR. w == 0) sa = ADJUSTL(sa)
  wtot = SUM(LEN_TRIM(sa)) + (SIZE(x) - 1) * (tosset0%seplen)
END FUNCTION len_f_byt8

PURE FUNCTION widthmax_byt8(x, fmt) RESULT(w)
  ! Maximum width of string representation of an element in x
  INTEGER(byt8), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(RANGE(x) + 2) sx(2)
  INTEGER w, d
  LOGICAL gedit
  CHARACTER(nnblk(fmt) + 5) :: fmt1
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w <= 0) THEN
    WRITE (sx, '(SS,I0)') MAXVAL(x), MINVAL(x)
    w = MAXVAL(LEN_TRIM(sx))
  END IF
END FUNCTION widthmax_byt8
! ************************************* END OF 8-BYTE INTEGER PROCEDURES ******************************************

END MODULE DISP_I8MOD
