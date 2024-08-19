MODULE DISP_R16MOD

#ifdef USE_Real128
! Add-on module to DISPMODULE to display selected_real_kind(25) reals
! (these are probably 16 bytes and possibly quadruple precision)
!
! This module is obtained by copying the section SINGLE PRECSION PROCEDURES from
! dispmodule.F90, replacing sngl with quad, single withe quadruple (only appears
! in comments) and cplx with cplq, adding a DECLARATIONS section, and defining
! the constant quad as selected_real_kind(25).
!
! Copyright (c) 2008, Kristj�n J�nasson, Dept. of Computer Science, University of
! Iceland (jonasson@hi.is). This software is free. For details see the file README.

! ******************************** DECLARATIONS ********************************************
USE dispmodule_util
USE GlobalData, ONLY: REAL128
PUBLIC DISP
PUBLIC TOSTRING

PRIVATE

INTERFACE Display
    module procedure disp_s_quad, disp_ts_quad, disp_v_quad, disp_tv_quad, disp_m_quad, disp_tm_quad
    module procedure disp_s_cplq, disp_ts_cplq, disp_v_cplq, disp_tv_cplq, disp_m_cplq, disp_tm_cplq
END INTERFACE

INTERFACE disp
    module procedure disp_s_quad, disp_ts_quad, disp_v_quad, disp_tv_quad, disp_m_quad, disp_tm_quad
    module procedure disp_s_cplq, disp_ts_cplq, disp_v_cplq, disp_tv_cplq, disp_m_cplq, disp_tm_cplq
END INTERFACE

INTERFACE tostring
    module procedure tostring_quad, tostring_f_quad, tostring_s_quad, tostring_sf_quad
    module procedure tostring_cplq, tostring_f_cplq, tostring_s_cplq, tostring_sf_cplq
END INTERFACE

INTEGER, PARAMETER :: quad = REAL128

CONTAINS

! **************************** QUADRUPLE PRECISION PROCEDURES *******************************
SUBROUTINE disp_s_quad(x, fmt, advance, digmax, sep, trim, unit, zeroas)
  ! quadruple precision scalar without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, trim, zeroas
  REAL(quad), INTENT(in) :: x
  INTEGER, INTENT(in), OPTIONAL :: unit, digmax
    call disp_ts_quad('', x, fmt, advance, digmax, sep, 'left', trim, unit, zeroas)
END SUBROUTINE disp_s_quad

  subroutine disp_v_quad(x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
  ! quadruple precision vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
  REAL(quad), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:), digmax
    call disp_tv_quad('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
END SUBROUTINE disp_v_quad

  subroutine disp_m_quad(x, fmt, advance, lbound, sep, style, trim, unit, digmax, zeroas)
  ! quadruple precision matrix without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim, zeroas
  REAL(quad), INTENT(in) :: x(:, :)
  INTEGER, INTENT(in), OPTIONAL :: unit, digmax, LBOUND(:)
    call disp_tm_quad('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
END SUBROUTINE disp_m_quad

  subroutine disp_ts_quad(title, x, fmt, advance, digmax, sep, style, trim, unit, zeroas)
  ! quadruple precision scalar with title
  CHARACTER(*), INTENT(in) :: title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, advance, sep, style, trim, zeroas
  REAL(quad), INTENT(in) :: x
  INTEGER, INTENT(in), OPTIONAL :: unit, digmax
    call disp_tm_quad(title, reshape((/x/), (/1, 1/)), fmt, advance, digmax, sep=sep, style=style, trim=trim, &
                    unit=unit, zeroas=zeroas)
END SUBROUTINE disp_ts_quad

  subroutine disp_tv_quad(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
  ! quadruple precision vector with title
  CHARACTER(*), INTENT(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
  REAL(quad), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:), digmax
  TYPE(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient, zeroas, digmax)
  IF (SE%row) THEN
    CALL disp_quad(title, RESHAPE(x, (/1, SIZE(x)/)), SE)
  ELSE
    CALL disp_quad(title, RESHAPE(x, (/SIZE(x), 1/)), SE)
  END IF
END SUBROUTINE disp_tv_quad

  subroutine disp_tm_quad(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
  ! quadruple precision matrix with title
  CHARACTER(*), INTENT(in) :: title ! The title to use for the matrix
  REAL(quad), INTENT(in) :: x(:, :) ! The matrix to be written
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt ! Editdit descriptor to use for each matrix element (e.g. 'F5.2')
  INTEGER, INTENT(in), OPTIONAL :: unit ! Unit to display on
  INTEGER, INTENT(in), OPTIONAL :: digmax ! Nbr of significant digits for largest abs value in x
  CHARACTER(*), INTENT(in), OPTIONAL :: advance ! 'No' to print next matrix to right of current, otherewise 'Yes'
  CHARACTER(*), INTENT(in), OPTIONAL :: sep ! Separator between matrix columns (e.g. ", ")
  CHARACTER(*), INTENT(in), OPTIONAL :: zeroas ! Zeros are replaced with this string if it is not empty
  CHARACTER(*), INTENT(in), OPTIONAL :: style ! Style(s): See NOTE 1 below
  CHARACTER(*), INTENT(in), OPTIONAL :: trim ! 'Auto' (the default) to trim if fmt absent, 'no' for no
  !                                                ! trimming, 'yes' for trimming
  INTEGER, INTENT(in), OPTIONAL :: LBOUND(:) ! Lower bounds of x
  TYPE(settings) :: SE
  !
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, zeroas=zeroas, digmax=digmax)
  CALL disp_quad(title, x, SE)
END SUBROUTINE disp_tm_quad

SUBROUTINE disp_quad(title, x, SE)
  ! quadruple precision item
  CHARACTER(*), INTENT(in) :: title
  REAL(quad), INTENT(in) :: x(:, :)
  TYPE(settings), INTENT(INOUT) :: SE
  INTEGER wid(SIZE(x, 2)), nbl(SIZE(x, 2))
  CALL find_editdesc_quad(x, SE, wid, nbl) ! determine also SE%w
  CALL tobox_quad(title, x, SE, wid, nbl)
END SUBROUTINE disp_quad

SUBROUTINE tobox_quad(title, x, SE, wid, nbl)
  ! Write quadruple precision matrix to box
  CHARACTER(*), INTENT(in) :: title ! title
  REAL(quad), INTENT(in) :: x(:, :) ! item
  TYPE(settings), INTENT(INOUT) :: SE ! settings
  INTEGER, INTENT(INOUT) :: wid(:) ! widths of columns
  INTEGER, INTENT(INOUT) :: nbl(:) ! number of blanks to trim from left
  CHARACTER(SE%w) :: s(SIZE(x, 1))
  INTEGER :: lin1, j, wleft, m, n, widp(SIZE(wid))
  CHARACTER, POINTER :: boxp(:, :)
  REAL(quad) :: xj(SIZE(x, 1)), h
  m = SIZE(x, 1)
  n = SIZE(x, 2)
  h = HUGE(x)
  CALL preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
  DO j = 1, n
    xj = x(:, j)
    IF (m > 0) WRITE (s, SE%ed) xj
      call replace_zeronaninf(s, SE%zas(1:SE%lzas), xj == 0, xj /= xj, xj < -h, xj > h)
    CALL copytobox(s, lin1, wid(j), widp(j), nbl(j), boxp, wleft)
    IF (j < n) CALL copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp, wleft)
  END DO
  CALL finishbox(title, SE, boxp)
END SUBROUTINE tobox_quad

PURE FUNCTION maxw_quad(x, d) RESULT(w)
  ! Find max field width needed (F0.d editing is specified)
  REAL(quad), INTENT(in) :: x(:)
  INTEGER, INTENT(in) :: d
  INTEGER expmax, expmin, w
  LOGICAL xfinite(SIZE(x))
  REAL(quad) xmax, xmin, h
  CHARACTER(12) :: f1, s(2)
  xmin = 0; xmax = 0; h = HUGE(h)
  xfinite = x == x .AND. x >= -h .AND. x <= h ! neither NaN, Inf nor -Inf
  IF (.NOT. ANY(xfinite)) THEN
    w = 4
  ELSE
    xmax = MAXVAL(x, mask=xfinite)
    xmin = MINVAL(x, mask=xfinite)
    f1 = '(SS,ES9.0E4)'
    WRITE (s, f1) xmax, xmin
    READ (s(:) (5:9), '(I5)') expmax, expmin
    w = MAX(0, expmax, expmin) + d + 4
  END IF
  IF (.NOT. ALL(xfinite)) w = MAX(w, 4)
END FUNCTION maxw_quad

SUBROUTINE find_editdesc_quad(x, SE, wid, nbl)
  ! Determine SE%ed, SE%w (unless specified) and wid.
  ! The if-block (*) is for safety: make f wider in case xm is written ok with the
  ! ES format in fmt but overflows with F format (the feature has been tested through
  ! manual changes to the program).
  REAL(quad), INTENT(in) :: x(:, :) ! Item to be written
  TYPE(settings), INTENT(INOUT) :: SE ! Settings
  INTEGER, INTENT(out) :: wid(SIZE(x, 2)) ! Widths of individual columns
  INTEGER, INTENT(out) :: nbl(SIZE(x, 2)) ! Blanks to trim from left of individual columns
  INTEGER :: expmax, expmin, ww, dd, dmx
  REAL(quad) xmaxv(SIZE(x, 2)), xminv(SIZE(x, 2)), xp, xm, h
  CHARACTER(14) :: f1 = '(SS,ESxx.xxE4)' ! could be ES99.89E4; default is ES14.05E4
  CHARACTER(99) s
    logical xzero(size(x,2)), xallz(size(x,2)), xfinite(size(x,1),size(x,2)), xnonn(size(x,2)), xalln(size(x,2))
  !
  dmx = SE%dmx
  h = HUGE(h)
  xfinite = x == x .AND. x >= -h .AND. x <= h ! neither NaN, Inf nor -Inf
  IF (SE%w == 0) THEN ! Edit descriptor 'F0.d' specified
    ww = maxw_quad(RESHAPE(x, (/SIZE(x)/)), SE%d)
    IF (SE%lzas > 0 .AND. ANY(x == 0._QUAD)) ww = MAX(ww, SE%lzas)
    CALL replace_w(SE%ed, ww)
    SE%w = ww
  ELSEIF (SE%w < 0) THEN ! No edit descriptor specified
    IF (SIZE(x) == 0) THEN
      SE%w = 0
      wid = 0
      nbl = 0
      RETURN
    END IF
    IF (ANY(xfinite)) THEN
      xp = MAXVAL(x, mask=xfinite)
      xm = MINVAL(x, mask=xfinite)
      WRITE (f1(7:11), '(SS,I2,".",I2.2)') dmx + 8, dmx - 1
      WRITE (s, f1) xp; READ (s(dmx + 4:dmx + 8), '(I5)') expmax
      WRITE (s, f1) xm; READ (s(dmx + 4:dmx + 8), '(I5)') expmin
      CALL find_editdesc_real(expmax, expmin, dmx, SE%ed, ww, dd, xm >= 0)
      IF (.NOT. ALL(xfinite)) ww = MAX(ww, 4)
      IF (SE%lzas > 0 .AND. ANY(x == 0._QUAD)) ww = MAX(ww, SE%lzas)
      IF (SE%ed(5:5) == 'F') THEN ! (*)
        WRITE (s, SE%ed) xp; IF (s(1:1) == '*') ww = ww + 1
        WRITE (s, SE%ed) xm; IF (s(1:1) == '*') ww = ww + 1
        WRITE (SE%ed(6:10), '(SS,I2,".",I2)') ww, dd
      END IF
    ELSE
      ww = 4
      SE%ed = '(F4.0)'
    END IF
    SE%w = ww
  END IF
  IF (SE%trm) THEN
    xmaxv = MAXVAL(x, 1, mask=xfinite) ! max in each column
    xminv = MINVAL(x, 1, mask=xfinite) ! min
    xzero = ANY(x == 0._QUAD, 1) ! true where column has some zeros
    xallz = ALL(x == 0._QUAD, 1) ! true where column has only zeros
    xnonn = ANY(x > h .OR. x < -h .OR. x /= x, 1) ! true where column has some nonnormals (inf, -inf, nan)
    xalln = ALL(x > h .OR. x < -h .OR. x /= x, 1) ! true where column has only nonnormals (inf, -inf, nan)
    CALL getwid_quad(xmaxv, xminv, xzero, xallz, xnonn, xalln, SE, wid, nbl)
  ELSE
    wid = SE%w
    nbl = 0
  END IF
END SUBROUTINE find_editdesc_quad

SUBROUTINE getwid_quad(xmaxv, xminv, xzero, xallz, xnonn, xalln, SE, wid, nbl)
  ! determine length of the strings that result when writing with edit descriptor SE%ed a
  ! vector v where v(i) is xmaxv(i) or xminv(i) depending on which gives longer output
  REAL(quad), INTENT(in) :: xmaxv(:), xminv(:) ! max and min values in each column
  LOGICAL, INTENT(in) :: xzero(:), xallz(:) ! true for columns with some/all zeros
  LOGICAL, INTENT(in) :: xnonn(:), xalln(:) ! true for columns with some/all nonnormals
  TYPE(settings), INTENT(in) :: SE ! settings
  INTEGER, INTENT(out) :: wid(:) ! widths of columns
  INTEGER, INTENT(out) :: nbl(:) ! number of blanks to peel from left (w-wid)
  CHARACTER(SE%w) :: stmax(SIZE(xmaxv)), stmin(SIZE(xmaxv))
  INTEGER w
  w = SE%w
  WRITE (stmin, SE%ed) xminv
  WRITE (stmax, SE%ed) xmaxv
  nbl = MOD(VERIFY(stmin, ' ') + w, w + 1) ! loc. of first nonblank
  nbl = MIN(nbl, MOD(VERIFY(stmax, ' ') + w, w + 1))
  IF (SE%gedit) THEN
    wid = w
  ELSE
    wid = LEN_TRIM(ADJUSTL(stmin))
    wid = MAX(wid, LEN_TRIM(ADJUSTL(stmax)))
  END IF
  IF (SE%lzas > 0) THEN
    wid = MERGE(SE%lzas, wid, xallz)
    wid = MAX(wid, MERGE(SE%lzas, 0, xzero))
  END IF
  wid = MERGE(4, wid, xalln)
  wid = MAX(wid, MERGE(4, 0, xnonn))
  nbl = w - wid
END SUBROUTINE getwid_quad

! ******** TOSTRING QUADRUPLE PRECISION PROCEDURES ***********
FUNCTION tostring_s_quad(x) RESULT(st)
  ! Scalar to string
  REAL(quad), INTENT(in) :: x
  CHARACTER(len_f_quad((/x/), tosset0%rfmt)) :: st
  st = tostring_f_quad((/x/), tosset0%rfmt)
END FUNCTION tostring_s_quad

FUNCTION tostring_sf_quad(x, fmt) RESULT(st)
  ! Scalar with specified format to string
  REAL(quad), INTENT(in) :: x
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(len_f_quad((/x/), fmt)) :: st
  st = tostring_f_quad((/x/), fmt)
END FUNCTION tostring_sf_quad

FUNCTION tostring_quad(x) RESULT(st)
  ! Vector to string
  REAL(quad), INTENT(in) :: x(:)
  CHARACTER(len_f_quad(x, tosset0%rfmt)) :: st
  st = tostring_f_quad(x, tosset0%rfmt)
END FUNCTION tostring_quad

FUNCTION tostring_f_quad(x, fmt) RESULT(st)
  ! Vector with specified format to string
  REAL(quad), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(len_f_quad(x, fmt)) :: st
  CHARACTER(widthmax_quad(x, fmt)) :: sa(SIZE(x))
  CHARACTER(nnblk(fmt) + 8) :: fmt1 !(5 for readfmt and 3 for replace_w)
  INTEGER :: w, d, ww
  LOGICAL :: gedit
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w < 0) THEN
    st = errormsg
    RETURN
  ELSEIF (w == 0) THEN
    ww = maxw_quad(x, d)
    CALL replace_w(fmt1, ww)
  END IF
  WRITE (sa, fmt1) x
  CALL trim_real(sa, gedit, w)
  CALL tostring_get(sa, st)
END FUNCTION tostring_f_quad

PURE FUNCTION len_f_quad(x, fmt) RESULT(wtot)
  ! Total length of returned string, vector s
  REAL(quad), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(widthmax_quad(x, fmt)) :: sa(SIZE(x))
  INTEGER :: wtot, w, d, ww
  LOGICAL :: gedit
  CHARACTER(nnblk(fmt) + 8) :: fmt1 !(5 for readfmt and 3 for replace_w)
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w < 0) THEN; wtot = LEN(errormsg); RETURN; END IF
  IF (w == 0) THEN
    ww = maxw_quad(x, d)
    CALL replace_w(fmt1, ww)
  END IF
  WRITE (sa, fmt1) x
  CALL trim_real(sa, gedit, w)
  wtot = SUM(LEN_TRIM(sa)) + (SIZE(x) - 1) * (tosset0%seplen)
END FUNCTION len_f_quad

PURE FUNCTION widthmax_quad(x, fmt) RESULT(w)
  ! Maximum width of an element of x
  REAL(quad), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(nnblk(fmt) + 5) :: fmt1
  INTEGER w, d
  LOGICAL gedit
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w < 0) THEN ! illegal format, use 1
    w = 1
  ELSEIF (w == 0) THEN
    w = maxw_quad(x, d)
  END IF
END FUNCTION widthmax_quad

! *************************************** END OF QUADRUPLE PRECISION PROCEDURES ***************************************

! *************************************** QUADRUPLE PRECISION COMPLEX PROCEDURES **************************************
SUBROUTINE disp_s_cplq(x, fmt, fmt_imag, advance, digmax, sep, trim, unit)
  ! quadruple precision complex scalar without title
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt, fmt_imag, advance, sep, trim
  COMPLEX(quad), INTENT(in) :: x
  INTEGER, INTENT(in), OPTIONAL :: unit, digmax
    call disp_ts_cplq('', x, fmt, fmt_imag, advance, digmax, sep, 'left', trim, unit)
END SUBROUTINE disp_s_cplq

  subroutine disp_v_cplq(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
  ! quadruple precision complex vector without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim, orient
  COMPLEX(quad), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:), digmax
    call disp_tv_cplq('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
END SUBROUTINE disp_v_cplq

  subroutine disp_m_cplq(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
  ! quadruple precision complex matrix without title
CHARACTER(*), INTENT(in), OPTIONAL :: fmt, fmt_imag, advance, sep, style, trim
  COMPLEX(quad), INTENT(in) :: x(:, :)
  INTEGER, INTENT(in), OPTIONAL :: unit, digmax, LBOUND(:)
    call disp_tm_cplq('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
END SUBROUTINE disp_m_cplq

  subroutine disp_ts_cplq(title, x, fmt, fmt_imag, advance, digmax, sep, style, trim, unit)
  ! quadruple precision complex scalar with title
  CHARACTER(*), INTENT(in) :: title
CHARACTER(*), INTENT(in), OPTIONAL :: fmt, fmt_imag, advance, sep, style, trim
  COMPLEX(quad), INTENT(in) :: x
  INTEGER, INTENT(in), OPTIONAL :: unit, digmax
    call disp_tm_cplq(title, reshape((/x/), (/1, 1/)), fmt, fmt_imag, advance, digmax, sep=sep, style=style, &
                    trim=trim, unit=unit)
END SUBROUTINE disp_ts_cplq

  subroutine disp_tv_cplq(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
  ! quadruple precision complex vector with title
  CHARACTER(*), INTENT(in) :: title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim, orient
  COMPLEX(quad), INTENT(in) :: x(:)
  INTEGER, INTENT(in), OPTIONAL :: unit, LBOUND(:), digmax
  TYPE(settings) SE, SEim
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient, digmax=digmax)
  IF (PRESENT(fmt_imag)) THEN
    IF (.NOT. PRESENT(fmt)) THEN
   CALL disp_errmsg('DISP: error, FMT must be present if FMT_IMAG is present'); RETURN; 
    END IF
    CALL get_SE(SEim, title, SHAPE(x), fmt_imag)
  ELSE
    SEim = SE
  END IF
  IF (SE%row) THEN
    CALL disp_cplq(title, RESHAPE(x, (/1, SIZE(x)/)), SE, SEim, n=SIZE(x))
  ELSE
    CALL disp_cplq(title, RESHAPE(x, (/SIZE(x), 1/)), SE, SEim, n=1)
  END IF
END SUBROUTINE disp_tv_cplq

  subroutine disp_tm_cplq(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
  ! quadruple precision complex matrix with title
  CHARACTER(*), INTENT(in) :: title ! The title to use for the matrix
  COMPLEX(quad), INTENT(in) :: x(:, :) ! The matrix to be written
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt ! Edit descriptor for each element (real element when fmt_imag &
  !                                                ! is present)
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt_imag ! Edit descriptor for each imaginary element
  INTEGER, INTENT(in), OPTIONAL :: unit ! Unit to display on
  INTEGER, INTENT(in), OPTIONAL :: digmax ! Nbr of significant digits for largest abs value in real(x) &
  !                                                ! and aimag(x)
  CHARACTER(*), INTENT(in), OPTIONAL :: advance ! 'No' to print next matrix to right of current, otherewise 'Yes'
  CHARACTER(*), INTENT(in), OPTIONAL :: sep ! Separator between matrix columns (e.g. ", ")
  CHARACTER(*), INTENT(in), OPTIONAL :: style ! Style(s): See NOTE 1 below
  CHARACTER(*), INTENT(in), OPTIONAL :: trim ! 'Auto' (the default) to trim if fmt absent, 'no' for no
  !                                                ! trimming, 'yes' for trimming
  INTEGER, INTENT(in), OPTIONAL :: LBOUND(:) ! Lower bounds of x
  !
  TYPE(settings) :: SE, SEim
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, digmax=digmax)
  IF (PRESENT(fmt_imag)) THEN
    IF (.NOT. PRESENT(fmt)) THEN
   CALL disp_errmsg('DISP: error, FMT must be present if FMT_IMAG is present'); RETURN
    END IF
    CALL get_SE(SEim, title, SHAPE(x), fmt_imag)
  ELSE
    SEim = SE
  END IF
  CALL disp_cplq(title, x, SE, SEim, n=SIZE(x, 2))
END SUBROUTINE disp_tm_cplq

SUBROUTINE disp_cplq(title, x, SE, SEim, n)
  ! quadruple precision item
  CHARACTER(*), INTENT(in) :: title
  COMPLEX(quad), INTENT(in) :: x(:, :)
  TYPE(settings), INTENT(INOUT) :: SE, SEim
  INTEGER, INTENT(in) :: n
  INTEGER, DIMENSION(n) :: widre(n), widim(n), nblre(n), nblim(n)
  CALL find_editdesc_quad(REAL(x), SE, widre, nblre) ! determine also SE%w
  CALL find_editdesc_quad(ABS(AIMAG(x)), SEim, widim, nblim) ! determine also SEim%w
    call tobox_cplq(title, x, SE, SEim, widre, widim, nblre, nblim, m = size(x,1), n = size(x,2))
END SUBROUTINE disp_cplq

SUBROUTINE tobox_cplq(title, x, SE, SEim, widre, widim, nblre, nblim, m, n)
  ! Write quadruple precision complex matrix to box
  CHARACTER(*), INTENT(in) :: title
  COMPLEX(quad), INTENT(in) :: x(:, :)
  INTEGER, INTENT(in) :: m, n, widre(:), widim(:), nblre(:), nblim(:)
  TYPE(settings), INTENT(INOUT) :: SE, SEim
  CHARACTER(SE%w) :: s(m)
  CHARACTER(SEim%w) :: sim(m)
  CHARACTER(3) :: sgn(m)
  INTEGER :: lin1, i, j, wleft, wid(n), widp(n)
  CHARACTER, POINTER :: boxp(:, :)
  SE%zas = ''
  SEim%zas = ''
  wid = widre + widim + 4
  CALL preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
  DO j = 1, n
    IF (m > 0) WRITE (s, SE%ed) (REAL(x(i, j)), i=1, m)
      call copytobox(s, lin1, widre(j), widp(j) - widim(j) - 4, nblre(j), boxp,  wleft)
    DO i = 1, m
      IF (AIMAG(x(i, j)) < 0) THEN; sgn(i) = ' - '; ELSE; sgn(i) = ' + '; END IF
    END DO
    CALL copytobox(sgn, lin1, 3, 3, 0, boxp, wleft)
    IF (m > 0) WRITE (sim, SEim%ed) (ABS(AIMAG(x(i, j))), i=1, m)
    CALL copytobox(sim, lin1, widim(j), widim(j), nblim(j), boxp, wleft)
    CALL copyseptobox('i', m, lin1, boxp, wleft)
    IF (j < n) CALL copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp, wleft)
  END DO
  CALL finishbox(title, SE, boxp)
END SUBROUTINE tobox_cplq

! ******* TOSTRING QUADRUPLE PRECISION COMPLEX PROCEDURES ********

FUNCTION tostring_s_cplq(x) RESULT(st)
  COMPLEX(quad), INTENT(in) :: x
  CHARACTER(len_s_cplq(x, tosset0%rfmt)) :: st
  st = tostring_f_cplq((/x/), tosset0%rfmt)
END FUNCTION tostring_s_cplq

FUNCTION tostring_sf_cplq(x, fmt) RESULT(st)
  COMPLEX(quad), INTENT(in) :: x
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(len_s_cplq(x, fmt)) :: st
  st = tostring_f_cplq((/x/), fmt)
END FUNCTION tostring_sf_cplq

FUNCTION tostring_cplq(x) RESULT(st)
  COMPLEX(quad), INTENT(in) :: x(:)
  CHARACTER(len_f_cplq(x, tosset0%rfmt)) :: st
  st = tostring_f_cplq(x, tosset0%rfmt)
END FUNCTION tostring_cplq

FUNCTION tostring_f_cplq(x, fmt) RESULT(st)
  COMPLEX(quad), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(len_f_cplq(x, fmt)) :: st
  CHARACTER(widthmax_quad(REAL(x), fmt)) :: sar(SIZE(x))
  CHARACTER(widthmax_quad(ABS(x - REAL(x)), fmt)) :: sai(SIZE(x)) ! x-real(x) instead of aimag(x) to enable the fnction
  CHARACTER(1) :: sgn(SIZE(x)) ! to pass -stand:f95 switch of the ifort compiler.
  INTEGER :: w, d, wr, wi, i
  LOGICAL :: gedit
  CHARACTER(nnblk(fmt) + 8) :: fmt1 !(5 for readfmt and 3 for replace_w)
  REAL(quad) :: xre(SIZE(x)), xim(SIZE(x)), h
  CALL readfmt(fmt, fmt1, w, d, gedit)
  xre = REAL(x)
  xim = AIMAG(x)
  h = HUGE(h)
  IF (w < 0) THEN
    st = errormsg
    RETURN
  ELSEIF (w == 0) THEN
    wr = maxw_quad(xre, d)
    wi = maxw_quad(xim, d)
    CALL replace_w(fmt1, MAX(wr, wi))
  END IF
  WRITE (sar, fmt1) REAL(x)
  WRITE (sai, fmt1) ABS(AIMAG(x))
  CALL trim_real(sar, gedit, w)
  CALL trim_real(sai, gedit, w)
  DO i = 1, SIZE(x); IF (AIMAG(x(i)) < 0) THEN; sgn(i) = '-'; ELSE; sgn(i) = '+'; END IF; END DO
  CALL tostring_get_complex(sar, sgn, sai, st)
END FUNCTION tostring_f_cplq

PURE FUNCTION len_s_cplq(x, fmt) RESULT(wtot)
  COMPLEX(quad), INTENT(in) :: x
  CHARACTER(*), INTENT(in) :: fmt
  INTEGER :: wtot, w, d
  LOGICAL :: gedit
  CHARACTER(nnblk(fmt) + 8) :: fmt1
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w < 0) THEN; wtot = LEN(errormsg); RETURN; END IF
  wtot = len_f_quad((/REAL(x)/), fmt) + len_f_quad((/ABS(AIMAG(x))/), fmt) + 4
END FUNCTION len_s_cplq

PURE FUNCTION len_f_cplq(x, fmt) RESULT(wtot)
  COMPLEX(quad), INTENT(in) :: x(:)
  CHARACTER(*), INTENT(in) :: fmt
  INTEGER :: wtot, w, d
  LOGICAL :: gedit
  CHARACTER(nnblk(fmt) + 8) :: fmt1
  CALL readfmt(fmt, fmt1, w, d, gedit)
  IF (w < 0) THEN; wtot = LEN(errormsg); RETURN; END IF
    wtot = len_f_quad(real(x), fmt) + len_f_quad(abs(aimag(x)), fmt) + size(x)*4 - (size(x) - 1)*(tosset0%seplen)
  ! subtract seplen because it has been added twice in len_f_quad
END FUNCTION len_f_cplq
! *************************************** END OF QUADRUPLE PRECISION COMPLEX PROCEDURES ********************************

#endif
END MODULE DISP_R16MOD
