! DISPMODULE, A FORTRAN 95 MODULE FOR PRETTY-PRINTING MATRICES.
! Version number 1.03 16-February-2009. This version published as Algorithm 892 in ACM TOMS.
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

MODULE DISPMODULE_UTIL
! Dispmodule_util contains utilities that are used by Dispmodule, and the add-on modules
! disp_i1mod, disp_i2mod,..., disp_l1mod and disp_r16mod. Note that the entities that are
! declared public below are not exported to the user. The private statements in dispmodule and
! the add-on modules prevent that from happening.

USE putstrmodule
IMPLICIT NONE

! ***************** PUBLIC ENTITIES (ONLY PUBLIC TO DISPMODULE, NOT TO USER PROGRAMS) *****************
PRIVATE
public :: disp_settings, defset, factory_settings, tosset, tosfac, errormsg, tostring_settings
public :: nnblk, upper, readfmt, replace_w, trim_real, get_SE, preparebox, copytobox, boxlist, boxnode
public :: copyseptobox, finishbox, tostring_get_complex, disp_errmsg, tostring_get, find_editdesc_real
public :: check_settings, tostring_check_settings, replace_zeronaninf, settings, trim_s_real

! *********************************** GENERAL DECLARATIONS ********************************************
TYPE disp_settings
  ! Settings used by subroutine disp and the utility procedures.
  CHARACTER(6) :: advance = 'YES'
  CHARACTER(9) :: matsep = '   '
  CHARACTER(3) :: orient = 'COL'
  CHARACTER(9) :: sep = '  '
  CHARACTER(9) :: style = 'LEFT'
  CHARACTER(4) :: trim = 'AUTO'
  CHARACTER(9) :: zeroas = ''
  INTEGER :: digmax = 6
  INTEGER :: matseplen = 3
  INTEGER :: seplen = 2
  INTEGER :: unit = DEFAULT_UNIT
  INTEGER :: zaslen = 0
END TYPE disp_settings

TYPE tostring_settings
  ! Settings used by function tostring.
  CHARACTER(10) :: ifmt = 'I0'
  CHARACTER(16) :: rfmt = '1PG12.5' ! 'SP,1P,G20.11E3' has length 14 and is about max
  CHARACTER(9) :: sep = ', '
  INTEGER :: seplen = 2
  CHARACTER(3) :: trimb = 'YES'
  CHARACTER(4) :: trimz = 'G'
END TYPE tostring_settings

TYPE settings
  ! Settings used (privately) by disp and the utility procedures, in the variable SE.
  CHARACTER(22) ed
  CHARACTER(9) sep, tsty, zas
  CHARACTER(1) tch
  INTEGER lun, dmx, w, d, lsep, lzas, m1, n1, adv
  LOGICAL trm, number, vec, row, gedit
END TYPE settings

TYPE(disp_settings) :: DEFSET
!! Current default settings for disp
TYPE(disp_settings) :: FACTORY_SETTINGS
!! Original (factory) settings for disp
TYPE(tostring_settings) :: tosset
!! Current settings for tostring
TYPE(tostring_settings) :: tosfac
!! Factory settings for tostring

CHARACTER(*), PARAMETER :: errormsg = 'Illegal format'

! ********************* BOX-PACKAGE DECLARATIONS (SEE EXPLANATION ABOUT BOX-PACKAGE BELOW) *****************
TYPE boxnode
  ! A box is the character representation of a printed item
  CHARACTER, POINTER :: box(:, :)
  TYPE(boxnode), POINTER :: nextbox => NULL()
END TYPE boxnode
!
TYPE boxlist
  ! There is one list of boxes associated with each logical unit
  INTEGER :: unit = 1
  TYPE(boxnode), POINTER :: firstbox => NULL()
  TYPE(boxnode), POINTER :: lastbox => NULL()
  TYPE(boxlist), POINTER :: nextboxlist => NULL()
END TYPE boxlist
!
TYPE(boxlist), POINTER :: firstboxlist => NULL()
! ************************ END OF BOX-PACKAGE DECLARATIONS ******************************

CONTAINS

! ***************************** GENERAL PROCEDURES **************************************
SUBROUTINE check_settings()
  ! Sanity check of display settings
  CHARACTER(9) :: tsty
  CHARACTER tch
  LOGICAL number, ok, dmxerr, orierr, styerr, adverr
  CHARACTER(6), PARAMETER :: ADVOK(3) = (/'NO    ', 'YES   ', 'DOUBLE'/)
  TYPE(disp_settings) ds
  ds = DEFSET
  CALL getstyles(ds%style, tsty, tch, number, ok)
  styerr = .NOT. ok
  dmxerr = ds%digmax < 1 .OR. ds%digmax > 89
  orierr = ALL(ds%orient /= (/'ROW', 'COL'/))
  adverr = ALL(ds%advance /= ADVOK)
  IF (dmxerr) DEFSET%digmax = 6
  IF (orierr) DEFSET%orient = 'COL'
  IF (styerr) DEFSET%style = 'LEFT'
  IF (adverr) DEFSET%advance = 'YES'
  !
    if (dmxerr) call disp_errmsg('DISP_SET: error, illegal digmax (must be 1-89), set to 6')
    if (orierr) call disp_errmsg('DISP_SET: error, illegal orient: ' // trim(ds%orient) // ', set to "COL"')
    if (styerr) call disp_errmsg('DISP_SET: error, illegal style: ' // trim(ds%style) // ', set to "LEFT"')
    if (adverr) call disp_errmsg('DISP_SET: error, illegal advance: ' // trim(ds%advance) // ', set to "YES"')
END SUBROUTINE check_settings

FUNCTION number_rows(SE) RESULT(nbr)
  ! Should rows be numbered?
  TYPE(settings), INTENT(in) :: SE
  LOGICAL nbr
  nbr = .FALSE.
  IF (.NOT. SE%number) RETURN
  IF (SE%vec .AND. SE%row) RETURN
  nbr = .TRUE.
END FUNCTION number_rows

FUNCTION number_cols(SE) RESULT(nbr)
  ! Should columns be numbered?
  TYPE(settings), INTENT(in) :: SE
  LOGICAL nbr
  nbr = .FALSE.
  IF (.NOT. SE%number) RETURN
  IF (SE%vec .AND. .NOT. SE%row) RETURN
  nbr = .TRUE.
END FUNCTION number_cols

SUBROUTINE preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
  ! Determine format to use to write matrix to box and row where matrix begins, copy
  CHARACTER(*), INTENT(in) :: title ! The title to use for the matrix
  TYPE(settings), INTENT(in) :: SE ! Settings
  INTEGER, INTENT(in) :: m ! Row count of matrix
  INTEGER, INTENT(in) :: n ! Column count of matrix
  INTEGER, INTENT(INOUT) :: wid(:) ! widths of columns in matrix
  INTEGER, INTENT(out) :: widp(:) ! widths of columns in box (max(wid, width of col nums))
  INTEGER, INTENT(out) :: lin1 ! Row number where matrix begins (tsty='left' 0, 'pad' 1, 'underline' 2)
  INTEGER, INTENT(out) :: wleft ! Number of spaces on left of matrix (when tsty is left or title long)
  CHARACTER, POINTER :: boxp(:, :) ! The box

  INTEGER wt, wa ! Char count of title, idth of matrix in characters (wbox = lm + wa + rm)
  INTEGER wbox, wrow ! Width of box in characters, width of row numbers in characters
  INTEGER lm ! Left margin
  INTEGER h, ws ! Height of box in characters, length of column separator
  INTEGER m1, n1, i ! lower bounds (for numbering), index
  CHARACTER(RANGE(0) + 2) sn(2), row_nums(m), col_nums(n)
  CHARACTER(10) fmt

  ! ----------wbox---------    -----------wbox----------     -----wbox------
  ! ---lm---                   --wleft-                             --wt-
  ! ----wleft---                lm wrow    wa       rm       wrow    wa
  !    wt   wrow    wa         ----====-----------======     ----===========
  ! --------====-----------    THIS-IS-A-VERY-LONG-TITLE            TITLE
  !               1     2                1     2                   1     2
  ! MATRIX = 1   4.50  6.80         1   4.50  6.80            1   4.50  6.80
  !          2   6.88  9.22         2   6.88  9.22            2   6.88  9.22
  !          3  19.44  0.08         3  19.44  0.08            3  19.44  0.08
  !          ...                    ...                       ...
  !         10   6.18  4.22        10   6.18  4.22           10   6.18  4.22
  ! rm = 0                     wt = wbox                     lm = rm = 0, wleft = wrow
  m1 = SE%m1
  n1 = SE%n1
  ws = SE%lsep
  wt = LEN(title)
  wrow = 0
  widp = wid
  IF (SE%number) THEN
    fmt = '(SS,I0)'
    IF (number_cols(SE)) THEN
      WRITE (col_nums, fmt) (/(i, i=n1, n1 + n - 1)/)
      widp = MAX(wid, LEN_TRIM(col_nums))
    END IF
    IF (number_rows(SE)) THEN
      WRITE (sn, fmt) m1, m1 + m - 1
      wrow = MAXVAL(LEN_TRIM(sn)) + ws ! determine max width of row numbers
      CALL replace_w(fmt, wrow - ws) ! to create e.g. 'I5' from 'I0'
      WRITE (row_nums, fmt) (/(i, i=m1, m1 + m - 1)/)
    END IF
  END IF
  wa = MAX(0, n - 1) * ws + SUM(widp)
  SELECT CASE (upper(SE%tsty))
  CASE ('LEFT'); lin1 = 1; wbox = wt + wrow + wa; h = MAX(1, m); lm = wt
  CASE ('PAD'); lin1 = 2; wbox = MAX(wt, wa + wrow); h = m + 1; lm = MAX(0, (wt - wa - wrow) / 2)
  CASE ('UNDERLINE'); lin1 = 3; wbox = MAX(wt, wa + wrow); h = m + 2; lm = MAX(0, (wt - wa - wrow) / 2)
  CASE default; lin1 = 1; wbox = 0; h = 0; lm = 0 ! should not happen
  END SELECT
  wleft = lm
  IF (number_cols(SE)) h = h + 1
  CALL newbox(SE%lun, h, wbox, boxp)
  IF (number_cols(SE)) THEN
CALL copycolumnnumberstobox(col_nums, wleft + wrow, wid, widp, ws, boxp, lin1)
  END IF
  IF (number_rows(SE)) THEN
      call copytobox(row_nums, lin1, wrow - ws, wrow - ws, nblj = 0, boxp = boxp, wleft = wleft)
    CALL copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp, wleft)
  END IF
END SUBROUTINE preparebox

SUBROUTINE copytobox(s, lin1, widj, widpj, nblj, boxp, wleft)
  ! Copy strings to column in boxp; update wleft to current char column in boxp
  CHARACTER(*), INTENT(in) :: s(:) ! the strings to copy
  INTEGER, INTENT(in) :: lin1, widj ! first line in box to copy to, width of column
  INTEGER, INTENT(in) :: nblj, widpj ! number of blank characters to trim from left of s, offset to next col
  CHARACTER, INTENT(INOUT) :: boxp(:, :) ! the box to accept the column
  INTEGER, INTENT(INOUT) :: wleft ! number of char-columns in box already written to
  INTEGER i, j
  wleft = wleft + widpj - widj
  ! forall(i = 1:widj, j=1:size(s)) boxp(wleft+i, j+lin1-1) = s(j)(i+nblj:i+nblj)
  DO CONCURRENT(i=1:widj, j=1:SIZE(s))
    boxp(wleft + i, j + lin1 - 1) = s(j) (i + nblj:i + nblj)
  END DO
  wleft = wleft + widj
END SUBROUTINE copytobox

SUBROUTINE copyseptobox(sep, m, lin1, boxp, wleft)
  ! Copy column separator to boxp; update wleft
  CHARACTER(*), INTENT(in) :: sep
  INTEGER, INTENT(in) :: m, lin1
  CHARACTER, INTENT(INOUT) :: boxp(:, :)
  INTEGER, INTENT(INOUT) :: wleft
  INTEGER i, j
  ! forall(i = 1:len(sep), j=1:m) boxp(wleft+i, j+lin1-1) = sep(i:i)
  DO CONCURRENT(i=1:LEN(sep), j=1:m)
    boxp(wleft + i, j + lin1 - 1) = sep(i:i)
  END DO
  wleft = wleft + LEN(sep)
END SUBROUTINE copyseptobox

SUBROUTINE copycolumnnumberstobox(s, wleft, wid, widp, lsep, boxp, lin1)
  CHARACTER(*), INTENT(in) :: s(:) ! strings with left-adjusted column numbers
  INTEGER, INTENT(in) :: wleft ! char positions on left of 1st col
  INTEGER, INTENT(in) :: wid(:) ! widths of columns in matrix
  INTEGER, INTENT(in) :: widp(:) ! widths of columns in box (max(wid, width of col nums))
  INTEGER, INTENT(in) :: lsep ! width of column separator
  CHARACTER, INTENT(INOUT) :: boxp(:, :) ! receives the numbers
  INTEGER, INTENT(INOUT) :: lin1 ! line number in box to copy to
  INTEGER ls(SIZE(s)), rmargmax, k, i, lmargin, j
  !
  ls = LEN_TRIM(s)
  rmargmax = (MAX(0, MINVAL(wid) - MAXVAL(ls))) / 2 ! locate according to narrowest column, widest number
  k = wleft
  DO i = 1, SIZE(wid)
    lmargin = MAX(0, widp(i) - ls(i) - rmargmax)
    k = k + lmargin
    DO CONCURRENT(j=1:ls(i))
      boxp(k + j, lin1) = s(i) (j:j)
    END DO
    k = k + widp(i) - lmargin + lsep
  END DO
  lin1 = lin1 + 1
END SUBROUTINE copycolumnnumberstobox

SUBROUTINE finishbox(title, SE, boxp)
  ! Finish creating a box and display it if advancing is turned on
  CHARACTER(*), INTENT(in) :: title ! The title to use for the matrix
  TYPE(settings), INTENT(in) :: SE ! Settings
  CHARACTER, INTENT(INOUT) :: boxp(:, :) ! The box
  !
  INTEGER i, wt, w, wpadright, wpadleft ! index, width of title, width of box and spacing on either side of it
  INTEGER lin1 ! line to put left title
  !
  wt = LEN(title)
  w = SIZE(boxp, 1)
  IF (upper(SE%tsty) == 'LEFT') THEN
    lin1 = 1
    IF (number_cols(SE)) lin1 = MIN(2, SIZE(boxp, 2))
    ! forall(i=1:wt) boxp(i,lin1) = title(i:i)
    DO CONCURRENT(i=1:wt)
      boxp(i, lin1) = title(i:i)
    END DO
  ELSE
    wpadright = (w - wt) / 2
    wpadleft = w - wpadright - wt
    ! forall(i=1:wt) boxp(wpadleft+i, 1) = title(i:i)
    DO CONCURRENT(i=1:wt)
      boxp(wpadleft + i, 1) = title(i:i)
    END DO
    IF (upper(SE%tsty) == 'PAD') THEN
      boxp(1:wpadleft, 1) = SE%tch
      boxp(w - wpadright + 1:w, 1) = SE%tch
    ELSE ! tsty == 'UNDERLINE'
      boxp(:, 2) = SE%tch
    END IF
  END IF
  IF (SE%adv >= 1) CALL dispboxlist(SE%lun, DEFSET%matsep(1:DEFSET%matseplen))
  IF (SE%adv >= 2) CALL dispnewline(SE%lun)
END SUBROUTINE finishbox

SUBROUTINE find_editdesc_real(exp, expm, dmx, edesc, flen, ndec, posit)
  ! Subroutine of find_editdesc_sngl and find_editdesc_dble
  INTEGER, INTENT(in) :: expm, dmx
  INTEGER, INTENT(INOUT) :: exp
  CHARACTER(14), INTENT(out) :: edesc
  INTEGER, INTENT(out) :: flen, ndec
  LOGICAL, INTENT(in) :: posit
  INTEGER :: neg, nxp
  exp = MAX(exp, expm)
  neg = 1
  IF (exp < dmx .AND. exp >= -1) THEN
    IF (posit .OR. exp > MAX(0, expm)) neg = 0
    edesc = '(SS,Fxx.yy)'
    ndec = MAX(0, dmx - exp - 1)
    flen = neg + 2 + ndec + MAX(0, exp) ! -X.YYYYY (2 covers X and .)
    WRITE (edesc(6:10), '(SS,I2,".",I2)') flen, ndec
  ELSE
    IF (posit) neg = 0
    IF (ABS(exp) > 999) THEN; nxp = 4
    ELSEIF (ABS(exp) > 99) THEN; nxp = 3
    ELSEIF (ABS(exp) > 9) THEN; nxp = 2
    ELSE; nxp = 1
    END IF
    flen = neg + 3 + dmx + nxp
    edesc = '(SS,ESxx.yyEz)'
    WRITE (edesc(7:13), '(SS,I2,".",I2,"E",I1)') flen, dmx - 1, nxp
    ndec = dmx - 1
  END IF
END SUBROUTINE find_editdesc_real

PURE SUBROUTINE readfmt(fmt, fmt1, w, d, gedit)
  ! Returns w and d when fmt is (Xw.d) or (Xw) (then d = 0), X = edit descriptor letter
  ! (I, F, etc). X can also be ES, DS, 1PG or 1PF. Returns w = -1 for illegal fmt.
  ! Returns gedit = .true. if fmt is Gw.d. How about SS,1PES4.3?
  CHARACTER(*), INTENT(in) :: fmt ! e.g. fmt = F 8.2
  CHARACTER(*), INTENT(out) :: fmt1 ! returns '(SS,F8.2)'
  CHARACTER ch
  INTEGER, INTENT(out) :: w, d
  LOGICAL, INTENT(out) :: gedit
  INTEGER :: k0, k1, k2, k3, k4
  CALL sszipfmt(fmt, fmt1)
  w = -1; d = 0; gedit = .FALSE.
  k1 = VERIFY(fmt1(2:), '0123456789') + 1
  IF (k1 == 0) RETURN ! only digits
  k2 = VERIFY(fmt1(k1:), 'ABDEFGILNOPSZabdefgilnopsz,') + k1 - 1 ! , for "1P,G12.3"
  IF (k2 <= k1) RETURN ! no letter or only letters
  ch = upper(fmt1(k2 - 1:k2 - 1))
  IF (ch == ',') THEN ! deal with SS,1PG13.5
    k0 = k2
    k1 = VERIFY(fmt1(k0:), '0123456789') + k0 - 1
    IF (k1 == 0) RETURN
    k2 = VERIFY(fmt1(k1:), 'ABDEFGILNOPSZabdefgilnopsz,') + k1 - 1
    IF (k2 <= k1) RETURN
    ch = upper(fmt1(k2 - 1:k2 - 1))
  END IF
  gedit = ch == 'G' .OR. ch == 'g'
  k3 = VERIFY(fmt1(k2:), '0123456789') + k2 - 1
  IF (k3 == k2) RETURN ! no digits
  READ (fmt1(k2:k3 - 1), *) w
  IF (k3 > LEN(fmt1)) RETURN
  IF (fmt1(k3:k3) /= '.') RETURN ! not . after w
  k4 = VERIFY(fmt1(k3 + 1:), '0123456789') + k3
  IF (k4 == k3 + 1) RETURN ! no digits
  READ (fmt1(k3 + 1:k4 - 1), *) d
END SUBROUTINE readfmt

PURE SUBROUTINE replace_w(fmt, wnew)
  ! Change e.g. '(F0.3)' to '(F5.3)'. Works also for '(SS,I0)' to '(SS,I5)'. If wnew > 999, set it to 999
  CHARACTER(*), INTENT(INOUT) :: fmt
  INTEGER, INTENT(in) :: wnew
  INTEGER :: k0, k1, k2, k3
  CHARACTER(3) rw
  k1 = VERIFY(fmt(2:), '0123456789') + 1
  k2 = VERIFY(fmt(k1:), 'ABDEFGILNOPSZabdefgilnopsz,') + k1 - 1
  IF (k2 == k1) RETURN ! no letter
  IF (fmt(k2 - 1:k2 - 1) == ',') THEN ! Handle (SS,1PF10.3)
    k0 = k2
    k1 = VERIFY(fmt(k0:), '0123456789') + 1
    IF (k1 == 0) RETURN
    k2 = VERIFY(fmt(k1:), 'ABDEFGILNOPSZabdefgilnopsz,') + k1 - 1
    IF (k2 <= k1) RETURN
  END IF
  k3 = VERIFY(fmt(k2:), '0123456789') + k2 - 1
  IF (k3 == k2) RETURN ! no digits
  WRITE (rw, '(SS,I0)') MIN(999, wnew)
  fmt = fmt(1:k2 - 1)//TRIM(rw)//fmt(k3:)
END SUBROUTINE replace_w

  subroutine get_SE(SE, title, shapex, fmt, advance, lbound, seperator, style, trim, unit, orient, zeroas, digmax)
  ! Get the settings from the optional parameters fmt...zeroas in to the structure SE.
  ! Replace absent arguments with corresponding values from the structure DEFSET.
  TYPE(settings), INTENT(out) :: SE
  CHARACTER(*), INTENT(in) :: title
  INTEGER, INTENT(in) :: shapex(:)
  CHARACTER(*), INTENT(in), OPTIONAL :: fmt
  INTEGER, INTENT(in), OPTIONAL :: unit, digmax, LBOUND(:)
    character(*),   intent(in), optional :: advance, seperator, style, zeroas, trim, orient
  LOGICAL ok
  !
  CHARACTER(22) ed
  CHARACTER(9) sep, tsty, zas
  CHARACTER(1) tch
  CHARACTER(6) advchr
  INTEGER lun, dmx, w, d, lsep, lzas, m1, n1, adv
  LOGICAL trm, number, vec, row, is_scalar, gedit
  !
  vec = (SIZE(shapex) == 1)
  is_scalar = SIZE(shapex) == 0
  IF (vec .AND. PRESENT(orient)) THEN
    SELECT CASE (upper(orient))
    CASE ('ROW'); row = .TRUE.
    CASE ('COL'); row = .FALSE.
    CASE default; 
        call disp_errmsg('DISP: error, wrong value of orient: '//orient(1:len_trim(orient))//', using "COL"')
      row = .FALSE.
    END SELECT
  ELSEIF (vec) THEN
    row = DEFSET%orient == 'ROW'
  ELSE
    row = .FALSE.
  END IF
  IF (PRESENT(fmt)) THEN
    CALL readfmt(fmt, ed, w, d, gedit)
  ELSE
    ed = '()'
    w = -1; d = 0; gedit = .FALSE.
  END IF
  IF (PRESENT(unit)) THEN
    lun = unit
  ELSE
    lun = DEFSET%unit
  END IF
  IF (.NOT. PRESENT(digmax)) THEN
    dmx = DEFSET%digmax
  ELSEIF (PRESENT(fmt)) THEN
 CALL disp_errmsg('DISP: error, both FMT and DIGMAX present, ignoring DIGMAX')
    dmx = 1
  ELSEIF (digmax < 1 .OR. digmax > 89) THEN
    CALL disp_errmsg('DISP: error, digmax must be >= 1 and < 90, using 6')
    dmx = 6
  ELSE
    dmx = digmax
  END IF
  IF (PRESENT(advance)) THEN
    advchr = upper(advance)
  ELSE
    advchr = DEFSET%advance
  END IF
  SELECT CASE (trims(advchr))
  CASE ('NO'); adv = 0
  CASE ('YES'); adv = 1
  CASE ('DOUBLE'); adv = 2
  CASE default
      call disp_errmsg('DISP: error, illegal advance: ' // trims(advance) // ', using "YES"')
    adv = 1
  END SELECT
  IF (PRESENT(trim)) THEN
      if (upper(trim) /= 'YES' .and. upper(trim) /= 'NO' .and. upper(trim) /= 'AUTO') then
 CALL disp_errmsg('DISP: error, illegal trim: '//trims(trim)//', using "YES"')
      trm = .TRUE.
    ELSE
trm = upper(trim) == 'YES' .OR. upper(trim) == 'AUTO' .AND. .NOT. PRESENT(FMT)
    END IF
  ELSEIF (w == 0) THEN
    trm = .TRUE.
  ELSE
trm = DEFSET%trim == 'YES' .OR. DEFSET%trim == 'AUTO' .AND. .NOT. PRESENT(FMT)
  END IF
  IF (PRESENT(seperator)) THEN
    sep = seperator
    lsep = LEN(seperator)
  ELSE
    sep = DEFSET%sep
    lsep = DEFSET%seplen
  END IF
  IF (PRESENT(style)) THEN
    CALL getstyles(style, tsty, tch, number, ok)
      if (.not. ok) call disp_errmsg('DISP: error, illegal style: '//style//'. Using default instead')
  ELSE
    CALL getstyles(DEFSET%style, tsty, tch, number, ok)
  END IF
  IF (title == '') tsty = 'LEFT'
  IF (is_scalar) number = .FALSE.
  IF (PRESENT(zeroas)) THEN
    zas = zeroas
    lzas = LEN(zeroas)
  ELSE
    zas = DEFSET%zeroas
    lzas = DEFSET%zaslen
  END IF
  IF (w > 0) lzas = MIN(w, lzas)
  zas = zas(1:lzas)
  m1 = 1
  n1 = 1
  IF (PRESENT(lbound)) THEN
    number = .TRUE.
    IF (SIZE(lbound) == 1) THEN
      IF (vec .AND. row) THEN
        n1 = LBOUND(1)
      ELSE
        m1 = LBOUND(1)
      END IF
    ELSEIF (SIZE(lbound) >= 2) THEN
      m1 = LBOUND(1)
      n1 = LBOUND(2)
    END IF
  END IF
    SE = settings(ed, sep, tsty, zas, tch, lun, dmx, w, d, lsep, lzas, m1, n1, adv, trm, number, vec, row, gedit)
CONTAINS
  FUNCTION trims(s) RESULT(t)
    CHARACTER(*), INTENT(in) :: s
    CHARACTER(LEN_TRIM(s)) :: t
    INTRINSIC trim
    t = TRIM(s)
  END FUNCTION trims
END SUBROUTINE get_SE

SUBROUTINE getstyles(style, tsty, tch, number, ok)
  ! Return tsty = 'LEFT', 'PAD', or 'UNDERLINE', tch = x from xPAD or xUNDERLINE, number = .true. if style includes
  ! NUMBER. If style has ABOVE, return tsty = 'PAD' and tch = ' '. Return tsty = 'LEFT' if error. See NOTE 1 below.
  CHARACTER(*), INTENT(in) :: style
  CHARACTER(9), INTENT(out) :: tsty
  CHARACTER(1), INTENT(out) :: tch
  LOGICAL, INTENT(out) :: number, ok
  INTEGER kamp, i, nsty
  CHARACTER(LEN(style)) :: sty(2)
    character(9), parameter :: LPUA(4) = (/'LEFT     ', 'PAD      ', 'UNDERLINE', 'ABOVE    '/)
  CHARACTER(9), PARAMETER :: PU(2) = (/'PAD      ', 'UNDERLINE'/)
  kamp = SCAN(upper(style), '&')
  ok = .TRUE.
  IF (kamp > 0) THEN
    sty(1) = ADJUSTL(upper(style(1:kamp - 1)))
    sty(2) = ADJUSTL(upper(style(kamp + 1:)))
    nsty = 2
  ELSE
    sty(1) = ADJUSTL(upper(style))
    nsty = 1
  END IF
  number = .FALSE.
  tsty = 'LEFT'
  tch = '-'
  DO i = 1, nsty
    IF (sty(i) == 'NUMBER') THEN
      number = .TRUE.
    ELSEIF (sty(i) == 'ABOVE') THEN
      tsty = 'PAD'
      tch = ' '
    ELSEIF (ANY(sty(i) == LPUA)) THEN
      tsty = sty(i)
    ELSEIF (ANY(sty(i) (2:) == PU)) THEN
      tsty = sty(i) (2:)
      tch = sty(i) (1:1)
    ELSE
      ok = .FALSE.
      RETURN
    END IF
  END DO
  ok = .TRUE.
END SUBROUTINE getstyles

SUBROUTINE replace_zeronaninf(s, zas, maskz, masknan, maskminf, maskinf)
  ! replace zeros in s (where maskz is true) with zas (i.e. zero-as string) also replace nans with 'NaN',
  ! infinities with '+Inf' and minus infinities with '-Inf'. Zeros are aligned with . if zas contains .
  ! otherwise right-adjusted. Nans, and infs are right adjusted.
  ! NOTE: There are compiler bugs in current versions of both the Absoft and the Pathscale compilers
  ! so the merge calls (commented out below) had to be replaced with do loops.
  CHARACTER(*), INTENT(INOUT) :: s(:)
  LOGICAL, INTENT(in) :: maskz(:), masknan(:), maskinf(:), maskminf(:)
  CHARACTER(*), INTENT(in) :: zas
  OPTIONAL :: masknan, maskminf, maskinf
  CHARACTER(LEN(s)) z, nan, minf, inf
  INTEGER w, wz, n, i, k, zasdot
  w = LEN(s)
  wz = LEN(zas)
  n = SIZE(maskz)
  IF (wz /= 0 .AND. wz <= w) THEN ! zas not empty and not too wide
    zasdot = INDEX(zas, '.')
    z = ''
    IF (zasdot > 0) THEN
      DO i = 1, n
        IF (maskz(i)) EXIT
      END DO
      IF (i <= n) THEN ! some zeros
        k = INDEX(s(i), '.')
        IF (k == 0 .OR. zasdot > k .OR. wz - zasdot > w - k) THEN ! cannot align .'s
          z(w - wz + 1:) = zas ! align right
        ELSE
          z(k - zasdot + 1:k - zasdot + wz) = zas
        END IF
      END IF
    ELSE
      z(w - wz + 1:) = zas
    END IF
    ! s = merge(z, s, maskz)
    DO i = 1, n
      IF (maskz(i)) s(i) = z
    END DO
  END IF
  IF (PRESENT(masknan)) THEN
    IF (w >= 4) THEN
      nan = REPEAT(' ', w - 4)//' NaN'
      minf = REPEAT(' ', w - 4)//'-Inf'
      inf = REPEAT(' ', w - 4)//'+Inf'
    ELSEIF (w == 3) THEN
      nan = 'NaN'
      minf = '***'
      inf = 'Inf'
    ELSE
      nan = REPEAT('*', w)
      minf = nan
      inf = nan
    END IF
    ! s = merge(nan, s, masknan)
    ! s = merge(minf, s, maskminf)
    ! s = merge(inf, s, maskinf)
    DO i = 1, n
      IF (masknan(i)) s(i) = nan
      IF (maskminf(i)) s(i) = minf
      IF (maskinf(i)) s(i) = inf
    END DO
  END IF
END SUBROUTINE replace_zeronaninf

PURE FUNCTION upper(s) RESULT(su) ! Change string to upper case
  CHARACTER(*), INTENT(in) :: s
  CHARACTER(LEN(s)) su
  CHARACTER(26), PARAMETER :: ll = 'abcdefghijklmnopqrstuvwxyz', &
                              ul = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  INTEGER i, k
  su = s
  DO i = 1, LEN(s)
    k = INDEX(ll, s(i:i))
    IF (k > 0) su(i:i) = ul(k:k)
  END DO
END FUNCTION upper

PURE SUBROUTINE sszipfmt(fmt, fmt1)
  ! Set fmt1 to '(SS,'//removeblanks(fmt)//')'. Caller is responsible that
  ! fmt1 has sufficient length.
  CHARACTER(*), INTENT(in) :: fmt
  CHARACTER(*), INTENT(out) :: fmt1
  INTEGER i, j
  fmt1 = '(SS,'
  j = 5
  DO i = 1, LEN(fmt)
    IF (fmt(i:i) /= ' ') THEN
      fmt1(j:j) = fmt(i:i)
      j = j + 1
    END IF
  END DO
  fmt1(j:j) = ')'
END SUBROUTINE sszipfmt

PURE FUNCTION nnblk(s) RESULT(n) ! count nonblanks in s
  CHARACTER(*), INTENT(in) :: s
  INTEGER i, n
  n = 0
  DO i = 1, LEN(s)
    IF (s(i:i) /= ' ') n = n + 1
  END DO
END FUNCTION nnblk

SUBROUTINE disp_errmsg(s)
  CHARACTER(*), INTENT(in) :: s
  INTEGER wleft
  CHARACTER(1), POINTER :: boxp(:, :)
  wleft = 0
  CALL newbox(DEFAULT_UNIT, 1, LEN(s), boxp)
    call copytobox((/s/), lin1 = 1, widj = len(s), widpj = len(s), nblj = 0, boxp = boxp, wleft = wleft)
  CALL dispboxlist(DEFAULT_UNIT, sep='')
END SUBROUTINE disp_errmsg
! *********************************** END OF GENERAL PROCEDURES *********************************

! ************************************* TOSTRING PROCEDURES *************************************
SUBROUTINE tostring_check_settings
  ! Sanity check of tostring settings
  TYPE(tostring_settings) ts
  INTEGER wi, wr, d
  CHARACTER(MAX(LEN(tosset%rfmt), LEN(tosset%ifmt)) + 5) fmt1
  LOGICAL gedit
  ts = tosset
  IF (ALL(ts%trimb /= (/'YES', 'NO '/))) tosset%trimb = tosfac%trimb
  IF (ALL(ts%trimz /= (/'NONE', 'ALL ', 'G   '/))) tosset%trimz = tosfac%trimz
  CALL readfmt(tosset%rfmt, fmt1, wr, d, gedit)
  CALL readfmt(tosset%ifmt, fmt1, wi, d, gedit)
  IF (wr < 0) tosset%rfmt = tosfac%rfmt
  IF (wi < 0) tosset%ifmt = tosfac%ifmt
  IF (ALL(ts%trimb /= (/'YES ', 'NO  ', 'AUTO'/))) CALL disp_errmsg( &
         'TOSTRING_SET: error, illegal trimb: '//trim(ts%trimb)//', set to ' // trim(tosfac%trimb))
  IF (ALL(ts%trimz /= (/'NONE', 'ALL ', 'G   '/))) CALL disp_errmsg( &
         'TOSTRING_SET: error, illegal trimz: '//trim(ts%trimz)//', set to '//trim(tosfac%trimz))
  IF (wr < 0) CALL disp_errmsg( &
         'TOSTRING_SET: error, illegal rfmt: '//trim(ts%rfmt)//', set to '//trim(tosfac%rfmt))
  IF (wi < 0) CALL disp_errmsg( &
         'TOSTRING_SET: error, illegal ifmt: '//trim(ts%ifmt)//', set to '//trim(tosfac%ifmt))
END SUBROUTINE tostring_check_settings

PURE SUBROUTINE trim_s_real(sa, gedit, w)
  ! Trim trailing zeros and possibly decimal point from fractional part.
  ! If sa = '52.2000E12' on entry then it is returned as '52.2E12   '.
  ! Whether trimming is actually done depends on tosset, gedit and w.
  CHARACTER(*), INTENT(INOUT) :: sa
  LOGICAL, INTENT(in) :: gedit
  INTEGER, INTENT(in) :: w
  INTEGER k, k2, k3
  IF (tosset%trimb == 'YES' .OR. w == 0) sa = ADJUSTL(sa)
  IF (tosset%trimz == 'ALL' .OR. tosset%trimz == 'G' .AND. gedit) THEN
    k = SCAN(sa, '.')
    IF (k > 0) THEN
      k2 = VERIFY(sa(k + 1:), '0123456789') + k
      IF (k2 == k) k2 = LEN(sa) + 1
      k3 = VERIFY(sa(k:k2 - 1), '0.', back=.TRUE.) + k - 1
      sa(k3 + 1:) = sa(k2:)
    END IF
  END IF
END SUBROUTINE trim_s_real

PURE SUBROUTINE trim_real(sa, gedit, w)
  ! Trim trailing zeros and possibly decimal point from fractional part.
  ! If sa = '52.2000E12' on entry then it is returned as '52.2E12   '.
  ! Whether trimming is actually done depends on tosset, gedit and w.
  CHARACTER(*), INTENT(INOUT) :: sa(:)
  LOGICAL, INTENT(in) :: gedit
  INTEGER, INTENT(in) :: w
  INTEGER i
  IF (tosset%trimb == 'YES' .OR. w == 0) sa = ADJUSTL(sa)
  IF (tosset%trimz == 'ALL' .OR. tosset%trimz == 'G' .AND. gedit) THEN
    DO i = 1, SIZE(sa) ! trim trailing zeros from fractional part
      CALL trim_s_real(sa(i), gedit, w)
    END DO
  END IF
END SUBROUTINE trim_real

PURE SUBROUTINE tostring_get(sa, st)
  ! Copy trimmed elements of sa (containing individual elements as strings) to the final
  ! tostring result st, separated by tosset%sep strings.
  CHARACTER(*), INTENT(in) :: sa(:)
  CHARACTER(*), INTENT(out) :: st
  INTEGER :: i, k, n, sepl
  sepl = tosset%seplen
  k = 0
  DO i = 1, SIZE(sa)
    IF (k > 0) st(k + 1:k + sepl) = tosset%sep(1:sepl)
    IF (k > 0) k = k + sepl
    n = LEN_TRIM(sa(i))
    st(k + 1:k + n) = TRIM(sa(i))
    k = k + n
  END DO
END SUBROUTINE tostring_get

PURE SUBROUTINE tostring_get_complex(sar, sgn, sai, st)
  ! Version of tostring_get for complex numbers
  CHARACTER(*), INTENT(in) :: sar(:), sai(:), sgn(*)
  CHARACTER(*), INTENT(out) :: st
  INTEGER :: i, k, n, sepl
  sepl = tosset%seplen
  k = 0
  DO i = 1, SIZE(sar)
    IF (k > 0) st(k + 1:k + sepl) = tosset%sep(1:sepl)
    IF (k > 0) k = k + sepl
    n = LEN_TRIM(sar(i))
    st(k + 1:k + n) = TRIM(sar(i))
    st(k + n + 1:k + n + 3) = ' '//sgn(i)//' '
    k = k + n + 3
    n = LEN_TRIM(sai(i))
    st(k + 1:k + n) = TRIM(sai(i))
    st(k + n + 1:k + n + 1) = 'i'
    k = k + n + 1
  END DO
END SUBROUTINE tostring_get_complex

! ********************************* END OF TOSTRING PROCEDURES *********************************

! *********************************** BOX-PACKAGE **********************************************
!
! A "box" is a variable dimension character matrix that can be created dynamically. There are
! linked lists of boxes, one for each logical unit. When disp is called the item to be displayed
! is written to a box. If advance = 'no' is in effect, the writing out of the items is delayed
! until disp is called on the same unit with advance = 'yes' in effect; then all the boxes in
! the relevant list are written to the unit. There are two subroutines that are meant to be
! called from outside the Box-package: NEWBOX and DISPBOXLIST:
!
! CALL NEWBOX(UNIT, M, N, BOXP) creates a box on unit UNIT. BOXP returns a pointer to the
! created box which is of type CHARACTER and DIMENSION (M,N).
!
! CALL DISPBOXLIST(UNIT, SEP) writes all the boxes in the list associated with UNIT to the file
! on UNIT, separated with the string SEP. The following example makes this clear: let SEP = ' : '
! and let the first box contain XXX and the second have two rows, both equal to YYYY. Then the
! text written will be: XXX : YYYY : YYYY
!
! To obtain tab-separated boxes when using ASCII, let SEP = char(9). After writing the boxes,
! the complete list is deallocated. If UNIT = -3 the asterisk unit (usually command window) is
! written to. If UNIT = -2 the routine putstr from the disp_where unit is used for writing. If
! UNIT = -1 all output will be discarded. With the iso_fortran_env module of Fortran 2003, unit
! may also equal OUTPUT_UNIT, unless the compiler sets that to -2.

FUNCTION getboxlist(unit) RESULT(p)
  ! Return boxlist associated with specified unit. If this list does not exist a new list is started.
  INTEGER, INTENT(in) :: unit
  TYPE(boxlist), POINTER :: p
  p => firstboxlist
  DO WHILE (ASSOCIATED(p))
    IF (p%unit == unit) RETURN
    p => p%nextboxlist
  END DO
  ALLOCATE (p)
  p%nextboxlist => firstboxlist ! put at head of list
  p%unit = unit
  firstboxlist => p
END FUNCTION getboxlist

SUBROUTINE clearboxlist(unit)
  ! Deallocate all boxes associated with unit
  INTEGER, INTENT(in) :: unit
  TYPE(boxnode), POINTER :: p, q
  TYPE(boxlist), POINTER :: blp
  blp => firstboxlist
  DO WHILE (ASSOCIATED(blp))
    IF (blp%unit == unit) EXIT
    blp => blp%nextboxlist
  END DO
  IF (.NOT. ASSOCIATED(blp)) RETURN
  p => blp%firstbox
  DO WHILE (ASSOCIATED(p))
    q => p
    p => p%nextbox
    DEALLOCATE (q%box)
    DEALLOCATE (q)
  END DO
  IF (ASSOCIATED(firstboxlist, blp)) THEN
    firstboxlist => blp%nextboxlist
  END IF
  DEALLOCATE (blp)
END SUBROUTINE clearboxlist

SUBROUTINE newbox(unit, m, n, boxp)
  ! Create a new box
  CHARACTER, POINTER :: boxp(:, :)
  INTEGER, INTENT(in) :: unit, m, n
  TYPE(boxnode), POINTER :: p
  TYPE(boxlist), POINTER :: blp
  ALLOCATE (p)
  ALLOCATE (p%box(n, m))
  blp => getboxlist(unit)
  IF (.NOT. ASSOCIATED(blp%firstbox)) THEN
    blp%firstbox => p
  ELSE
    blp%lastbox%nextbox => p
  END IF
  blp%lastbox => p
  boxp => p%box
  boxp = ' '
END SUBROUTINE newbox

FUNCTION tostr(a) RESULT(s)
  ! Copy char array to string
  CHARACTER, INTENT(in) :: a(:)
  CHARACTER(SIZE(a)) s
  INTEGER i
  DO i = 1, SIZE(a)
    s(i:i) = a(i)
  END DO
END FUNCTION tostr

SUBROUTINE dispboxlist(unit, sep)
  ! Display the list of boxes associated with unit
  INTEGER, INTENT(in) :: unit
  TYPE(boxnode), POINTER :: pfirst, p
  TYPE(boxlist), POINTER :: blp
  INTEGER k, nlines, h, w, ns
  CHARACTER(*), INTENT(in) :: sep
  blp => getboxlist(unit)
  pfirst => blp%firstbox
  nlines = 0
  p => pfirst
  DO WHILE (ASSOCIATED(p))
    nlines = MAX(nlines, SIZE(p%box, 2))
    p => p%nextbox
  END DO
  DO k = 1, nlines
    p => pfirst
    ns = 0
    DO WHILE (ASSOCIATED(p))
      h = SIZE(p%box, 2)
      w = SIZE(p%box, 1)
      IF (k <= h) THEN
        SELECT CASE (unit)
        CASE (-1)
          CONTINUE
        CASE (-2)
          CALL putstr(sep(1:ns)//tostr(p%box(:, k)))
        CASE (-3)
          WRITE (*, '(2A)', advance='no') sep(1:ns), tostr(p%box(:, k))
        CASE default
          WRITE (unit, '(2A)', advance='no') sep(1:ns), tostr(p%box(:, k))
        END SELECT
      ELSE
        SELECT CASE (unit)
        CASE (-1)
          CONTINUE
        CASE (-2)
          CALL putstr(sep(1:ns)//REPEAT(' ', w))
        CASE (-3)
          WRITE (*, '(2A)', advance='no') sep(1:ns), REPEAT(' ', w)
        CASE default
          WRITE (unit, '(2A)', advance='no') sep(1:ns), REPEAT(' ', w)
        END SELECT
      END IF
      p => p%nextbox
      ns = LEN(sep)
    END DO
    CALL dispnewline(unit)
  END DO
  CALL clearboxlist(unit)
END SUBROUTINE dispboxlist

SUBROUTINE dispnewline(unit)
  INTEGER, INTENT(in) :: unit
  SELECT CASE (unit)
  CASE (-1); CONTINUE
  CASE (-2); CALL putnl
  CASE (-3); WRITE (*, *)
  CASE default; WRITE (unit, *)
  END SELECT
END SUBROUTINE dispnewline

!   subroutine print_boxes
!     ! Print info on all boxes (used for debug purposes)
!     integer :: k
!     type(boxlist), pointer :: bl
!     type(boxnode), pointer :: p
!     bl => firstboxlist
!     write(*,'("BOXES:")')
!     do while (associated(bl))
!       write(*,'("UNIT=",SS,I0,":")') bl%unit
!       p => bl%firstbox
!       k = 1
!       do while(associated(p))
!         write(*,'("  box ",SS,I0,", size=(",I0,",",I0,")")') k, shape(p%box)
!         k = k+1
!         p => p%nextbox
!       enddo
!       bl => bl%nextboxlist
!     enddo
!   end subroutine print_boxes

! ******************************** END OF BOX-PACKAGE *******************************

END MODULE DISPMODULE_UTIL
