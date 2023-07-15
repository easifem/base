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

  use putstrmodule
  implicit none

  ! ***************** PUBLIC ENTITIES (ONLY PUBLIC TO DISPMODULE, NOT TO USER PROGRAMS) *****************
  private
  public disp_settings, defset, factory_settings, tosset, tosfac, errormsg, tostring_settings
  public nnblk, upper, readfmt, replace_w, trim_real, get_SE, preparebox, copytobox, boxlist, boxnode
  public copyseptobox, finishbox, tostring_get_complex, disp_errmsg, tostring_get, find_editdesc_real
  public check_settings, tostring_check_settings, replace_zeronaninf, settings, trim_s_real

  ! *********************************** GENERAL DECLARATIONS ********************************************
  type disp_settings
    ! Settings used by subroutine disp and the utility procedures.
    character(6) :: advance     = 'YES'
    character(9) :: matsep      = '   '
    character(3) :: orient      = 'COL'
    character(9) :: sep         = '  '
    character(9) :: style       = 'LEFT'
    character(4) :: trim        = 'AUTO'
    character(9) :: zeroas      = ''
    integer      :: digmax      = 6
    integer      :: matseplen   = 3
    integer      :: seplen      = 2
    integer      :: unit        = DEFAULT_UNIT
    integer      :: zaslen      = 0
  end type disp_settings

  type tostring_settings
    ! Settings used by function tostring.
    character(10) :: ifmt = 'I0'
    character(16) :: rfmt = '1PG12.5'  ! 'SP,1P,G20.11E3' has length 14 and is about max
    character(9)  :: sep = ', '
    integer       :: seplen = 2
    character(3)  :: trimb = 'YES'
    character(4)  :: trimz = 'G'
  end type tostring_settings

  type settings
    ! Settings used (privately) by disp and the utility procedures, in the variable SE.
    character(22) ed
    character(9) sep, tsty, zas
    character(1) tch
    integer lun, dmx, w, d, lsep, lzas, m1, n1, adv
    logical trm, number, vec, row, gedit
  end type settings

  type(disp_settings), save :: DEFSET, &        ! Current default settings for disp
       &                       FACTORY_SETTINGS ! Original (factory) settings for disp
  type(tostring_settings), save :: tosset, & ! Current settings for tostring
       &                           tosfac    ! Factory settings for tostring

  character(*), parameter :: errormsg = 'Illegal format'

  ! ********************* BOX-PACKAGE DECLARATIONS (SEE EXPLANATION ABOUT BOX-PACKAGE BELOW) *****************
  type boxnode
    ! A box is the character representation of a printed item
    character, pointer     :: box(:,:)
    type(boxnode), pointer :: nextbox => null()
  end type boxnode
  !
  type boxlist
    ! There is one list of boxes associated with each logical unit
    integer :: unit = 1
    type(boxnode), pointer :: firstbox => null()
    type(boxnode), pointer :: lastbox => null()
    type(boxlist), pointer :: nextboxlist => null()
  end type boxlist
  !
  type(boxlist), pointer :: firstboxlist => null()
  ! ************************ END OF BOX-PACKAGE DECLARATIONS ******************************

CONTAINS

  ! ***************************** GENERAL PROCEDURES **************************************
  subroutine check_settings()
    ! Sanity check of display settings
    character(9) :: tsty
    character tch
    logical number, ok, dmxerr, orierr, styerr, adverr
    character(6), parameter :: ADVOK(3) = (/'NO    ', 'YES   ', 'DOUBLE'/)
    type(disp_settings) ds
    ds = DEFSET
    call getstyles(ds%style, tsty, tch, number, ok)
    styerr = .not. ok
    dmxerr = ds%digmax < 1 .or. ds%digmax > 89
    orierr = all(ds%orient /= (/'ROW', 'COL'/))
    adverr = all(ds%advance /= ADVOK)
    if (dmxerr) DEFSET%digmax = 6
    if (orierr) DEFSET%orient = 'COL'
    if (styerr) DEFSET%style = 'LEFT'
    if (adverr) DEFSET%advance = 'YES'
    !
    if (dmxerr) call disp_errmsg('DISP_SET: error, illegal digmax (must be 1-89), set to 6')
    if (orierr) call disp_errmsg('DISP_SET: error, illegal orient: ' // trim(ds%orient) // ', set to "COL"')
    if (styerr) call disp_errmsg('DISP_SET: error, illegal style: ' // trim(ds%style) // ', set to "LEFT"')
    if (adverr) call disp_errmsg('DISP_SET: error, illegal advance: ' // trim(ds%advance) // ', set to "YES"')
  end subroutine check_settings

  function number_rows(SE) result(nbr)
    ! Should rows be numbered?
    type(settings), intent(in) :: SE
    logical nbr
    nbr = .false.
    if (.not. SE%number) return
    if (SE%vec .and. SE%row) return
    nbr = .true.
  end function number_rows

  function number_cols(SE) result(nbr)
    ! Should columns be numbered?
    type(settings), intent(in) :: SE
    logical nbr
    nbr = .false.
    if (.not. SE%number) return
    if (SE%vec .and. .not. SE%row) return
    nbr = .true.
  end function number_cols

  subroutine preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
    ! Determine format to use to write matrix to box and row where matrix begins, copy
    character(*),   intent(in)    :: title     ! The title to use for the matrix
    type(settings), intent(in)    :: SE        ! Settings
    integer,        intent(in)    :: m         ! Row count of matrix
    integer,        intent(in)    :: n         ! Column count of matrix
    integer,        intent(INOUT ) :: wid(:)    ! widths of columns in matrix
    integer,        intent(out)   :: widp(:)   ! widths of columns in box (max(wid, width of col nums))
    integer,        intent(out)   :: lin1      ! Row number where matrix begins (tsty='left' 0, 'pad' 1, 'underline' 2)
    integer,        intent(out)   :: wleft     ! Number of spaces on left of matrix (when tsty is left or title long)
    character, pointer            :: boxp(:,:) ! The box

    integer wt, wa          ! Char count of title, idth of matrix in characters (wbox = lm + wa + rm)
    integer wbox, wrow      ! Width of box in characters, width of row numbers in characters
    integer lm              ! Left margin
    integer h,ws            ! Height of box in characters, length of column separator
    integer m1, n1, i       ! lower bounds (for numbering), index
    character(range(0) + 2) sn(2), row_nums(m), col_nums(n)
    character(10) fmt

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
    wt = len(title)
    wrow = 0
    widp = wid
    if (SE%number) then
      fmt = '(SS,I0)'
      if (number_cols(SE)) then
        write(col_nums, fmt) (/ (i, i = n1, n1 + n - 1) /)
        widp = max(wid, len_trim(col_nums))
      endif
      if (number_rows(SE)) then
        write(sn, fmt) m1, m1 + m - 1
        wrow = maxval(len_trim(sn)) + ws  ! determine max width of row numbers
        call replace_w(fmt, wrow - ws) ! to create e.g. 'I5' from 'I0'
        write(row_nums, fmt) (/ (i, i = m1, m1 + m - 1) /)
      endif
    endif
    wa = max(0,n-1)*ws + sum(widp)
    select case(upper(SE%tsty))
    case('LEFT');      lin1 = 1; wbox = wt + wrow + wa;     h = max(1,m); lm = wt
    case('PAD');       lin1 = 2; wbox = max(wt, wa + wrow); h = m + 1;    lm = max(0, (wt - wa - wrow)/2)
    case('UNDERLINE'); lin1 = 3; wbox = max(wt, wa + wrow); h = m + 2;    lm = max(0, (wt - wa - wrow)/2)
    case default;      lin1 = 1; wbox = 0; h = 0; lm = 0 ! should not happen
    end select
    wleft = lm
    if (number_cols(SE)) h = h + 1
    call newbox(SE%lun, h, wbox, boxp)
    if (number_cols(SE)) then
      call copycolumnnumberstobox(col_nums, wleft + wrow, wid, widp, ws,  boxp, lin1)
    endif
    if (number_rows(SE)) then
      call copytobox(row_nums, lin1, wrow - ws, wrow - ws, nblj = 0, boxp = boxp, wleft = wleft)
      call copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp, wleft)
    endif
  end subroutine preparebox

  subroutine copytobox(s, lin1, widj, widpj, nblj, boxp,  wleft)
    ! Copy strings to column in boxp; update wleft to current char column in boxp
    character(*), intent(in)    :: s(:)        ! the strings to copy
    integer,      intent(in)    :: lin1, widj  ! first line in box to copy to, width of column
    integer,      intent(in)    :: nblj, widpj ! number of blank characters to trim from left of s, offset to next col
    character,    intent(INOUT ) :: boxp(:,:)   ! the box to accept the column
    integer,      intent(INOUT ) :: wleft       ! number of char-columns in box already written to
    integer i, j
    wleft = wleft + widpj - widj
    ! forall(i = 1:widj, j=1:size(s)) boxp(wleft+i, j+lin1-1) = s(j)(i+nblj:i+nblj)
    do concurrent(i = 1:widj, j=1:size(s)) 
      boxp(wleft+i, j+lin1-1) = s(j)(i+nblj:i+nblj)
    end do
    wleft = wleft + widj
  end subroutine copytobox

  subroutine copyseptobox(sep, m, lin1, boxp,  wleft)
    ! Copy column separator to boxp; update wleft
    character(*), intent(in)    :: sep
    integer,      intent(in)    :: m, lin1
    character,    intent(INOUT ) :: boxp(:,:)
    integer,      intent(INOUT ) :: wleft
    integer i, j
    ! forall(i = 1:len(sep), j=1:m) boxp(wleft+i, j+lin1-1) = sep(i:i)
    do concurrent(i = 1:len(sep), j=1:m) 
      boxp(wleft+i, j+lin1-1) = sep(i:i)
    end do
    wleft = wleft + len(sep)
  end subroutine copyseptobox

  subroutine copycolumnnumberstobox(s, wleft, wid, widp, lsep, boxp, lin1)
    character(*), intent(in)    :: s(:)      ! strings with left-adjusted column numbers
    integer,      intent(in)    :: wleft     ! char positions on left of 1st col
    integer,      intent(in)    :: wid(:)    ! widths of columns in matrix
    integer,      intent(in)    :: widp(:)   ! widths of columns in box (max(wid, width of col nums))
    integer,      intent(in)    :: lsep      ! width of column separator
    character,    intent(INOUT ) :: boxp(:,:) ! receives the numbers
    integer,      intent(INOUT ) :: lin1      ! line number in box to copy to
    integer ls(size(s)), rmargmax, k, i, lmargin, j
    !
    ls = len_trim(s)
    rmargmax = (max(0, minval(wid) - maxval(ls)))/2 ! locate according to narrowest column, widest number
    k = wleft
    do i = 1, size(wid)
      lmargin = max(0, widp(i) - ls(i) - rmargmax)
      k = k + lmargin
      do concurrent(j = 1:ls(i)) 
        boxp(k+j, lin1) = s(i)(j:j)
      end do
      k = k + widp(i) - lmargin + lsep
    enddo
    lin1 = lin1 + 1
  end subroutine copycolumnnumberstobox

  subroutine finishbox(title, SE, boxp)
    ! Finish creating a box and display it if advancing is turned on
    character(*),   intent(in)    :: title     ! The title to use for the matrix
    type(settings), intent(in)    :: SE        ! Settings
    character,      intent(INOUT ) :: boxp(:,:) ! The box
    !
    integer i, wt, w, wpadright, wpadleft ! index, width of title, width of box and spacing on either side of it
    integer lin1 ! line to put left title
    !
    wt = len(title)
    w = size(boxp,1)
    if (upper(SE%tsty) == 'LEFT') then
      lin1 = 1
      if (number_cols(SE)) lin1 = min(2,size(boxp,2))
      ! forall(i=1:wt) boxp(i,lin1) = title(i:i)
      do concurrent(i=1:wt) 
        boxp(i,lin1) = title(i:i)
      end do
    else
      wpadright = (w - wt)/2
      wpadleft = w - wpadright - wt
      ! forall(i=1:wt) boxp(wpadleft+i, 1) = title(i:i)
      do concurrent(i=1:wt) 
        boxp(wpadleft+i, 1) = title(i:i)
      end do
      if (upper(SE%tsty) == 'PAD') then
        boxp(1:wpadleft, 1) = SE%tch
        boxp(w-wpadright+1:w, 1) = SE%tch
      else ! tsty == 'UNDERLINE'
        boxp(:,2) = SE%tch
      endif
    endif
    if (SE%adv >= 1) call dispboxlist(SE%lun, DEFSET%matsep(1:DEFSET%matseplen))
    if (SE%adv >= 2) call dispnewline(SE%lun)
  end subroutine finishbox

  subroutine find_editdesc_real(exp, expm, dmx,  edesc, flen, ndec, posit)
    ! Subroutine of find_editdesc_sngl and find_editdesc_dble
    integer,       intent(in)    :: expm, dmx
    integer,       intent(INOUT ) :: exp
    character(14), intent(out)   :: edesc
    integer,       intent(out)   :: flen, ndec
    logical,       intent(in)    :: posit
    integer :: neg, nxp
    exp = max(exp, expm)
    neg = 1
    if (exp < dmx .and. exp >= -1) then
      if (posit .or. exp > max(0, expm)) neg = 0
      edesc = '(SS,Fxx.yy)'
      ndec = max(0, dmx - exp - 1)
      flen = neg + 2 + ndec + max(0,exp) ! -X.YYYYY (2 covers X and .)
      write(edesc(6:10), '(SS,I2,".",I2)') flen, ndec
    else
      if (posit) neg = 0
      if     (abs(exp) > 999) then; nxp = 4
      elseif (abs(exp) >  99) then; nxp = 3
      elseif (abs(exp) >   9) then; nxp = 2
      else                        ; nxp = 1
      endif
      flen = neg + 3 + dmx + nxp
      edesc = '(SS,ESxx.yyEz)'
      write(edesc(7:13), '(SS,I2,".",I2,"E",I1)') flen, dmx - 1, nxp
      ndec = dmx - 1
    endif
  end subroutine find_editdesc_real

  pure subroutine readfmt(fmt, fmt1, w, d, gedit)
    ! Returns w and d when fmt is (Xw.d) or (Xw) (then d = 0), X = edit descriptor letter
    ! (I, F, etc). X can also be ES, DS, 1PG or 1PF. Returns w = -1 for illegal fmt.
    ! Returns gedit = .true. if fmt is Gw.d. How about SS,1PES4.3?
    character(*), intent(in)  :: fmt  ! e.g. fmt = F 8.2
    character(*), intent(out) :: fmt1 ! returns '(SS,F8.2)'
    character ch
    integer, intent(out) :: w, d
    logical, intent(out) :: gedit
    integer :: k0, k1, k2, k3, k4
    call sszipfmt(fmt, fmt1)
    w = -1; d = 0; gedit = .false.
    k1 = verify(fmt1(2:), '0123456789') + 1
    if (k1 == 0) return ! only digits
    k2 = verify(fmt1(k1:), 'ABDEFGILNOPSZabdefgilnopsz,') + k1 - 1 ! , for "1P,G12.3"
    if (k2 <= k1) return ! no letter or only letters
    ch = upper(fmt1(k2-1:k2-1))
    if (ch == ',') then ! deal with SS,1PG13.5
      k0 = k2
      k1 = verify(fmt1(k0:),'0123456789') + k0 - 1
      if (k1==0) return
      k2 = verify(fmt1(k1:),'ABDEFGILNOPSZabdefgilnopsz,') + k1 - 1
      if (k2 <= k1) return
      ch = upper(fmt1(k2-1:k2-1))
    endif
    gedit = ch == 'G' .or. ch == 'g'
    k3 = verify(fmt1(k2:), '0123456789') + k2 - 1
    if (k3 == k2) return ! no digits
    read(fmt1(k2:k3-1), *) w
    if (k3 > len(fmt1)) return
    if (fmt1(k3:k3) /= '.') return ! not . after w
    k4 = verify(fmt1(k3+1:), '0123456789') + k3
    if (k4 == k3+1) return ! no digits
    read(fmt1(k3+1:k4-1), *) d
  end subroutine readfmt

  pure subroutine replace_w(fmt, wnew)
    ! Change e.g. '(F0.3)' to '(F5.3)'. Works also for '(SS,I0)' to '(SS,I5)'. If wnew > 999, set it to 999
    character(*), intent(INOUT ) :: fmt
    integer, intent(in) :: wnew
    integer :: k0, k1, k2, k3
    character(3) rw
    k1 = verify(fmt(2:), '0123456789') + 1
    k2 = verify(fmt(k1:), 'ABDEFGILNOPSZabdefgilnopsz,') + k1 - 1
    if (k2 == k1) return ! no letter
    if (fmt(k2-1:k2-1)==',') then ! Handle (SS,1PF10.3)
      k0 = k2
      k1 = verify(fmt(k0:),'0123456789') + 1
      if (k1==0) return
      k2 = verify(fmt(k1:),'ABDEFGILNOPSZabdefgilnopsz,') + k1 - 1
      if (k2 <= k1) return
    end if
    k3 = verify(fmt(k2:), '0123456789') + k2 - 1
    if (k3 == k2) return ! no digits
    write(rw, '(SS,I0)') min(999,wnew)
    fmt = fmt(1:k2-1) // trim(rw) // fmt(k3:)
  end subroutine replace_w

  subroutine get_SE(SE, title, shapex, fmt, advance, lbound, seperator, style, trim, unit, orient, zeroas, digmax)
    ! Get the settings from the optional parameters fmt...zeroas in to the structure SE.
    ! Replace absent arguments with corresponding values from the structure DEFSET.
    type(settings), intent(out)          :: SE
    character(*),   intent(in)           :: title
    integer,        intent(in)           :: shapex(:)
    character(*),   intent(in), optional :: fmt
    integer,        intent(in), optional :: unit, digmax, lbound(:)
    character(*),   intent(in), optional :: advance, seperator, style, zeroas, trim, orient
    logical ok
    !
    character(22) ed
    character(9) sep, tsty, zas
    character(1) tch
    character(6) advchr
    integer lun, dmx, w, d, lsep, lzas, m1, n1, adv
    logical trm, number, vec, row, is_scalar, gedit
    !
    vec = (size(shapex) == 1)
    is_scalar = size(shapex) == 0
    if (vec .and. present(orient)) then
      select case(upper(orient))
      case('ROW');  row = .true.
      case('COL');  row = .false.
      case default;
        call disp_errmsg('DISP: error, wrong value of orient: '//orient(1:len_trim(orient))//', using "COL"')
        row = .false.
      end select
    elseif (vec) then
      row = DEFSET%orient == 'ROW'
    else
      row = .false.
    endif
    if (present(fmt)) then
      call readfmt(fmt, ed, w, d, gedit)
    else
      ed = '()'
      w = -1; d = 0; gedit = .false.
    endif
    if (present(unit)) then
      lun = unit
    else
      lun = DEFSET%unit
    endif
    if (.not.present(digmax)) then
      dmx = DEFSET%digmax
    elseif (present(fmt)) then
      call disp_errmsg('DISP: error, both FMT and DIGMAX present, ignoring DIGMAX')
      dmx = 1
    elseif (digmax < 1 .or. digmax > 89) then
      call disp_errmsg('DISP: error, digmax must be >= 1 and < 90, using 6')
      dmx = 6
    else
      dmx = digmax
    endif
    if (present(advance)) then
      advchr = upper(advance)
    else
      advchr = DEFSET%advance
    endif
    select case(trims(advchr))
    case('NO');     adv = 0
    case('YES');    adv = 1
    case('DOUBLE'); adv = 2
    case default
      call disp_errmsg('DISP: error, illegal advance: ' // trims(advance) // ', using "YES"')
      adv = 1
    end select
    if (present(trim)) then
      if (upper(trim) /= 'YES' .and. upper(trim) /= 'NO' .and. upper(trim) /= 'AUTO') then
        call disp_errmsg('DISP: error, illegal trim: ' // trims(trim) // ', using "YES"')
        trm = .true.
      else
        trm = upper(trim) == 'YES' .or. upper(trim) == 'AUTO' .and. .not.present(FMT)
      endif
    elseif (w == 0) then
      trm = .true.
    else
      trm = DEFSET%trim == 'YES' .or. DEFSET%trim == 'AUTO' .and. .not.present(FMT)
    endif
    if (present(seperator)) then
      sep = seperator
      lsep = len(seperator)
    else
      sep = DEFSET%sep
      lsep = DEFSET%seplen
    endif
    if (present(style)) then
      call getstyles(style, tsty, tch, number, ok)
      if (.not. ok) call disp_errmsg('DISP: error, illegal style: '//style//'. Using default instead')
    else
      call getstyles(DEFSET%style, tsty, tch, number, ok)
    endif
    if (title == '') tsty = 'LEFT'
    if (is_scalar) number = .false.
    if (present(zeroas)) then
      zas = zeroas
      lzas = len(zeroas)
    else
      zas = DEFSET%zeroas
      lzas = DEFSET%zaslen
    endif
    if (w > 0) lzas = min(w, lzas)
    zas = zas(1:lzas)
    m1 = 1
    n1 = 1
    if (present(lbound)) then
      number = .true.
      if (size(lbound) == 1) then
        if (vec .and. row) then
          n1 = lbound(1)
        else
          m1 = lbound(1)
        endif
      elseif (size(lbound) >= 2) then
        m1 = lbound(1)
        n1 = lbound(2)
      endif
    endif
    SE = settings(ed, sep, tsty, zas, tch, lun, dmx, w, d, lsep, lzas, m1, n1, adv, trm, number, vec, row, gedit)
  contains
    function trims(s) result(t)
      character(*), intent(in) :: s
      character(len_trim(s)) :: t
      intrinsic trim
      t = trim(s)
    end function trims
  end subroutine get_SE

  subroutine getstyles(style, tsty, tch, number, ok)
    ! Return tsty = 'LEFT', 'PAD', or 'UNDERLINE', tch = x from xPAD or xUNDERLINE, number = .true. if style includes
    ! NUMBER. If style has ABOVE, return tsty = 'PAD' and tch = ' '. Return tsty = 'LEFT' if error. See NOTE 1 below.
    character(*), intent(in) :: style
    character(9), intent(out) :: tsty
    character(1), intent(out) :: tch
    logical,      intent(out) :: number, ok
    integer kamp, i, nsty
    character(len(style))   :: sty(2)
    character(9), parameter :: LPUA(4) = (/'LEFT     ', 'PAD      ', 'UNDERLINE', 'ABOVE    '/)
    character(9), parameter :: PU(2) = (/'PAD      ', 'UNDERLINE'/)
    kamp = scan(upper(style), '&')
    ok = .true.
    if (kamp > 0) then
      sty(1) = adjustl(upper(style(1:kamp-1)))
      sty(2) = adjustl(upper(style(kamp+1:)))
      nsty = 2
    else
      sty(1) = adjustl(upper(style))
      nsty = 1
    end if
    number = .false.
    tsty = 'LEFT'
    tch = '-'
    do i = 1, nsty
      if (sty(i) == 'NUMBER') then
        number = .true.
      elseif (sty(i) == 'ABOVE') then
        tsty = 'PAD'
        tch = ' '
      elseif (any(sty(i) == LPUA)) then
        tsty = sty(i)
      elseif (any(sty(i)(2:) == PU)) then
        tsty = sty(i)(2:)
        tch = sty(i)(1:1)
      else
        ok = .false.
        return
      endif
    enddo
    ok = .true.
  end subroutine getstyles

  subroutine replace_zeronaninf(s, zas, maskz, masknan, maskminf, maskinf)
    ! replace zeros in s (where maskz is true) with zas (i.e. zero-as string) also replace nans with 'NaN',
    ! infinities with '+Inf' and minus infinities with '-Inf'. Zeros are aligned with . if zas contains .
    ! otherwise right-adjusted. Nans, and infs are right adjusted.
    ! NOTE: There are compiler bugs in current versions of both the Absoft and the Pathscale compilers
    ! so the merge calls (commented out below) had to be replaced with do loops.
    character(*), intent(INOUT ) :: s(:)
    logical     , intent(in)    :: maskz(:), masknan(:), maskinf(:), maskminf(:)
    character(*), intent(in)    :: zas
    optional                    :: masknan, maskminf, maskinf
    character(len(s)) z, nan, minf, inf
    integer w, wz, n, i, k, zasdot
    w = len(s)
    wz = len(zas)
    n = size(maskz)
    if (wz /= 0 .and. wz <= w) then ! zas not empty and not too wide
      zasdot = index(zas, '.')
      z = ''
      if (zasdot > 0) then
        do i=1,n
          if (maskz(i)) exit
        enddo
        if (i<=n) then ! some zeros
          k = index(s(i), '.')
          if (k == 0 .or. zasdot > k .or. wz-zasdot > w-k) then ! cannot align .'s
            z(w-wz+1:) = zas ! align right
          else
            z(k-zasdot+1:k-zasdot+wz) = zas
          endif
        endif
      else
        z(w-wz+1:) = zas
      end if
      ! s = merge(z, s, maskz)
      do i=1,n
        if (maskz(i)) s(i) = z
      enddo
    endif
    if (present(masknan)) then
      if (w >= 4) then
        nan = repeat(' ', w-4) // ' NaN'
        minf = repeat(' ', w-4) // '-Inf'
        inf = repeat(' ', w-4) // '+Inf'
      elseif (w == 3) then
        nan = 'NaN'
        minf = '***'
        inf = 'Inf'
      else
        nan = repeat('*',w)
        minf = nan
        inf = nan
      endif
      ! s = merge(nan, s, masknan)
      ! s = merge(minf, s, maskminf)
      ! s = merge(inf, s, maskinf)
      do i=1,n
        if (masknan(i)) s(i) = nan
        if (maskminf(i)) s(i) = minf
        if (maskinf(i)) s(i) = inf
      enddo
    endif
  end subroutine replace_zeronaninf

  pure function upper(s) result(su) ! Change string to upper case
    character(*), intent(in) :: s
    character(len(s)) su
    character(26), parameter :: ll = 'abcdefghijklmnopqrstuvwxyz', &
         ul = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    integer i, k
    su = s
    do i = 1,len(s)
      k = index(ll, s(i:i))
      if (k > 0) su(i:i) = ul(k:k)
    end do
  end function upper

  pure subroutine sszipfmt(fmt, fmt1)
    ! Set fmt1 to '(SS,'//removeblanks(fmt)//')'. Caller is responsible that
    ! fmt1 has sufficient length.
    character(*), intent(in) :: fmt
    character(*), intent(out) :: fmt1
    integer i,j
    fmt1 = '(SS,'
    j = 5
    do i = 1,len(fmt)
      if (fmt(i:i) /= ' ') then
        fmt1(j:j) = fmt(i:i)
        j = j+1
      endif
    enddo
    fmt1(j:j) = ')'
  end subroutine sszipfmt

  pure function nnblk(s) result(n) ! count nonblanks in s
    character(*), intent(in) :: s
    integer i, n
    n = 0
    do i = 1,len(s)
      if (s(i:i) /= ' ') n = n+1
    enddo
  end function nnblk

  subroutine disp_errmsg(s)
    character(*), intent(in) :: s
    integer wleft
    character(1), pointer :: boxp(:,:)
    wleft = 0
    call newbox(DEFAULT_UNIT, 1, len(s), boxp)
    call copytobox((/s/), lin1 = 1, widj = len(s), widpj = len(s), nblj = 0, boxp = boxp, wleft = wleft)
    call dispboxlist(DEFAULT_UNIT, sep = '')
  end subroutine disp_errmsg
  ! *********************************** END OF GENERAL PROCEDURES *********************************

  ! ************************************* TOSTRING PROCEDURES *************************************
  subroutine tostring_check_settings
    ! Sanity check of tostring settings
    type(tostring_settings) ts
    integer wi, wr, d
    character(max(len(tosset%rfmt), len(tosset%ifmt)) + 5) fmt1
    logical gedit
    ts = tosset
    if (all(ts%trimb /= (/'YES', 'NO '/)))           tosset%trimb = tosfac%trimb
    if (all(ts%trimz /= (/'NONE', 'ALL ', 'G   '/))) tosset%trimz = tosfac%trimz
    call readfmt(tosset%rfmt, fmt1, wr, d, gedit)
    call readfmt(tosset%ifmt, fmt1, wi, d, gedit)
    if (wr < 0) tosset%rfmt = tosfac%rfmt
    if (wi < 0) tosset%ifmt = tosfac%ifmt
    if (all(ts%trimb /= (/'YES ', 'NO  ', 'AUTO'/))) call disp_errmsg( &
         'TOSTRING_SET: error, illegal trimb: '//trim(ts%trimb)//', set to ' // trim(tosfac%trimb))
    if (all(ts%trimz /= (/'NONE', 'ALL ', 'G   '/))) call disp_errmsg( &
         'TOSTRING_SET: error, illegal trimz: '//trim(ts%trimz)//', set to '//trim(tosfac%trimz))
    if (wr < 0) call disp_errmsg( &
         'TOSTRING_SET: error, illegal rfmt: '//trim(ts%rfmt)//', set to '//trim(tosfac%rfmt))
    if (wi < 0) call disp_errmsg( &
         'TOSTRING_SET: error, illegal ifmt: '//trim(ts%ifmt)//', set to '//trim(tosfac%ifmt))
  end subroutine tostring_check_settings

  pure subroutine trim_s_real(sa, gedit, w)
    ! Trim trailing zeros and possibly decimal point from fractional part.
    ! If sa = '52.2000E12' on entry then it is returned as '52.2E12   '.
    ! Whether trimming is actually done depends on tosset, gedit and w.
    character(*), intent(INOUT ) :: sa
    logical, intent(in) :: gedit
    integer, intent(in) :: w
    integer k, k2, k3
    if (tosset%trimb == 'YES' .or. w == 0) sa = adjustl(sa)
    if (tosset%trimz == 'ALL' .or. tosset%trimz == 'G' .and. gedit) then
      k = scan(sa, '.')
      if (k > 0) then
        k2 = verify(sa(k+1:), '0123456789') + k
        if (k2 == k) k2 = len(sa) + 1
        k3 = verify(sa(k:k2-1), '0.', back=.true.) + k - 1
        sa(k3+1:) = sa(k2:)
      endif
    endif
  end subroutine trim_s_real

  pure subroutine trim_real(sa, gedit, w)
    ! Trim trailing zeros and possibly decimal point from fractional part.
    ! If sa = '52.2000E12' on entry then it is returned as '52.2E12   '.
    ! Whether trimming is actually done depends on tosset, gedit and w.
    character(*), intent(INOUT ) :: sa(:)
    logical, intent(in) :: gedit
    integer, intent(in) :: w
    integer i
    if (tosset%trimb == 'YES' .or. w == 0) sa = adjustl(sa)
    if (tosset%trimz == 'ALL' .or. tosset%trimz == 'G' .and. gedit) then
      do i=1,size(sa) ! trim trailing zeros from fractional part
        call trim_s_real(sa(i), gedit, w)
      enddo
    endif
  end subroutine trim_real

  pure subroutine tostring_get(sa, st)
    ! Copy trimmed elements of sa (containing individual elements as strings) to the final
    ! tostring result st, separated by tosset%sep strings.
    character(*), intent(in)  :: sa(:)
    character(*), intent(out) :: st
    integer                   :: i, k, n, sepl
    sepl = tosset%seplen
    k = 0
    do i = 1,size(sa)
      if (k>0) st(k+1:k+sepl) = tosset%sep(1:sepl)
      if (k>0) k = k + sepl
      n = len_trim(sa(i))
      st(k+1:k+n) = trim(sa(i))
      k = k + n
    end do
  end subroutine tostring_get

  pure subroutine tostring_get_complex(sar, sgn, sai, st)
    ! Version of tostring_get for complex numbers
    character(*), intent(in)  :: sar(:), sai(:), sgn(*)
    character(*), intent(out) :: st
    integer                   :: i, k, n, sepl
    sepl = tosset%seplen
    k = 0
    do i = 1,size(sar)
      if (k>0) st(k+1:k+sepl) = tosset%sep(1:sepl)
      if (k>0) k = k + sepl
      n = len_trim(sar(i))
      st(k+1:k+n) = trim(sar(i))
      st(k+n+1:k+n+3) = ' '//sgn(i)//' '
      k = k + n + 3
      n = len_trim(sai(i))
      st(k+1:k+n) = trim(sai(i))
      st(k+n+1:k+n+1) = 'i'
      k = k + n + 1
    end do
  end subroutine tostring_get_complex

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

  function getboxlist(unit) result(p)
    ! Return boxlist associated with specified unit. If this list does not exist a new list is started.
    integer, intent(in) :: unit
    type(boxlist), pointer :: p
    p => firstboxlist
    do while(associated(p))
      if (p%unit == unit) return
      p => p%nextboxlist
    end do
    allocate(p)
    p%nextboxlist => firstboxlist  ! put at head of list
    p%unit = unit
    firstboxlist => p
  end function getboxlist

  subroutine clearboxlist(unit)
    ! Deallocate all boxes associated with unit
    integer, intent(in) :: unit
    type(boxnode), pointer :: p, q
    type(boxlist), pointer :: blp
    blp => firstboxlist
    do while(associated(blp))
      if (blp%unit == unit) exit
      blp => blp%nextboxlist
    end do
    if (.not. associated(blp)) return
    p => blp%firstbox
    do while(associated(p))
      q => p
      p => p%nextbox
      deallocate(q%box)
      deallocate(q)
    enddo
    if (associated(firstboxlist, blp)) then
      firstboxlist => blp%nextboxlist
    endif
    deallocate(blp)
  end subroutine clearboxlist

  subroutine newbox(unit, m, n, boxp)
    ! Create a new box
    character, pointer :: boxp(:,:)
    integer, intent(in) :: unit, m, n
    type(boxnode), pointer :: p
    type(boxlist), pointer :: blp
    allocate(p)
    allocate(p%box(n, m))
    blp => getboxlist(unit)
    if (.not.associated(blp%firstbox)) then
      blp%firstbox => p
    else
      blp%lastbox%nextbox => p
    end if
    blp%lastbox => p
    boxp => p%box
    boxp = ' '
  end subroutine newbox

  function tostr(a) result(s)
    ! Copy char array to string
    character, intent(in) :: a(:)
    character(size(a)) s
    integer i
    do i=1,size(a)
      s(i:i) = a(i)
    enddo
  end function tostr

  subroutine dispboxlist(unit, sep)
    ! Display the list of boxes associated with unit
    integer, intent(in) :: unit
    type(boxnode), pointer :: pfirst, p
    type(boxlist), pointer :: blp
    integer k, nlines, h, w, ns
    character(*), intent(in) :: sep
    blp => getboxlist(unit)
    pfirst => blp%firstbox
    nlines = 0
    p => pfirst
    do while (associated(p))
      nlines = max(nlines, size(p%box, 2))
      p => p%nextbox
    enddo
    do k=1,nlines
      p => pfirst
      ns = 0
      do while (associated(p))
        h = size(p%box, 2)
        w = size(p%box, 1)
        if (k <= h) then
          select case(unit)
          case(-1)
            continue
          case(-2)
            call putstr(sep(1:ns) // tostr(p%box(:,k)))
          case(-3)
            write(*,    '(2A)', advance = 'no') sep(1:ns), tostr(p%box(:,k))
          case default
            write(unit, '(2A)', advance = 'no') sep(1:ns), tostr(p%box(:,k))
          end select
        else
          select case(unit)
          case(-1)
            continue
          case(-2)
            call putstr(sep(1:ns) // repeat(' ', w))
          case(-3)
            write(*,    '(2A)', advance = 'no') sep(1:ns), repeat(' ', w)
          case default
            write(unit, '(2A)', advance = 'no') sep(1:ns), repeat(' ', w)
          end select
        end if
        p => p%nextbox
        ns = len(sep)
      enddo
      call dispnewline(unit)
    enddo
    call clearboxlist(unit)
  end subroutine dispboxlist

  subroutine dispnewline(unit)
    integer, intent(in) :: unit
    select case(unit)
    case(-1); continue
    case(-2); call putnl
    case(-3); write(*,*)
    case default; write(unit,*)
    end select
  end subroutine dispnewline

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
