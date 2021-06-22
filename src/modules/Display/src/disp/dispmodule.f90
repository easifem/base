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

MODULE DISPMODULE
  use dispmodule_util
  implicit none

  PRIVATE  ! Everything not explicitly declared public should be private (including entities from dispmodule_util)

  ! ********************************** PUBLIC DECLARATIONS *************************************

  PUBLIC DISP                 ! Main routine of package, "pretty-prints" vectors and matrices
  PUBLIC DISP_SET             ! Subroutine to change default settings for DISP
  PUBLIC DISP_GET             ! Obtain current default settings
  PUBLIC DISP_SET_FACTORY     ! Call (without parameters) to restore original default settings
  PUBLIC TOSTRING             ! Convert numbers to strings
  PUBLIC TOSTRING_SET         ! Change settings for tostring
  PUBLIC TOSTRING_SET_FACTORY ! Restore original default settings for tostring
  !
  PUBLIC DISP_SETTINGS        ! Derived type with settings
  !
  PUBLIC ASTERISK_UNIT        ! Constant to specify displaying on asterisk unit (normally the screen)
  PUBLIC PUTSTR_UNIT          ! Constant to specify the use of subroutines putstr and putnl to display
  PUBLIC NULL_UNIT            ! Constant to specify discarding of all displayed output

  ! ********************************** INTERFACE DECLARATIONS *************************************
  interface disp_set
    module procedure disp_set, disp_set_ds
  end interface

  interface disp
    module procedure disp_s_dint, disp_ts_dint, disp_v_dint, disp_tv_dint, disp_m_dint, disp_tm_dint
    module procedure disp_s_sngl, disp_ts_sngl, disp_v_sngl, disp_tv_sngl, disp_m_sngl, disp_tm_sngl
    module procedure disp_s_dble, disp_ts_dble, disp_v_dble, disp_tv_dble, disp_m_dble, disp_tm_dble
    module procedure disp_s_cplx, disp_ts_cplx, disp_v_cplx, disp_tv_cplx, disp_m_cplx, disp_tm_cplx
    module procedure disp_s_cpld, disp_ts_cpld, disp_v_cpld, disp_tv_cpld, disp_m_cpld, disp_tm_cpld
    module procedure disp_s_dlog, disp_ts_dlog, disp_v_dlog, disp_tv_dlog, disp_m_dlog, disp_tm_dlog
    module procedure              disp_ts_dchr, disp_v_dchr, disp_tv_dchr, disp_m_dchr, disp_tm_dchr
  end interface

  interface tostring
    module procedure tostring_dint, tostring_f_dint, tostring_s_dint, tostring_sf_dint
    module procedure tostring_dlog, tostring_f_dlog, tostring_s_dlog, tostring_sf_dlog
    module procedure tostring_sngl, tostring_f_sngl, tostring_s_sngl, tostring_sf_sngl
    module procedure tostring_dble, tostring_f_dble, tostring_s_dble, tostring_sf_dble
    module procedure tostring_cplx, tostring_f_cplx, tostring_s_cplx, tostring_sf_cplx
    module procedure tostring_cpld, tostring_f_cpld, tostring_s_cpld, tostring_sf_cpld
  end interface

  ! *********************** DEFINITION OF TYPED CONSTANTS: UNITS AND KIND PARAMETERS ********************
  integer, parameter ::    &
       ASTERISK_UNIT = -3  ,&
       PUTSTR_UNIT   = -2  ,&
       NULL_UNIT     = -1

  integer, parameter :: dint = kind(0)       ! default integer
  integer, parameter :: sngl = kind(0.0)     ! single precision (default real)
  integer, parameter :: dble = kind(0d0)     ! double precision
  integer, parameter :: dlog = kind(.false.) ! default logical

  ! The above are also used as specific procedure (i.e. module procedure) name extensions, together
  ! with the following:
  !        cplx = complex single precision (default complex)
  !        cpld = complex double precision

CONTAINS

  ! ******************************* SETTING AND GETTING PROCEDURES *************************************
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

  subroutine disp_set_factory()
    ! Change display settings to the original default
    DEFSET = FACTORY_SETTINGS
  end subroutine disp_set_factory

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

  subroutine tostring_set_factory()
    logical dummy
    dummy = .false.
    if (dummy) call avoid_compiler_warnings
    tosset = tosfac
  end subroutine tostring_set_factory

  subroutine disp_set_ds(settings)
    ! Change display settings according to the structure "settings"
    type(disp_settings), intent(in) :: settings
    DEFSET = settings
    call check_settings
  end subroutine disp_set_ds

  function disp_get() result(defs)
    ! Return current display settings
    type(disp_settings) :: defs
    defs = DEFSET
  end function disp_get

  ! ********************************* DEFAULT INTEGER PROCEDURES ****************************************
  subroutine disp_s_dint(x, fmt, advance, sep, trim, unit, zeroas)
    ! Default integer scalar without title
    character(*), intent(in), optional :: fmt, advance, sep, trim, zeroas
    integer(dint), intent(in) :: x
    integer, intent(in), optional :: unit
    call disp_ts_dint('', x, fmt, advance, sep, 'left', trim, unit, zeroas)
  end subroutine disp_s_dint

  subroutine disp_v_dint(x, fmt, advance, lbound, sep, style, trim, unit, orient, zeroas)
    ! Default integer vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
    integer(dint), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:)
    call disp_tv_dint('', x, fmt, advance, lbound, sep, style, trim, unit, orient, zeroas)
  end subroutine disp_v_dint

  subroutine disp_m_dint(x, fmt, advance, lbound, sep, style, trim, unit, zeroas)
    ! Default integer matrix without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    integer(dint), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, lbound(:)
    call disp_tm_dint('', x, fmt, advance, lbound, sep, style, trim, unit, zeroas)
  end subroutine disp_m_dint

  subroutine disp_ts_dint(title, x, fmt, advance, sep, style, trim, unit, zeroas)
    ! Default integer scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    integer(dint), intent(in) :: x
    integer, intent(in), optional :: unit
    call disp_tm_dint(title, reshape((/x/), (/1, 1/)), fmt, advance, sep=sep, style=style, trim=trim, unit=unit, &
         zeroas=zeroas)
  end subroutine disp_ts_dint

  subroutine disp_tv_dint(title, x, fmt, advance, lbound, sep, style, trim, unit, orient, zeroas)
    ! Default integer vector with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
    integer(dint), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:)
    type(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient, zeroas)
    if (SE%row) then
      call disp_dint(title, reshape(x, (/1, size(x)/)), SE)
    else
      call disp_dint(title, reshape(x, (/size(x), 1/)), SE)
    end if
  end subroutine disp_tv_dint

  subroutine disp_tm_dint(title, x, fmt, advance, lbound, sep, style, trim, unit, zeroas)
    ! Default integer matrix with title
    character(*), intent(in)           :: title      ! The title to use for the matrix
    integer(dint),intent(in)           :: x(:,:)     ! The matrix to be written
    character(*), intent(in), optional :: fmt        ! Format edit descriptor to use for each matrix element (e.g.'I4')
    integer,      intent(in), optional :: unit       ! Unit to display on
    character(*), intent(in), optional :: advance    ! 'No' to print next matrix to right of current, otherewise 'Yes'
    character(*), intent(in), optional :: sep        ! Separator between matrix columns (e.g. ", ")
    character(*), intent(in), optional :: zeroas     ! Zeros are replaced by this string
    character(*), intent(in), optional :: style      ! Style(s): See NOTE 1 below
    character(*), intent(in), optional :: trim       ! 'Auto' (the default) to trim if fmt absent, 'no' for no trimming,
    !                                                ! trimming, 'yes' for trimming
    integer,      intent(in), optional :: lbound(:)  ! Lower bounds of x
    type(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, zeroas=zeroas)
    call disp_dint(title, x, SE)
  end subroutine disp_tm_dint

  subroutine disp_dint(title, x, SE)
    ! Default integer item
    character(*),   intent(in)    :: title
    integer(dint),  intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer wid(size(x,2)), nbl(size(x,2))
    call find_editdesc_dint(x, SE, wid, nbl) ! determine also SE%w
    call tobox_dint(title, x, SE, wid, nbl)
  end subroutine disp_dint

  subroutine tobox_dint(title, x, SE, wid, nbl)
    ! Write default integer matrix to box
    character(*),   intent(in)    :: title
    integer(dint),  intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer,        intent(inout) :: wid(:)
    integer,        intent(inout) :: nbl(:)
    character(SE%w)  :: s(size(x,1))
    integer            :: lin1, j, wleft, m, n, widp(size(wid))
    character, pointer :: boxp(:,:)
    m = size(x,1)
    n = size(x,2)
    call preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
    do j=1,n
      if (m > 0) write(s, SE%ed) x(:,j)
      if (SE%lzas > 0) call replace_zeronaninf(s, SE%zas(1:SE%lzas), x(:,j) == 0)
      call copytobox(s, lin1, wid(j), widp(j), nbl(j), boxp,  wleft)
      if (j<n) call copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp,  wleft)
    enddo
    call finishbox(title, SE, boxp)
  end subroutine tobox_dint

  subroutine find_editdesc_dint(x, SE, wid, nbl)
    ! Determine SE%ed, SE%w (unless specified) and wid
    integer(dint),  intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer,        intent(out)   :: wid(size(x,2)), nbl(size(x,2))
    !
    integer(dint) xmaxv(size(x,2)), xminv(size(x,2)), xp, xm
    logical xzero(size(x,2)), xallz(size(x,2))
    character(22) s
    integer ww
    !
    if (SE%w == 0) then
      xp = maxval(x)
      xm = minval(x)
      write(s, '(SS,I0)') xp; ww = len_trim(s)
      write(s, '(SS,I0)') xm; ww = max(ww, len_trim(s))
      SE%w = max(SE%lzas, ww)
      call replace_w(SE%ed, ww)
    elseif (SE%w < 0) then ! obtain max-width of x
      if (size(x) == 0) then
        SE%ed = '()'
        SE%w = 0
        wid = 0
        return
      endif
      xp = maxval(x)
      xm = minval(x)
      write(s, '(SS,I0)') xp; ww = len_trim(s)
      write(s, '(SS,I0)') xm; ww = max(ww, len_trim(s))
      ww = max(SE%lzas, ww)
      SE%ed = '(SS,Ixx)'
      write(SE%ed(6:7), '(SS,I2)') ww
      SE%w = ww
    endif
    if (SE%trm) then
      xmaxv = maxval(x, 1) ! max in each column
      xminv = minval(x, 1) ! min
      xzero = any(x == 0_dint, 1) ! true where column has some zeros
      xallz = all(x == 0_dint, 1) ! true where column has only zeros
      call getwid_dint(xmaxv, xminv, xzero, xallz, SE,  wid, nbl)
    else
      wid = SE%w
      nbl = 0
    endif
  end subroutine find_editdesc_dint

  subroutine getwid_dint(xmaxv, xminv, xzero, xallz, SE,  wid, nbl)
    integer(dint),  intent(in)  :: xmaxv(:), xminv(:)
    logical,        intent(in)  :: xzero(:), xallz(:) ! True for columns with some/all zeros
    type(settings), intent(in)  :: SE                 ! Settings
    integer,        intent(out) :: wid(:)             ! Widths of columns
    integer,        intent(out) :: nbl(:)             ! n of blanks to peel from left (w-wid)
    character(SE%w) :: stmax(size(xmaxv)), stmin(size(xmaxv))
    integer w
    w = SE%w
    write(stmax, SE%ed) xmaxv
    write(stmin, SE%ed) xminv
    nbl = mod(verify(stmin, ' ') + w, w + 1) ! loc. of first nonblank
    nbl = min(nbl, mod(verify(stmax, ' ') + w, w + 1))
    wid = w - nbl
    if (SE%lzas > 0) then
      wid = merge(SE%lzas, wid, xallz)
      wid = max(wid, merge(SE%lzas, 0, xzero))
      nbl = w - wid
    endif
  end subroutine getwid_dint

  ! ********* DEFAULT INTEGER TOSTRING PROCEDURES *********
  function tostring_s_dint(x) result(st)
    ! Scalar to string
    integer(dint), intent(in)                   :: x
    character(len_f_dint((/x/), tosset%ifmt)) :: st
    st = tostring_f_dint((/x/), tosset%ifmt)
  end function tostring_s_dint

  function tostring_sf_dint(x, fmt) result(st)
    ! Scalar with specified format to string
    integer(dint),intent(in)        :: x
    character(*), intent(in)        :: fmt
    character(len_f_dint((/x/), fmt)) :: st
    st = tostring_f_dint((/x/), fmt)
  end function tostring_sf_dint

  function tostring_dint(x) result(st)
    ! Vector to string
    integer(dint), intent(in)               :: x(:)
    character(len_f_dint(x, tosset%ifmt)) :: st
    st = tostring_f_dint(x, tosset%ifmt)
  end function tostring_dint

  function tostring_f_dint(x, fmt) result(st)
    ! Vector with specified format to string
    integer(dint), intent(in)        :: x(:)
    character(*), intent(in)         :: fmt
    character(len_f_dint(x, fmt))    :: st
    character(widthmax_dint(x, fmt)) :: sa(size(x))
    integer                          :: w, d
    logical                          :: gedit
    character(nnblk(fmt)+5)          :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; st = errormsg; return; endif
    write(sa, fmt1) x
    if (tosset%trimb == 'YES' .or. w == 0) sa = adjustl(sa)
    call tostring_get(sa, st)
  end function tostring_f_dint

  pure function len_f_dint(x, fmt) result(wtot)
    ! Total width of tostring representation of x
    integer(dint), intent(in)        :: x(:)
    character(*), intent(in)         :: fmt
    character(widthmax_dint(x, fmt)) :: sa(size(x))
    integer                          :: wtot, w, d
    logical                          :: gedit
    character(nnblk(fmt)+5)          :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; wtot = len(errormsg); return; endif
    write(sa, fmt1) x
    if (tosset%trimb == 'YES' .or. w == 0) sa = adjustl(sa)
    wtot = sum(len_trim(sa)) + (size(x) - 1)*(tosset%seplen)
  end function len_f_dint

  pure function widthmax_dint(x, fmt) result(w)
    ! Maximum width of string representation of an element in x
    integer(dint), intent(in)  :: x(:)
    character(*), intent(in) :: fmt
    character(range(x)+2) sx(2)
    integer w, d
    logical gedit
    character(nnblk(fmt)+5) :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w<=0) then
      write(sx, '(SS,I0)') maxval(x), minval(x)
      w = maxval(len_trim(sx))
    end if
  end function widthmax_dint
  ! ************************************* END OF DEFAULT INTEGER PROCEDURES ******************************************

  ! **************************************** SINGLE PRECISION PROCEDURES *********************************************
  subroutine disp_s_sngl(x, fmt, advance, digmax, sep, trim, unit, zeroas)
    ! Single precision scalar without title
    character(*), intent(in), optional :: fmt, advance, sep, trim, zeroas
    real(sngl), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_ts_sngl('', x, fmt, advance, digmax, sep, 'left', trim, unit, zeroas)
  end subroutine disp_s_sngl

  subroutine disp_v_sngl(x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
    ! Single precision vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
    real(sngl), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    call disp_tv_sngl('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
  end subroutine disp_v_sngl

  subroutine disp_m_sngl(x, fmt, advance, lbound, sep, style, trim, unit, digmax, zeroas)
    ! Single precision matrix without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    real(sngl), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, digmax, lbound(:)
    call disp_tm_sngl('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
  end subroutine disp_m_sngl

  subroutine disp_ts_sngl(title, x, fmt, advance, digmax, sep, style, trim, unit, zeroas)
    ! Single precision scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    real(sngl), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_tm_sngl(title, reshape((/x/), (/1, 1/)), fmt, advance, digmax, sep=sep, style=style, trim=trim, &
         unit=unit, zeroas=zeroas)
  end subroutine disp_ts_sngl

  subroutine disp_tv_sngl(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
    ! Single precision vector with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
    real(sngl), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    type(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient, zeroas, digmax)
    if (SE%row) then
      call disp_sngl(title, reshape(x, (/1, size(x)/)), SE)
    else
      call disp_sngl(title, reshape(x, (/size(x), 1/)), SE)
    end if
  end subroutine disp_tv_sngl

  subroutine disp_tm_sngl(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
    ! Single precision matrix with title
    character(*), intent(in)           :: title      ! The title to use for the matrix
    real(sngl),   intent(in)           :: x(:,:)     ! The matrix to be written
    character(*), intent(in), optional :: fmt        ! Editdit descriptor to use for each matrix element (e.g. 'F5.2')
    integer,      intent(in), optional :: unit       ! Unit to display on
    integer,      intent(in), optional :: digmax     ! Nbr of significant digits for largest abs value in x
    character(*), intent(in), optional :: advance    ! 'No' to print next matrix to right of current, otherewise 'Yes'
    character(*), intent(in), optional :: sep        ! Separator between matrix columns (e.g. ", ")
    character(*), intent(in), optional :: zeroas     ! Zeros are replaced with this string if it is not empty
    character(*), intent(in), optional :: style      ! Style(s): See NOTE 1 below
    character(*), intent(in), optional :: trim       ! 'Auto' (the default) to trim if fmt absent, 'no' for no
    !                                                ! trimming, 'yes' for trimming
    integer,      intent(in), optional :: lbound(:)  ! Lower bounds of x
    type(settings) :: SE
    !
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, zeroas=zeroas, digmax=digmax)
    call disp_sngl(title, x, SE)
  end subroutine disp_tm_sngl

  subroutine disp_sngl(title, x, SE)
    ! Single precision item
    character(*),   intent(in)    :: title
    real(sngl),     intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer wid(size(x,2)), nbl(size(x,2))
    call find_editdesc_sngl(x, SE, wid, nbl) ! determine also SE%w
    call tobox_sngl(title, x, SE, wid, nbl)
  end subroutine disp_sngl

  subroutine tobox_sngl(title, x, SE, wid, nbl)
    ! Write single precision matrix to box
    character(*),   intent(in)    :: title   ! title
    real(sngl),     intent(in)    :: x(:,:)  ! item
    type(settings), intent(inout) :: SE      ! settings
    integer,        intent(inout) :: wid(:)  ! widths of columns
    integer,        intent(inout) :: nbl(:)  ! number of blanks to trim from left
    character(SE%w)  :: s(size(x,1))
    integer            :: lin1, j, wleft, m, n, widp(size(wid))
    character, pointer :: boxp(:,:)
    real(sngl)         :: xj(size(x,1)), h
    m = size(x,1)
    n = size(x,2)
    h = huge(x)
    call preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
    do j=1,n
      xj = x(:, j)
      if (m > 0) write(s, SE%ed) xj
      call replace_zeronaninf(s, SE%zas(1:SE%lzas), xj == 0, xj /= xj, xj < -h, xj > h)
      call copytobox(s, lin1, wid(j), widp(j), nbl(j), boxp,  wleft)
      if (j<n) call copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp,  wleft)
    enddo
    call finishbox(title, SE, boxp)
  end subroutine tobox_sngl

  pure function maxw_sngl(x, d) result(w)
    ! Find max field width needed (F0.d editing is specified)
    real(sngl), intent(in) :: x(:)
    integer, intent(in) :: d
    integer expmax, expmin, w
    logical xfinite(size(x))
    real(sngl) xmax, xmin, h
    character(12) :: f1, s(2)
    xmin = 0; xmax = 0; h = huge(h)
    xfinite = x == x .and. x >= -h .and. x <= h ! neither NaN, Inf nor -Inf
    if (.not. any(xfinite)) then
      w = 4
    else
      xmax = maxval(x, mask=xfinite)
      xmin = minval(x, mask=xfinite)
      f1 = '(SS,ES9.0E4)'
      write(s,f1) xmax, xmin
      read(s(:)(5:9),'(I5)') expmax, expmin
      w = max(0, expmax, expmin) + d + 4
    end if
    if (.not. all(xfinite)) w = max(w, 4)
  end function maxw_sngl

  subroutine find_editdesc_sngl(x, SE, wid, nbl)
    ! Determine SE%ed, SE%w (unless specified) and wid.
    ! The if-block (*) is for safety: make f wider in case xm is written ok with the
    ! ES format in fmt but overflows with F format (the feature has been tested through
    ! manual changes to the program).
    real(sngl),     intent(in)    :: x(:,:)         ! Item to be written
    type(settings), intent(inout) :: SE             ! Settings
    integer,        intent(out)   :: wid(size(x,2)) ! Widths of individual columns
    integer,        intent(out)   :: nbl(size(x,2)) ! Blanks to trim from left of individual columns
    integer :: expmax, expmin, ww, dd, dmx
    real(sngl) xmaxv(size(x,2)), xminv(size(x,2)), xp, xm, h
    character(14) :: f1 = '(SS,ESxx.xxE4)'  ! could be ES99.89E4; default is ES14.05E4
    character(99) s
    logical xzero(size(x,2)), xallz(size(x,2)), xfinite(size(x,1),size(x,2)), xnonn(size(x,2)), xalln(size(x,2))
    !
    dmx = SE%dmx
    h = huge(h)
    xfinite = x == x .and. x >= -h .and. x <= h ! neither NaN, Inf nor -Inf
    if (SE%w == 0) then  ! Edit descriptor 'F0.d' specified
      ww = maxw_sngl(reshape(x, (/size(x)/)), SE%d)
      if (SE%lzas > 0 .and. any(x == 0._sngl))  ww = max(ww, SE%lzas)
      call replace_w(SE%ed, ww)
      SE%w = ww
    elseif (SE%w < 0) then ! No edit descriptor specified
      if (size(x) == 0) then
        SE%w = 0
        wid = 0
        nbl = 0
        return
      endif
      if (any(xfinite)) then
        xp = maxval(x, mask=xfinite)
        xm = minval(x, mask=xfinite)
        write(f1(7:11), '(SS,I2,".",I2.2)') dmx + 8, dmx - 1
        write(s,f1) xp; read(s(dmx+4:dmx+8),'(I5)') expmax
        write(s,f1) xm; read(s(dmx+4:dmx+8),'(I5)') expmin
        call find_editdesc_real(expmax, expmin, dmx,  SE%ed, ww, dd, xm >= 0)
        if (.not. all(xfinite))                     ww = max(ww, 4)
        if (SE%lzas > 0 .and. any(x == 0._sngl))  ww = max(ww, SE%lzas)
        if (SE%ed(5:5)=='F') then  ! (*)
          write(s, SE%ed) xp; if (s(1:1) == '*') ww = ww + 1
          write(s, SE%ed) xm; if (s(1:1) == '*') ww = ww + 1
          write(SE%ed(6:10), '(SS,I2,".",I2)') ww, dd
        endif
      else
        ww = 4
        SE%ed = '(F4.0)'
      endif
      SE%w = ww
    endif
    if (SE%trm) then
      xmaxv = maxval(x, 1, mask=xfinite)  ! max in each column
      xminv = minval(x, 1, mask=xfinite)  ! min
      xzero = any(x == 0._sngl, 1) ! true where column has some zeros
      xallz = all(x == 0._sngl, 1) ! true where column has only zeros
      xnonn = any(x > h .or. x < -h .or. x /= x, 1)  ! true where column has some nonnormals (inf, -inf, nan)
      xalln = all(x > h .or. x < -h .or. x /= x, 1)  ! true where column has only nonnormals (inf, -inf, nan)
      call getwid_sngl(xmaxv, xminv, xzero, xallz, xnonn, xalln, SE,  wid, nbl)
    else
      wid = SE%w
      nbl = 0
    endif
  end subroutine find_editdesc_sngl

  subroutine getwid_sngl(xmaxv, xminv, xzero, xallz, xnonn, xalln, SE,  wid, nbl)
    ! determine length of the strings that result when writing with edit descriptor SE%ed a
    ! vector v where v(i) is xmaxv(i) or xminv(i) depending on which gives longer output
    real(sngl),     intent(in)  :: xmaxv(:), xminv(:) ! max and min values in each column
    logical,        intent(in)  :: xzero(:), xallz(:) ! true for columns with some/all zeros
    logical,        intent(in)  :: xnonn(:), xalln(:) ! true for columns with some/all nonnormals
    type(settings), intent(in)  :: SE                 ! settings
    integer,        intent(out) :: wid(:)             ! widths of columns
    integer,        intent(out) :: nbl(:)             ! number of blanks to peel from left (w-wid)
    character(SE%w) :: stmax(size(xmaxv)), stmin(size(xmaxv))
    integer w
    w = SE%w
    write(stmin, SE%ed) xminv
    write(stmax, SE%ed) xmaxv
    nbl = mod(verify(stmin, ' ') + w, w + 1) ! loc. of first nonblank
    nbl = min(nbl, mod(verify(stmax, ' ') + w, w + 1))
    if (SE%gedit) then
      wid = w
    else
      wid = len_trim(adjustl(stmin))
      wid = max(wid, len_trim(adjustl(stmax)))
    endif
    if (SE%lzas > 0) then
      wid = merge(SE%lzas, wid, xallz)
      wid = max(wid, merge(SE%lzas, 0, xzero))
    endif
    wid = merge(4, wid, xalln)
    wid = max(wid, merge(4, 0, xnonn))
    nbl = w - wid
  end subroutine getwid_sngl

  ! ******** TOSTRING SINGLE PRECISION PROCEDURES ***********
  function tostring_s_sngl(x) result(st)
    ! Scalar to string
    real(sngl), intent(in) :: x
    character(len_f_sngl((/x/), tosset%rfmt)) :: st
    st = tostring_f_sngl((/x/), tosset%rfmt)
  end function tostring_s_sngl

  function tostring_sf_sngl(x, fmt) result(st)
    ! Scalar with specified format to string
    real(sngl),   intent(in) :: x
    character(*), intent(in) :: fmt
    character(len_f_sngl((/x/), fmt)) :: st
    st = tostring_f_sngl((/x/), fmt)
  end function tostring_sf_sngl

  function tostring_sngl(x) result(st)
    ! Vector to string
    real(sngl), intent(in) :: x(:)
    character(len_f_sngl(x, tosset%rfmt)) :: st
    st = tostring_f_sngl(x, tosset%rfmt)
  end function tostring_sngl

  function tostring_f_sngl(x, fmt) result(st)
    ! Vector with specified format to string
    real(sngl)    ,       intent(in) :: x(:)
    character(*),         intent(in) :: fmt
    character(len_f_sngl(x, fmt))    :: st
    character(widthmax_sngl(x, fmt)) :: sa(size(x))
    character(nnblk(fmt)+8)          :: fmt1  !(5 for readfmt and 3 for replace_w)
    integer                          :: w, d, ww
    logical                          :: gedit
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then
      st = errormsg
      return
    elseif (w == 0) then
      ww = maxw_sngl(x, d)
      call replace_w(fmt1, ww)
    endif
    write(sa, fmt1) x
    call trim_real(sa, gedit, w)
    call tostring_get(sa, st)
  end function tostring_f_sngl

  pure function len_f_sngl(x, fmt) result(wtot)
    ! Total length of returned string, vector s
    real(sngl), intent(in)           :: x(:)
    character(*), intent(in)         :: fmt
    character(widthmax_sngl(x, fmt)) :: sa(size(x))
    integer                          :: wtot, w, d, ww
    logical                          :: gedit
    character(nnblk(fmt)+8)          :: fmt1  !(5 for readfmt and 3 for replace_w)
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; wtot = len(errormsg); return; endif
    if (w == 0) then
      ww = maxw_sngl(x, d)
      call replace_w(fmt1, ww)
    endif
    write(sa, fmt1) x
    call trim_real(sa, gedit, w)
    wtot = sum(len_trim(sa)) + (size(x) - 1)*(tosset%seplen)
  end function len_f_sngl

  pure function widthmax_sngl(x, fmt) result(w)
    ! Maximum width of an element of x
    real(sngl), intent(in)   :: x(:)
    character(*), intent(in) :: fmt
    character(nnblk(fmt)+5)  :: fmt1
    integer w, d
    logical gedit
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then ! illegal format, use 1
      w = 1
    elseif (w == 0) then
      w = maxw_sngl(x, d)
    endif
  end function widthmax_sngl

  ! *************************************** END OF SINGLE PRECISION PROCEDURES ***************************************

  ! *************************************** SINGLE PRECISION COMPLEX PROCEDURES **************************************
  subroutine disp_s_cplx(x, fmt, fmt_imag, advance, digmax, sep, trim, unit)
    ! single precision complex scalar without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, trim
    complex(sngl), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_ts_cplx('', x, fmt, fmt_imag, advance, digmax, sep, 'left', trim, unit)
  end subroutine disp_s_cplx

  subroutine disp_v_cplx(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
    ! single precision complex vector without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim, orient
    complex(sngl), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    call disp_tv_cplx('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
  end subroutine disp_v_cplx

  subroutine disp_m_cplx(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
    ! single precision complex matrix without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim
    complex(sngl), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, digmax, lbound(:)
    call disp_tm_cplx('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
  end subroutine disp_m_cplx

  subroutine disp_ts_cplx(title, x, fmt, fmt_imag, advance, digmax, sep, style, trim, unit)
    ! single precision complex scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim
    complex(sngl), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_tm_cplx(title, reshape((/x/), (/1, 1/)), fmt, fmt_imag, advance, digmax, sep=sep, style=style, &
                                                       trim=trim, unit=unit)
  end subroutine disp_ts_cplx

  subroutine disp_tv_cplx(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
    ! single precision complex vector with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim, orient
    complex(sngl), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    type(settings) SE, SEim
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient, digmax=digmax)
    if (present(fmt_imag)) then
      if (.not.present(fmt)) then
        call disp_errmsg('DISP: error, FMT must be present if FMT_IMAG is present'); return;
      endif
      call get_SE(SEim, title, shape(x), fmt_imag)
    else
      SEim = SE
    end if
    if (SE%row) then
      call disp_cplx(title, reshape(x, (/1, size(x)/)), SE, SEim, n = size(x))
    else
      call disp_cplx(title, reshape(x, (/size(x), 1/)), SE, SEim, n = 1)
    end if
  end subroutine disp_tv_cplx

  subroutine disp_tm_cplx(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
    ! single precision complex matrix with title
    character(*), intent(in)           :: title      ! The title to use for the matrix
    complex(sngl),  intent(in)         :: x(:,:)     ! The matrix to be written
    character(*), intent(in), optional :: fmt        ! Edit descriptor for each element (real element when fmt_imag &
    !                                                ! is present)
    character(*), intent(in), optional :: fmt_imag   ! Edit descriptor for each imaginary element
    integer,      intent(in), optional :: unit       ! Unit to display on
    integer,      intent(in), optional :: digmax     ! Nbr of significant digits for largest abs value in real(x) &
    !                                                ! and aimag(x)
    character(*), intent(in), optional :: advance    ! 'No' to print next matrix to right of current, otherewise 'Yes'
    character(*), intent(in), optional :: sep        ! Separator between matrix columns (e.g. ", ")
    character(*), intent(in), optional :: style      ! Style(s): See NOTE 1 below
    character(*), intent(in), optional :: trim       ! 'Auto' (the default) to trim if fmt absent, 'no' for no
    !                                                ! trimming, 'yes' for trimming
    integer,      intent(in), optional :: lbound(:)  ! Lower bounds of x
    !
    type(settings) :: SE, SEim
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, digmax=digmax)
    if (present(fmt_imag)) then
      if (.not.present(fmt)) then
        call disp_errmsg('DISP: error, FMT must be present if FMT_IMAG is present'); return
      endif
      call get_SE(SEim, title, shape(x), fmt_imag)
    else
      SEim = SE
    end if
    call disp_cplx(title, x, SE, SEim, n = size(x,2))
  end subroutine disp_tm_cplx

  subroutine disp_cplx(title, x, SE, SEim, n)
    ! Single precision item
    character(*),   intent(in)    :: title
    complex(sngl),  intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE, SEim
    integer,        intent(in)    :: n
    integer, dimension(n) :: widre(n), widim(n), nblre(n), nblim(n)
    call find_editdesc_sngl(real(x), SE, widre, nblre)         ! determine also SE%w
    call find_editdesc_sngl(abs(aimag(x)), SEim, widim, nblim) ! determine also SEim%w
    call tobox_cplx(title, x, SE, SEim, widre, widim, nblre, nblim, m = size(x,1), n = size(x,2))
  end subroutine disp_cplx

  subroutine tobox_cplx(title, x, SE, SEim, widre, widim, nblre, nblim, m, n)
    ! Write single precision complex matrix to box
    character(*),   intent(in)    :: title
    complex(sngl),  intent(in)    :: x(:,:)
    integer,        intent(in)    :: m, n, widre(:), widim(:), nblre(:), nblim(:)
    type(settings), intent(inout) :: SE, SEim
    character(SE%w)   :: s(m)
    character(SEim%w) :: sim(m)
    character(3)        :: sgn(m)
    integer             :: lin1, i, j, wleft, wid(n), widp(n)
    character, pointer  :: boxp(:,:)
    SE%zas = ''
    SEim%zas = ''
    wid = widre + widim + 4
    call preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
    do j=1,n
      if (m > 0) write(s, SE%ed) (real(x(i,j)), i=1,m)
      call copytobox(s, lin1, widre(j), widp(j) - widim(j) - 4, nblre(j), boxp,  wleft)
      do i=1,m
        if (aimag(x(i,j)) < 0) then; sgn(i) = ' - '; else; sgn(i) = ' + '; endif
        enddo
      call copytobox(sgn, lin1, 3, 3, 0, boxp,  wleft)
      if (m > 0) write(sim, SEim%ed) (abs(aimag(x(i,j))), i=1,m)
      call copytobox(sim, lin1, widim(j), widim(j), nblim(j), boxp,  wleft)
      call copyseptobox('i', m, lin1, boxp, wleft)
      if (j<n) call copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp,  wleft)
    enddo
    call finishbox(title, SE, boxp)
  end subroutine tobox_cplx

  ! ******* TOSTRING SINGLE PRECISION COMPLEX PROCEDURES ********

  function tostring_s_cplx(x) result(st)
    complex(sngl), intent(in)                   :: x
    character(len_s_cplx(x, tosset%rfmt)) :: st
    st = tostring_f_cplx((/x/), tosset%rfmt)
  end function tostring_s_cplx

  function tostring_sf_cplx(x, fmt) result(st)
    complex(sngl),  intent(in)        :: x
    character(*), intent(in)          :: fmt
    character(len_s_cplx(x, fmt)) :: st
    st = tostring_f_cplx((/x/), fmt)
  end function tostring_sf_cplx

  function tostring_cplx(x) result(st)
    complex(sngl), intent(in)               :: x(:)
    character(len_f_cplx(x, tosset%rfmt)) :: st
    st = tostring_f_cplx(x, tosset%rfmt)
  end function tostring_cplx

  function tostring_f_cplx(x, fmt) result(st)
    complex(sngl),  intent(in)                    :: x(:)
    character(*),   intent(in)                    :: fmt
    character(len_f_cplx(x, fmt))                 :: st
    character(widthmax_sngl(real(x), fmt))        :: sar(size(x))
    character(widthmax_sngl(abs(x-real(x)), fmt)) :: sai(size(x))  ! x-real(x) instead of aimag(x) to enable the fnction
    character(1)                                  :: sgn(size(x))  ! to pass -stand:f95 switch of the ifort compiler.
    integer                                       :: w, d, wr, wi, i
    logical                                       :: gedit
    character(nnblk(fmt)+8)                       :: fmt1  !(5 for readfmt and 3 for replace_w)
    real(sngl)                                    :: xre(size(x)), xim(size(x)), h
    call readfmt(fmt, fmt1, w, d, gedit)
    xre = real(x)
    xim = aimag(x)
    h = huge(h)
    if (w < 0) then
      st = errormsg
      return
    elseif (w == 0) then
      wr = maxw_sngl(xre, d)
      wi = maxw_sngl(xim, d)
      call replace_w(fmt1, max(wr, wi))
    endif
    write(sar, fmt1) real(x)
    write(sai, fmt1) abs(aimag(x))
    call trim_real(sar, gedit, w)
    call trim_real(sai, gedit, w)
    do i = 1,size(x); if (aimag(x(i)) < 0) then; sgn(i) = '-'; else; sgn(i) = '+'; endif; enddo
    call tostring_get_complex(sar, sgn, sai, st)
  end function tostring_f_cplx

  pure function len_s_cplx(x, fmt) result(wtot)
    complex(sngl), intent(in) :: x
    character(*), intent(in)  :: fmt
    integer                   :: wtot, w, d
    logical                   :: gedit
    character(nnblk(fmt)+8)   :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; wtot = len(errormsg); return; endif
    wtot = len_f_sngl((/real(x)/), fmt) + len_f_sngl((/abs(aimag(x))/), fmt) + 4
  end function len_s_cplx

  pure function len_f_cplx(x, fmt) result(wtot)
    complex(sngl), intent(in) :: x(:)
    character(*), intent(in)  :: fmt
    integer                   :: wtot, w, d
    logical                   :: gedit
    character(nnblk(fmt)+8)   :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; wtot = len(errormsg); return; endif
    wtot = len_f_sngl(real(x), fmt) + len_f_sngl(abs(aimag(x)), fmt) + size(x)*4 - (size(x) - 1)*(tosset%seplen)
    ! subtract seplen because it has been added twice in len_f_sngl
  end function len_f_cplx
  ! *************************************** END OF SINGLE PRECISION COMPLEX PROCEDURES ********************************

  ! ************************************* DOUBLE PRECISION PROCEDURES (SEE NOTE 2 BELOW) ******************************
  subroutine disp_s_dble(x, fmt, advance, digmax, sep, trim, unit, zeroas)
    ! Double precision scalar without title
    character(*), intent(in), optional :: fmt, advance, sep, trim, zeroas
    real(dble), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_ts_dble('', x, fmt, advance, digmax, sep, 'left', trim, unit, zeroas)
  end subroutine disp_s_dble

  subroutine disp_v_dble(x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
    ! Double precision vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
    real(dble), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    call disp_tv_dble('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
  end subroutine disp_v_dble

  subroutine disp_m_dble(x, fmt, advance, lbound, sep, style, trim, unit, digmax, zeroas)
    ! Double precision matrix without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    real(dble), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, digmax, lbound(:)
    call disp_tm_dble('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
  end subroutine disp_m_dble

  subroutine disp_ts_dble(title, x, fmt, advance, digmax, sep, style, trim, unit, zeroas)
    ! Double precision scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    real(dble), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_tm_dble(title, reshape((/x/), (/1, 1/)), fmt, advance, digmax, sep=sep, style=style, trim=trim, &
         unit=unit, zeroas=zeroas)
  end subroutine disp_ts_dble

  subroutine disp_tv_dble(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
    ! Double precision vector with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
    real(dble), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    type(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient, zeroas, digmax)
    if (SE%row) then
      call disp_dble(title, reshape(x, (/1, size(x)/)), SE)
    else
      call disp_dble(title, reshape(x, (/size(x), 1/)), SE)
    end if
  end subroutine disp_tv_dble

  subroutine disp_tm_dble(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
    ! Double precision matrix with title
    character(*), intent(in)           :: title      ! The title to use for the matrix
    real(dble),   intent(in)           :: x(:,:)     ! The matrix to be written
    character(*), intent(in), optional :: fmt        ! Editdit descriptor to use for each matrix element (e.g. 'F5.2')
    integer,      intent(in), optional :: unit       ! Unit to display on
    integer,      intent(in), optional :: digmax     ! Nbr of significant digits for largest abs value in x
    character(*), intent(in), optional :: advance    ! 'No' to print next matrix to right of current, otherewise 'Yes'
    character(*), intent(in), optional :: sep        ! Separator between matrix columns (e.g. ", ")
    character(*), intent(in), optional :: zeroas     ! Zeros are replaced with this string if it is not empty
    character(*), intent(in), optional :: style      ! Style(s): See NOTE 1 below
    character(*), intent(in), optional :: trim       ! 'Auto' (the default) to trim if fmt absent, 'no' for no
    !                                                ! trimming, 'yes' for trimming
    integer,      intent(in), optional :: lbound(:)  ! Lower bounds of x
    type(settings) :: SE
    !
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, zeroas=zeroas, digmax=digmax)
    call disp_dble(title, x, SE)
  end subroutine disp_tm_dble

  subroutine disp_dble(title, x, SE)
    ! Double precision item
    character(*),   intent(in)    :: title
    real(dble),     intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer wid(size(x,2)), nbl(size(x,2))
    call find_editdesc_dble(x, SE, wid, nbl) ! determine also SE%w
    call tobox_dble(title, x, SE, wid, nbl)
  end subroutine disp_dble

  subroutine tobox_dble(title, x, SE, wid, nbl)
    ! Write double precision matrix to box
    character(*),   intent(in)    :: title   ! title
    real(dble),     intent(in)    :: x(:,:)  ! item
    type(settings), intent(inout) :: SE      ! settings
    integer,        intent(inout) :: wid(:)  ! widths of columns
    integer,        intent(inout) :: nbl(:)  ! number of blanks to trim from left
    character(SE%w)  :: s(size(x,1))
    integer            :: lin1, j, wleft, m, n, widp(size(wid))
    character, pointer :: boxp(:,:)
    real(dble)         :: xj(size(x,1)), h
    m = size(x,1)
    n = size(x,2)
    h = huge(x)
    call preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
    do j=1,n
      xj = x(:, j)
      if (m > 0) write(s, SE%ed) xj
      call replace_zeronaninf(s, SE%zas(1:SE%lzas), xj == 0, xj /= xj, xj < -h, xj > h)
      call copytobox(s, lin1, wid(j), widp(j), nbl(j), boxp,  wleft)
      if (j<n) call copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp,  wleft)
    enddo
    call finishbox(title, SE, boxp)
  end subroutine tobox_dble

  pure function maxw_dble(x, d) result(w)
    ! Find max field width needed (F0.d editing is specified)
    real(dble), intent(in) :: x(:)
    integer, intent(in) :: d
    integer expmax, expmin, w
    logical xfinite(size(x))
    real(dble) xmax, xmin, h
    character(12) :: f1, s(2)
    xmin = 0; xmax = 0; h = huge(h)
    xfinite = x == x .and. x >= -h .and. x <= h ! neither NaN, Inf nor -Inf
    if (.not. any(xfinite)) then
      w = 4
    else
      xmax = maxval(x, mask=xfinite)
      xmin = minval(x, mask=xfinite)
      f1 = '(SS,ES9.0E4)'
      write(s,f1) xmax, xmin
      read(s(:)(5:9),'(I5)') expmax, expmin
      w = max(0, expmax, expmin) + d + 4
    end if
    if (.not. all(xfinite)) w = max(w, 4)
  end function maxw_dble

  subroutine find_editdesc_dble(x, SE, wid, nbl)
    ! Determine SE%ed, SE%w (unless specified) and wid.
    ! The if-block (*) is for safety: make f wider in case xm is written ok with the
    ! ES format in fmt but overflows with F format (the feature has been tested through
    ! manual changes to the program).
    real(dble),     intent(in)    :: x(:,:)         ! Item to be written
    type(settings), intent(inout) :: SE             ! Settings
    integer,        intent(out)   :: wid(size(x,2)) ! Widths of individual columns
    integer,        intent(out)   :: nbl(size(x,2)) ! Blanks to trim from left of individual columns
    integer :: expmax, expmin, ww, dd, dmx
    real(dble) xmaxv(size(x,2)), xminv(size(x,2)), xp, xm, h
    character(14) :: f1 = '(SS,ESxx.xxE4)'  ! could be ES99.89E4; default is ES14.05E4
    character(99) s
    logical xzero(size(x,2)), xallz(size(x,2)), xfinite(size(x,1),size(x,2)), xnonn(size(x,2)), xalln(size(x,2))
    !
    dmx = SE%dmx
    h = huge(h)
    xfinite = x == x .and. x >= -h .and. x <= h ! neither NaN, Inf nor -Inf
    if (SE%w == 0) then  ! Edit descriptor 'F0.d' specified
      ww = maxw_dble(reshape(x, (/size(x)/)), SE%d)
      if (SE%lzas > 0 .and. any(x == 0._dble))  ww = max(ww, SE%lzas)
      call replace_w(SE%ed, ww)
      SE%w = ww
    elseif (SE%w < 0) then ! No edit descriptor specified
      if (size(x) == 0) then
        SE%w = 0
        wid = 0
        nbl = 0
        return
      endif
      if (any(xfinite)) then
        xp = maxval(x, mask=xfinite)
        xm = minval(x, mask=xfinite)
        write(f1(7:11), '(SS,I2,".",I2.2)') dmx + 8, dmx - 1
        write(s,f1) xp; read(s(dmx+4:dmx+8),'(I5)') expmax
        write(s,f1) xm; read(s(dmx+4:dmx+8),'(I5)') expmin
        call find_editdesc_real(expmax, expmin, dmx,  SE%ed, ww, dd, xm >= 0)
        if (.not. all(xfinite))                     ww = max(ww, 4)
        if (SE%lzas > 0 .and. any(x == 0._dble))  ww = max(ww, SE%lzas)
        if (SE%ed(5:5)=='F') then  ! (*)
          write(s, SE%ed) xp; if (s(1:1) == '*') ww = ww + 1
          write(s, SE%ed) xm; if (s(1:1) == '*') ww = ww + 1
          write(SE%ed(6:10), '(SS,I2,".",I2)') ww, dd
        endif
      else
        ww = 4
        SE%ed = '(F4.0)'
      endif
      SE%w = ww
    endif
    if (SE%trm) then
      xmaxv = maxval(x, 1, mask=xfinite)  ! max in each column
      xminv = minval(x, 1, mask=xfinite)  ! min
      xzero = any(x == 0._dble, 1) ! true where column has some zeros
      xallz = all(x == 0._dble, 1) ! true where column has only zeros
      xnonn = any(x > h .or. x < -h .or. x /= x, 1)  ! true where column has some nonnormals (inf, -inf, nan)
      xalln = all(x > h .or. x < -h .or. x /= x, 1)  ! true where column has only nonnormals (inf, -inf, nan)
      call getwid_dble(xmaxv, xminv, xzero, xallz, xnonn, xalln, SE,  wid, nbl)
    else
      wid = SE%w
      nbl = 0
    endif
  end subroutine find_editdesc_dble

  subroutine getwid_dble(xmaxv, xminv, xzero, xallz, xnonn, xalln, SE,  wid, nbl)
    ! determine length of the strings that result when writing with edit descriptor SE%ed a
    ! vector v where v(i) is xmaxv(i) or xminv(i) depending on which gives longer output
    real(dble),     intent(in)  :: xmaxv(:), xminv(:) ! max and min values in each column
    logical,        intent(in)  :: xzero(:), xallz(:) ! true for columns with some/all zeros
    logical,        intent(in)  :: xnonn(:), xalln(:) ! true for columns with some/all nonnormals
    type(settings), intent(in)  :: SE                 ! settings
    integer,        intent(out) :: wid(:)             ! widths of columns
    integer,        intent(out) :: nbl(:)             ! number of blanks to peel from left (w-wid)
    character(SE%w) :: stmax(size(xmaxv)), stmin(size(xmaxv))
    integer w
    w = SE%w
    write(stmin, SE%ed) xminv
    write(stmax, SE%ed) xmaxv
    nbl = mod(verify(stmin, ' ') + w, w + 1) ! loc. of first nonblank
    nbl = min(nbl, mod(verify(stmax, ' ') + w, w + 1))
    if (SE%gedit) then
      wid = w
    else
      wid = len_trim(adjustl(stmin))
      wid = max(wid, len_trim(adjustl(stmax)))
    endif
    if (SE%lzas > 0) then
      wid = merge(SE%lzas, wid, xallz)
      wid = max(wid, merge(SE%lzas, 0, xzero))
    endif
    wid = merge(4, wid, xalln)
    wid = max(wid, merge(4, 0, xnonn))
    nbl = w - wid
  end subroutine getwid_dble

  ! ******** TOSTRING DOUBLE PRECISION PROCEDURES ***********
  function tostring_s_dble(x) result(st)
    ! Scalar to string
    real(dble), intent(in) :: x
    character(len_f_dble((/x/), tosset%rfmt)) :: st
    st = tostring_f_dble((/x/), tosset%rfmt)
  end function tostring_s_dble

  function tostring_sf_dble(x, fmt) result(st)
    ! Scalar with specified format to string
    real(dble),   intent(in) :: x
    character(*), intent(in) :: fmt
    character(len_f_dble((/x/), fmt)) :: st
    st = tostring_f_dble((/x/), fmt)
  end function tostring_sf_dble

  function tostring_dble(x) result(st)
    ! Vector to string
    real(dble), intent(in) :: x(:)
    character(len_f_dble(x, tosset%rfmt)) :: st
    st = tostring_f_dble(x, tosset%rfmt)
  end function tostring_dble

  function tostring_f_dble(x, fmt) result(st)
    ! Vector with specified format to string
    real(dble)    ,       intent(in) :: x(:)
    character(*),         intent(in) :: fmt
    character(len_f_dble(x, fmt))    :: st
    character(widthmax_dble(x, fmt)) :: sa(size(x))
    character(nnblk(fmt)+8)          :: fmt1  !(5 for readfmt and 3 for replace_w)
    integer                          :: w, d, ww
    logical                          :: gedit
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then
      st = errormsg
      return
    elseif (w == 0) then
      ww = maxw_dble(x, d)
      call replace_w(fmt1, ww)
    endif
    write(sa, fmt1) x
    call trim_real(sa, gedit, w)
    call tostring_get(sa, st)
  end function tostring_f_dble

  pure function len_f_dble(x, fmt) result(wtot)
    ! Total length of returned string, vector s
    real(dble), intent(in)           :: x(:)
    character(*), intent(in)         :: fmt
    character(widthmax_dble(x, fmt)) :: sa(size(x))
    integer                          :: wtot, w, d, ww
    logical                          :: gedit
    character(nnblk(fmt)+8)          :: fmt1  !(5 for readfmt and 3 for replace_w)
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; wtot = len(errormsg); return; endif
    if (w == 0) then
      ww = maxw_dble(x, d)
      call replace_w(fmt1, ww)
    endif
    write(sa, fmt1) x
    call trim_real(sa, gedit, w)
    wtot = sum(len_trim(sa)) + (size(x) - 1)*(tosset%seplen)
  end function len_f_dble

  pure function widthmax_dble(x, fmt) result(w)
    ! Maximum width of an element of x
    real(dble), intent(in)   :: x(:)
    character(*), intent(in) :: fmt
    character(nnblk(fmt)+5)  :: fmt1
    integer w, d
    logical gedit
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then ! illegal format, use 1
      w = 1
    elseif (w == 0) then
      w = maxw_dble(x, d)
    endif
  end function widthmax_dble

  ! *************************************** END OF DOUBLE PRECISION PROCEDURES ***************************************

  ! *************************************** DOUBLE PRECISION COMPLEX PROCEDURES **************************************
  subroutine disp_s_cpld(x, fmt, fmt_imag, advance, digmax, sep, trim, unit)
    ! double precision complex scalar without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, trim
    complex(dble), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_ts_cpld('', x, fmt, fmt_imag, advance, digmax, sep, 'left', trim, unit)
  end subroutine disp_s_cpld

  subroutine disp_v_cpld(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
    ! double precision complex vector without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim, orient
    complex(dble), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    call disp_tv_cpld('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
  end subroutine disp_v_cpld

  subroutine disp_m_cpld(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
    ! double precision complex matrix without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim
    complex(dble), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, digmax, lbound(:)
    call disp_tm_cpld('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
  end subroutine disp_m_cpld

  subroutine disp_ts_cpld(title, x, fmt, fmt_imag, advance, digmax, sep, style, trim, unit)
    ! double precision complex scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim
    complex(dble), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_tm_cpld(title, reshape((/x/), (/1, 1/)), fmt, fmt_imag, advance, digmax, sep=sep, style=style, &
                                                       trim=trim, unit=unit)
  end subroutine disp_ts_cpld

  subroutine disp_tv_cpld(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
    ! double precision complex vector with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim, orient
    complex(dble), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    type(settings) SE, SEim
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient, digmax=digmax)
    if (present(fmt_imag)) then
      if (.not.present(fmt)) then
        call disp_errmsg('DISP: error, FMT must be present if FMT_IMAG is present'); return;
      endif
      call get_SE(SEim, title, shape(x), fmt_imag)
    else
      SEim = SE
    end if
    if (SE%row) then
      call disp_cpld(title, reshape(x, (/1, size(x)/)), SE, SEim, n = size(x))
    else
      call disp_cpld(title, reshape(x, (/size(x), 1/)), SE, SEim, n = 1)
    end if
  end subroutine disp_tv_cpld

  subroutine disp_tm_cpld(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
    ! double precision complex matrix with title
    character(*), intent(in)           :: title      ! The title to use for the matrix
    complex(dble),  intent(in)         :: x(:,:)     ! The matrix to be written
    character(*), intent(in), optional :: fmt        ! Edit descriptor for each element (real element when fmt_imag &
    !                                                ! is present)
    character(*), intent(in), optional :: fmt_imag   ! Edit descriptor for each imaginary element
    integer,      intent(in), optional :: unit       ! Unit to display on
    integer,      intent(in), optional :: digmax     ! Nbr of significant digits for largest abs value in real(x) &
    !                                                ! and aimag(x)
    character(*), intent(in), optional :: advance    ! 'No' to print next matrix to right of current, otherewise 'Yes'
    character(*), intent(in), optional :: sep        ! Separator between matrix columns (e.g. ", ")
    character(*), intent(in), optional :: style      ! Style(s): See NOTE 1 below
    character(*), intent(in), optional :: trim       ! 'Auto' (the default) to trim if fmt absent, 'no' for no
    !                                                ! trimming, 'yes' for trimming
    integer,      intent(in), optional :: lbound(:)  ! Lower bounds of x
    !
    type(settings) :: SE, SEim
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, digmax=digmax)
    if (present(fmt_imag)) then
      if (.not.present(fmt)) then
        call disp_errmsg('DISP: error, FMT must be present if FMT_IMAG is present'); return
      endif
      call get_SE(SEim, title, shape(x), fmt_imag)
    else
      SEim = SE
    end if
    call disp_cpld(title, x, SE, SEim, n = size(x,2))
  end subroutine disp_tm_cpld

  subroutine disp_cpld(title, x, SE, SEim, n)
    ! Double precision item
    character(*),   intent(in)    :: title
    complex(dble),  intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE, SEim
    integer,        intent(in)    :: n
    integer, dimension(n) :: widre(n), widim(n), nblre(n), nblim(n)
    call find_editdesc_dble(real(x), SE, widre, nblre)         ! determine also SE%w
    call find_editdesc_dble(abs(aimag(x)), SEim, widim, nblim) ! determine also SEim%w
    call tobox_cpld(title, x, SE, SEim, widre, widim, nblre, nblim, m = size(x,1), n = size(x,2))
  end subroutine disp_cpld

  subroutine tobox_cpld(title, x, SE, SEim, widre, widim, nblre, nblim, m, n)
    ! Write double precision complex matrix to box
    character(*),   intent(in)    :: title
    complex(dble),  intent(in)    :: x(:,:)
    integer,        intent(in)    :: m, n, widre(:), widim(:), nblre(:), nblim(:)
    type(settings), intent(inout) :: SE, SEim
    character(SE%w)   :: s(m)
    character(SEim%w) :: sim(m)
    character(3)        :: sgn(m)
    integer             :: lin1, i, j, wleft, wid(n), widp(n)
    character, pointer  :: boxp(:,:)
    SE%zas = ''
    SEim%zas = ''
    wid = widre + widim + 4
    call preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
    do j=1,n
      if (m > 0) write(s, SE%ed) (real(x(i,j)), i=1,m)
      call copytobox(s, lin1, widre(j), widp(j) - widim(j) - 4, nblre(j), boxp,  wleft)
      do i=1,m
        if (aimag(x(i,j)) < 0) then; sgn(i) = ' - '; else; sgn(i) = ' + '; endif
        enddo
      call copytobox(sgn, lin1, 3, 3, 0, boxp,  wleft)
      if (m > 0) write(sim, SEim%ed) (abs(aimag(x(i,j))), i=1,m)
      call copytobox(sim, lin1, widim(j), widim(j), nblim(j), boxp,  wleft)
      call copyseptobox('i', m, lin1, boxp, wleft)
      if (j<n) call copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp,  wleft)
    enddo
    call finishbox(title, SE, boxp)
  end subroutine tobox_cpld

  ! ******* TOSTRING DOUBLE PRECISION COMPLEX PROCEDURES ********

  function tostring_s_cpld(x) result(st)
    complex(dble), intent(in)                   :: x
    character(len_s_cpld(x, tosset%rfmt)) :: st
    st = tostring_f_cpld((/x/), tosset%rfmt)
  end function tostring_s_cpld

  function tostring_sf_cpld(x, fmt) result(st)
    complex(dble),  intent(in)        :: x
    character(*), intent(in)          :: fmt
    character(len_s_cpld(x, fmt)) :: st
    st = tostring_f_cpld((/x/), fmt)
  end function tostring_sf_cpld

  function tostring_cpld(x) result(st)
    complex(dble), intent(in)               :: x(:)
    character(len_f_cpld(x, tosset%rfmt)) :: st
    st = tostring_f_cpld(x, tosset%rfmt)
  end function tostring_cpld

  function tostring_f_cpld(x, fmt) result(st)
    complex(dble),  intent(in)                    :: x(:)
    character(*),   intent(in)                    :: fmt
    character(len_f_cpld(x, fmt))                 :: st
    character(widthmax_dble(real(x), fmt))        :: sar(size(x))
    character(widthmax_dble(abs(x-real(x)), fmt)) :: sai(size(x))  ! x-real(x) instead of aimag(x) to enable the fnction
    character(1)                                  :: sgn(size(x))  ! to pass -stand:f95 switch of the ifort compiler.
    integer                                       :: w, d, wr, wi, i
    logical                                       :: gedit
    character(nnblk(fmt)+8)                       :: fmt1  !(5 for readfmt and 3 for replace_w)
    real(dble)                                    :: xre(size(x)), xim(size(x)), h
    call readfmt(fmt, fmt1, w, d, gedit)
    xre = real(x)
    xim = aimag(x)
    h = huge(h)
    if (w < 0) then
      st = errormsg
      return
    elseif (w == 0) then
      wr = maxw_dble(xre, d)
      wi = maxw_dble(xim, d)
      call replace_w(fmt1, max(wr, wi))
    endif
    write(sar, fmt1) real(x)
    write(sai, fmt1) abs(aimag(x))
    call trim_real(sar, gedit, w)
    call trim_real(sai, gedit, w)
    do i = 1,size(x); if (aimag(x(i)) < 0) then; sgn(i) = '-'; else; sgn(i) = '+'; endif; enddo
    call tostring_get_complex(sar, sgn, sai, st)
  end function tostring_f_cpld

  pure function len_s_cpld(x, fmt) result(wtot)
    complex(dble), intent(in) :: x
    character(*), intent(in)  :: fmt
    integer                   :: wtot, w, d
    logical                   :: gedit
    character(nnblk(fmt)+8)   :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; wtot = len(errormsg); return; endif
    wtot = len_f_dble((/real(x)/), fmt) + len_f_dble((/abs(aimag(x))/), fmt) + 4
  end function len_s_cpld

  pure function len_f_cpld(x, fmt) result(wtot)
    complex(dble), intent(in) :: x(:)
    character(*), intent(in)  :: fmt
    integer                   :: wtot, w, d
    logical                   :: gedit
    character(nnblk(fmt)+8)   :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; wtot = len(errormsg); return; endif
    wtot = len_f_dble(real(x), fmt) + len_f_dble(abs(aimag(x)), fmt) + size(x)*4 - (size(x) - 1)*(tosset%seplen)
    ! subtract seplen because it has been added twice in len_f_dble
  end function len_f_cpld
  ! *************************************** END OF DOUBLE PRECISION COMPLEX PROCEDURES ********************************

  ! ********************************************** DEFAULT LOGICAL PROCEDURES *****************************************
  subroutine disp_s_dlog(x, fmt, advance, sep, trim, unit)
    ! Default logical scalar without title
    character(*), intent(in), optional :: fmt, advance, sep, trim
    logical(dlog), intent(in) :: x
    integer, intent(in), optional :: unit
    call disp_ts_dlog('', x, fmt, advance, sep, 'left', trim, unit)
  end subroutine disp_s_dlog

  subroutine disp_v_dlog(x, fmt, advance, lbound, sep, style, trim, unit, orient)
    ! Default logical vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, orient
    logical(dlog), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:)
    call disp_tv_dlog('', x, fmt, advance, lbound, sep, style, trim, unit, orient)
  end subroutine disp_v_dlog

  subroutine disp_m_dlog(x, fmt, advance, lbound, sep, style, trim, unit)
    ! Default logical matrix without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim
    logical(dlog), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, lbound(:)
    call disp_tm_dlog('', x, fmt, advance, lbound, sep, style, trim, unit)
  end subroutine disp_m_dlog

  subroutine disp_ts_dlog(title, x, fmt, advance, sep, style, trim, unit)
    ! Default logical scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim
    logical(dlog), intent(in) :: x
    integer, intent(in), optional :: unit
    call disp_tm_dlog(title, reshape((/x/), (/1, 1/)), fmt, advance, sep=sep, style=style, trim=trim, unit=unit)
  end subroutine disp_ts_dlog

  subroutine disp_tv_dlog(title, x, fmt, advance, lbound, sep, style, trim, unit, orient)
    ! Default logical vector with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, orient
    logical(dlog), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:)
    type(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient)
    if (SE%row) then
      call disp_dlog(title, reshape(x, (/1, size(x)/)), SE)
    else
      call disp_dlog(title, reshape(x, (/size(x), 1/)), SE)
    end if
  end subroutine disp_tv_dlog

  subroutine disp_tm_dlog(title, x, fmt, advance, lbound, sep, style, trim, unit)
    ! Default logical matrix with title
    character(*), intent(in)           :: title     ! The title to use for the matrix
    logical(dlog),intent(in)           :: x(:,:)    ! The matrix to be written
    character(*), intent(in), optional :: fmt       ! Format edit descriptor to use for each matrix element (e.g. 'L1')
    integer,      intent(in), optional :: unit      ! Unit to display on
    character(*), intent(in), optional :: advance   ! 'No' to print next matrix to right of current, otherewise 'Yes'
    character(*), intent(in), optional :: sep       ! Separator between matrix columns (e.g. ", ")
    character(*), intent(in), optional :: style     ! Style(s): See NOTE 1 below
    character(*), intent(in), optional :: trim      ! 'Auto' (the default) to trim if fmt absent, 'no' for no trimming,
    !                                               ! 'yes' for trimming
    integer,      intent(in), optional :: lbound(:) ! Lower bounds of x
    type(settings) :: SE
    !
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit)
    call disp_dlog(title, x, SE)
  end subroutine disp_tm_dlog

  subroutine disp_dlog(title, x, SE)
    ! Write default logical to box or unit
    character(*),   intent(in)    :: title
    logical(dlog),  intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer wid(size(x,2)), nbl(size(x,2))
    if (SE%w <= 0 .or. SE%trm) then
      SE%ed = '(L1)'
      if (size(x) == 0) then
        wid = 0
      else
        wid = 1
      endif
      SE%w = 1
      nbl = SE%w - wid
    else
      wid = SE%w
      nbl = 0
    endif
    call tobox_dlog(title, x, SE, wid, nbl)
  end subroutine disp_dlog

  subroutine tobox_dlog(title, x, SE, wid, nbl)
    character(*),   intent(in)    :: title
    logical(dlog),  intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer,        intent(inout) :: wid(:)
    integer,        intent(inout) :: nbl(:)
    character(SE%w)  :: s(size(x,1))
    integer            :: m, n, lin1, i, j, wleft, widp(size(wid))
    character, pointer :: boxp(:,:)
    m = size(x,1)
    n = size(x,2)
    call preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
    do j=1,n
      if (m > 0) write(s, SE%ed) (x(i,j), i=1,m)
      call copytobox(s, lin1, wid(j), widp(j), nbl(j), boxp,  wleft)
      if (j<n) call copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp,  wleft)
    enddo
    call finishbox(title, SE, boxp)
  end subroutine tobox_dlog

  ! ********** DEFAULT LOGICAL TOSTRING PROCEDURES *********
  function tostring_s_dlog(x) result(st)
    logical(dlog), intent(in) :: x
    character(1)            :: st
    st = tostring_f_dlog((/x/), 'L1')
  end function tostring_s_dlog

  function tostring_sf_dlog(x, fmt) result(st)
    logical(dlog),intent(in)        :: x
    character(*), intent(in)        :: fmt
    character(len_f_dlog((/x/), fmt)) :: st
    st = tostring_f_dlog((/x/), fmt)
  end function tostring_sf_dlog

  function tostring_dlog(x) result(st)
    logical(dlog), intent(in)                          :: x(:)
    character(1 + (size(x) - 1)*(1 + tosset%seplen)) :: st
    st = tostring_f_dlog(x, 'L1')
  end function tostring_dlog

  function tostring_f_dlog(x, fmt) result(st)
    logical(dlog), intent(in)     :: x(:)
    character(*), intent(in)      :: fmt
    character(len_f_dlog(x, fmt)) :: st
    character(widthmax_dlog(fmt)) :: sa(size(x))
    integer                       :: w, d
    logical                       :: gedit
    character(nnblk(fmt)+5)       :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w <= 0) then; st = errormsg; return; endif
    write(sa, fmt1) x
    if (tosset%trimb == 'YES') sa = adjustl(sa)
    call tostring_get(sa, st)
  end function tostring_f_dlog

  pure function len_f_dlog(x, fmt) result(wtot)
    logical(dlog), intent(in)  :: x(:)
    character(*), intent(in)   :: fmt
    integer                    :: wtot, w, d
    logical                    :: gedit
    character(nnblk(fmt)+5)    :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w <= 0) then; wtot = len(errormsg); return; endif
    if (tosset%trimb == 'YES') wtot = size(x)
    if (tosset%trimb == 'NO' ) wtot = w*size(x)
    wtot = wtot + (size(x) - 1)*(tosset%seplen)
  end function len_f_dlog

  pure function widthmax_dlog(fmt) result(w)
    character(*), intent(in) :: fmt
    integer w, d
    logical gedit
    character(nnblk(fmt)+5) :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w <= 0) w = 1
  end function widthmax_dlog
  ! ****************************** END OF DEFAULT LOGICAL PROCEDURES *******************************

  ! ******************************* DEFAULT CHARACTER PROCEDURES **********************************
  subroutine disp_v_dchr(x, fmt, advance, lbound, sep, style, trim, unit, orient)
    ! Default character vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, orient
    character(*), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:)
    call disp_tv_dchr('', x, fmt, advance, lbound, sep, style, trim, unit, orient)
  end subroutine disp_v_dchr

  subroutine disp_m_dchr(x, fmt, advance, lbound, sep, style, trim, unit)
    ! Default character matrix without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim
    character(*), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, lbound(:)
    call disp_tm_dchr('', x, fmt, advance, lbound, sep, style, trim, unit)
  end subroutine disp_m_dchr

  subroutine disp_ts_dchr(title, x, fmt, advance, sep, style, trim, unit)
    ! Default character scalar with title
    character(*), intent(in), optional :: title, x, fmt, advance, sep, style, trim
    character(0) empty(1,0)
    integer, intent(in), optional :: unit
    empty = ''
    if (present(title).and.present(x)) then
      call disp_nonopt_dchr(title, x, fmt, advance, sep=sep, style=style, trim=trim, unit=unit)
    elseif (present(x)) then
      call disp_nonopt_dchr('', x, fmt, advance, sep=sep, style='left', trim=trim, unit=unit)
    elseif (present(title)) then
      call disp_nonopt_dchr('', title, fmt, advance, sep=sep, style='left', trim=trim, unit=unit)
    else
      call disp_tm_dchr('', empty, fmt, advance, sep=sep, style=style, trim=trim, unit=unit)
    end if
  end subroutine disp_ts_dchr

  subroutine disp_nonopt_dchr(title, x, fmt, advance, sep, style, trim, unit)
    ! This routine exists to circumvent bug in gfortran, that made it not possible to change scalar strings
    ! to matrices with reshape in calls of disp_tm_dchr. This intermediate routine provides work-around.
    character(*), intent(in) :: title, x, fmt, advance, sep, style, trim
    optional fmt, advance, sep, style, trim
    integer, intent(in), optional :: unit
    character(len(x)) :: xm(1,1)
    xm(1,1) = x
    call disp_tm_dchr(title, xm, fmt, advance, sep=sep, style=style, trim=trim, unit=unit)
  end subroutine disp_nonopt_dchr

  subroutine disp_tv_dchr(title, x, fmt, advance, lbound, sep, style, trim, unit, orient)
    ! Default character vector with title
    character(*), intent(in) :: title, x(:)
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, orient
    integer, intent(in), optional :: unit, lbound(:)
    type(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit, orient)
    if (SE%row) then
      call disp_dchr(title, reshape(x, (/1, size(x)/)), SE)
    else
      call disp_dchr(title, reshape(x, (/size(x), 1/)), SE)
    end if
  end subroutine disp_tv_dchr

  subroutine disp_tm_dchr(title, x, fmt, advance, lbound, sep, style, trim, unit)
    ! Default character matrix with title
    character(*), intent(in)           :: title      ! The title to use for the matrix
    character(*), intent(in)           :: x(:,:)     ! The matrix to be written
    character(*), intent(in), optional :: fmt        ! Format edit descriptor to use for each matrix element (e.g.'A4')
    integer,      intent(in), optional :: unit       ! Unit to display on
    character(*), intent(in), optional :: advance    ! 'No' to print next matrix to right of current, otherewise 'Yes'
    character(*), intent(in), optional :: sep        ! Separator between matrix columns (e.g. ", ")
    character(*), intent(in), optional :: style      ! Style(s): see NOTE 1 below
    character(*), intent(in), optional :: trim       ! 'Auto' (the default) to trim if fmt absent, 'no' for no
    !                                                ! trimming, 'yes' for trimming
    integer,      intent(in), optional :: lbound(:)  ! Lower bounds of x
    !
    type(settings) :: SE
    call get_SE(SE, title, shape(x), fmt, advance, lbound, sep, style, trim, unit)
    call disp_dchr(title, x, SE)
  end subroutine disp_tm_dchr

  subroutine disp_dchr(title, x, SE)
    ! Default character item to box
    character(*), intent(in)      :: title, x(:,:)
    type(settings), intent(inout) :: SE
    character(13)                 :: edesc
    character, pointer            :: boxp(:,:)
    integer                       :: m, n, j, lin1, wleft, lx, w
    integer, dimension(size(x,2)) :: wid, nbl, n1, n2, widp
    m = size(x,1)
    n = size(x,2)
    lx = len(x)
    w = SE%w
    if (w <= 0) then
      w = lx
      if (w < 0) then
        edesc = '(A__________)'
        write(edesc(3:12), '(SS,I10)') w
        SE%ed = edesc
      end if
    end if
    if (SE%trm .and. size(x) > 0) then
      n1 = minval(mod(verify(x, ' ') - w - 1, w + 1), 1) + w + 1
      n2 = maxval(verify(x, ' ', back = .true.), 1)
      wid = n2 - n1 + 1
      nbl = w - wid
    else
      n1 = 1
      n2 = w
      wid = w
      nbl = 0
    end if
    if (all(wid == 0)) n = 0
    SE%w = w
    call preparebox(title, SE, m, n, wid, widp, lin1, wleft, boxp)
    do j=1,n
      if (SE%trm) then
        call copytobox(x(:,j)(n1(j):n2(j)), lin1, wid(j), widp(j), nbl(j), boxp,  wleft)
      else
        if (widp(j) > lx) call copyseptobox(repeat(' ', widp(j)-lx), m, lin1, boxp,  wleft)
        call copytobox(x(:,j), lin1, lx, lx, 0, boxp,  wleft)
      end if
      if (j<n) call copyseptobox(SE%sep(1:SE%lsep), m, lin1, boxp,  wleft)
    enddo
    call finishbox(title, SE, boxp)
  end subroutine disp_dchr

  ! ************************* END OF DEFAULT CHARACTER PROCEDURES ********************************

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

END MODULE DISPMODULE
