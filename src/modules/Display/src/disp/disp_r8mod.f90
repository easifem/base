! Add-on module to DISPMODULE to display selected_real_kind(25) reals
! (these are probably 16 bytes and possibly dbleruple precision)
!
! This module is obtained by copying the section SINGLE PRECSION PROCEDURES from
! dispmodule.f90, replacing sngl with dble, single withe dbleruple (only appears
! in comments) and cplx with cpld, adding a DECLARATIONS section, and defining
! the constant dble as selected_real_kind(25).
!
! Copyright (c) 2008, Kristj�n J�nasson, Dept. of Computer Science, University of
! Iceland (jonasson@hi.is). This software is free. For details see the file README.

MODULE DISP_R8MOD
USE DISPMODULE_UTIL
USE GlobalData, ONLY: Real64
PUBLIC DISP
PUBLIC TOSTRING
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE DISP
  MODULE PROCEDURE disp_s_dble, disp_ts_dble, disp_v_dble, disp_tv_dble, disp_m_dble, disp_tm_dble
  MODULE PROCEDURE disp_s_cpld, disp_ts_cpld, disp_v_cpld, disp_tv_cpld, disp_m_cpld, disp_tm_cpld
END INTERFACE DISP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE TOSTRING
  MODULE PROCEDURE tostring_dble, tostring_f_dble, tostring_s_dble, tostring_sf_dble
  MODULE PROCEDURE tostring_cpld, tostring_f_cpld, tostring_s_cpld, tostring_sf_cpld
END INTERFACE TOSTRING

INTEGER, PARAMETER :: dble = Real64

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_s_dble(x, fmt, advance, digmax, sep, trim, unit, zeroas)
    ! dbleruple precision scalar without title
    character(*), intent(in), optional :: fmt, advance, sep, trim, zeroas
    real(dble), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_ts_dble('', x, fmt, advance, digmax, sep, 'left', trim, unit, zeroas)
  end subroutine disp_s_dble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_v_dble(x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
    ! dbleruple precision vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
    real(dble), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    call disp_tv_dble('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
  end subroutine disp_v_dble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_m_dble(x, fmt, advance, lbound, sep, style, trim, unit, digmax, zeroas)
    ! dbleruple precision matrix without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    real(dble), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, digmax, lbound(:)
    call disp_tm_dble('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
  end subroutine disp_m_dble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_ts_dble(title, x, fmt, advance, digmax, sep, style, trim, unit, zeroas)
    ! dbleruple precision scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    real(dble), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_tm_dble(title, reshape((/x/), (/1, 1/)), fmt, advance, digmax, sep=sep, style=style, trim=trim, &
      & unit=unit, zeroas=zeroas)
  end subroutine disp_ts_dble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_tv_dble(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
    ! dbleruple precision vector with title
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_tm_dble(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
    ! dbleruple precision matrix with title
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_dble(title, x, SE)
    ! dbleruple precision item
    character(*),   intent(in)    :: title
    real(dble),     intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer wid(size(x,2)), nbl(size(x,2))
    call find_editdesc_dble(x, SE, wid, nbl) ! determine also SE%w
    call tobox_dble(title, x, SE, wid, nbl)
  end subroutine disp_dble

  subroutine tobox_dble(title, x, SE, wid, nbl)
    ! Write dbleruple precision matrix to box
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  function tostring_s_dble(x) result(st)
    ! Scalar to string
    real(dble), intent(in) :: x
    character(len_f_dble((/x/), tosset%rfmt)) :: st
    st = tostring_f_dble((/x/), tosset%rfmt)
  end function tostring_s_dble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  function tostring_sf_dble(x, fmt) result(st)
    ! Scalar with specified format to string
    real(dble),   intent(in) :: x
    character(*), intent(in) :: fmt
    character(len_f_dble((/x/), fmt)) :: st
    st = tostring_f_dble((/x/), fmt)
  end function tostring_sf_dble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  function tostring_dble(x) result(st)
    ! Vector to string
    real(dble), intent(in) :: x(:)
    character(len_f_dble(x, tosset%rfmt)) :: st
    st = tostring_f_dble(x, tosset%rfmt)
  end function tostring_dble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_s_cpld(x, fmt, fmt_imag, advance, digmax, sep, trim, unit)
    ! dbleruple precision complex scalar without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, trim
    complex(dble), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_ts_cpld('', x, fmt, fmt_imag, advance, digmax, sep, 'left', trim, unit)
  end subroutine disp_s_cpld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_v_cpld(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
    ! dbleruple precision complex vector without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim, orient
    complex(dble), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    call disp_tv_cpld('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
  end subroutine disp_v_cpld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_m_cpld(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
    ! dbleruple precision complex matrix without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim
    complex(dble), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, digmax, lbound(:)
    call disp_tm_cpld('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
  end subroutine disp_m_cpld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_ts_cpld(title, x, fmt, fmt_imag, advance, digmax, sep, style, trim, unit)
    ! dbleruple precision complex scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim
    complex(dble), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_tm_cpld(title, reshape((/x/), (/1, 1/)), fmt, fmt_imag, &
      & advance, digmax, sep=sep, style=style, trim=trim, unit=unit)
  end subroutine disp_ts_cpld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_tv_cpld(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
    ! dbleruple precision complex vector with title
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_tm_cpld(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
    ! dbleruple precision complex matrix with title
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine disp_cpld(title, x, SE, SEim, n)
    ! dbleruple precision item
    character(*),   intent(in)    :: title
    complex(dble),  intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE, SEim
    integer,        intent(in)    :: n
    integer, dimension(n) :: widre(n), widim(n), nblre(n), nblim(n)
    call find_editdesc_dble(real(x), SE, widre, nblre)         ! determine also SE%w
    call find_editdesc_dble(abs(aimag(x)), SEim, widim, nblim) ! determine also SEim%w
    call tobox_cpld(title, x, SE, SEim, widre, widim, nblre, nblim, m = size(x,1), n = size(x,2))
  end subroutine disp_cpld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  subroutine tobox_cpld(title, x, SE, SEim, widre, widim, nblre, nblim, m, n)
    ! Write dbleruple precision complex matrix to box
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  function tostring_s_cpld(x) result(st)
    complex(dble), intent(in)                   :: x
    character(len_s_cpld(x, tosset%rfmt)) :: st
    st = tostring_f_cpld((/x/), tosset%rfmt)
  end function tostring_s_cpld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  function tostring_sf_cpld(x, fmt) result(st)
    complex(dble),  intent(in)        :: x
    character(*), intent(in)          :: fmt
    character(len_s_cpld(x, fmt)) :: st
    st = tostring_f_cpld((/x/), fmt)
  end function tostring_sf_cpld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  function tostring_cpld(x) result(st)
    complex(dble), intent(in)               :: x(:)
    character(len_f_cpld(x, tosset%rfmt)) :: st
    st = tostring_f_cpld(x, tosset%rfmt)
  end function tostring_cpld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  pure function len_f_cpld(x, fmt) result(wtot)
    complex(dble), intent(in) :: x(:)
    character(*), intent(in)  :: fmt
    integer                   :: wtot, w, d
    logical                   :: gedit
    character(nnblk(fmt)+8)   :: fmt1
    call readfmt(fmt, fmt1, w, d, gedit)
    if (w < 0) then; wtot = len(errormsg); return; endif
    wtot = len_f_dble(real(x), fmt) + len_f_dble(abs(aimag(x)), fmt) &
      & + size(x)*4 - (size(x) - 1)*(tosset%seplen)
    ! subtract seplen because it has been added twice in len_f_dble
  end function len_f_cpld

END MODULE DISP_R8MOD
