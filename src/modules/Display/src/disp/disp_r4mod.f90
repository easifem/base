! Add-on module to DISPMODULE to display selected_real_kind(25) reals
! (these are probably 16 bytes and possibly snglruple precision)
!
! This module is obtained by copying the section SINGLE PRECSION PROCEDURES from
! dispmodule.f90, replacing sngl with sngl, single withe snglruple (only appears
! in comments) and cplx with cplx, adding a DECLARATIONS section, and defining
! the constant sngl as selected_real_kind(25).
!
! Copyright (c) 2008, Kristj�n J�nasson, Dept. of Computer Science, University of
! Iceland (jonasson@hi.is). This software is free. For details see the file README.

MODULE DISP_R4MOD
USE DISPMODULE_UTIL
USE GlobalData, ONLY: Real32
PUBLIC DISP
PUBLIC TOSTRING
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE DISP
  MODULE PROCEDURE disp_s_sngl, disp_ts_sngl, disp_v_sngl, disp_tv_sngl, disp_m_sngl, disp_tm_sngl
  MODULE PROCEDURE disp_s_cplx, disp_ts_cplx, disp_v_cplx, disp_tv_cplx, disp_m_cplx, disp_tm_cplx
END INTERFACE DISP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE TOSTRING
  MODULE PROCEDURE tostring_sngl, tostring_f_sngl, tostring_s_sngl, tostring_sf_sngl
  MODULE PROCEDURE tostring_cplx, tostring_f_cplx, tostring_s_cplx, tostring_sf_cplx
END INTERFACE TOSTRING

INTEGER, PARAMETER :: sngl = Real32

CONTAINS

  subroutine disp_s_sngl(x, fmt, advance, digmax, sep, trim, unit, zeroas)
    ! snglruple precision scalar without title
    character(*), intent(in), optional :: fmt, advance, sep, trim, zeroas
    real(sngl), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_ts_sngl('', x, fmt, advance, digmax, sep, 'left', trim, unit, zeroas)
  end subroutine disp_s_sngl

  subroutine disp_v_sngl(x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
    ! snglruple precision vector without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas, orient
    real(sngl), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    call disp_tv_sngl('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
  end subroutine disp_v_sngl

  subroutine disp_m_sngl(x, fmt, advance, lbound, sep, style, trim, unit, digmax, zeroas)
    ! snglruple precision matrix without title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    real(sngl), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, digmax, lbound(:)
    call disp_tm_sngl('', x, fmt, advance, digmax, lbound, sep, style, trim, unit, zeroas)
  end subroutine disp_m_sngl

  subroutine disp_ts_sngl(title, x, fmt, advance, digmax, sep, style, trim, unit, zeroas)
    ! snglruple precision scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, advance, sep, style, trim, zeroas
    real(sngl), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_tm_sngl(title, reshape((/x/), (/1, 1/)), fmt, advance, digmax, sep=sep, style=style, trim=trim, &
         unit=unit, zeroas=zeroas)
  end subroutine disp_ts_sngl

  subroutine disp_tv_sngl(title, x, fmt, advance, digmax, lbound, sep, style, trim, unit, orient, zeroas)
    ! snglruple precision vector with title
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
    ! snglruple precision matrix with title
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
    ! snglruple precision item
    character(*),   intent(in)    :: title
    real(sngl),     intent(in)    :: x(:,:)
    type(settings), intent(inout) :: SE
    integer wid(size(x,2)), nbl(size(x,2))
    call find_editdesc_sngl(x, SE, wid, nbl) ! determine also SE%w
    call tobox_sngl(title, x, SE, wid, nbl)
  end subroutine disp_sngl

  subroutine tobox_sngl(title, x, SE, wid, nbl)
    ! Write snglruple precision matrix to box
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

  ! ******** TOSTRING snglRUPLE PRECISION PROCEDURES ***********
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

  ! *************************************** END OF snglRUPLE PRECISION PROCEDURES ***************************************

  ! *************************************** snglRUPLE PRECISION COMPLEX PROCEDURES **************************************
  subroutine disp_s_cplx(x, fmt, fmt_imag, advance, digmax, sep, trim, unit)
    ! snglruple precision complex scalar without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, trim
    complex(sngl), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_ts_cplx('', x, fmt, fmt_imag, advance, digmax, sep, 'left', trim, unit)
  end subroutine disp_s_cplx

  subroutine disp_v_cplx(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
    ! snglruple precision complex vector without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim, orient
    complex(sngl), intent(in) :: x(:)
    integer, intent(in), optional :: unit, lbound(:), digmax
    call disp_tv_cplx('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
  end subroutine disp_v_cplx

  subroutine disp_m_cplx(x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
    ! snglruple precision complex matrix without title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim
    complex(sngl), intent(in) :: x(:,:)
    integer, intent(in), optional :: unit, digmax, lbound(:)
    call disp_tm_cplx('', x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit)
  end subroutine disp_m_cplx

  subroutine disp_ts_cplx(title, x, fmt, fmt_imag, advance, digmax, sep, style, trim, unit)
    ! snglruple precision complex scalar with title
    character(*), intent(in) :: title
    character(*), intent(in), optional :: fmt, fmt_imag, advance, sep, style, trim
    complex(sngl), intent(in) :: x
    integer, intent(in), optional :: unit, digmax
    call disp_tm_cplx(title, reshape((/x/), (/1, 1/)), fmt, fmt_imag, advance, digmax, sep=sep, style=style, &
                                                       trim=trim, unit=unit)
  end subroutine disp_ts_cplx

  subroutine disp_tv_cplx(title, x, fmt, fmt_imag, advance, digmax, lbound, sep, style, trim, unit, orient)
    ! snglruple precision complex vector with title
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
    ! snglruple precision complex matrix with title
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
    ! snglruple precision item
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
    ! Write snglruple precision complex matrix to box
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

  ! ******* TOSTRING snglRUPLE PRECISION COMPLEX PROCEDURES ********

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

END MODULE DISP_R4MOD
