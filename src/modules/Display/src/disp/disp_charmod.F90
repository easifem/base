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
USE GlobalData, ONLY: Real32
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
  character(*), intent(in), optional :: fmt, advance, sep, style, trim, orient
  character(*), intent(in) :: x(:)
  integer, intent(in), optional :: unit, lbound(:)
  call disp_tv_dchr('', x, fmt, advance, lbound, sep, style, trim, unit, orient)
end subroutine disp_v_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine disp_m_dchr(x, fmt, advance, lbound, sep, style, trim, unit)
  ! Default character matrix without title
  character(*), intent(in), optional :: fmt, advance, sep, style, trim
  character(*), intent(in) :: x(:,:)
  integer, intent(in), optional :: unit, lbound(:)
  call disp_tm_dchr('', x, fmt, advance, lbound, sep, style, trim, unit)
end subroutine disp_m_dchr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DISP_CHARMOD
