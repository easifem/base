! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

! 11a, 11b, 11c
!       ij = outerprod(vbar(:,ips, ipt), [1.0_DFP])
!       ij = diag(vbar(:,ips, ipt))
!       ij = outerprod(vbar(:,ips, ipt), vbar(:,ips,ipt))

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_none
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! vector
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
REAL(DFP), ALLOCATABLE :: ij(:, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, ans=c1bar, val=c1)
CALL getInterpolation(obj=trial, ans=vbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & _DIM1_, _DIM2_, &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    ij = _KIJ_
    !!
    DO b = 1, SIZE(trial(1)%T)
      m6(:, :, :, :, :, b) = m6(:, :, :, :, :, b) &
        & + realval(ips) &
        & * outerprod(  &
          & test(ipt)%N(:, ips), &
          & trial(ipt)%dNTdt(:, b, ips), &
          & ij, test(ipt)%T)
    END DO
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, ij, c1bar, vbar, realval)
