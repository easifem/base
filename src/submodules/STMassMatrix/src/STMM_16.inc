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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_16(ans, test, trial, term1, term2, rho)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_t
CLASS(FEVariable_), INTENT(IN) :: rho
!! matrix
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, ans=kbar, val=rho)

!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(kbar,1), size(kbar,2), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
    & trial(ipt)%wt * trial(ipt)%jt
  !!
  DO ips = 1, SIZE(realval)
    !!
    DO b = 1, SIZE(m6, 6)
      DO a = 1, SIZE(m6, 5)
        m6(:, :, :, :, a, b) = m6(:, :, :, :, a, b) &
          & + realval(ips) &
          & * outerprod( &
          & test(ipt)%dNTdt(:,a, ips), &
          & trial(ipt)%dNTdt(:,b, ips), &
          & kbar(:,:,ips, ipt) )
      END DO
    END DO
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
DEALLOCATE (m6, kbar, realval)
  !!
END SUBROUTINE STMM_16
