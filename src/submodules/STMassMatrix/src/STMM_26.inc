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
!

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STMM_26(ans, test, trial, term1, term2, c1, c2)
REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
CLASS(STElemshapeData_), INTENT(IN) :: test(:)
CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
INTEGER(I4B), INTENT(IN) :: term1
!! del_t
INTEGER(I4B), INTENT(IN) :: term2
!! del_none
CLASS(FEVariable_), INTENT(IN) :: c1
!! scalar
CLASS(FEVariable_), INTENT(IN) :: c2
!! matrix
!!
!! Internal variable
REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: IJija(:, :, :, :, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: Jij(:, :, :)
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ipt, ips, a, b
!!
!! main
!!
CALL getInterpolation(obj=trial, interpol=c1bar, val=c1)
CALL getInterpolation(obj=trial, interpol=kbar, val=c2)
!!
CALL Reallocate(m6, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(kbar,1), SIZE(kbar,2), &
  & SIZE(test(1)%T), &
  & SIZE(trial(1)%T))
!!
CALL Reallocate(IJija, &
  & SIZE(test(1)%N, 1), &
  & SIZE(trial(1)%N, 1), &
  & SIZE(m6,3), SIZE(m6,4), &
  & SIZE(test(1)%T))
!!
DO ipt = 1, SIZE(trial)
  !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
        & trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  !!
  DO ips = 1, SIZE(realval)
    !!
    Jij = OUTERPROD( trial(ipt)%N(:, ips), kbar(:,:,ips,ipt) )
    !!
    DO a = 1, SIZE(m6, 5)
      IJija(:, :, :, :, a) = outerprod(test(ipt)%dNTdt(:, a, ips), Jij)
    END DO
    !!
    m6 = m6 + realval(ips) * outerprod(IJija, trial(ipt)%T)
    !!
  END DO
END DO
!!
CALL Convert(from=m6, to=ans)
!!
DEALLOCATE (m6, IJija, kbar, Jij, realval)
  !!
END SUBROUTINE STMM_26
