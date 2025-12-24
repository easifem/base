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

PURE SUBROUTINE STDM_13a(ans, test, trial, c1, c2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: c1
  !! Scalar variable
  CLASS(FEVariable_), INTENT(IN) :: c2
  !! Scalar variable
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt=1
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c1bar(:, :)
  REAL(DFP), ALLOCATABLE :: c2bar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  INTEGER(I4B) :: ips, ipt, ii, jj, nsd, a, b
  !!
  !! main
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & trial(1)%refelem%nsd, &
    & trial(1)%refelem%nsd, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  CALL getInterpolation(obj=trial, ans=c1bar, val=c1)
  CALL getInterpolation(obj=trial, ans=c2bar, val=c2)
  !!
  nsd = trial(1)%refelem%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt) * c2bar(:, ipt)
    !!
    DO ips = 1, SIZE(realval)
      !!
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, nsd
            DO ii = 1, nsd
              !!
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * &
                & OUTERPROD( &
                & test(ipt)%dNTdXt(:, a, ii, ips), &
                & trial(ipt)%dNTdXt(:, b, jj, ips))
              !!
            END DO
          END DO
        END DO
      END DO
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (realval, m6, c1bar, c2bar)
  !!
END SUBROUTINE STDM_13a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STDM_13b(ans, test, trial, c1, c2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: c1
  !! Scalar variable
  CLASS(FEVariable_), INTENT(IN) :: c2
  !! Scalar variable
  INTEGER(I4B), INTENT(IN) :: opt
  !! 2
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c1bar(:, :)
  REAL(DFP), ALLOCATABLE :: c2bar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  INTEGER(I4B) :: ips, ipt, ii, jj, nsd, a, b
  !!
  !! main
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & trial(1)%refelem%nsd, &
    & trial(1)%refelem%nsd, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  CALL getInterpolation(obj=trial, ans=c1bar, val=c1)
  CALL getInterpolation(obj=trial, ans=c2bar, val=c2)
  !!
  nsd = trial(1)%refelem%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt) * c2bar(:,ipt)
    !!
    DO ips = 1, SIZE(realval)
      !!
      DO b = 1, SIZE(m6, 6)
        DO a = 1, SIZE(m6, 5)
          DO jj = 1, nsd
            DO ii = 1, nsd
              !!
              m6(:, :, ii, jj, a, b) = m6(:, :, ii, jj, a, b) &
                & + realval(ips) * &
                & OUTERPROD( &
                & test(ipt)%dNTdXt(:, a, jj, ips), &
                & trial(ipt)%dNTdXt(:, b, ii, ips))
              !!
            END DO
          END DO
        END DO
      END DO
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (realval, m6, c1bar, c2bar)
  !!
END SUBROUTINE STDM_13b
