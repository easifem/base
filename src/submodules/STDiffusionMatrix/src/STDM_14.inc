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

PURE SUBROUTINE STDM_14a(ans, test, trial, c1, c2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: c1
  !! Scalar
  CLASS(FEVariable_), INTENT(IN) :: c2
  !! Vector
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt=1
  !!
  !! Internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: IJab(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  INTEGER(I4B) :: ips, ipt, ii, nsd, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=cbar, val=c1)
  CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
  !!
  CALL Reallocate( &
    & IJab, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  CALL Reallocate( &
    & m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(vbar, 1), &
    & 1, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  nsd = trial(1)%refelem%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt * cbar(:, ipt)
    !!
    DO ips = 1, SIZE(realval)
      !!
      IJab = 0.0_DFP
      !!
      DO ii = 1, nsd
        DO b = 1, SIZE(IJab, 4)
          DO a = 1, SIZE(IJab, 3)
            !!
            IJab(:,:,a,b) = IJab(:,:,a,b) &
              & + OUTERPROD( &
              & test(ipt)%dNTdXt(:, a, ii, ips), &
              & trial(ipt)%dNTdXt(:, b, ii, ips))
            !!
          END DO
        END DO
      END DO
      !!
      DO ii = 1, SIZE(m6, 3)
        m6(:,:,ii,1,:,:) = m6(:,:,ii,1,:,:) &
          & + realval( ips ) * vbar(ii, ips, ipt) &
          & * IJab
      END DO
      !!
    END DO
  END DO
  !!
  CALL Convert( from=m6, to=ans)
  !!
  DEALLOCATE (realval, IJab, vbar, m6, cbar)
  !!
END SUBROUTINE STDM_14a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STDM_14b(ans, test, trial, c1, c2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: c1
  !! Scalar
  CLASS(FEVariable_), INTENT(IN) :: c2
  !! Vector variable
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt=2
  !!
  !! Internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: IJab(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  INTEGER(I4B) :: ips, ipt, ii, nsd
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=cbar, val=c1)
  CALL getInterpolation(obj=trial, interpol=vbar, val=c2)
  !!
  CALL Reallocate( &
    & IJab, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  CALL Reallocate( &
    & m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & 1, &
    & SIZE(vbar, 1), &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  nsd = trial(1)%refelem%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt * cbar(:,ipt)
    !!
    DO ips = 1, SIZE(realval)
      !!
      IJab = 0.0_DFP
      !!
      DO ii = 1, nsd
        DO b = 1, SIZE(IJab, 4)
          DO a = 1, SIZE(IJab, 3)
            !!
            IJab(:,:,a,b) = IJab(:,:,a,b) &
              & + OUTERPROD( &
              & test(ipt)%dNTdXt(:, a, ii, ips), &
              & trial(ipt)%dNTdXt(:, b, ii, ips))
            !!
          END DO
        END DO
      END DO
      !!
      DO ii = 1, SIZE(m6, 4)
        m6(:,:,1,ii,:,:) = m6(:,:,1,ii,:,:) &
          & + realval( ips ) * vbar(ii, ips, ipt) &
          & * IJab
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m6, to=ans)
  !!
  DEALLOCATE (realval, IJab, vbar, m6)
  !!
END SUBROUTINE STDM_14b
