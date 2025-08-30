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

SUBMODULE(STDiffusionMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STDM_11a(ans, test, trial, k, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: k
  !! Scalar variable
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt=1
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: kbar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  INTEGER(I4B) :: ips, ipt, ii, jj, nsd, a, b
  !!
  !! main
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & trial(1)%nsd, &
    & trial(1)%nsd, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  CALL GetInterpolation(obj=trial, ans=kbar, val=k)
  !!
  nsd = trial(1)%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt * kbar(:, ipt)
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
  DEALLOCATE (realval, m6, kbar)
  !!
END SUBROUTINE STDM_11a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STDM_11b(ans, test, trial, k, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: k
  !! Scalar variable
  INTEGER(I4B), INTENT(IN) :: opt
  !! 2
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: kbar(:, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  INTEGER(I4B) :: ips, ipt, ii, jj, nsd, a, b
  !!
  !! main
  !!
  CALL Reallocate(m6, &
    & SIZE(test(1)%N, 1), &
    & SIZE(trial(1)%N, 1), &
    & trial(1)%nsd, &
    & trial(1)%nsd, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  CALL getInterpolation(obj=trial, ans=kbar, val=k)
  !!
  nsd = trial(1)%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt * kbar(:, ipt)
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
  DEALLOCATE (realval, m6, kbar)
  !!
END SUBROUTINE STDM_11b

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STDM_12a(ans, test, trial, k, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: k
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
  INTEGER(I4B) :: ips, ipt, ii, nsd, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, ans=vbar, val=k)
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
  nsd = trial(1)%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      !!
      IJab = 0.0_DFP
      !!
      DO ii = 1, nsd
        DO b = 1, SIZE(IJab, 4)
          DO a = 1, SIZE(IJab, 3)
            !!
            IJab(:, :, a, b) = IJab(:, :, a, b) &
              & + OUTERPROD( &
              & test(ipt)%dNTdXt(:, a, ii, ips), &
              & trial(ipt)%dNTdXt(:, b, ii, ips))
            !!
          END DO
        END DO
      END DO
      !!
      DO ii = 1, SIZE(m6, 3)
        m6(:, :, ii, 1, :, :) = m6(:, :, ii, 1, :, :) &
          & + realval(ips) * vbar(ii, ips, ipt) &
          & * IJab
      END DO
      !!
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (realval, IJab, vbar, m6)
  !!
END SUBROUTINE STDM_12a

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE STDM_12b(ans, test, trial, k, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: k
  !! Vector
  INTEGER(I4B), INTENT(IN) :: opt
  !! opt=2
  !!
  !! Internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: IJab(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: m6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :, :)
  INTEGER(I4B) :: ips, ipt, ii, nsd, a, b
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, ans=vbar, val=k)
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
  nsd = trial(1)%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      !!
      IJab = 0.0_DFP
      !!
      DO ii = 1, nsd
        DO b = 1, SIZE(IJab, 4)
          DO a = 1, SIZE(IJab, 3)
            !!
            IJab(:, :, a, b) = IJab(:, :, a, b) &
              & + OUTERPROD( &
              & test(ipt)%dNTdXt(:, a, ii, ips), &
              & trial(ipt)%dNTdXt(:, b, ii, ips))
            !!
          END DO
        END DO
      END DO
      !!
      DO ii = 1, SIZE(m6, 4)
        m6(:, :, 1, ii, :, :) = m6(:, :, 1, ii, :, :) &
          & + realval(ips) * vbar(ii, ips, ipt) &
          & * IJab
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (realval, IJab, vbar, m6)
  !!
END SUBROUTINE STDM_12b

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
    & trial(1)%nsd, &
    & trial(1)%nsd, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  CALL GetInterpolation(obj=trial, ans=c1bar, val=c1)
  CALL GetInterpolation(obj=trial, ans=c2bar, val=c2)
  !!
  nsd = trial(1)%nsd
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
    & trial(1)%nsd, &
    & trial(1)%nsd, &
    & SIZE(test(1)%T), &
    & SIZE(trial(1)%T))
  !!
  CALL GetInterpolation(obj=trial, ans=c1bar, val=c1)
  CALL GetInterpolation(obj=trial, ans=c2bar, val=c2)
  !!
  nsd = trial(1)%nsd
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
  CALL GetInterpolation(obj=trial, ans=cbar, val=c1)
  CALL GetInterpolation(obj=trial, ans=vbar, val=c2)
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
  nsd = trial(1)%nsd
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
            IJab(:, :, a, b) = IJab(:, :, a, b) &
              & + OUTERPROD( &
              & test(ipt)%dNTdXt(:, a, ii, ips), &
              & trial(ipt)%dNTdXt(:, b, ii, ips))
            !!
          END DO
        END DO
      END DO
      !!
      DO ii = 1, SIZE(m6, 3)
        m6(:, :, ii, 1, :, :) = m6(:, :, ii, 1, :, :) &
          & + realval(ips) * vbar(ii, ips, ipt) &
          & * IJab
      END DO
      !!
    END DO
  END DO
  !!
  CALL Convert(from=m6, to=ans)
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
  INTEGER(I4B) :: ips, ipt, ii, nsd, a, b
  !!
  !! main
  !!
  CALL GetInterpolation(obj=trial, ans=cbar, val=c1)
  CALL getInterpolation(obj=trial, ans=vbar, val=c2)
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
  nsd = trial(1)%nsd
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
            IJab(:, :, a, b) = IJab(:, :, a, b) &
              & + OUTERPROD( &
              & test(ipt)%dNTdXt(:, a, ii, ips), &
              & trial(ipt)%dNTdXt(:, b, ii, ips))
            !!
          END DO
        END DO
      END DO
      !!
      DO ii = 1, SIZE(m6, 4)
        m6(:, :, 1, ii, :, :) = m6(:, :, 1, ii, :, :) &
          & + realval(ips) * vbar(ii, ips, ipt) &
          & * IJab
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert(from=m6, to=ans)
  !!
  DEALLOCATE (realval, IJab, vbar, m6)
  !!
END SUBROUTINE STDM_14b

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE MakeDiagonalCopiesIJab(ans, ncopy)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(IN) :: ncopy
  !!
  REAL(DFP), ALLOCATABLE :: m2(:, :), m4(:, :, :, :)
  INTEGER(I4B) :: a, b
  !!
  m4 = ans
  !!
  CALL Reallocate(ans, &
    & ncopy * SIZE(m4, 1), &
    & ncopy * SIZE(m4, 2), &
    & SIZE(m4, 3), &
    & SIZE(m4, 4))
  !!
  DO b = 1, SIZE(m4, 4)
    DO a = 1, SIZE(m4, 3)
      CALL MakeDiagonalCopies(from=m4(:, :, a, b), to=m2, ncopy=ncopy)
      ans(:, :, a, b) = m2
    END DO
  END DO
  !!
  DEALLOCATE (m2, m4)
END SUBROUTINE MakeDiagonalCopiesIJab

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_1
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
INTEGER(I4B) :: ips, ipt, ii, nsd
!!
!! main
!!
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
nsd = trial(1)%nsd
!!
DO ipt = 1, SIZE(trial)
  realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
   & * trial(ipt)%Wt * trial(ipt)%Jt
  DO ips = 1, SIZE(trial(1)%N, 2)
    DO ii = 1, nsd
      iajb = iajb + realval(ips) &
        & * OUTERPROD(test(ipt)%dNTdXt(:, :, ii, ips),&
        & trial(ipt)%dNTdXt(:, :, ii, ips))
    END DO
  END DO
END DO
!!
CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
!!
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
!!
DEALLOCATE (realval, iajb)
END PROCEDURE mat4_STDiffusionMatrix_1

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_2
! CALL STDM_1(ans=ans, test=test, trial=trial, k=k, opt=opt)
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :)
INTEGER(I4B) :: ips, ipt, ii, nsd
  !!
  !! main
  !!
CALL GetInterpolation(obj=trial, ans=kbar, val=k)
  !!
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
nsd = trial(1)%nsd
  !!
DO ipt = 1, SIZE(trial)
    !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
   & * trial(ipt)%wt * trial(ipt)%jt * kbar(:, ipt)
    !!
  DO ips = 1, SIZE(realval)
      !!
    DO ii = 1, nsd
        !!
      iajb = iajb + realval(ips) &
        & * OUTERPROD(test(ipt)%dNTdXt(:, :, ii, ips),&
        & trial(ipt)%dNTdXt(:, :, ii, ips))
        !!
    END DO
  END DO
END DO
  !!
CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
DEALLOCATE (realval, iajb, kbar)
END PROCEDURE mat4_STDiffusionMatrix_2

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_3
! CALL STDM_2(ans=ans, test=test, trial=trial, k=k, opt=opt)
  !! Internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: p1(:, :, :)
REAL(DFP), ALLOCATABLE :: p2(:, :, :)
INTEGER(I4B) :: ips, ipt
  !!
  !! main
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  !!
DO ipt = 1, SIZE(trial)
    !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
   & * trial(ipt)%wt * trial(ipt)%jt
    !!
  CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p1, val=k)
    !!
  CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p2, val=k)
    !!
  DO ips = 1, SIZE(realval)
      !!
    iajb = iajb + realval(ips) * OUTERPROD(p1(:, :, ips), p2(:, :, ips))
      !!
  END DO
END DO
  !!
CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
DEALLOCATE (realval, iajb, p1, p2)
END PROCEDURE mat4_STDiffusionMatrix_3

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_4
! CALL STDM_3(ans=ans, test=test, trial=trial, k=k, opt=opt)
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
INTEGER(I4B) :: ips, ipt, ii, jj, nsd
  !!
  !! main
CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
CALL getInterpolation(obj=trial, ans=kbar, val=k)
  !!
nsd = trial(1)%nsd
  !!
DO ipt = 1, SIZE(trial)
    !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
   & * trial(ipt)%wt * trial(ipt)%jt
    !!
  DO ips = 1, SIZE(realval)
      !!
    DO jj = 1, nsd
        !!
      DO ii = 1, nsd
          !!
        IaJb = IaJb + realval(ips) * kbar(ii, jj, ips, ipt) * &
          & OUTERPROD(test(ipt)%dNTdXt(:, :, ii, ips), &
          & trial(ipt)%dNTdXt(:, :, jj, ips))
          !!
      END DO
    END DO
  END DO
END DO
  !!
CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
DEALLOCATE (realval, KBar, IaJb)
END PROCEDURE mat4_STDiffusionMatrix_4

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_5
  !!
  !! scalar
  !! scalar
  !!
! CALL STDM_6(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !!
  !! Internal variable
  !!
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: c2bar(:, :)
INTEGER(I4B) :: ips, ipt, ii, nsd
  !!
  !! main
  !!
CALL GetInterpolation(obj=trial, ans=c1bar, val=c1)
CALL GetInterpolation(obj=trial, ans=c2bar, val=c2)
  !!
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
nsd = trial(1)%nsd
  !!
DO ipt = 1, SIZE(trial)
    !!
  realval = trial(ipt)%Js * trial(ipt)%Ws * trial(ipt)%Thickness &
   & * trial(ipt)%Wt * trial(ipt)%Jt * c1bar(:, ipt) * c2bar(:, ipt)
    !!
  DO ips = 1, SIZE(realval)
      !!
    DO ii = 1, nsd
        !!
      iajb = iajb + realval(ips) &
        & * OUTERPROD(test(ipt)%dNTdXt(:, :, ii, ips),&
        & trial(ipt)%dNTdXt(:, :, ii, ips))
        !!
    END DO
  END DO
END DO
  !!
CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
DEALLOCATE (realval, iajb, c1bar, c2bar)
END PROCEDURE mat4_STDiffusionMatrix_5

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_6
  !!
  !! scalar
  !! vector
  !!
! CALL STDM_7(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !! Internal variable
  !!
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1bar(:, :)
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: p1(:, :, :)
REAL(DFP), ALLOCATABLE :: p2(:, :, :)
INTEGER(I4B) :: ips, ipt
  !!
  !! main
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
CALL GetInterpolation(obj=trial, ans=c1bar, val=c1)
  !!
DO ipt = 1, SIZE(trial)
    !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
   & * trial(ipt)%wt * trial(ipt)%jt * c1bar(:, ipt)
  CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p1, val=c2)
  CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p2, val=c2)
    !!
  DO ips = 1, SIZE(realval)
      !!
    iajb = iajb + realval(ips) * OUTERPROD(p1(:, :, ips), p2(:, :, ips))
      !!
  END DO
END DO
  !!
CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
DEALLOCATE (realval, c1bar, iajb, p1, p2)
  !!
END PROCEDURE mat4_STDiffusionMatrix_6

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_7
  !!
  !! scalar
  !! matrix
  !!
! CALL STDM_5(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !! Internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: rhobar(:, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
INTEGER(I4B) :: ips, ipt, ii, jj, nsd
  !!
  !! main
  !!
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
CALL GetInterpolation(obj=trial, ans=rhobar, val=c1)
CALL GetInterpolation(obj=trial, ans=kbar, val=c2)
  !!
nsd = trial(1)%nsd
  !!
DO ipt = 1, SIZE(trial)
    !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
   & * trial(ipt)%wt * trial(ipt)%jt * rhobar(:, ipt)
    !!
  DO ips = 1, SIZE(realval)
      !!
    DO jj = 1, nsd
        !!
      DO ii = 1, nsd
          !!
        iajb = iajb + realval(ips) * kbar(ii, jj, ips, ipt) * &
            & OUTERPROD(test(ipt)%dNTdXt(:, :, ii, ips), &
            & trial(ipt)%dNTdXt(:, :, jj, ips))
          !!
      END DO
    END DO
  END DO
END DO
  !!
CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
DEALLOCATE (realval, iajb, rhobar, kbar)
END PROCEDURE mat4_STDiffusionMatrix_7

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_8
  !!
  !! vector
  !! scalar
  !!
! CALL STDM_7(ans=ans, test=test, trial=trial, c1=c2, c2=c1, opt=opt)
  !!
ans = STDiffusionMatrix(test=test, trial=trial, c1=c2, c2=c1, &
  & c1rank=TypeFEVariableScalar, c2rank=TypeFEVariableVector, opt=opt)
END PROCEDURE mat4_STDiffusionMatrix_8

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_9
  !!
  !! vector
  !! vector
  !!
! CALL STDM_4(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !! Internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: p1(:, :, :)
REAL(DFP), ALLOCATABLE :: p2(:, :, :)
INTEGER(I4B) :: ips, ipt
  !!
  !! main
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
  & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  !!
DO ipt = 1, SIZE(trial)
    !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
   & * trial(ipt)%wt * trial(ipt)%jt
  CALL GetProjectionOfdNTdXt(obj=test(ipt), cdNTdXt=p1, val=c1)
  CALL GetProjectionOfdNTdXt(obj=trial(ipt), cdNTdXt=p2, val=c2)
    !!
  DO ips = 1, SIZE(realval)
      !!
    iajb = iajb + realval(ips) * OUTERPROD(p1(:, :, ips), p2(:, :, ips))
      !!
  END DO
END DO
  !!
CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
DEALLOCATE (realval, iajb, p1, p2)
END PROCEDURE mat4_STDiffusionMatrix_9

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_10
  !!
  !! vector
  !! matrix
  !!
  !! CALL STDM_10(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
END PROCEDURE mat4_STDiffusionMatrix_10

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_11
  !!
  !! matrix
  !! scalar
  !!
! CALL STDM_5(ans=ans, test=test, trial=trial, c1=c2, c2=c1, opt=opt)
  !!
ans = STDiffusionMatrix(test=test, trial=trial, c1=c2, c2=c1, &
  & c1rank=TypeFEVariableScalar, c2rank=TypeFEVariableMatrix, opt=opt)
  !!
END PROCEDURE mat4_STDiffusionMatrix_11

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_12
  !!
  !! matrix
  !! vector
  !!
  !!CALL STDM_9(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
END PROCEDURE mat4_STDiffusionMatrix_12

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_13
  !!
  !! matrix
  !! matrix
  !!
! CALL STDM_8(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=opt)
  !!
  !! Internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: m2(:, :)
REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
REAL(DFP), ALLOCATABLE :: k1bar(:, :, :, :)
REAL(DFP), ALLOCATABLE :: k2bar(:, :, :, :)
INTEGER(I4B) :: ips, ipt, ii, jj, nsd
  !!
  !! main
CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
CALL getInterpolation(obj=trial, ans=k1bar, val=c1)
CALL getInterpolation(obj=trial, ans=k2bar, val=c2)
nsd = trial(1)%nsd
  !!
DO ipt = 1, SIZE(trial)
    !!
  realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
   & * trial(ipt)%wt * trial(ipt)%jt
    !!
  DO ips = 1, SIZE(realval)
      !!
    m2 = MATMUL(k1bar(:, :, ips, ipt), k2bar(:, :, ips, ipt))
      !!
    DO jj = 1, nsd
        !!
      DO ii = 1, nsd
          !!
        iajb = iajb + realval(ips) * m2(ii, jj) * &
            & OUTERPROD(test(ipt)%dNTdXt(:, :, ii, ips), &
            & trial(ipt)%dNTdXt(:, :, jj, ips))
          !!
      END DO
    END DO
  END DO
END DO
  !!
CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
IF (PRESENT(opt)) CALL MakeDiagonalCopiesIJab(ans, opt)
  !!
DEALLOCATE (realval, m2, iajb, k1bar, k2bar)
  !!
END PROCEDURE mat4_STDiffusionMatrix_13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_14
IF (opt(1) .EQ. 1) THEN
  CALL STDM_11a(ans=ans, test=test, trial=trial, k=k, opt=1)
ELSE
  CALL STDM_11b(ans=ans, test=test, trial=trial, k=k, opt=2)
END IF
END PROCEDURE mat4_STDiffusionMatrix_14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_15
IF (opt(1) .EQ. 1) THEN
  CALL STDM_12a(ans=ans, test=test, trial=trial, k=k, opt=1)
ELSE
  CALL STDM_12b(ans=ans, test=test, trial=trial, k=k, opt=2)
END IF
END PROCEDURE mat4_STDiffusionMatrix_15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_16
IF (opt(1) .EQ. 1) THEN
  CALL STDM_13a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=1)
ELSE
  CALL STDM_13b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=2)
END IF
END PROCEDURE mat4_STDiffusionMatrix_16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STDiffusionMatrix_17
IF (opt(1) .EQ. 1) THEN
  CALL STDM_14a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=1)
ELSE
  CALL STDM_14b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, opt=2)
END IF
END PROCEDURE mat4_STDiffusionMatrix_17

END SUBMODULE Methods
