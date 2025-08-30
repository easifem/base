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

SUBMODULE(MassMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2a(ans, test, trial, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  ! vector variable
  ! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ii, ips

  ! main
  CALL GetInterpolation(obj=trial, ans=vbar, val=rho)
  CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), SIZE(vbar, 1), 1)
  realval = trial%js * trial%ws * trial%thickness

  DO ips = 1, SIZE(realval)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO ii = 1, SIZE(vbar, 1)
      m4(:, :, ii, 1) = m4(:, :, ii, 1) &
        & + realval(ips) * vbar(ii, ips) * m2
    END DO
  END DO

  CALL Convert(From=m4, To=ans)
  DEALLOCATE (realval, m2, vbar, m4)
END SUBROUTINE MM_2a

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2b(ans, test, trial, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  ! vector variable

  ! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ii, ips

  ! main
  CALL GetInterpolation(obj=trial, ans=vbar, val=rho)
  CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), 1, SIZE(vbar, 1))
  realval = trial%js * trial%ws * trial%thickness

  DO ips = 1, SIZE(realval)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO ii = 1, SIZE(vbar, 1)
      m4(:, :, 1, ii) = m4(:, :, 1, ii) &
        & + realval(ips) * vbar(ii, ips) * m2
    END DO
  END DO

  CALL Convert(From=m4, To=ans)
  DEALLOCATE (realval, m2, vbar, m4)
END SUBROUTINE MM_2b

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2c(ans, test, trial, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  ! vector variable
  ! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ips, ii

  ! main
  CALL GetInterpolation(obj=trial, ans=vbar, val=rho)
  CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
    & SIZE(vbar, 1), SIZE(vbar, 1))

  realval = trial%js * trial%ws * trial%thickness

  DO ips = 1, SIZE(vbar, 2)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO ii = 1, SIZE(vbar, 1)
      m4(:, :, ii, ii) = m4(:, :, ii, ii) &
        & + realval(ips) * vbar(ii, ips) * m2
    END DO
  END DO

  CALL Convert(from=m4, to=ans)

  DEALLOCATE (realval, m2, vbar, m4)
END SUBROUTINE MM_2c

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2d(ans, test, trial, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  ! vector variable
  ! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ips, ii, jj

  ! main
  CALL GetInterpolation(obj=trial, ans=vbar, val=rho)
  CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
    & SIZE(vbar, 1), SIZE(vbar, 1))

  realval = trial%js * trial%ws * trial%thickness

  DO ips = 1, SIZE(vbar, 2)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO jj = 1, SIZE(vbar, 1)
      DO ii = 1, SIZE(vbar, 1)
        m4(:, :, ii, jj) = m4(:, :, ii, jj) &
          & + realval(ips) * vbar(ii, ips) &
          & * vbar(jj, ips) * m2
      END DO
    END DO
  END DO

  CALL Convert(from=m4, to=ans)

  DEALLOCATE (realval, m2, vbar, m4)
END SUBROUTINE MM_2d

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_1
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ips

! main
CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
realval = trial%js * trial%ws * trial%thickness

DO ips = 1, SIZE(trial%N, 2)
  ans = ans + realval(ips) * &
    & OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
END DO

IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (realval)
END PROCEDURE MassMatrix_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Massmatrix1_
REAL(DFP), PARAMETER :: one = 1.0_DFP
REAL(DFP) :: realval
INTEGER(I4B) :: ii, jj, ips

nrow = test%nns
ncol = trial%nns
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, trial%nips
  realval = trial%js(ips) * trial%ws(ips) * trial%thickness(ips)

  CALL OuterProd_(a=test%N(1:nrow, ips), &
                  b=trial%N(1:ncol, ips), &
                  nrow=ii, ncol=jj, ans=ans, scale=realval, anscoeff=one)

END DO

IF (PRESENT(opt)) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE Massmatrix1_

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_2
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ips

! main
CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
CALL GetInterpolation(obj=trial, ans=realval, val=rho)
realval = trial%js * trial%ws * trial%thickness * realval

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * &
    & OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
END DO

IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
DEALLOCATE (realval)
END PROCEDURE MassMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix2_
REAL(DFP) :: realval(trial%nips)
REAL(DFP), PARAMETER :: one = 1.0_DFP
INTEGER(I4B) :: ips, ii, jj

nrow = test%nns
ncol = trial%nns
realval = 0.0_DFP
CALL GetInterpolation_(obj=trial, ans=realval, val=rho, tsize=ii)
realval = trial%js * trial%ws * trial%thickness * realval

DO ips = 1, SIZE(realval)
  CALL OuterProd_(a=test%N(1:nrow, ips), &
                  b=trial%N(1:ncol, ips), &
                  nrow=ii, ncol=jj, ans=ans, scale=realval(ips), &
                  anscoeff=one)
END DO

IF (PRESENT(opt)) THEN
  CALL MakeDiagonalCopies_(mat=ans, ncopy=opt, nrow=nrow, ncol=ncol)
  nrow = opt * nrow
  ncol = opt * ncol
END IF

END PROCEDURE MassMatrix2_

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_3
SELECT CASE (opt)
CASE (1)
  CALL MM_2a(ans=ans, test=test, trial=trial, rho=rho)
CASE (2)
  CALL MM_2b(ans=ans, test=test, trial=trial, rho=rho)
CASE (3)
  CALL MM_2c(ans=ans, test=test, trial=trial, rho=rho)
CASE (4)
  CALL MM_2d(ans=ans, test=test, trial=trial, rho=rho)
END SELECT
END PROCEDURE MassMatrix_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE massmatrix3_
! TODO: implement
END PROCEDURE massmatrix3_

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_4
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: m2(:, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :)
REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
INTEGER(I4B) :: ii, jj, ips

! main
CALL GetInterpolation(obj=trial, ans=kbar, val=rho)
CALL Reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
  & SIZE(kbar, 1), SIZE(kbar, 2))

realval = trial%js * trial%ws * trial%thickness

DO ips = 1, SIZE(realval)
  m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
  DO jj = 1, SIZE(kbar, 2)
    DO ii = 1, SIZE(kbar, 1)
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * kbar(ii, jj, ips) * m2
    END DO
  END DO
END DO

CALL Convert(From=m4, To=ans)
DEALLOCATE (realval, m2, kbar, m4)
END PROCEDURE MassMatrix_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix4_
! TODO: implement
END PROCEDURE MassMatrix4_

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_5
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: m2(:, :), eyemat(:, :), nij(:, :)
REAL(DFP), ALLOCATABLE :: lambdaBar(:)
REAL(DFP), ALLOCATABLE :: muBar(:)
REAL(DFP), ALLOCATABLE :: rhoBar(:)
REAL(DFP), ALLOCATABLE :: acoeff(:)
REAL(DFP), ALLOCATABLE :: bcoeff(:)
REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
INTEGER(I4B) :: ii, jj, ips, nsd, nns

! main
CALL GetInterpolation(obj=trial, ans=lambdaBar, val=lambda)
CALL GetInterpolation(obj=trial, ans=muBar, val=mu)
CALL GetInterpolation(obj=trial, ans=rhoBar, val=rho)

ALLOCATE (acoeff(SIZE(lambdaBar, 1)), bcoeff(SIZE(lambdaBar, 1)))

bcoeff = SQRT(rhoBar * muBar)
acoeff = SQRT(rhoBar * (lambdaBar + 2.0_DFP * muBar)) - bcoeff

nsd = trial%nsd
eyemat = Eye(nsd, 1.0_DFP)
nns = SIZE(test%N, 1)
ALLOCATE (m4(nns, nns, nsd, nsd))

realval = trial%js * trial%ws * trial%thickness

DO ips = 1, SIZE(realval)
  m2 = OUTERPROD(a=test%normal(:, ips), b=trial%normal(:, ips))
  nij = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))

  DO jj = 1, nsd
    DO ii = 1, nsd

      m4(:, :, ii, jj) = m4(:, :, ii, jj) + realval(ips) *  &
        & (acoeff(ips) * m2(ii, jj) + bcoeff(ips) * eyemat(ii, jj)) * nij

    END DO
  END DO
END DO

CALL Convert(From=m4, To=ans)

DEALLOCATE (realval, m2, lambdaBar, muBar, rhoBar, acoeff, bcoeff, m4,  &
  & eyemat, nij)
END PROCEDURE MassMatrix_5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
