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

SUBMODULE(LineInterpolationUtility) OrthogonalMethods
USE BaseType, ONLY: polyopt => TypePolynomialOpt
USE StringUtility, ONLY: UpperCase
USE MappingUtility, ONLY: FromUnitLine2BiUnitLine_
USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol_, &
                                       EvalAllOrthopol_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "LineInterpolationUtility@OrthogonalMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                        OrthogonalBasis_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasis_Line1
INTEGER(I4B) :: nrow, ncol
CALL OrthogonalBasis_Line1_(order=order, xij=xij, refline=refline, &
                            basisType=basisType, ans=ans, nrow=nrow, &
                            ncol=ncol, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE OrthogonalBasis_Line1

!----------------------------------------------------------------------------
!                                                     OrthogonalBasis_Line1_
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasis_Line1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "OrthogonalBasis_Line1_()"
LOGICAL(LGT) :: isok, abool
#endif

CHARACTER(1) :: astr
REAL(DFP) :: x(SIZE(xij, 2))

nrow = SIZE(xij, 2)
ncol = order + 1
ans(1:nrow, 1:ncol) = 0.0_DFP

#ifdef DEBUG_VER
abool = basisType .EQ. polyopt%Jacobi
IF (abool) THEN
  isok = PRESENT(alpha) .AND. PRESENT(beta)
  CALL AssertError1(isok, myName, modName, __LINE__, &
                    "alpha and beta should be present for basisType=Jacobi")
END IF

abool = basisType .EQ. polyopt%Ultraspherical
IF (abool) THEN
  isok = PRESENT(lambda)
  CALL AssertError1(isok, myName, modName, __LINE__, &
                    "lambda should be present for basisType=Ultraspherical")
END IF
#endif

astr = UpperCase(refLine(1:1))

SELECT CASE (astr)
CASE ("U")
  CALL FromUnitLine2BiUnitLine_(xin=xij(1, :), ans=x, tsize=nrow)
  CALL EvalAllOrthopol_(n=order, x=x, orthopol=basisType, alpha=alpha, &
                        beta=beta, lambda=lambda, ans=ans, nrow=nrow, &
                        ncol=ncol)

CASE ("B")
  CALL EvalAllOrthopol_(n=order, x=xij(1, :), orthopol=basisType, &
                        alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                        nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, modName, __LINE__, &
                    "No case found for refLine.")
#endif
END SELECT
END PROCEDURE OrthogonalBasis_Line1_

!----------------------------------------------------------------------------
!                                            OrthogonalBasisGradient_Line1
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Line1
INTEGER(I4B) :: dim1, dim2, dim3
CALL OrthogonalBasisGradient_Line1_( &
  order=order, xij=xij, refline=refline, basisType=basisType, ans=ans, &
  dim1=dim1, dim2=dim2, dim3=dim3, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE OrthogonalBasisGradient_Line1

!----------------------------------------------------------------------------
!                                                OrthogonalBasisGradient_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Line1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "OrthogonalBasisGradient_Line1_()"
#endif

CHARACTER(1) :: astr
REAL(DFP) :: x(SIZE(xij, 2))
INTEGER(I4B) :: ii, jj

astr = UpperCase(refline(1:1))
dim1 = SIZE(xij, 2)
dim2 = order + 1
dim3 = 1

SELECT CASE (astr)
CASE ("U")
  CALL FromUnitLine2BiUnitLine_(xin=xij(1, :), ans=x, tsize=dim1)
  CALL GradientEvalAllOrthopol_(n=order, x=x, orthopol=basisType, &
                                ans=ans(:, :, 1), nrow=dim1, ncol=dim2)

  DO CONCURRENT(ii=1:dim1, jj=1:dim2)
    ans(ii, jj, 1) = ans(ii, jj, 1) * 2.0_DFP
  END DO

CASE ("B")
  CALL GradientEvalAllOrthopol_(n=order, x=xij(1, :), orthopol=basisType, &
                                ans=ans(:, :, 1), nrow=dim1, ncol=dim2)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, modName, __LINE__, &
                    "No case found for refline")
#endif
END SELECT
END PROCEDURE OrthogonalBasisGradient_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE OrthogonalMethods
