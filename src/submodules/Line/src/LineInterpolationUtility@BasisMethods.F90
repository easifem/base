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

SUBMODULE(LineInterpolationUtility) BasisMethods
USE BaseType, ONLY: polyopt => TypePolynomialOpt
USE Display_Method, ONLY: ToString
USE StringUtility, ONLY: UpperCase
USE InputUtility, ONLY: Input
USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol_, &
                                       EvalAllOrthopol_

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "LineInterpolationUtility@BasisMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                               EvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line1
INTEGER(I4B) :: tsize
CALL BasisEvalAll_Line1_( &
  order=order, x=x, ans=ans, tsize=tsize, refline=refline, &
  basistype=basistype, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE BasisEvalAll_Line1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "BasisEvalAll_Line1_()"
LOGICAL(LGT) :: isok
CHARACTER(1) :: astr
#endif

INTEGER(I4B) :: ii, basisType0, nrow, ncol
REAL(DFP) :: temp(1, 100), x1(1)

tsize = order + 1

#ifdef DEBUG_VER
isok = astr .EQ. "B"
CALL AssertError1(isok, myName, modName, __LINE__, &
                  "refLine should be BIUNIT")
#endif

basisType0 = Input(default=polyopt%Monomial, option=basisType)

SELECT CASE (basisType0)

CASE (polyopt%Monomial)
  ans(1) = 1.0_DFP
  DO ii = 1, order
    ans(ii + 1) = ans(ii) * x
  END DO

CASE DEFAULT

#ifdef DEBUG_VER
  IF (basisType0 .EQ. polyopt%Jacobi) THEN
    isok = PRESENT(alpha) .AND. PRESENT(beta)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                      "alpha and beta should be present for basisType=Jacobi")
  END IF

  IF (basisType0 .EQ. polyopt%Ultraspherical) THEN
    isok = PRESENT(lambda)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                  msg="lambda should be present for basisType=Ultraspherical")
  END IF

  isok = order + 1 .LE. SIZE(temp, 2)
  CALL AssertError1(isok, myName, modName, __LINE__, &
                    "order+1 is greater than number of col in temp")
#endif

  x1(1) = x
  CALL EvalAllOrthopol_(n=order, x=x1, orthopol=basisType0, alpha=alpha, &
                        beta=beta, lambda=lambda, ans=temp, nrow=nrow, &
                        ncol=ncol)

  ans(1:tsize) = temp(1, 1:tsize)

END SELECT

END PROCEDURE BasisEvalAll_Line1_

!----------------------------------------------------------------------------
!                                                 BasisGradientEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line1
INTEGER(I4B) :: tsize
CALL BasisGradientEvalAll_Line1_( &
  order=order, x=x, refLine=refLine, basisType=basisType, alpha=alpha, &
  beta=beta, lambda=lambda, ans=ans, tsize=tsize)
END PROCEDURE BasisGradientEvalAll_Line1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "BasisGradientEvalAll_Line1_()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, basisType0
CHARACTER(1) :: astr
REAL(DFP) :: areal, breal, x1(1), temp(1, order + 1)

tsize = order + 1

astr = UpperCase(refline(1:1))

#ifdef DEBUG_VER
isok = astr .EQ. "B"
CALL AssertError1(isok, myName, modName, __LINE__, &
                  "refline should be BIUNIT")
#endif

basisType0 = Input(default=polyopt%Monomial, option=basisType)

SELECT CASE (basisType0)

CASE (polyopt%Monomial)
  ans(1) = 0.0_DFP
  DO ii = 1, order
    areal = REAL(ii, kind=DFP)
    breal = x**(ii - 1)
    ans(ii + 1) = areal * breal
  END DO

CASE DEFAULT

#ifdef DEBUG_VER
  IF (basisType0 .EQ. polyopt%Jacobi) THEN
    isok = PRESENT(alpha) .AND. PRESENT(beta)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                      "alpha and beta should be present for basisType=Jacobi")
  END IF

  IF (basisType0 .EQ. polyopt%Ultraspherical) THEN
    isok = PRESENT(lambda)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                      "lambda should be present for basisType=Ultraspherical")
  END IF
#endif

  x1(1) = x
  CALL GradientEvalAllOrthopol_(n=order, x=x1, orthopol=basisType0, &
                                alpha=alpha, beta=beta, lambda=lambda, &
                                ans=temp, nrow=ii, ncol=tsize)

  ans(1:tsize) = temp(1, 1:tsize)
END SELECT

END PROCEDURE BasisGradientEvalAll_Line1_

!----------------------------------------------------------------------------
!                                                 BasisGradientEvalAll_Line_
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line2
INTEGER(I4B) :: nrow, ncol
CALL BasisGradientEvalAll_Line2_( &
  order=order, x=x, ans=ans, nrow=nrow, ncol=ncol, refLine=refLine, &
  basisType=basisType, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE BasisGradientEvalAll_Line2

!----------------------------------------------------------------------------
!                                                 BasisGradientEvalAll_Line_
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "BasisGradientEvalAll_Line2_()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, basisType0, jj
REAL(DFP) :: areal, breal
CHARACTER(1) :: astr

nrow = SIZE(x)
ncol = 1 + order

astr = UpperCase(refLine(1:1))

#ifdef DEBUG_VER
isok = astr .EQ. "B"
CALL AssertError1(isok, myName, modName, __LINE__, &
                  "refLine should be Biunit")
#endif

basisType0 = Input(default=polyopt%Monomial, option=basisType)

SELECT CASE (basisType0)

CASE (polyopt%Monomial)
  ans(1:nrow, 1) = 0.0_DFP

  DO ii = 1, order
    areal = REAL(ii, kind=dfp)
    DO jj = 1, nrow
      breal = x(jj)**(ii - 1)
      ans(jj, ii + 1) = areal * breal
    END DO
  END DO

CASE DEFAULT

#ifdef DEBUG_VER
  IF (basisType0 .EQ. polyopt%Jacobi) THEN
    isok = PRESENT(alpha) .AND. PRESENT(beta)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                      "alpha and beta should be present for basisType=Jacobi")
  END IF

  IF (basisType0 .EQ. polyopt%Ultraspherical) THEN
    isok = PRESENT(lambda)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                      "lambda should be present for basisType=Ultraspherical")
  END IF
#endif

  CALL GradientEvalAllOrthopol_(n=order, x=x, orthopol=basisType0, &
                                alpha=alpha, beta=beta, lambda=lambda, &
                                ans=ans, nrow=nrow, ncol=ncol)

END SELECT
END PROCEDURE BasisGradientEvalAll_Line2_

!----------------------------------------------------------------------------
!                                                          BasisEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line2
INTEGER(I4B) :: nrow, ncol
CALL BasisEvalAll_Line2_( &
  order=order, x=x, ans=ans, nrow=nrow, ncol=ncol, refline=refline, &
  basistype=basistype, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE BasisEvalAll_Line2

!----------------------------------------------------------------------------
!                                                           BasisEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "BasisEvalAll_Line2_()"
LOGICAL(LGT) :: isok
CHARACTER(1) :: astr
#endif

INTEGER(I4B) :: ii, basisType0

nrow = SIZE(x)
ncol = order + 1

#ifdef DEBUG_VER
astr = UpperCase(refline(1:1))
isok = astr .EQ. "B"
CALL AssertError1(isok, myName, modName, __LINE__, &
                  "refLine should be Biunit")
#endif

basisType0 = Input(default=polyopt%Monomial, option=basisType)

SELECT CASE (basisType0)

CASE (polyopt%Monomial)
  ans(1:nrow, 1) = 1.0_DFP
  DO ii = 1, order
    ans(1:nrow, ii + 1) = ans(1:nrow, ii) * x
  END DO

CASE DEFAULT

#ifdef DEBUG_VER
  IF (basisType0 .EQ. polyopt%Jacobi) THEN
    isok = PRESENT(alpha) .AND. PRESENT(beta)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                      "alpha and beta should be present for basisType=Jacobi")
  END IF

  IF (basisType0 .EQ. polyopt%Ultraspherical) THEN
    isok = PRESENT(lambda)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                      "lambda should be present for basisType=Ultraspherical")
  END IF
#endif

  CALL EvalAllOrthopol_(n=order, x=x, orthopol=basisType0, alpha=alpha, &
                        beta=beta, lambda=lambda, ans=ans, nrow=nrow, &
                        ncol=ncol)
END SELECT
END PROCEDURE BasisEvalAll_Line2_

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BasisMethods
