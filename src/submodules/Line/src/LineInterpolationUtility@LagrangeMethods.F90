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

SUBMODULE(LineInterpolationUtility) LagrangeMethods
USE BaseType, ONLY: polyopt => TypePolynomialOpt, elmopt => TypeElemNameOpt
USE Display_Method, ONLY: ToString
USE InputUtility, ONLY: Input
USE Lapack_Method, ONLY: GetLU, LUSolve, GetInvMat
USE F95_BLAS, ONLY: GEMM
USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol_, &
                                       EvalAllOrthopol_
USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde_

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "LineInterpolationUtility@LagrangeMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                       LagrangeDegree_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Line
INTEGER(I4B) :: ii, n
n = LagrangeDOF_Line(order=order)
ALLOCATE (ans(n, 1))
DO ii = 1, n
  ans(ii, 1) = ii - 1
END DO
END PROCEDURE LagrangeDegree_Line

!----------------------------------------------------------------------------
!                                                          LagrangeDOF_Point
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Point
ans = 1_I4B
END PROCEDURE LagrangeDOF_Point

!----------------------------------------------------------------------------
!                                                         LagrangeDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Line
ans = order + 1
END PROCEDURE LagrangeDOF_Line

!----------------------------------------------------------------------------
!                                                         LagrangeInDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Line
ans = order - 1_I4B
END PROCEDURE LagrangeInDOF_Line

!----------------------------------------------------------------------------
!                                                         GetTotalDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Line
ans = order + 1
END PROCEDURE GetTotalDOF_Line

!----------------------------------------------------------------------------
!                                                         LagrangeInDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Line
ans = order - 1_I4B
IF (ans .LT. 0_I4B) ans = 0_I4B
END PROCEDURE GetTotalInDOF_Line

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line1
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Line1_(order=order, i=i, xij=xij, ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Line1

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line1_
REAL(DFP) :: v(SIZE(xij, 2), SIZE(xij, 2))
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info, nrow, ncol

tsize = order + 1
CALL LagrangeVandermonde_(order=order, xij=xij, elemType=elmopt%Line, &
                          ans=v, nrow=nrow, ncol=ncol)

CALL GetLU(A=v, IPIV=ipiv, info=info)

ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP

CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Line1_

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line2
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Line2_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                          ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Line2

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line2_
REAL(DFP) :: vtemp(SIZE(v, 1), SIZE(v, 2))
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

tsize = order + 1

vtemp = v
! ipiv = 0

CALL GetLU(A=vtemp, IPIV=ipiv, info=info)

ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP

CALL LUSolve(A=vtemp, B=ans(1:tsize), IPIV=ipiv, info=info)

END PROCEDURE LagrangeCoeff_Line2_

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Line3_(order=order, i=i, v=v, ipiv=ipiv, ans=ans, &
                          tsize=tsize)
END PROCEDURE LagrangeCoeff_Line3

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line3_
INTEGER(I4B) :: info
tsize = 1 + order
ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Line3_

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line4
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Line4_(order=order, xij=xij, ans=ans, nrow=nrow, &
                          ncol=ncol)
END PROCEDURE LagrangeCoeff_Line4

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line4_
CALL LagrangeVandermonde_(order=order, xij=xij, elemType=elmopt%Line, &
                          ans=ans, nrow=nrow, ncol=ncol)
CALL GetInvMat(ans(1:nrow, 1:ncol))
END PROCEDURE LagrangeCoeff_Line4_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line5
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Line5_( &
  order=order, xij=xij, basisType=basisType, alpha=alpha, beta=beta, &
  lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Line5

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line5_
IF (basisType .EQ. polyopt%Monomial) THEN
  CALL LagrangeCoeff_Line_(order=order, xij=xij, ans=ans, nrow=nrow, &
                           ncol=ncol)
  RETURN
END IF

CALL EvalAllOrthopol_(n=order, x=xij(1, :), orthopol=basisType, alpha=alpha, &
                      beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)

CALL GetInvMat(ans(1:nrow, 1:ncol))
END PROCEDURE LagrangeCoeff_Line5_

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line1
INTEGER(I4B) :: tsize
CALL LagrangeEvalAll_Line1_( &
  order=order, x=x, xij=xij, coeff=coeff, firstCall=firstCall, &
  basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
  tsize=tsize)
END PROCEDURE LagrangeEvalAll_Line1

!----------------------------------------------------------------------------
!                                                     LagrangeEvalAll_Line_
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LagrangeEvalAll_Line1_()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2)), x1(1)
INTEGER(I4B) :: ii, orthopol0, nrow, ncol

tsize = SIZE(xij, 2)

#ifdef DEBUG_VER
isok = tsize .EQ. order + 1
CALL AssertError1(isok, myName, modName, __LINE__, &
    'Size(xij, 1)='//ToString(tsize)//' .NE. order+1 = '//ToString(order + 1))
#endif

orthopol0 = Input(default=polyopt%Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

! make coeff0

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                             alpha=alpha, beta=beta, lambda=lambda, &
                             ans=coeff, nrow=nrow, ncol=ncol)
  END IF
  coeff0(1:tsize, 1:tsize) = coeff(1:tsize, 1:tsize)

ELSE
  CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                           alpha=alpha, beta=beta, lambda=lambda, &
                           ans=coeff0, nrow=nrow, ncol=ncol)
END IF

IF (orthopol0 .EQ. polyopt%monomial) THEN

  xx(1, 1) = 1.0_DFP
  DO ii = 1, order
    xx(1, ii + 1) = xx(1, ii) * x
  END DO

ELSE

  x1(1) = x
  CALL EvalAllOrthopol_(n=order, x=x1, orthopol=orthopol0, &
                        alpha=alpha, beta=beta, lambda=lambda, &
                        ans=xx, nrow=nrow, ncol=ncol)

END IF

DO CONCURRENT(ii=1:tsize)
  ans(ii) = DOT_PRODUCT(coeff0(:, ii), xx(1, :))
END DO
END PROCEDURE LagrangeEvalAll_Line1_

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line2
INTEGER(I4B) :: nrow, ncol
CALL LagrangeEvalAll_Line2_(order=order, x=x, xij=xij, coeff=coeff, &
                            firstCall=firstCall, basisType=basisType, &
                            alpha=alpha, beta=beta, lambda=lambda, &
                            ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeEvalAll_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LagrangeEvalAll_Line2_()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(SIZE(x, 2), SIZE(xij, 2))
INTEGER(I4B) :: ii, orthopol0, aint, bint

nrow = SIZE(x, 2)
ncol = SIZE(xij, 2)

#ifdef DEBUG_VER
isok = ncol .EQ. order + 1
CALL AssertError1(isok, myName, modName, __LINE__, &
     'Size(xij, 2)='//ToString(ncol)//' .NE. order+1 = '//ToString(order + 1))
#endif

orthopol0 = Input(default=polyopt%Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN

  IF (firstCall0) THEN
    ! coeff = LagrangeCoeff_Line(&
    CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                             alpha=alpha, beta=beta, lambda=lambda, &
                             ans=coeff, nrow=aint, ncol=bint)
  END IF

  coeff0(1:ncol, 1:ncol) = coeff(1:ncol, 1:ncol)

ELSE

  ! coeff0 = LagrangeCoeff_Line(&
  CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                          alpha=alpha, beta=beta, lambda=lambda, ans=coeff0, &
                           nrow=aint, ncol=bint)

END IF

IF (orthopol0 .EQ. polyopt%monomial) THEN

  xx(:, 1) = 1.0_DFP
  DO ii = 1, order
    xx(:, ii + 1) = xx(:, ii) * x(1, :)
  END DO

ELSE

  CALL EvalAllOrthopol_(n=order, x=x(1, :), orthopol=orthopol0, alpha=alpha, &
                       beta=beta, lambda=lambda, ans=xx, nrow=aint, ncol=bint)

END IF

! ans = MATMUL(xx, coeff0)
CALL GEMM(C=ans, alpha=1.0_DFP, A=xx, B=coeff0)

END PROCEDURE LagrangeEvalAll_Line2_

!----------------------------------------------------------------------------
!                                              LagrangeGradientEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Line1
INTEGER(I4B) :: dim1, dim2, dim3
CALL LagrangeGradientEvalAll_Line_( &
  order=order, x=x, xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
  coeff=coeff, firstCall=firstCall, basisType=basisType, alpha=alpha, &
  beta=beta, lambda=lambda)
END PROCEDURE LagrangeGradientEvalAll_Line1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Line1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LagrangeGradientEvalAll_Line1_()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(order + 1, order + 1), xx(SIZE(x, 2), order + 1), areal
INTEGER(I4B) :: ii, orthopol0, indx(2), jj

dim1 = SIZE(x, 2)
dim2 = SIZE(xij, 2)
dim3 = 1

orthopol0 = Input(default=polyopt%Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                             alpha=alpha, beta=beta, lambda=lambda, &
                             ans=coeff, nrow=indx(1), ncol=indx(2))
  END IF
  coeff0(1:dim2, 1:dim2) = coeff(1:dim2, 1:dim2)

ELSE
  CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                           alpha=alpha, beta=beta, lambda=lambda, &
                           ans=coeff0, nrow=indx(1), ncol=indx(2))
END IF

SELECT CASE (orthopol0)
CASE (polyopt%Monomial)

#ifdef DEBUG_VER
  isok = dim2 .EQ. order + 1
  CALL AssertError1(isok, myName, modName, __LINE__, &
                    "size(xij, 2) is not same as order+1")
#endif

  DO ii = 0, order
    indx(1) = MAX(ii - 1_I4B, 0_I4B)
    areal = REAL(ii, kind=DFP)
    DO jj = 1, dim1
      xx(jj, ii + 1) = areal * (x(1, jj)**(indx(1)))
    END DO
  END DO

CASE DEFAULT
  CALL GradientEvalAllOrthopol_(n=order, x=x(1, :), orthopol=orthopol0, &
                                alpha=alpha, beta=beta, lambda=lambda, &
                                ans=xx, nrow=dim1, ncol=dim2)

END SELECT

CALL GEMM(C=ans(1:dim1, 1:dim2, 1), alpha=1.0_DFP, A=xx, B=coeff0)
END PROCEDURE LagrangeGradientEvalAll_Line1_

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LagrangeMethods
