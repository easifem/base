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

SUBMODULE(TriangleInterpolationUtility) LagrangeBasisMethods
USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde_
USE ErrorHandling, ONLY: Errormsg
USE InputUtility, ONLY: Input
USE GE_CompRoutineMethods, ONLY: GetInvMat
USE GE_LUMethods, ONLY: LUSolve, GetLU

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Triangle
INTEGER(I4B) :: nrow, ncol
nrow = (order + 1) * (order + 2) / 2_I4B
ncol = 2
ALLOCATE (ans(nrow, ncol))
CALL LagrangeDegree_Triangle_(order=order, ans=ans, ncol=ncol, nrow=nrow)
END PROCEDURE LagrangeDegree_Triangle

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle_
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Triangle_
INTEGER(I4B) :: ii, jj, kk

nrow = (order + 1) * (order + 2) / 2_I4B
ncol = 2

kk = 0
DO jj = 0, order
  DO ii = 0, order - jj
    kk = kk + 1
    ans(kk, 1) = ii
    ans(kk, 2) = jj
  END DO
END DO

END PROCEDURE LagrangeDegree_Triangle_

!----------------------------------------------------------------------------
!                                                      LagrangeDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Triangle
ans = (order + 1) * (order + 2) / 2_I4B
END PROCEDURE LagrangeDOF_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Triangle
ans = (order - 1) * (order - 2) / 2_I4B
END PROCEDURE LagrangeInDOF_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Triangle1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info, nrow, ncol

ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP

CALL LagrangeVandermonde_(order=order, xij=xij, elemType=Triangle, ans=V, &
                          nrow=nrow, ncol=ncol)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Triangle1

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Triangle2
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

vtemp = v; ans = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Triangle2

!----------------------------------------------------------------------------
!                                                     LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Triangle3
INTEGER(I4B) :: info
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Triangle3

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Triangle4
INTEGER(I4B) :: basisType0, nrow, ncol
CHARACTER(:), ALLOCATABLE :: ref0

basisType0 = Input(default=Monomial, option=basisType)
ref0 = Input(default="UNIT", option=refTriangle)
CALL LagrangeCoeff_Triangle4_(order=order, xij=xij, basisType=basisType0, &
                              refTriangle=ref0, ans=ans, nrow=nrow, ncol=ncol)
ref0 = ""
END PROCEDURE LagrangeCoeff_Triangle4

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Triangle4
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Triangle4_

SELECT CASE (basisType)

CASE (Monomial)
  CALL LagrangeVandermonde_(order=order, xij=xij, elemType=Triangle, &
                            ans=ans, nrow=nrow, ncol=ncol)

CASE (Jacobi, Orthogonal, Legendre, Lobatto, Ultraspherical)

  CALL Dubiner_Triangle_(order=order, xij=xij, refTriangle=refTriangle, &
                         ans=ans, nrow=nrow, ncol=ncol)

CASE (Heirarchical)

  CALL HeirarchicalBasis_Triangle_(order=order, pe1=order, pe2=order, &
                                pe3=order, xij=xij, refTriangle=refTriangle, &
                                   ans=ans, nrow=nrow, ncol=ncol)
END SELECT

CALL GetInvMat(ans(1:nrow, 1:ncol))

END PROCEDURE LagrangeCoeff_Triangle4_

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Triangle1
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof, ncol, nrow
INTEGER(I4B) :: degree(SIZE(xij, 2), 2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2))

basisType0 = Input(default=Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN

  IF (firstCall0) THEN
    CALL LagrangeCoeff_Triangle_(order=order, xij=xij, &
                              basisType=basisType0, refTriangle=refTriangle, &
                                 ans=coeff, nrow=nrow, ncol=ncol)
    coeff0 = TRANSPOSE(coeff)
  ELSE
    coeff0 = TRANSPOSE(coeff)
  END IF

ELSE

  CALL LagrangeCoeff_Triangle_(order=order, xij=xij, &
                              basisType=basisType0, refTriangle=refTriangle, &
                               ans=coeff0, nrow=nrow, ncol=ncol)
  coeff0 = TRANSPOSE(coeff0)

END IF

SELECT CASE (basisType0)

CASE (Monomial)

  CALL LagrangeDegree_Triangle_(order=order, ans=degree, nrow=nrow, ncol=ncol)

  tdof = SIZE(xij, 2)

  DO ii = 1, tdof
    xx(1, ii) = x(1)**degree(ii, 1) * x(2)**degree(ii, 2)
  END DO

CASE (Heirarchical)

  CALL HeirarchicalBasis_Triangle_(order=order, pe1=order, &
                               pe2=order, pe3=order, xij=RESHAPE(x, [2, 1]), &
                        refTriangle=refTriangle, ans=xx, ncol=ncol, nrow=nrow)

CASE (Jacobi, Orthogonal, Legendre, Lobatto, Ultraspherical)

  CALL Dubiner_Triangle_(order=order, xij=RESHAPE(x, [2, 1]), &
                        refTriangle=refTriangle, ans=xx, nrow=nrow, ncol=ncol)

END SELECT

ans = MATMUL(coeff0, xx(1, :))
END PROCEDURE LagrangeEvalAll_Triangle1

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Triangle2
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof, ncol, nrow
INTEGER(I4B) :: degree(SIZE(xij, 2), 2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(SIZE(x, 2), SIZE(xij, 2))

basisType0 = Input(default=Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN

    CALL LagrangeCoeff_Triangle_(order=order, xij=xij, basisType=basisType0, &
                     refTriangle=refTriangle, ans=coeff, nrow=nrow, ncol=ncol)
    coeff0 = coeff

  ELSE

    coeff0 = coeff

  END IF
ELSE

  CALL LagrangeCoeff_Triangle_(order=order, xij=xij, basisType=basisType0, &
                    refTriangle=refTriangle, ans=coeff0, nrow=nrow, ncol=ncol)

END IF

SELECT CASE (basisType0)

CASE (Monomial)

  CALL LagrangeDegree_Triangle_(order=order, ans=degree, nrow=nrow, ncol=ncol)
  tdof = SIZE(xij, 2)

  DO ii = 1, tdof
    xx(:, ii) = x(1, :)**degree(ii, 1) * x(2, :)**degree(ii, 2)
  END DO

CASE (Heirarchical)

  CALL HeirarchicalBasis_Triangle_(order=order, pe1=order, pe2=order, &
      pe3=order, xij=x, refTriangle=refTriangle, ans=xx, ncol=ncol, nrow=nrow)

CASE (Jacobi, Orthogonal, Legendre, Lobatto, Ultraspherical)

  CALL Dubiner_Triangle_(order=order, xij=x, refTriangle=refTriangle, &
                         ans=xx, nrow=nrow, ncol=ncol)

END SELECT

ans = MATMUL(xx, coeff0)
END PROCEDURE LagrangeEvalAll_Triangle2

!----------------------------------------------------------------------------
!                                           LagrangeGradientEvalAll_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Triangle1
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof, ai, bi, s(3)
INTEGER(I4B) :: degree(SIZE(xij, 2), 2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), &
  & xx(SIZE(x, 2), SIZE(xij, 2), 2), ar, br

basisType0 = Input(default=Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    CALL LagrangeCoeff_Triangle_(order=order, xij=xij, basisType=basisType0, &
                     refTriangle=refTriangle, ans=coeff, nrow=s(1), ncol=s(2))
  END IF

  coeff0 = coeff
ELSE
  CALL LagrangeCoeff_Triangle_(order=order, xij=xij, basisType=basisType0, &
                    refTriangle=refTriangle, ans=coeff0, nrow=s(1), ncol=s(2))
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  CALL LagrangeDegree_Triangle_(order=order, ans=degree, nrow=s(1), ncol=s(2))

  tdof = SIZE(xij, 2)

  DO ii = 1, tdof
    ai = MAX(degree(ii, 1_I4B) - 1_I4B, 0_I4B)
    bi = MAX(degree(ii, 2_I4B) - 1_I4B, 0_I4B)
    ar = REAL(degree(ii, 1_I4B), DFP)
    br = REAL(degree(ii, 2_I4B), DFP)
    xx(:, ii, 1) = (ar * x(1, :)**ai) * x(2, :)**degree(ii, 2)
    xx(:, ii, 2) = x(1, :)**degree(ii, 1) * (br * x(2, :)**bi)
  END DO

CASE (Heirarchical)

 CALL HeirarchicalBasisGradient_Triangle_(order=order, pe1=order, pe2=order, &
             pe3=order, xij=x, refTriangle=refTriangle, ans=xx, tsize1=s(1), &
                                           tsize2=s(2), tsize3=s(3))

CASE (Jacobi, Orthogonal, Legendre, Lobatto, Ultraspherical)

  CALL OrthogonalBasisGradient_Triangle_(order=order, xij=x, &
       refTriangle=refTriangle, ans=xx, tsize1=s(1), tsize2=s(2), tsize3=s(3))

END SELECT

DO ii = 1, 2
  ! ans(:, ii, :) = TRANSPOSE(MATMUL(xx(:, :, ii), coeff0))
  ans(:, :, ii) = MATMUL(xx(:, :, ii), coeff0)
END DO

END PROCEDURE LagrangeGradientEvalAll_Triangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE LagrangeBasisMethods
