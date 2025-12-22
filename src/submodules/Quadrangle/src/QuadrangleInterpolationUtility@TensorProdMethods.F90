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

SUBMODULE(QuadrangleInterpolationUtility) TensorProdMethods
USE LineInterpolationUtility, ONLY: BasisEvalAll_Line_, &
                                    BasisGradientEvalAll_Line_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                              TensorProdOrthoPol_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle1
INTEGER(I4B) :: nrow, ncol
CALL TensorProdBasis_Quadrangle1_( &
  p=p, q=q, xij=xij, ans=ans, nrow=nrow, ncol=ncol, basisType1=basisType1, &
  basisType2=basisType2, alpha1=alpha1, beta1=beta1, lambda1=lambda1, &
  alpha2=alpha2, beta2=beta2, lambda2=lambda2)
END PROCEDURE TensorProdBasis_Quadrangle1

!----------------------------------------------------------------------------
!                                                 TensorProdBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle1_
REAL(DFP) :: P1(SIZE(xij, 2), p + 1), Q1(SIZE(xij, 2), q + 1)
INTEGER(I4B) :: k1, k2, ii

nrow = SIZE(xij, 2)
ncol = (p + 1) * (q + 1)

CALL BasisEvalAll_Line_( &
  order=p, x=xij(1, :), refLine="BIUNIT", basisType=basisType1, &
  alpha=alpha1, beta=beta1, lambda=lambda1, ans=P1, nrow=k1, ncol=k2)

CALL BasisEvalAll_Line_( &
  order=q, x=xij(2, :), refLine="BIUNIT", basisType=basisType1, &
  alpha=alpha2, beta=beta2, lambda=lambda2, ans=Q1, nrow=k1, ncol=k2)

DO CONCURRENT(k1=1:p + 1, k2=1:q + 1, ii=1:nrow)
  ans(ii, (k2 - 1) * (p + 1) + k1) = P1(ii, k1) * Q1(ii, k2)
END DO

END PROCEDURE TensorProdBasis_Quadrangle1_

!----------------------------------------------------------------------------
!                                              TensorProdOrthoPol_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle2
INTEGER(I4B) :: nrow, ncol
CALL TensorProdBasis_Quadrangle2_( &
  p=p, q=q, x=x, y=y, ans=ans, nrow=nrow, ncol=ncol, basisType1=basisType1, &
  basisType2=basisType2, alpha1=alpha1, beta1=beta1, lambda1=lambda1, &
  alpha2=alpha2, beta2=beta2, lambda2=lambda2)
END PROCEDURE TensorProdBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                              TensorProdOrthoPol_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle2_
REAL(DFP) :: xij(2, SIZE(x) * SIZE(y))
INTEGER(I4B) :: ii, jj

nrow = SIZE(x)
ncol = SIZE(y)

DO CONCURRENT(ii=1:nrow, jj=1:ncol)
  xij(1, ncol * (ii - 1) + jj) = x(ii)
  xij(2, ncol * (ii - 1) + jj) = y(jj)
END DO

CALL TensorProdBasis_Quadrangle1_( &
  p=p, q=q, xij=xij, basisType1=basisType1, basisType2=basisType2, &
  alpha1=alpha1, alpha2=alpha2, beta1=beta1, beta2=beta2, lambda1=lambda1, &
  lambda2=lambda2, ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE TensorProdBasis_Quadrangle2_

!----------------------------------------------------------------------------
!                                       TensorProdBasisGradient_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1
INTEGER(I4B) :: dim1, dim2, dim3
CALL TensorProdBasisGradient_Quadrangle1_( &
  p=p, q=q, xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
  basisType1=basisType1, basisType2=basisType2, alpha1=alpha1, &
  beta1=beta1, lambda1=lambda1, alpha2=alpha2, beta2=beta2, lambda2=lambda2)
END PROCEDURE TensorProdBasisGradient_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1_
REAL(DFP) :: P1(SIZE(xij, 2), p + 1), Q1(SIZE(xij, 2), q + 1)
REAL(DFP) :: dP1(SIZE(xij, 2), p + 1), dQ1(SIZE(xij, 2), q + 1)
INTEGER(I4B) :: k1, k2, cnt, indx(3)

dim1 = SIZE(xij, 2)
dim2 = (p + 1) * (q + 1)
dim3 = 2

! P1
CALL BasisEvalAll_Line_( &
  order=p, x=xij(1, :), refLine="BIUNIT", basisType=basisType1, &
  alpha=alpha1, beta=beta1, lambda=lambda1, ans=P1, nrow=indx(1), &
  ncol=indx(2))

! Q1 = BasisEvalAll_Line( &
CALL BasisEvalAll_Line_( &
  order=q, x=xij(2, :), refLine="BIUNIT", basisType=basisType1, &
  alpha=alpha2, beta=beta2, lambda=lambda2, ans=Q1, nrow=indx(1), &
  ncol=indx(2))

! dP1 = BasisGradientEvalAll_Line( &
CALL BasisGradientEvalAll_Line_( &
  order=p, x=xij(1, :), refLine="BIUNIT", basisType=basisType1, &
  alpha=alpha1, beta=beta1, lambda=lambda1, ans=dP1, nrow=indx(1), &
  ncol=indx(2))

! dQ1 = BasisGradientEvalAll_Line( &
CALL BasisGradientEvalAll_Line_( &
  order=q, x=xij(2, :), refLine="BIUNIT", basisType=basisType1, &
  alpha=alpha2, beta=beta2, lambda=lambda2, ans=dQ1, nrow=indx(1), &
  ncol=indx(2))

cnt = 0

DO k2 = 1, q + 1

  DO k1 = 1, p + 1
    cnt = cnt + 1
    ans(1:dim1, cnt, 1) = dP1(1:dim1, k1) * Q1(1:dim1, k2)
    ans(1:dim1, cnt, 2) = P1(1:dim1, k1) * dQ1(1:dim1, k2)
  END DO

END DO

END PROCEDURE TensorProdBasisGradient_Quadrangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE TensorProdMethods
