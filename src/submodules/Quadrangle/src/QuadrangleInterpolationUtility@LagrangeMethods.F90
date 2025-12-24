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

SUBMODULE(QuadrangleInterpolationUtility) LagrangeMethods
USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde_
USE GE_LUMethods, ONLY: GetLU, LUSolve
USE InputUtility, ONLY: Input
USE ErrorHandling, ONLY: Errormsg
USE F95_BLAS, ONLY: GEMM
USE StringUtility, ONLY: UpperCase
USE GE_CompRoutineMethods, ONLY: GetInvMat

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "QuadrangleInterpolationUtility@LagrangeMethods"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Quadrangle1
ans = (order + 1)**2
END PROCEDURE LagrangeDOF_Quadrangle1

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Quadrangle2
ans = (p + 1) * (q + 1)
END PROCEDURE LagrangeDOF_Quadrangle2

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Quadrangle1
ans = (order - 1)**2
END PROCEDURE LagrangeInDOF_Quadrangle1

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Quadrangle2
ans = (p - 1) * (q - 1)
END PROCEDURE LagrangeInDOF_Quadrangle2

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle1
INTEGER(I4B) :: nrow, ncol
nrow = LagrangeDOF_Quadrangle(order=order)
ALLOCATE (ans(nrow, 2))
CALL LagrangeDegree_Quadrangle1_(ans=ans, nrow=nrow, ncol=ncol, order=order)
END PROCEDURE LagrangeDegree_Quadrangle1

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle1_
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle1_
CALL LagrangeDegree_Quadrangle2_(ans=ans, p=order, q=order, nrow=nrow, &
                                 ncol=ncol)
END PROCEDURE LagrangeDegree_Quadrangle1_

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle2
INTEGER(I4B) :: nrow, ncol

nrow = LagrangeDOF_Quadrangle(p=p, q=q)
ALLOCATE (ans(nrow, 2))
CALL LagrangeDegree_Quadrangle2_(ans=ans, nrow=nrow, ncol=ncol, &
                                 p=p, q=q)
END PROCEDURE LagrangeDegree_Quadrangle2

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle2_
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle2_
INTEGER(I4B) :: ii, jj, p1

nrow = LagrangeDOF_Quadrangle(p=p, q=q)
ncol = 2
p1 = p + 1

DO CONCURRENT(jj=0:q, ii=0:p)
  ans(p1 * jj + ii + 1, 1) = ii
  ans(p1 * jj + ii + 1, 2) = jj
END DO

END PROCEDURE LagrangeDegree_Quadrangle2_

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle2_
!----------------------------------------------------------------------------

MODULE PROCEDURE MonomialBasis_Quadrangle_
INTEGER(I4B) :: ii, jj, p1, ip

nrow = SIZE(xij, 2)
ncol = (p + 1) * (q + 1)

p1 = p + 1

DO CONCURRENT(ii=0:p, jj=0:q, ip=1:nrow)
  ans(ip, p1 * jj + ii + 1) = xij(1, ip)**ii * xij(2, ip)**jj
END DO

END PROCEDURE MonomialBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle1
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Quadrangle1_(order=order, i=i, xij=xij, ans=ans, &
                                tsize=tsize)
END PROCEDURE LagrangeCoeff_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle1_
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info, nrow, ncol

tsize = SIZE(xij, 2)

ipiv = 0_I4B; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LagrangeVandermonde_( &
  order=order, xij=xij, elemType=TypeElemNameOpt%Quadrangle, ans=V, &
  nrow=nrow, ncol=ncol)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle1_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle2
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Quadrangle2_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                                ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle2_
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

vtemp = v; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle2_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Quadrangle3_(order=order, i=i, v=v, ipiv=ipiv, &
                                ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Quadrangle3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle3_
INTEGER(I4B) :: info
tsize = SIZE(v, 1)
ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle3_

!----------------------------------------------------------------------------
!                                                 LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle4
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Quadrangle4_( &
  order=order, xij=xij, basisType=basisType, alpha=alpha, beta=beta, &
  lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Quadrangle4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle4_
INTEGER(I4B) :: basisType0

basisType0 = Input(default=TypePolynomialOpt%monomial, option=basisType)

IF (basisType0 .EQ. TypePolynomialOpt%hierarchical) THEN
  CALL HeirarchicalBasis_Quadrangle2_(p=order, q=order, xij=xij, &
                                      ans=ans, nrow=nrow, ncol=ncol)
  CALL GetInvMat(ans(1:nrow, 1:ncol))
  RETURN
END IF

! ans(1:nrow, 1:ncol) = TensorProdBasis_Quadrangle1(p=order, q=order, &
CALL TensorProdBasis_Quadrangle1_( &
  p=order, q=order, xij=xij, basisType1=basisType0, basisType2=basisType0, &
  alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
  lambda2=lambda, ans=ans, nrow=nrow, ncol=ncol)

CALL GetInvMat(ans(1:nrow, 1:ncol))

END PROCEDURE LagrangeCoeff_Quadrangle4_

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle5
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Quadrangle5_( &
  p=p, q=q, xij=xij, basisType1=basisType1, basisType2=basisType2, &
  alpha1=alpha1, beta1=beta1, lambda1=lambda1, alpha2=alpha2, beta2=beta2, &
  lambda2=lambda2, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Quadrangle5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle5_
INTEGER(I4B) :: basisType(2)
LOGICAL(LGT) :: isok

basisType(1) = Input(default=TypePolynomialOpt%monomial, option=basisType1)
basisType(2) = Input(default=TypePolynomialOpt%monomial, option=basisType2)

isok = ALL(basisType .EQ. TypePolynomialOpt%hierarchical)
IF (isok) THEN
  ! ans(1:nrow, 1:ncol) = HeirarchicalBasis_Quadrangle2(p=p, q=q, xij=xij)
  CALL HeirarchicalBasis_Quadrangle2_(p=p, q=q, xij=xij, &
                                      ans=ans, nrow=nrow, ncol=ncol)

  CALL GetInvMat(ans(1:nrow, 1:ncol))
  RETURN
END IF

CALL TensorProdBasis_Quadrangle1_( &
  p=p, q=q, xij=xij, basisType1=basisType(1), alpha1=alpha1, beta1=beta1, &
  lambda1=lambda1, basisType2=basisType(2), alpha2=alpha2, beta2=beta2, &
  lambda2=lambda2, ans=ans, nrow=nrow, ncol=ncol)

CALL GetInvMat(ans(1:nrow, 1:ncol))

END PROCEDURE LagrangeCoeff_Quadrangle5_

!----------------------------------------------------------------------------
!                                                LagrangeEvallAll_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle1
INTEGER(I4B) :: tsize
CALL LagrangeEvalAll_Quadrangle1_( &
  order=order, x=x, xij=xij, ans=ans, tsize=tsize, coeff=coeff, &
  firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
  lambda=lambda)
END PROCEDURE LagrangeEvalAll_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle1_
LOGICAL(LGT) :: firstCall0, isCoeff
INTEGER(I4B) :: ii, basisType0, degree(SIZE(xij, 2), 2), indx(2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2)), &
             x21(2, 1)

tsize = SIZE(xij, 2)

basisType0 = INPUT(default=TypePolynomialOpt%monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

isCoeff = PRESENT(coeff)
IF (isCoeff) THEN

  IF (firstCall0) THEN
    CALL LagrangeCoeff_Quadrangle_( &
      order=order, xij=xij, basisType=basisType0, alpha=alpha, beta=beta, &
      lambda=lambda, ans=coeff, nrow=indx(1), ncol=indx(2))
  END IF

  ! coeff0 = TRANSPOSE(coeff)
  coeff0(1:tsize, 1:tsize) = coeff(1:tsize, 1:tsize)

ELSE

  CALL LagrangeCoeff_Quadrangle_( &
    order=order, xij=xij, basisType=basisType0, alpha=alpha, beta=beta, &
    lambda=lambda, ans=coeff0, nrow=indx(1), ncol=indx(2))

  ! coeff0 = TRANSPOSE(coeff0)

END IF

SELECT CASE (basisType0)

CASE (TypePolynomialOpt%monomial)

  CALL LagrangeDegree_Quadrangle_(order=order, ans=degree, nrow=indx(1), &
                                  ncol=indx(2))
#ifdef DEBUG_VER

  IF (tsize .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(msg="tdof is not same as size(degree,1)", &
                  routine="LagrangeEvalAll_Quadrangle1", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

#endif

  DO ii = 1, tsize
    indx(1:2) = degree(ii, 1:2)
    xx(1, ii) = x(1)**indx(1) * x(2)**indx(2)
  END DO

CASE (TypePolynomialOpt%hierarchical)

  ! xx = HeirarchicalBasis_Quadrangle( &
  x21(1:2, 1) = x(1:2)
  CALL HeirarchicalBasis_Quadrangle_( &
    p=order, q=order, xij=x21, ans=xx, nrow=indx(1), ncol=indx(2))

CASE DEFAULT

  x21(1:2, 1) = x(1:2)
  CALL TensorProdBasis_Quadrangle_( &
    p=order, q=order, xij=x21, basisType1=basisType0, basisType2=basisType0, &
    alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
    lambda2=lambda, ans=xx, nrow=indx(1), ncol=indx(2))

END SELECT

DO CONCURRENT(ii=1:tsize)
  ans(ii) = DOT_PRODUCT(coeff0(:, ii), xx(1, :))
END DO

END PROCEDURE LagrangeEvalAll_Quadrangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle2
INTEGER(I4B) :: nrow, ncol
CALL LagrangeEvalAll_Quadrangle2_( &
  order=order, x=x, xij=xij, ans=ans, nrow=nrow, ncol=ncol, coeff=coeff, &
  firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
  lambda=lambda)
END PROCEDURE LagrangeEvalAll_Quadrangle2

!----------------------------------------------------------------------------
!                                               LagrangeEvalAll_Quadrangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle2_
LOGICAL(LGT) :: isok, firstCall0
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(SIZE(x, 2), SIZE(xij, 2))

firstCall0 = Input(default=.TRUE., option=firstCall)
isok = PRESENT(coeff)

IF (isok) THEN

  CALL LagrangeEvalAll_Quadrangle_( &
    order=order, x=x, xij=xij, ans=ans, nrow=nrow, ncol=ncol, coeff=coeff, &
    xx=xx, firstCall=firstCall0, basisType=basisType, alpha=alpha, &
    beta=beta, lambda=lambda)

ELSE

  CALL LagrangeEvalAll_Quadrangle_( &
    order=order, x=x, xij=xij, ans=ans, nrow=nrow, ncol=ncol, coeff=coeff0, &
    xx=xx, firstCall=firstCall0, basisType=basisType, alpha=alpha, &
    beta=beta, lambda=lambda)

END IF
END PROCEDURE LagrangeEvalAll_Quadrangle2_

!----------------------------------------------------------------------------
!                                                LagrangeEvalAll_Quadrangle_
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle3_
INTEGER(I4B) :: basisType0, indx(2)

! coeff0(SIZE(xij, 2), SIZE(xij, 2))
! xx(SIZE(x, 2), SIZE(xij, 2))
! degree(SIZE(xij, 2), 2)

nrow = SIZE(x, 2)
ncol = SIZE(xij, 2)

basisType0 = INPUT(default=TypePolynomialOpt%Monomial, option=basisType)

! coeff = LagrangeCoeff_Quadrangle(&
IF (firstCall) &
  CALL LagrangeCoeff_Quadrangle_( &
  order=order, xij=xij, basisType=basisType0, alpha=alpha, beta=beta, &
  lambda=lambda, ans=coeff, nrow=indx(1), ncol=indx(2))

SELECT CASE (basisType0)

CASE (TypePolynomialOpt%Monomial)
  CALL MonomialBasis_Quadrangle_(p=order, q=order, xij=x, ans=xx, &
                                 nrow=indx(1), ncol=indx(2))

CASE (TypePolynomialOpt%Hierarchical)
  CALL HeirarchicalBasis_Quadrangle_(p=order, q=order, xij=x, ans=xx, &
                                     nrow=indx(1), ncol=indx(2))

CASE DEFAULT
  CALL TensorProdBasis_Quadrangle_( &
    p=order, q=order, xij=x, basisType1=basisType0, basisType2=basisType0, &
    alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
    lambda2=lambda, ans=xx, nrow=indx(1), ncol=indx(2))

END SELECT

! indx(1) should be equal to nrow
! indx(2) should be equal to ncol
! ans = MATMUL(xx, coeff0)
CALL GEMM(C=ans(1:nrow, 1:ncol), alpha=1.0_DFP, A=xx(1:nrow, 1:ncol), &
          B=coeff(1:ncol, 1:ncol))

END PROCEDURE LagrangeEvalAll_Quadrangle3_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Quadrangle1
INTEGER(I4B) :: dim1, dim2, dim3
CALL LagrangeGradientEvalAll_Quadrangle1_( &
  order=order, x=x, xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
  coeff=coeff, firstCall=firstCall, basisType=basisType, alpha=alpha, &
  beta=beta, lambda=lambda)
END PROCEDURE LagrangeGradientEvalAll_Quadrangle1

!----------------------------------------------------------------------------
!                                       LagrangeGradientEvalAll_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Quadrangle1_
LOGICAL(LGT) :: firstCall0, isCoeff
INTEGER(I4B) :: ii, basisType0, ai, bi, indx(3), degree(SIZE(xij, 2), 2), jj
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), &
             xx(SIZE(x, 2), SIZE(xij, 2), 2), ar, br, areal, breal

dim1 = SIZE(x, 2)
dim2 = SIZE(xij, 2)
dim3 = 2

basisType0 = INPUT(default=TypePolynomialOpt%monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

isCoeff = PRESENT(coeff)

IF (isCoeff) THEN

  IF (firstCall0) THEN
    ! coeff = LagrangeCoeff_Quadrangle(&
    CALL LagrangeCoeff_Quadrangle_( &
      order=order, xij=xij, basisType=basisType0, alpha=alpha, beta=beta, &
      lambda=lambda, ans=coeff, nrow=indx(1), ncol=indx(2))
  END IF

  coeff0(1:dim2, 1:dim2) = coeff(1:dim2, 1:dim2)

ELSE

  ! coeff0 = LagrangeCoeff_Quadrangle(&
  CALL LagrangeCoeff_Quadrangle_( &
    order=order, xij=xij, basisType=basisType0, alpha=alpha, beta=beta, &
    lambda=lambda, ans=coeff0, nrow=indx(1), ncol=indx(2))

END IF

SELECT CASE (basisType0)

CASE (TypePolynomialOpt%monomial)
  ! degree = LagrangeDegree_Quadrangle(order=order)
  CALL LagrangeDegree_Quadrangle_(order=order, ans=degree, nrow=indx(1), &
                                  ncol=indx(2))

#ifdef DEBUG_VER
  IF (dim2 .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(msg="tdof is not same as size(degree,1)", &
                  routine="LagrangeEvalAll_Quadrangle1", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF
#endif

  DO ii = 1, dim2
    ai = MAX(degree(ii, 1_I4B) - 1_I4B, 0_I4B)
    bi = MAX(degree(ii, 2_I4B) - 1_I4B, 0_I4B)
    ar = REAL(degree(ii, 1_I4B), DFP)
    br = REAL(degree(ii, 2_I4B), DFP)

    indx(1:2) = degree(ii, 1:2)

    DO jj = 1, dim1
      areal = (ar * x(1, jj)**ai) * x(2, jj)**indx(2)
      breal = x(1, jj)**indx(1) * (br * x(2, jj)**bi)
      xx(jj, ii, 1) = areal
      xx(jj, ii, 2) = breal

    END DO

  END DO

CASE (TypePolynomialOpt%hierarchical)

  ! xx = HeirarchicalBasisGradient_Quadrangle( &
  CALL HeirarchicalBasisGradient_Quadrangle_( &
    p=order, q=order, xij=x, ans=xx, dim1=indx(1), dim2=indx(2), dim3=indx(3))

CASE DEFAULT

  ! xx = OrthogonalBasisGradient_Quadrangle( &
  CALL OrthogonalBasisGradient_Quadrangle_(p=order, q=order, xij=x, &
     basisType1=basisType0, basisType2=basisType0, alpha1=alpha, beta1=beta, &
           lambda1=lambda, alpha2=alpha, beta2=beta, lambda2=lambda, ans=xx, &
                                     dim1=indx(1), dim2=indx(2), dim3=indx(3))

END SELECT

DO ii = 1, 2
  ! ans(:, ii, :) = TRANSPOSE(MATMUL(xx(:, :, ii), coeff0))
  ans(1:dim1, 1:dim2, ii) = MATMUL(xx(1:dim1, 1:dim2, ii), coeff0)
END DO

END PROCEDURE LagrangeGradientEvalAll_Quadrangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LagrangeMethods
