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

SUBMODULE(LineInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       RefElemDomain_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Line
ans = "BIUNIT"
END PROCEDURE RefElemDomain_Line

!----------------------------------------------------------------------------
!                                                     QuadratureNumber_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadratureNumber_Line
SELECT CASE (quadType)
CASE (GaussLegendre, GaussChebyshev, GaussJacobi, GaussUltraspherical)
  ans = 1_I4B + INT(order / 2, kind=I4B)
CASE DEFAULT
  ans = 2_I4B + INT(order / 2, kind=I4B)
END SELECT
END PROCEDURE QuadratureNumber_Line

!----------------------------------------------------------------------------
!                                                           ToVEFC_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE ToVEFC_Line
REAL(DFP) :: t1
INTEGER(I4B) :: np
np = SIZE(pt)
t1 = pt(np)
IF (np .GT. 2) THEN
  pt(3:np) = pt(2:np - 1)
  pt(2) = t1
END IF
END PROCEDURE ToVEFC_Line

!----------------------------------------------------------------------------
!                                                              RefLineCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefLineCoord
TYPE(String) :: astr
astr = UpperCase(refLine)
SELECT CASE (astr%chars())
CASE ("UNIT")
  ans(1, :) = [0.0_DFP, 1.0_DFP]
CASE ("BIUNIT")
  ans(1, :) = [-1.0_DFP, 1.0_DFP]
END SELECT
END PROCEDURE RefLineCoord

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
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line1
INTEGER(I4B) :: n, ii
REAL(DFP) :: avar
IF (order .LE. 1_I4B) THEN
  ALLOCATE (ans(0))
  RETURN
END IF
n = LagrangeInDOF_Line(order=order)
ALLOCATE (ans(n))
avar = (xij(2) - xij(1)) / order
DO ii = 1, n
  ans(ii) = xij(1) + ii * avar
END DO
END PROCEDURE EquidistanceInPoint_Line1

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line2
INTEGER(I4B) :: n, ii, nsd
REAL(DFP) :: x0(3, 2)
REAL(DFP) :: avar(3)
IF (order .LE. 1_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x0(1:nsd, 1) = xij(1:nsd, 1)
  x0(1:nsd, 2) = xij(1:nsd, 2)
ELSE
  nsd = 1_I4B
  x0(1:nsd, 1) = [-1.0]
  x0(1:nsd, 2) = [1.0]
END IF
n = LagrangeInDOF_Line(order=order)
ALLOCATE (ans(nsd, n))
avar(1:nsd) = (x0(1:nsd, 2) - x0(1:nsd, 1)) / order
DO ii = 1, n
  ans(1:nsd, ii) = x0(1:nsd, 1) + ii * avar(1:nsd)
END DO
END PROCEDURE EquidistanceInPoint_Line2

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line1
CALL Reallocate(ans, order + 1)
IF (order .EQ. 0_I4B) THEN
  ans(1) = 0.5_DFP * (xij(1) + xij(2))
  RETURN
END IF
ans(1) = xij(1)
ans(2) = xij(2)
IF (order .GE. 2) THEN
  ans(3:) = EquidistanceInPoint_Line(order=order, xij=xij)
END IF
END PROCEDURE EquidistancePoint_Line1

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line2
INTEGER(I4B) :: nsd

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  CALL Reallocate(ans, nsd, order + 1)
  IF (order .EQ. 0_I4B) THEN
    ans(1:nsd, 1) = 0.5_DFP * (xij(1:nsd, 1) + xij(1:nsd, 2))
    RETURN
  END IF
  ans(1:nsd, 1) = xij(1:nsd, 1)
  ans(1:nsd, 2) = xij(1:nsd, 2)
ELSE
  nsd = 1_I4B
  CALL Reallocate(ans, nsd, order + 1)
  IF (order .EQ. 0_I4B) THEN
    ans(1:nsd, 1) = 0.0_DFP
    RETURN
  END IF
  ans(1:nsd, 1) = [-1.0]
  ans(1:nsd, 2) = [1.0]
END IF
IF (order .GE. 2) THEN
  ans(1:nsd, 3:) = EquidistanceInPoint_Line(order=order, xij=xij)
END IF
END PROCEDURE EquidistancePoint_Line2

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line1
CHARACTER(20) :: astr
INTEGER(I4B) :: nsd, ii
REAL(DFP) :: temp(order + 1), t1

IF (order .EQ. 0_I4B) THEN
  IF (PRESENT(xij)) THEN
    nsd = SIZE(xij, 1)
    CALL Reallocate(ans, nsd, 1)
    ans(1:nsd, 1) = 0.5_DFP * (xij(1:nsd, 1) + xij(1:nsd, 2))
  ELSE
    CALL Reallocate(ans, 1, 1)
    ans = 0.0_DFP
  END IF
  RETURN
END IF

astr = TRIM(UpperCase(layout))

SELECT CASE (ipType)

CASE (Equidistance)
  ans = EquidistancePoint_Line(xij=xij, order=order)
  IF (astr .EQ. "INCREASING") THEN
    DO ii = 1, SIZE(ans, 1)
      ans(ii, :) = SORT(ans(ii, :))
    END DO
  END IF
  RETURN
CASE (GaussLegendre)
  CALL LegendreQuadrature(n=order + 1, pt=temp, quadType=Gauss)
CASE (GaussLegendreLobatto)
  CALL LegendreQuadrature(n=order + 1, pt=temp, quadType=GaussLobatto)
  IF (layout .EQ. "VEFC") THEN
    t1 = temp(order + 1)
    IF (order .GE. 2) THEN
      temp(3:) = temp(2:order)
    END IF
    temp(2) = t1
  END IF

CASE (GaussChebyshev)
  CALL Chebyshev1Quadrature(n=order + 1, pt=temp, quadType=Gauss)

CASE (GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=order + 1, pt=temp, quadType=GaussLobatto)
  IF (layout .EQ. "VEFC") THEN
    t1 = temp(order + 1)
    IF (order .GE. 2) THEN
      temp(3:) = temp(2:order)
    END IF
    temp(2) = t1
  END IF

CASE (GaussJacobi)
  IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
    CALL ErrorMsg(&
      & msg="alpha and beta should be present for ipType=GaussJacobi", &
      & file=__FILE__, &
      & routine="InterpolationPoint_Line1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  CALL JacobiQuadrature( &
    & n=order + 1, &
    & pt=temp, &
    & quadType=Gauss, &
    & alpha=alpha, &
    & beta=beta)

CASE (GaussJacobiLobatto)
  IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
    CALL ErrorMsg(&
      & msg="alpha and beta should be present for ipType=GaussJacobi", &
      & file=__FILE__, &
      & routine="InterpolationPoint_Line1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  CALL JacobiQuadrature( &
    & n=order + 1, &
    & pt=temp, &
    & quadType=GaussLobatto, &
    & alpha=alpha, &
    & beta=beta)

  IF (layout .EQ. "VEFC") THEN
    t1 = temp(order + 1)
    IF (order .GE. 2) THEN
      temp(3:) = temp(2:order)
    END IF
    temp(2) = t1
  END IF

CASE (GaussUltraspherical)
  IF (.NOT. PRESENT(lambda)) THEN
    CALL ErrorMsg(&
      & msg="lambda should be present for ipType=GaussUltraspherical", &
      & file=__FILE__, &
      & routine="InterpolationPoint_Line1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  CALL UltrasphericalQuadrature( &
    & n=order + 1, &
    & pt=temp, &
    & quadType=Gauss, &
    & lambda=lambda)

CASE (GaussUltrasphericalLobatto)
  IF (.NOT. PRESENT(lambda)) THEN
    CALL ErrorMsg(&
     & msg="lambda should be present for ipType=GaussUltrasphericalLobatto", &
      & file=__FILE__, &
      & routine="InterpolationPoint_Line1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  CALL UltrasphericalQuadrature( &
    & n=order + 1, &
    & pt=temp, &
    & quadType=GaussLobatto, &
    & lambda=lambda)

  IF (layout .EQ. "VEFC") THEN
    t1 = temp(order + 1)
    IF (order .GE. 2) THEN
      temp(3:) = temp(2:order)
    END IF
    temp(2) = t1
  END IF

CASE DEFAULT
  CALL ErrorMsg(&
    & msg="Unknown iptype", &
    & file=__FILE__, &
    & routine="InterpolationPoint_Line1", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT

IF (ipType .NE. Equidistance) THEN
  IF (PRESENT(xij)) THEN
    nsd = SIZE(xij, 1)
    CALL Reallocate(ans, nsd, order + 1)
    ans = FromBiunitLine2Segment(xin=temp, x1=xij(:, 1), &
      & x2=xij(:, 2))
  ELSE
    CALL Reallocate(ans, 1, order + 1)
    ans(1, :) = temp
  END IF
END IF
END PROCEDURE InterpolationPoint_Line1

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line2
CHARACTER(20) :: astr
REAL(DFP) :: t1

IF (order .EQ. 0_I4B) THEN
  ans = [0.5_DFP * (xij(1) + xij(2))]
  RETURN
END IF

CALL Reallocate(ans, order + 1)
astr = TRIM(UpperCase(layout))

SELECT CASE (ipType)
CASE (Equidistance)
  ans = EquidistancePoint_Line(xij=xij, order=order)
  IF (astr .EQ. "INCREASING") ans = SORT(ans)
  RETURN

CASE (GaussLegendre)
  CALL LegendreQuadrature(n=order + 1, pt=ans, quadType=Gauss)

CASE (GaussLegendreLobatto)
  CALL LegendreQuadrature(n=order + 1, pt=ans, quadType=GaussLobatto)
  IF (layout .EQ. "VEFC") THEN
    t1 = ans(order + 1)
    IF (order .GE. 2) THEN
      ans(3:) = ans(2:order)
    END IF
    ans(2) = t1
  END IF

CASE (GaussChebyshev)
  CALL Chebyshev1Quadrature(n=order + 1, pt=ans, quadType=Gauss)

CASE (GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=order + 1, pt=ans, quadType=GaussLobatto)
  IF (layout .EQ. "VEFC") THEN
    t1 = ans(order + 1)
    IF (order .GE. 2) THEN
      ans(3:) = ans(2:order)
    END IF
    ans(2) = t1
  END IF

CASE (GaussJacobi)
  IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
    CALL ErrorMsg(&
      & msg="alpha and beta should be present for ipType=GaussJacobi", &
      & file=__FILE__, &
      & routine="InterpolationPoint_Line2", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  CALL JacobiQuadrature( &
    & n=order + 1, &
    & pt=ans, &
    & quadType=Gauss, &
    & alpha=alpha, &
    & beta=beta)

CASE (GaussJacobiLobatto)
  IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
    CALL ErrorMsg(&
    & msg="alpha and beta should be present for ipType=GaussJacobiLobatto", &
    & file=__FILE__, &
    & routine="InterpolationPoint_Line2", &
    & line=__LINE__, &
    & unitno=stderr)
  END IF

  CALL JacobiQuadrature( &
    & n=order + 1, &
    & pt=ans, &
    & quadType=GaussLobatto, &
    & alpha=alpha, &
    & beta=beta)

  IF (layout .EQ. "VEFC") THEN
    t1 = ans(order + 1)
    IF (order .GE. 2) THEN
      ans(3:) = ans(2:order)
    END IF
    ans(2) = t1
  END IF

CASE (GaussUltraspherical)
  IF (.NOT. PRESENT(lambda)) THEN
    CALL ErrorMsg(&
      & msg="lambda should be present for ipType=GaussUltraspherical", &
      & file=__FILE__, &
      & routine="InterpolationPoint_Line2", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  CALL UltrasphericalQuadrature( &
    & n=order + 1, &
    & pt=ans, &
    & quadType=Gauss, &
    & lambda=lambda)

CASE (GaussUltrasphericalLobatto)
  IF (.NOT. PRESENT(lambda)) THEN
    CALL ErrorMsg(&
     & msg="lambda should be present for ipType=GaussUltrasphericalLobatto", &
      & file=__FILE__, &
      & routine="InterpolationPoint_Line2", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  CALL UltrasphericalQuadrature( &
    & n=order + 1, &
    & pt=ans, &
    & quadType=GaussLobatto, &
    & lambda=lambda)

  IF (layout .EQ. "VEFC") THEN
    t1 = ans(order + 1)
    IF (order .GE. 2) THEN
      ans(3:) = ans(2:order)
    END IF
    ans(2) = t1
  END IF

CASE DEFAULT
  CALL ErrorMsg(&
    & msg="Unknown iptype", &
    & file=__FILE__, &
    & routine="InterpolationPoint_Line2", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT

IF (ipType .NE. Equidistance) THEN
  ans = FromBiunitLine2Segment(xin=ans, x1=xij(1), x2=xij(2))
END IF
END PROCEDURE InterpolationPoint_Line2

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line1
REAL(DFP) :: v(SIZE(xij, 2), SIZE(xij, 2))
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info
v = LagrangeVandermonde(order=order, xij=xij, elemType=Line2)
CALL getLU(A=v, IPIV=ipiv, info=info)
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Line1

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line2
REAL(DFP) :: vtemp(SIZE(v, 1), SIZE(v, 2))
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info
vtemp = v; ipiv = 0
CALL getLU(A=vtemp, IPIV=ipiv, info=info)
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=vtemp, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Line2

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line3
INTEGER(I4B) :: info
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Line3

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line4
ans = LagrangeVandermonde(order=order, xij=xij, elemType=Line2)
CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Line4

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line5
SELECT CASE (basisType)
CASE (Monomial)
  ans = LagrangeCoeff_Line(order=order, xij=xij)
CASE DEFAULT
  ans = EvalAllOrthopol(&
    & n=order, &
    & x=xij(1, :), &
    & orthopol=basisType, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
  CALL GetInvMat(ans)
END SELECT
END PROCEDURE LagrangeCoeff_Line5

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line1
LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2))
INTEGER(I4B) :: ii, orthopol0

IF (SIZE(xij, 2) .NE. order + 1) THEN
  CALL Errormsg(&
    & msg="Size(xij, 1) .NE. order+1 ", &
    & file=__FILE__, &
    & routine="LagrangeEvalAll_Line2", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

orthopol0 = input(default=Monomial, option=basisType)
firstCall0 = input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Line(&
      & order=order, &
      & xij=xij, &
      & basisType=orthopol0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda)
  END IF
  coeff0 = TRANSPOSE(coeff)
ELSE
  coeff0 = TRANSPOSE(LagrangeCoeff_Line(&
    & order=order, &
    & xij=xij, &
    & basisType=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda &
    & ))
END IF

SELECT CASE (orthopol0)
CASE (Monomial)
  xx(1, 1) = 1.0_DFP
  DO ii = 1, order
    xx(1, ii + 1) = xx(1, ii) * x
  END DO
CASE DEFAULT
  xx = EvalAllOrthopol(&
    & n=order, &
    & x=[x], &
    & orthopol=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END SELECT

ans = MATMUL(coeff0, xx(1, :))

END PROCEDURE LagrangeEvalAll_Line1

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line2
LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(SIZE(x, 2), SIZE(xij, 2))
INTEGER(I4B) :: ii, orthopol0

IF (SIZE(xij, 2) .NE. order + 1) THEN
  CALL Errormsg(&
    & msg="Size(xij, 1) .NE. order+1 ", &
    & file=__FILE__, &
    & routine="LagrangeEvalAll_Line2", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

orthopol0 = Input(default=Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Line(&
      & order=order, &
      & xij=xij, &
      & basisType=orthopol0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda)
  END IF
  coeff0 = coeff
ELSE
  coeff0 = LagrangeCoeff_Line(&
    & order=order, &
    & xij=xij, &
    & basisType=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda &
    & )
END IF

SELECT CASE (orthopol0)
CASE (Monomial)
  xx(:, 1) = 1.0_DFP
  DO ii = 1, order
    xx(:, ii + 1) = xx(:, ii) * x(1, :)
  END DO
CASE DEFAULT
  xx = EvalAllOrthopol(&
    & n=order, &
    & x=x(1, :), &
    & orthopol=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END SELECT

ans = MATMUL(xx, coeff0)

END PROCEDURE LagrangeEvalAll_Line2

!----------------------------------------------------------------------------
!                                                               EvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line1
INTEGER(I4B) :: ii, basisType0
TYPE(String) :: astr
astr = UpperCase(refLine)

IF (astr%chars() .EQ. "UNIT") THEN
  CALL Errormsg(&
    & msg="refLine should be BIUNIT", &
    & file=__FILE__, &
    & routine="BasisEvalAll_Line1", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

basisType0 = input(default=Monomial, option=basisType)
SELECT CASE (basisType0)
CASE (Monomial)
  ans(1) = 1.0_DFP
  DO ii = 1, order
    ans(ii + 1) = ans(ii) * x
  END DO
CASE DEFAULT

  IF (basisType0 .EQ. Jacobi) THEN
    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg(&
        & msg="alpha and beta should be present for basisType=Jacobi", &
        & file=__FILE__, &
        & routine="BasisEvalAll_Line1", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  IF (basisType0 .EQ. Ultraspherical) THEN
    IF (.NOT. PRESENT(lambda)) THEN
      CALL Errormsg(&
        & msg="lambda should be present for basisType=Ultraspherical", &
        & file=__FILE__, &
        & routine="BasisEvalAll_Line1", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  ans = RESHAPE(EvalAllOrthopol(&
    & n=order, &
    & x=[x], &
    & orthopol=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda), [order + 1])
END SELECT

END PROCEDURE BasisEvalAll_Line1

!----------------------------------------------------------------------------
!                                                 BasisGradientEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line1
INTEGER(I4B) :: ii, basisType0
TYPE(String) :: astr
astr = UpperCase(refLine)

IF (astr%chars() .EQ. "UNIT") THEN
  CALL Errormsg(&
    & msg="refLine should be BIUNIT", &
    & file=__FILE__, &
    & routine="BasisGradientEvalAll_Line1", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

basisType0 = input(default=Monomial, option=basisType)
SELECT CASE (basisType0)
CASE (Monomial)
  ans(1) = 0.0_DFP
  DO ii = 1, order
    ans(ii + 1) = REAL(ii, dfp) * x**(ii - 1)
  END DO
CASE DEFAULT

  IF (basisType0 .EQ. Jacobi) THEN
    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg(&
        & msg="alpha and beta should be present for basisType=Jacobi", &
        & file=__FILE__, &
        & routine="BasisGradientEvalAll_Line1", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  IF (basisType0 .EQ. Ultraspherical) THEN
    IF (.NOT. PRESENT(lambda)) THEN
      CALL Errormsg(&
        & msg="lambda should be present for basisType=Ultraspherical", &
        & file=__FILE__, &
        & routine="BasisGradientEvalAll_Line1", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  ans = RESHAPE(GradientEvalAllOrthopol(&
    & n=order, &
    & x=[x], &
    & orthopol=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda), [order + 1])
END SELECT

END PROCEDURE BasisGradientEvalAll_Line1

!----------------------------------------------------------------------------
!                                                        BasisEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line2
INTEGER(I4B) :: ii, basisType0
TYPE(String) :: astr
astr = UpperCase(refLine)

IF (astr%chars() .EQ. "UNIT") THEN
  CALL Errormsg(&
    & msg="refLine should be BIUNIT", &
    & file=__FILE__, &
    & routine="BasisEvalAll_Line2", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

basisType0 = input(default=Monomial, option=basisType)
SELECT CASE (basisType0)
CASE (Monomial)
  ans(:, 1) = 1.0_DFP
  DO ii = 1, order
    ans(:, ii + 1) = ans(:, ii) * x
  END DO
CASE DEFAULT

  IF (basisType0 .EQ. Jacobi) THEN
    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg(&
        & msg="alpha and beta should be present for basisType=Jacobi", &
        & file=__FILE__, &
        & routine="BasisEvalAll_Line2", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  IF (basisType0 .EQ. Ultraspherical) THEN
    IF (.NOT. PRESENT(lambda)) THEN
      CALL Errormsg(&
        & msg="lambda should be present for basisType=Ultraspherical", &
        & file=__FILE__, &
        & routine="BasisEvalAll_Line2", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  ans = EvalAllOrthopol(&
    & n=order, &
    & x=x, &
    & orthopol=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END SELECT

END PROCEDURE BasisEvalAll_Line2

!----------------------------------------------------------------------------
!                                                  BasisGradientEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line2
INTEGER(I4B) :: ii, basisType0
TYPE(String) :: astr
astr = UpperCase(refLine)

IF (astr%chars() .EQ. "UNIT") THEN
  CALL Errormsg(&
    & msg="refLine should be BIUNIT", &
    & file=__FILE__, &
    & routine="BasisGradientEvalAll_Line2", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

basisType0 = input(default=Monomial, option=basisType)
SELECT CASE (basisType0)
CASE (Monomial)
  ans(:, 1) = 0.0_DFP
  DO ii = 1, order
    ans(:, ii + 1) = REAL(ii, dfp) * x**(ii - 1)
  END DO
CASE DEFAULT

  IF (basisType0 .EQ. Jacobi) THEN
    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg(&
        & msg="alpha and beta should be present for basisType=Jacobi", &
        & file=__FILE__, &
        & routine="BasisGradientEvalAll_Line2", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  IF (basisType0 .EQ. Ultraspherical) THEN
    IF (.NOT. PRESENT(lambda)) THEN
      CALL Errormsg(&
        & msg="lambda should be present for basisType=Ultraspherical", &
        & file=__FILE__, &
        & routine="BasisGradientEvalAll_Line2", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  ans = GradientEvalAllOrthopol(&
    & n=order, &
    & x=x, &
    & orthopol=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END SELECT

END PROCEDURE BasisGradientEvalAll_Line2

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line1
INTEGER(I4B) :: nips(1)
nips(1) = QuadratureNumber_Line(order=order, quadType=quadType)
ans = QuadraturePoint_Line3(nips=nips, quadType=quadType, &
& layout=layout, xij=xij, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE QuadraturePoint_Line1

!----------------------------------------------------------------------------
!                                                       QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line2
ans = QuadraturePoint_Line1(&
    & order=order, &
    & quadType=quadType, &
    & layout=layout, &
    & xij=RESHAPE(xij, [1, 2]), &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END PROCEDURE QuadraturePoint_Line2

!----------------------------------------------------------------------------
!                                                        QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line4
ans = QuadraturePoint_Line3(&
    & nips=nips, &
    & quadType=quadType, &
    & layout=layout, &
    & xij=RESHAPE(xij, [1, 2]), &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END PROCEDURE QuadraturePoint_Line4

!----------------------------------------------------------------------------
!                                                       QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line3
CHARACTER(20) :: astr
INTEGER(I4B) :: np, nsd, ii
REAL(DFP) :: pt(nips(1)), wt(nips(1))
REAL(DFP) :: t1
LOGICAL(LGT) :: changeLayout

IF (ANY([GaussJacobi, GaussJacobiLobatto] .EQ. quadType)) THEN
  IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
    CALL ErrorMsg(&
      & msg="alpha and beta should be present for quadType=GaussJacobi", &
      & file=__FILE__, &
      & routine="QuadraturePoint_Line3", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF
  RETURN
ELSEIF (ANY([GaussJacobi, GaussJacobiLobatto] .EQ. quadType)) THEN
  IF (.NOT. PRESENT(lambda)) THEN
    CALL ErrorMsg(&
      & msg="lambda should be present for quadType=GaussUltraspherical", &
      & file=__FILE__, &
      & routine="QuadraturePoint_Line3", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF
  RETURN
END IF

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 1
END IF

astr = TRIM(UpperCase(layout))
np = nips(1)
CALL Reallocate(ans, nsd + 1_I4B, np)
changeLayout = .FALSE.

SELECT CASE (quadType)

CASE (GaussLegendre)
  CALL LegendreQuadrature(n=np, pt=pt, wt=wt, quadType=Gauss)

CASE (GaussLegendreRadauLeft)
  CALL LegendreQuadrature(n=np, pt=pt, wt=wt, quadType=GaussRadauLeft)

CASE (GaussLegendreRadauRight)
  CALL LegendreQuadrature(n=np, pt=pt, wt=wt, quadType=GaussRadauRight)

CASE (GaussLegendreLobatto)
  CALL LegendreQuadrature(n=np, pt=pt, wt=wt, quadType=GaussLobatto)
  IF (layout .EQ. "VEFC") changeLayout = .TRUE.

CASE (GaussChebyshev)
  CALL Chebyshev1Quadrature(n=np, pt=pt, wt=wt, quadType=Gauss)

CASE (GaussChebyshevRadauLeft)
  CALL Chebyshev1Quadrature(n=np, pt=pt, wt=wt, quadType=GaussRadauLeft)

CASE (GaussChebyshevRadauRight)
  CALL Chebyshev1Quadrature(n=np, pt=pt, wt=wt, quadType=GaussRadauRight)

CASE (GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=np, pt=pt, wt=wt, quadType=GaussLobatto)
  IF (layout .EQ. "VEFC") changeLayout = .TRUE.

CASE (GaussJacobi)
  CALL JacobiQuadrature( &
    & n=np, &
    & pt=pt, &
    & wt=wt, &
    & quadType=Gauss, &
    & alpha=alpha, &
    & beta=beta)

CASE (GaussJacobiRadauLeft)
  CALL JacobiQuadrature( &
    & n=np, &
    & pt=pt, &
    & wt=wt, &
    & quadType=GaussRadauLeft, &
    & alpha=alpha, &
    & beta=beta)

CASE (GaussJacobiRadauRight)
  CALL JacobiQuadrature( &
    & n=np, &
    & pt=pt, &
    & wt=wt, &
    & quadType=GaussRadauRight, &
    & alpha=alpha, &
    & beta=beta)

CASE (GaussJacobiLobatto)
  CALL JacobiQuadrature( &
    & n=np, &
    & pt=pt, &
    & wt=wt, &
    & quadType=GaussLobatto, &
    & alpha=alpha, &
    & beta=beta)
  IF (layout .EQ. "VEFC") changeLayout = .TRUE.

CASE (GaussUltraspherical)
  CALL UltrasphericalQuadrature( &
    & n=np, &
    & pt=pt, &
    & wt=wt, &
    & quadType=Gauss, &
    & lambda=lambda)

CASE (GaussUltrasphericalRadauLeft)
  CALL UltrasphericalQuadrature( &
    & n=np, &
    & pt=pt, &
    & wt=wt, &
    & quadType=GaussRadauLeft, &
    & lambda=lambda)

CASE (GaussUltrasphericalRadauRight)
  CALL UltrasphericalQuadrature( &
    & n=np, &
    & pt=pt, &
    & wt=wt, &
    & quadType=GaussRadauRight, &
    & lambda=lambda)

CASE (GaussUltrasphericalLobatto)
  CALL UltrasphericalQuadrature( &
    & n=np, &
    & pt=pt, &
    & wt=wt, &
    & quadType=GaussLobatto, &
    & lambda=lambda)
  IF (layout .EQ. "VEFC") changeLayout = .TRUE.

CASE DEFAULT
  CALL ErrorMsg(&
    & msg="Unknown iptype", &
    & file=__FILE__, &
    & routine="QuadraturePoint_Line3", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END SELECT

IF (changeLayout) THEN
  CALL ToVEFC_Line(pt)
  CALL ToVEFC_Line(wt)
END IF

IF (PRESENT(xij)) THEN
  ans(1:nsd, :) = FromBiunitLine2Segment( &
    & xin=pt, &
    & x1=xij(:, 1), &
    & x2=xij(:, 2))
  ans(nsd + 1, :) = wt * NORM2(xij(:, 2) - xij(:, 1)) / 2.0_DFP
ELSE
  ans(1, :) = pt
  ans(nsd + 1, :) = wt
END IF
END PROCEDURE QuadraturePoint_Line3

!----------------------------------------------------------------------------
!                                              LagrangeGradientEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Line1
LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(order + 1, order + 1), xx(SIZE(x, 2), order + 1)
INTEGER(I4B) :: ii, orthopol0

orthopol0 = input(default=Monomial, option=basisType)
firstCall0 = input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Line(&
      & order=order, &
      & xij=xij, &
      & basisType=orthopol0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda)
  END IF
  coeff0 = coeff
ELSE
  coeff0 = LagrangeCoeff_Line(&
    & order=order, &
    & xij=xij, &
    & basisType=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda &
    & )
END IF

SELECT CASE (orthopol0)
CASE (Monomial)

  IF (SIZE(xij, 2) .NE. order + 1) THEN
    CALL Errormsg(&
      & msg="size(xij, 2) is not same as order+1", &
      & file=__FILE__, &
      & routine="LagrangeGradientEvalAll_Line1", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF

  DO ii = 0, order
    xx(:, ii + 1) = REAL(ii, kind=DFP) * x(1, :)**(MAX(ii - 1_I4B, 0_I4B))
  END DO

CASE DEFAULT
  xx = GradientEvalAllOrthopol(&
    & n=order, &
    & x=x(1, :), &
    & orthopol=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END SELECT

ans(:, :, 1) = MATMUL(xx, coeff0)

END PROCEDURE LagrangeGradientEvalAll_Line1

!----------------------------------------------------------------------------
!                                                        BasisEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Line1
TYPE(String) :: astr
astr = UpperCase(refLine)

SELECT CASE (astr%chars())
CASE ("UNIT")
  ans = EvalAllOrthopol( &
    & n=order, &
    & x=FromUnitLine2BiUnitLine(xin=xij(1, :)), &
    & orthopol=Lobatto)
CASE ("BIUNIT")
  ans = EvalAllOrthopol( &
    & n=order, &
    & x=xij(1, :), &
    & orthopol=Lobatto)
CASE DEFAULT
  ans = 0.0_DFP
  CALL Errormsg(&
    & msg="No case found for refline.", &
    & file=__FILE__, &
    & routine="HeirarchicalBasis_Line1", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END SELECT

END PROCEDURE HeirarchicalBasis_Line1

!----------------------------------------------------------------------------
!                                            HeirarchicalGradientBasis_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line1
TYPE(String) :: astr
astr = UpperCase(refLine)

SELECT CASE (astr%chars())
CASE ("UNIT")
  ans(:, :, 1) = GradientEvalAllOrthopol( &
    & n=order, &
    & x=FromUnitLine2BiUnitLine(xin=xij(1, :)), &
    & orthopol=Lobatto)
  ans = ans * 2.0_DFP
CASE ("BIUNIT")
  ans(:, :, 1) = GradientEvalAllOrthopol( &
    & n=order, &
    & x=xij(1, :), &
    & orthopol=Lobatto)
CASE DEFAULT
  ans = 0.0_DFP
  CALL Errormsg(&
    & msg="No case found for refline.", &
    & file=__FILE__, &
    & routine="HeirarchicalGradientBasis_Line1", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END SELECT

END PROCEDURE HeirarchicalGradientBasis_Line1

!----------------------------------------------------------------------------
!                                                        OrthogonalBasis_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasis_Line1
INTEGER(I4B) :: ii
TYPE(String) :: astr

ans = 0.0_DFP
astr = UpperCase(refLine)

IF (basisType .EQ. Jacobi) THEN
  IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
    CALL Errormsg(&
      & msg="alpha and beta should be present for basisType=Jacobi", &
      & file=__FILE__, &
      & routine="BasisEvalAll_Line2", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF
END IF

IF (basisType .EQ. Ultraspherical) THEN
  IF (.NOT. PRESENT(lambda)) THEN
    CALL Errormsg(&
      & msg="lambda should be present for basisType=Ultraspherical", &
      & file=__FILE__, &
      & routine="BasisEvalAll_Line2", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF
END IF

SELECT CASE (astr%chars())
CASE ("UNIT")
  ans = EvalAllOrthopol(&
    & n=order, &
    & x=FromUnitLine2BiUnitLine(xin=xij(1, :)), &
    & orthopol=basisType, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)

CASE ("BIUNIT")
  ans = EvalAllOrthopol(&
    & n=order, &
    & x=xij(1, :), &
    & orthopol=basisType, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)

CASE DEFAULT
  ans = 0.0_DFP
  CALL Errormsg(&
    & msg="No case found for refLine.", &
    & file=__FILE__, &
    & routine="OrthogonalBasis_Line1()", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END SELECT

END PROCEDURE OrthogonalBasis_Line1

!----------------------------------------------------------------------------
!                                            OrthogonalBasisGradient_Line1 
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Line1
TYPE(String) :: astr
astr = UpperCase(refLine)

SELECT CASE (astr%chars())
CASE ("UNIT")
  ans(:, :, 1) = GradientEvalAllOrthopol( &
    & n=order, &
    & x=FromUnitLine2BiUnitLine(xin=xij(1, :)), &
    & orthopol=basisType)
  ans = ans * 2.0_DFP
CASE ("BIUNIT")
  ans(:, :, 1) = GradientEvalAllOrthopol( &
    & n=order, &
    & x=xij(1, :), &
    & orthopol=basisType)
CASE DEFAULT
  ans = 0.0_DFP
  CALL Errormsg(&
    & msg="No case found for refline.", &
    & file=__FILE__, &
    & routine=" OrthogonalBasisGradient_Line1", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END SELECT
END PROCEDURE OrthogonalBasisGradient_Line1 

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
