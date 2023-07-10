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
!
IF (order .LE. 1_I4B) THEN
  ALLOCATE (ans(0))
  RETURN
END IF
!
n = LagrangeInDOF_Line(order=order)
ALLOCATE (ans(n))
!
avar = (xij(2) - xij(1)) / order
!
DO ii = 1, n
  ans(ii) = xij(1) + ii * avar
END DO
!
END PROCEDURE EquidistanceInPoint_Line1

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line2
INTEGER(I4B) :: n, ii, nsd
REAL(DFP) :: x0(3, 2)
REAL(DFP) :: avar(3)
!
IF (order .LE. 1_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF
!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x0(1:nsd, 1) = xij(1:nsd, 1)
  x0(1:nsd, 2) = xij(1:nsd, 2)
ELSE
  nsd = 3_I4B
  x0(1:nsd, 1) = [-1.0, 0.0, 0.0]
  x0(1:nsd, 2) = [1.0, 0.0, 0.0]
END IF
!
n = LagrangeInDOF_Line(order=order)
ALLOCATE (ans(nsd, n))
!
avar(1:nsd) = (x0(1:nsd, 2) - x0(1:nsd, 1)) / order
!
DO ii = 1, n
  ans(1:nsd, ii) = x0(1:nsd, 1) + ii * avar(1:nsd)
END DO
!
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
!
ans(1) = xij(1)
ans(2) = xij(2)
!
IF (order .GE. 2) THEN
  ans(3:) = EquidistanceInPoint_Line(order=order, xij=xij)
END IF
END PROCEDURE EquidistancePoint_Line1

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line2
INTEGER(I4B) :: nsd
!
IF (PRESENT(xij)) THEN
  !
  nsd = SIZE(xij, 1)
  !
  CALL Reallocate(ans, nsd, order + 1)
  !
  IF (order .EQ. 0_I4B) THEN
    ans(1:nsd, 1) = 0.5_DFP * (xij(1:nsd, 1) + xij(1:nsd, 2))
    RETURN
  END IF
  !
  ans(1:nsd, 1) = xij(1:nsd, 1)
  ans(1:nsd, 2) = xij(1:nsd, 2)
  !
ELSE
  nsd = 3_I4B
  !
  CALL Reallocate(ans, nsd, order + 1)
  !
  IF (order .EQ. 0_I4B) THEN
    ans(1:nsd, 1) = 0.0_DFP
    RETURN
  END IF
  !
  ans(1:nsd, 1) = [-1.0, 0.0, 0.0]
  ans(1:nsd, 2) = [1.0, 0.0, 0.0]
  !
END IF
!
IF (order .GE. 2) THEN
  ans(1:nsd, 3:) = EquidistanceInPoint_Line(order=order, xij=xij)
END IF
!
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
    ans(1:nsd, 1) = 0.0_DFP
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

astr = TRIM(UpperCase(layout))

SELECT CASE (ipType)
CASE (Equidistance)
  ans = EquidistancePoint_Line(xij=xij, order=order)
  IF (astr .EQ. "INCREASING") ans = SORT(ans)
  RETURN
CASE (GaussLegendre)
  CALL Reallocate(ans, order + 1)
  CALL LegendreQuadrature(n=order + 1, pt=ans, quadType=Gauss)
CASE (GaussLegendreLobatto)
  CALL Reallocate(ans, order + 1)
  CALL LegendreQuadrature(n=order + 1, pt=ans, quadType=GaussLobatto)
  IF (layout .EQ. "VEFC") THEN
    t1 = ans(order + 1)
    IF (order .GE. 2) THEN
      ans(3:) = ans(2:order)
    END IF
    ans(2) = t1
  END IF
CASE (GaussChebyshev)
  CALL Reallocate(ans, order + 1)
  CALL Chebyshev1Quadrature(n=order + 1, pt=ans, quadType=Gauss)
CASE (GaussChebyshevLobatto)
  CALL Reallocate(ans, order + 1)
  CALL Chebyshev1Quadrature(n=order + 1, pt=ans, quadType=GaussLobatto)
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
SELECT CASE (orthopol)
CASE (Monomial)
  ans = LagrangeCoeff_Line(order=order, xij=xij)
CASE DEFAULT
  ans = EvalAllOrthopol(&
    & n=order, &
    & x=xij(1, :), &
    & orthopol=orthopol, &
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
REAL(DFP) :: coeff0(order + 1, order + 1), xx(1, order + 1)
INTEGER(I4B) :: ii, orthopol0

orthopol0 = input(default=Monomial, option=orthopol)
firstCall0 = input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    IF (.NOT. PRESENT(xij)) THEN
      CALL Errormsg(&
        & msg="xij should be present!", &
        & file=__FILE__, &
        & routine="LagrangeEvalAll_Line1", &
        & line=__LINE__, &
        & unitno=stderr)
    END IF
    coeff = LagrangeCoeff_Line(&
      & order=order, &
      & xij=xij, &
      & orthopol=orthopol0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda)
    coeff0 = TRANSPOSE(coeff)
  ELSE
    coeff0 = TRANSPOSE(coeff)
  END IF
ELSE
  IF (.NOT. PRESENT(xij)) THEN
    CALL Errormsg(&
      & msg="xij should be present!", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Line1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF
  coeff0 = TRANSPOSE(LagrangeCoeff_Line(&
    & order=order, &
    & xij=xij, &
    & orthopol=orthopol0, &
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
REAL(DFP) :: coeff0(order + 1, order + 1), xx(SIZE(x), order + 1)
INTEGER(I4B) :: ii, orthopol0

orthopol0 = input(default=Monomial, option=orthopol)
firstCall0 = input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    IF (.NOT. PRESENT(xij)) THEN
      CALL Errormsg(&
        & msg="xij should be present!", &
        & file=__FILE__, &
        & routine="LagrangeEvalAll_Line2", &
        & line=__LINE__, &
        & unitno=stderr)
    END IF
    coeff = LagrangeCoeff_Line(&
      & order=order, &
      & xij=xij, &
      & orthopol=orthopol0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda)
    coeff0 = coeff
  ELSE
    coeff0 = coeff
  END IF
ELSE
  IF (.NOT. PRESENT(xij)) THEN
    CALL Errormsg(&
      & msg="xij should be present!", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Line1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF
  ! coeff0 = TRANSPOSE(LagrangeCoeff_Line(order=order, xij=xij))
  coeff0 = LagrangeCoeff_Line(&
    & order=order, &
    & xij=xij, &
    & orthopol=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda &
    & )
END IF

SELECT CASE (orthopol0)
CASE (Monomial)
  xx(:, 1) = 1.0_DFP
  DO ii = 1, order
    xx(:, ii + 1) = xx(:, ii) * x
  END DO
CASE DEFAULT
  xx = EvalAllOrthopol(&
    & n=order, &
    & x=x, &
    & orthopol=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END SELECT

ans = MATMUL(xx, coeff0)

END PROCEDURE LagrangeEvalAll_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
