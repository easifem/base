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

SUBMODULE(TriangleInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         RefTriangleCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefTriangleCoord
CHARACTER(20) :: layout
layout = TRIM(UpperCase(refTriangle))
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  ans(:, 1) = [-1.0_DFP, -1.0_DFP]
  ans(:, 2) = [1.0_DFP, -1.0_DFP]
  ans(:, 3) = [-1.0_DFP, 1.0_DFP]
CASE ("UNIT")
  ans(:, 1) = [0.0_DFP, 0.0_DFP]
  ans(:, 2) = [1.0_DFP, 0.0_DFP]
  ans(:, 3) = [0.0_DFP, 1.0_DFP]
END SELECT
END PROCEDURE RefTriangleCoord

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Triangle
INTEGER(I4B) :: n, ii, jj, kk
!!
n = LagrangeDOF_Triangle(order=order)
ALLOCATE (ans(n, 2))
!!
kk = 0
!!
!! left diagonal
!!
DO jj = 0, order
  DO ii = 0, order - jj
    kk = kk + 1
    ans(kk, 1) = ii
    ans(kk, 2) = jj
  END DO
END DO
!!
!! right diagonal
!!
! DO ii = 0, order
!   DO jj = 0, order - ii
!     kk = kk + 1
!     ans(kk, 1) = ii
!     ans(kk, 2) = jj
!   END DO
! END DO
!!
!!
!! base
!!
! DO ii = 0, order
!   DO jj = 0, ii
!     kk = kk + 1
!     ans(kk, 1) = ii-jj
!     ans(kk, 2) = jj
!   END DO
! END DO
!!
END PROCEDURE LagrangeDegree_Triangle

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
!                                                 EquidistancePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Triangle
INTEGER(I4B) :: nsd, n, ne, i1, i2
REAL(DFP) :: x(3, 3), xin(3, 3), e1(3), e2(3), lam, avar, mu
  !!
x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP
  !!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:3) = xij(1:nsd, 1:3)
ELSE
  nsd = 3_I4B
  x(1:nsd, 1) = [0.0, 0.0, 0.0]
  x(1:nsd, 2) = [1.0, 0.0, 0.0]
  x(1:nsd, 3) = [0.0, 1.0, 0.0]
END IF
  !!
n = LagrangeDOF_Triangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP
  !!
  !! points on vertex
  !!
ans(1:nsd, 1:3) = x(1:nsd, 1:3)
  !!
  !! points on edge
  !!
ne = LagrangeInDOF_Line(order=order)
  !!
i2 = 3
IF (order .GT. 1_I4B) THEN
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [1, 2]))
    !!
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [2, 3]))
    !!
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [3, 1]))
    !!
END IF
  !!
  !! points on face
  !!
IF (order .GT. 2_I4B) THEN
    !!
  IF (order .EQ. 3_I4B) THEN
    i1 = i2 + 1
    ans(1:nsd, i1) = (x(1:nsd, 1) + x(1:nsd, 2) + x(1:nsd, 3)) / 3.0_DFP
  ELSE
      !!
    e1 = x(:, 2) - x(:, 1)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 3) - x(:, 1)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 1) = x(1:nsd, 1) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    e1 = x(:, 3) - x(:, 2)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 1) - x(:, 2)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 2) = x(1:nsd, 2) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    e1 = x(:, 1) - x(:, 3)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 2) - x(:, 3)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 3) = x(1:nsd, 3) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    i1 = i2 + 1
    ans(1:nsd, i1:) = EquidistancePoint_Triangle( &
      & order=order - 3, &
      & xij=xin(1:nsd, 1:3))
      !!
  END IF
END IF
  !!
END PROCEDURE EquidistancePoint_Triangle

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Triangle
INTEGER(I4B) :: nsd, n, ne
REAL(DFP) :: x(3, 3), xin(3, 3), e1(3), e2(3), lam, avar, mu
  !!
IF (order .LT. 3_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF
  !!
x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP
  !!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:3) = xij(1:nsd, 1:3)
ELSE
  nsd = 3_I4B
  x(1:nsd, 1) = [0.0, 0.0, 0.0]
  x(1:nsd, 2) = [1.0, 0.0, 0.0]
  x(1:nsd, 3) = [0.0, 1.0, 0.0]
END IF
  !!
n = LagrangeInDOF_Triangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP
  !!
  !! points on face
  !!
IF (order .EQ. 3_I4B) THEN
  ans(1:nsd, 1) = (x(1:nsd, 1) + x(1:nsd, 2) + x(1:nsd, 3)) / 3.0_DFP
ELSE
    !!
  e1 = x(:, 2) - x(:, 1)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 3) - x(:, 1)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 1) = x(1:nsd, 1) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  e1 = x(:, 3) - x(:, 2)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 1) - x(:, 2)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 2) = x(1:nsd, 2) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  e1 = x(:, 1) - x(:, 3)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 2) - x(:, 3)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 3) = x(1:nsd, 3) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  ans(1:nsd, 1:) = EquidistancePoint_Triangle( &
    & order=order - 3, &
    & xij=xin(1:nsd, 1:3))
    !!
END IF
  !!
END PROCEDURE EquidistanceInPoint_Triangle

!----------------------------------------------------------------------------
!                                                   BlythPozrikidis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BlythPozrikidis_Triangle
REAL(DFP) :: v(order + 1), xi(order + 1, order + 1), eta(order + 1, order + 1)
REAL(DFP), ALLOCATABLE :: temp(:, :)
INTEGER(I4B) :: nsd, N, ii, jj, kk
CHARACTER(*), PARAMETER :: myName = "BlythPozrikidis_Triangle"
!!
v = InterpolationPoint_Line(order=order, ipType=ipType, &
  & xij=[0.0_DFP, 1.0_DFP], layout="INCREASING")
!!
N = LagrangeDOF_Triangle(order=order)
!!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 2
END IF
!!
CALL Reallocate(ans, nsd, N)
CALL Reallocate(temp, 2, N)
!!
xi = 0.0_DFP
eta = 0.0_DFP
!!
DO ii = 1, order + 1
  DO jj = 1, order + 2 - ii
    kk = order + 3 - ii - jj
    xi(ii, jj) = (1.0 + 2.0 * v(ii) - v(jj) - v(kk)) / 3.0_DFP
    eta(ii, jj) = (1.0 + 2.0 * v(jj) - v(ii) - v(kk)) / 3.0_DFP
  END DO
END DO
!!
IF (layout .EQ. "VEFC") THEN
  !!
  CALL IJ2VEFC(xi=xi, eta=eta, temp=temp, order=order, N=N, myname=myname)
  !!
  IF (PRESENT(xij)) THEN
    ans = FromUnitTriangle2Triangle(xin=temp, &
      & x1=xij(:, 1), x2=xij(:, 2), x3=xij(:, 3))
  ELSE
    ans = temp
  END IF
  !!
ELSE
  CALL ErrorMsg( &
    & msg="Only layout=VEFC is allowed, given layout is " &
    & //TRIM(layout), &
    & file=__FILE__, &
    & routine=myname, &
    & line=__LINE__, &
    & unitno=stderr)
  STOP
END IF
!!
IF (ALLOCATED(temp)) DEALLOCATE (temp)
!!
END PROCEDURE BlythPozrikidis_Triangle

!----------------------------------------------------------------------------
!                                                   BlythPozrikidis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Isaac_Triangle
REAL(DFP) :: xi(order + 1, order + 1), eta(order + 1, order + 1)
REAL(DFP), ALLOCATABLE :: temp(:, :), rPoints(:, :)
INTEGER(I4B) :: nsd, N, cnt, ii, jj
CHARACTER(*), PARAMETER :: myName = "Isaac_Triangle"
!!
rPoints = RecursiveNode2D(order=order, ipType=ipType)
N = SIZE(rPoints, 2)
!!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 2
END IF
!!
CALL Reallocate(ans, nsd, N)
!!
!! convert from rPoints to xi and eta
!!
cnt = 0
xi = 0.0_DFP
eta = 0.0_DFP
!!
DO ii = 1, order + 1
  DO jj = 1, order + 2 - ii
    cnt = cnt + 1
    xi(ii, jj) = rPoints(1, cnt)
    eta(ii, jj) = rPoints(2, cnt)
  END DO
END DO
!!
IF (layout .EQ. "VEFC") THEN
  !!
  CALL Reallocate(temp, 2, N)
  CALL IJ2VEFC(xi=xi, eta=eta, temp=temp, order=order, N=N, myname=myname)
  !!
  IF (PRESENT(xij)) THEN
    ans = FromUnitTriangle2Triangle(xin=temp, &
      & x1=xij(:, 1), x2=xij(:, 2), x3=xij(:, 3))
  ELSE
    ans = temp
  END IF
  !!
ELSE
  CALL ErrorMsg( &
    & msg="Only layout=VEFC is allowed, given layout is " &
    & //TRIM(layout), &
    & file=__FILE__, &
    & routine=myname, &
    & line=__LINE__, &
    & unitno=stderr)
  STOP
END IF
!!
IF (ALLOCATED(temp)) DEALLOCATE (temp)
IF (ALLOCATED(rPoints)) DEALLOCATE (rPoints)
!!
END PROCEDURE Isaac_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE IJ2VEFC(xi, eta, temp, order, N, myname)
  REAL(DFP), INTENT(IN) :: xi(:, :)
  REAL(DFP), INTENT(IN) :: eta(:, :)
  REAL(DFP), INTENT(OUT) :: temp(:, :)
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B), INTENT(IN) :: N
  CHARACTER(*), INTENT(IN) :: myname
  !!
  INTEGER(I4B) :: cnt, m, ii, jj, kk, ll, llt, llr
  !!
  !! vertices
  !!
  cnt = 0
  m = order
  llt = INT((m - 1) / 3)
  llr = MOD(m - 1, 3)
  DO ll = 0, llt
    !!
    !! v1
    !!
    cnt = cnt + 1
    ii = 1 + ll; jj = 1 + ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
    !!
    !! v2
    !!
    cnt = cnt + 1
    ii = m + 1 - 2 * ll; jj = 1 + ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
    !!
    !! v3
    !!
    cnt = cnt + 1
    ii = 1 + ll; jj = m + 1 - 2 * ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
    !!
    !! nodes on edge 12
    !!
    jj = ll + 1
    DO ii = 2 + ll, m - 2 * ll
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END DO
    !!
    !! nodes on edge 23
    !!
    DO jj = 2 + ll, m - 2 * ll
      cnt = cnt + 1
      ii = m - ll + 2 - jj
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END DO
    !!
    !! nodes on edge 31
    !!
    ii = ll + 1
    DO jj = m - 2 * ll, 2 + ll, -1
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END DO
    !!
    !! internal nodes
    !!
  END DO
  !!
  IF (llr .EQ. 2_I4B) THEN
    !!
    !! a internal point
    !!
    cnt = cnt + 1
    ll = llt + 1
    ii = 1 + ll; jj = 1 + ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
  END IF
  !!
  IF (cnt .NE. N) THEN
    CALL ErrorMsg( &
      & msg="cnt="//tostring(cnt)//" not equal to total DOF, N=" &
      & //tostring(N), &
      & file=__FILE__, &
      & routine=myname, &
      & line=__LINE__, &
      & unitno=stderr)
    STOP
  END IF
END SUBROUTINE IJ2VEFC

!----------------------------------------------------------------------------
!                                               InterpolationPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Triangle
CHARACTER(*), PARAMETER :: myName = "InterpolationPoint_Triangle"
SELECT CASE (ipType)
CASE (Equidistance)
  ans = EquidistancePoint_Triangle(xij=xij, order=order)
CASE (Feket, Hesthaven, ChenBabuska)
  CALL ErrorMsg(msg="Feket, Hesthaven, ChenBabuska nodes not available", &
    & file=__FILE__, &
    & routine=myname, &
    & line=__LINE__, &
    & unitno=stderr)
  STOP
CASE (BlythPozLegendre)
  ans = BlythPozrikidis_Triangle(order=order, &
    & ipType=GaussLegendreLobatto,  &
    & layout="VEFC", xij=xij)
CASE (BlythPozChebyshev)
  ans = BlythPozrikidis_Triangle(order=order, &
    & ipType=GaussChebyshevLobatto,  &
    & layout="VEFC", xij=xij)
CASE (GaussLegendreLobatto, IsaacLegendre)
  ans = Isaac_Triangle(order=order, ipType=GaussLegendreLobatto, &
    & layout="VEFC", xij=xij)
CASE (GaussChebyshevLobatto, IsaacChebyshev)
  ans = Isaac_Triangle(order=order, ipType=GaussChebyshevLobatto, &
    & layout="VEFC", xij=xij)
CASE DEFAULT
  CALL ErrorMsg(msg="Unknown interpolation point type (ipType)", &
    & file=__FILE__, &
    & routine=myname, &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
END PROCEDURE InterpolationPoint_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Triangle1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info
ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Triangle)
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
INTEGER(I4B) :: basisType0
basisType0 = input(default=Monomial, option=basisType)
SELECT CASE (basisType0)
CASE (Monomial)
  ans = LagrangeVandermonde(order=order, xij=xij, elemType=Triangle)
CASE (Jacobi)
  IF (PRESENT(refTriangle)) THEN
    ans = Dubiner_Triangle(order=order, xij=xij, refTriangle=refTriangle)
  ELSE
    ans = Dubiner_Triangle(order=order, xij=xij, refTriangle="UNIT")
  END IF
CASE (Heirarchical)
  IF (PRESENT(refTriangle)) THEN
    ans = HeirarchicalBasis_Triangle(&
      & order=order, &
      & pe1=order, &
      & pe2=order, &
      & pe3=order, &
      & xij=xij, &
      & refTriangle=refTriangle &
      & )
  ELSE
    ans = HeirarchicalBasis_Triangle(&
      & order=order, &
      & pe1=order, &
      & pe2=order, &
      & pe3=order, &
      & xij=xij, &
      & refTriangle="UNIT" &
      & )
  END IF
CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for basisType", &
    & file=__FILE__, &
    & routine="LagrangeCoeff_Triangle4", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Triangle4

!----------------------------------------------------------------------------
!                                                       Dubiner_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Triangle1
CHARACTER(20) :: layout
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
!!
layout = TRIM(UpperCase(refTriangle))
!!
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
CASE ("UNIT")
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END SELECT
!!
ans = Dubiner_Quadrangle(order=order, xij=x)
!!
END PROCEDURE Dubiner_Triangle1

!----------------------------------------------------------------------------
!                                                       Dubiner_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Triangle2
CHARACTER(20) :: layout
REAL(DFP) :: x0(SIZE(x)), y0(SIZE(y))
!!
layout = TRIM(UpperCase(refTriangle))
!!
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  x0 = x
  y0 = y
CASE ("UNIT")
  x0 = FromUnitLine2BiUnitLine(xin=x)
  y0 = FromUnitLine2BiUnitLine(xin=y)
END SELECT
!!
ans = Dubiner_Quadrangle(order=order, x=x0, y=y0)
!!
END PROCEDURE Dubiner_Triangle2

!----------------------------------------------------------------------------
!                                            BarycentricVertexBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricVertexBasis_Triangle
ans = TRANSPOSE(lambda)
END PROCEDURE BarycentricVertexBasis_Triangle

!----------------------------------------------------------------------------
!                                             BarycentricEdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasis_Triangle
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2))
INTEGER(I4B) :: maxP, tPoints
!!
tPoints = SIZE(lambda, 2)
maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2)
!!
d_lambda(1:tPoints) = lambda(2, :) - lambda(1, :)
d_lambda(1 + tPoints:2 * tPoints) = lambda(3, :) - lambda(1, :)
d_lambda(1 + 2 * tPoints:3 * tPoints) = lambda(3, :) - lambda(2, :)
phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)
!!
ans = BarycentricEdgeBasis_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
  & lambda=lambda, phi=phi)
!!
END PROCEDURE BarycentricEdgeBasis_Triangle

!----------------------------------------------------------------------------
!                                            BarycentricEdgeBasis_Triangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasis_Triangle2
INTEGER(I4B) :: tPoints, a, ii
REAL(DFP) :: temp(SIZE(lambda, 2))
!!
ans = 0.0_DFP
tPoints = SIZE(lambda, 2)
!!
a = 0
!!
!! edge(1) = (v1, v2)
!!
temp = lambda(1, :) * lambda(2, :)
!!
DO ii = 1, pe1 - 1
  ans(:, a + ii) = temp * phi(1:tPoints, ii - 1)
END DO
!!
!! edge(2) = (v1, v3)
!!
a = pe1 - 1
temp = lambda(1, :) * lambda(3, :)
!!
DO ii = 1, pe2 - 1
  ans(:, a + ii) = temp &
                  & * phi(1 + tPoints:2 * tPoints, ii - 1)
END DO
!!
!! edge(3) = (v2, v3)
!!
a = pe1 - 1 + pe2 - 1
temp = lambda(2, :) * lambda(3, :)
!!
DO ii = 1, pe3 - 1
  ans(:, a + ii) = temp &
                  & * phi(1 + 2 * tPoints:3 * tPoints, ii - 1)
END DO
!!
END PROCEDURE BarycentricEdgeBasis_Triangle2

!----------------------------------------------------------------------------
!                                              BarycentricCellBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCellBasis_Triangle2
INTEGER(I4B) :: tPoints, k1, k2, cnt
REAL(DFP) :: temp(SIZE(lambda, 2))
!!
tPoints = SIZE(lambda, 2)
temp = lambda(1, :) * lambda(2, :) * lambda(3, :)
cnt = 0
!!
DO k1 = 1, order - 2
  DO k2 = 1, order - 1 - k1
    cnt = cnt + 1
    ans(:, cnt) = temp * phi(1:tPoints, k1 - 1) * &
      & phi(1 + tPoints:2 * tPoints, k2 - 1)
  END DO
END DO
!!
END PROCEDURE BarycentricCellBasis_Triangle2

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasis_Triangle1
CHARACTER(20) :: layout
INTEGER(I4B) :: a, b
INTEGER(I4B) :: maxP, tPoints
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), &
  & 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2))
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
!!
layout = TRIM(UpperCase(refTriangle))
!!
tPoints = SIZE(lambda, 2)
maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2)
d_lambda(1:tPoints) = lambda(2, :) - lambda(1, :)
d_lambda(1 + tPoints:2 * tPoints) = lambda(3, :) - lambda(1, :)
d_lambda(1 + 2 * tPoints:3 * tPoints) = lambda(3, :) - lambda(2, :)
phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)
!!
!! Vertex basis function
!!
ans = 0.0_DFP
ans(:, 1:3) = BarycentricVertexBasis_Triangle(lambda=lambda)
!!
!! Edge basis function
!!
b = 3
!!
IF (pe1 .GE. 2_I4B .OR. pe2 .GE. 2_I4B .OR. pe3 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  ans(:, a:b) = BarycentricEdgeBasis_Triangle2( &
    & pe1=pe1, pe2=pe2, pe3=pe3, lambda=lambda, phi=phi)
END IF
!!
!! Cell basis function
!!
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  ans(:, a:b) = BarycentricCellBasis_Triangle2(order=order, &
    & lambda=lambda, phi=phi)
END IF
!!
END PROCEDURE BarycentricHeirarchicalBasis_Triangle1

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasis_Triangle2
ans = BarycentricHeirarchicalBasis_Triangle1(order=order, pe1=order, &
  & pe2=order, pe3=order, lambda=lambda, refTriangle=refTriangle)
END PROCEDURE BarycentricHeirarchicalBasis_Triangle2

!----------------------------------------------------------------------------
!                                                      VertexBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Triangle
CHARACTER(20) :: layout
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)
!!
layout = TRIM(UpperCase(refTriangle))
!!
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
CASE ("UNIT")
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END SELECT
!!
Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))
!!
ans = VertexBasis_Triangle2(Lo1=Lo1, Lo2=Lo2)
!!
END PROCEDURE VertexBasis_Triangle

!----------------------------------------------------------------------------
!                                                       VertexBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Triangle2
ans(:, 1) = Lo1(:, 0) * Lo2(:, 0)
ans(:, 2) = Lo1(:, 1) * Lo2(:, 0)
ans(:, 3) = Lo1(:, 1) * Lo2(:, 1) + Lo1(:, 0) * Lo2(:, 1)
END PROCEDURE VertexBasis_Triangle2

!----------------------------------------------------------------------------
!                                                         EdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Triangle
CHARACTER(20) :: layout
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
REAL(DFP) :: L1(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3))
REAL(DFP) :: L2(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3))
REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)
INTEGER(I4B) :: maxP
!!
layout = TRIM(UpperCase(refTriangle))
!!
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
CASE ("UNIT")
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END SELECT
!!
maxP = MAX(pe1, pe2, pe3)
L1 = JacobiEvalAll(n=maxP, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
L2 = JacobiEvalAll(n=maxP, x=x(2, :), alpha=1.0_DFP, beta=1.0_DFP)
!!
Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))
!!
ans = EdgeBasis_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, L1=L1, L2=L2, &
  & Lo1=Lo1, Lo2=Lo2)
!!
END PROCEDURE EdgeBasis_Triangle

!----------------------------------------------------------------------------
!                                                         EdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Triangle2
CHARACTER(20) :: layout
INTEGER(I4B) :: maxP, k1, k2, a
!!
maxP = MAX(pe1, pe2, pe3)
!!
!! edge(1)
!!
a = 0
!!
DO k1 = 2, pe1
  ! ans(:, k1 - 1) = L1(:, k1) * (L2(:, 0)**k1)
  ans(:, k1 - 1) = Lo1(:, 0) * Lo1(:, 1) * L1(:, k1 - 2) * (Lo2(:, 0)**k1)
END DO
!!
!! edge(2)
!!
a = pe1 - 1
DO k2 = 2, pe2
  ! ans(:, a + k2 - 1) = L1(:, 0) * L2(:, k2)
  ans(:, a + k2 - 1) = Lo1(:, 0) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
END DO
!!
!! edge(3)
!!
a = pe1 - 1 + pe2 - 1
DO k2 = 2, pe3
  ! ans(:, a + k2 - 1) = L1(:, 1) * L2(:, k2)
  ans(:, a + k2 - 1) = Lo1(:, 1) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
END DO
!!
END PROCEDURE EdgeBasis_Triangle2

!----------------------------------------------------------------------------
!                                              CellBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Triangle
CHARACTER(20) :: layout
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
REAL(DFP) :: L1(SIZE(xij, 2), 0:order)
REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)
!!
layout = TRIM(UpperCase(refTriangle))
!!
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
CASE ("UNIT")
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END SELECT
!!
Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))
L1 = JacobiEvalAll(n=order, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
!!
ans = CellBasis_Triangle2(order=order, L1=L1, Lo1=Lo1, &
  & Lo2=Lo2, eta_ij=x)
!!
END PROCEDURE CellBasis_Triangle

!----------------------------------------------------------------------------
!                                                        CellBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Triangle2
REAL(DFP) :: P2(SIZE(eta_ij, 2), 0:order)
REAL(DFP) :: avec(SIZE(eta_ij, 2)), alpha, beta
INTEGER(I4B) :: k1, k2, max_k2, cnt
!!
alpha = 0.0_DFP
beta = 1.0_DFP
cnt = 0
!!
DO k1 = 2, order - 1
  !!
  avec = (Lo2(:, 0)**k1) * Lo2(:, 1) * Lo1(:, 0) * Lo1(:, 1)
  !!
  alpha = 2.0_DFP * k1 - 1.0_DFP
  !
  max_k2 = MAX(order - k1 - 1, 0)
  !!
  P2(:, 0:max_k2) = JacobiEvalAll(n=max_k2, x=eta_ij(2, :), &
    & alpha=alpha, beta=beta)
  !!
  DO k2 = 2, order - k1 + 1
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1 - 2) * avec * P2(:, k2 - 2)
  END DO
  !!
END DO

END PROCEDURE CellBasis_Triangle2

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle1
CHARACTER(20) :: layout
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
REAL(DFP) :: L1(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
REAL(DFP) :: L2(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)
INTEGER(I4B) :: maxP, a, b, ii
!!
layout = TRIM(UpperCase(refTriangle))
!!
IF (layout .EQ. "BIUNIT") THEN
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
ELSE
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END IF
!!
Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))
!!
!! Vertex basis function
!!
ans = 0.0_DFP
ans(:, 1:3) = VertexBasis_Triangle2(Lo1=Lo1, Lo2=Lo2)
!!
maxP = MAX(pe1, pe2, pe3, order)
L1 = JacobiEvalAll(n=maxP, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
L2 = JacobiEvalAll(n=maxP, x=x(2, :), alpha=1.0_DFP, beta=1.0_DFP)
!!
!! Edge basis function
!!
b = 3
!!
IF (pe1 .GE. 2_I4B .OR. pe2 .GE. 2_I4B .OR. pe3 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  ans(:, a:b) = EdgeBasis_Triangle2( &
    & pe1=pe1, pe2=pe2, pe3=pe3, L1=L1, L2=L2, Lo1=Lo1, &
    & Lo2=Lo2)
END IF
!!
!! Cell basis function
!!
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  ans(:, a:b) = CellBasis_Triangle2(order=order, L1=L1, &
    & Lo1=Lo1, Lo2=Lo2, eta_ij=x)
END IF
!!
END PROCEDURE HeirarchicalBasis_Triangle1

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Triangle1
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof
INTEGER(I4B) :: degree(SIZE(xij, 2), 2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2))

basisType0 = input(default=Monomial, option=basisType)
firstCall0 = input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Triangle(&
      & order=order, &
      & xij=xij, &
      & basisType=basisType0, &
      & refTriangle=refTriangle &
      & )
    coeff0 = TRANSPOSE(coeff)
  ELSE
    coeff0 = TRANSPOSE(coeff)
  END IF
ELSE
  coeff0 = TRANSPOSE(LagrangeCoeff_Triangle(&
    & order=order, &
    & xij=xij, &
    & basisType=basisType0, &
    & refTriangle=refTriangle &
    & ))
END IF

SELECT CASE (basisType0)
CASE (Monomial)
  degree = LagrangeDegree_Triangle(order=order)
  tdof = SIZE(xij, 2)
  IF (tdof .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(&
      & msg="tdof is not same as size(degree,1)", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Triangle1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  DO ii = 1, tdof
    xx(1, ii) = x(1)**degree(ii, 1) * x(2)**degree(ii, 2)
  END DO
CASE (Heirarchical)
  xx = HeirarchicalBasis_Triangle( &
    & order=order, &
    & pe1=order,  &
    & pe2=order,  &
    & pe3=order,  &
    & xij=RESHAPE(x, [2, 1]),  &
    & refTriangle=refTriangle)
CASE (Jacobi)
  xx = Dubiner_Triangle( &
    & order=order, &
    & xij=RESHAPE(x, [2, 1]),  &
    & refTriangle=refTriangle)
CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for basisType", &
    & file=__FILE__, &
    & routine="LagrangeEvalAll_Triangle1", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT

ans = MATMUL(coeff0, xx(1, :))

END PROCEDURE LagrangeEvalAll_Triangle1

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Triangle2
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof
INTEGER(I4B) :: degree(SIZE(xij, 2), 2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(SIZE(x, 2), SIZE(xij, 2))

basisType0 = input(default=Monomial, option=basisType)
firstCall0 = input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Triangle(&
      & order=order, &
      & xij=xij, &
      & basisType=basisType0, &
      & refTriangle=refTriangle &
      & )
    coeff0 = coeff
  ELSE
    coeff0 = coeff
  END IF
ELSE
  coeff0 = LagrangeCoeff_Triangle(&
    & order=order, &
    & xij=xij, &
    & basisType=basisType0, &
    & refTriangle=refTriangle &
    & )
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Triangle(order=order)
  tdof = SIZE(xij, 2)
  IF (tdof .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(&
      & msg="tdof is not same as size(degree,1)", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Triangle1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  DO ii = 1, tdof
    xx(:, ii) = x(1, :)**degree(ii, 1) * x(2, :)**degree(ii, 2)
  END DO

CASE (Heirarchical)

  xx = HeirarchicalBasis_Triangle( &
    & order=order, &
    & pe1=order,  &
    & pe2=order,  &
    & pe3=order,  &
    & xij=x,  &
    & refTriangle=refTriangle)

CASE (Jacobi)

  xx = Dubiner_Triangle( &
    & order=order, &
    & xij=x,  &
    & refTriangle=refTriangle)

CASE DEFAULT

  CALL Errormsg(&
    & msg="No case found for basisType", &
    & file=__FILE__, &
    & routine="LagrangeEvalAll_Triangle1", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT

ans = MATMUL(xx, coeff0)
END PROCEDURE LagrangeEvalAll_Triangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods
