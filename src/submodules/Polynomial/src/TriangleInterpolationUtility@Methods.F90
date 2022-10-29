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
CHARACTER(LEN=*), PARAMETER :: myName = "BlythPozrikidis_Triangle"
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
CHARACTER(LEN=*), PARAMETER :: myName = "Isaac_Triangle"
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
  CHARACTER(LEN=*), INTENT(IN) :: myname
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
CHARACTER(LEN=*), PARAMETER :: myName = "InterpolationPoint_Triangle"
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
END SELECT
END PROCEDURE InterpolationPoint_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Triangle1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info
!!
ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Triangle)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans, IPIV=ipiv, info=info)
!!
END PROCEDURE LagrangeCoeff_Triangle1

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Triangle2
!!
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info
!!
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
ans = LagrangeVandermonde(order=order, xij=xij, elemType=Triangle)
CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Triangle4

!----------------------------------------------------------------------------
!                                                       Dubiner_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Triangle1
CHARACTER(LEN=20) :: layout
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
CHARACTER(LEN=20) :: layout
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

END SUBMODULE Methods
