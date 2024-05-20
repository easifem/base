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

SUBMODULE(TriangleInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                      GetTotalDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Triangle
ans = (order + 1) * (order + 2) / 2_I4B
END PROCEDURE GetTotalDOF_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Triangle
ans = (order - 1) * (order - 2) / 2_I4B
END PROCEDURE GetTotalInDOF_Triangle

!----------------------------------------------------------------------------
!                                                   RefElemDomain_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Triangle
SELECT CASE (UpperCase(baseContinuity))
CASE ("H1")
  SELECT CASE (UpperCase(baseInterpol))
  CASE ("LAGRANGEPOLYNOMIAL", "LAGRANGE", "LAGRANGEINTERPOLATION")
    ans = "UNIT"
  CASE ("SERENDIPITYPOLYNOMIAL", "SERENDIPITY", "SERENDIPITYINTERPOLATION")
    ans = "UNIT"
  CASE ("HERMITPOLYNOMIAL", "HERMIT", "HERMITINTERPOLATION")
    ans = "UNIT"
  CASE ( &
    & "HIERARCHICALPOLYNOMIAL", &
    & "HIERARCHY", &
    & "HEIRARCHICALPOLYNOMIAL", &
    & "HEIRARCHY", &
    & "HIERARCHYINTERPOLATION", &
    & "HEIRARCHYINTERPOLATION")
    ans = "BIUNIT"
  CASE ("ORTHOGONALPOLYNOMIAL", "ORTHOGONAL", "ORTHOGONALINTERPOLATION")
    ans = "BIUNIT"
  CASE DEFAULT
    CALL Errormsg(&
      & msg="No case found for given baseInterpol="//TRIM(baseInterpol), &
      & file=__FILE__, &
      & line=__LINE__,&
      & routine="RefElemDomain_Triangle()", &
      & unitno=stderr)
  END SELECT
CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for given baseContinuity="//TRIM(baseContinuity), &
    & file=__FILE__, &
    & line=__LINE__,&
    & routine="RefElemDomain_Triangle()", &
    & unitno=stderr)
END SELECT
END PROCEDURE RefElemDomain_Triangle

!----------------------------------------------------------------------------
!                                                       FacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Triangle
TYPE(String) :: baseInterpol0
TYPE(String) :: baseContinuity0

baseInterpol0 = UpperCase(baseInterpol)
baseContinuity0 = UpperCase(baseContinuity)

SELECT CASE (baseInterpol0%chars())
CASE ( &
  & "HIERARCHYPOLYNOMIAL", &
  & "HIERARCHY", &
  & "HEIRARCHYPOLYNOMIAL", &
  & "HEIRARCHY", &
  & "HIERARCHYINTERPOLATION", &
  & "HEIRARCHYINTERPOLATION", &
  & "ORTHOGONALPOLYNOMIAL", &
  & "ORTHOGONAL", &
  & "ORTHOGONALINTERPOLATION")
  ans(:, 1) = [1, 2]
  ans(:, 2) = [1, 3]
  ans(:, 3) = [2, 3]
CASE DEFAULT
  ans(:, 1) = [1, 2]
  ans(:, 2) = [2, 3]
  ans(:, 3) = [3, 1]
END SELECT
END PROCEDURE FacetConnectivity_Triangle

!----------------------------------------------------------------------------
!                                                 EquidistancePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Triangle
INTEGER(I4B) :: nsd, n, ne, i1, i2
REAL(DFP) :: x(3, 3), xin(3, 3), e1(3), e2(3), lam, avar, mu

x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:3) = xij(1:nsd, 1:3)
ELSE
  nsd = 2_I4B
  x(1:nsd, 1) = [0.0, 0.0]
  x(1:nsd, 2) = [1.0, 0.0]
  x(1:nsd, 3) = [0.0, 1.0]
END IF

n = LagrangeDOF_Triangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP

!! points on vertex
ans(1:nsd, 1:3) = x(1:nsd, 1:3)

!! points on edge
ne = LagrangeInDOF_Line(order=order)
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

!! points on face
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

END PROCEDURE EquidistancePoint_Triangle

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Triangle
INTEGER(I4B) :: nsd, n
REAL(DFP) :: x(3, 3), xin(3, 3), e1(3), e2(3), lam, avar, mu

IF (order .LT. 3_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF

x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:3) = xij(1:nsd, 1:3)
ELSE
  nsd = 2_I4B
  x(1:nsd, 1) = [0.0, 0.0]
  x(1:nsd, 2) = [1.0, 0.0]
  x(1:nsd, 3) = [0.0, 1.0]
END IF

n = LagrangeInDOF_Triangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP

!! points on face
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

END PROCEDURE EquidistanceInPoint_Triangle

!----------------------------------------------------------------------------
!                                                   BlythPozrikidis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BlythPozrikidis_Triangle
REAL(DFP) :: v(order + 1), xi(order + 1, order + 1), eta(order + 1, order + 1)
REAL(DFP), ALLOCATABLE :: temp(:, :)
INTEGER(I4B) :: nsd, N, ii, jj, kk
CHARACTER(*), PARAMETER :: myName = "BlythPozrikidis_Triangle"

v = InterpolationPoint_Line( &
  & order=order, &
  & ipType=ipType, &
  & xij=[0.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & lambda=lambda, &
  & beta=beta, &
  & alpha=alpha)

N = LagrangeDOF_Triangle(order=order)

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 2
END IF

CALL Reallocate(ans, nsd, N)
CALL Reallocate(temp, 2, N)

xi = 0.0_DFP
eta = 0.0_DFP

DO ii = 1, order + 1
  DO jj = 1, order + 2 - ii
    kk = order + 3 - ii - jj
    xi(ii, jj) = (1.0 + 2.0 * v(ii) - v(jj) - v(kk)) / 3.0_DFP
    eta(ii, jj) = (1.0 + 2.0 * v(jj) - v(ii) - v(kk)) / 3.0_DFP
  END DO
END DO

IF (layout .EQ. "VEFC") THEN

  CALL IJ2VEFC_Triangle(xi=xi, eta=eta, temp=temp, order=order, N=N)

  IF (PRESENT(xij)) THEN
    ans = FromUnitTriangle2Triangle(xin=temp, &
      & x1=xij(:, 1), x2=xij(:, 2), x3=xij(:, 3))
  ELSE
    ans = temp
  END IF

ELSE
  CALL ErrorMsg( &
    & msg="Only layout=VEFC is allowed, given layout is " &
    & //TRIM(layout), &
    & file=__FILE__, &
    & routine=myname, &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

IF (ALLOCATED(temp)) DEALLOCATE (temp)

END PROCEDURE BlythPozrikidis_Triangle

!----------------------------------------------------------------------------
!                                                            Isaac_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Isaac_Triangle
REAL(DFP) :: xi(order + 1, order + 1), eta(order + 1, order + 1)
REAL(DFP), ALLOCATABLE :: temp(:, :), rPoints(:, :)
INTEGER(I4B) :: nsd, N, cnt, ii, jj
CHARACTER(*), PARAMETER :: myName = "Isaac_Triangle"

rPoints = RecursiveNode2D(order=order, ipType=ipType, domain="UNIT",  &
  & alpha=alpha, beta=beta, lambda=lambda)

N = SIZE(rPoints, 2)

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 2
END IF

CALL Reallocate(ans, nsd, N)

!! convert from rPoints to xi and eta
cnt = 0
xi = 0.0_DFP
eta = 0.0_DFP

DO ii = 1, order + 1
  DO jj = 1, order + 2 - ii
    cnt = cnt + 1
    xi(ii, jj) = rPoints(1, cnt)
    eta(ii, jj) = rPoints(2, cnt)
  END DO
END DO

IF (layout .EQ. "VEFC") THEN
  CALL Reallocate(temp, 2, N)
  CALL IJ2VEFC_Triangle(xi=xi, eta=eta, temp=temp, order=order, N=N)
  IF (PRESENT(xij)) THEN
    ans = FromUnitTriangle2Triangle(xin=temp, &
      & x1=xij(:, 1), x2=xij(:, 2), x3=xij(:, 3))
  ELSE
    ans = temp
  END IF
ELSE
  CALL ErrorMsg( &
    & msg="Only layout=VEFC is allowed, given layout is " &
    & //TRIM(layout), &
    & file=__FILE__, &
    & routine=myname, &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

IF (ALLOCATED(temp)) DEALLOCATE (temp)
IF (ALLOCATED(rPoints)) DEALLOCATE (rPoints)
END PROCEDURE Isaac_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJ2VEFC_Triangle
INTEGER(I4B) :: cnt, m, ii, jj, ll, llt, llr

cnt = 0
m = order
llt = INT((m - 1) / 3)
llr = MOD(m - 1, 3)
DO ll = 0, llt
  !! v1
  cnt = cnt + 1
  ii = 1 + ll; jj = 1 + ll
  temp(1, cnt) = xi(ii, jj)
  temp(2, cnt) = eta(ii, jj)
  !! v2
  cnt = cnt + 1
  ii = m + 1 - 2 * ll; jj = 1 + ll
  temp(1, cnt) = xi(ii, jj)
  temp(2, cnt) = eta(ii, jj)
  !! v3
  cnt = cnt + 1
  ii = 1 + ll; jj = m + 1 - 2 * ll
  temp(1, cnt) = xi(ii, jj)
  temp(2, cnt) = eta(ii, jj)
  !! nodes on edge 12
  jj = ll + 1
  DO ii = 2 + ll, m - 2 * ll
    cnt = cnt + 1
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
  END DO
  !! nodes on edge 23
  DO jj = 2 + ll, m - 2 * ll
    cnt = cnt + 1
    ii = m - ll + 2 - jj
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
  END DO
  !! nodes on edge 31
  ii = ll + 1
  DO jj = m - 2 * ll, 2 + ll, -1
    cnt = cnt + 1
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
  END DO
  !! internal nodes
END DO

IF (llr .EQ. 2_I4B) THEN
  !! a internal point
  cnt = cnt + 1
  ll = llt + 1
  ii = 1 + ll; jj = 1 + ll
  temp(1, cnt) = xi(ii, jj)
  temp(2, cnt) = eta(ii, jj)
END IF

IF (cnt .NE. N) THEN
  CALL ErrorMsg( &
    & msg="cnt="//tostring(cnt)//" not equal to total DOF, N=" &
    & //tostring(N), &
    & file=__FILE__, &
    & routine="IJ2VEFC_Triangle()", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF
END PROCEDURE IJ2VEFC_Triangle

!----------------------------------------------------------------------------
!                                               InterpolationPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Triangle
CHARACTER(*), PARAMETER :: myName = "InterpolationPoint_Triangle"

SELECT CASE (ipType)
CASE (Equidistance)
  ans = EquidistancePoint_Triangle(xij=xij, order=order)
CASE (Feket, Hesthaven, ChenBabuska)
  CALL ErrorMsg( &
    & msg="Feket, Hesthaven, ChenBabuska nodes not available", &
    & file=__FILE__, &
    & routine=myname, &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
CASE (BlythPozLegendre)
  ans = BlythPozrikidis_Triangle( &
    & order=order, &
    & ipType=GaussLegendreLobatto,  &
    & layout="VEFC", &
    & xij=xij, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
CASE (BlythPozChebyshev)
  ans = BlythPozrikidis_Triangle( &
    & order=order, &
    & ipType=GaussChebyshevLobatto,  &
    & layout="VEFC", &
    & xij=xij, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
CASE (IsaacLegendre, GaussLegendreLobatto)
  ans = Isaac_Triangle( &
    & order=order, &
    & ipType=GaussLegendreLobatto, &
    & layout="VEFC", &
    & xij=xij, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
CASE (IsaacChebyshev, GaussChebyshevLobatto)
  ans = Isaac_Triangle( &
    & order=order, &
    & ipType=GaussChebyshevLobatto, &
    & layout="VEFC", &
    & xij=xij, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
CASE DEFAULT
  ans = Isaac_Triangle( &
    & order=order, &
    & ipType=ipType, &
    & layout="VEFC", &
    & xij=xij, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END SELECT
END PROCEDURE InterpolationPoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
