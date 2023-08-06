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

SUBMODULE(QuadrangleInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                               QuadratureNumber_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadratureNumber_Quadrangle
ans(1) = QuadratureNumber_Line(order=p, quadType=quadType1)
ans(2) = QuadratureNumber_Line(order=q, quadType=quadType2)
END PROCEDURE QuadratureNumber_Quadrangle

!----------------------------------------------------------------------------
!                                                        RefQuadrangleCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefQuadrangleCoord
TYPE(String) :: astr
astr = UpperCase(refQuadrangle)
SELECT CASE (astr%chars())
CASE ("UNIT")
  ans(1, :) = [0.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP]
  ans(2, :) = [0.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP]
CASE ("BIUNIT")
  ans(1, :) = [-1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP]
  ans(2, :) = [-1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP]
END SELECT
END PROCEDURE RefQuadrangleCoord

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle1
INTEGER(I4B) :: n, ii, jj, kk
n = LagrangeDOF_Quadrangle(order=order)
ALLOCATE (ans(n, 2))
kk = 0
DO jj = 0, order
  DO ii = 0, order
    kk = kk + 1
    ans(kk, 1) = ii
    ans(kk, 2) = jj
  END DO
END DO
END PROCEDURE LagrangeDegree_Quadrangle1

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle2
INTEGER(I4B) :: n, ii, jj, kk
n = LagrangeDOF_Quadrangle(p=p, q=q)
ALLOCATE (ans(n, 2))
kk = 0
DO jj = 0, q
  DO ii = 0, p
    kk = kk + 1
    ans(kk, 1) = ii
    ans(kk, 2) = jj
  END DO
END DO
END PROCEDURE LagrangeDegree_Quadrangle2

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
!                                               EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle1
INTEGER(I4B) :: nsd, n, ne, i1, i2
REAL(DFP) :: x(3, 4), xin(3, 4), e1(3), e2(3), lam, avar, mu

x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:4) = xij(1:nsd, 1:4)
ELSE
  nsd = 2_I4B
  x = 0.0_DFP
  x(1:2, :) = RefQuadrangleCoord("BIUNIT")
END IF

n = LagrangeDOF_Quadrangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP

! points on vertex
ans(1:nsd, 1:4) = x(1:nsd, 1:4)

! points on edge
ne = LagrangeInDOF_Line(order=order)

i2 = 4
IF (order .GT. 1_I4B) THEN
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [1, 2]))

  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [2, 3]))

  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [3, 4]))

  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [4, 1]))

END IF

! points on face
IF (order .GT. 1_I4B) THEN

  IF (order .EQ. 2_I4B) THEN
    i1 = i2 + 1
    ans(1:nsd, i1) = SUM(x(1:nsd, :), dim=2_I4B) / 4.0_DFP
  ELSE

    e1 = x(:, 2) - x(:, 1)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 4) - x(:, 1)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 1) = x(1:nsd, 1) + lam * e1(1:nsd) + mu * e2(1:nsd)

    e1 = x(:, 3) - x(:, 2)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 1) - x(:, 2)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 2) = x(1:nsd, 2) + lam * e1(1:nsd) + mu * e2(1:nsd)

    e1 = x(:, 2) - x(:, 3)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 4) - x(:, 3)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 3) = x(1:nsd, 3) + lam * e1(1:nsd) + mu * e2(1:nsd)

    e1 = x(:, 3) - x(:, 4)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 1) - x(:, 4)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 4) = x(1:nsd, 4) + lam * e1(1:nsd) + mu * e2(1:nsd)

    i1 = i2 + 1
    ans(1:nsd, i1:) = EquidistancePoint_Quadrangle( &
      & order=order - 2, &
      & xij=xin(1:nsd, 1:4))

  END IF
END IF

END PROCEDURE EquidistancePoint_Quadrangle1

!----------------------------------------------------------------------------
!                                               EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle2
ans = InterpolationPoint_Quadrangle2(  &
  & p=p,  &
  & q=q,  &
  & xij=xij,  &
  & ipType1=Equidistance,  &
  & ipType2=Equidistance,  &
  & layout="VEFC")
END PROCEDURE EquidistancePoint_Quadrangle2

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Quadrangle1
INTEGER(I4B) :: nsd, n, ne, i1, i2
REAL(DFP) :: x(3, 4), xin(3, 4), e1(3), e2(3), lam, avar, mu

IF (order .LT. 2_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF

x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:4) = xij(1:nsd, 1:4)
ELSE
  nsd = 2_I4B
  x(1:nsd, 1) = [-1.0, -1.0]
  x(1:nsd, 2) = [1.0, -1.0]
  x(1:nsd, 3) = [1.0, 1.0]
  x(1:nsd, 4) = [-1.0, 1.0]
END IF

n = LagrangeInDOF_Quadrangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP

! points on face
IF (order .EQ. 2_I4B) THEN
  ans(1:nsd, 1) = SUM(x, dim=2_I4B) / 4.0_DFP
ELSE
  e1 = x(:, 2) - x(:, 1)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 4) - x(:, 1)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 1) = x(1:nsd, 1) + lam * e1(1:nsd) + mu * e2(1:nsd)

  e1 = x(:, 3) - x(:, 2)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 1) - x(:, 2)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 2) = x(1:nsd, 2) + lam * e1(1:nsd) + mu * e2(1:nsd)

  e1 = x(:, 2) - x(:, 3)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 4) - x(:, 3)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 3) = x(1:nsd, 3) + lam * e1(1:nsd) + mu * e2(1:nsd)

  e1 = x(:, 3) - x(:, 4)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 1) - x(:, 4)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 4) = x(1:nsd, 4) + lam * e1(1:nsd) + mu * e2(1:nsd)

  ans(1:nsd, 1:) = EquidistancePoint_Quadrangle1( &
    & order=order - 2, &
    & xij=xin(1:nsd, 1:4))

END IF
END PROCEDURE EquidistanceInPoint_Quadrangle1

!----------------------------------------------------------------------------
!                                             EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Quadrangle2
END PROCEDURE EquidistanceInPoint_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJ2VEFC_Quadrangle
CALL IJ2VEFC_Quadrangle_AntiClockwise(xi, eta, temp, p, q, 1_I4B)
END PROCEDURE IJ2VEFC_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJ2VEFC_Quadrangle_Clockwise
! internal variables
INTEGER(I4B) :: cnt, m, ii, jj, kk, ll, N, ij(2, 4), iedge, p1, p2
INTEGER(I4B), PARAMETER :: tEdges = 4
INTEGER(I4B) :: edgeConnectivity(2, 4), ii1, ii2, dii, jj1, jj2, djj, &
& pointsOrder(4)
REAL(DFP), ALLOCATABLE :: xi_in(:, :), eta_in(:, :),  &
  & temp_in(:, :)

! vertices
N = (p + 1) * (q + 1)
cnt = 0
ll = -1

SELECT CASE (startNode)
CASE (1)
  edgeConnectivity(:, 1) = [1, 4]
  edgeConnectivity(:, 2) = [4, 3]
  edgeConnectivity(:, 3) = [3, 2]
  edgeConnectivity(:, 4) = [2, 1]
  pointsOrder = [1, 4, 3, 2]
CASE (2)
  edgeConnectivity(:, 1) = [2, 1]
  edgeConnectivity(:, 2) = [1, 4]
  edgeConnectivity(:, 3) = [4, 3]
  edgeConnectivity(:, 4) = [3, 2]
  pointsOrder = [2, 1, 4, 3]
CASE (3)
  edgeConnectivity(:, 1) = [3, 2]
  edgeConnectivity(:, 2) = [2, 1]
  edgeConnectivity(:, 3) = [1, 4]
  edgeConnectivity(:, 4) = [4, 3]
  pointsOrder = [3, 2, 1, 4]
CASE (4)
  edgeConnectivity(:, 1) = [4, 3]
  edgeConnectivity(:, 2) = [3, 2]
  edgeConnectivity(:, 3) = [2, 1]
  edgeConnectivity(:, 4) = [1, 4]
  pointsOrder = [4, 3, 2, 1]
END SELECT

IF (ALL([p, q] .EQ. 0_I4B)) THEN
  temp(:, 1) = [xi(1, 1), eta(1, 1)]
  RETURN
END IF

ij(:, 1) = [1, 1]
ij(:, 2) = [p + 1, 1]
ij(:, 3) = [p + 1, q + 1]
ij(:, 4) = [1, q + 1]

IF (ALL([p, q] .GE. 1_I4B)) THEN
  DO ii = 1, 4
    cnt = cnt + 1
    jj = pointsOrder(ii)
    temp(1:2, ii) = [ &
      & xi(ij(1, jj), ij(2, jj)), &
      & eta(ij(1, jj), ij(2, jj)) &
      & ]
  END DO
  IF (ALL([p, q] .EQ. 1_I4B)) RETURN

ELSE
  IF (p .EQ. 0_I4B) THEN
    DO jj = 1, q + 1
      cnt = cnt + 1
      temp(1:2, jj) = [xi(1, jj), eta(1, jj)]
    END DO
  END IF

  IF (q .EQ. 0_I4B) THEN
    DO ii = 1, p + 1
      cnt = cnt + 1
      temp(1:2, ii) = [xi(ii, 1), eta(ii, 1)]
    END DO
  END IF

END IF

IF (ALL([p, q] .GE. 1_I4B)) THEN
  DO iedge = 1, tEdges
    p1 = edgeConnectivity(1, iedge)
    p2 = edgeConnectivity(2, iedge)

    IF (ij(1, p1) .EQ. ij(1, p2)) THEN
      ii1 = ij(1, p1)
      ii2 = ii1
      dii = 1
    ELSE IF (ij(1, p1) .LT. ij(1, p2)) THEN
      ii1 = ij(1, p1) + 1
      ii2 = ij(1, p2) - 1
      dii = 1
    ELSE IF (ij(1, p1) .GT. ij(1, p2)) THEN
      ii1 = ij(1, p1) - 1
      ii2 = ij(1, p2) + 1
      dii = -1
    END IF

    IF (ij(2, p1) .EQ. ij(2, p2)) THEN
      jj1 = ij(2, p1)
      jj2 = jj1
      djj = 1
    ELSE IF (ij(2, p1) .LT. ij(2, p2)) THEN
      jj1 = ij(2, p1) + 1
      jj2 = ij(2, p2) - 1
      djj = 1
    ELSE IF (ij(2, p1) .GT. ij(2, p2)) THEN
      jj1 = ij(2, p1) - 1
      jj2 = ij(2, p2) + 1
      djj = -1
    END IF

    DO ii = ii1, ii2, dii
      DO jj = jj1, jj2, djj
        cnt = cnt + 1
        temp(:, cnt) = [xi(ii, jj), eta(ii, jj)]
      END DO
    END DO
  END DO

  ! internal nodes
  IF (ALL([p, q] .GE. 2_I4B)) THEN

    CALL Reallocate( &
      & xi_in,  &
      & MAX(p - 1, 1_I4B), &
      & MAX(q - 1_I4B, 1_I4B))
    CALL Reallocate(eta_in, SIZE(xi_in, 1), SIZE(xi_in, 2))
    CALL Reallocate(temp_in, 2, SIZE(xi_in))

    IF (p .LE. 1_I4B) THEN
      ii1 = 1
      ii2 = 1
    ELSE
      ii1 = 2
      ii2 = p
    END IF

    IF (q .LE. 1_I4B) THEN
      jj1 = 1
      jj2 = 1
    ELSE
      jj1 = 2
      jj2 = q
    END IF

    xi_in = xi(ii1:ii2, jj1:jj2)
    eta_in = eta(ii1:ii2, jj1:jj2)

    CALL IJ2VEFC_Quadrangle_Clockwise( &
      & xi=xi_in,  &
      & eta=eta_in, &
      & temp=temp_in, &
      & p=MAX(p - 2, 0_I4B), &
      & q=MAX(q - 2, 0_I4B), &
      & startNode=startNode)

    ii1 = cnt + 1
    ii2 = ii1 + SIZE(temp_in, 2) - 1
    temp(1:2, ii1:ii2) = temp_in
  END IF

END IF

IF (ALLOCATED(xi_in)) DEALLOCATE (xi_in)
IF (ALLOCATED(eta_in)) DEALLOCATE (eta_in)
IF (ALLOCATED(temp_in)) DEALLOCATE (temp_in)

END PROCEDURE IJ2VEFC_Quadrangle_Clockwise

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJ2VEFC_Quadrangle_AntiClockwise
! internal variables
INTEGER(I4B) :: cnt, m, ii, jj, kk, ll, N, ij(2, 4), iedge, p1, p2
INTEGER(I4B), PARAMETER :: tEdges = 4
INTEGER(I4B) :: edgeConnectivity(2, 4), ii1, ii2, dii, jj1, jj2, djj, &
& pointsOrder(4)
REAL(DFP), ALLOCATABLE :: xi_in(:, :), eta_in(:, :),  &
  & temp_in(:, :)

! vertices
N = (p + 1) * (q + 1)
cnt = 0
ll = -1

SELECT CASE (startNode)
CASE (1)
  edgeConnectivity(:, 1) = [1, 2]
  edgeConnectivity(:, 2) = [2, 3]
  edgeConnectivity(:, 3) = [3, 4]
  edgeConnectivity(:, 4) = [4, 1]
  pointsOrder = [1, 2, 3, 4]
CASE (2)
  edgeConnectivity(:, 1) = [2, 3]
  edgeConnectivity(:, 2) = [3, 4]
  edgeConnectivity(:, 3) = [4, 1]
  edgeConnectivity(:, 4) = [1, 2]
  pointsOrder = [2, 3, 4, 1]
CASE (3)
  edgeConnectivity(:, 1) = [3, 4]
  edgeConnectivity(:, 2) = [4, 1]
  edgeConnectivity(:, 3) = [1, 2]
  edgeConnectivity(:, 4) = [2, 3]
  pointsOrder = [3, 4, 1, 2]
CASE (4)
  edgeConnectivity(:, 1) = [4, 1]
  edgeConnectivity(:, 2) = [1, 2]
  edgeConnectivity(:, 3) = [2, 3]
  edgeConnectivity(:, 4) = [3, 4]
  pointsOrder = [4, 1, 2, 3]
END SELECT

IF (ALL([p, q] .EQ. 0_I4B)) THEN
  temp(:, 1) = [xi(1, 1), eta(1, 1)]
  RETURN
END IF

ij(:, 1) = [1, 1]
ij(:, 2) = [p + 1, 1]
ij(:, 3) = [p + 1, q + 1]
ij(:, 4) = [1, q + 1]

IF (ALL([p, q] .GE. 1_I4B)) THEN
  DO ii = 1, 4
    cnt = cnt + 1
    jj = pointsOrder(ii)
    temp(1:2, ii) = [&
      & xi(ij(1, jj), ij(2, jj)), &
      & eta(ij(1, jj), ij(2, jj)) &
      & ]
  END DO
  IF (ALL([p, q] .EQ. 1_I4B)) RETURN

ELSE
  DO ii = 1, MIN(p, 1) + 1
    DO jj = 1, MIN(q, 1) + 1
      cnt = cnt + 1
      temp(1:2, cnt) = [&
        & xi(ij(1, cnt), ij(2, cnt)), &
        & eta(ij(1, cnt), ij(2, cnt))]
    END DO
  END DO
END IF

IF (ALL([p, q] .GE. 1_I4B)) THEN
  DO iedge = 1, tEdges
    p1 = edgeConnectivity(1, iedge)
    p2 = edgeConnectivity(2, iedge)

    IF (ij(1, p1) .EQ. ij(1, p2)) THEN
      ii1 = ij(1, p1)
      ii2 = ii1
      dii = 1
    ELSE IF (ij(1, p1) .LT. ij(1, p2)) THEN
      ii1 = ij(1, p1) + 1
      ii2 = ij(1, p2) - 1
      dii = 1
    ELSE IF (ij(1, p1) .GT. ij(1, p2)) THEN
      ii1 = ij(1, p1) - 1
      ii2 = ij(1, p2) + 1
      dii = -1
    END IF

    IF (ij(2, p1) .EQ. ij(2, p2)) THEN
      jj1 = ij(2, p1)
      jj2 = jj1
      djj = 1
    ELSE IF (ij(2, p1) .LT. ij(2, p2)) THEN
      jj1 = ij(2, p1) + 1
      jj2 = ij(2, p2) - 1
      djj = 1
    ELSE IF (ij(2, p1) .GT. ij(2, p2)) THEN
      jj1 = ij(2, p1) - 1
      jj2 = ij(2, p2) + 1
      djj = -1
    END IF

    DO ii = ii1, ii2, dii
      DO jj = jj1, jj2, djj
        cnt = cnt + 1
        temp(:, cnt) = [xi(ii, jj), eta(ii, jj)]
      END DO
    END DO
  END DO

  ! internal nodes
  IF (ALL([p, q] .GE. 2_I4B)) THEN

    CALL Reallocate( &
      & xi_in,  &
      & MAX(p - 1, 1_I4B), &
      & MAX(q - 1_I4B, 1_I4B))
    CALL Reallocate(eta_in, SIZE(xi_in, 1), SIZE(xi_in, 2))
    CALL Reallocate(temp_in, 2, SIZE(xi_in))

    IF (p .LE. 1_I4B) THEN
      ii1 = 1
      ii2 = 1
    ELSE
      ii1 = 2
      ii2 = p
    END IF

    IF (q .LE. 1_I4B) THEN
      jj1 = 1
      jj2 = 1
    ELSE
      jj1 = 2
      jj2 = q
    END IF

    xi_in = xi(ii1:ii2, jj1:jj2)
    eta_in = eta(ii1:ii2, jj1:jj2)

    CALL IJ2VEFC_Quadrangle_Clockwise( &
      & xi=xi_in,  &
      & eta=eta_in, &
      & temp=temp_in, &
      & p=MAX(p - 2, 0_I4B), &
      & q=MAX(q - 2, 0_I4B), &
      & startNode=startNode)

    ii1 = cnt + 1
    ii2 = ii1 + SIZE(temp_in, 2) - 1
    temp(1:2, ii1:ii2) = temp_in
  END IF

END IF

IF (ALLOCATED(xi_in)) DEALLOCATE (xi_in)
IF (ALLOCATED(eta_in)) DEALLOCATE (eta_in)
IF (ALLOCATED(temp_in)) DEALLOCATE (temp_in)

END PROCEDURE IJ2VEFC_Quadrangle_AntiClockwise

!----------------------------------------------------------------------------
!                                              InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle1
ans = InterpolationPoint_Quadrangle2( &
  & p=order, &
  & q=order, &
  & ipType1=ipType, &
  & ipType2=ipType, &
  & xij=xij, &
  & layout=layout, &
  & alpha1=alpha, &
  & beta1=beta, &
  & lambda1=lambda, &
  & alpha2=alpha, &
  & beta2=beta, &
  & lambda2=lambda &
  & )
END PROCEDURE InterpolationPoint_Quadrangle1

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle2
! internal variables
REAL(DFP) :: x(p + 1), y(q + 1), &
  & xi(p + 1, q + 1), eta(p + 1, q + 1)
REAL(DFP), ALLOCATABLE :: temp(:, :)
INTEGER(I4B) :: ii, jj, kk, nsd

x = InterpolationPoint_Line( &
  & order=p, &
  & ipType=ipType1, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha1, &
  & beta=beta1, &
  & lambda=lambda1)

y = InterpolationPoint_Line( &
  & order=q,  &
  & ipType=ipType2, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha2, &
  & beta=beta2, &
  & lambda=lambda2)

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 2
END IF

CALL Reallocate(ans, nsd, (p + 1) * (q + 1))
CALL Reallocate(temp, 2, (p + 1) * (q + 1))

xi = 0.0_DFP
eta = 0.0_DFP

DO ii = 1, p + 1
  DO jj = 1, q + 1
    xi(ii, jj) = x(ii)
    eta(ii, jj) = y(jj)
  END DO
END DO

IF (layout .EQ. "VEFC") THEN
  CALL IJ2VEFC_Quadrangle(xi=xi, eta=eta, temp=temp, p=p, q=q)
ELSE
  kk = 0
  DO ii = 1, p + 1
    DO jj = 1, q + 1
      kk = kk + 1
      temp(1, kk) = xi(ii, jj)
      temp(2, kk) = eta(ii, jj)
    END DO
  END DO
END IF

IF (PRESENT(xij)) THEN
  ans = FromBiUnitQuadrangle2Quadrangle(xin=temp, x1=xij(:, 1), &
    & x2=xij(:, 2), x3=xij(:, 3), x4=xij(:, 4))
ELSE
  ans = temp
END IF
END PROCEDURE InterpolationPoint_Quadrangle2

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info
ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Quadrangle)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle1

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle2

REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info
vtemp = v; ans = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle2

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle3
INTEGER(I4B) :: info
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle3

!----------------------------------------------------------------------------
!                                                 LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle4
INTEGER(I4B) :: basisType0, ii, jj, indx
REAL(DFP) :: ans1(SIZE(xij, 2), 0:order)
REAL(DFP) :: ans2(SIZE(xij, 2), 0:order)

basisType0 = input(default=Monomial, option=basisType)

IF (basisType0 .EQ. Heirarchical) THEN
  ans = HeirarchicalBasis_Quadrangle2(p=order, q=order, xij=xij)
ELSE
  ans = TensorProdBasis_Quadrangle1(  &
    & p=order, &
    & q=order, &
    & xij=xij, &
    & basisType1=basisType0,  &
    & basisType2=basisType0, &
    & alpha1=alpha,  &
    & beta1=beta, &
    & lambda1=lambda, &
    & alpha2=alpha,  &
    & beta2=beta, &
    & lambda2=lambda)
END IF

CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Quadrangle4

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle5
INTEGER(I4B) :: ii, jj, kk, indx, basisType(2)
REAL(DFP) :: ans1(SIZE(xij, 2), 0:p)
REAL(DFP) :: ans2(SIZE(xij, 2), 0:q)

basisType(1) = input(default=Monomial, option=basisType1)
basisType(2) = input(default=Monomial, option=basisType2)

IF (ALL(basisType .EQ. Heirarchical)) THEN
  ans = HeirarchicalBasis_Quadrangle2(p=p, q=q, xij=xij)
ELSE
  ans = TensorProdBasis_Quadrangle1(  &
    & p=p, &
    & q=q, &
    & xij=xij, &
    & basisType1=basisType(1),  &
    & basisType2=basisType(2), &
    & alpha1=alpha1,  &
    & beta1=beta1, &
    & lambda1=lambda1, &
    & alpha2=alpha2,  &
    & beta2=beta2, &
    & lambda2=lambda2)
END IF

CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Quadrangle5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Quadrangle1
REAL(DFP) :: P1(SIZE(xij, 2), order + 1), P2(SIZE(xij, 2), order + 1)
REAL(DFP) :: x(SIZE(xij, 2)), y(SIZE(xij, 2))
REAL(DFP) :: avec(SIZE(xij, 2)), alpha, beta
INTEGER(I4B) :: k1, k2, max_k2, cnt

x = xij(1, :)
y = xij(2, :)

P1 = LegendreEvalAll(n=order, x=x)

! we do not need x now, so let store (1-y)/2 in x
x = 0.5_DFP * (1.0_DFP - y)
alpha = 0.0_DFP
beta = 0.0_DFP
cnt = 0

DO k1 = 0, order

  avec = (x)**k1 ! note here x = 0.5_DFP*(1-y)
  alpha = 2.0_DFP * k1 + 1.0_DFP

  max_k2 = order - k1

  P2(:, 1:max_k2 + 1) = JacobiEvalAll(n=max_k2, x=y, alpha=alpha, beta=beta)

  DO k2 = 0, max_k2
    cnt = cnt + 1
    ans(:, cnt) = P1(:, k1 + 1) * avec * P2(:, k2 + 1)
  END DO

END DO

END PROCEDURE Dubiner_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Quadrangle2
REAL(DFP) :: xij(2, SIZE(x) * SIZE(y))
INTEGER(I4B) :: ii, jj, cnt

xij = 0.0_DFP
cnt = 0
DO ii = 1, SIZE(x)
  DO jj = 1, SIZE(y)
    cnt = cnt + 1
    xij(1, cnt) = x(ii)
    xij(2, cnt) = y(jj)
  END DO
END DO

ans = Dubiner_Quadrangle1(order=order, xij=xij)

END PROCEDURE Dubiner_Quadrangle2

!----------------------------------------------------------------------------
!                                              TensorProdOrthoPol_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle1
REAL(DFP) :: x(SIZE(xij, 2)), y(SIZE(xij, 2))
REAL(DFP) :: P1(SIZE(xij, 2), p + 1), Q1(SIZE(xij, 2), q + 1)
INTEGER(I4B) :: ii, k1, k2, cnt

x = xij(1, :)
y = xij(2, :)

P1 = BasisEvalAll_Line( &
  & order=p, &
  & x=x, &
  & refLine="BIUNIT", &
  & basisType=basisType1,  &
  & alpha=alpha1, &
  & beta=beta1, &
  & lambda=lambda1)

Q1 = BasisEvalAll_Line( &
  & order=q, &
  & x=y, &
  & refLine="BIUNIT", &
  & basisType=basisType1,  &
  & alpha=alpha2, &
  & beta=beta2, &
  & lambda=lambda2)

cnt = 0

DO k2 = 1, q + 1
  DO k1 = 1, p + 1
    cnt = cnt + 1
    ans(:, cnt) = P1(:, k1) * Q1(:, k2)
  END DO
END DO

END PROCEDURE TensorProdBasis_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle2
REAL(DFP) :: xij(2, SIZE(x) * SIZE(y))
INTEGER(I4B) :: ii, jj, cnt

xij = 0.0_DFP
cnt = 0
DO ii = 1, SIZE(x)
  DO jj = 1, SIZE(y)
    cnt = cnt + 1
    xij(1, cnt) = x(ii)
    xij(2, cnt) = y(jj)
  END DO
END DO

ans = TensorProdBasis_Quadrangle1( &
  & p=p, &
  & q=q, &
  & xij=xij, &
  & basisType1=basisType1, &
  & basisType2=basisType2, &
  & alpha1=alpha1, &
  & alpha2=alpha2, &
  & beta1=beta1, &
  & beta2=beta2, &
  & lambda1=lambda1, &
  & lambda2=lambda2)

END PROCEDURE TensorProdBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                                     VertexBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle1
ans(:, 1) = 0.25_DFP * (1.0_DFP - x) * (1.0_DFP - y)
ans(:, 2) = 0.25_DFP * (1.0_DFP + x) * (1.0_DFP - y)
ans(:, 3) = 0.25_DFP * (1.0_DFP + x) * (1.0_DFP + y)
ans(:, 4) = 0.25_DFP * (1.0_DFP - x) * (1.0_DFP + y)
END PROCEDURE VertexBasis_Quadrangle1

!----------------------------------------------------------------------------
!                                                    VertexBasis_Quadrangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle2
ans(:, 1) = L1(:, 0) * L2(:, 0)
ans(:, 2) = L1(:, 1) * L2(:, 0)
ans(:, 3) = L1(:, 1) * L2(:, 1)
ans(:, 4) = L1(:, 0) * L2(:, 1)
END PROCEDURE VertexBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                                    VertexBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle3
ans = VertexBasis_Quadrangle1( &
  & x=xij(1, :), &
  & y=xij(2, :))
END PROCEDURE VertexBasis_Quadrangle3

!----------------------------------------------------------------------------
!                                               VerticalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VerticalEdgeBasis_Quadrangle
REAL(DFP) :: L2(1:SIZE(y), 0:MAX(qe1, qe2))
INTEGER(I4B) :: maxQ, k2, cnt

maxQ = MAX(qe1, qe2)

L2 = LobattoEvalAll(n=maxQ, x=y)

cnt = 0

DO k2 = 2, qe1
  cnt = cnt + 1
  ans(:, cnt) = 0.5_DFP * (1.0_DFP - x) * L2(:, k2)
END DO

DO k2 = 2, qe2
  cnt = cnt + 1
  ans(:, cnt) = 0.5_DFP * (1.0_DFP + x) * L2(:, k2)
END DO

END PROCEDURE VerticalEdgeBasis_Quadrangle

!----------------------------------------------------------------------------
!                                             VerticalEdgeBasis_Quadrangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE VerticalEdgeBasis_Quadrangle2
INTEGER(I4B) :: k2, cnt

cnt = 0
DO k2 = 2, qe1
  cnt = cnt + 1
  ans(:, cnt) = L1(:, 0) * L2(:, k2)
END DO
DO k2 = 2, qe2
  cnt = cnt + 1
  ans(:, cnt) = L1(:, 1) * L2(:, k2)
END DO

END PROCEDURE VerticalEdgeBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                             HorizontalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HorizontalEdgeBasis_Quadrangle
REAL(DFP) :: L1(1:SIZE(x), 0:MAX(pe3, pe4))
INTEGER(I4B) :: maxP, k1, cnt

maxP = MAX(pe3, pe4)

L1 = LobattoEvalAll(n=maxP, x=x)

cnt = 0

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = 0.5_DFP * (1.0_DFP - y) * L1(:, k1)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = 0.5_DFP * (1.0_DFP + y) * L1(:, k1)
END DO

END PROCEDURE HorizontalEdgeBasis_Quadrangle

!----------------------------------------------------------------------------
!                                             HorizontalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HorizontalEdgeBasis_Quadrangle2
INTEGER(I4B) :: k1, cnt

cnt = 0

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = L1(:, k1) * L2(:, 0)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = L1(:, k1) * L2(:, 1)
END DO

END PROCEDURE HorizontalEdgeBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                                      CellBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Quadrangle
REAL(DFP) :: L1(1:SIZE(x), 0:pb)
REAL(DFP) :: L2(1:SIZE(y), 0:qb)
INTEGER(I4B) :: k1, k2, cnt

L1 = LobattoEvalAll(n=pb, x=x)
L2 = LobattoEvalAll(n=qb, x=y)

cnt = 0

DO k1 = 2, pb
  DO k2 = 2, qb
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L2(:, k2)
  END DO
END DO

END PROCEDURE CellBasis_Quadrangle

!----------------------------------------------------------------------------
!                                                      CellBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Quadrangle2
INTEGER(I4B) :: k1, k2, cnt

cnt = 0

DO k1 = 2, pb
  DO k2 = 2, qb
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L2(:, k2)
  END DO
END DO

END PROCEDURE CellBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle1
INTEGER(I4B) :: a, b, maxP, maxQ
REAL(DFP) :: L1(1:SIZE(xij, 2), 0:MAX(pe3, pe4, pb))
REAL(DFP) :: L2(1:SIZE(xij, 2), 0:MAX(qe1, qe2, qb))

maxP = MAX(pe3, pe4, pb)
maxQ = MAX(qe1, qe2, qb)

L1 = LobattoEvalAll(n=maxP, x=xij(1, :))
L2 = LobattoEvalAll(n=maxQ, x=xij(2, :))

! Vertex basis function

ans(:, 1:4) = VertexBasis_Quadrangle2(L1=L1, L2=L2)

! Edge basis function

b = 4
!
IF (qe1 .GE. 2_I4B .OR. qe2 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + qe1 + qe2 - 2 !4+qe1 + qe2 - 2
  ans(:, a:b) = VerticalEdgeBasis_Quadrangle2( &
    & qe1=qe1, qe2=qe2, L1=L1, L2=L2)
END IF

! Edge basis function

IF (pe3 .GE. 2_I4B .OR. pe4 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe3 + pe4 - 2 !4+pe3 + pe4 - 2
  ans(:, a:b) = HorizontalEdgeBasis_Quadrangle2( &
    & pe3=pe3, pe4=pe4, L1=L1, L2=L2)
END IF

! Cell basis function

IF (pb .GE. 2_I4B .OR. qb .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + (pb - 1) * (qb - 1)
  ans(:, a:b) = CellBasis_Quadrangle2(pb=pb, qb=qb, L1=L1, L2=L2)
END IF
END PROCEDURE HeirarchicalBasis_Quadrangle1

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle2
ans = HeirarchicalBasis_Quadrangle1(pb=p, pe3=p, pe4=p, &
  & qb=q, qe1=q, qe2=q, xij=xij)
END PROCEDURE HeirarchicalBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                                LagrangeEvallAll_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle1
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof
INTEGER(I4B) :: degree(SIZE(xij, 2), 2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2))

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Quadrangle(&
      & order=order, &
      & xij=xij, &
      & basisType=basisType0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda &
      & )
    coeff0 = TRANSPOSE(coeff)
  ELSE
    coeff0 = TRANSPOSE(coeff)
  END IF
ELSE
  coeff0 = TRANSPOSE(LagrangeCoeff_Quadrangle(&
    & order=order, &
    & xij=xij, &
    & basisType=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda &
    & ))
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Quadrangle(order=order)
  tdof = SIZE(xij, 2)

  IF (tdof .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(&
      & msg="tdof is not same as size(degree,1)", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Quadrangle1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  DO ii = 1, tdof
    xx(1, ii) = x(1)**degree(ii, 1) * x(2)**degree(ii, 2)
  END DO

CASE (Heirarchical)

  xx = HeirarchicalBasis_Quadrangle( &
    & p=order, &
    & q=order,  &
    & xij=RESHAPE(x, [2, 1]))

CASE DEFAULT

  xx = TensorProdBasis_Quadrangle( &
    & p=order, &
    & q=order, &
    & xij=RESHAPE(x, [2, 1]),  &
    & basisType1=basisType0, &
    & basisType2=basisType0, &
    & alpha1=alpha, &
    & beta1=beta, &
    & lambda1=lambda, &
    & alpha2=alpha, &
    & beta2=beta, &
    & lambda2=lambda)

END SELECT

ans = MATMUL(coeff0, xx(1, :))

END PROCEDURE LagrangeEvalAll_Quadrangle1

!----------------------------------------------------------------------------
!                                               LagrangeEvalAll_Quadrangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle2
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof
INTEGER(I4B) :: degree(SIZE(xij, 2), 2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2))
REAL(DFP) :: xx(SIZE(x, 2), SIZE(xij, 2))

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Quadrangle(&
      & order=order, &
      & xij=xij, &
      & basisType=basisType0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda &
      & )
    coeff0 = coeff
  ELSE
    coeff0 = coeff
  END IF
ELSE
  coeff0 = LagrangeCoeff_Quadrangle(&
    & order=order, &
    & xij=xij, &
    & basisType=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda &
    & )
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Quadrangle(order=order)
  tdof = SIZE(xij, 2)

  IF (tdof .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(&
      & msg="tdof is not same as size(degree,1)", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Quadrangle1", &
      & line=__LINE__, &
      & unitno=stderr)
  END IF

  DO ii = 1, tdof
    xx(:, ii) = x(1, :)**degree(ii, 1) * x(2, :)**degree(ii, 2)
  END DO

CASE (Heirarchical)

  xx = HeirarchicalBasis_Quadrangle( &
    & p=order, &
    & q=order,  &
    & xij=x)

CASE DEFAULT

  xx = TensorProdBasis_Quadrangle( &
    & p=order, &
    & q=order, &
    & xij=x,  &
    & basisType1=basisType0, &
    & basisType2=basisType0, &
    & alpha1=alpha, &
    & beta1=beta, &
    & lambda1=lambda, &
    & alpha2=alpha, &
    & beta2=beta, &
    & lambda2=lambda)

END SELECT

ans = MATMUL(xx, coeff0)

END PROCEDURE LagrangeEvalAll_Quadrangle2

!----------------------------------------------------------------------------
!                                              QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle1
ans = QuadraturePoint_Quadrangle2( &
  & p=order, &
  & q=order, &
  & quadType1=quadType, &
  & quadType2=quadType, &
  & xij=xij, &
  & refQuadrangle=refQuadrangle, &
  & alpha1=alpha, &
  & beta1=beta, &
  & lambda1=lambda, &
  & alpha2=alpha, &
  & beta2=beta, &
  & lambda2=lambda &
  & )
END PROCEDURE QuadraturePoint_Quadrangle1

!----------------------------------------------------------------------------
!                                             QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle2
! internal variables
REAL(DFP), ALLOCATABLE :: x(:, :), y(:, :), temp(:, :)
INTEGER(I4B) :: ii, jj, kk, nsd, np, nq
TYPE(String) :: astr

astr = TRIM(UpperCase(refQuadrangle))

x = QuadraturePoint_Line( &
  & order=p, &
  & quadType=quadType1, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha1, &
  & beta=beta1, &
  & lambda=lambda1)

np = SIZE(x, 2)

y = QuadraturePoint_Line( &
  & order=q,  &
  & quadType=quadType2, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha2, &
  & beta=beta2, &
  & lambda=lambda2)

nq = SIZE(y, 2)

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 2
END IF

CALL Reallocate(ans, nsd + 1_I4B, np * nq)
CALL Reallocate(temp, 3_I4B, np * nq)

kk = 0
DO ii = 1, np
  DO jj = 1, nq
    kk = kk + 1
    temp(1, kk) = x(1, ii)
    temp(2, kk) = y(1, jj)
    temp(3, kk) = x(2, ii) * y(2, jj)
  END DO
END DO

IF (PRESENT(xij)) THEN
  ans(1:nsd, :) = FromBiUnitQuadrangle2Quadrangle( &
    & xin=temp(1:2, :), &
    & x1=xij(:, 1), &
    & x2=xij(:, 2), &
    & x3=xij(:, 3), &
    & x4=xij(:, 4))
  ans(nsd + 1, :) = temp(3, :)
ELSE
  IF (astr%chars() .EQ. "UNIT") THEN
    ans(1:nsd, :) = FromBiUnitQuadrangle2UnitQuadrangle( &
      & xin=temp(1:2, :))
    ans(nsd + 1, :) = temp(3, :)
  ELSE
    ans = temp
  END IF
END IF

IF (ALLOCATED(temp)) DEALLOCATE (temp)
IF (ALLOCATED(x)) DEALLOCATE (x)
IF (ALLOCATED(y)) DEALLOCATE (y)

END PROCEDURE QuadraturePoint_Quadrangle2

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle3
ans = QuadraturePoint_Quadrangle4( &
  & nipsx=nips, &
  & nipsy=nips, &
  & quadType1=quadType, &
  & quadType2=quadType, &
  & refQuadrangle=refQuadrangle, &
  & xij=xij, &
  & alpha1=alpha, &
  & beta1=beta, &
  & lambda1=lambda, &
  & alpha2=alpha, &
  & beta2=beta, &
  & lambda2=lambda &
  & )
END PROCEDURE QuadraturePoint_Quadrangle3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle4
! internal variables
REAL(DFP) :: x(2, nipsx(1)), y(2, nipsy(1)), temp(3, nipsy(1) * nipsx(1))
INTEGER(I4B) :: ii, jj, kk, nsd, np, nq
TYPE(String) :: astr

astr = TRIM(UpperCase(refQuadrangle))

x = QuadraturePoint_Line( &
  & nips=nipsx, &
  & quadType=quadType1, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha1, &
  & beta=beta1, &
  & lambda=lambda1)

np = SIZE(x, 2)

y = QuadraturePoint_Line( &
  & nips=nipsy,  &
  & quadType=quadType2, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha2, &
  & beta=beta2, &
  & lambda=lambda2)

nq = SIZE(y, 2)

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 2
END IF

CALL Reallocate(ans, nsd + 1_I4B, np * nq)

kk = 0
DO ii = 1, np
  DO jj = 1, nq
    kk = kk + 1
    temp(1, kk) = x(1, ii)
    temp(2, kk) = y(1, jj)
    temp(3, kk) = x(2, ii) * y(2, jj)
  END DO
END DO

IF (PRESENT(xij)) THEN
  ans(1:nsd, :) = FromBiUnitQuadrangle2Quadrangle( &
    & xin=temp(1:2, :), &
    & x1=xij(:, 1), &
    & x2=xij(:, 2), &
    & x3=xij(:, 3), &
    & x4=xij(:, 4))
  ans(nsd + 1, :) = temp(3, :)
ELSE
  IF (astr%chars() .EQ. "UNIT") THEN
    ans(1:nsd, :) = FromBiUnitQuadrangle2UnitQuadrangle( &
      & xin=temp(1:2, :))
    ans(nsd + 1, :) = temp(3, :)
  ELSE
    ans = temp
  END IF
END IF

END PROCEDURE QuadraturePoint_Quadrangle4

!----------------------------------------------------------------------------
!                                               QuadraturePoint_Quadrangle3
!----------------------------------------------------------------------------

END SUBMODULE Methods
