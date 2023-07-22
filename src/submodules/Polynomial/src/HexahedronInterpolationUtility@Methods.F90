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

SUBMODULE(HexahedronInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                               QuadratureNumber_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadratureNumber_Hexahedron
ans(1) = QuadratureNumber_Line(order=p, quadType=quadType1)
ans(2) = QuadratureNumber_Line(order=q, quadType=quadType2)
ans(3) = QuadratureNumber_Line(order=r, quadType=quadType3)
END PROCEDURE QuadratureNumber_Hexahedron

!----------------------------------------------------------------------------
!                                               EdgeConnectivity_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeConnectivity_Hexahedron
ans(:, 1) = [1, 2]
ans(:, 2) = [1, 4]
ans(:, 3) = [1, 5]
ans(:, 4) = [2, 3]
ans(:, 5) = [2, 6]
ans(:, 6) = [3, 4]
ans(:, 7) = [3, 7]
ans(:, 8) = [4, 8]
ans(:, 9) = [5, 6]
ans(:, 10) = [5, 8]
ans(:, 11) = [6, 7]
ans(:, 12) = [7, 8]
END PROCEDURE EdgeConnectivity_Hexahedron

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Hexahedron
ans(:, 1) = [1, 4, 3, 2] ! back
ans(:, 2) = [5, 6, 7, 8] ! front
ans(:, 3) = [1, 5, 8, 4] ! left
ans(:, 4) = [2, 3, 7, 6] ! right
ans(:, 5) = [3, 4, 8, 7] ! bottom
ans(:, 6) = [1, 2, 6, 5] ! top

! B, F, L, R, T, Bo
END PROCEDURE FacetConnectivity_Hexahedron

!----------------------------------------------------------------------------
!                                                       RefHexahedronCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefHexahedronCoord
REAL(DFP) :: one, mone
TYPE(String) :: astr

astr = UpperCase(refHexahedron)

SELECT CASE (astr%chars())
CASE ("UNIT")
  one = 1.0_DFP
  mone = 0.0_DFP
CASE ("BIUNIT")
  one = 1.0_DFP
  mone = -1.0_DFP
END SELECT

ans(3, 1:4) = mone
ans(3, 5:8) = one
ans(1:2, 1:4) = RefQuadrangleCoord(refHexahedron)
ans(1:2, 5:8) = ans(1:2, 1:4)
END PROCEDURE RefHexahedronCoord

!----------------------------------------------------------------------------
!                                                LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Hexahedron1
INTEGER(I4B) :: n, ii, jj, kk, indx
n = LagrangeDOF_Hexahedron(order=order)
ALLOCATE (ans(n, 3))
indx = 0
DO kk = 0, order
  DO jj = 0, order
    DO ii = 0, order
      indx = indx + 1
      ans(indx, 1) = ii
      ans(indx, 2) = jj
      ans(indx, 3) = kk
    END DO
  END DO
END DO
END PROCEDURE LagrangeDegree_Hexahedron1

!----------------------------------------------------------------------------
!                                                LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Hexahedron2
INTEGER(I4B) :: n, ii, jj, kk, indx
n = LagrangeDOF_Hexahedron(p=p, q=q, r=r)
ALLOCATE (ans(n, 3))
indx = 0
DO kk = 0, r
  DO jj = 0, q
    DO ii = 0, p
      indx = indx + 1
      ans(indx, 1) = ii
      ans(indx, 2) = jj
      ans(indx, 3) = kk
    END DO
  END DO
END DO
END PROCEDURE LagrangeDegree_Hexahedron2

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Hexahedron1
ans = (order + 1)**3
END PROCEDURE LagrangeDOF_Hexahedron1

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Hexahedron2
ans = (p + 1) * (q + 1) * (r + 1)
END PROCEDURE LagrangeDOF_Hexahedron2

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Hexahedron1
ans = (order - 1)**3
END PROCEDURE LagrangeInDOF_Hexahedron1

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Hexahedron2
ans = (p - 1) * (q - 1) * (r - 1)
END PROCEDURE LagrangeInDOF_Hexahedron2

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron1
ans = EquidistancePoint_Hexahedron2(p=order, q=order, r=order, xij=xij)
END PROCEDURE EquidistancePoint_Hexahedron1

!----------------------------------------------------------------------------
!                                             EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron2
! internal variables
REAL(DFP) :: x(p + 1), y(q + 1), z(r + 1), temp0
REAL(DFP), DIMENSION(p + 1, q + 1, r + 1) :: xi, eta, zeta
REAL(DFP) :: temp(3, (p + 1) * (q + 1) * (r + 1))
INTEGER(I4B) :: ii, jj, kk, nsd

x = EquidistancePoint_Line(order=p, xij=[-1.0_DFP, 1.0_DFP])
y = EquidistancePoint_Line(order=q, xij=[-1.0_DFP, 1.0_DFP])
z = EquidistancePoint_Line(order=r, xij=[-1.0_DFP, 1.0_DFP])
IF (p .GT. 0_I4B) THEN
  temp0 = x(2)
END IF
DO ii = 2, p
  x(ii) = x(ii + 1)
END DO
x(p + 1) = temp0

IF (q .GT. 0_I4B) THEN
  temp0 = y(2)
END IF
DO ii = 2, q
  y(ii) = y(ii + 1)
END DO
y(q + 1) = temp0

IF (r .GT. 0_I4B) THEN
  temp0 = z(2)
END IF
DO ii = 2, r
  z(ii) = z(ii + 1)
END DO
z(r + 1) = temp0

nsd = 3
CALL Reallocate(ans, nsd, (p + 1) * (q + 1) * (r + 1))

DO ii = 1, p + 1
  DO jj = 1, q + 1
    DO kk = 1, r + 1
      xi(ii, jj, kk) = x(ii)
      eta(ii, jj, kk) = y(jj)
      zeta(ii, jj, kk) = z(kk)
    END DO
  END DO
END DO

CALL IJK2VEFC_Hexahedron( &
  & xi=xi,  &
  & eta=eta, &
  & zeta=zeta, &
  & temp=temp, &
  & p=p, &
  & q=q, &
  & r=r)

IF (PRESENT(xij)) THEN
  ans = FromBiUnitHexahedron2Hexahedron( &
    & xin=temp,     &
    & x1=xij(:, 1), &
    & x2=xij(:, 2), &
    & x3=xij(:, 3), &
    & x4=xij(:, 4), &
    & x5=xij(:, 5), &
    & x6=xij(:, 6), &
    & x7=xij(:, 7), &
    & x8=xij(:, 8)  &
    & )
ELSE
  ans = temp
END IF

END PROCEDURE EquidistancePoint_Hexahedron2

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Hexahedron1
ans = EquidistanceInPoint_Hexahedron2(p=order, q=order, r=order, xij=xij)
END PROCEDURE EquidistanceInPoint_Hexahedron1

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Hexahedron2
INTEGER(I4B) :: i1, i2, ii
REAL(DFP), ALLOCATABLE :: ans0(:, :)

ans0 = EquidistancePoint_Hexahedron(p=p, q=q, r=r, xij=xij)
i1 = LagrangeDOF_Hexahedron(p=p, q=q, r=r)
i2 = LagrangeInDOF_Hexahedron(p=p, q=q, r=r)
CALL reallocate(ans, 3, i2)
ii = i1 - i2
IF (ii + 1 .LE. SIZE(ans0, 2)) ans = ans0(:, ii + 1:)
IF (ALLOCATED(ans0)) DEALLOCATE (ans0)
END PROCEDURE EquidistanceInPoint_Hexahedron2

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Hexahedron1
ans = InterpolationPoint_Hexahedron( &
  & p=order, &
  & q=order, &
  & r=order, &
  & layout=layout, &
  & ipType1=ipType, &
  & ipType2=ipType,  &
  & ipType3=ipType, &
  & alpha1=alpha, &
  & alpha2=alpha, &
  & alpha3=alpha, &
  & beta1=beta, &
  & beta2=beta, &
  & beta3=beta, &
  & lambda1=lambda, &
  & lambda2=lambda, &
  & lambda3=lambda, &
  & xij=xij)
END PROCEDURE InterpolationPoint_Hexahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Hexahedron2
! internal variables
REAL(DFP) :: x(p + 1), y(q + 1), z(r + 1)
REAL(DFP), DIMENSION(p + 1, q + 1, r + 1) :: xi, eta, zeta
REAL(DFP), ALLOCATABLE :: temp(:, :)
INTEGER(I4B) :: ii, jj, kk, nsd
CHARACTER(*), PARAMETER :: myName = "InterpolationPoint_Hexahedron2"

x = InterpolationPoint_Line(order=p, ipType=ipType1, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha1, beta=beta1, lambda=lambda1 &
  & )

y = InterpolationPoint_Line(order=q, ipType=ipType2, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha2, beta=beta2, lambda=lambda2 &
  & )

z = InterpolationPoint_Line(order=r, ipType=ipType3, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha3, beta=beta3, lambda=lambda3 &
  & )

nsd = 3

CALL Reallocate(ans, nsd, (p + 1) * (q + 1) * (r + 1))
CALL Reallocate(temp, nsd, (p + 1) * (q + 1) * (r + 1))

xi = 0.0_DFP
eta = 0.0_DFP
zeta = 0.0_DFP

DO ii = 1, p + 1
  DO jj = 1, q + 1
    DO kk = 1, r + 1
      xi(ii, jj, kk) = x(ii)
      eta(ii, jj, kk) = y(jj)
      zeta(ii, jj, kk) = z(kk)
    END DO
  END DO
END DO

CALL IJK2VEFC_Hexahedron( &
  & xi=xi,  &
  & eta=eta, &
  & zeta=zeta, &
  & temp=temp, &
  & p=p, &
  & q=q, &
  & r=r)

IF (PRESENT(xij)) THEN
  ans = FromBiUnitHexahedron2Hexahedron( &
    & xin=temp,     &
    & x1=xij(:, 1), &
    & x2=xij(:, 2), &
    & x3=xij(:, 3), &
    & x4=xij(:, 4), &
    & x5=xij(:, 5), &
    & x6=xij(:, 6), &
    & x7=xij(:, 7), &
    & x8=xij(:, 8)  &
    & )
ELSE
  ans = temp
END IF
END PROCEDURE InterpolationPoint_Hexahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJK2VEFC_Hexahedron
! internal variables
INTEGER(I4B) :: cnt, ii, jj, kk, ll, N, &
  & ii1, ii2, jj1, jj2, kk1, kk2, ijk(3, 8),  &
  & iedge, iface, p1, p2, dii, djj, dkk, startNode
INTEGER(I4B), PARAMETER :: tPoints = 8, tEdges = 12, tFacets = 6
INTEGER(I4B) :: edgeConnectivity(2, tEdges)
INTEGER(I4B) :: facetConnectivity(4, tFacets)
REAL(DFP), ALLOCATABLE :: temp2d(:, :), temp_in(:, :)
REAL(DFP), ALLOCATABLE :: xi_in(:, :, :), eta_in(:, :, :), zeta_in(:, :, :)

! vertices
IF (ALL([p, q, r] .EQ. 0_I4B)) THEN
  temp(:, 1) = [xi(1, 1, 1), eta(1, 1, 1), zeta(1, 1, 1)]
  RETURN
END IF

N = (p + 1) * (q + 1) * (r + 1)
cnt = 0

ijk(:, 1) = [1, 1, 1]
ijk(:, 2) = [p + 1, 1, 1]
ijk(:, 3) = [p + 1, q + 1, 1]
ijk(:, 4) = [1, q + 1, 1]
ijk(:, 5) = [1, 1, r + 1]
ijk(:, 6) = [p + 1, 1, r + 1]
ijk(:, 7) = [p + 1, q + 1, r + 1]
ijk(:, 8) = [1, q + 1, r + 1]

edgeConnectivity = EdgeConnectivity_Hexahedron()
facetConnectivity = FacetConnectivity_Hexahedron()

IF (ALL([p, q, r] .GE. 1_I4B)) THEN
  DO ii = 1, 8
    cnt = cnt + 1
    temp(:, ii) = [&
      & xi(ijk(1, ii), ijk(2, ii), ijk(3, ii)), &
      & eta(ijk(1, ii), ijk(2, ii), ijk(3, ii)), &
      & zeta(ijk(1, ii), ijk(2, ii), ijk(3, ii))]
  END DO

  IF (ALL([p, q, r] .EQ. 1_I4B)) RETURN

ELSE

  DO ii = 1, p + 1
    DO jj = 1, q + 1
      DO kk = 1, r + 1
        cnt = cnt + 1
        temp(:, cnt) = &
            & [ &
            & xi(ii, jj, kk),  &
            & eta(ii, jj, kk), &
            & zeta(ii, jj, kk) &
            & ]
      END DO
    END DO
  END DO

END IF

IF (ALL([p, q, r] .GE. 1_I4B)) THEN
  DO iedge = 1, tEdges
    p1 = edgeConnectivity(1, iedge)
    p2 = edgeConnectivity(2, iedge)

    IF (ijk(1, p1) .EQ. ijk(1, p2)) THEN
      ii1 = ijk(1, p1)
      ii2 = ii1
      dii = 1
    ELSE IF (ijk(1, p1) .LT. ijk(1, p2)) THEN
      ii1 = ijk(1, p1) + 1
      ii2 = ijk(1, p2) - 1
      dii = 1
    ELSE IF (ijk(1, p1) .GT. ijk(1, p2)) THEN
      ii1 = ijk(1, p1) - 1
      ii2 = ijk(1, p2) + 1
      dii = -1
    END IF

    IF (ijk(2, p1) .EQ. ijk(2, p2)) THEN
      jj1 = ijk(2, p1)
      jj2 = jj1
      djj = 1
    ELSE IF (ijk(2, p1) .LT. ijk(2, p2)) THEN
      jj1 = ijk(2, p1) + 1
      jj2 = ijk(2, p2) - 1
      djj = 1
    ELSE IF (ijk(2, p1) .GT. ijk(2, p2)) THEN
      jj1 = ijk(2, p1) - 1
      jj2 = ijk(2, p2) + 1
      djj = -1
    END IF

    IF (ijk(3, p1) .EQ. ijk(3, p2)) THEN
      kk1 = ijk(3, p1)
      kk2 = kk1
      dkk = 1
    ELSE IF (ijk(3, p1) .LT. ijk(3, p2)) THEN
      kk1 = ijk(3, p1) + 1
      kk2 = ijk(3, p2) - 1
      dkk = 1
    ELSE IF (ijk(3, p1) .GT. ijk(3, p2)) THEN
      kk1 = ijk(3, p1) - 1
      kk2 = ijk(3, p2) + 1
      dkk = -1
    END IF

    DO ii = ii1, ii2, dii
      DO jj = jj1, jj2, djj
        DO kk = kk1, kk2, dkk
          cnt = cnt + 1
          temp(:, cnt) = [ &
            & xi(ii, jj, kk), &
            & eta(ii, jj, kk), &
            & zeta(ii, jj, kk)]
        END DO
      END DO
    END DO
  END DO

  ! face 1, x-y, clockwise, startNode
  kk = 1
  startNode = 1
  CALL Reallocate(temp2d, 2, (p + 1) * (q + 1))
  CALL IJ2VEFC_Quadrangle_Clockwise( &
    & xi=xi(:, :, kk), &
    & eta=eta(:, :, kk), &
    & temp=temp2d, &
    & p=p, &
    & q=q, &
    & startNode=startNode)

  IF ((p + 1) * (q + 1) .GE. 2 * (p + q) + 1) THEN
    ii1 = cnt + 1
    ii2 = cnt + (p - 1) * (q - 1)
    cnt = ii2
    temp(1:2, ii1:ii2) = temp2d(1:2, 2 * (p + q) + 1:)
    temp(3, ii1:ii2) = zeta(1, 1, kk) !!-1.0_DFP ! TODO
  END IF

  ! face 2, x-y, anticlockwise
  kk = r + 1
  startNode = 1
  CALL IJ2VEFC_Quadrangle_AntiClockwise( &
    & xi=xi(:, :, kk), &
    & eta=eta(:, :, kk), &
    & temp=temp2d, &
    & p=p, &
    & q=q, &
    & startNode=startNode)

  IF ((p + 1) * (q + 1) .GE. 2 * (p + q) + 1) THEN
    ii1 = cnt + 1
    ii2 = cnt + (p - 1) * (q - 1)
    cnt = ii2
    temp(1:2, ii1:ii2) = temp2d(1:2, 2 * (p + q) + 1:)
    temp(3, ii1:ii2) = zeta(1, 1, kk) !! 1.0_DFP ! TODO
  END IF

  ! face-3
  ! z-y
  ! clockwise
  ii = 1
  startNode = 1
  CALL Reallocate(temp2d, 2, (r + 1) * (q + 1))
  CALL IJ2VEFC_Quadrangle_AntiClockwise( &
    & xi=TRANSPOSE(zeta(ii, :, :)), &
    & eta=TRANSPOSE(eta(ii, :, :)), &
    & temp=temp2d, &
    & p=r, &
    & q=q, &
    & startNode=startNode)

  IF ((r + 1) * (q + 1) .GE. 2 * (r + q) + 1) THEN
    ii1 = cnt + 1
    ii2 = cnt + (r - 1) * (q - 1)
    cnt = ii2
    temp(1, ii1:ii2) = xi(ii, 1, 1) !!-1.0_DFP
    temp(2, ii1:ii2) = temp2d(2, 2 * (r + q) + 1:)
    temp(3, ii1:ii2) = temp2d(1, 2 * (r + q) + 1:)
  END IF

  ! face 4
  ! z-y
  ! anticlockwise
  ii = p + 1
  startNode = 1
  CALL IJ2VEFC_Quadrangle_Clockwise( &
    & xi=TRANSPOSE(zeta(ii, :, :)), &
    & eta=TRANSPOSE(eta(ii, :, :)), &
    & temp=temp2d, &
    & p=r, &
    & q=q, &
    & startNode=startNode)

  IF ((r + 1) * (q + 1) .GE. 2 * (r + q) + 1) THEN
    ii1 = cnt + 1
    ii2 = cnt + (r - 1) * (q - 1)
    cnt = ii2
    temp(1, ii1:ii2) = xi(ii, 1, 1) !!1.0_DFP ! TODO
    temp(2, ii1:ii2) = temp2d(2, 2 * (r + q) + 1:)
    temp(3, ii1:ii2) = temp2d(1, 2 * (r + q) + 1:)
  END IF

  ! face 5
  ! z-x
  ! anticlockwise
  jj = q + 1
  startNode = 4
  CALL Reallocate(temp2d, 2, (r + 1) * (p + 1))
  CALL IJ2VEFC_Quadrangle_AntiClockwise( &
    & xi=TRANSPOSE(zeta(:, jj, :)), &
    & eta=TRANSPOSE(xi(:, jj, :)), &
    & temp=temp2d, &
    & p=r, &
    & q=p, &
    & startNode=startNode)

  IF ((r + 1) * (p + 1) .GE. 2 * (r + p) + 1) THEN
    ii1 = cnt + 1
    ii2 = cnt + (r - 1) * (p - 1)
    cnt = ii2
    temp(1, ii1:ii2) = temp2d(2, 2 * (r + p) + 1:)
    temp(2, ii1:ii2) = eta(1, jj, 1)
    temp(3, ii1:ii2) = temp2d(1, 2 * (r + p) + 1:)
  END IF

  ! face 6
  ! z-x
  ! clockwise
  jj = 1
  startNode = 1
  CALL IJ2VEFC_Quadrangle_Clockwise( &
    & xi=TRANSPOSE(zeta(:, jj, :)), &
    & eta=TRANSPOSE(xi(:, jj, :)), &
    & temp=temp2d, &
    & p=r, &
    & q=p, &
    & startNode=startNode)

  IF ((r + 1) * (p + 1) .GE. 2 * (r + p) + 1) THEN
    ii1 = cnt + 1
    ii2 = cnt + (r - 1) * (p - 1)
    cnt = ii2
    temp(1, ii1:ii2) = temp2d(2, 2 * (r + p) + 1:)
    temp(2, ii1:ii2) = eta(1, jj, 1)
    temp(3, ii1:ii2) = temp2d(1, 2 * (r + p) + 1:)
  END IF

  ! internal nodes
  IF (ALL([p, q, r] .GE. 2_I4B)) THEN

    CALL Reallocate(  &
      & xi_in, &
      & MAX(p - 1, 1_I4B), &
      & MAX(q - 1_I4B, 1_I4B), &
      & MAX(r - 1_I4B, 1_I4B))
    CALL Reallocate(eta_in, SIZE(xi_in, 1), SIZE(xi_in, 2), SIZE(xi_in, 3))
    CALL Reallocate(zeta_in, SIZE(xi_in, 1), SIZE(xi_in, 2), SIZE(xi_in, 3))
    CALL Reallocate(temp_in, 3, SIZE(xi_in))

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

    IF (r .LE. 1_I4B) THEN
      kk1 = 1
      kk2 = 1
    ELSE
      kk1 = 2
      kk2 = r
    END IF

    xi_in = xi(ii1:p, jj1:q, kk1:r)
    eta_in = eta(ii1:p, jj1:q, kk1:r)
    zeta_in = zeta(ii1:p, jj1:q, kk1:r)

    CALL IJK2VEFC_Hexahedron( &
      & xi=xi_in,  &
      & eta=eta_in, &
      & zeta=zeta_in, &
      & temp=temp_in, &
      & p=MAX(p - 2, 0_I4B), &
      & q=MAX(q - 2, 0_I4B), &
      & r=MAX(r - 2, 0_I4B))

    ii1 = cnt + 1
    ii2 = ii1 + SIZE(temp_in, 2) - 1
    temp(1:3, ii1:ii2) = temp_in
  END IF

END IF

END PROCEDURE IJK2VEFC_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info

ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Hexahedron)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans, IPIV=ipiv, info=info)

END PROCEDURE LagrangeCoeff_Hexahedron1

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron2
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info
vtemp = v; ans = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Hexahedron2

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron3
INTEGER(I4B) :: info
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Hexahedron3

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron4
INTEGER(I4B) :: basisType0, ii, jj, kk, indx
REAL(DFP) :: ans1(SIZE(xij, 2), 0:order)
REAL(DFP) :: ans2(SIZE(xij, 2), 0:order)
REAL(DFP) :: ans3(SIZE(xij, 2), 0:order)

basisType0 = input(default=Monomial, option=basisType)
SELECT CASE (basisType0)
CASE (Monomial)
  ans = LagrangeVandermonde(order=order, xij=xij, elemType=Hexahedron)

CASE (Legendre, Jacobi, Lobatto, Chebyshev, Ultraspherical)

  IF (basisType0 .EQ. Jacobi) THEN
    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg(&
        & msg="alpha and beta should be present for basisType=Jacobi", &
        & file=__FILE__, &
        & routine="LagrangeCoeff_Hexahedron4", &
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
        & routine="LagrangeCoeff_Hexahedron4", &
        & line=__LINE__, &
        & unitno=stderr)
      RETURN
    END IF
  END IF

  ans1 = EvalAllOrthopol(  &
    & n=order, &
    & x=xij(1, :), &
    & orthopol=basisType0,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & lambda=lambda)

  ans2 = EvalAllOrthopol(  &
    & n=order, &
    & x=xij(2, :), &
    & orthopol=basisType0,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & lambda=lambda)

  ans3 = EvalAllOrthopol(  &
    & n=order, &
    & x=xij(3, :), &
    & orthopol=basisType0,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & lambda=lambda)

  indx = 0
  DO kk = 0, order
    DO jj = 0, order
      DO ii = 0, order
        indx = indx + 1
        ans(:, indx) = ans1(:, ii) * ans2(:, jj) * ans3(:, kk)
      END DO
    END DO
  END DO

CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for basisType", &
    & file=__FILE__, &
    & routine="LagrangeCoeff_Hexahedron4", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Hexahedron4

!----------------------------------------------------------------------------
!                                                 LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron5
INTEGER(I4B) :: basisType0, ii, jj, kk, indx, basisType(3)
REAL(DFP) :: ans1(SIZE(xij, 2), 0:p)
REAL(DFP) :: ans2(SIZE(xij, 2), 0:q)
REAL(DFP) :: ans3(SIZE(xij, 2), 0:r)

basisType(1) = input(default=Monomial, option=basisType1)
basisType(2) = input(default=Monomial, option=basisType2)
basisType(3) = input(default=Monomial, option=basisType3)

basisType0 = basisType(1)
SELECT CASE (basisType0)
CASE (Monomial)
  ans1 = LagrangeVandermonde(order=p, xij=xij(1:1, :), elemType=Line)

CASE (Legendre, Jacobi, Lobatto, Chebyshev, Ultraspherical)

  ans1 = EvalAllOrthopol(  &
    & n=p, &
    & x=xij(1, :), &
    & orthopol=basisType0,  &
    & alpha=alpha1,  &
    & beta=beta1,  &
    & lambda=lambda1)

CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for basisType1", &
    & file=__FILE__, &
    & routine="LagrangeCoeff_Hexahedron5", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT

basisType0 = basisType(2)
SELECT CASE (basisType0)
CASE (Monomial)
  ans2 = LagrangeVandermonde(order=q, xij=xij(2:2, :), elemType=Line)

CASE (Legendre, Jacobi, Lobatto, Chebyshev, Ultraspherical)

  ans2 = EvalAllOrthopol(  &
    & n=q, &
    & x=xij(2, :), &
    & orthopol=basisType0,  &
    & alpha=alpha2,  &
    & beta=beta2,  &
    & lambda=lambda2)

CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for basisType2", &
    & file=__FILE__, &
    & routine="LagrangeCoeff_Hexahedron5", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT

basisType0 = basisType(3)
SELECT CASE (basisType0)
CASE (Monomial)
  ans3 = LagrangeVandermonde(order=r, xij=xij(3:3, :), elemType=Line)

CASE (Legendre, Jacobi, Lobatto, Chebyshev, Ultraspherical)

  ans3 = EvalAllOrthopol(  &
    & n=r, &
    & x=xij(3, :), &
    & orthopol=basisType0,  &
    & alpha=alpha3,  &
    & beta=beta3,  &
    & lambda=lambda3)

CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for basisType3", &
    & file=__FILE__, &
    & routine="LagrangeCoeff_Hexahedron5", &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT

indx = 0
DO kk = 0, r
  DO jj = 0, q
    DO ii = 0, p
      indx = indx + 1
      ans(:, indx) = ans1(:, ii) * ans2(:, jj) * ans3(:, kk)
    END DO
  END DO
END DO

CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Hexahedron5

!----------------------------------------------------------------------------
!                                               TensorProdBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Hexahedron1
REAL(DFP) :: x(SIZE(xij, 2)), y(SIZE(xij, 2)), z(SIZE(xij, 2))
REAL(DFP) :: P1(SIZE(xij, 2), p + 1)
REAL(DFP) :: Q1(SIZE(xij, 2), q + 1)
REAL(DFP) :: R1(SIZE(xij, 2), r + 1)
INTEGER(I4B) :: ii, k1, k2, k3, cnt

x = xij(1, :)
y = xij(2, :)
z = xij(3, :)

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

R1 = BasisEvalAll_Line( &
  & order=r, &
  & x=z, &
  & refLine="BIUNIT", &
  & basisType=basisType3,  &
  & alpha=alpha3, &
  & beta=beta3, &
  & lambda=lambda3)

cnt = 0

DO k3 = 1, r + 1
  DO k2 = 1, q + 1
    DO k1 = 1, p + 1
      cnt = cnt + 1
      ans(:, cnt) = P1(:, k1) * Q1(:, k2) * R1(:, k3)
    END DO
  END DO
END DO

END PROCEDURE TensorProdBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                 TensorProdBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Hexahedron2
REAL(DFP) :: xij(3, SIZE(x) * SIZE(y) * SIZE(z))
INTEGER(I4B) :: ii, jj, cnt, kk

xij = 0.0_DFP
cnt = 0
DO ii = 1, SIZE(x)
  DO jj = 1, SIZE(y)
    DO kk = 1, SIZE(z)
      cnt = cnt + 1
      xij(1, cnt) = x(ii)
      xij(2, cnt) = y(jj)
      xij(3, cnt) = z(kk)
    END DO
  END DO
END DO

ans = TensorProdBasis_Hexahedron1( &
  & p=p, &
  & q=q, &
  & r=r, &
  & xij=xij, &
  & basisType1=basisType1, &
  & basisType2=basisType2, &
  & basisType3=basisType3, &
  & alpha1=alpha1, &
  & alpha2=alpha2, &
  & alpha3=alpha3, &
  & beta1=beta1, &
  & beta2=beta2, &
  & beta3=beta3, &
  & lambda1=lambda1, &
  & lambda2=lambda2, &
  & lambda3=lambda3)

END PROCEDURE TensorProdBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                                     VertexBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Hexahedron1
ans(:, 1) = 0.125_DFP * (1.0_DFP - x) * (1.0_DFP - y) * (1.0_DFP - z)
ans(:, 2) = 0.125_DFP * (1.0_DFP + x) * (1.0_DFP - y) * (1.0_DFP - z)
ans(:, 3) = 0.125_DFP * (1.0_DFP + x) * (1.0_DFP + y) * (1.0_DFP - z)
ans(:, 4) = 0.125_DFP * (1.0_DFP - x) * (1.0_DFP + y) * (1.0_DFP - z)
ans(:, 5) = 0.125_DFP * (1.0_DFP - x) * (1.0_DFP - y) * (1.0_DFP + z)
ans(:, 6) = 0.125_DFP * (1.0_DFP + x) * (1.0_DFP - y) * (1.0_DFP + z)
ans(:, 7) = 0.125_DFP * (1.0_DFP + x) * (1.0_DFP + y) * (1.0_DFP + z)
ans(:, 8) = 0.125_DFP * (1.0_DFP - x) * (1.0_DFP + y) * (1.0_DFP + z)
END PROCEDURE VertexBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                   VertexBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Hexahedron2
ans(:, 1) = L1(:, 0) * L2(:, 0) * L3(:, 0)
ans(:, 2) = L1(:, 1) * L2(:, 0) * L3(:, 0)
ans(:, 3) = L1(:, 1) * L2(:, 1) * L3(:, 0)
ans(:, 4) = L1(:, 0) * L2(:, 1) * L3(:, 0)
ans(:, 5) = L1(:, 0) * L2(:, 0) * L3(:, 1)
ans(:, 6) = L1(:, 1) * L2(:, 0) * L3(:, 1)
ans(:, 7) = L1(:, 1) * L2(:, 1) * L3(:, 1)
ans(:, 8) = L1(:, 0) * L2(:, 1) * L3(:, 1)
END PROCEDURE VertexBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                                   VertexBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Hexahedron3
ans = VertexBasis_Hexahedron1( &
  & x=xij(1, :), &
  & y=xij(2, :), &
  & z=xij(3, :) &
  & )
END PROCEDURE VertexBasis_Hexahedron3

!----------------------------------------------------------------------------
!                                                   xEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xEdgeBasis_Hexahedron1
REAL(DFP) :: L1(1:SIZE(x), 0:MAXVAL([pe1, pe2, pe3, pe4]))
INTEGER(I4B) :: maxP, k1, cnt

maxP = SIZE(L1, 2) - 1_I4B
L1 = LobattoEvalAll(n=maxP, x=x)

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L1(:, k1) * (1.0_DFP - y) * (1.0_DFP - z)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L1(:, k1) * (1.0_DFP + y) * (1.0_DFP - z)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L1(:, k1) * (1.0_DFP - y) * (1.0_DFP + z)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L1(:, k1) * (1.0_DFP + y) * (1.0_DFP + z)
END DO

END PROCEDURE xEdgeBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                   xEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xEdgeBasis_Hexahedron2
INTEGER(I4B) :: cnt, k1

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt) = L1(:, k1) * L2(:, 0) * L3(:, 0)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt) = L1(:, k1) * L2(:, 1) * L3(:, 0)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = L1(:, k1) * L2(:, 0) * L3(:, 1)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = L1(:, k1) * L2(:, 1) * L3(:, 1)
END DO
END PROCEDURE xEdgeBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                                     yEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE yEdgeBasis_Hexahedron1
REAL(DFP) :: L2(1:SIZE(y), 0:MAXVAL([pe1, pe2, pe3, pe4]))
INTEGER(I4B) :: maxP, k1, cnt

maxP = SIZE(L2, 2) - 1_I4B
L2 = LobattoEvalAll(n=maxP, x=y)

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L2(:, k1) * (1.0_DFP - x) * (1.0_DFP - z)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L2(:, k1) * (1.0_DFP + x) * (1.0_DFP - z)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L2(:, k1) * (1.0_DFP - x) * (1.0_DFP + z)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L2(:, k1) * (1.0_DFP + x) * (1.0_DFP + z)
END DO

END PROCEDURE yEdgeBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                   yEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE yEdgeBasis_Hexahedron2
INTEGER(I4B) :: cnt, k1

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt) = L2(:, k1) * L1(:, 0) * L3(:, 0)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt) = L2(:, k1) * L1(:, 1) * L3(:, 0)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = L2(:, k1) * L1(:, 0) * L3(:, 1)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = L2(:, k1) * L1(:, 1) * L3(:, 1)
END DO
END PROCEDURE yEdgeBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                                     zEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE zEdgeBasis_Hexahedron1
REAL(DFP) :: L3(1:SIZE(y), 0:MAXVAL([pe1, pe2, pe3, pe4]))
INTEGER(I4B) :: maxP, k1, cnt

maxP = SIZE(L3, 2) - 1_I4B
L3 = LobattoEvalAll(n=maxP, x=z)

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L3(:, k1) * (1.0_DFP - x) * (1.0_DFP - y)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L3(:, k1) * (1.0_DFP + x) * (1.0_DFP - y)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L3(:, k1) * (1.0_DFP - x) * (1.0_DFP + y)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * L3(:, k1) * (1.0_DFP + x) * (1.0_DFP + y)
END DO
END PROCEDURE zEdgeBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                   zEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE zEdgeBasis_Hexahedron2
INTEGER(I4B) :: cnt, k1

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt) = L3(:, k1) * L1(:, 0) * L2(:, 0)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt) = L3(:, k1) * L1(:, 1) * L2(:, 0)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = L3(:, k1) * L1(:, 0) * L2(:, 1)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = L3(:, k1) * L1(:, 1) * L2(:, 1)
END DO
END PROCEDURE zEdgeBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                                     EdgeBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Hexahedron1
SELECT CASE (dim)
CASE (1_I4B)
  ans = xEdgeBasis_Hexahedron1(pe1=pe1, pe2=pe2, pe3=pe3, pe4=pe4, &
    &  x=x, y=y, z=z)
CASE (2_I4B)
  ans = yEdgeBasis_Hexahedron1(pe1=pe1, pe2=pe2, pe3=pe3, pe4=pe4, &
    &  x=x, y=y, z=z)
CASE (3_I4B)
  ans = zEdgeBasis_Hexahedron1(pe1=pe1, pe2=pe2, pe3=pe3, pe4=pe4, &
    &  x=x, y=y, z=z)
END SELECT
END PROCEDURE EdgeBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                   EdgeBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Hexahedron2
SELECT CASE (dim)
CASE (1_I4B)
  ans = xEdgeBasis_Hexahedron2(pe1=pe1, pe2=pe2, pe3=pe3, pe4=pe4, &
    & L1=L1, L2=L2, L3=L3)
CASE (2_I4B)
  ans = yEdgeBasis_Hexahedron2(pe1=pe1, pe2=pe2, pe3=pe3, pe4=pe4, &
    & L1=L1, L2=L2, L3=L3)
CASE (3_I4B)
  ans = zEdgeBasis_Hexahedron2(pe1=pe1, pe2=pe2, pe3=pe3, pe4=pe4, &
    & L1=L1, L2=L2, L3=L3)
END SELECT
END PROCEDURE EdgeBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                                    xyFacetBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xyFacetBasis_Hexahedron1
REAL(DFP) :: L1(1:SIZE(x), 0:n1)
REAL(DFP) :: L2(1:SIZE(y), 0:n2)
INTEGER(I4B) :: k1, cnt, k2

L1 = LobattoEvalAll(n=n1, x=x)
L2 = LobattoEvalAll(n=n2, x=y)

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L2(:, k2) * 0.5_DFP * (1.0_DFP - z)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L2(:, k2) * 0.5_DFP * (1.0_DFP + z)
  END DO
END DO
END PROCEDURE xyFacetBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                    xyFacetBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xyFacetBasis_Hexahedron2
INTEGER(I4B) :: k1, cnt, k2

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L2(:, k2) * L3(:, 0)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L2(:, k2) * L3(:, 1)
  END DO
END DO
END PROCEDURE xyFacetBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                                    yzFacetBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE yzFacetBasis_Hexahedron1
REAL(DFP) :: L2(1:SIZE(y), 0:n1)
REAL(DFP) :: L3(1:SIZE(z), 0:n2)
INTEGER(I4B) :: k1, cnt, k2

L2 = LobattoEvalAll(n=n1, x=y)
L3 = LobattoEvalAll(n=n2, x=z)

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L2(:, k1) * L3(:, k2) * 0.5_DFP * (1.0_DFP - x)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L2(:, k1) * L3(:, k2) * 0.5_DFP * (1.0_DFP + x)
  END DO
END DO
END PROCEDURE yzFacetBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                    yzFacetBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE yzFacetBasis_Hexahedron2
INTEGER(I4B) :: k1, cnt, k2

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L2(:, k1) * L3(:, k2) * L1(:, 0)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L2(:, k1) * L3(:, k2) * L1(:, 1)
  END DO
END DO
END PROCEDURE yzFacetBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                                    xzFacetBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xzFacetBasis_Hexahedron1
REAL(DFP) :: L1(1:SIZE(x), 0:n1)
REAL(DFP) :: L3(1:SIZE(z), 0:n2)
INTEGER(I4B) :: k1, cnt, k2

L1 = LobattoEvalAll(n=n1, x=x)
L3 = LobattoEvalAll(n=n2, x=z)

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L3(:, k2) * 0.5_DFP * (1.0_DFP - y)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L3(:, k2) * 0.5_DFP * (1.0_DFP + y)
  END DO
END DO
END PROCEDURE xzFacetBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                    xzFacetBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xzFacetBasis_Hexahedron2
INTEGER(I4B) :: k1, cnt, k2

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L3(:, k2) * L2(:, 0)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L3(:, k2) * L2(:, 1)
  END DO
END DO
END PROCEDURE xzFacetBasis_Hexahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
