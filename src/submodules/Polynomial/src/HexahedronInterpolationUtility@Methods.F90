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
!                                                 RefElemDomain_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Hexahedron
ans = "BIUNIT"
END PROCEDURE RefElemDomain_Hexahedron

!----------------------------------------------------------------------------
!                                                 GetVertexDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVertexDOF_Hexahedron
ans = 8_I4B
END PROCEDURE GetVertexDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   GetEdgeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeDOF_Hexahedron1
ans = MAX(pe1 + pe2 + pe3 + pe4 - 4_I4B, 0_I4B)
END PROCEDURE GetEdgeDOF_Hexahedron1

!----------------------------------------------------------------------------
!                                                   GetEdgeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeDOF_Hexahedron2
ans = GetEdgeDOF_Hexahedron(p, p, p, p) &
  & + GetEdgeDOF_Hexahedron(q, q, q, q) &
  & + GetEdgeDOF_Hexahedron(r, r, r, r)
END PROCEDURE GetEdgeDOF_Hexahedron2

!----------------------------------------------------------------------------
!                                                   GetEdgeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeDOF_Hexahedron3
ans = GetEdgeDOF_Hexahedron(p, p, p)
END PROCEDURE GetEdgeDOF_Hexahedron3

!----------------------------------------------------------------------------
!                                                   GetEdgeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeDOF_Hexahedron4
ans = GetEdgeDOF_Hexahedron(px1, px2, px3, px4) &
  & + GetEdgeDOF_Hexahedron(py1, py2, py3, py4) &
  & + GetEdgeDOF_Hexahedron(pz1, pz2, pz3, pz4)
END PROCEDURE GetEdgeDOF_Hexahedron4

!----------------------------------------------------------------------------
!                                                   GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFacetDOF_Hexahedron1
ans = GetFacetDOF_Hexahedron(pxy1, pxy2) &
  & + GetFacetDOF_Hexahedron(pxz1, pxz2) &
  & + GetFacetDOF_Hexahedron(pyz1, pyz2)
ans = 2_I4B * ans
END PROCEDURE GetFacetDOF_Hexahedron1

!----------------------------------------------------------------------------
!                                                   GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFacetDOF_Hexahedron2
ans = GetFacetDOF_Hexahedron(p, q) &
  & + GetFacetDOF_Hexahedron(p, r) &
  & + GetFacetDOF_Hexahedron(q, r)
ans = ans * 2_I4B
END PROCEDURE GetFacetDOF_Hexahedron2

!----------------------------------------------------------------------------
!                                                   GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFacetDOF_Hexahedron3
ans = (p - 1) * (q - 1)
END PROCEDURE GetFacetDOF_Hexahedron3

!----------------------------------------------------------------------------
!                                                   GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFacetDOF_Hexahedron4
ans = GetFacetDOF_Hexahedron(p, p) * 6_I4B
END PROCEDURE GetFacetDOF_Hexahedron4

!----------------------------------------------------------------------------
!                                                   GetCellDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetCellDOF_Hexahedron1
ans = (p - 1) * (q - 1) * (r - 1)
END PROCEDURE GetCellDOF_Hexahedron1

!----------------------------------------------------------------------------
!                                                   GetCellDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetCellDOF_Hexahedron2
ans = GetCellDOF_Hexahedron(p, p, p)
END PROCEDURE GetCellDOF_Hexahedron2

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
CALL GetEdgeConnectivity_Hexahedron(con=ans)
END PROCEDURE EdgeConnectivity_Hexahedron

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Hexahedron
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
  ans(:, 1) = [1, 2, 3, 4] ! back
  ans(:, 2) = [5, 6, 7, 8] ! front
  ans(:, 3) = [1, 4, 8, 5] ! left
  ans(:, 4) = [2, 3, 7, 6] ! right
  ans(:, 5) = [1, 2, 6, 5] ! bottom
  ans(:, 6) = [4, 3, 7, 8] ! top
CASE DEFAULT
  ans(:, 1) = [1, 4, 3, 2] ! back
  ans(:, 2) = [5, 6, 7, 8] ! front
  ans(:, 3) = [1, 5, 8, 4] ! left
  ans(:, 4) = [2, 3, 7, 6] ! right
  ans(:, 5) = [1, 2, 6, 5] ! bottom
  ans(:, 6) = [3, 4, 8, 7] ! top
END SELECT

END PROCEDURE FacetConnectivity_Hexahedron

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
!                                                    GetTotalDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Hexahedron
ans = (order + 1)**3
END PROCEDURE GetTotalDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   GetTotalInDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Hexahedron
ans = (order - 1)**3
END PROCEDURE GetTotalInDOF_Hexahedron

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
!
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron1
INTEGER(I4B) :: nrow, ncol
nrow = 3
ncol = LagrangeDOF_Hexahedron(order=order)
ALLOCATE (ans(nrow, ncol))
CALL EquidistancePoint_Hexahedron1_(order=order, ans=ans, nrow=nrow, &
                                    ncol=ncol, xij=xij)
END PROCEDURE EquidistancePoint_Hexahedron1

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron1_
CALL EquidistancePoint_Hexahedron2_(p=order, q=order, r=order, xij=xij, &
                                    ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE EquidistancePoint_Hexahedron1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron2
INTEGER(I4B) :: nrow, ncol
nrow = 3
ncol = LagrangeDOF_Hexahedron(p=p, q=q, r=r)
ALLOCATE (ans(nrow, ncol))
CALL EquidistancePoint_Hexahedron2_(p=p, q=q, r=r, ans=ans, nrow=nrow, &
                                    ncol=ncol, xij=xij)
END PROCEDURE EquidistancePoint_Hexahedron2

!----------------------------------------------------------------------------
!                                             EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron2_
! internal variables
REAL(DFP) :: x(p + 1), y(q + 1), z(r + 1), temp0
REAL(DFP), DIMENSION(p + 1, q + 1, r + 1) :: xi, eta, zeta
REAL(DFP) :: temp(3, (p + 1) * (q + 1) * (r + 1))
INTEGER(I4B) :: ii, jj, kk, nsd

nrow = 3
ncol = LagrangeDOF_Hexahedron(p=p, q=q, r=r)

x = EquidistancePoint_Line(order=p, xij=[-1.0_DFP, 1.0_DFP])
y = EquidistancePoint_Line(order=q, xij=[-1.0_DFP, 1.0_DFP])
z = EquidistancePoint_Line(order=r, xij=[-1.0_DFP, 1.0_DFP])

IF (p .GT. 0_I4B) temp0 = x(2)
DO CONCURRENT(ii=2:p)
  x(ii) = x(ii + 1)
END DO
x(p + 1) = temp0

IF (q .GT. 0_I4B) temp0 = y(2)
DO CONCURRENT(ii=2:q)
  y(ii) = y(ii + 1)
END DO
y(q + 1) = temp0

IF (r .GT. 0_I4B) temp0 = z(2)
DO CONCURRENT(ii=2:r)
  z(ii) = z(ii + 1)
END DO
z(r + 1) = temp0

! nsd = 3
! CALL Reallocate(ans, nsd, (p + 1) * (q + 1) * (r + 1))

DO CONCURRENT(ii=1:p + 1, jj=1:q + 1, kk=1:r + 1)
  xi(ii, jj, kk) = x(ii)
  eta(ii, jj, kk) = y(jj)
  zeta(ii, jj, kk) = z(kk)
END DO

CALL IJK2VEFC_Hexahedron(xi=xi, eta=eta, zeta=zeta, temp=temp, p=p, q=q, r=r)

IF (PRESENT(xij)) THEN

  ans(1:nrow, 1:ncol) = FromBiUnitHexahedron2Hexahedron(xin=temp, &
                     x1=xij(:, 1), x2=xij(:, 2), x3=xij(:, 3), x4=xij(:, 4), &
                       x5=xij(:, 5), x6=xij(:, 6), x7=xij(:, 7), x8=xij(:, 8))

ELSE

  ans(1:nrow, 1:ncol) = temp

END IF

END PROCEDURE EquidistancePoint_Hexahedron2_

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
INTEGER(I4B) :: ii, jj, kk, nsd, cnt
TYPE(String) :: astr

astr = TRIM(UpperCase(layout))

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

IF (astr%chars() .EQ. "VEFC") THEN
  CALL IJK2VEFC_Hexahedron( &
    & xi=xi,  &
    & eta=eta, &
    & zeta=zeta, &
    & temp=temp, &
    & p=p, &
    & q=q, &
    & r=r)
ELSE
  cnt = 0
  DO ii = 1, p + 1
    DO jj = 1, q + 1
      DO kk = 1, r + 1
        cnt = cnt + 1
        temp(1, cnt) = x(ii)
        temp(2, cnt) = y(ii)
        temp(3, cnt) = z(ii)
      END DO
    END DO
  END DO
END IF

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

edgeConnectivity = EdgeConnectivity_Hexahedron( &
  & baseInterpol="Lagrange",  &
  & baseContinuity="H1")

facetConnectivity = FacetConnectivity_Hexahedron( &
  & baseInterpol="Lagrange",  &
  & baseContinuity="H1")

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
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Hexahedron1_(order=order, i=i, xij=xij, ans=ans, &
                                tsize=tsize)
END PROCEDURE LagrangeCoeff_Hexahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron1_
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: v
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info, nrow, ncol

tsize = SIZE(xij, 2)

ipiv = 0_I4B; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP

CALL LagrangeVandermonde_(order=order, xij=xij, elemType=Hexahedron, &
                          ans=v, nrow=nrow, ncol=ncol)
CALL GetLU(A=v, IPIV=ipiv, info=info)
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)

END PROCEDURE LagrangeCoeff_Hexahedron1_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron2
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Hexahedron2_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                                ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Hexahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron2_
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

vtemp = v; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Hexahedron2_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Hexahedron3_(order=order, i=i, v=v, ipiv=ipiv, &
                                ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Hexahedron3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron3_
INTEGER(I4B) :: info

tsize = SIZE(v, 1)
ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Hexahedron3_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron4
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Hexahedron5_(p=order, q=order, r=order, xij=xij, &
           basisType1=basisType, basisType2=basisType, basisType3=basisType, &
         alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
                   lambda2=lambda, alpha3=alpha, beta3=beta, lambda3=lambda, &
                   refHexahedron=refHexahedron, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Hexahedron4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron4_
CALL LagrangeCoeff_Hexahedron5_(p=order, q=order, r=order, xij=xij, &
           basisType1=basisType, basisType2=basisType, basisType3=basisType, &
         alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
                   lambda2=lambda, alpha3=alpha, beta3=beta, lambda3=lambda, &
                   refHexahedron=refHexahedron, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Hexahedron4_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron5
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Hexahedron5_(p=p, q=q, r=r, xij=xij, &
        basisType1=basisType1, basisType2=basisType2, basisType3=basisType3, &
    alpha1=alpha1, beta1=beta1, lambda1=lambda1, alpha2=alpha2, beta2=beta2, &
               lambda2=lambda2, alpha3=alpha3, beta3=beta3, lambda3=lambda3, &
                   refHexahedron=refHexahedron, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Hexahedron5

!----------------------------------------------------------------------------
!                                                 LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron5_
INTEGER(I4B) :: basisType0, ii, jj, kk, indx, basisType(3)
REAL(DFP) :: ans1(SIZE(xij, 2), 0:p)
REAL(DFP) :: ans2(SIZE(xij, 2), 0:q)
REAL(DFP) :: ans3(SIZE(xij, 2), 0:r)

basisType(1) = Input(default=Monomial, option=basisType1)
basisType(2) = Input(default=Monomial, option=basisType2)
basisType(3) = Input(default=Monomial, option=basisType3)

nrow = SIZE(xij, 2)
ncol = nrow

basisType0 = basisType(1)
SELECT CASE (basisType0)
CASE (Monomial)
  CALL LagrangeVandermonde_(order=p, xij=xij(1:1, :), elemType=Line, &
                            ans=ans1, nrow=ii, ncol=jj)

CASE (Legendre, Jacobi, Lobatto, Chebyshev, Ultraspherical)
  CALL EvalAllOrthopol_(n=p, x=xij(1, :), orthopol=basisType0, &
                        alpha=alpha1, beta=beta1, lambda=lambda1, &
                        ans=ans1, nrow=ii, ncol=jj)

CASE DEFAULT
  CALL Errormsg(msg="No case found for basisType1", &
                routine="LagrangeCoeff_Hexahedron5", &
                file=__FILE__, line=__LINE__, unitno=stderr)

  RETURN
END SELECT

basisType0 = basisType(2)
SELECT CASE (basisType0)
CASE (Monomial)
  CALL LagrangeVandermonde_(order=q, xij=xij(2:2, :), elemType=Line, &
                            ans=ans2, nrow=ii, ncol=jj)

CASE (Legendre, Jacobi, Lobatto, Chebyshev, Ultraspherical)

  CALL EvalAllOrthopol_(n=q, x=xij(2, :), orthopol=basisType0, &
                        alpha=alpha2, beta=beta2, lambda=lambda2, &
                        ans=ans2, nrow=ii, ncol=jj)

CASE DEFAULT
  CALL Errormsg(msg="No case found for basisType2", &
                routine="LagrangeCoeff_Hexahedron5", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN
END SELECT

basisType0 = basisType(3)
SELECT CASE (basisType0)
CASE (Monomial)
  CALL LagrangeVandermonde_(order=r, xij=xij(3:3, :), elemType=Line, &
                            ans=ans3, nrow=ii, ncol=jj)

CASE (Legendre, Jacobi, Lobatto, Chebyshev, Ultraspherical)
  CALL EvalAllOrthopol_(n=r, x=xij(3, :), orthopol=basisType0, &
                        alpha=alpha3, beta=beta3, lambda=lambda3, &
                        ans=ans3, nrow=ii, ncol=jj)

CASE DEFAULT
  CALL Errormsg(msg="No case found for basisType3", &
                routine="LagrangeCoeff_Hexahedron5", &
                file=__FILE__, line=__LINE__, unitno=stderr)

  RETURN
END SELECT

indx = 0
DO kk = 0, r
  DO jj = 0, q
    DO ii = 0, p
      indx = indx + 1
      ans(1:nrow, indx) = &
        ans1(1:nrow, ii) * ans2(1:nrow, jj) * ans3(1:nrow, kk)
    END DO
  END DO
END DO

CALL GetInvMat(ans(1:nrow, 1:ncol))
END PROCEDURE LagrangeCoeff_Hexahedron5_

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
!                                                     VertexBasis_Hexahedron
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
!                                             VertexBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasisGradient_Hexahedron2
ans(:, 1, 1) = dL1(:, 0) * L2(:, 0) * L3(:, 0)
ans(:, 2, 1) = dL1(:, 1) * L2(:, 0) * L3(:, 0)
ans(:, 3, 1) = dL1(:, 1) * L2(:, 1) * L3(:, 0)
ans(:, 4, 1) = dL1(:, 0) * L2(:, 1) * L3(:, 0)
ans(:, 5, 1) = dL1(:, 0) * L2(:, 0) * L3(:, 1)
ans(:, 6, 1) = dL1(:, 1) * L2(:, 0) * L3(:, 1)
ans(:, 7, 1) = dL1(:, 1) * L2(:, 1) * L3(:, 1)
ans(:, 8, 1) = dL1(:, 0) * L2(:, 1) * L3(:, 1)

ans(:, 1, 2) = L1(:, 0) * dL2(:, 0) * L3(:, 0)
ans(:, 2, 2) = L1(:, 1) * dL2(:, 0) * L3(:, 0)
ans(:, 3, 2) = L1(:, 1) * dL2(:, 1) * L3(:, 0)
ans(:, 4, 2) = L1(:, 0) * dL2(:, 1) * L3(:, 0)
ans(:, 5, 2) = L1(:, 0) * dL2(:, 0) * L3(:, 1)
ans(:, 6, 2) = L1(:, 1) * dL2(:, 0) * L3(:, 1)
ans(:, 7, 2) = L1(:, 1) * dL2(:, 1) * L3(:, 1)
ans(:, 8, 2) = L1(:, 0) * dL2(:, 1) * L3(:, 1)

ans(:, 1, 3) = L1(:, 0) * L2(:, 0) * dL3(:, 0)
ans(:, 2, 3) = L1(:, 1) * L2(:, 0) * dL3(:, 0)
ans(:, 3, 3) = L1(:, 1) * L2(:, 1) * dL3(:, 0)
ans(:, 4, 3) = L1(:, 0) * L2(:, 1) * dL3(:, 0)
ans(:, 5, 3) = L1(:, 0) * L2(:, 0) * dL3(:, 1)
ans(:, 6, 3) = L1(:, 1) * L2(:, 0) * dL3(:, 1)
ans(:, 7, 3) = L1(:, 1) * L2(:, 1) * dL3(:, 1)
ans(:, 8, 3) = L1(:, 0) * L2(:, 1) * dL3(:, 1)
END PROCEDURE VertexBasisGradient_Hexahedron2

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
!                                             xEdgeBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xEdgeBasisGradient_Hexahedron2
INTEGER(I4B) :: cnt, k1

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, k1) * L2(:, 0) * L3(:, 0)
  ans(:, cnt, 2) = L1(:, k1) * dL2(:, 0) * L3(:, 0)
  ans(:, cnt, 3) = L1(:, k1) * L2(:, 0) * dL3(:, 0)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, k1) * L2(:, 1) * L3(:, 0)
  ans(:, cnt, 2) = L1(:, k1) * dL2(:, 1) * L3(:, 0)
  ans(:, cnt, 3) = L1(:, k1) * L2(:, 1) * dL3(:, 0)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, k1) * L2(:, 0) * L3(:, 1)
  ans(:, cnt, 2) = L1(:, k1) * dL2(:, 0) * L3(:, 1)
  ans(:, cnt, 3) = L1(:, k1) * L2(:, 0) * dL3(:, 1)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, k1) * L2(:, 1) * L3(:, 1)
  ans(:, cnt, 2) = L1(:, k1) * dL2(:, 1) * L3(:, 1)
  ans(:, cnt, 3) = L1(:, k1) * L2(:, 1) * dL3(:, 1)
END DO
END PROCEDURE xEdgeBasisGradient_Hexahedron2

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
!                                             yEdgeBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE yEdgeBasisGradient_Hexahedron2
INTEGER(I4B) :: cnt, k1

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, 0) * L2(:, k1) * L3(:, 0)
  ans(:, cnt, 2) = L1(:, 0) * dL2(:, k1) * L3(:, 0)
  ans(:, cnt, 3) = L1(:, 0) * L2(:, k1) * dL3(:, 0)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, 1) * L2(:, k1) * L3(:, 0)
  ans(:, cnt, 2) = L1(:, 1) * dL2(:, k1) * L3(:, 0)
  ans(:, cnt, 3) = L1(:, 1) * L2(:, k1) * dL3(:, 0)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, 0) * L2(:, k1) * L3(:, 1)
  ans(:, cnt, 2) = L1(:, 0) * dL2(:, k1) * L3(:, 1)
  ans(:, cnt, 3) = L1(:, 0) * L2(:, k1) * dL3(:, 1)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, 1) * L2(:, k1) * L3(:, 1)
  ans(:, cnt, 2) = L1(:, 1) * dL2(:, k1) * L3(:, 1)
  ans(:, cnt, 3) = L1(:, 1) * L2(:, k1) * dL3(:, 1)
END DO
END PROCEDURE yEdgeBasisGradient_Hexahedron2

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
!                                             zEdgeBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE zEdgeBasisGradient_Hexahedron2
INTEGER(I4B) :: cnt, k1

cnt = 0
DO k1 = 2, pe1
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, 0) * L2(:, 0) * L3(:, k1)
  ans(:, cnt, 2) = L1(:, 0) * dL2(:, 0) * L3(:, k1)
  ans(:, cnt, 3) = L1(:, 0) * L2(:, 0) * dL3(:, k1)
END DO

DO k1 = 2, pe2
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, 1) * L2(:, 0) * L3(:, k1)
  ans(:, cnt, 2) = L1(:, 1) * dL2(:, 0) * L3(:, k1)
  ans(:, cnt, 3) = L1(:, 1) * L2(:, 0) * dL3(:, k1)
END DO

DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, 0) * L2(:, 1) * L3(:, k1)
  ans(:, cnt, 2) = L1(:, 0) * dL2(:, 1) * L3(:, k1)
  ans(:, cnt, 3) = L1(:, 0) * L2(:, 1) * dL3(:, k1)
END DO

DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt, 1) = dL1(:, 1) * L2(:, 1) * L3(:, k1)
  ans(:, cnt, 2) = L1(:, 1) * dL2(:, 1) * L3(:, k1)
  ans(:, cnt, 3) = L1(:, 1) * L2(:, 1) * dL3(:, k1)
END DO
END PROCEDURE zEdgeBasisGradient_Hexahedron2

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
!                                             EdgeBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasisGradient_Hexahedron2
SELECT CASE (dim)
CASE (1_I4B)
  ans = xEdgeBasisGradient_Hexahedron2( &
    & pe1=pe1, &
    & pe2=pe2, &
    & pe3=pe3, &
    & pe4=pe4, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
CASE (2_I4B)
  ans = yEdgeBasisGradient_Hexahedron2( &
    & pe1=pe1, &
    & pe2=pe2, &
    & pe3=pe3, &
    & pe4=pe4, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
CASE (3_I4B)
  ans = zEdgeBasisGradient_Hexahedron2( &
    & pe1=pe1, &
    & pe2=pe2, &
    & pe3=pe3, &
    & pe4=pe4, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
END SELECT
END PROCEDURE EdgeBasisGradient_Hexahedron2

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
!                                           xyFacetBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xyFacetBasisGradient_Hexahedron2
INTEGER(I4B) :: k1, cnt, k2

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt, 1) = dL1(:, k1) * L2(:, k2) * L3(:, 0)
    ans(:, cnt, 2) = L1(:, k1) * dL2(:, k2) * L3(:, 0)
    ans(:, cnt, 3) = L1(:, k1) * L2(:, k2) * dL3(:, 0)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt, 1) = dL1(:, k1) * L2(:, k2) * L3(:, 1)
    ans(:, cnt, 2) = L1(:, k1) * dL2(:, k2) * L3(:, 1)
    ans(:, cnt, 3) = L1(:, k1) * L2(:, k2) * dL3(:, 1)
  END DO
END DO
END PROCEDURE xyFacetBasisGradient_Hexahedron2

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
!                                           yzFacetBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE yzFacetBasisGradient_Hexahedron2
INTEGER(I4B) :: k1, cnt, k2

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt, 1) = dL1(:, 0) * L2(:, k1) * L3(:, k2)
    ans(:, cnt, 2) = L1(:, 0) * dL2(:, k1) * L3(:, k2)
    ans(:, cnt, 3) = L1(:, 0) * L2(:, k1) * dL3(:, k2)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt, 1) = dL1(:, 1) * L2(:, k1) * L3(:, k2)
    ans(:, cnt, 2) = L1(:, 1) * dL2(:, k1) * L3(:, k2)
    ans(:, cnt, 3) = L1(:, 1) * L2(:, k1) * dL3(:, k2)
  END DO
END DO
END PROCEDURE yzFacetBasisGradient_Hexahedron2

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
!                                           xzFacetBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE xzFacetBasisGradient_Hexahedron2
INTEGER(I4B) :: k1, cnt, k2

cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt, 1) = dL1(:, k1) * L2(:, 0) * L3(:, k2)
    ans(:, cnt, 2) = L1(:, k1) * dL2(:, 0) * L3(:, k2)
    ans(:, cnt, 3) = L1(:, k1) * L2(:, 0) * dL3(:, k2)
  END DO
END DO

DO k1 = 2, n1
  DO k2 = 2, n2
    cnt = cnt + 1
    ans(:, cnt, 1) = dL1(:, k1) * L2(:, 1) * L3(:, k2)
    ans(:, cnt, 2) = L1(:, k1) * dL2(:, 1) * L3(:, k2)
    ans(:, cnt, 3) = L1(:, k1) * L2(:, 1) * dL3(:, k2)
  END DO
END DO
END PROCEDURE xzFacetBasisGradient_Hexahedron2

!----------------------------------------------------------------------------
!                                                      FacetBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetBasis_Hexahedron1

SELECT CASE (dim1)
CASE (1_I4B)
  SELECT CASE (dim2)
  CASE (2_I4B)
    ans = xyFacetBasis_Hexahedron1(n1=n1, n2=n2, x=x, y=y, z=z)
  CASE (3_I4B)
    ans = xzFacetBasis_Hexahedron1(n1=n1, n2=n2, x=x, y=y, z=z)
  END SELECT
CASE (2_I4B)
  SELECT CASE (dim2)
  CASE (1_I4B)
    ans = xyFacetBasis_Hexahedron1(n1=n1, n2=n2, x=x, y=y, z=z)
  CASE (3_I4B)
    ans = yzFacetBasis_Hexahedron1(n1=n1, n2=n2, x=x, y=y, z=z)
  END SELECT
CASE (3_I4B)
  SELECT CASE (dim2)
  CASE (1_I4B)
    ans = xzFacetBasis_Hexahedron1(n1=n1, n2=n2, x=x, y=y, z=z)
  CASE (2_I4B)
    ans = yzFacetBasis_Hexahedron1(n1=n1, n2=n2, x=x, y=y, z=z)
  END SELECT
END SELECT

END PROCEDURE FacetBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                     FacetBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetBasis_Hexahedron2

SELECT CASE (dim1)
CASE (1_I4B)
  SELECT CASE (dim2)
  CASE (2_I4B)
    ans = xyFacetBasis_Hexahedron2(n1=n1, n2=n2, L1=L1, L2=L2, L3=L3)
  CASE (3_I4B)
    ans = xzFacetBasis_Hexahedron2(n1=n1, n2=n2, L1=L1, L2=L2, L3=L3)
  END SELECT
CASE (2_I4B)
  SELECT CASE (dim2)
  CASE (1_I4B)
    ans = xyFacetBasis_Hexahedron2(n1=n1, n2=n2, L1=L1, L2=L2, L3=L3)
  CASE (3_I4B)
    ans = yzFacetBasis_Hexahedron2(n1=n1, n2=n2, L1=L1, L2=L2, L3=L3)
  END SELECT
CASE (3_I4B)
  SELECT CASE (dim2)
  CASE (1_I4B)
    ans = xzFacetBasis_Hexahedron2(n1=n1, n2=n2, L1=L1, L2=L2, L3=L3)
  CASE (2_I4B)
    ans = yzFacetBasis_Hexahedron2(n1=n1, n2=n2, L1=L1, L2=L2, L3=L3)
  END SELECT
END SELECT
END PROCEDURE FacetBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                             FacetBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetBasisGradient_Hexahedron2

SELECT CASE (dim1)
CASE (1_I4B)
  SELECT CASE (dim2)
  CASE (2_I4B)
    ans = xyFacetBasisGradient_Hexahedron2( &
      & n1=n1, &
      & n2=n2, &
      & L1=L1, &
      & L2=L2, &
      & L3=L3, &
      & dL1=dL1, &
      & dL2=dL2, &
      & dL3=dL3 &
      & )
  CASE (3_I4B)
    ans = xzFacetBasisGradient_Hexahedron2( &
      & n1=n1, &
      & n2=n2, &
      & L1=L1, &
      & L2=L2, &
      & L3=L3, &
      & dL1=dL1, &
      & dL2=dL2, &
      & dL3=dL3 &
      & )
  END SELECT
CASE (2_I4B)
  SELECT CASE (dim2)
  CASE (1_I4B)
    ans = xyFacetBasisGradient_Hexahedron2( &
      & n1=n1, &
      & n2=n2, &
      & L1=L1, &
      & L2=L2, &
      & L3=L3, &
      & dL1=dL1, &
      & dL2=dL2, &
      & dL3=dL3 &
      & )
  CASE (3_I4B)
    ans = yzFacetBasisGradient_Hexahedron2( &
      & n1=n1, &
      & n2=n2, &
      & L1=L1, &
      & L2=L2, &
      & L3=L3, &
      & dL1=dL1, &
      & dL2=dL2, &
      & dL3=dL3 &
      & )
  END SELECT
CASE (3_I4B)
  SELECT CASE (dim2)
  CASE (1_I4B)
    ans = xzFacetBasisGradient_Hexahedron2( &
      & n1=n1, &
      & n2=n2, &
      & L1=L1, &
      & L2=L2, &
      & L3=L3, &
      & dL1=dL1, &
      & dL2=dL2, &
      & dL3=dL3 &
      & )
  CASE (2_I4B)
    ans = yzFacetBasisGradient_Hexahedron2( &
      & n1=n1, &
      & n2=n2, &
      & L1=L1, &
      & L2=L2, &
      & L3=L3, &
      & dL1=dL1, &
      & dL2=dL2, &
      & dL3=dL3 &
      & )
  END SELECT
END SELECT
END PROCEDURE FacetBasisGradient_Hexahedron2

!----------------------------------------------------------------------------
!                                                       CellBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Hexahedron1
REAL(DFP) :: L1(1:SIZE(x), 0:n1)
REAL(DFP) :: L2(1:SIZE(y), 0:n2)
REAL(DFP) :: L3(1:SIZE(z), 0:n3)
INTEGER(I4B) :: k1, cnt, k2, k3
L1 = LobattoEvalAll(n=n1, x=x)
L2 = LobattoEvalAll(n=n2, x=y)
L3 = LobattoEvalAll(n=n3, x=z)
cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    DO k3 = 2, n3
      cnt = cnt + 1
      ans(:, cnt) = L1(:, k1) * L2(:, k2) * L3(:, k3)
    END DO
  END DO
END DO
END PROCEDURE CellBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                                       CellBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Hexahedron2
INTEGER(I4B) :: k1, cnt, k2, k3
cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    DO k3 = 2, n3
      cnt = cnt + 1
      ans(:, cnt) = L1(:, k1) * L2(:, k2) * L3(:, k3)
    END DO
  END DO
END DO
END PROCEDURE CellBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                              CellBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasisGradient_Hexahedron2
INTEGER(I4B) :: k1, cnt, k2, k3
cnt = 0
DO k1 = 2, n1
  DO k2 = 2, n2
    DO k3 = 2, n3
      cnt = cnt + 1
      ans(:, cnt, 1) = dL1(:, k1) * L2(:, k2) * L3(:, k3)
      ans(:, cnt, 2) = L1(:, k1) * dL2(:, k2) * L3(:, k3)
      ans(:, cnt, 3) = L1(:, k1) * L2(:, k2) * dL3(:, k3)
    END DO
  END DO
END DO
END PROCEDURE CellBasisGradient_Hexahedron2

!----------------------------------------------------------------------------
!                                               HeirarchicalBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Hexahedron1

#define _maxP_ MAXVAL([pb1, px1, px2, px3, px4, pxy1, pxz1])
#define _maxQ_ MAXVAL([pb2, py1, py2, py3, py4, pxy2, pyz1])
#define _maxR_ MAXVAL([pb3, pz1, pz2, pz3, pz4, pxz2, pyz2])

INTEGER(I4B) :: a, b, maxP, maxQ, maxR
REAL(DFP) :: L1(1:SIZE(xij, 2), 0:_maxP_)
REAL(DFP) :: L2(1:SIZE(xij, 2), 0:_maxQ_)
REAL(DFP) :: L3(1:SIZE(xij, 2), 0:_maxR_)

#undef _maxP_
#undef _maxQ_
#undef _maxR_

maxP = SIZE(L1, 2) - 1
maxQ = SIZE(L2, 2) - 1
maxR = SIZE(L3, 2) - 1

L1 = LobattoEvalAll(n=maxP, x=xij(1, :))
L2 = LobattoEvalAll(n=maxQ, x=xij(2, :))
L3 = LobattoEvalAll(n=maxR, x=xij(3, :))

! Vertex basis function

ans(:, 1:8) = VertexBasis_Hexahedron2(L1=L1, L2=L2, L3=L3)

! Edge basis function

b = 8

IF (ANY([px1, px2, px3, px4] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + px1 + px2 + px3 + px4 - 4
  ans(:, a:b) = xEdgeBasis_Hexahedron2( &
    & pe1=px1, pe2=px2, pe3=px3, pe4=px4, L1=L1, L2=L2, L3=L3)
END IF

IF (ANY([py1, py2, py3, py4] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + py1 + py2 + py3 + py4 - 4
  ans(:, a:b) = yEdgeBasis_Hexahedron2( &
    & pe1=py1, pe2=py2, pe3=py3, pe4=py4, L1=L1, L2=L2, L3=L3)
END IF

IF (ANY([pz1, pz2, pz3, pz4] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + pz1 + pz2 + pz3 + pz4 - 4
  ans(:, a:b) = zEdgeBasis_Hexahedron2( &
    & pe1=pz1, pe2=pz2, pe3=pz3, pe4=pz4, L1=L1, L2=L2, L3=L3)
END IF

! Facet basis function

IF (ANY([pxy1, pxy2] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + 2 * (pxy1 - 1) * (pxy2 - 1)
  ans(:, a:b) = xyFacetBasis_Hexahedron2( &
    & n1=pxy1, n2=pxy2, L1=L1, L2=L2, L3=L3)
END IF

IF (ANY([pxz1, pxz2] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + 2 * (pxz1 - 1) * (pxz2 - 1)
  ans(:, a:b) = xzFacetBasis_Hexahedron2( &
    & n1=pxz1, n2=pxz2, L1=L1, L2=L2, L3=L3)
END IF

IF (ANY([pyz1, pyz2] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + 2 * (pyz1 - 1) * (pyz2 - 1)
  ans(:, a:b) = yzFacetBasis_Hexahedron2( &
    & n1=pyz1, n2=pyz2, L1=L1, L2=L2, L3=L3)
END IF

IF (ANY([pb1, pb2, pb3] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + (pb1 - 1) * (pb2 - 1) * (pb3 - 1)
  ans(:, a:b) = cellBasis_Hexahedron2( &
    & n1=pb1, n2=pb2, n3=pb3, L1=L1, L2=L2, L3=L3)
END IF
END PROCEDURE HeirarchicalBasis_Hexahedron1

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Hexahedron2
ans = HeirarchicalBasis_Hexahedron1(&
  & pb1=p, pb2=q, pb3=r, &
  & pxy1=p, pxy2=q, &
  & pxz1=p, pxz2=r, &
  & pyz1=q, pyz2=r, &
  & px1=p, px2=p, px3=p, px4=p, &
  & py1=q, py2=q, py3=q, py4=q, &
  & pz1=r, pz2=r, pz3=r, pz4=r, &
  & xij=xij)
END PROCEDURE HeirarchicalBasis_Hexahedron2

!----------------------------------------------------------------------------
!                                              QuadraturePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Hexahedron1
ans = QuadraturePoint_Hexahedron2( &
  & p=order, &
  & q=order, &
  & r=order, &
  & quadType1=quadType, &
  & quadType2=quadType, &
  & quadType3=quadType, &
  & refHexahedron=refHexahedron, &
  & xij=xij, &
  & alpha1=alpha, &
  & beta1=beta, &
  & lambda1=lambda, &
  & alpha2=alpha, &
  & beta2=beta, &
  & lambda2=lambda, &
  & alpha3=alpha, &
  & beta3=beta, &
  & lambda3=lambda &
  & )
END PROCEDURE QuadraturePoint_Hexahedron1

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Hexahedron2
! internal variables
REAL(DFP), ALLOCATABLE :: x(:, :), y(:, :), z(:, :), temp(:, :)
INTEGER(I4B) :: ii, jj, kk, nsd, np, nq, nr, cnt
TYPE(String) :: astr

astr = UpperCase(refHexahedron)

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

z = QuadraturePoint_Line( &
  & order=r,  &
  & quadType=quadType2, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha3, &
  & beta=beta3, &
  & lambda=lambda3)
nr = SIZE(z, 2)

nsd = 3
CALL Reallocate(ans, 4_I4B, np * nq * nr)
CALL Reallocate(temp, 4_I4B, np * nq * nr)

cnt = 0
DO ii = 1, np
  DO jj = 1, nq
    DO kk = 1, nr
      cnt = cnt + 1
      temp(1, cnt) = x(1, ii)
      temp(2, cnt) = y(1, jj)
      temp(3, cnt) = z(1, kk)
      temp(4, cnt) = x(2, ii) * y(2, jj) * z(2, kk)
    END DO
  END DO
END DO

IF (PRESENT(xij)) THEN
  ans(1:nsd, :) = FromBiUnitHexahedron2Hexahedron( &
    & xin=temp(1:3, :), &
    & x1=xij(:, 1), &
    & x2=xij(:, 2), &
    & x3=xij(:, 3), &
    & x4=xij(:, 4), &
    & x5=xij(:, 5), &
    & x6=xij(:, 6), &
    & x7=xij(:, 7), &
    & x8=xij(:, 8)  &
    & )
  ans(4, :) = temp(4, :) * JacobianHexahedron( &
    & from="BIUNIT", to="HEXAHEDRON", xij=xij)

ELSE
  IF (astr%chars() .EQ. "UNIT") THEN
    ans(1:nsd, :) = FromBiUnitHexahedron2UnitHexahedron( &
      & xin=temp(1:3, :))
    ans(4, :) = temp(4, :) * JacobianHexahedron( &
      & from="BIUNIT", to="UNIT", xij=xij)
  ELSE
    ans = temp
  END IF
END IF

IF (ALLOCATED(temp)) DEALLOCATE (temp)
IF (ALLOCATED(x)) DEALLOCATE (x)
IF (ALLOCATED(y)) DEALLOCATE (y)
IF (ALLOCATED(z)) DEALLOCATE (z)

END PROCEDURE QuadraturePoint_Hexahedron2

!----------------------------------------------------------------------------
!                                                QuadraturePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Hexahedron3
ans = QuadraturePoint_Hexahedron4( &
  & nipsx=nips, &
  & nipsy=nips, &
  & nipsz=nips, &
  & quadType1=quadType, &
  & quadType2=quadType, &
  & quadType3=quadType, &
  & refHexahedron=refHexahedron, &
  & xij=xij, &
  & alpha1=alpha, &
  & beta1=beta, &
  & lambda1=lambda, &
  & alpha2=alpha, &
  & beta2=beta, &
  & lambda2=lambda, &
  & alpha3=alpha, &
  & beta3=beta, &
  & lambda3=lambda &
  & )
END PROCEDURE QuadraturePoint_Hexahedron3

!----------------------------------------------------------------------------
!                                                QuadraturePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Hexahedron4
! internal variables
REAL(DFP) :: x(2, nipsx(1)), y(2, nipsy(1)), z(2, nipsz(1)), &
& temp(4, nipsy(1) * nipsx(1) * nipsz(1))
INTEGER(I4B) :: ii, jj, kk, nsd, np, nq, nr, cnt
TYPE(String) :: astr

astr = UpperCase(refHexahedron)

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

z = QuadraturePoint_Line( &
  & nips=nipsz,  &
  & quadType=quadType3, &
  & xij=[-1.0_DFP, 1.0_DFP], &
  & layout="INCREASING", &
  & alpha=alpha3, &
  & beta=beta3, &
  & lambda=lambda3)
nr = SIZE(z, 2)

nsd = 3
CALL Reallocate(ans, 4_I4B, np * nq * nr)

cnt = 0
DO ii = 1, np
  DO jj = 1, nq
    DO kk = 1, nr
      cnt = cnt + 1
      temp(1, cnt) = x(1, ii)
      temp(2, cnt) = y(1, jj)
      temp(3, cnt) = z(1, kk)
      temp(4, cnt) = x(2, ii) * y(2, jj) * z(2, kk)
    END DO
  END DO
END DO

IF (PRESENT(xij)) THEN
  ans(1:nsd, :) = FromBiUnitHexahedron2Hexahedron( &
    & xin=temp(1:3, :), &
    & x1=xij(:, 1), &
    & x2=xij(:, 2), &
    & x3=xij(:, 3), &
    & x4=xij(:, 4), &
    & x5=xij(:, 5), &
    & x6=xij(:, 6), &
    & x7=xij(:, 7), &
    & x8=xij(:, 8)  &
    & )
  ans(4, :) = temp(4, :) * JacobianHexahedron( &
    & from="BIUNIT", to="HEXAHEDRON", xij=xij)

ELSE
  IF (astr%chars() .EQ. "UNIT") THEN
    ans(1:nsd, :) = FromBiUnitHexahedron2UnitHexahedron( &
      & xin=temp(1:3, :))
    ans(4, :) = temp(4, :) * JacobianHexahedron( &
      & from="BIUNIT", to="UNIT", xij=xij)
  ELSE
    ans = temp
  END IF
END IF

END PROCEDURE QuadraturePoint_Hexahedron4

!----------------------------------------------------------------------------
!                                               LagrangeEvallAll_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Hexahedron1
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof
INTEGER(I4B) :: degree(SIZE(xij, 2), 3)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2))

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Hexahedron(&
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
  coeff0 = TRANSPOSE( &
    & LagrangeCoeff_Hexahedron(&
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

  degree = LagrangeDegree_Hexahedron(order=order)
  tdof = SIZE(xij, 2)

  IF (tdof .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(&
      & msg="tdof is not same as size(degree,1)", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Hexahedron1", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF

  DO ii = 1, tdof
    xx(1, ii) = x(1)**degree(ii, 1) &
               & * x(2)**degree(ii, 2) &
               & * x(3)**degree(ii, 3)
  END DO

CASE (Heirarchical)

  xx = HeirarchicalBasis_Hexahedron( &
    & p=order, &
    & q=order,  &
    & r=order,  &
    & xij=RESHAPE(x, [3, 1]))

CASE DEFAULT

  xx = TensorProdBasis_Hexahedron( &
    & p=order, &
    & q=order, &
    & r=order, &
    & xij=RESHAPE(x, [3, 1]),  &
    & basisType1=basisType0, &
    & basisType2=basisType0, &
    & basisType3=basisType0, &
    & alpha1=alpha, &
    & beta1=beta, &
    & lambda1=lambda, &
    & alpha2=alpha, &
    & beta2=beta, &
    & lambda2=lambda,  &
    & alpha3=alpha, &
    & beta3=beta, &
    & lambda3=lambda &
    & )

END SELECT

ans = MATMUL(coeff0, xx(1, :))

END PROCEDURE LagrangeEvalAll_Hexahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Hexahedron1_
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, indx(7)
INTEGER(I4B) :: degree(SIZE(xij, 2), 3)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2)), &
             x31(3, 1)

tsize = SIZE(xij, 2)

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN

    CALL LagrangeCoeff_Hexahedron_(order=order, xij=xij, &
                basisType=basisType0, alpha=alpha, beta=beta, lambda=lambda, &
                                   ans=coeff, nrow=indx(1), ncol=indx(2))

  END IF

  coeff0(1:tsize, 1:tsize) = coeff(1:tsize, 1:tsize)

ELSE

  ! coeff0 = LagrangeCoeff_Hexahedron(&
  CALL LagrangeCoeff_Hexahedron_(order=order, xij=xij, ans=coeff0, &
              nrow=indx(1), ncol=indx(2), basisType=basisType0, alpha=alpha, &
                                 beta=beta, lambda=lambda)
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Hexahedron(order=order)

#ifdef DEBUG_VER

  IF (tsize .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(msg="tdof is not same as size(degree,1)", &
                  routine="LagrangeEvalAll_Hexahedron1", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

#endif

  DO ii = 1, tsize
    indx(1:3) = degree(ii, 1:3)
    xx(1, ii) = x(1)**indx(1) * x(2)**indx(2) * x(3)**indx(3)
  END DO

CASE (Heirarchical)

  x31(1:3, 1) = x(1:3)
  xx = HeirarchicalBasis_Hexahedron(p=order, q=order, r=order, xij=x31)

CASE DEFAULT

  x31(1:3, 1) = x(1:3)

  xx = TensorProdBasis_Hexahedron(p=order, q=order, r=order, xij=x31, &
        basisType1=basisType0, basisType2=basisType0, basisType3=basisType0, &
         alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
                     lambda2=lambda, alpha3=alpha, beta3=beta, lambda3=lambda)

END SELECT

DO ii = 1, tsize
  ans(ii) = DOT_PRODUCT(coeff0(:, ii), xx(1, :))
END DO

END PROCEDURE LagrangeEvalAll_Hexahedron1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Hexahedron2
INTEGER(I4B) :: nrow, ncol
CALL LagrangeEvalAll_Hexahedron2_(order=order, x=x, xij=xij, ans=ans, &
                     nrow=nrow, ncol=ncol, coeff=coeff, firstCall=firstCall, &
                   basisType=basisType, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE LagrangeEvalAll_Hexahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Hexahedron2_
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, jj, basisType0, indx(3), degree(SIZE(xij, 2), 3)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), &
             xx(SIZE(x, 2), SIZE(xij, 2)), areal

nrow = SIZE(x, 2)
ncol = SIZE(xij, 2)

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    ! coeff = LagrangeCoeff_Hexahedron(&
    CALL LagrangeCoeff_Hexahedron_(order=order, xij=xij, &
     basisType=basisType0, alpha=alpha, beta=beta, lambda=lambda, ans=coeff, &
                                   nrow=indx(1), ncol=indx(2))

  END IF

  coeff0(1:ncol, 1:ncol) = coeff(1:ncol, 1:ncol)

ELSE

  ! coeff0 = LagrangeCoeff_Hexahedron(&
  CALL LagrangeCoeff_Hexahedron_(order=order, xij=xij, &
    basisType=basisType0, alpha=alpha, beta=beta, lambda=lambda, ans=coeff0, &
                                 nrow=indx(1), ncol=indx(2))

END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Hexahedron(order=order)

#ifdef DEBUG_VER
  IF (ncol .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(msg="ncol is not same as size(degree,1)", &
                  routine="LagrangeEvalAll_Hexahedron1", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF
#endif

  DO ii = 1, ncol

    indx(1:3) = degree(ii, 1:3)

    DO jj = 1, nrow
      areal = x(1, jj)**indx(1) * x(2, jj)**indx(2) * x(3, jj)**indx(3)
      xx(jj, ii) = areal
    END DO

  END DO

CASE (Heirarchical)

  xx = HeirarchicalBasis_Hexahedron(p=order, q=order, r=order, xij=x)

CASE DEFAULT

  xx = TensorProdBasis_Hexahedron(p=order, q=order, r=order, xij=x, &
        basisType1=basisType0, basisType2=basisType0, basisType3=basisType0, &
         alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
                     lambda2=lambda, alpha3=alpha, beta3=beta, lambda3=lambda)

END SELECT

! ans = MATMUL(xx, coeff0)
CALL GEMM(C=ans(1:nrow, 1:ncol), alpha=1.0_DFP, A=xx, B=coeff0)

END PROCEDURE LagrangeEvalAll_Hexahedron2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Hexahedron1
INTEGER(I4B) :: dim1, dim2, dim3
CALL LagrangeGradientEvalAll_Hexahedron1_(order=order, x=x, xij=xij, &
                      ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, coeff=coeff, &
           firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
                                          lambda=lambda)
END PROCEDURE LagrangeGradientEvalAll_Hexahedron1

!----------------------------------------------------------------------------
!                                       LagrangeGradientEvalAll_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Hexahedron1_
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, ai, bi, ci,d1, d2, d3,  degree(SIZE(xij, 2), 3), indx(3)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), &
             xx(SIZE(x, 2), SIZE(xij, 2), 3), ar, br, cr

dim1 = SIZE(x, 2)
dim2 = SIZE(xij, 2)
dim3 = 3

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN

  IF (firstCall0) THEN
    ! coeff = LagrangeCoeff_Hexahedron(&
    CALL LagrangeCoeff_Hexahedron_(order=order, xij=xij, &
                basisType=basisType0, alpha=alpha, beta=beta, lambda=lambda, &
                                   ans=coeff, nrow=indx(1), ncol=indx(2))
  END IF
  coeff0(1:dim2, 1:dim2) = coeff(1:dim2, 1:dim2)

ELSE

  CALL LagrangeCoeff_Hexahedron_(order=order, xij=xij, basisType=basisType0, &
            alpha=alpha, beta=beta, lambda=lambda, ans=coeff0, nrow=indx(1), &
                                 ncol=indx(2))

END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Hexahedron(order=order)

#ifdef DEBUG_VER

  IF (dim2 .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(msg="tdof is not same as size(degree,1)", &
                  routine="LagrangeEvalAll_Hexahedron1", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

#endif

  DO ii = 1, dim2
    d1 = degree(ii, 1)
    d2 = degree(ii, 2)
    d3 = degree(ii, 3)

    ai = MAX(d1 - 1_I4B, 0_I4B)
    bi = MAX(d2 - 1_I4B, 0_I4B)
    ci = MAX(d3 - 1_I4B, 0_I4B)

    ar = REAL(d1, DFP)
    br = REAL(d2, DFP)
    cr = REAL(d3, DFP)

    xx(:, ii, 1) = (ar * x(1, :)**ai) *  &
                & x(2, :)**d2 *  &
                & x(3, :)**d3

    xx(:, ii, 2) = x(1, :)**d1 *  &
                & (br * x(2, :)**bi) *  &
                & x(3, :)**d3

    xx(:, ii, 3) = x(1, :)**d1 *  &
                & x(2, :)**d2 * &
                & (cr * x(3, :)**ci)

  END DO

CASE (Heirarchical)
  xx = HeirarchicalBasisGradient_Hexahedron( &
    & p=order, &
    & q=order,  &
    & r=order,  &
    & xij=x)

CASE DEFAULT
  xx = OrthogonalBasisGradient_Hexahedron( &
    & p=order, &
    & q=order, &
    & r=order, &
    & xij=x,  &
    & basisType1=basisType0, &
    & basisType2=basisType0, &
    & basisType3=basisType0, &
    & alpha1=alpha, &
    & beta1=beta, &
    & lambda1=lambda, &
    & alpha2=alpha, &
    & beta2=beta, &
    & lambda2=lambda,  &
    & alpha3=alpha, &
    & beta3=beta, &
    & lambda3=lambda)

END SELECT

DO ii = 1, 3
  ! ans(:, ii, :) = TRANSPOSE(MATMUL(xx(:, :, ii), coeff0))
  ans(1:dim1, 1:dim2, ii) = MATMUL(xx(:, :, ii), coeff0)
END DO

END PROCEDURE LagrangeGradientEvalAll_Hexahedron1_

!----------------------------------------------------------------------------
!                                         TensorProdBasisGradient_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasisGradient_Hexahedron1
REAL(DFP) :: x(SIZE(xij, 2)), y(SIZE(xij, 2)), z(SIZE(xij, 2))
REAL(DFP) :: P1(SIZE(xij, 2), p + 1)
REAL(DFP) :: Q1(SIZE(xij, 2), q + 1)
REAL(DFP) :: R1(SIZE(xij, 2), r + 1)
REAL(DFP) :: dP1(SIZE(xij, 2), p + 1)
REAL(DFP) :: dQ1(SIZE(xij, 2), q + 1)
REAL(DFP) :: dR1(SIZE(xij, 2), r + 1)

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

dP1 = BasisGradientEvalAll_Line( &
  & order=p, &
  & x=x, &
  & refLine="BIUNIT", &
  & basisType=basisType1,  &
  & alpha=alpha1, &
  & beta=beta1, &
  & lambda=lambda1)

dQ1 = BasisGradientEvalAll_Line( &
  & order=q, &
  & x=y, &
  & refLine="BIUNIT", &
  & basisType=basisType1,  &
  & alpha=alpha2, &
  & beta=beta2, &
  & lambda=lambda2)

dR1 = BasisGradientEvalAll_Line( &
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
      ans(:, cnt, 1) = dP1(:, k1) * Q1(:, k2) * R1(:, k3)
      ans(:, cnt, 2) = P1(:, k1) * dQ1(:, k2) * R1(:, k3)
      ans(:, cnt, 3) = P1(:, k1) * Q1(:, k2) * dR1(:, k3)
    END DO
  END DO
END DO
END PROCEDURE TensorProdBasisGradient_Hexahedron1

!----------------------------------------------------------------------------
!                                    HeirarchicalBasisGradient_Hexahedron1
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Hexahedron1
#define _maxP_ MAXVAL([pb1, px1, px2, px3, px4, pxy1, pxz1])
#define _maxQ_ MAXVAL([pb2, py1, py2, py3, py4, pxy2, pyz1])
#define _maxR_ MAXVAL([pb3, pz1, pz2, pz3, pz4, pxz2, pyz2])

INTEGER(I4B) :: a, b, maxP, maxQ, maxR
REAL(DFP) :: L1(1:SIZE(xij, 2), 0:_maxP_)
REAL(DFP) :: L2(1:SIZE(xij, 2), 0:_maxQ_)
REAL(DFP) :: L3(1:SIZE(xij, 2), 0:_maxR_)
REAL(DFP) :: dL1(1:SIZE(xij, 2), 0:_maxP_)
REAL(DFP) :: dL2(1:SIZE(xij, 2), 0:_maxQ_)
REAL(DFP) :: dL3(1:SIZE(xij, 2), 0:_maxR_)

#undef _maxP_
#undef _maxQ_
#undef _maxR_

maxP = SIZE(L1, 2) - 1
maxQ = SIZE(L2, 2) - 1
maxR = SIZE(L3, 2) - 1

L1 = LobattoEvalAll(n=maxP, x=xij(1, :))
L2 = LobattoEvalAll(n=maxQ, x=xij(2, :))
L3 = LobattoEvalAll(n=maxR, x=xij(3, :))

dL1 = LobattoGradientEvalAll(n=maxP, x=xij(1, :))
dL2 = LobattoGradientEvalAll(n=maxQ, x=xij(2, :))
dL3 = LobattoGradientEvalAll(n=maxR, x=xij(3, :))

! Vertex basis function
ans(:, 1:8, :) = VertexBasisGradient_Hexahedron2( &
  & L1=L1, &
  & L2=L2, &
  & L3=L3, &
  & dL1=dL1, &
  & dL2=dL2, &
  & dL3=dL3  &
  & )

! Edge basis function
b = 8

IF (ANY([px1, px2, px3, px4] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + px1 + px2 + px3 + px4 - 4
  ans(:, a:b, :) = xEdgeBasisGradient_Hexahedron2( &
    & pe1=px1, &
    & pe2=px2, &
    & pe3=px3, &
    & pe4=px4, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
END IF

IF (ANY([py1, py2, py3, py4] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + py1 + py2 + py3 + py4 - 4
  ans(:, a:b, :) = yEdgeBasisGradient_Hexahedron2( &
    & pe1=py1, &
    & pe2=py2, &
    & pe3=py3, &
    & pe4=py4, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
END IF

IF (ANY([pz1, pz2, pz3, pz4] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + pz1 + pz2 + pz3 + pz4 - 4
  ans(:, a:b, :) = zEdgeBasisGradient_Hexahedron2( &
    & pe1=pz1, &
    & pe2=pz2, &
    & pe3=pz3, &
    & pe4=pz4, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
END IF

! Facet basis function

IF (ANY([pxy1, pxy2] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + 2 * (pxy1 - 1) * (pxy2 - 1)
  ans(:, a:b, :) = xyFacetBasisGradient_Hexahedron2( &
    & n1=pxy1, &
    & n2=pxy2, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
END IF

IF (ANY([pxz1, pxz2] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + 2 * (pxz1 - 1) * (pxz2 - 1)
  ans(:, a:b, :) = xzFacetBasisGradient_Hexahedron2( &
    & n1=pxz1, &
    & n2=pxz2, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
END IF

IF (ANY([pyz1, pyz2] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + 2 * (pyz1 - 1) * (pyz2 - 1)
  ans(:, a:b, :) = yzFacetBasisGradient_Hexahedron2( &
    & n1=pyz1, &
    & n2=pyz2, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
END IF

IF (ANY([pb1, pb2, pb3] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + (pb1 - 1) * (pb2 - 1) * (pb3 - 1)
  ans(:, a:b, :) = cellBasisGradient_Hexahedron2( &
    & n1=pb1, &
    & n2=pb2, &
    & n3=pb3, &
    & L1=L1, &
    & L2=L2, &
    & L3=L3, &
    & dL1=dL1, &
    & dL2=dL2, &
    & dL3=dL3 &
    & )
END IF
END PROCEDURE HeirarchicalBasisGradient_Hexahedron1

!----------------------------------------------------------------------------
!                                     HeirarchicalBasisGradient_Hexahedron2
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Hexahedron2
ans = HeirarchicalBasisGradient_Hexahedron1(&
  & pb1=p, pb2=q, pb3=r, &
  & pxy1=p, pxy2=q, &
  & pxz1=p, pxz2=r, &
  & pyz1=q, pyz2=r, &
  & px1=p, px2=p, px3=p, px4=p, &
  & py1=q, py2=q, py3=q, py4=q, &
  & pz1=r, pz2=r, pz3=r, pz4=r, &
  & xij=xij)
END PROCEDURE HeirarchicalBasisGradient_Hexahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Hexahedron1_
CALL ErrorMsg(&
  & msg="InterpolationPoint_Hexahedron1_ is not implemented", &
  & file=__FILE__, &
  & routine="InterpolationPoint_Hexahedron1_", &
  & line=__LINE__, &
  & unitno=stderr)
! STOP
END PROCEDURE InterpolationPoint_Hexahedron1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Hexahedron2_
CALL ErrorMsg(&
  & msg="InterpolationPoint_Hexahedron2_ is not implemented", &
  & file=__FILE__, &
  & routine="InterpolationPoint_Hexahedron2_", &
  & line=__LINE__, &
  & unitno=stderr)
STOP
END PROCEDURE InterpolationPoint_Hexahedron2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
