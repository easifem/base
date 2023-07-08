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
ans(:, 1) = [1, 4, 3, 2] !! back
ans(:, 2) = [5, 6, 7, 8] !! front
ans(:, 3) = [1, 5, 8, 4] !! left
ans(:, 4) = [2, 3, 7, 6] !! right
ans(:, 5) = [1, 2, 6, 5] !! top
ans(:, 6) = [3, 4, 8, 7] !! bottom

!! B, F, L, R, T, Bo
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

MODULE PROCEDURE LagrangeDegree_Hexahedron
INTEGER(I4B) :: n, ii, jj, kk, indx
n = LagrangeDOF_Quadrangle(order=order)
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
END PROCEDURE LagrangeDegree_Hexahedron

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Hexahedron
ans = (order + 1)**3
END PROCEDURE LagrangeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Hexahedron
ans = (order - 1)**3
END PROCEDURE LagrangeInDOF_Hexahedron

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron
INTEGER(I4B) :: nsd, n, ne, i1, i2, ii
REAL(DFP) :: x(3, 8), xin(3, 8), F(3, 3)
INTEGER(I4B), PARAMETER :: tEdges = 12, tFacets = 6
INTEGER(I4B) :: edgeConnectivity(2, tEdges), facetConnectivity(4, tFacets)

x = 0.0_DFP
xin = 0.0_DFP
nsd = 3_I4B

IF (PRESENT(xij)) THEN
  x(1:nsd, 1:8) = xij(1:nsd, 1:8)
ELSE
  x = RefHexahedronCoord("BIUNIT")
END IF

F = 0.0_DFP
F(1, 1) = NORM2(x(:, 1) - x(:, 2)) / REAL(order, DFP)
F(2, 2) = NORM2(x(:, 1) - x(:, 4)) / REAL(order, DFP)
F(3, 3) = NORM2(x(:, 1) - x(:, 5)) / REAL(order, DFP)

n = LagrangeDOF_Hexahedron(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP

! points on vertex
ans(1:nsd, 1:8) = x(1:nsd, 1:8)

IF (order .EQ. 1_I4B) RETURN

! points on edge
ne = LagrangeInDOF_Line(order=order)
edgeConnectivity = EdgeConnectivity_Hexahedron()
i2 = 8
DO ii = 1, tEdges
  i1 = i2 + 1
  i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, edgeConnectivity(:, ii)))
END DO

! points on facet
ne = LagrangeInDOF_Quadrangle(order=order)
facetConnectivity = FacetConnectivity_Hexahedron()
DO ii = 1, tFacets
  i1 = i2 + 1
  i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Quadrangle( &
    & order=order, &
    & xij=x(1:nsd, facetConnectivity(:, ii)))
END DO

! internal points
SELECT CASE (order)
CASE (2_I4B)
  i1 = i2 + 1
  i2 = i1
  ans(1:nsd, i1:i2) = 0.0_DFP
CASE (3_I4B)
  xin = MATMUL(F, x)
  ne = LagrangeInDOF_Hexahedron(order)
  i1 = i2 + 1
  i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = xin
CASE DEFAULT
  xin = MATMUL(F, x)
  ne = LagrangeInDOF_Hexahedron(order)
  i1 = i2 + 1
  i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistancePoint_Hexahedron( &
    & order=order - 2_I4B, xij=xin)
END SELECT

END PROCEDURE EquidistancePoint_Hexahedron

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Hexahedron
INTEGER(I4B) :: nsd, n, ne, i1, i2, ii
REAL(DFP) :: x(3, 8), xin(3, 8), F(3, 3)

x = 0.0_DFP
xin = 0.0_DFP
nsd = 3_I4B

IF (PRESENT(xij)) THEN
  x(1:nsd, 1:8) = xij(1:nsd, 1:8)
ELSE
  x = RefHexahedronCoord("BIUNIT")
END IF

F = 0.0_DFP
F(1, 1) = NORM2(x(:, 1) - x(:, 2)) / REAL(order, DFP)
F(2, 2) = NORM2(x(:, 1) - x(:, 4)) / REAL(order, DFP)
F(3, 3) = NORM2(x(:, 1) - x(:, 5)) / REAL(order, DFP)

n = LagrangeInDOF_Hexahedron(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP

IF (order .EQ. 1_I4B) RETURN

i2 = 0
! internal points
SELECT CASE (order)
CASE (2_I4B)
  ans(1:nsd, 1) = SUM(x, dim=2_I4B) / 8.0_DFP
CASE (3_I4B)
  xin = MATMUL(F, x)
  ne = LagrangeInDOF_Hexahedron(order)
  i1 = i2 + 1
  i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = xin
CASE DEFAULT
  xin = MATMUL(F, x)
  ne = LagrangeInDOF_Hexahedron(order)
  i1 = i2 + 1
  i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistancePoint_Hexahedron( &
    & order=order - 2_I4B, xij=xin)
END SELECT
END PROCEDURE EquidistanceInPoint_Hexahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Hexahedron
SELECT CASE (ipType)
CASE (Equidistance)
  nodecoord = EquidistancePoint_Hexahedron(xij=xij, order=order)
CASE (GaussLegendre)
CASE (GaussLegendreLobatto)
CASE (GaussChebyshev)
CASE (GaussChebyshevLobatto)
END SELECT
END PROCEDURE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info
!!
ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Hexahedron)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans, IPIV=ipiv, info=info)
!!
END PROCEDURE LagrangeCoeff_Hexahedron1

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron2
!!
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info
!!
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
ans = LagrangeVandermonde(order=order, xij=xij, elemType=Hexahedron)
CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Hexahedron4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
