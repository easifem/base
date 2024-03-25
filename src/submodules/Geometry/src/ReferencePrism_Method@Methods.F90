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

!> author: Vikas Sharma, Ph. D.
! date: 2 March 2021
! summary: This submodule defines methods for [[ReferencePrism_]]

SUBMODULE(ReferencePrism_Method) Methods
USE ArangeUtility
USE ApproxUtility
USE StringUtility
USE ReferenceElement_Method

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        ElementName_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementName_Prism
SELECT CASE (elemType)
CASE (Prism6)
  ans = "Prism6"

CASE (Prism15)
  ans = "Prism15"

CASE (Prism18)
  ans = "Prism18"

CASE DEFAULT
  ans = "NONE"

END SELECT
END PROCEDURE ElementName_Prism

!----------------------------------------------------------------------------
!                                                     FaceTopology_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetTopology_Prism
! TODO:
END PROCEDURE FacetTopology_Prism

!----------------------------------------------------------------------------
!                                                     TotalEntities_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalEntities_Prism
!TODO:
ans = 0
END PROCEDURE TotalEntities_Prism

!----------------------------------------------------------------------------
!                                                 TotalNodesInElements_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalNodesInElement_Prism
!TODO:
ans = 0
END PROCEDURE TotalNodesInElement_Prism

!----------------------------------------------------------------------------
!                                                         ElementType_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementType_Prism
!TODO:
ans = 0
END PROCEDURE ElementType_Prism

!----------------------------------------------------------------------------
!                                                        ElementOrder_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementOrder_Prism
!TODO:
ans = 0
END PROCEDURE ElementOrder_Prism

!----------------------------------------------------------------------------
!                                                     FacetElements_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Prism1
! TODO:
END PROCEDURE FacetElements_Prism1

!----------------------------------------------------------------------------
!                                                     FacetElements_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Prism2
! TODO:
END PROCEDURE FacetElements_Prism2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_Ref_Prism
INTEGER(I4B) :: ii, jj
INTEGER(I4B), PARAMETER :: tNodes = 6, tFaces = 5, tEdges = 9, xidim = 3, &
  & max_nodes_face = 4, name = Prism
INTEGER(I4B) :: p1p2(2, tEdges), lloop(max_nodes_face + 2, tFaces), &
  & vol(tNodes, 1)
REAL(DFP) :: unit_xij(xidim, tNodes), biunit_xij(xidim, tNodes)

CALL DEALLOCATE (obj)

CALL GetEdgeConnectivity_Prism(con=p1p2, opt=1_I4B, order=1_I4B)
CALL GetFaceConnectivity_Prism(con=lloop, opt=1_I4B, order=1_I4B)

vol(:, 1) = arange(1_I4B, tNodes)

unit_xij = RefCoord_Prism("UNIT")
biunit_xij = RefCoord_Prism("BIUNIT")

IF (PRESENT(xij)) THEN
  obj%xij = xij(:xidim, :tNodes)

  IF (ALL(obj%xij(:xidim, :tNodes) .approxeq.unit_xij)) THEN
    obj%domainName = "UNIT"
  ELSE IF (ALL(obj%xij(:xidim, :tNodes) .approxeq.biunit_xij)) THEN
    obj%domainName = "BIUNIT"
  ELSE
    obj%domainName = "GENERAL"
  END IF

ELSE

  IF (PRESENT(domainName)) THEN
    obj%domainName = UpperCase(domainName)
    IF (obj%domainName .EQ. "UNIT" .OR. obj%domainName .EQ. "BIUNIT") THEN
      obj%xij = RefCoord_Prism(obj%domainName)
    END IF
  ELSE
    obj%domainName = "UNIT"
    obj%xij = RefCoord_Prism(obj%domainName)
  END IF

END IF

obj%entityCounts = [tNodes, tEdges, tFaces, 1_I4B]
obj%xidimension = xidim
obj%name = name
obj%order = 1_I4B
obj%nsd = nsd

ALLOCATE (obj%topology(SUM(obj%entityCounts)))
DO ii = 1, obj%entityCounts(1)
  obj%topology(ii) = ReferenceTopology([ii], Point)
END DO

jj = obj%entityCounts(1)
DO ii = 1, obj%entityCounts(2)
  obj%topology(jj + ii) = ReferenceTopology(p1p2(:, ii), Line2)
END DO

jj = jj + obj%entityCounts(2)
DO ii = 1, obj%entityCounts(3)
  obj%topology(jj + ii) = ReferenceTopology(  &
    & lloop(2 + 1:2 + lloop(1, ii), ii), lloop(2, ii))
END DO

jj = jj + obj%entityCounts(3)
DO ii = 1, obj%entityCounts(4)
  obj%topology(jj + ii) = ReferenceTopology(vol(:, ii), name)
END DO

obj%highorderElement => highorderElement_Prism
END PROCEDURE Initiate_Ref_Prism

!----------------------------------------------------------------------------
!                                                            ReferencePrism
!----------------------------------------------------------------------------

MODULE PROCEDURE Reference_Prism
CALL Initiate_Ref_Prism(obj=obj, nsd=NSD, xij=xij, domainName=domainName)
END PROCEDURE Reference_Prism

!----------------------------------------------------------------------------
!                                                     ReferencePrism_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Reference_Prism_Pointer
ALLOCATE (obj)
CALL Initiate_Ref_Prism(obj=obj, nsd=NSD, xij=xij, domainName=domainName)
END PROCEDURE Reference_Prism_Pointer

!----------------------------------------------------------------------------
!                                                     HighOrderElement_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE HighOrderElement_Prism
! TODO:
! FIX: #250 Implement HighOrderElement_Prism
END PROCEDURE HighOrderElement_Prism

!----------------------------------------------------------------------------
!                                                              MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Prism
INTEGER(I4B) :: fm(5, 7), node0(5, 4), order0(5), b, iface

fm = FacetMatrix(refelem)
DO iface = 1, 5
  order0(iface) = fm(iface, 3)
  b = order0(iface) + 3
  node0(iface, 1:order0(iface)) = fm(iface, 4:b)
END DO
CALL POLYHEDRONVOLUME3D(coord=XiJ(1:3, 1:6), &
  & order_max=4, face_num=5,  &
  & node=node0, node_num=6, &
  & order=order0, &
  & ans=ans)
END PROCEDURE Measure_Simplex_Prism

!----------------------------------------------------------------------------
!                                                             Prism_Quality
!----------------------------------------------------------------------------

MODULE PROCEDURE Prism_Quality
! TODO:
ans = 0.0_DFP
END PROCEDURE Prism_Quality

!----------------------------------------------------------------------------
!                                                        PolyhedronVolume3D
!----------------------------------------------------------------------------

MODULE PROCEDURE PolyhedronVolume3D
INTEGER(I4B) :: iFace
INTEGER(I4B) :: n1
INTEGER(I4B) :: n2
INTEGER(I4B) :: n3
INTEGER(I4B) :: v

ans = 0.0_DFP
! Triangulate each iFace.
DO iface = 1, face_num
  n3 = node(iface, order(iface))
  DO v = 1, order(iface) - 2
    n1 = node(iface, v)
    n2 = node(iface, v + 1)
    ans = ans &
          + coord(1, n1) &
          * (coord(2, n2) * coord(3, n3) - coord(2, n3) * coord(3, n2)) &
          + coord(1, n2) &
          * (coord(2, n3) * coord(3, n1) - coord(2, n1) * coord(3, n3)) &
          + coord(1, n3) &
          * (coord(2, n1) * coord(3, n2) - coord(2, n2) * coord(3, n1))
  END DO
END DO
ans = ans / 6.0_DFP
END PROCEDURE PolyhedronVolume3D

!----------------------------------------------------------------------------
!                                                          Refcoord_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE RefCoord_Prism
ans = 0.0_DFP
!TODO:
!FIX: Implement RefCoord_Prism
!ISSUE: #251
END PROCEDURE RefCoord_Prism

!----------------------------------------------------------------------------
!                                                 GetEdgeConnectivity_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity_Prism
con(1:2, 1) = [1, 2]
con(1:2, 2) = [1, 3]
con(1:2, 3) = [1, 4]
con(1:2, 4) = [2, 3]
con(1:2, 5) = [2, 5]
con(1:2, 6) = [3, 6]
con(1:2, 7) = [4, 5]
con(1:2, 8) = [4, 6]
con(1:2, 9) = [5, 6]
END PROCEDURE GetEdgeConnectivity_Prism

!----------------------------------------------------------------------------
!                                                 GetFaceConnectivity_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity_Prism
con(1:4, 1) = [1, 3, 2, 0]
con(1:4, 2) = [2, 3, 6, 5]
con(1:4, 3) = [1, 2, 5, 4]
con(1:4, 4) = [1, 4, 6, 3]
con(1:4, 5) = [4, 5, 6, 0]
END PROCEDURE GetFaceConnectivity_Prism

!----------------------------------------------------------------------------
!                                                      GetFaceElemType_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Prism
faceElemType(1:5) = [Triangle3, Quadrangle4, Quadrangle4, Quadrangle4,  &
  & Triangle3]
IF (PRESENT(tFaceNodes)) tFaceNodes(1:5) = [3, 4, 4, 4, 3]
END PROCEDURE GetFaceElemType_Prism

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
