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
! summary:         This submodule contians methods for [[ReferencePyramid_]]

SUBMODULE(ReferencePyramid_Method) Methods
USE ArangeUtility
USE ApproxUtility
USE StringUtility
USE ReferenceElement_Method
USE ReferencePrism_Method, ONLY: PolyhedronVolume3D

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       ElementName_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementName_Pyramid
SELECT CASE (elemType)
CASE (Pyramid5)
  ans = "Pyramid5"

CASE (Pyramid13)
  ans = "Pyramid13"

CASE (Pyramid14)
  ans = "Pyramid14"

CASE default
  ans = "NONE"
END SELECT
END PROCEDURE ElementName_Pyramid

!----------------------------------------------------------------------------
!                                                     FaceTopology_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetTopology_Pyramid
! TODO:
! ALLOCATE (ans(5))
! ans(1)%nptrs = nptrs([1, 2, 5])
! ans(2)%nptrs = nptrs([2, 3, 5])
! ans(3)%nptrs = nptrs([3, 4, 5])
! ans(4)%nptrs = nptrs([1, 5, 4])
! ans(5)%nptrs = nptrs([4, 3, 2, 1])
! ans(:)%xiDimension = 2
! ans(1:4)%name = Triangle3
! ans(5)%name = Quadrangle4
! Order=2 elements

! CASE (Pyramid13)
!   ! ALLOCATE (ans(5))
!   ans(1)%nptrs = nptrs([1, 2, 5, 6, 10, 8])
!   ans(2)%nptrs = nptrs([2, 3, 5, 9, 12, 10])
!   ans(3)%nptrs = nptrs([3, 4, 5, 11, 13, 12])
!   ans(4)%nptrs = nptrs([1, 5, 4, 8, 13, 7])
!   ans(5)%nptrs = nptrs([4, 3, 2, 1, 11, 9, 6, 7])
!   ans(:)%xiDimension = 2
!   ans(1:4)%name = Triangle6
!   ans(5)%name = Quadrangle8
!
! CASE (Pyramid14)
!   ! ALLOCATE (ans(5))
!   ans(1)%nptrs = nptrs([1, 2, 5, 6, 10, 8])
!   ans(2)%nptrs = nptrs([2, 3, 5, 9, 12, 10])
!   ans(3)%nptrs = nptrs([3, 4, 5, 11, 13, 12])
!   ans(4)%nptrs = nptrs([1, 5, 4, 8, 13, 7])
!   ans(5)%nptrs = nptrs([4, 3, 2, 1, 11, 9, 6, 7, 13])
!   ans(:)%xiDimension = 2
!   ans(1:4)%name = Triangle6
!   ans(5)%name = Quadrangle9
END PROCEDURE FacetTopology_Pyramid

!----------------------------------------------------------------------------
!                                                     TotalEntities_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalEntities_Pyramid
ans(2:4) = [9, 5, 1]
ans(1) = TotalNodesInElement_Pyramid(elemType)
END PROCEDURE TotalEntities_Pyramid

!----------------------------------------------------------------------------
!                                               TotalNodesInElements_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE TotalNodesInElement_Pyramid
SELECT CASE (elemType)
CASE (Pyramid5)
  ans = 5

CASE (Pyramid13)
  ans = 13

CASE (Pyramid14)
  ans = 14

CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE TotalNodesInElement_Pyramid

!----------------------------------------------------------------------------
!                                                        ElementType_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementType_Pyramid
SELECT CASE (elemName)
CASE ("Pyramid", "Pyramid5")
  ans = Pyramid5
CASE ("Pyramid13")
  ans = Pyramid13
CASE ("Pyramid14")
  ans = Pyramid14
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementType_Pyramid

!----------------------------------------------------------------------------
!                                                       ElementOrder_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE ElementOrder_Pyramid
SELECT CASE (elemType)
CASE (Pyramid5)
  ans = 1

CASE (Pyramid13)
  ans = 2

CASE (Pyramid14)
  ans = 2

CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE ElementOrder_Pyramid

!----------------------------------------------------------------------------
!                                                     FacetElements_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Pyramid1
! TODO:
END PROCEDURE FacetElements_Pyramid1

!----------------------------------------------------------------------------
!                                                     FacetElements_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetElements_Pyramid2
! TODO:
END PROCEDURE FacetElements_Pyramid2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate_Ref_Pyramid
INTEGER(I4B) :: ii, jj
INTEGER(I4B), PARAMETER :: tNodes = 5, tFaces = 5, tEdges = 8, xidim = 3, &
  & max_nodes_face = 4, name = Pyramid
INTEGER(I4B) :: p1p2(2, tEdges), lloop(max_nodes_face + 2, tFaces), &
  & vol(tNodes, 1)
REAL(DFP) :: unit_xij(xidim, tNodes), biunit_xij(xidim, tNodes)

CALL DEALLOCATE (obj)

CALL GetEdgeConnectivity_Pyramid(con=p1p2, opt=1_I4B, order=1_I4B)
CALL GetFaceConnectivity_Pyramid(con=lloop, opt=1_I4B, order=1_I4B)

vol(:, 1) = arange(1_I4B, tNodes)

unit_xij = RefCoord_Pyramid("UNIT")
biunit_xij = RefCoord_Pyramid("BIUNIT")

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
      obj%xij = RefCoord_Pyramid(obj%domainName)
    END IF
  ELSE
    obj%domainName = "UNIT"
    obj%xij = RefCoord_Pyramid(obj%domainName)
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

obj%highorderElement => highorderElement_Pyramid
END PROCEDURE Initiate_Ref_Pyramid

!----------------------------------------------------------------------------
!                                                      ReferencePyramid
!----------------------------------------------------------------------------
MODULE PROCEDURE Reference_Pyramid
CALL Initiate_Ref_Pyramid(obj=obj, nsd=NSD, xij=xij, domainName=domainName)
END PROCEDURE Reference_Pyramid

!----------------------------------------------------------------------------
!                                                      ReferencePyramid
!----------------------------------------------------------------------------
MODULE PROCEDURE Reference_Pyramid_Pointer
ALLOCATE (obj)
CALL Initiate_Ref_Pyramid(obj=obj, nsd=NSD, xij=xij, domainName=domainName)
END PROCEDURE Reference_Pyramid_Pointer

!----------------------------------------------------------------------------
!                                                             LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE HighOrderElement_Pyramid
! FIX:
!TODO:
END PROCEDURE HighOrderElement_Pyramid

!-----------------------------------------------------------------------------
!                                                              MeasureSimplex
!-----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Pyramid
INTEGER(I4B) :: FM(5, 7), Node0(5, 4), Order0(5), iFace, b

FM = FacetMatrix(RefElem)

DO iFace = 1, 5
  Order0(iFace) = FM(iFace, 3)
  b = Order0(iFace) + 3
  Node0(iFace, 1:Order0(iFace)) = FM(iFace, 4:b)
END DO

CALL PolyhedronVolume3D(coord=XiJ(1:3, 1:5), &
  & order_max=4, face_num=5,  &
  & node=Node0, node_num=5, &
  & order=Order0, &
  & ans=ans)

END PROCEDURE Measure_Simplex_Pyramid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Pyramid_Quality
ans = 0.0_DFP
!FIX: Implement Pyramid_Quality
!TODO:
END PROCEDURE Pyramid_Quality

!----------------------------------------------------------------------------
!                                                          Refcoord_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE RefCoord_Pyramid
!FIX: Implement RefCoord
ans = 0.0_DFP
!TODO:
END PROCEDURE RefCoord_Pyramid

!----------------------------------------------------------------------------
!                                               GetEdgeConnectivity_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeConnectivity_Pyramid
con(1:2, 1) = [1, 2]
con(1:2, 2) = [1, 4]
con(1:2, 3) = [1, 5]
con(1:2, 4) = [2, 3]
con(1:2, 5) = [2, 5]
con(1:2, 6) = [3, 4]
con(1:2, 7) = [3, 5]
con(1:2, 8) = [4, 5]

IF (PRESENT(nrow)) nrow = 2
IF (PRESENT(ncol)) ncol = 8
END PROCEDURE GetEdgeConnectivity_Pyramid

!----------------------------------------------------------------------------
!                                               GetFaceConnectivity_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceConnectivity_Pyramid
con(1:4, 1) = [1, 4, 3, 2]
con(1:4, 2) = [2, 3, 5, 0]
con(1:4, 3) = [3, 4, 5, 0]
con(1:4, 4) = [1, 5, 4, 0]
con(1:4, 5) = [1, 2, 5, 0]
IF (PRESENT(nrow)) nrow = 4
IF (PRESENT(ncol)) ncol = 5
END PROCEDURE GetFaceConnectivity_Pyramid

!----------------------------------------------------------------------------
!                                               GetFaceElemType_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFaceElemType_Pyramid

IF (PRESENT(faceElemType)) &
  faceElemType(1:5) = [Quadrangle4, Triangle3, Triangle3, Triangle3, &
                       Triangle3]

IF (PRESENT(tFaceNodes)) tFaceNodes(1:5) = [4, 3, 3, 3, 3]
END PROCEDURE GetFaceElemType_Pyramid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
