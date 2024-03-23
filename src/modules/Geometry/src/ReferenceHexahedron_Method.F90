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
! date:         5 March 2021
! summary: This module contains methods for [[ReferenceHexahedron_]]

MODULE ReferenceHexahedron_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate
PUBLIC :: ReferenceHexahedron
PUBLIC :: ReferenceHexahedron_Pointer
PUBLIC :: HighorderElement_Hexahedron
PUBLIC :: Measure_Simplex_Hexahedron
PUBLIC :: Hexahedron_Quality
PUBLIC :: Quality_Hexahedron
PUBLIC :: HexahedronVolume3D
PUBLIC :: GetEdgeConnectivity_Hexahedron
PUBLIC :: GetFaceConnectivity_Hexahedron
PUBLIC :: RefCoord_Hexahedron
PUBLIC :: RefHexahedronCoord
PUBLIC :: GetFaceElemType_Hexahedron
PUBLIC :: FacetElements_Hexahedron

PUBLIC :: ElementOrder_Hexahedron
PUBLIC :: ElementType_Hexahedron
PUBLIC :: TotalNodesInElement_Hexahedron
PUBLIC :: TotalEntities_Hexahedron
! PUBLIC :: FacetTopology_Hexahedron

!----------------------------------------------------------------------------
!                                                   TotalEntities_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total entities in Hexahedron

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Hexahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                            TotalNodesInElement_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total nodes in element

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Hexahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the type of element from char name

INTERFACE
  MODULE PURE FUNCTION ElementType_Hexahedron(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ElementOrder_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the order of element

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Hexahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElements_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Hexahedron
  MODULE SUBROUTINE FacetElements_Hexahedron1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Hexahedron1
END INTERFACE FacetElements_Hexahedron

!----------------------------------------------------------------------------
!                                                  FacetElements_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Hexahedron
  MODULE SUBROUTINE FacetElements_Hexahedron2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Hexahedron2
END INTERFACE FacetElements_Hexahedron

!----------------------------------------------------------------------------
!                                                       Initiate@Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary: This subroutine for constructing the object

INTERFACE Initiate
  MODULE PURE SUBROUTINE initiate_ref_Hexahedron(obj, nsd, xij, domainName)
    CLASS(ReferenceHexahedron_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE initiate_ref_Hexahedron
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceHexahedron@Hexahedron
!----------------------------------------------------------------------------

INTERFACE ReferenceHexahedron
  MODULE PURE FUNCTION reference_Hexahedron(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferenceHexahedron_) :: obj
  END FUNCTION reference_Hexahedron
END INTERFACE ReferenceHexahedron

!----------------------------------------------------------------------------
!                                   ReferenceHexahedron_Pointer@Hexahedron
!----------------------------------------------------------------------------

INTERFACE ReferenceHexahedron_Pointer
  MODULE FUNCTION reference_Hexahedron_Pointer(nsd, xij, domainName)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferenceHexahedron_), POINTER :: obj
  END FUNCTION reference_Hexahedron_Pointer
END INTERFACE ReferenceHexahedron_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Hexahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE highorderElement_Hexahedron(refelem, order, obj, &
    & ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE highorderElement_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Hexahedron(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Hexahedron_quality
!----------------------------------------------------------------------------

INTERFACE Quality_Hexahedron
  MODULE FUNCTION Hexahedron_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Hexahedron_Quality
END INTERFACE Quality_Hexahedron

!----------------------------------------------------------------------------
!                                                       HexahedronVolume3D
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE HexahedronVolume3D(xij, ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE HexahedronVolume3D
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
!> author: Shion Shimizu
! update: 2024-03-22
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Hexahedron(con, opt, order)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the edge number
    !! The row represents a edge
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt =2, then edge connectivity for Lagrangian approximation
    !! opt=1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order default is 1
  END SUBROUTINE GetEdgeConnectivity_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetFaceConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
!> author: Shion Shimizu
! update : 2024-03-22
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Hexahedron(con, opt, order)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the face number
    !! The row represents a face
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then face connectivity for hierarchial approximation
    !! If opt =2, then face connectivity for Lagrangian approximation
    !! opt=1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order default is 1
  END SUBROUTINE GetFaceConnectivity_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       RefHexahedronCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-07
! summary:  Returns coordinates of reference Hexahedron

INTERFACE RefCoord_Hexahedron
  MODULE PURE FUNCTION RefHexahedronCoord(refHexahedron) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refHexahedron
    !! UNIT
    !! BIUNIT
    REAL(DFP) :: ans(3, 8)
  END FUNCTION RefHexahedronCoord
END INTERFACE RefCoord_Hexahedron

!----------------------------------------------------------------------------
!                                        GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-11
! summary:  Returns the element type of each face

INTERFACE
  MODULE PURE SUBROUTINE GetFaceElemType_Hexahedron(faceElemType, opt,  &
    & tFaceNodes, elemType)
    INTEGER(I4B), INTENT(INOUT) :: faceElemType(:)
    !! Face element type
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! total nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! This denotes the element type of Hexahedron
    !! Default value is Hexahedron6
  END SUBROUTINE GetFaceElemType_Hexahedron
END INTERFACE

END MODULE ReferenceHexahedron_Method
