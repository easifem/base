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
! summary: This module contains methods for [[ReferenceTetrahedron_]]

MODULE ReferenceTetrahedron_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate
PUBLIC :: ReferenceTetrahedron
PUBLIC :: ReferenceTetrahedron_Pointer
PUBLIC :: HighOrderElement_Tetrahedron
PUBLIC :: Measure_Simplex_Tetrahedron
PUBLIC :: Tetrahedron_Quality
PUBLIC :: TetrahedronVolume3D
PUBLIC :: Quality_Tetrahedron
PUBLIC :: GetEdgeConnectivity_Tetrahedron
PUBLIC :: GetFaceConnectivity_Tetrahedron
PUBLIC :: RefCoord_Tetrahedron
PUBLIC :: GetFaceElemType_Tetrahedron
PUBLIC :: FacetElements_Tetrahedron
PUBLIC :: ElementOrder_Tetrahedron
PUBLIC :: ElementType_Tetrahedron
PUBLIC :: TotalNodesInElement_Tetrahedron
PUBLIC :: TotalEntities_Tetrahedron

!----------------------------------------------------------------------------
!                                                 TotalEntities_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total entities in Tetrahedron

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                            TotalNodesInElement_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total nodes in element

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the type of element from char name

INTERFACE
  MODULE PURE FUNCTION ElementType_Tetrahedron(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ElementOrder_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the order of element

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElements_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Tetrahedron
  MODULE SUBROUTINE FacetElements_Tetrahedron1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Tetrahedron1
END INTERFACE FacetElements_Tetrahedron

!----------------------------------------------------------------------------
!                                                  FacetElements_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Tetrahedron
  MODULE SUBROUTINE FacetElements_Tetrahedron2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Tetrahedron2
END INTERFACE FacetElements_Tetrahedron

!----------------------------------------------------------------------------
!                                                       Initiate@Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary: This subroutine for constructing the object

INTERFACE Initiate
  MODULE PURE SUBROUTINE initiate_ref_Tetrahedron(obj, nsd, xij, domainName)
    CLASS(ReferenceTetrahedron_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE initiate_ref_Tetrahedron
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceTetrahedron@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE ReferenceTetrahedron
  MODULE PURE FUNCTION reference_Tetrahedron(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferenceTetrahedron_) :: obj
  END FUNCTION reference_Tetrahedron
END INTERFACE ReferenceTetrahedron

!----------------------------------------------------------------------------
!                                   ReferenceTetrahedron_Pointer@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE ReferenceTetrahedron_Pointer
  MODULE FUNCTION reference_Tetrahedron_Pointer(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferenceTetrahedron_), POINTER :: obj
  END FUNCTION reference_Tetrahedron_Pointer
END INTERFACE ReferenceTetrahedron_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE HighOrderElement_Tetrahedron( &
    & refelem, &
    & order, &
    & obj, &
    & ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighOrderElement_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Tetrahedron(RefElem, XiJ) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Tetrahedron_Quality
!----------------------------------------------------------------------------

INTERFACE Quality_Tetrahedron
  MODULE FUNCTION Tetrahedron_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Tetrahedron_Quality
END INTERFACE Quality_Tetrahedron

!----------------------------------------------------------------------------
!                                                       TetrahedronVolume3D
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE TetrahedronVolume3D(xij, ans)
    REAL(DFP), INTENT(IN) :: xij(3, 4)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE TetrahedronVolume3D
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Tetrahedron(con, opt,  &
    & order)
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
    !! Order of the edge
  END SUBROUTINE GetEdgeConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetFaceConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Tetrahedron(con, opt, order)
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
    !! order
  END SUBROUTINE GetFaceConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       RefCoord_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference Tetrahedron

INTERFACE
  MODULE PURE FUNCTION RefCoord_Tetrahedron(refTetrahedron) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    REAL(DFP) :: ans(3, 4)
  END FUNCTION RefCoord_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-11
! summary:  Returns the element type of each face

INTERFACE
  MODULE PURE SUBROUTINE GetFaceElemType_Tetrahedron(faceElemType, opt,  &
    & tFaceNodes)
    INTEGER(I4B), INTENT(INOUT) :: faceElemType(:)
    !! Face element type
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! total nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Tetrahedron
END INTERFACE

END MODULE ReferenceTetrahedron_Method
