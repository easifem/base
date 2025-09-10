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

MODULE ElemshapeData_Hierarchical
USE BaseType, ONLY: ElemshapeData_, &
                    QuadraturePoint_, &
                    ReferenceElement_, &
                    H1_, &
                    HierarchyInterpolation_

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: HierarchicalElemShapeData
PUBLIC :: HierarchicalFacetElemShapeData
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: This routine initiate the shape data

INTERFACE HierarchicalElemShapeData
  MODULE SUBROUTINE HierarchicalElemShapeData1( &
    obj, quad, nsd, xidim, elemType, refelemCoord, domainName, cellOrder, &
    faceOrder, edgeOrder, cellOrient, faceOrient, edgeOrient)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point
    INTEGER(I4B), INTENT(IN) :: nsd
    !! number of spatial dimension
    INTEGER(I4B), INTENT(IN) :: xidim
    !!  dimension of xi
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: refelemCoord(:, :)
    !! coordinate of reference element
    CHARACTER(*), INTENT(IN) :: domainName
    !! name of reference element domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, always needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order, needed for 3D elements only
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! orientation of cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! orientation of face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orientation
  END SUBROUTINE HierarchicalElemShapeData1
END INTERFACE HierarchicalElemShapeData

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: This routine initiate the shape data

INTERFACE HierarchicalElemShapeData
  MODULE SUBROUTINE HierarchicalElemShapeData2( &
    obj, quad, refelem, cellOrder, faceOrder, edgeOrder, cellOrient, &
    faceOrient, edgeOrient)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, always needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order, needed for 3D elements only
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! orientation of cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! orientation of face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orientation
  END SUBROUTINE HierarchicalElemShapeData2
END INTERFACE HierarchicalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HierarchicalElemShapeData
  MODULE SUBROUTINE HierarchicalElemShapeData3( &
    obj, quad, refelem, baseContinuity, baseInterpolation, cellOrder, &
    faceOrder, edgeOrder, cellOrient, faceOrient, edgeOrient)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    TYPE(H1_), INTENT(IN) :: baseContinuity
    !! base continuity
    TYPE(HierarchyInterpolation_), INTENT(IN) :: baseInterpolation
    !! base interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, always needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order, needed for 3D elements only
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! orientation of cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! orientation of face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orientation
  END SUBROUTINE HierarchicalElemShapeData3
END INTERFACE HierarchicalElemShapeData

INTERFACE Initiate
  MODULE PROCEDURE HierarchicalElemShapeData3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: This routine initiate the shape data

INTERFACE HierarchicalFacetElemShapeData
  MODULE SUBROUTINE HierarchicalFacetElemShapeData1( &
    obj, facetElemsd, quad, facetQuad, localFaceNumber, nsd, xidim, &
    elemType, refelemCoord, domainName, cellOrder, faceOrder, edgeOrder, &
    cellOrient, faceOrient, edgeOrient)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj, facetElemsd
    !! element shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad, facetQuad
    !! quadrature point
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(IN) :: nsd
    !! number of spatial dimension
    INTEGER(I4B), INTENT(IN) :: xidim
    !!  dimension of xi
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: refelemCoord(:, :)
    !! coordinate of reference element
    CHARACTER(*), INTENT(IN) :: domainName
    !! name of reference element domain
    INTEGER(I4B), INTENT(IN) :: cellOrder(:)
    !! cell order, always needed
    INTEGER(I4B), INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), INTENT(IN) :: edgeOrder(:)
    !! edge order, needed for 3D elements only
    INTEGER(I4B), INTENT(IN) :: cellOrient(:)
    !! orientation of cell
    INTEGER(I4B), INTENT(IN) :: faceOrient(:, :)
    !! orientation of face
    INTEGER(I4B), INTENT(IN) :: edgeOrient(:)
    !! edge orientation
  END SUBROUTINE HierarchicalFacetElemShapeData1
END INTERFACE HierarchicalFacetElemShapeData

END MODULE ElemshapeData_Hierarchical
