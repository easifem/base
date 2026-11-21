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
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: H1_
USE BaseType, ONLY: HierarchyInterpolation_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceElement_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
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
! summary: Initiates the hierarchical shape data from element data
!
! Fills `obj` with hierarchical basis functions and their gradients at
! the quadrature points `quad`.
! The arguments `nsd`, `xidim`, `elemType`, `refelemCoord` and
! `domainName` describe the element.
!
! The routine does the following.
!
! 1. It counts the basis functions from `elemType` and the orders.
! 2. It allocates `obj` for that many functions and points.
! 3. It stores the quadrature weights in `obj%ws`.
! 4. It stores the basis values in `obj%N` and the gradients in
!    `obj%dNdXi`.
!
! The orders and orientations are optional, but the element needs some.
! Pass `cellOrder` and `cellOrient` for every element.
! Pass `faceOrient` for 2D elements.
! Pass `faceOrder` for 2D and 3D elements.
! Pass `edgeOrder` for 3D elements.
!
! @note
! The routine does not use `refelemCoord`.
! The basis exists only for lines, triangles and quadrangles.
! For other elements `obj%N` and `obj%dNdXi` are not valid.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/HierarchicalElemShapeData_test_1.F90)
!
! See also: [[HierarchicalElemShapeData2]], [[HierarchicalElemShapeData3]]

INTERFACE HierarchicalElemShapeData
  MODULE SUBROUTINE HierarchicalElemShapeData1( &
    obj, quad, nsd, xidim, elemType, refelemCoord, domainName, cellOrder, &
    faceOrder, edgeOrder, cellOrient, faceOrient, edgeOrient)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data, the routine allocates and fills it
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points and weights
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
! summary: Initiates the hierarchical shape data from a reference element
!
! Fills `obj` with hierarchical basis functions and their gradients at
! the quadrature points `quad`.
! The routine reads the element data from `refelem`.
! Then it calls [[HierarchicalElemShapeData1]].
!
! Pass `cellOrder` for every element.
! Pass `faceOrder` for 2D and 3D elements.
! Pass `edgeOrder` for 3D elements.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/HierarchicalElemShapeData_test_1.F90)
!
! See also: [[HierarchicalElemShapeData1]]

INTERFACE HierarchicalElemShapeData
  MODULE SUBROUTINE HierarchicalElemShapeData2( &
    obj, quad, refelem, cellOrder, faceOrder, edgeOrder, cellOrient, &
    faceOrient, edgeOrient)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data, the routine allocates and fills it
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points and weights
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
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
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Initiates the hierarchical shape data, with continuity types
!
! Fills `obj` with hierarchical basis functions and their gradients at
! the quadrature points `quad`.
! This routine is also available under the generic name `Initiate`.
!
! It accepts `baseContinuity` and `baseInterpolation` but does not use
! them.
! It calls [[HierarchicalElemShapeData2]] with the other arguments.
!
! @note
! The result does not depend on `baseContinuity` or `baseInterpolation`.
! @endnote
!
! Pass `cellOrder` for every element.
! Pass `faceOrder` for 2D and 3D elements.
! Pass `edgeOrder` for 3D elements.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/HierarchicalElemShapeData_test_1.F90)
! test 6, with `Initiate`
!
! See also: [[HierarchicalElemShapeData2]]

INTERFACE HierarchicalElemShapeData
  MODULE SUBROUTINE HierarchicalElemShapeData3( &
    obj, quad, refelem, baseContinuity, baseInterpolation, cellOrder, &
    faceOrder, edgeOrder, cellOrient, faceOrient, edgeOrient)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data, the routine allocates and fills it
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points and weights
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    TYPE(H1_), INTENT(IN) :: baseContinuity
    !! base continuity, not used
    TYPE(HierarchyInterpolation_), INTENT(IN) :: baseInterpolation
    !! base interpolation, not used
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
! summary: Initiates the hierarchical shape data of an element and a facet
!
! Fills `obj` with the shape data of the element, as
! [[HierarchicalElemShapeData1]] does.
! Then it fills `facetElemsd` with the shape data of the face
! `localFaceNumber`, at the facet quadrature points `facetQuad`.
!
! The facet data uses `faceOrder(:, localFaceNumber)` as its cell order.
! It uses no orientations.
!
! @warning
! This routine is under development.
! In a debug build it stops with an error.
! @endwarning
!
! See also: [[HierarchicalElemShapeData1]]

INTERFACE HierarchicalFacetElemShapeData
  MODULE SUBROUTINE HierarchicalFacetElemShapeData1( &
    obj, facetElemsd, quad, facetQuad, localFaceNumber, nsd, xidim, &
    elemType, refelemCoord, domainName, cellOrder, faceOrder, edgeOrder, &
    cellOrient, faceOrient, edgeOrient)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj, facetElemsd
    !! `obj` is the element shape data
    !! `facetElemsd` is the facet shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad, facetQuad
    !! `quad` is the quadrature of the element
    !! `facetQuad` is the quadrature of the facet
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number, used as a column of `faceOrder`
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
