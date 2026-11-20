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

MODULE ElemshapeData_Orthogonal

USE BaseType, ONLY: ElemshapeData_, &
                    QuadraturePoint_, &
                    ReferenceElement_, &
                    OrthogonalInterpolation_, &
                    H1_

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: OrthogonalElemShapeData
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Initiates orthogonal shape function data from an element type
!   and a domain name.
!
! This routine fills `obj` with orthogonal shape functions at the
! quadrature points `quad`.
!
! 1. It allocates `obj` for the number of shape functions of `order`.
! 2. It copies the quadrature weights into `obj%ws`.
! 3. It evaluates the orthogonal polynomials into `obj%N`.
! 4. It evaluates their gradients into `obj%dNdXi`.
!
! @note
! The routine sets only `obj%ws`, `obj%N` and `obj%dNdXi`.
! It does not compute the Jacobian or the physical coordinates.
! @endnote
!
! @note
! The routine does not read `refelemCoord`.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/OrthogonalElemShapeData_test_1.F90)
!
! See also: [[OrthogonalElemShapeData2]], [[OrthogonalElemShapeData3]]

INTERFACE OrthogonalElemShapeData
  MODULE SUBROUTINE OrthogonalElemShapeData1(obj, quad, nsd, xidim, &
                       elemType, refelemCoord, domainName, order, basisType, &
                                             alpha, beta, lambda)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points
    INTEGER(I4B), INTENT(IN) :: nsd
    !! number of spatial dimensions
    INTEGER(I4B), INTENT(IN) :: xidim
    !! dimension of `xi`
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: refelemCoord(:, :)
    !! coordinate of reference element
    CHARACTER(*), INTENT(IN) :: domainName
    !! name of reference element domain
    INTEGER(I4B), INTENT(IN) :: order
    !! cell order, always needed
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! parameters for Jacobi and ultraspherical polynomials
  END SUBROUTINE OrthogonalElemShapeData1
END INTERFACE OrthogonalElemShapeData

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Initiates orthogonal shape function data from a reference
!   element.
!
! This routine reads `nsd`, the dimension of `xi`, the element type,
! the reference coordinates and the domain name from `refelem`.
! It then calls [[OrthogonalElemShapeData1]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/OrthogonalElemShapeData_test_1.F90)
!
! See also: [[OrthogonalElemShapeData1]]

INTERFACE OrthogonalElemShapeData
  MODULE SUBROUTINE OrthogonalElemShapeData2(obj, quad, refelem, order, &
                                             basisType, alpha, beta, lambda)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    INTEGER(I4B), INTENT(IN) :: order
    !! cell order, always needed
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! parameters for Jacobi and ultraspherical polynomials,
    !! needed for line, quadrangle and hexahedron elements
  END SUBROUTINE OrthogonalElemShapeData2
END INTERFACE OrthogonalElemShapeData

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Initiates orthogonal shape function data from a reference
!   element, a continuity type and an interpolation type.
!
! This routine fills `obj` with orthogonal shape functions at the
! quadrature points `quad`.
! It gives the same result as [[OrthogonalElemShapeData2]].
!
! @note
! `baseContinuity` and `baseInterpolation` only select this overload.
! The routine does not read them.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/OrthogonalElemShapeData_test_1.F90)
!
! See also: [[OrthogonalElemShapeData1]], [[OrthogonalElemShapeData2]]

INTERFACE OrthogonalElemShapeData
  MODULE SUBROUTINE OrthogonalElemShapeData3(obj, quad, refelem, &
     baseContinuity, baseInterpolation, order, basisType, alpha, beta, lambda)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    TYPE(H1_), INTENT(IN) :: baseContinuity
    !! base continuity, only used to select this overload
    TYPE(OrthogonalInterpolation_), INTENT(IN) :: baseInterpolation
    !! base interpolation, only used to select this overload
    INTEGER(I4B), INTENT(IN) :: order
    !! cell order, always needed
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! parameters for Jacobi and ultraspherical polynomials
  END SUBROUTINE OrthogonalElemShapeData3
END INTERFACE OrthogonalElemShapeData

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> Initiate is a generic name for [[OrthogonalElemShapeData3]].
!
! See [[OrthogonalElemShapeData3]] for the arguments.

INTERFACE Initiate
  MODULE PROCEDURE OrthogonalElemShapeData3
END INTERFACE Initiate

END MODULE ElemshapeData_Orthogonal
