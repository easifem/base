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

MODULE ElemshapeData_H1Methods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: H1_
USE BaseType, ONLY: HermitInterpolation_
USE BaseType, ONLY: HierarchyInterpolation_
USE BaseType, ONLY: LagrangeInterpolation_
USE BaseType, ONLY: OrthogonalInterpolation_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: SerendipityInterpolation_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: Initiate the shape data with hierarchical H1 basis functions
!
! This routine fills `obj` with the values and the local gradients of
! hierarchical basis functions at the quadrature points.
!
! The element type comes from `refelem`. Supported types are `Line`,
! `Triangle`, `Quadrangle`, `Tetrahedron` and `Hexahedron`. For any other
! type the routine writes an error message, and `obj` is not valid.
!
! The routine first deallocates `obj`. It then allocates `obj` for
! the given quadrature points and fills it. It copies the quadrature
! weights to `obj%ws`.
!
! @note
! The arguments `ipType`, `basisType`, `alpha`, `beta` and `lambda` are
! not used. They exist so that all `Initiate` routines of the H1 family
! share one signature.
! @endnote
!
! See also: [[ElemshapeData_H1Methods:H1_Orthogonal1]]

INTERFACE Initiate
  MODULE SUBROUTINE H1_Hierarchy1(obj, quad, refelem, baseContinuity, &
                                  baseInterpolation, order, ipType, &
                                  basisType, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point type
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element type
    CLASS(H1_), INTENT(IN) :: baseContinuity
    !! Base continuity type
    CLASS(HierarchyInterpolation_), INTENT(IN) :: baseInterpolation
    !! Base Interpolation type
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of the polynomials
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation type. Not used.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function type. Not used.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! `alpha` and `beta` are Jacobi polynomial parameters.
    !! `lambda` is the Ultraspherical polynomial parameter.
    !! Not used.
  END SUBROUTINE H1_Hierarchy1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: Initiate the shape data with orthogonal H1 basis functions
!
! This routine fills `obj` with the values and the local gradients of
! orthogonal basis functions at the quadrature points.
!
! The element type comes from `refelem`. Supported types are `Line`,
! `Triangle`, `Quadrangle`, `Tetrahedron` and `Hexahedron`. For any other
! type the routine writes an error message, and `obj` is not valid.
!
! The routine first deallocates `obj`. It then allocates `obj` for
! the given quadrature points and fills it. It copies the quadrature
! weights to `obj%ws`.
!
! If `basisType` is absent, the routine uses `Legendre`.
!
! @note
! `basisType`, `alpha`, `beta` and `lambda` only act on `Line`,
! `Quadrangle` and `Hexahedron`. The other elements ignore them.
! The argument `ipType` is never used.
! @endnote
!
! See also: [[ElemshapeData_H1Methods:H1_Hierarchy1]]

INTERFACE Initiate
  MODULE SUBROUTINE H1_Orthogonal1(obj, quad, refelem, baseContinuity, &
                                   baseInterpolation, order, ipType, &
                                   basisType, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point type
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element type
    CLASS(H1_), INTENT(IN) :: baseContinuity
    !! Base continuity type
    CLASS(OrthogonalInterpolation_), INTENT(IN) :: baseInterpolation
    !! Base Interpolation type
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of the polynomials
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation type. Not used.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function type, default `Legendre`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! `alpha` and `beta` are Jacobi polynomial parameters.
    !! `lambda` is the Ultraspherical polynomial parameter.
  END SUBROUTINE H1_Orthogonal1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiate the shape data with Hermit H1 basis functions
!
! This routine is not available yet. A call only writes the message
! "WORK IN PROGRESS" to `stderr`. It does not change `obj`.
!
! @note
! Do not use this routine until the implementation exists.
! @endnote
!
! See also: [[ElemshapeData_H1Methods:H1_Hierarchy1]]

INTERFACE Initiate
  MODULE SUBROUTINE H1_Hermit1(obj, quad, refelem, baseContinuity, &
                               baseInterpolation, order, ipType, &
                               basisType, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point type
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element type
    CLASS(H1_), INTENT(IN) :: baseContinuity
    !! Base continuity type
    CLASS(HermitInterpolation_), INTENT(IN) :: baseInterpolation
    !! Base Interpolation type
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of polynomials
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! alpha and beta are Jacobi polynomial param
    !! lambda is Ultraspherical polynomial param
  END SUBROUTINE H1_Hermit1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiate the shape data with serendipity H1 basis functions
!
! This routine is not available yet. A call only writes the message
! "WORK IN PROGRESS" to `stderr`. It does not change `obj`.
!
! @note
! Do not use this routine until the implementation exists.
! @endnote
!
! See also: [[ElemshapeData_H1Methods:H1_Hierarchy1]]

INTERFACE Initiate
  MODULE SUBROUTINE H1_Serendipity1(obj, quad, refelem, baseContinuity, &
                                    baseInterpolation, order, ipType, &
                                    basisType, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point type
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element type
    CLASS(H1_), INTENT(IN) :: baseContinuity
    !! Base continuity type
    CLASS(SerendipityInterpolation_), INTENT(IN) :: baseInterpolation
    !! Base Interpolation type
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of polynomials
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! alpha and beta are Jacobi polynomial param
    !! lambda is Ultraspherical polynomial param
  END SUBROUTINE H1_Serendipity1
END INTERFACE Initiate

END MODULE ElemshapeData_H1Methods
