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

MODULE ElemshapeData_HCurlMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Initiates the shape data with Lagrange HCurl basis (not yet
!   available)
!
! This routine will initiate the shape function data of an element.
! It will use Lagrange interpolation with H(curl) continuity.
!
! @warning
! This method is not implemented yet. It only writes a "work in progress"
! message to `stderr` and leaves `obj` unchanged.
! @endwarning
!
! See also: [[HCurl_Hierarchy1]], [[HCurl_Orthogonal1]]

INTERFACE Initiate
  MODULE SUBROUTINE HCurl_Lagrange1( &
    & obj, &
    & quad, &
    & refelem, &
    & baseContinuity, &
    & baseInterpolation, &
    & order,  &
    & ipType, &
    & basisType, &
    & coeff,  &
    & firstCall,  &
    & alpha, &
    & beta, &
    & lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! Quadrature points
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element
    CLASS(HCurl_), INTENT(IN) :: baseContinuity
    !! Base continuity type
    CLASS(LagrangeInterpolation_), INTENT(IN) :: baseInterpolation
    !! Base interpolation type
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of polynomials
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If `.TRUE.`, the routine will compute `coeff`.
    !! If `.FALSE.`, the routine will use the given `coeff`.
    !! The default is `.TRUE.`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! alpha and beta are Jacobi polynomial param
    !! lambda is Ultraspherical polynomial param
  END SUBROUTINE HCurl_Lagrange1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@HCurlHierarchy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: Initiates the shape data with hierarchic HCurl basis (not yet
!   available)
!
! This routine will initiate the shape function data of an element.
! It will use hierarchic interpolation with H(curl) continuity.
!
! @warning
! This method is not implemented yet. It only writes a "work in progress"
! message to `stderr` and leaves `obj` unchanged.
! @endwarning
!
! See also: [[HCurl_Lagrange1]], [[HCurl_Orthogonal1]]

INTERFACE Initiate
  MODULE SUBROUTINE HCurl_Hierarchy1( &
    & obj, &
    & quad, &
    & refelem, &
    & baseContinuity, &
    & baseInterpolation,  &
    & order,  &
    & ipType,  &
    & basisType,  &
    & alpha, beta, lambda &
    &)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point type
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element type
    CLASS(HCurl_), INTENT(IN) :: baseContinuity
    !! Base continuity type
    CLASS(HierarchyInterpolation_), INTENT(IN) :: baseInterpolation
    !! Base Interpolation type
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of polynomials
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation type
    !! This argument is not needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function type
    !! This argument is not needed
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! alpha and beta are Jacobi polynomial param
    !! lambda is Ultraspherical polynomial param
    !! This argument is not needed
  END SUBROUTINE HCurl_Hierarchy1
END INTERFACE Initiate


!----------------------------------------------------------------------------
!                                                    Initiate@HCurlOrthogonal
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: Initiates the shape data with orthogonal HCurl basis (not yet
!   available)
!
! This routine will initiate the shape function data of an element.
! It will use orthogonal interpolation with H(curl) continuity.
!
! @warning
! This method is not implemented yet. It only writes a "work in progress"
! message to `stderr` and leaves `obj` unchanged.
! @endwarning
!
! See also: [[HCurl_Lagrange1]], [[HCurl_Hierarchy1]]

INTERFACE Initiate
  MODULE SUBROUTINE HCurl_Orthogonal1( &
    & obj, &
    & quad, &
    & refelem, &
    & baseContinuity, &
    & baseInterpolation,  &
    & order,  &
    & ipType,  &
    & basisType,  &
    & alpha, beta, lambda &
    &)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point type
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element type
    CLASS(HCurl_), INTENT(IN) :: baseContinuity
    !! Base continuity type
    CLASS(OrthogonalInterpolation_), INTENT(IN) :: baseInterpolation
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
  END SUBROUTINE HCurl_Orthogonal1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@HCurlHermit
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiates the shape data with Hermit HCurl basis (not yet
!   available)
!
! This routine will initiate the shape function data of an element.
! It will use Hermit interpolation with H(curl) continuity.
!
! @warning
! This method is not implemented yet. It only writes a "work in progress"
! message to `stderr` and leaves `obj` unchanged.
! @endwarning
!
! See also: [[HCurl_Lagrange1]], [[HCurl_Serendipity1]]

INTERFACE Initiate
  MODULE SUBROUTINE HCurl_Hermit1( &
    & obj, &
    & quad, &
    & refelem, &
    & baseContinuity, &
    & baseInterpolation,  &
    & order,  &
    & ipType,  &
    & basisType,  &
    & alpha, beta, lambda &
    &)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point type
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element type
    CLASS(HCurl_), INTENT(IN) :: baseContinuity
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
  END SUBROUTINE HCurl_Hermit1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@HCurlSerendipity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiates the shape data with serendipity HCurl basis (not yet
!   available)
!
! This routine will initiate the shape function data of an element.
! It will use serendipity interpolation with H(curl) continuity.
!
! @warning
! This method is not implemented yet. It only writes a "work in progress"
! message to `stderr` and leaves `obj` unchanged.
! @endwarning
!
! See also: [[HCurl_Lagrange1]], [[HCurl_Hermit1]]

INTERFACE Initiate
  MODULE SUBROUTINE HCurl_Serendipity1( &
    & obj, &
    & quad, &
    & refelem, &
    & baseContinuity, &
    & baseInterpolation,  &
    & order,  &
    & ipType,  &
    & basisType,  &
    & alpha, beta, lambda &
    &)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point type
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element type
    CLASS(HCurl_), INTENT(IN) :: baseContinuity
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
  END SUBROUTINE HCurl_Serendipity1
END INTERFACE Initiate

END MODULE ElemshapeData_HCurlMethods
