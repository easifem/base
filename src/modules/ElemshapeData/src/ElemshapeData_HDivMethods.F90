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

MODULE ElemshapeData_HDivMethods
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
! summary: This routine initiate the shape data

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Lagrange1( &
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
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    CLASS(HDiv_), INTENT(IN) :: baseContinuity
    CLASS(LagrangeInterpolation_), INTENT(IN) :: baseInterpolation
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE HDiv_Lagrange1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@HDivHierarchy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Hierarchy1( &
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
    CLASS(HDiv_), INTENT(IN) :: baseContinuity
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
  END SUBROUTINE HDiv_Hierarchy1
END INTERFACE Initiate


!----------------------------------------------------------------------------
!                                                    Initiate@HDivOrthogonal
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Orthogonal1( &
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
    CLASS(HDiv_), INTENT(IN) :: baseContinuity
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
  END SUBROUTINE HDiv_Orthogonal1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@HDivHermit
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Hermit1( &
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
    CLASS(HDiv_), INTENT(IN) :: baseContinuity
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
  END SUBROUTINE HDiv_Hermit1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@HDivSerendipity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Serendipity1( &
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
    CLASS(HDiv_), INTENT(IN) :: baseContinuity
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
  END SUBROUTINE HDiv_Serendipity1
END INTERFACE Initiate

END MODULE ElemshapeData_HDivMethods
