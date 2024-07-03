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
USE BaseType, ONLY: ElemshapeData_, &
                    QuadraturePoint_, &
                    ReferenceElement_, &
                    H1_, &
                    LagrangeInterpolation_, &
                    HierarchyInterpolation_, &
                    OrthogonalInterpolation_, &
                    HermitInterpolation_, &
                    SerendipityInterpolation_

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@H1Hierarchy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.

INTERFACE Initiate
  MODULE SUBROUTINE H1_Hierarchy1(obj, quad, refelem, baseContinuity, &
             baseInterpolation, order, ipType, basisType, alpha, beta, lambda)
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
  END SUBROUTINE H1_Hierarchy1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@H1Orthogonal
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.

INTERFACE Initiate
  MODULE SUBROUTINE H1_Orthogonal1(obj, quad, refelem, baseContinuity, &
             baseInterpolation, order, ipType, basisType, alpha, beta, lambda)
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
    !! Order of polynomials
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! alpha and beta are Jacobi polynomial param
    !! lambda is Ultraspherical polynomial param
  END SUBROUTINE H1_Orthogonal1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@H1Hermit
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.

INTERFACE Initiate
  MODULE SUBROUTINE H1_Hermit1(obj, quad, refelem, baseContinuity, &
             baseInterpolation, order, ipType, basisType, alpha, beta, lambda)
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
!                                                    Initiate@H1Serendipity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data

INTERFACE Initiate
  MODULE SUBROUTINE H1_Serendipity1(obj, quad, refelem, baseContinuity, &
             baseInterpolation, order, ipType, basisType, alpha, beta, lambda)
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
