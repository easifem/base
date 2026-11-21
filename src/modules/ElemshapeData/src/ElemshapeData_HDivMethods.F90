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
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: HDiv_
USE BaseType, ONLY: LagrangeInterpolation_
USE BaseType, ONLY: HierarchyInterpolation_
USE BaseType, ONLY: OrthogonalInterpolation_
USE BaseType, ONLY: HermitInterpolation_
USE BaseType, ONLY: SerendipityInterpolation_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Initiate the shape data with Lagrange basis for H(div) elements
!
! Once implemented, this routine fills `obj` with the shape data of an
! H(div) element that uses Lagrange polynomials.
!
! @warning
! This method is not implemented yet. It only prints a
! "work in progress" error message and leaves `obj` unchanged.
! @endwarning

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Lagrange1(obj, quad, refelem, baseContinuity, &
                                   baseInterpolation, order, ipType, &
                                   basisType, coeff, firstCall, alpha, beta, &
                                   lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! Element shape data
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! Quadrature points
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference element
    CLASS(HDiv_), INTENT(IN) :: baseContinuity
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
    !! If `firstCall` is true, the routine computes `coeff`.
    !! If false, it reuses `coeff`. The default is true.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! alpha and beta are Jacobi polynomial param
    !! lambda is Ultraspherical polynomial param
  END SUBROUTINE HDiv_Lagrange1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: Initiate the shape data with hierarchic basis for H(div) elements
!
! Once implemented, this routine fills `obj` with the shape data of an
! H(div) element that uses hierarchic polynomials.
!
! @warning
! This method is not implemented yet. It only prints a
! "work in progress" error message and leaves `obj` unchanged.
! @endwarning

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Hierarchy1(obj, quad, refelem, baseContinuity, &
                                    baseInterpolation, order, ipType, &
                                    basisType, alpha, beta, lambda)
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
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-02
! summary: Initiate the shape data with orthogonal basis for H(div) elements
!
! Once implemented, this routine fills `obj` with the shape data of an
! H(div) element that uses orthogonal polynomials.
!
! @warning
! This method is not implemented yet. It only prints a
! "work in progress" error message and leaves `obj` unchanged.
! @endwarning

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Orthogonal1(obj, quad, refelem, baseContinuity, &
                                     baseInterpolation, order, ipType, &
                                     basisType, alpha, beta, lambda)
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
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiate the shape data with Hermite basis for H(div) elements
!
! Once implemented, this routine fills `obj` with the shape data of an
! H(div) element that uses Hermite polynomials.
!
! @warning
! This method is not implemented yet. It only prints a
! "work in progress" error message and leaves `obj` unchanged.
! @endwarning

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Hermit1(obj, quad, refelem, baseContinuity, &
                                 baseInterpolation, order, ipType, &
                                 basisType, &
                                 alpha, beta, lambda)
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
!                                                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiate the shape data with serendipity basis for H(div) elements
!
! Once implemented, this routine fills `obj` with the shape data of an
! H(div) element that uses serendipity polynomials.
!
! @warning
! This method is not implemented yet. It only prints a
! "work in progress" error message and leaves `obj` unchanged.
! @endwarning

INTERFACE Initiate
  MODULE SUBROUTINE HDiv_Serendipity1(obj, quad, refelem, baseContinuity, &
                                      baseInterpolation, order, ipType, &
                                      basisType, alpha, beta, lambda)
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
