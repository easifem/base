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

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Lagrange shape function data of cells and facets
!
! Compute Lagrange shape functions and their derivatives with respect to
! the reference coordinates at the quadrature points.

MODULE ElemshapeData_Lagrange
USE BaseType, ONLY: ElemshapeData_, &
                    QuadraturePoint_, &
                    ReferenceElement_, &
                    H1_, &
                    LagrangeInterpolation_

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: LagrangeElemShapeData
PUBLIC :: LagrangeFacetElemShapeData
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                               LagrangeElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Lagrange shape data from element type and reference coordinates
!
! Computes Lagrange shape functions and their local derivatives at the
! quadrature points of `quad`.
!
! It allocates `obj` with `nns = LagrangeDOF(order, elemType)` nodes
! and `nips` quadrature points, see [[obj_Allocate]]. Then it sets:
!
! - `ws` to the quadrature weights of `quad`
! - `N(i, ip)` to shape function `i` at quadrature point `ip`
! - `dNdXi(i, k, ip)` to the derivative of `N(i, ip)` along `xi_k`
!
! It places the interpolation points with `ipType` on the reference
! element `refelemCoord`, in "VEFC" layout.
!
! If `coeff` is present and `firstCall` is true, it computes the
! coefficients into `coeff`. If `firstCall` is false, it reuses them.
!
! @note
! It does not set `dNdXt`, `jacobian`, `js` or `coord`.
! These depend on the physical element; set them with [[Set]].
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/LagrangeElemShapeData_test_1.F90) first three blocks: line, quadratic line, quadrangle
!
! See also: [[LagrangeElemShapeData2]], [[LagrangeFacetElemShapeData]]

INTERFACE LagrangeElemShapeData
  MODULE SUBROUTINE LagrangeElemShapeData1(obj, quad, nsd, xidim, &
                                           elemType, refelemCoord, &
                                           domainName, order, ipType, &
                                           basisType, coeff, firstCall, &
                                           alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
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
    !! coordinate of reference element, refelemCoord(xidim, nodes)
    CHARACTER(*), INTENT(IN) :: domainName
    !! name of reference element domain, "UNIT" or "BIUNIT"
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials, coeff(nns, nns)
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is false, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultra-spherical parameter
  END SUBROUTINE LagrangeElemShapeData1
END INTERFACE LagrangeElemShapeData

!----------------------------------------------------------------------------
!                                               LagrangeElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Lagrange shape data from a reference element
!
! Computes Lagrange shape data like [[LagrangeElemShapeData1]].
! It takes `nsd`, `xidim`, `elemType`, `refelemCoord` and
! `domainName` from `refelem`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/LagrangeElemShapeData_test_1.F90) fourth block, from `ReferenceLine`
!
! See also: [[LagrangeElemShapeData1]]

INTERFACE LagrangeElemShapeData
  MODULE SUBROUTINE LagrangeElemShapeData2(obj, quad, refelem, order, &
                                           ipType, basisType, coeff, &
                                           firstCall, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials, coeff(nns, nns)
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is false, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultra-spherical parameter
  END SUBROUTINE LagrangeElemShapeData2
END INTERFACE LagrangeElemShapeData

!----------------------------------------------------------------------------
!                                               LagrangeElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Lagrange shape data for H1 continuity
!
! Computes Lagrange shape data like [[LagrangeElemShapeData2]].
! The generic `Initiate` calls it when `baseContinuity` is `TypeH1`
! and `baseInterpolation` is `TypeLagrangeInterpolation`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/LagrangeElemShapeData_test_1.F90) fourth block, with `Initiate`
!
! See also: [[LagrangeElemShapeData2]]

INTERFACE LagrangeElemShapeData
  MODULE SUBROUTINE LagrangeElemShapeData3( &
    obj, quad, refelem, baseContinuity, baseInterpolation, order, ipType, &
    basisType, coeff, firstCall, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    TYPE(H1_), INTENT(IN) :: baseContinuity
    !! TypeH1
    TYPE(LagrangeInterpolation_), INTENT(IN) :: baseInterpolation
    !! TypeLagrangeInterpolation
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials, coeff(nns, nns)
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is false, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultra-spherical parameter
  END SUBROUTINE LagrangeElemShapeData3
END INTERFACE LagrangeElemShapeData

INTERFACE Initiate
  MODULE PROCEDURE LagrangeElemShapeData3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                          LagrangeFacetElemShapeData@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: Lagrange shape data of a cell and one of its facets
!
! Computes the shape data of a cell and of one of its facets.
!
! 1. Computes the cell data in `obj` like [[LagrangeElemShapeData1]].
! 2. Computes the data of facet `localFaceNumber` in `facetElemsd`,
!    at the quadrature points `facetQuad`.
!
! The facet uses the face element type and its reference coordinates.
! Its `xidim` is `xidim - 1`. It uses the same `nsd`, `order`,
! `ipType` and `basisType` as the cell.
!
! @note
! `coeff` and `firstCall` apply to the cell only.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/LagrangeFacetElemShapeData_test_1.F90) quadrangle and its first face
!
! See also: [[LagrangeElemShapeData1]]

INTERFACE LagrangeFacetElemShapeData
  MODULE SUBROUTINE LagrangeFacetElemShapeData1( &
    obj, facetElemsd, quad, facetQuad, localFaceNumber, nsd, xidim, &
    elemType, refelemCoord, domainName, order, ipType, basisType, coeff, &
    firstCall, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    CLASS(ElemshapeData_), INTENT(INOUT) :: facetElemsd
    !! facet element shape data
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature point
    TYPE(QuadraturePoint_), INTENT(IN) :: facetQuad
    !! quadrature point on local facet
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(IN) :: nsd
    !! number of spatial dimension
    INTEGER(I4B), INTENT(IN) :: xidim
    !!  dimension of xi
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: refelemCoord(:, :)
    !! coordinate of reference element, refelemCoord(xidim, nodes)
    CHARACTER(*), INTENT(IN) :: domainName
    !! name of reference element domain, "UNIT" or "BIUNIT"
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials, coeff(nns, nns)
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is false, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultra-spherical parameter
  END SUBROUTINE LagrangeFacetElemShapeData1
END INTERFACE LagrangeFacetElemShapeData

END MODULE ElemshapeData_Lagrange
