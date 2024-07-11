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
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: This routine initiate the shape data

INTERFACE LagrangeElemShapeData
  MODULE SUBROUTINE LagrangeElemShapeData1(obj, quad, nsd, xidim, &
               elemType, refelemCoord, domainName, order, ipType, basisType, &
                                        coeff, firstCall, alpha, beta, lambda)
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
    !! coordinate of reference element
    CHARACTER(*), INTENT(IN) :: domainName
    !! name of reference element domain
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is false, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! Jacobi parameter and Ultra-spherical parameter
  END SUBROUTINE LagrangeElemShapeData1
END INTERFACE LagrangeElemShapeData

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: This routine initiate the shape data

INTERFACE LagrangeElemShapeData
  MODULE SUBROUTINE LagrangeElemShapeData2(obj, quad, refelem, order, &
                     ipType, basisType, coeff, firstCall, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE LagrangeElemShapeData2
END INTERFACE LagrangeElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeElemShapeData
MODULE SUBROUTINE LagrangeElemShapeData3(obj, quad, refelem, baseContinuity, &
              baseInterpolation, order, ipType, basisType, coeff, firstCall, &
                                           alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    CLASS(H1_), INTENT(IN) :: baseContinuity
    CLASS(LagrangeInterpolation_), INTENT(IN) :: baseInterpolation
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type
    !! Default value is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis function types
    !! Default value is Monomial
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE LagrangeElemShapeData3
END INTERFACE LagrangeElemShapeData

INTERFACE Initiate
  MODULE PROCEDURE LagrangeElemShapeData3
END INTERFACE Initiate

END MODULE ElemshapeData_Lagrange
