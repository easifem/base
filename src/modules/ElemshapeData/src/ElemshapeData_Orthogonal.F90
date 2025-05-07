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
! summary: This routine initiate orthogonal shape function data

INTERFACE OrthogonalElemShapeData
  MODULE SUBROUTINE OrthogonalElemShapeData1(obj, quad, nsd, xidim, &
                       elemType, refelemCoord, domainName, order, basisType, &
                                             alpha, beta, lambda)
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
    INTEGER(I4B), INTENT(IN) :: order
    !! cell order, always needed
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! parameters for Jacobi and Ultraspherical poly
  END SUBROUTINE OrthogonalElemShapeData1
END INTERFACE OrthogonalElemShapeData

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: This routine initiate the shape data

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
    !! needed for line, quad, and hexa element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE OrthogonalElemShapeData2
END INTERFACE OrthogonalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE OrthogonalElemShapeData
  MODULE SUBROUTINE OrthogonalElemShapeData3(obj, quad, refelem, &
     baseContinuity, baseInterpolation, order, basisType, alpha, beta, lambda)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    TYPE(H1_), INTENT(IN) :: baseContinuity
    !! base continuity
    TYPE(OrthogonalInterpolation_), INTENT(IN) :: baseInterpolation
    !! base interpolation
    INTEGER(I4B), INTENT(IN) :: order
    !! cell order, always needed
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE OrthogonalElemShapeData3
END INTERFACE OrthogonalElemShapeData

INTERFACE Initiate
  MODULE PROCEDURE OrthogonalElemShapeData3
END INTERFACE Initiate

END MODULE ElemshapeData_Orthogonal
