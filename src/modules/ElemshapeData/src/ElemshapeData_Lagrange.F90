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
                    LagrangeInterpolation_, &
                    HierarchyInterpolation_, &
                    OrthogonalInterpolation_, &
                    HermitInterpolation_, &
                    SerendipityInterpolation_

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: ElemshapeData_InitiateLagrange

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: This routine initiate the shape data

INTERFACE ElemshapeData_InitiateLagrange
  MODULE SUBROUTINE ElemshapeData_InitiateLagrange1(obj, quad, nsd, xidim, &
                      elemType, refelemCoord, refelemDomain, baseContinuity, &
                                baseInterpolation, order, ipType, basisType, &
                                        coeff, firstCall, alpha, beta, lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
    INTEGER(I4B), INTENT(IN) :: nsd
    INTEGER(I4B), INTENT(IN) :: xidim
    INTEGER(I4B), INTENT(IN) :: elemType
    REAL(DFP), INTENT(IN) :: refelemCoord(:, :)
    CHARACTER(*), INTENT(IN) :: refelemDomain
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
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
  END SUBROUTINE ElemshapeData_InitiateLagrange1
END INTERFACE ElemshapeData_InitiateLagrange

END MODULE ElemshapeData_Lagrange
