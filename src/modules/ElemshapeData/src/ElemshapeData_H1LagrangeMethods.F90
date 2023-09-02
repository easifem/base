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
!

MODULE ElemshapeData_H1LagrangeMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                        Initiate@H1Lagrange
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data

INTERFACE Initiate
  MODULE PURE SUBROUTINE H1_Lagrange1( &
    & obj, &
    & quad, &
    & refelem, &
    & continuityType, &
    & interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    CLASS(H1_), INTENT(IN) :: continuityType
    CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1_Lagrange1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-16
! summary: This routine initiate the shape data

INTERFACE Initiate
  MODULE SUBROUTINE H1_Lagrange2( &
    & obj, &
    & quad, &
    & refelem, &
    & baseContinuity, &
    & baseInterpolation, &
    & ipType, &
    & basisType, &
    & order,  &
    & coeff,  &
    & firstCall,  &
    & alpha, &
    & beta, &
    & lambda)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    CLASS(H1_), INTENT(IN) :: baseContinuity
    CLASS(LagrangeInterpolation_), INTENT(IN) :: baseInterpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    INTEGER(I4B), INTENT(IN) :: basisType
    INTEGER(I4B), INTENT(IN) :: order
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE H1_Lagrange2
END INTERFACE Initiate

END MODULE ElemshapeData_H1LagrangeMethods
