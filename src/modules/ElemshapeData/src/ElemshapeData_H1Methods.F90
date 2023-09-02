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
! summary: This routine initiates H1 Lagrange polynomials

INTERFACE
  MODULE SUBROUTINE ElemshapeData_H1Methods1( &
    & obj, &
    & quad, &
    & refelem, &
    & baseInterpol,  &
    & basisType,  &
    & order)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    CLASS(LagrangeInterpolation_), INTENT(IN) :: baseInterpol
    INTEGER(I4B), INTENT(IN) :: basisType
    INTEGER(I4B), INTENT(IN) :: order
  END SUBROUTINE ElemshapeData_H1Methods1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Initiate@H1Lagrange
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates H1 Lagrange polynomials

INTERFACE
  MODULE SUBROUTINE ElemshapeData_H1Methods2( &
    & obj, &
    & quad, &
    & refelem, &
    & baseInterpol,  &
    & basisType,  &
    & order)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    CLASS(LagrangeInterpolation_), INTENT(IN) :: baseInterpol
    INTEGER(I4B), INTENT(IN) :: basisType
    INTEGER(I4B), INTENT(IN) :: order
  END SUBROUTINE ElemshapeData_H1Methods1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

END MODULE ElemshapeData_H1Methods
