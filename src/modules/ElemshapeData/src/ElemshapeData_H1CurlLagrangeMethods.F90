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

module ElemshapeData_H1CurlLagrangeMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

public :: Initiate

!----------------------------------------------------------------------------
!                                                  Initiate@H1CurlLagrange
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Curl_Lagrange(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Curl_), INTENT(IN) :: continuityType
    CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Curl_Lagrange
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Curl_Lagrange
END INTERFACE Initiate

end module ElemshapeData_H1CurlLagrangeMethods
