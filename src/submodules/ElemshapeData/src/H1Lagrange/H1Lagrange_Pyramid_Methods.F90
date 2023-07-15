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

MODULE H1Lagrange_Pyramid_Methods
USE BaseMethod
IMPLICIT NONE
PRIVATE
PUBLIC :: Pyramid_H1_Lagrange
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

PURE SUBROUTINE Pyramid_H1_Lagrange(obj, quad, refelem, &
  & continuityType, interpolType)
  CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  CLASS(QuadraturePoint_), INTENT(IN) :: quad
  CLASS(ReferenceElement_), INTENT(IN) :: refelem
  CLASS(H1_), INTENT(IN) :: continuityType
  CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType
END SUBROUTINE Pyramid_H1_Lagrange

END MODULE H1Lagrange_Pyramid_Methods
