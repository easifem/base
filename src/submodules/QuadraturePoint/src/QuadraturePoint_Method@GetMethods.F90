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

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary:  Constructor methods for [[Quadraturepoints_]]

SUBMODULE(QuadraturePoint_Method) GetMethods
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = SIZE(obj%points, dims)
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                  getTotalQuadraturepoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturepoints
ans = SIZE(obj, 2)
END PROCEDURE obj_GetTotalQuadraturepoints

!----------------------------------------------------------------------------
!                                                         getQuadraturepoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints1
points = 0.0_DFP
points(1:obj%tXi) = obj%points(1:obj%tXi, Num)
weights = obj%points(obj%tXi + 1, Num)
END PROCEDURE obj_GetQuadraturePoints1

!----------------------------------------------------------------------------
!                                                         getQuadraturepoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints2
INTEGER(I4B) :: n
n = SIZE(obj%points, 2) !#column
CALL Reallocate(points, 3, n)
points(1:obj%tXi, 1:n) = obj%points(1:obj%tXi, 1:n)
weights = obj%points(obj%tXi + 1, 1:n)
END PROCEDURE obj_GetQuadraturePoints2

!----------------------------------------------------------------------------
!                                                         getQuadraturepoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints1_
nrow = 3
ncol = SIZE(obj%points, 2) !#column

! CALL Reallocate(points, 3, n)
points(1:obj%tXi, 1:ncol) = obj%points(1:obj%tXi, 1:ncol)
weights(1:ncol) = obj%points(obj%tXi + 1, 1:ncol)
END PROCEDURE obj_GetQuadraturePoints1_

!----------------------------------------------------------------------------
!                                                                Outerprod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Outerprod
INTEGER(I4B) :: n1, n2, n
INTEGER(I4B) :: ii, a, b

n1 = SIZE(obj1, 2)
n2 = SIZE(obj2, 2)
n = n1 * n2

CALL Reallocate(ans%points, 3, n)

DO ii = 1, n1
  a = (ii - 1) * n2 + 1
  b = ii * n2
  ans%points(1, a:b) = obj1%points(1, ii)
  ans%points(2, a:b) = obj2%points(1, :)
  ans%points(3, a:b) = obj1%points(2, ii) * obj2%points(2, :)
END DO

! CALL Initiate(obj=ans, points=points)
ans%tXi = SIZE(ans%points, 1) - 1
END PROCEDURE obj_Outerprod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
