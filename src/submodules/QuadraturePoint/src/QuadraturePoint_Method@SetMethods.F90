! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(QuadraturePoint_Method) SetMethods
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(points, 1)
ncol = SIZE(points, 2)

CALL Reallocate(obj%points, nrow, ncol)

obj%points(1:nrow, 1:ncol) = points
obj%tXi = nrow - 1
END PROCEDURE obj_Set1

END SUBMODULE SetMethods
