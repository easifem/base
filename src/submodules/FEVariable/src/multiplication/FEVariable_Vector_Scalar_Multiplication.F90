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

MODULE FEVariable_Vector_Scalar_Multiplication
USE BaseType, ONLY: FEVariable_

IMPLICIT NONE

PRIVATE
PUBLIC :: Vector_Scalar_Master

CONTAINS

!----------------------------------------------------------------------------
!                                                       Scalar_Scalar_Master
!----------------------------------------------------------------------------

PURE SUBROUTINE Vector_Scalar_Master(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans
END SUBROUTINE Vector_Scalar_Master

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_Vector_Scalar_Multiplication
