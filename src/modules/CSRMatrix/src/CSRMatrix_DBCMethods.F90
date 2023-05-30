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

MODULE CSRMatrix_DBCMethods
USE BaseType, ONLY: CSRMatrix_
USE GlobalData, ONLY: I4B
IMPLICIT NONE
PRIVATE
PUBLIC :: ApplyDBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE csrMat_ApplyDBC(obj, dbcptrs)
    CLASS(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dbcptrs(:)
  END SUBROUTINE csrMat_ApplyDBC
END INTERFACE

INTERFACE ApplyDBC
  MODULE PROCEDURE csrMat_ApplyDBC
END INTERFACE ApplyDBC

END MODULE CSRMatrix_DBCMethods
