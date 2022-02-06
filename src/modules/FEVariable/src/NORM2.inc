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

PUBLIC :: NORM2

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-12
! update: 2021-12-12
! summary: FEVariable = NORM2(FEVariable)

INTERFACE
  MODULE PURE FUNCTION fevar_norm2(obj) RESULT(Ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    TYPE(FEVariable_) :: ans
  END FUNCTION fevar_norm2
END INTERFACE

INTERFACE NORM2
  MODULE PROCEDURE fevar_norm2
END INTERFACE NORM2
