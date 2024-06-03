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

MODULE RealVector_IOMethods
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: RealVector_

IMPLICIT NONE

PRIVATE

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

INTERFACE Display
  MODULE SUBROUTINE obj_display1(obj, msg, unitno)
    CLASS(RealVector_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_display1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE Display
  MODULE SUBROUTINE obj_display2(obj, msg, unitno)
    CLASS(RealVector_), INTENT(IN) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE obj_display2
END INTERFACE Display

END MODULE RealVector_IOMethods
