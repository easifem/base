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

MODULE IntVector_IOMethod
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: IntVector_
PRIVATE
PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         25 Feb 2021
! summary: Display the content of [[IntVector_]]

INTERFACE Display
  MODULE SUBROUTINE intVec_Display1(obj, msg, UnitNo, orient)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
    CHARACTER(*), OPTIONAL, INTENT(IN) :: orient
  END SUBROUTINE intVec_Display1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         25 Feb 2021
! summary: Display the content of [[IntVector_]]

INTERFACE Display
  MODULE SUBROUTINE intVec_Display2(obj, msg, UnitNo, orient)
    CLASS(IntVector_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
    CHARACTER(*), OPTIONAL, INTENT(IN) :: orient
  END SUBROUTINE intVec_Display2
END INTERFACE Display

END MODULE IntVector_IOMethod
