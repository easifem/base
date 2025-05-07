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

MODULE ElemshapeData_IOMethods
USE BaseType, ONLY: ElemshapeData_, STElemShapeData_

USE GlobalData, ONLY: I4B, DFP, LGT

USE String_Class, ONLY: String

IMPLICIT NONE
PRIVATE

PUBLIC :: Display
PUBLIC :: ElemshapeData_MdEncode
PUBLIC :: MdEncode
PUBLIC :: ElemshapeData_ReactEncode

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Display the content of [[elemshapedata_]] and [[stelemshapedata_]]

INTERFACE Display
  MODULE SUBROUTINE elemsd_display_1(obj, msg, unitNo)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
  END SUBROUTINE elemsd_display_1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                          ElemshapeData_MdEncode@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Display the content of [[elemshapedata_]] and [[stelemshapedata_]]

INTERFACE MdEncode
  MODULE FUNCTION ElemshapeData_MdEncode(obj) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION ElemshapeData_MdEncode
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                       ElemshapeData_ReactEncode@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Display the content of [[elemshapedata_]] and [[stelemshapedata_]]

INTERFACE
  MODULE FUNCTION ElemshapeData_ReactEncode(obj) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION ElemshapeData_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Display the content of [[elemshapedata_]] and [[stelemshapedata_]]

INTERFACE Display
  MODULE SUBROUTINE elemsd_display_2(obj, msg, unitNo)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
  END SUBROUTINE elemsd_display_2
END INTERFACE Display

END MODULE ElemshapeData_IOMethods
