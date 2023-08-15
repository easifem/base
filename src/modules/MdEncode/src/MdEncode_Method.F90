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

MODULE MdEncode_Method
USE String_Class, ONLY: String
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: MdEncode
PUBLIC :: React_StartTabs
PUBLIC :: React_StartTabItem
PUBLIC :: React_EndTabs
PUBLIC :: React_EndTabItem

CHARACTER(3), PARAMETER :: avert = " | "
CHARACTER(2), PARAMETER :: ivert = "| "
CHARACTER(2), PARAMETER :: evert = " |"
CHARACTER(1), PARAMETER :: abr = CHAR_LF
CHARACTER(1), PARAMETER :: ablank = CHAR_BLANK
CHARACTER(5), PARAMETER :: adash = " --- "

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode_1(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_1
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode_2(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode_2
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode_3(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode_3
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode_4(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode_4
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode_5(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode_5
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode_6(val, rh, ch) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode_6
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode_7(val, rh, ch) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:, :)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode_7
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                 StartTabs
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION React_StartTabs() RESULT(ans)
    TYPE(String) :: ans
  END FUNCTION React_StartTabs
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 EndTabs
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION React_EndTabs() RESULT(ans)
    TYPE(String) :: ans
  END FUNCTION React_EndTabs
END INTERFACE

!----------------------------------------------------------------------------
!                                                             StartTabItem
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION React_StartTabItem(VALUE, label) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: VALUE
    CHARACTER(*), INTENT(IN) :: label
    TYPE(String) :: ans
  END FUNCTION React_StartTabItem
END INTERFACE

!----------------------------------------------------------------------------
!                                                             EndTabItem
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION React_EndTabItem() RESULT(ans)
    TYPE(String) :: ans
  END FUNCTION React_EndTabItem
END INTERFACE

END MODULE MdEncode_Method
