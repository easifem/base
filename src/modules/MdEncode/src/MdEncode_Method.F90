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

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION MdEncode_1(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_1
END INTERFACE

INTERFACE MdEncode
  MODULE PROCEDURE MdEncode_1
END INTERFACE MdEncode

PUBLIC :: MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION MdEncode_2(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode_2
END INTERFACE

INTERFACE MdEncode
  MODULE PROCEDURE MdEncode_2
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION MdEncode_3(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode_3
END INTERFACE

INTERFACE MdEncode
  MODULE PROCEDURE MdEncode_3
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION MdEncode_4(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode_4
END INTERFACE

INTERFACE MdEncode
  MODULE PROCEDURE MdEncode_4
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION MdEncode_5(val) RESULT(ans)
    CLASS(*), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode_5
END INTERFACE

INTERFACE MdEncode
  MODULE PROCEDURE MdEncode_5
END INTERFACE MdEncode

END MODULE MdEncode_Method
