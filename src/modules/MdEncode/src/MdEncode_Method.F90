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
  MODULE FUNCTION MdEncode_Int8(val) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_Int8

  MODULE FUNCTION MdEncode_Int16(val) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_Int16

  MODULE FUNCTION MdEncode_Int32(val) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_Int32

  MODULE FUNCTION MdEncode_Int64(val) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_Int64

  MODULE FUNCTION MdEncode_Real32(val) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_Real32

  MODULE FUNCTION MdEncode_Real64(val) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_Real64

  MODULE FUNCTION MdEncode_Char(val) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_Char

  MODULE FUNCTION MdEncode_String(val) RESULT(ans)
    TYPE(String), INTENT(IN) :: val
    TYPE(String) :: ans
  END FUNCTION MdEncode_String
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode2_Int8(val) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode2_Int8

  MODULE FUNCTION MdEncode2_Int16(val) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode2_Int16

  MODULE FUNCTION MdEncode2_Int32(val) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode2_Int32

  MODULE FUNCTION MdEncode2_Int64(val) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode2_Int64

  MODULE FUNCTION MdEncode2_Real32(val) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode2_Real32

  MODULE FUNCTION MdEncode2_Real64(val) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode2_Real64

  MODULE FUNCTION MdEncode2_String(val) RESULT(ans)
    TYPE(String), INTENT(IN) :: val(:)
    TYPE(String) :: ans
  END FUNCTION MdEncode2_String
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode3_Int8(val) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode3_Int8

  MODULE FUNCTION MdEncode3_Int16(val) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode3_Int16

  MODULE FUNCTION MdEncode3_Int32(val) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode3_Int32

  MODULE FUNCTION MdEncode3_Int64(val) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode3_Int64

  MODULE FUNCTION MdEncode3_Real32(val) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode3_Real32

  MODULE FUNCTION MdEncode3_Real64(val) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode3_Real64

  MODULE FUNCTION MdEncode3_String(val) RESULT(ans)
    TYPE(String), INTENT(IN) :: val(:, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode3_String
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode4_Int8(val) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode4_Int8

  MODULE FUNCTION MdEncode4_Int16(val) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode4_Int16

  MODULE FUNCTION MdEncode4_Int32(val) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode4_Int32

  MODULE FUNCTION MdEncode4_Int64(val) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode4_Int64

  MODULE FUNCTION MdEncode4_Real32(val) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode4_Real32

  MODULE FUNCTION MdEncode4_Real64(val) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode4_Real64

  MODULE FUNCTION MdEncode4_String(val) RESULT(ans)
    TYPE(String), INTENT(IN) :: val(:, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode4_String
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode5_Int8(val) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode5_Int8

  MODULE FUNCTION MdEncode5_Int16(val) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode5_Int16

  MODULE FUNCTION MdEncode5_Int32(val) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode5_Int32

  MODULE FUNCTION MdEncode5_Int64(val) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode5_Int64

  MODULE FUNCTION MdEncode5_Real32(val) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode5_Real32

  MODULE FUNCTION MdEncode5_Real64(val) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode5_Real64

  MODULE FUNCTION MdEncode5_String(val) RESULT(ans)
    TYPE(String), INTENT(IN) :: val(:, :, :, :)
    TYPE(String) :: ans
  END FUNCTION MdEncode5_String
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode6_Int8(val, rh, ch) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: val(:)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode6_Int8

  MODULE FUNCTION MdEncode6_Int16(val, rh, ch) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: val(:)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode6_Int16

  MODULE FUNCTION MdEncode6_Int32(val, rh, ch) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: val(:)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode6_Int32

  MODULE FUNCTION MdEncode6_Int64(val, rh, ch) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: val(:)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode6_Int64

  MODULE FUNCTION MdEncode6_Real32(val, rh, ch) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: val(:)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode6_Real32

  MODULE FUNCTION MdEncode6_Real64(val, rh, ch) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: val(:)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode6_Real64

  MODULE FUNCTION MdEncode6_String(val, rh, ch) RESULT(ans)
    TYPE(String), INTENT(IN) :: val(:)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode6_String
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

INTERFACE MdEncode
  MODULE FUNCTION MdEncode7_Int8(val, rh, ch) RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: val(:, :)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode7_Int8

  MODULE FUNCTION MdEncode7_Int16(val, rh, ch) RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: val(:, :)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode7_Int16

  MODULE FUNCTION MdEncode7_Int32(val, rh, ch) RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: val(:, :)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode7_Int32

  MODULE FUNCTION MdEncode7_Int64(val, rh, ch) RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: val(:, :)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode7_Int64

  MODULE FUNCTION MdEncode7_Real32(val, rh, ch) RESULT(ans)
    REAL(Real32) , INTENT(IN) :: val(:, :)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode7_Real32

  MODULE FUNCTION MdEncode7_Real64(val, rh, ch) RESULT(ans)
    REAL(Real64) , INTENT(IN) :: val(:, :)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode7_Real64

  MODULE FUNCTION MdEncode7_String(val, rh, ch) RESULT(ans)
    TYPE(String) , INTENT(IN) :: val(:, :)
    TYPE(String), INTENT(IN) :: rh(:)
    !! Row header
    TYPE(String), INTENT(IN) :: ch(:)
    !! Col header
    TYPE(String) :: ans
  END FUNCTION MdEncode7_String

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
