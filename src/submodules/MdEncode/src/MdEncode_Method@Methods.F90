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

SUBMODULE(MdEncode_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_Int8
ans = Tostring(val)
END PROCEDURE MdEncode_Int8

MODULE PROCEDURE MdEncode_Int16
ans = Tostring(val)
END PROCEDURE MdEncode_Int16

MODULE PROCEDURE MdEncode_Int32
ans = Tostring(val)
END PROCEDURE MdEncode_Int32

MODULE PROCEDURE MdEncode_Int64
ans = Tostring(val)
END PROCEDURE MdEncode_Int64

MODULE PROCEDURE MdEncode_Real32
ans = Tostring(val)
END PROCEDURE MdEncode_Real32

MODULE PROCEDURE MdEncode_Real64
ans = Tostring(val)
END PROCEDURE MdEncode_Real64

MODULE PROCEDURE MdEncode_Char
ans = TRIM(val)
END PROCEDURE MdEncode_Char

MODULE PROCEDURE MdEncode_String
ans = val%chars()
END PROCEDURE MdEncode_String

!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

! MODULE PROCEDURE MdEncode_2
! INTEGER(I4B) :: ii, n
!
! n = SIZE(val)
! ans = "| "
! DO ii = 1, n
!   ans = ans//" | "
! END DO
! ans = ans//CHAR_LF
!
! ans = ans//"| "
! DO ii = 1, n
!   ans = ans//" --- | "
! END DO
! ans = ans//CHAR_LF
!
! SELECT TYPE (val)
! TYPE IS (REAL(REAL32))
! #include "./inc/MdEncode_2.inc"
! TYPE IS (REAL(REAL64))
! #include "./inc/MdEncode_2.inc"
! TYPE IS (INTEGER(INT8))
! #include "./inc/MdEncode_2.inc"
! TYPE IS (INTEGER(INT16))
! #include "./inc/MdEncode_2.inc"
! TYPE IS (INTEGER(INT32))
! #include "./inc/MdEncode_2.inc"
! TYPE IS (INTEGER(INT64))
! #include "./inc/MdEncode_2.inc"
! TYPE IS (CHARACTER(LEN=*))
!   ans = ans//"| "
!   DO ii = 1, n
!     ans = ans//TRIM(val(ii))//" | "
!   END DO
!   ans = ans//CHAR_LF
! TYPE IS (String)
!   ans = ans//"| "
!   DO ii = 1, n
!     ans = ans//TRIM(val(ii))//" | "
!   END DO
!   ans = ans//CHAR_LF
! END SELECT
!
! END PROCEDURE MdEncode_2

!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode2_Int8
#include "./inc/MdEncode_2.inc"
END PROCEDURE MdEncode2_Int8

MODULE PROCEDURE MdEncode2_Int16
#include "./inc/MdEncode_2.inc"
END PROCEDURE MdEncode2_Int16

MODULE PROCEDURE MdEncode2_Int32
#include "./inc/MdEncode_2.inc"
END PROCEDURE MdEncode2_Int32

MODULE PROCEDURE MdEncode2_Int64
#include "./inc/MdEncode_2.inc"
END PROCEDURE MdEncode2_Int64

MODULE PROCEDURE MdEncode2_Real32
#include "./inc/MdEncode_2.inc"
END PROCEDURE MdEncode2_Real32

MODULE PROCEDURE MdEncode2_Real64
#include "./inc/MdEncode_2.inc"
END PROCEDURE MdEncode2_Real64

MODULE PROCEDURE MdEncode2_String
#include "./inc/MdEncode_2.inc"
END PROCEDURE MdEncode2_String

!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode3_Int8
#include "./inc/MdEncode_3.inc"
END PROCEDURE MdEncode3_Int8

MODULE PROCEDURE MdEncode3_Int16
#include "./inc/MdEncode_3.inc"
END PROCEDURE MdEncode3_Int16

MODULE PROCEDURE MdEncode3_Int32
#include "./inc/MdEncode_3.inc"
END PROCEDURE MdEncode3_Int32

MODULE PROCEDURE MdEncode3_Int64
#include "./inc/MdEncode_3.inc"
END PROCEDURE MdEncode3_Int64

MODULE PROCEDURE MdEncode3_Real32
#include "./inc/MdEncode_3.inc"
END PROCEDURE MdEncode3_Real32

MODULE PROCEDURE MdEncode3_Real64
#include "./inc/MdEncode_3.inc"
END PROCEDURE MdEncode3_Real64

MODULE PROCEDURE MdEncode3_String
#include "./inc/MdEncode_3.inc"
END PROCEDURE MdEncode3_String

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode4_Int8
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val, 3)
  ans = ans//"( :, :,  "//tostring(ii)//" ) = "//CHAR_LF2 &
      & //MdEncode(val(:, :, ii))
END DO
END PROCEDURE MdEncode4_Int8

MODULE PROCEDURE MdEncode4_Int16
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val, 3)
  ans = ans//"( :, :,  "//tostring(ii)//" ) = "//CHAR_LF2 &
      & //MdEncode(val(:, :, ii))
END DO
END PROCEDURE MdEncode4_Int16

MODULE PROCEDURE MdEncode4_Int32
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val, 3)
  ans = ans//"( :, :,  "//tostring(ii)//" ) = "//CHAR_LF2 &
      & //MdEncode(val(:, :, ii))
END DO
END PROCEDURE MdEncode4_Int32

MODULE PROCEDURE MdEncode4_Int64
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val, 3)
  ans = ans//"( :, :,  "//tostring(ii)//" ) = "//CHAR_LF2 &
      & //MdEncode(val(:, :, ii))
END DO
END PROCEDURE MdEncode4_Int64

MODULE PROCEDURE MdEncode4_Real32
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val, 3)
  ans = ans//"( :, :,  "//tostring(ii)//" ) = "//CHAR_LF2 &
      & //MdEncode(val(:, :, ii))
END DO
END PROCEDURE MdEncode4_Real32

MODULE PROCEDURE MdEncode4_Real64
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val, 3)
  ans = ans//"( :, :,  "//tostring(ii)//" ) = "//CHAR_LF2 &
      & //MdEncode(val(:, :, ii))
END DO
END PROCEDURE MdEncode4_Real64

MODULE PROCEDURE MdEncode4_String
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val, 3)
  ans = ans//"( :, :,  "//tostring(ii)//" ) = "//CHAR_LF2 &
      & //MdEncode(val(:, :, ii))
END DO
END PROCEDURE MdEncode4_String

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode5_Int8
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(val, 4)
  DO ii = 1, SIZE(val, 3)
    ans = ans//"( :, :,  "//tostring(ii)//", "//tostring(jj)//" ) = " &
        & //CHAR_LF//CHAR_LF//MdEncode(val(:, :, ii, jj))
  END DO
END DO
END PROCEDURE MdEncode5_Int8

MODULE PROCEDURE MdEncode5_Int16
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(val, 4)
  DO ii = 1, SIZE(val, 3)
    ans = ans//"( :, :,  "//tostring(ii)//", "//tostring(jj)//" ) = " &
        & //CHAR_LF//CHAR_LF//MdEncode(val(:, :, ii, jj))
  END DO
END DO
END PROCEDURE MdEncode5_Int16

MODULE PROCEDURE MdEncode5_Int32
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(val, 4)
  DO ii = 1, SIZE(val, 3)
    ans = ans//"( :, :,  "//tostring(ii)//", "//tostring(jj)//" ) = " &
        & //CHAR_LF//CHAR_LF//MdEncode(val(:, :, ii, jj))
  END DO
END DO
END PROCEDURE MdEncode5_Int32

MODULE PROCEDURE MdEncode5_Int64
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(val, 4)
  DO ii = 1, SIZE(val, 3)
    ans = ans//"( :, :,  "//tostring(ii)//", "//tostring(jj)//" ) = " &
        & //CHAR_LF//CHAR_LF//MdEncode(val(:, :, ii, jj))
  END DO
END DO
END PROCEDURE MdEncode5_Int64

MODULE PROCEDURE MdEncode5_Real32
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(val, 4)
  DO ii = 1, SIZE(val, 3)
    ans = ans//"( :, :,  "//tostring(ii)//", "//tostring(jj)//" ) = " &
        & //CHAR_LF//CHAR_LF//MdEncode(val(:, :, ii, jj))
  END DO
END DO
END PROCEDURE MdEncode5_Real32

MODULE PROCEDURE MdEncode5_Real64
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(val, 4)
  DO ii = 1, SIZE(val, 3)
    ans = ans//"( :, :,  "//tostring(ii)//", "//tostring(jj)//" ) = " &
        & //CHAR_LF//CHAR_LF//MdEncode(val(:, :, ii, jj))
  END DO
END DO
END PROCEDURE MdEncode5_Real64

MODULE PROCEDURE MdEncode5_String
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(val, 4)
  DO ii = 1, SIZE(val, 3)
    ans = ans//"( :, :,  "//tostring(ii)//", "//tostring(jj)//" ) = " &
        & //CHAR_LF//CHAR_LF//MdEncode(val(:, :, ii, jj))
  END DO
END DO
END PROCEDURE MdEncode5_String

!----------------------------------------------------------------------------
!                                                               Mdencode
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode6_Int8
#include "./inc/MdEncode_6.inc"
END PROCEDURE MdEncode6_Int8

MODULE PROCEDURE MdEncode6_Int16
#include "./inc/MdEncode_6.inc"
END PROCEDURE MdEncode6_Int16

MODULE PROCEDURE MdEncode6_Int32
#include "./inc/MdEncode_6.inc"
END PROCEDURE MdEncode6_Int32

MODULE PROCEDURE MdEncode6_Int64
#include "./inc/MdEncode_6.inc"
END PROCEDURE MdEncode6_Int64

MODULE PROCEDURE MdEncode6_Real32
#include "./inc/MdEncode_6.inc"
END PROCEDURE MdEncode6_Real32

MODULE PROCEDURE MdEncode6_Real64
#include "./inc/MdEncode_6.inc"
END PROCEDURE MdEncode6_Real64

MODULE PROCEDURE MdEncode6_String
#include "./inc/MdEncode_6.inc"
END PROCEDURE MdEncode6_String

!----------------------------------------------------------------------------
!                                                           MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode7_Int8
#include "./inc/MdEncode_7.inc"
END PROCEDURE MdEncode7_Int8

MODULE PROCEDURE MdEncode7_Int16
#include "./inc/MdEncode_7.inc"
END PROCEDURE MdEncode7_Int16

MODULE PROCEDURE MdEncode7_Int32
#include "./inc/MdEncode_7.inc"
END PROCEDURE MdEncode7_Int32

MODULE PROCEDURE MdEncode7_Int64
#include "./inc/MdEncode_7.inc"
END PROCEDURE MdEncode7_Int64

MODULE PROCEDURE MdEncode7_Real32
#include "./inc/MdEncode_7.inc"
END PROCEDURE MdEncode7_Real32

MODULE PROCEDURE MdEncode7_Real64
#include "./inc/MdEncode_7.inc"
END PROCEDURE MdEncode7_Real64

MODULE PROCEDURE MdEncode7_String
#include "./inc/MdEncode_7.inc"
END PROCEDURE MdEncode7_String

!----------------------------------------------------------------------------
!                                                           StartTab
!----------------------------------------------------------------------------

MODULE PROCEDURE React_StartTabs
ans = "<Tabs>"//char_lf
END PROCEDURE React_StartTabs

!----------------------------------------------------------------------------
!                                                           EndTabs
!----------------------------------------------------------------------------

MODULE PROCEDURE React_EndTabs
ans = "</Tabs>"//char_lf
END PROCEDURE React_EndTabs

!----------------------------------------------------------------------------
!                                                         StartTabItem
!----------------------------------------------------------------------------

MODULE PROCEDURE React_StartTabItem
ans = "<TabItem value="//'"'//TRIM(VALUE)//'"'  &
& //" label="//'"'//TRIM(label)//'" >'//char_lf
END PROCEDURE React_StartTabItem

!----------------------------------------------------------------------------
!                                                         StartTabItem
!----------------------------------------------------------------------------

MODULE PROCEDURE React_EndTabItem
ans = "</TabItem>"//char_lf
END PROCEDURE React_EndTabItem

END SUBMODULE Methods
