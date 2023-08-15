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

MODULE PROCEDURE MdEncode_1

SELECT TYPE (val)
TYPE IS (REAL(REAL32))
  ans = TOSTRING(val)
TYPE IS (REAL(REAL64))
  ans = TOSTRING(val)
TYPE IS (INTEGER(INT8))
  ans = TOSTRING(val)
TYPE IS (INTEGER(INT16))
  ans = TOSTRING(val)
TYPE IS (INTEGER(INT32))
  ans = TOSTRING(val)
TYPE IS (INTEGER(INT64))
  ans = TOSTRING(val)
TYPE IS (CHARACTER(LEN=*))
  ans = TRIM(val)
TYPE IS (String)
  ans = TRIM(val)
CLASS IS (QuadraturePoint_)
  ans = QuadraturePoint_MdEncode(val)
END SELECT

END PROCEDURE MdEncode_1
!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_2
INTEGER(I4B) :: ii, n

n = SIZE(val)
ans = "| "
DO ii = 1, n
  ans = ans//" | "
END DO
ans = ans//CHAR_LF

ans = ans//" | "
DO ii = 1, n
  ans = ans//" --- | "
END DO
ans = ans//CHAR_LF

SELECT TYPE (val)
TYPE IS (REAL(REAL32))
#include "./inc/MdEncode_2.inc"
TYPE IS (REAL(REAL64))
#include "./inc/MdEncode_2.inc"
TYPE IS (INTEGER(INT8))
#include "./inc/MdEncode_2.inc"
TYPE IS (INTEGER(INT16))
#include "./inc/MdEncode_2.inc"
TYPE IS (INTEGER(INT32))
#include "./inc/MdEncode_2.inc"
TYPE IS (INTEGER(INT64))
#include "./inc/MdEncode_2.inc"
TYPE IS (CHARACTER(LEN=*))
  ans = ans//" | "
  DO ii = 1, n
    ans = ans//TRIM(val(ii))//" | "
  END DO
  ans = ans//CHAR_LF
TYPE IS (String)
  ans = ans//" | "
  DO ii = 1, n
    ans = ans//TRIM(val(ii))//" | "
  END DO
  ans = ans//CHAR_LF
END SELECT

END PROCEDURE MdEncode_2

!----------------------------------------------------------------------------
!                                                   MdEncode_Method@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_3
INTEGER(I4B) :: ii, jj, m, n

m = SIZE(val, 1)
n = SIZE(val, 2)
ans = "| "
DO ii = 1, n
  ans = ans//" | "
END DO
ans = ans//CHAR_LF

ans = ans//" | "
DO ii = 1, n
  ans = ans//" --- | "
END DO
ans = ans//CHAR_LF

SELECT TYPE (val)
TYPE IS (REAL(REAL32))
#include "./inc/MdEncode_3.inc"
TYPE IS (REAL(REAL64))
#include "./inc/MdEncode_3.inc"
TYPE IS (INTEGER(INT8))
#include "./inc/MdEncode_3.inc"
TYPE IS (INTEGER(INT16))
#include "./inc/MdEncode_3.inc"
TYPE IS (INTEGER(INT32))
#include "./inc/MdEncode_3.inc"
TYPE IS (INTEGER(INT64))
#include "./inc/MdEncode_3.inc"
TYPE IS (CHARACTER(LEN=*))
#include "./inc/MdEncode_3b.inc"
TYPE IS (String)
#include "./inc/MdEncode_3b.inc"
END SELECT

END PROCEDURE MdEncode_3

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_4
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val, 3)
  ans = ans//"( :, :,  "//tostring(ii)//" ) = "//CHAR_LF//CHAR_LF &
      & //MdEncode(val(:, :, ii))
END DO
END PROCEDURE MdEncode_4

!----------------------------------------------------------------------------
!                                                                   MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_5
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(val, 4)
  DO ii = 1, SIZE(val, 3)
    ans = ans//"( :, :,  "//tostring(ii)//", "//tostring(jj)//" ) = " &
        & //CHAR_LF//CHAR_LF//MdEncode(val(:, :, ii, jj))
  END DO
END DO
END PROCEDURE MdEncode_5

!----------------------------------------------------------------------------
!                                                               Mdencode
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_6
INTEGER(I4B) :: nc, nr, n, ii
nc = SIZE(ch)
nr = SIZE(rh)
n = SIZE(val)

SELECT CASE (nc)
CASE (1)

  IF (nr .EQ. n) THEN

    ans = ivert//avert//ch(1)%chars()//evert//abr
    ans = ans// &
        & ivert//adash//avert//adash//evert//abr

    DO ii = 1, n
      ans = ans// &
          & ivert//rh(ii)%chars()//avert//mdencode(val(ii))//evert//abr
    END DO
    ans = ans//abr

  ELSE

    IF (ch(1)%LEN_TRIM() .EQ. 0_I4B) THEN

      ans = ivert//avert

      DO ii = 1, n
        ans = ans//ablank//evert
      END DO
      ans = ans//abr

      ans = ans//ivert//adash//evert
      DO ii = 1, n
        ans = ans//adash//evert
      END DO
      ans = ans//abr

      ans = ans//ivert//rh(1)%chars()//evert

      DO ii = 1, n
        ans = ans//mdencode(val(ii))//evert
      END DO
      ans = ans//abr
    ELSE

      ans = ivert//ch(1)%chars()//evert//abr
      ans = ans// &
          & ivert//adash//evert//abr

      DO ii = 1, n
        ans = ans// &
            & ivert//mdencode(val(ii))//evert//abr
      END DO
      ans = ans//abr
    END IF

  END IF

CASE default

  IF (nc .EQ. n) THEN

    ans = ivert//avert

    DO ii = 1, n
      ans = ans//ch(ii)%chars()//evert
    END DO
    ans = ans//abr

    ans = ans//ivert//adash//evert
    DO ii = 1, n
      ans = ans//adash//evert
    END DO
    ans = ans//abr

    ans = ans//ivert//rh(1)%chars()//evert

    DO ii = 1, n
      ans = ans//mdencode(val(ii))//evert
    END DO
    ans = ans//abr

  ELSE

    ans = ivert//avert

    DO ii = 1, n
      ans = ans//ablank//evert
    END DO
    ans = ans//abr

    ans = ans//ivert//adash//evert
    DO ii = 1, n
      ans = ans//adash//evert
    END DO
    ans = ans//abr

    ans = ans//ivert//rh(1)%chars()//evert

    DO ii = 1, n
      ans = ans//mdencode(val(ii))//evert
    END DO
    ans = ans//abr

  END IF

END SELECT

END PROCEDURE MdEncode_6

!----------------------------------------------------------------------------
!                                                           MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE MdEncode_7
INTEGER(I4B) :: nc, nr, n, ii, m, jj
LOGICAL(LGT) :: norow, nocol
nc = SIZE(ch)
nr = SIZE(rh)
m = SIZE(val, 1)
n = SIZE(val, 2)

IF (m .EQ. 1) THEN
  ans = MdEncode(val=val(1, :), rh=rh, ch=ch)
  RETURN
END IF

IF (n .EQ. 1) THEN
  ans = MdEncode(val=val(:, 1), rh=rh, ch=ch)
  RETURN
END IF

IF (nc .LT. n) THEN
  nocol = .TRUE.
ELSE
  nocol = .FALSE.
END IF

IF (nr .LT. m) THEN
  norow = .TRUE.
ELSE
  norow = .FALSE.
END IF

IF (nocol .AND. norow) THEN
  ans = MdEncode(val)
  RETURN
END IF

IF (norow .AND. (.NOT. nocol)) THEN
  ! | col-1 | col-2 | col-3 |
  ! | ---- | ----- | ----- |
  ! |  1  |   2   |     3 |

  ans = ivert

  DO ii = 1, n
    ans = ans//ch(ii)%chars()//avert
  END DO
  ans = ans//abr

  ans = ans//ivert
  DO ii = 1, n
    ans = ans//adash//avert
  END DO
  ans = ans//abr

  ans = ans//ivert

  DO ii = 1, m
    DO jj = 1, n
      ans = ans//mdencode(val(ii, jj))//avert
    END DO
    ans = ans//abr
  END DO
  ans = ans//abr
  RETURN
END IF

IF (nocol .AND. (.NOT. norow)) THEN
  ! |       |      |       |       |
  ! | ----- | ---- | ----- | ----- |
  ! | row-1 |  1  |   2   |     3  |
  ! | row-2 |  1  |   2   |     3  |
  ! | row-3 |  1  |   2   |     3 |

  ans = ivert//avert
  DO ii = 1, n
    ans = ans//ablank//avert
  END DO
  ans = ans//abr

  ans = ans//ivert//adash//avert
  DO ii = 1, n
    ans = ans//adash//avert
  END DO
  ans = ans//abr

  DO ii = 1, m
    ans = ans//ivert//rh(ii)%chars()//avert
    DO jj = 1, n
      ans = ans//mdencode(val(ii, jj))//avert
    END DO
    ans = ans//abr
  END DO
  ans = ans//abr
  RETURN
END IF

! |     | col-1 | col-2 | col-3 |
! | ----- | ---- | ----- | ----- |
! | row-1 |  1  |   2   |     3  |
! | row-2 |  1  |   2   |     3  |
! | row-3 |  1  |   2   |     3 |

ans = ivert//avert
DO ii = 1, n
  ans = ans//ch(ii)%chars()//avert
END DO
ans = ans//abr

ans = ans//ivert//adash//avert
DO ii = 1, n
  ans = ans//adash//avert
END DO
ans = ans//abr

DO ii = 1, m
  ans = ans//ivert//rh(ii)%chars()//avert
  DO jj = 1, n
    ans = ans//mdencode(val(ii, jj))//avert
  END DO
  ans = ans//abr
END DO
ans = ans//abr

END PROCEDURE MdEncode_7

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
