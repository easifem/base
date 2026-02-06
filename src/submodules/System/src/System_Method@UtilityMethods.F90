! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(System_Method) UtilityMethods
USE ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                C2F_String
!----------------------------------------------------------------------------

MODULE PROCEDURE C2F_string
CHARACTER(kind=C_CHAR), DIMENSION(:), POINTER :: &
  char_array_pointer => NULL()
INTEGER, PARAMETER :: max_len = 4096
CHARACTER(len=max_len) :: aux_string
INTEGER :: i
INTEGER :: length

length = 0
CALL C_F_POINTER(c_string_pointer, char_array_pointer, [max_len])

IF (.NOT. ASSOCIATED(char_array_pointer)) THEN
  IF (ALLOCATED(f_string)) DEALLOCATE (f_string)
  ALLOCATE (CHARACTER(len=4) :: f_string)
  f_string = C_NULL_CHAR
  RETURN
END IF

aux_string = " "

DO i = 1, max_len
  IF (char_array_pointer(i) == C_NULL_CHAR) THEN
    length = i - 1; EXIT
  END IF
  aux_string(i:i) = char_array_pointer(i)
END DO

IF (ALLOCATED(f_string)) DEALLOCATE (f_string)
ALLOCATE (CHARACTER(len=length) :: f_string)
f_string = aux_string(1:length)
END PROCEDURE C2F_String

!----------------------------------------------------------------------------
!                                                                   Str2_Carr
!----------------------------------------------------------------------------

MODULE PROCEDURE Str2_Carr
INTEGER :: i
DO i = 1, LEN_TRIM(string)
  array(i) = string(i:i)
END DO
array(i:i) = C_NULL_CHAR
END PROCEDURE Str2_Carr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Timestamp
epoch = C_Time(INT(0, kind=8))
END PROCEDURE Timestamp

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Arr2Str
INTEGER :: i

string = ' '
DO i = 1, SIZE(array)
  IF (array(i) .EQ. CHAR(0)) THEN
    EXIT
  ELSE
    string(i:i) = array(i)
  END IF
END DO
END PROCEDURE Arr2Str

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matchw
CHARACTER(len=LEN(tame) + 1) :: tametext
CHARACTER(len=LEN(wild) + 1) :: wildtext
CHARACTER(len=1), PARAMETER :: NULL = CHAR(0)
INTEGER :: wlen, ti, wi, i
CHARACTER(len=:), ALLOCATABLE :: tbookmark, wbookmark

! These two values are set when we observe a wildcard character. They
! represent the locations, in the two strings, from which we start once
! we've observed it.
tametext = tame//NULL
wildtext = wild//NULL
tbookmark = NULL
wbookmark = NULL
wlen = LEN(wild)
wi = 1
ti = 1
DO
! Walk the text strings one character at a time.
  IF (wildtext(wi:wi) == '*') THEN
    ! How do you match a unique text string?
    DO i = wi, wlen
      ! Easy: unique up on it!
      IF (wildtext(wi:wi) .EQ. '*') THEN
        wi = wi + 1
      ELSE
        EXIT
      END IF
    END DO
    IF (wildtext(wi:wi) .EQ. NULL) THEN
      ! "x" matches "*"
      Matchw = .TRUE.
      RETURN
    END IF
    IF (wildtext(wi:wi) .NE. '?') THEN
      ! Fast-forward to next possible match.
      DO WHILE (tametext(ti:ti) .NE. wildtext(wi:wi))
        ti = ti + 1
        IF (tametext(ti:ti) .EQ. NULL) THEN
          Matchw = .FALSE.
          RETURN
          ! "x" doesn't match "*y*"
        END IF
      END DO
    END IF
    wbookmark = wildtext(wi:)
    tbookmark = tametext(ti:)
  ELSEIF ((tametext(ti:ti) .NE. wildtext(wi:wi)) &
          .AND. (wildtext(wi:wi) .NE. '?')) THEN
    ! Got a non-match. If we've set our bookmarks,
    !  back up to one or both of them and retry.
    IF (wbookmark .NE. NULL) THEN
      IF (wildtext(wi:) .NE. wbookmark) THEN
        wildtext = wbookmark
        wlen = LEN_TRIM(wbookmark)
        wi = 1
        ! Don't go this far back again.
        IF (tametext(ti:ti) .NE. wildtext(wi:wi)) THEN
          tbookmark = tbookmark(2:)
          tametext = tbookmark
          ti = 1
          CYCLE
          ! "xy" matches "*y"
        ELSE
          wi = wi + 1
        END IF
      END IF
      IF (tametext(ti:ti) .NE. NULL) THEN
        ti = ti + 1
        CYCLE
        ! "mississippi" matches "*sip*"
      END IF
    END IF
    Matchw = .FALSE.
    RETURN
    ! "xy" doesn't match "x"
  END IF
  ti = ti + 1
  wi = wi + 1
  IF (tametext(ti:ti) .EQ. NULL) THEN
    ! How do you match a tame text string?
    IF (wildtext(wi:wi) .NE. NULL) THEN
      DO WHILE (wildtext(wi:wi) == '*')
        ! The tame way: unique up on it!
        wi = wi + 1
        ! "x" matches "x*"
        IF (wildtext(wi:wi) .EQ. NULL) EXIT
      END DO
    END IF
    IF (wildtext(wi:wi) .EQ. NULL) THEN
      Matchw = .TRUE.
      RETURN
      ! "x" matches "x"
    END IF
    Matchw = .FALSE.
    RETURN
    ! "x" doesn't match "xy"
  END IF
END DO
END PROCEDURE Matchw

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Anyinteger_to_64bit
SELECT TYPE (intin)
TYPE IS (INTEGER(kind=INT8))
  ii38 = INT(intin, kind=INT64)
TYPE IS (INTEGER(kind=INT16))
  ii38 = INT(intin, kind=INT64)
TYPE IS (INTEGER(kind=INT32))
  ii38 = intin
TYPE IS (INTEGER(kind=INT64))
  ii38 = intin
  !class default
  !write(error_unit,*)'ERROR: unknown integer type'
  !stop 'ERROR: *Anyinteger_to_64* unknown integer type'
END SELECT
END PROCEDURE Anyinteger_to_64bit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE f_handler
LOGICAL :: isok

isok = ASSOCIATED(handler_ptr_array(signum)%sub)
IF (isok) THEN
  CALL handler_ptr_array(signum)%sub(signum)
END IF
END PROCEDURE f_handler

!----------------------------------------------------------------------------
!                                                            Include Error
!----------------------------------------------------------------------------

END SUBMODULE UtilityMethods
