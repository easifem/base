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

SUBMODULE(System_Method) EnquiryMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              System_Access
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Access
IF (C_Access(str2_carr(TRIM(pathname)), INT(amode, kind=C_INT)) .EQ. 0) THEN
  system_access = .TRUE.
ELSE
  system_access = .FALSE.
END IF
END PROCEDURE System_Access

!----------------------------------------------------------------------------
!                                                             System_Issock
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Issock
IF (c_issock(str2_carr(TRIM(pathname))) .EQ. 1) THEN
  system_issock = .TRUE.
ELSE
  system_issock = .FALSE.
END IF
END PROCEDURE System_Issock

!----------------------------------------------------------------------------
!                                                               System_Isfifo
!----------------------------------------------------------------------------

MODULE PROCEDURE system_isfifo

INTERFACE
  FUNCTION c_isfifo(pathname) BIND(C, name="my_isfifo") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_isfifo
END INTERFACE

IF (c_isfifo(str2_carr(TRIM(pathname))) .EQ. 1) THEN
  system_isfifo = .TRUE.
ELSE
  system_isfifo = .FALSE.
END IF

END PROCEDURE system_isfifo

!----------------------------------------------------------------------------
!                                                               System_Ischr
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Ischr
INTERFACE
  FUNCTION c_ischr(pathname) BIND(C, name="my_ischr") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_ischr
END INTERFACE

IF (c_ischr(str2_carr(TRIM(pathname))) .EQ. 1) THEN
  System_Ischr = .TRUE.
ELSE
  System_Ischr = .FALSE.
END IF
END PROCEDURE System_Ischr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_isreg
INTERFACE
  FUNCTION c_isreg(pathname) BIND(C, name="my_isreg") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_isreg
END INTERFACE

IF (c_isreg(str2_carr(TRIM(pathname))) .EQ. 1) THEN
  system_isreg = .TRUE.
ELSE
  system_isreg = .FALSE.
END IF
END PROCEDURE system_isreg

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Islnk
INTERFACE
  FUNCTION c_islnk(pathname) BIND(C, name="my_islnk") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_islnk
END INTERFACE

IF (c_islnk(str2_carr(TRIM(pathname))) .EQ. 1) THEN
  System_Islnk = .TRUE.
ELSE
  System_Islnk = .FALSE.
END IF
END PROCEDURE System_Islnk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Isblk
INTERFACE
  FUNCTION c_isblk(pathname) BIND(C, name="my_isblk") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_isblk
END INTERFACE

IF (c_isblk(str2_carr(TRIM(pathname))) .EQ. 1) THEN
  system_isblk = .TRUE.
ELSE
  system_isblk = .FALSE.
END IF
END PROCEDURE System_Isblk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Isdir
INTERFACE
  FUNCTION c_isdir(dirname) BIND(C, name="my_isdir") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: dirname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_isdir
END INTERFACE

IF (c_isdir(str2_carr(TRIM(dirname))) .EQ. 1) THEN
  System_Isdir = .TRUE.
ELSE
  System_Isdir = .FALSE.
END IF
END PROCEDURE System_Isdir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_getcwd
INTEGER(kind=C_LONG), PARAMETER :: length = 4097_C_LONG
CHARACTER(kind=C_CHAR, len=1) :: buffer(length)
TYPE(C_PTR) :: buffer2
INTERFACE
  FUNCTION c_getcwd(buffer, size) BIND(c, name="getcwd") RESULT(buffer_result)
    IMPORT C_CHAR, C_SIZE_T, C_PTR
    CHARACTER(kind=C_CHAR), INTENT(out) :: buffer(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(in) :: size
    TYPE(C_PTR) :: buffer_result
  END FUNCTION
END INTERFACE

buffer = ' '
buffer2 = c_getcwd(buffer, length)
IF (.NOT. C_ASSOCIATED(buffer2)) THEN
  output = ''
  ierr = -1
ELSE
  output = TRIM(arr2str(buffer))
  ierr = 0
END IF
END PROCEDURE system_getcwd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE EnquiryMethods
