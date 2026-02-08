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

SUBMODULE(SystemEnquiry_Method) Methods
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_ASSOCIATED
USE System_Utility, ONLY: Arr2Str
USE System_Utility, ONLY: Str2_Carr
USE SystemInterface, ONLY: C_Access
USE SystemInterface, ONLY: C_Issock
USE SystemInterface, ONLY: C_Isfifo
USE SystemInterface, ONLY: C_Ischr
USE SystemInterface, ONLY: C_Isreg
USE SystemInterface, ONLY: C_Islnk
USE SystemInterface, ONLY: C_Isblk
USE SystemInterface, ONLY: C_Isdir

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              System_Access
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Access
LOGICAL :: isok

isok = C_Access(str2_carr(TRIM(pathname)), INT(amode, kind=C_INT)) .EQ. 0
IF (isok) THEN
  system_access = .TRUE.
ELSE
  system_access = .FALSE.
END IF
END PROCEDURE System_Access

!----------------------------------------------------------------------------
!                                                             System_Issock
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Issock
LOGICAL :: isok

isok = C_Issock(Str2_Carr(TRIM(pathname))) .EQ. 1

IF (isok) THEN
  System_Issock = .TRUE.
ELSE
  System_Issock = .FALSE.
END IF
END PROCEDURE System_Issock

!----------------------------------------------------------------------------
!                                                               System_Isfifo
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Isfifo
LOGICAL :: isok

isok = C_Isfifo(Str2_Carr(TRIM(pathname))) .EQ. 1

IF (isok) THEN
  System_Isfifo = .TRUE.
ELSE
  System_Isfifo = .FALSE.
END IF

END PROCEDURE System_Isfifo

!----------------------------------------------------------------------------
!                                                               System_Ischr
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Ischr
LOGICAL :: isok

isok = C_Ischr(Str2_Carr(TRIM(pathname))) .EQ. 1
IF (isok) THEN
  System_Ischr = .TRUE.
ELSE
  System_Ischr = .FALSE.
END IF
END PROCEDURE System_Ischr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Isreg
LOGICAL :: isok

isok = C_Isreg(Str2_Carr(TRIM(pathname))) .EQ. 1
IF (isok) THEN
  System_Isreg = .TRUE.
ELSE
  System_Isreg = .FALSE.
END IF
END PROCEDURE System_Isreg

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Islnk
LOGICAL :: isok

isok = C_Islnk(Str2_Carr(TRIM(pathname))) .EQ. 1
IF (isok) THEN
  System_Islnk = .TRUE.
ELSE
  System_Islnk = .FALSE.
END IF
END PROCEDURE System_Islnk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Isblk
LOGICAL :: isok

isok = C_Isblk(Str2_Carr(TRIM(pathname))) .EQ. 1
IF (isok) THEN
  System_Isblk = .TRUE.
ELSE
  System_Isblk = .FALSE.
END IF
END PROCEDURE System_Isblk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Isdir
LOGICAL :: isok

isok = C_Isdir(Str2_Carr(TRIM(dirname))) .EQ. 1

IF (isok) THEN
  System_Isdir = .TRUE.
ELSE
  System_Isdir = .FALSE.
END IF
END PROCEDURE System_Isdir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
