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

SUBMODULE(SystemProcess_Method) Methods
USE ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT
USE ISO_C_BINDING, ONLY: C_LONG_LONG
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_ASSOCIATED
USE System_Utility, ONLY: Anyinteger_to_64bit
USE System_Utility, ONLY: Arr2Str
USE System_Utility, ONLY: C2F_String
USE System_Utility, ONLY: Str2_Carr
USE SystemInterface, ONLY: C_CPU_Time
USE SystemInterface, ONLY: System_Umask
USE SystemInterface, ONLY: C_Perror
USE SystemInterface, ONLY: C_Flush
USE SystemInterface, ONLY: C_Uname
USE SystemInterface, ONLY: C_Gethostname
USE SystemInterface, ONLY: C_Getlogin
USE SystemInterface, ONLY: C_Perm
USE SystemInterface, ONLY: C_Getgrgid
USE SystemInterface, ONLY: C_Getpwuid
USE SystemInterface, ONLY: C_Stat
USE SystemInterface, ONLY: System_Geteuid

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            system_cpu_time
!----------------------------------------------------------------------------

MODULE PROCEDURE System_CPU_Time
REAL(C_FLOAT) :: C_User, C_System, C_Total

CALL C_CPU_Time(C_Total, C_User, C_System)
user = C_User
system = C_System
total = C_Total
END PROCEDURE System_CPU_Time

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Getumask
INTEGER :: idum
INTEGER(C_INT) :: Old_Umask

Old_Umask = System_Umask(0_C_INT)
! get current umask but by setting umask to 0
!  (a conservative mask so no vulnerability is open)
idum = System_Umask(Old_Umask)
! set back to original mask
Umask_Value = Old_Umask
END PROCEDURE System_Getumask

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Perror
INTEGER :: ios
FLUSH (unit=ERROR_UNIT, iostat=ios)
FLUSH (unit=OUTPUT_UNIT, iostat=ios)
FLUSH (unit=INPUT_UNIT, iostat=ios)
CALL C_Perror(Str2_Carr((TRIM(prefix))))
CALL C_Flush()
END PROCEDURE System_Perror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Uname
NAMEOUT = 'unknown'
CALL C_Uname(WHICH, NAMEOUT, INT(LEN(NAMEOUT), KIND(0_C_INT)))
END PROCEDURE System_Uname

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Gethostname
CHARACTER(kind=C_CHAR, len=1) :: C_BUFF(HOST_NAME_MAX + 1)
C_BUFF = ' '
ierr = C_Gethostname(C_BUFF, HOST_NAME_MAX)
! Host names are limited to {HOST_NAME_MAX} bytes.
NAME = TRIM(arr2str(C_BUFF))
END PROCEDURE System_Gethostname

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Getlogin
TYPE(C_PTR) :: username
username = C_Getlogin()
IF (.NOT. C_ASSOCIATED(username)) THEN
  ! In windows 10 subsystem running Ubunto does not work
  !write(*,'(a)')'*System_Getlogin* Error getting username. not associated'
  !fname=C_null_Char
  fname = System_Getpwuid(System_Geteuid())
ELSE
  fname = C2f_String(username)
END IF
END PROCEDURE System_Getlogin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Perm
TYPE(C_PTR) :: permissions
INTEGER(C_LONG) :: Mode_Local

Mode_Local = INT(Anyinteger_to_64bit(mode), kind=C_LONG)
permissions = C_Perm(Mode_Local)
IF (.NOT. C_ASSOCIATED(permissions)) THEN
  WRITE (*, '(a)') '*System_Perm* Error getting permissions. not associated'
  perms = C_NULL_CHAR
ELSE
  perms = C2f_String(permissions)
END IF
END PROCEDURE System_Perm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Getgrgid
CHARACTER(kind=C_CHAR, len=1) :: groupname(4097)
! assumed long enough for any groupname
INTEGER :: ierr
INTEGER(C_LONG_LONG) :: Gid_Local

Gid_Local = Anyinteger_to_64bit(gid)
ierr = C_Getgrgid(Gid_Local, groupname)
IF (ierr .EQ. 0) THEN
  gname = TRIM(arr2str(groupname))
ELSE
  gname = ''
END IF
END PROCEDURE System_Getgrgid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Getpwuid
CHARACTER(kind=C_CHAR, len=1) :: username(4097)
! assumed long enough for any username
INTEGER :: ierr
INTEGER(kind=C_LONG_LONG) :: Uid_Local

Uid_Local = Anyinteger_to_64bit(uid)
ierr = C_Getpwuid(Uid_Local, username)
IF (ierr .EQ. 0) THEN
  uname = TRIM(arr2str(username))
ELSE
  uname = ''
END IF
END PROCEDURE System_Getpwuid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Stat
INTEGER(kind=C_LONG) :: cvalues(13)
INTEGER(kind=C_INT) :: cierr

CALL C_Stat(Str2_Carr(TRIM(pathname)), cvalues, cierr, 0_C_INT)
values = cvalues
IF (PRESENT(ierr)) THEN
  ierr = cierr
END IF
END PROCEDURE System_Stat

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

END SUBMODULE Methods
