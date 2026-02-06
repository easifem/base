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

SUBMODULE(System_Method) GetMethods
USE ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            system_cpu_time
!----------------------------------------------------------------------------

MODULE PROCEDURE system_cpu_time
REAL(kind=C_FLOAT) :: c_user, c_system, c_total
INTERFACE
  SUBROUTINE c_cpu_time(c_total, c_user, c_system) BIND(C, NAME='my_cpu_time')
    IMPORT :: C_FLOAT
    REAL(kind=C_FLOAT) :: c_total, c_user, c_system
  END SUBROUTINE c_cpu_time
END INTERFACE

CALL c_cpu_time(c_total, c_user, c_system)
user = c_user
system = c_system
total = c_total
END PROCEDURE system_cpu_time

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_link
INTEGER(kind=C_INT) :: c_ierr

INTERFACE
  FUNCTION c_link(c_oldname, c_newname) BIND(C, name="link") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: c_oldname(*)
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: c_newname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_link
END INTERFACE

c_ierr = c_link(str2_carr(TRIM(oldname)), str2_carr(TRIM(newname)))
ierr = c_ierr
END PROCEDURE system_link

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_unlink
INTERFACE
  FUNCTION c_unlink(c_fname) BIND(C, name="unlink") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1) :: c_fname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_unlink
END INTERFACE
ierr = c_unlink(str2_carr(TRIM(fname)))
END PROCEDURE system_unlink

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_setumask
INTEGER(kind=C_INT) :: umask_c
umask_c = umask_value
old_umask = system_umask(umask_c) ! set current umask
END PROCEDURE system_setumask

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_getumask
INTEGER :: idum
INTEGER(kind=C_INT) :: old_umask

old_umask = system_umask(0_C_INT)
! get current umask but by setting umask to 0
!  (a conservative mask so no vulnerability is open)
idum = system_umask(old_umask)
! set back to original mask
umask_value = old_umask
END PROCEDURE system_getumask

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_perror
INTEGER :: ios

INTERFACE
  SUBROUTINE c_perror(c_prefix) BIND(C, name="perror")
    IMPORT C_CHAR
    CHARACTER(kind=C_CHAR) :: c_prefix(*)
  END SUBROUTINE c_perror
END INTERFACE

FLUSH (unit=ERROR_UNIT, iostat=ios)
FLUSH (unit=OUTPUT_UNIT, iostat=ios)
FLUSH (unit=INPUT_UNIT, iostat=ios)
CALL c_perror(str2_carr((TRIM(prefix))))
CALL c_flush()
END PROCEDURE system_perror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_uname
! describe the C routine to Fortran
! void system_uname(char *which, char *buf, int *buflen);
INTERFACE
  SUBROUTINE system_uname_c(WHICH, BUF, BUFLEN) BIND(C, NAME='my_uname')
    IMPORT C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(KIND=C_CHAR), INTENT(in) :: WHICH
    CHARACTER(KIND=C_CHAR), INTENT(out) :: BUF(*)
    INTEGER(kind=C_INT), INTENT(in) :: BUFLEN
  END SUBROUTINE system_uname_c
END INTERFACE

NAMEOUT = 'unknown'
CALL system_uname_c(WHICH, NAMEOUT, INT(LEN(NAMEOUT), KIND(0_C_INT)))
END PROCEDURE system_uname

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_gethostname
CHARACTER(kind=C_CHAR, len=1) :: C_BUFF(HOST_NAME_MAX + 1)

! describe the C routine to Fortran
!int gethostname(char *name, size_t namelen);
INTERFACE
  FUNCTION system_gethostname_c(c_buf, c_buflen) BIND(C, NAME='gethostname')
    IMPORT C_CHAR, C_INT
    IMPLICIT NONE
    INTEGER(kind=C_INT) :: system_gethostname_c
    CHARACTER(KIND=C_CHAR), INTENT(out) :: c_buf(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: c_buflen
  END FUNCTION system_gethostname_c
END INTERFACE

C_BUFF = ' '
ierr = system_gethostname_c(C_BUFF, HOST_NAME_MAX) ! Host names are limited to {HOST_NAME_MAX} bytes.
NAME = TRIM(arr2str(C_BUFF))
END PROCEDURE system_gethostname

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_getlogin
TYPE(C_PTR) :: username

INTERFACE
  FUNCTION c_getlogin() BIND(c, name="getlogin") RESULT(c_username)
    IMPORT C_INT, C_PTR
    TYPE(C_PTR) :: c_username
  END FUNCTION c_getlogin
END INTERFACE

username = c_getlogin()
IF (.NOT. C_ASSOCIATED(username)) THEN
  ! In windows 10 subsystem running Ubunto does not work
  !write(*,'(a)')'*system_getlogin* Error getting username. not associated'
  !fname=c_null_char
  fname = system_getpwuid(system_geteuid())
ELSE
  fname = c2f_string(username)
END IF
END PROCEDURE system_getlogin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_perm
TYPE(C_PTR) :: permissions
INTEGER(kind=C_LONG) :: mode_local

INTERFACE
  FUNCTION c_perm(c_mode) BIND(c, name="my_get_perm") RESULT(c_permissions)
    IMPORT C_INT, C_PTR, C_LONG
    INTEGER(kind=C_LONG), VALUE :: c_mode
    TYPE(C_PTR) :: c_permissions
  END FUNCTION c_perm
END INTERFACE

mode_local = INT(anyinteger_to_64bit(mode), kind=C_LONG)
permissions = c_perm(mode_local)
IF (.NOT. C_ASSOCIATED(permissions)) THEN
  WRITE (*, '(a)') '*system_perm* Error getting permissions. not associated'
  perms = C_NULL_CHAR
ELSE
  perms = c2f_string(permissions)
END IF
END PROCEDURE system_perm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_getgrgid
CHARACTER(kind=C_CHAR, len=1) :: groupname(4097)
! assumed long enough for any groupname
INTEGER :: ierr
INTEGER(kind=C_LONG_LONG) :: gid_local

INTERFACE
   function c_getgrgid(c_gid,c_groupname) bind(c,name="my_getgrgid") result(c_ierr)
    IMPORT C_INT, C_PTR, C_CHAR, C_LONG_LONG
    INTEGER(kind=C_LONG_LONG), VALUE, INTENT(in) :: c_gid
    CHARACTER(kind=C_CHAR), INTENT(out) :: c_groupname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_getgrgid
END INTERFACE

gid_local = anyinteger_to_64bit(gid)
ierr = c_getgrgid(gid_local, groupname)
IF (ierr .EQ. 0) THEN
  gname = TRIM(arr2str(groupname))
ELSE
  gname = ''
END IF
END PROCEDURE system_getgrgid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_getpwuid
CHARACTER(kind=C_CHAR, len=1) :: username(4097)
! assumed long enough for any username
INTEGER :: ierr
INTEGER(kind=C_LONG_LONG) :: uid_local

INTERFACE
   function c_getpwuid(c_uid,c_username) bind(c,name="my_getpwuid") result(c_ierr)
    IMPORT C_INT, C_PTR, C_CHAR, C_LONG_LONG
    INTEGER(kind=C_LONG_LONG), VALUE, INTENT(in) :: c_uid
    CHARACTER(kind=C_CHAR), INTENT(out) :: c_username(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION c_getpwuid
END INTERFACE

uid_local = anyinteger_to_64bit(uid)
ierr = c_getpwuid(uid_local, username)
IF (ierr .EQ. 0) THEN
  uname = TRIM(arr2str(username))
ELSE
  uname = ''
END IF
END PROCEDURE system_getpwuid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_stat
INTEGER(kind=C_LONG) :: cvalues(13)
INTEGER(kind=C_INT) :: cierr

INTERFACE
  SUBROUTINE c_stat(buffer, cvalues, cierr, cdebug) BIND(c, name="my_stat")
    IMPORT C_CHAR, C_SIZE_T, C_PTR, C_INT, C_LONG
    CHARACTER(kind=C_CHAR), INTENT(in) :: buffer(*)
    INTEGER(kind=C_LONG), INTENT(out) :: cvalues(*)
    INTEGER(kind=C_INT) :: cierr
    INTEGER(kind=C_INT), INTENT(in) :: cdebug
  END SUBROUTINE c_stat
END INTERFACE

CALL c_stat(str2_carr(TRIM(pathname)), cvalues, cierr, 0_C_INT)
values = cvalues
IF (PRESENT(ierr)) THEN
  ierr = cierr
END IF
END PROCEDURE system_stat

END SUBMODULE GetMethods
