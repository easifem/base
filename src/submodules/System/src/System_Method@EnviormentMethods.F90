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
!
SUBMODULE(System_Method) EnvironmentMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_putenv
INTERFACE
  INTEGER(kind=C_INT) FUNCTION c_putenv(c_string) BIND(C, name="putenv")
    IMPORT C_INT, C_CHAR
    CHARACTER(kind=C_CHAR) :: c_string(*)
  END FUNCTION
END INTERFACE

INTEGER :: loc_err
INTEGER :: i

! PUTENV actually adds the data to the environment so the string passed should be saved or will vanish on exit
CHARACTER(len=1, kind=C_CHAR), SAVE, POINTER :: memleak(:)

ALLOCATE (memleak(LEN(string) + 1))
DO i = 1, LEN(string)
  memleak(i) = string(i:i)
END DO
memleak(LEN(string) + 1) = C_NULL_CHAR

loc_err = c_putenv(memleak)
IF (PRESENT(err)) err = loc_err
END PROCEDURE system_putenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_getenv
INTEGER :: howbig
INTEGER :: stat

IF (NAME .NE. '') THEN
      call get_environment_variable(name, length=howbig, status=stat, trim_name=.true.)  ! get length required to hold value
  IF (howbig .NE. 0) THEN
    SELECT CASE (stat)
    CASE (1)
      ! print *, NAME, " is not defined in the environment. Strange..."
      VALUE = ''
    CASE (2)
      ! print *, "This processor doesn't support environment variables. Boooh!"
      VALUE = ''
    CASE default
      ! make string to hold value of sufficient size and get value
      IF (ALLOCATED(VALUE)) DEALLOCATE (VALUE)
      ALLOCATE (CHARACTER(len=MAX(howbig, 1)) :: VALUE)
     CALL GET_ENVIRONMENT_VARIABLE(name, VALUE, status=stat, trim_name=.TRUE.)
      IF (stat .NE. 0) VALUE = ''
    END SELECT
  ELSE
    VALUE = ''
  END IF
ELSE
  VALUE = ''
END IF
IF (VALUE .EQ. '' .AND. PRESENT(default)) VALUE = default
END PROCEDURE system_getenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE set_environment_variable
INTEGER :: loc_err

INTERFACE
 INTEGER(kind=C_INT) FUNCTION c_setenv(c_name, c_VALUE) BIND(C, NAME="setenv")
    IMPORT C_INT, C_CHAR
    CHARACTER(kind=C_CHAR) :: c_name(*)
    CHARACTER(kind=C_CHAR) :: c_VALUE(*)
  END FUNCTION
END INTERFACE

loc_err = c_setenv(str2_carr(TRIM(NAME)), str2_carr(VALUE))
IF (PRESENT(STATUS)) STATUS = loc_err

END PROCEDURE set_environment_variable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_clearenv
!  emulating because not available on some platforms
CHARACTER(len=:), ALLOCATABLE :: string
INTEGER :: ierr_local1, ierr_local2

ierr_local2 = 0

INFINITE: DO
  CALL system_initenv()
  ! important -- changing table causes undefined behavior
  !  so reset after each unsetenv
  string = system_readenv()
  ! get first name=value pair
  IF (string .EQ. '') EXIT INFINITE
  CALL system_unsetenv(string(1:INDEX(string, '=') - 1), ierr_local1) ! remove first name=value pair
  IF (ierr_local1 .NE. 0) ierr_local2 = ierr_local1
END DO INFINITE

IF (PRESENT(ierr)) THEN
  ierr = ierr_local2
ELSEIF (ierr_local2 .NE. 0) THEN
! if error occurs and not being returned, stop
  WRITE (*, *) '*system_clearenv* error=', ierr_local2
  STOP
END IF
END PROCEDURE system_clearenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_unsetenv
INTEGER :: ierr_local

INTERFACE
  INTEGER(kind=C_INT) FUNCTION c_unsetenv(c_name) BIND(C, NAME="unsetenv")
    IMPORT C_INT, C_CHAR
    CHARACTER(len=1, kind=C_CHAR) :: c_name(*)
  END FUNCTION
END INTERFACE

ierr_local = c_unsetenv(str2_carr(TRIM(NAME)))

IF (PRESENT(ierr)) THEN
  ierr = ierr_local
ELSEIF (ierr_local .NE. 0) THEN ! if error occurs and not being returned, stop
  WRITE (*, *) '*system_unsetenv* error=', ierr_local
  STOP
END IF

END PROCEDURE system_unsetenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_readenv
CHARACTER(kind=C_CHAR) :: c_buff(longest_env_variable + 1)

INTERFACE
  SUBROUTINE c_readenv(c_string) BIND(C, NAME='my_readenv')
    IMPORT C_CHAR, C_INT, C_PTR, C_SIZE_T
    CHARACTER(kind=C_CHAR), INTENT(out) :: c_string(*)
  END SUBROUTINE c_readenv
END INTERFACE

c_buff = ' '
c_buff(longest_env_variable + 1:longest_env_variable + 1) = C_NULL_CHAR
CALL c_readenv(c_buff)
string = TRIM(arr2str(c_buff))
END PROCEDURE system_readenv

END SUBMODULE EnvironmentMethods
