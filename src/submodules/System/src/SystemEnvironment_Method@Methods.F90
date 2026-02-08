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
SUBMODULE(SystemEnvironment_Method) Methods
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_CHAR
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE System_Utility, ONLY: Arr2Str
USE System_Utility, ONLY: Str2_Carr
USE SystemInterface, ONLY: C_Setenv
USE SystemInterface, ONLY: C_Unsetenv
USE SystemInterface, ONLY: C_Readenv
USE SystemInterface, ONLY: C_Putenv
USE SystemInterface, ONLY: System_Initenv
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Putenv
INTEGER :: Loc_Err
INTEGER :: i
! PUTENV actually adds the data to the environment so the string passed
!  should be saved or will vanish on exit
CHARACTER(len=1, kind=C_CHAR), SAVE, POINTER :: memleak(:)

ALLOCATE (memleak(LEN(string) + 1))
DO i = 1, LEN(string)
  memleak(i) = string(i:i)
END DO
memleak(LEN(string) + 1) = C_NULL_CHAR

Loc_Err = C_Putenv(memleak)
IF (PRESENT(err)) err = Loc_Err
END PROCEDURE System_Putenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Getenv
INTEGER :: howbig
INTEGER :: stat

IF (NAME .NE. '') THEN
  CALL GET_ENVIRONMENT_VARIABLE(name, length=howbig, status=stat, &
                                Trim_Name=.TRUE.)
  ! get length required to hold value
  IF (howbig .NE. 0) THEN
    SELECT CASE (stat)
    CASE (1)
      ! print *, NAME, " is not defined in the environment. Strange..."
      VALUE = ''
    CASE (2)
      ! print *, "This processor doesn't support environment variables.
      ! Boooh!"
      VALUE = ''
    CASE DEFAULT
      ! make string to hold value of sufficient size and get value
      IF (ALLOCATED(VALUE)) DEALLOCATE (VALUE)
      ALLOCATE (CHARACTER(len=MAX(howbig, 1)) :: VALUE)
      CALL GET_ENVIRONMENT_VARIABLE(name, VALUE, &
                                    status=stat, trim_name=.TRUE.)
      IF (stat .NE. 0) VALUE = ''
    END SELECT
  ELSE
    VALUE = ''
  END IF
ELSE
  VALUE = ''
END IF
IF (VALUE .EQ. '' .AND. PRESENT(default)) VALUE = default
END PROCEDURE System_Getenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Set_Environment_Variable
INTEGER :: loc_err
loc_err = C_Setenv(Str2_Carr(TRIM(NAME)), Str2_Carr(VALUE))
IF (PRESENT(STATUS)) STATUS = Loc_Err
END PROCEDURE Set_Environment_Variable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Clearenv
!  emulating because not available on some platforms
CHARACTER(len=:), ALLOCATABLE :: string
INTEGER :: ierr_local1, ierr_local2

ierr_local2 = 0

INFINITE: DO
  CALL System_Initenv()
  ! important -- changing table causes undefined behavior
  !  so reset after each unsetenv
  string = System_Readenv()
  ! get first name=value pair
  IF (string .EQ. '') EXIT INFINITE
  CALL System_Unsetenv(string(1:INDEX(string, '=') - 1), Ierr_Local1)
  ! remove first name=value pair
  IF (Ierr_Local1 .NE. 0) Ierr_Local2 = Ierr_Local1
END DO INFINITE

IF (PRESENT(ierr)) THEN
  ierr = Ierr_Local2
ELSEIF (Ierr_Local2 .NE. 0) THEN
  ! if error occurs and not being returned, stop
  WRITE (*, *) '*System_Clearenv* error=', Ierr_Local2
  STOP
END IF
END PROCEDURE System_Clearenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Unsetenv
INTEGER :: ierr_local

ierr_local = C_Unsetenv(Str2_Carr(TRIM(NAME)))

IF (PRESENT(ierr)) THEN
  ierr = Ierr_Local
ELSEIF (Ierr_Local .NE. 0) THEN
  ! if error occurs and not being returned, stop
  WRITE (*, *) '*System_Unsetenv* error=', Ierr_Local
  STOP
END IF
END PROCEDURE System_Unsetenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Readenv
CHARACTER(kind=C_CHAR) :: C_Buff(LONGEST_ENV_VARIABLE + 1)

C_Buff = ' '
C_Buff(Longest_env_Variable + 1:Longest_env_Variable + 1) = C_NULL_CHAR
CALL C_Readenv(C_Buff)
string = TRIM(arr2str(C_Buff))
END PROCEDURE System_Readenv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
