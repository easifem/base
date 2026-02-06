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

END SUBMODULE EnvironmentMethods
