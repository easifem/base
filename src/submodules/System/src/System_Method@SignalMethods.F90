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

SUBMODULE(System_Method) SignalMethods
USE ISO_C_BINDING, ONLY: C_FUNLOC

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               System_Signal
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Signal
TYPE(C_FUNPTR) :: ret, c_handler

IF (PRESENT(handler_routine)) THEN
  handler_ptr_array(signum)%sub => handler_routine
ELSE
  handler_ptr_array(signum)%sub => NULL()
END IF

c_handler = C_FUNLOC(f_handler)
ret = C_Signal(signum, c_handler)
END PROCEDURE System_Signal

END SUBMODULE SignalMethods
