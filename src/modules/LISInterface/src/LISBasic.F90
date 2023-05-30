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

MODULE LISBasic
USE GlobalData, ONLY: I4B
IMPLICIT NONE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE

  SUBROUTINE lis_init_f(ierr)
    IMPORT :: I4B
    INTEGER(I4B), INTENT(IN) :: ierr
  END SUBROUTINE lis_init_f
END INTERFACE

PUBLIC :: lis_initialize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_finalize(void);

INTERFACE
  FUNCTION lis_finalize() &
    & BIND(C, name="lis_finalize")
    IMPORT :: I4B
    INTEGER(I4B) :: lis_finalize
  END FUNCTION lis_finalize
END INTERFACE

PUBLIC :: lis_finalize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION lis_initialize()
  INTEGER(I4B) :: lis_initialize
  CALL lis_init_f(lis_initialize)
END FUNCTION lis_initialize

END MODULE LISBasic
