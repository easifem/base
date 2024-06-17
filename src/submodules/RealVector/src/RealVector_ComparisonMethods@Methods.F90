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

SUBMODULE(RealVector_ComparisonMethods) Methods
USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 isEqual
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isEqual
IF (.NOT. ALLOCATED(obj%val)) THEN
  ans = .FALSE.
  RETURN
END IF

IF (.NOT. ALLOCATED(obj2%val)) THEN
  ans = .FALSE.
  RETURN
END IF

IF (SIZE(obj%val) .NE. SIZE(obj2%val)) THEN
  ans = .FALSE.
  RETURN
END IF

IF (ALL(obj%val.APPROXEQ.obj2%val)) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_isEqual

END SUBMODULE Methods
