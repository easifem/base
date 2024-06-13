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

SUBMODULE(FEVariable_Method) EqualMethods
USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_isequal
!! Internal variable
ans = .FALSE.
IF (obj1%len .NE. obj2%len) RETURN
IF (obj1%defineon .NE. obj2%defineon) RETURN
IF (obj1%rank .NE. obj2%rank) RETURN
IF (obj1%varType .NE. obj2%varType) RETURN
IF (ANY(obj1%s .NE. obj2%s)) RETURN

IF (ALL(obj1%val.APPROXEQ.obj2%val)) ans = .TRUE.
!!
END PROCEDURE fevar_isequal

!----------------------------------------------------------------------------
!                                                             NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_notEqual
ans = .FALSE.
IF (.NOT. ALL(obj1%val.APPROXEQ.obj2%val)) THEN
  ans = .TRUE.
  RETURN
END IF

IF (obj1%defineon .NE. obj2%defineon) THEN
  ans = .TRUE.
  RETURN
END IF

IF (obj1%rank .NE. obj2%rank) THEN
  ans = .TRUE.
  RETURN
END IF

IF (obj1%varType .NE. obj2%varType) THEN
  ans = .TRUE.
  RETURN
END IF

IF (ANY(obj1%s .NE. obj2%s)) THEN
  ans = .TRUE.
  RETURN
END IF

END PROCEDURE fevar_notEqual

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE EqualMethods
