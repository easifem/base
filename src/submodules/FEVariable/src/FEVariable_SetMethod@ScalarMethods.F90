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

SUBMODULE(FEVariable_SetMethod) ScalarMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
obj%len = 1
obj%s(1) = obj%len
IF (addContribution) THEN
  obj%val(1) = obj%val(1) + scale * val
ELSE
  obj%val(1) = scale * val
END IF
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
obj%len = SIZE(val)
obj%s(1) = obj%len
IF (addContribution) THEN
  obj%val(1:obj%len) = obj%val(1:obj%len) + scale * val(1:obj%len)
ELSE
  obj%val(1:obj%len) = scale * val(1:obj%len)
END IF
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
INTEGER(I4B) :: ii, jj, cnt

obj%s(1:2) = SHAPE(val)
obj%len = obj%s(1) * obj%s(2)

cnt = 0

IF (addContribution) THEN
  DO jj = 1, obj%s(2)
    DO ii = 1, obj%s(1)
      cnt = cnt + 1
      obj%val(cnt) = obj%val(cnt) + scale * val(ii, jj)
    END DO
  END DO
ELSE
  DO jj = 1, obj%s(2)
    DO ii = 1, obj%s(1)
      cnt = cnt + 1
      obj%val(cnt) = scale * val(ii, jj)
    END DO
  END DO
END IF
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
obj%len = SIZE(val)
obj%s(1) = obj%len
IF (addContribution) THEN
  obj%val(1:obj%len) = obj%val(1:obj%len) + scale * val(1:obj%len)
ELSE
  obj%val(1:obj%len) = scale * val(1:obj%len)
END IF
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ScalarMethods
