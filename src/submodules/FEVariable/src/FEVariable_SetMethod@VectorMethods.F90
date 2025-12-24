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

SUBMODULE(FEVariable_SetMethod) VectorMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
obj%len = SIZE(val)
obj%s(1) = SIZE(val)
IF (addContribution) THEN
  obj%val(1:obj%len) = obj%val(1:obj%len) + scale * val(1:obj%len)
ELSE
  obj%val(1:obj%len) = scale * val(1:obj%len)
END IF
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
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
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
INTEGER(I4B) :: ii, jj, kk, cnt

obj%s(1:3) = SHAPE(val)
obj%len = obj%s(1) * obj%s(2) * obj%s(3)

cnt = 0
IF (addContribution) THEN
  DO kk = 1, obj%s(3)
    DO jj = 1, obj%s(2)
      DO ii = 1, obj%s(1)
        cnt = cnt + 1
        obj%val(cnt) = obj%val(cnt) + scale * val(ii, jj, kk)
      END DO
    END DO
  END DO
ELSE
  DO kk = 1, obj%s(3)
    DO jj = 1, obj%s(2)
      DO ii = 1, obj%s(1)
        cnt = cnt + 1
        obj%val(cnt) = scale * val(ii, jj, kk)
      END DO
    END DO
  END DO
END IF
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
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
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE VectorMethods
