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

SUBMODULE(RealVector_ShallowCopyMethods) Methods
USE ReallocateUtility, ONLY: Reallocate
USE RealVector_ConstructorMethods, ONLY: Size

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy1a
CALL Reallocate(Y, SIZE(X))
END PROCEDURE obj_ShallowCopy1a

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy1b
CALL Reallocate(Y, SIZE(X))
END PROCEDURE obj_ShallowCopy1b

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy1c
CALL Reallocate(Y, SIZE(X))
END PROCEDURE obj_ShallowCopy1c

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy1d
CALL Reallocate(Y, SIZE(X))
END PROCEDURE obj_ShallowCopy1d

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy2
CALL ShallowCopy(Y=Y%Val, X=X%Val)
END PROCEDURE obj_ShallowCopy2

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy3
INTEGER(I4B) :: i
IF (ALLOCATED(Y)) THEN
  IF (SIZE(Y) .NE. SIZE(X)) THEN
    DEALLOCATE (Y)
    ALLOCATE (Y(SIZE(X)))
  END IF
ELSE
  ALLOCATE (Y(SIZE(X)))
END IF
DO i = 1, SIZE(Y)
  CALL ShallowCopy(Y=Y(i)%Val, X=X(i)%Val)
END DO
END PROCEDURE obj_ShallowCopy3

!----------------------------------------------------------------------------
!                                                               ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy4
INTEGER(I4B) :: i, tNodes
tNodes = 0
DO i = 1, SIZE(X)
  tNodes = tNodes + SIZE(X(i)%Val)
END DO
CALL Reallocate(Y%Val, tNodes)
END PROCEDURE obj_ShallowCopy4

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy5a
CALL ShallowCopy(Y=Y%Val, X=X)
END PROCEDURE obj_ShallowCopy5a

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy5b
CALL ShallowCopy(Y=Y%Val, X=X)
END PROCEDURE obj_ShallowCopy5b

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy6a
CALL ShallowCopy(Y=Y, X=X%Val)
END PROCEDURE obj_ShallowCopy6a

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy6b
CALL ShallowCopy(Y=Y, X=X%Val)
END PROCEDURE obj_ShallowCopy6b

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy7a
INTEGER(I4B) :: ii, m
m = 0
DO ii = 1, SIZE(X)
  m = m + SIZE(X(ii))
END DO
CALL Reallocate(Y, m)
END PROCEDURE obj_ShallowCopy7a

!----------------------------------------------------------------------------
!                                                                 ShallowCopy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ShallowCopy7b
INTEGER(I4B) :: ii, m
m = 0
DO ii = 1, SIZE(X)
  m = m + SIZE(X(ii))
END DO
CALL Reallocate(Y, m)
END PROCEDURE obj_ShallowCopy7b

END SUBMODULE Methods
