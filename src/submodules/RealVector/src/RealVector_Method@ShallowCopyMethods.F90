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

SUBMODULE(RealVector_Method) ShallowCopyMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY1a
  CALL Reallocate( Y, SIZE( X ) )
END PROCEDURE realVec_SHALLOWCOPY1a
MODULE PROCEDURE realVec_SHALLOWCOPY1b
  CALL Reallocate( Y, SIZE( X ) )
END PROCEDURE realVec_SHALLOWCOPY1b
MODULE PROCEDURE realVec_SHALLOWCOPY1c
  CALL Reallocate( Y, SIZE( X ) )
END PROCEDURE realVec_SHALLOWCOPY1c
MODULE PROCEDURE realVec_SHALLOWCOPY1d
  CALL Reallocate( Y, SIZE( X ) )
END PROCEDURE realVec_SHALLOWCOPY1d

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY2
  CALL SHALLOWCOPY( Y=Y%Val, X=X%Val )
END PROCEDURE realVec_SHALLOWCOPY2

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY3
  INTEGER( I4B ) :: i
  IF( ALLOCATED( Y ) ) THEN
    IF( SIZE( Y ) .NE. SIZE( X ) ) THEN
      DEALLOCATE( Y )
      ALLOCATE( Y( SIZE( X ) ) )
    END IF
  ELSE
    ALLOCATE( Y( SIZE( X ) ) )
  END IF
  DO i = 1, SIZE( Y )
    CALL SHALLOWCOPY( Y=Y( i )%Val, X=X( i )%Val )
  END DO
END PROCEDURE realVec_SHALLOWCOPY3

!----------------------------------------------------------------------------
!                                                               SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY4
  INTEGER( I4B ) :: i, tNodes
  tNodes = 0
  DO i = 1, SIZE( X )
    tNodes = tNodes + SIZE( X( i )%Val )
  END DO
  CALL Reallocate( Y%Val, tNodes )
END PROCEDURE realVec_SHALLOWCOPY4

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY5a
  CALL SHALLOWCOPY( Y=Y%Val, X=X )
END PROCEDURE realVec_SHALLOWCOPY5a

MODULE PROCEDURE realVec_SHALLOWCOPY5b
  CALL SHALLOWCOPY( Y=Y%Val, X=X )
END PROCEDURE realVec_SHALLOWCOPY5b

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY6a
  CALL SHALLOWCOPY( Y=Y, X=X%Val )
END PROCEDURE realVec_SHALLOWCOPY6a
MODULE PROCEDURE realVec_SHALLOWCOPY6b
  CALL SHALLOWCOPY( Y=Y, X=X%Val )
END PROCEDURE realVec_SHALLOWCOPY6b

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY7a
  INTEGER( I4B ) :: ii, m
  m = 0
  DO ii = 1, SIZE( X )
    m = m + SIZE( X( ii ) )
  END DO
  CALL Reallocate( Y, m )
END PROCEDURE realVec_SHALLOWCOPY7a
MODULE PROCEDURE realVec_SHALLOWCOPY7b
  INTEGER( I4B ) :: ii, m
  m = 0
  DO ii = 1, SIZE( X )
    m = m + SIZE( X( ii ) )
  END DO
  CALL Reallocate( Y, m )
END PROCEDURE realVec_SHALLOWCOPY7b

END SUBMODULE ShallowCopyMethods