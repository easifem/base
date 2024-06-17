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

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This module contains constructor methods of [[RealVector_]]

SUBMODULE(RealVector_ConstructorMethods) Methods
USE SafeSizeUtility, ONLY: SafeSize

USE F95_BLAS, ONLY: COPY

USE DOF_Method, ONLY: OPERATOR(.tnodes.), &
                      OPERATOR(.tDOF.)

USE ReallocateUtility, ONLY: Util_Reallocate => Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               isAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isAllocated
ans = ALLOCATED(obj%val)
END PROCEDURE obj_isAllocated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape
ans(1) = SafeSize(obj%val)
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = SafeSize(obj%val)
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                         getTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE RealVec_getTotalDimension
ans = obj%tDimension
END PROCEDURE RealVec_getTotalDimension

!----------------------------------------------------------------------------
!                                                        setTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE RealVec_setTotalDimension
obj%tDimension = tDimension
END PROCEDURE RealVec_setTotalDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Allocate
CALL Util_Reallocate(obj%val, Dims)
CALL SetTotalDimension(obj, 1_I4B)
END PROCEDURE obj_Allocate

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Reallocate
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj)

IF (.NOT. isok) THEN
  ALLOCATE (obj(row))
  RETURN
END IF

isok = SIZE(obj) .NE. row
IF (isok) THEN
  DEALLOCATE (obj)
  ALLOCATE (obj(row))
END IF

END PROCEDURE obj_Reallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
IF (ALLOCATED(obj%val)) DEALLOCATE (obj%val)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CALL ALLOCATE (obj, tSize)
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
INTEGER(I4B) :: n, i
n = SIZE(tSize)

IF (ALLOCATED(obj)) THEN
  IF (SIZE(obj) .NE. n) THEN
    DEALLOCATE (obj)
    ALLOCATE (obj(n))
  END IF
ELSE
  ALLOCATE (obj(n))
END IF

DO i = 1, n
  CALL ALLOCATE (obj(i), tSize(i))
END DO
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
IF (ALLOCATED(obj%val)) DEALLOCATE (obj%val)
ALLOCATE (obj%val(a:b))
obj%val = 0.0_DFP
CALL SetTotalDimension(obj, 1_I4B)
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
CALL Initiate(obj=obj, tSize=(.tNodes.dofobj))
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5
INTEGER(I4B) :: ii
INTEGER(I4B), ALLOCATABLE :: tsize(:)
ASSOCIATE (Map => dofobj%Map)
  ALLOCATE (tsize(.tDOF.dofobj))
  DO ii = 1, SIZE(Map, 1) - 1
    tsize(Map(ii, 5):Map(ii + 1, 5) - 1) = Map(ii, 6)
  END DO
  CALL Initiate(obj=obj, tsize=tsize)
  DEALLOCATE (tsize)
END ASSOCIATE
END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Random_Number1
CALL Initiate(obj=obj, tSize=tSize)
CALL RANDOM_NUMBER(obj%val)
END PROCEDURE obj_Random_Number1

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Random_Number2
INTEGER(I4B) :: ii, n
n = SIZE(tSize)
IF (ALLOCATED(obj)) THEN
  IF (SIZE(obj) .NE. n) THEN
    DEALLOCATE (obj)
    ALLOCATE (obj(n))
  END IF
ELSE
  ALLOCATE (obj(n))
END IF
DO ii = 1, n
  CALL Initiate(obj=obj(ii), tSize=tSize(ii))
  CALL RANDOM_NUMBER(obj(ii)%val)
END DO
END PROCEDURE obj_Random_Number2

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL ALLOCATE (obj, tSize)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor2
CALL ALLOCATE (obj, SIZE(val))
CALL COPY(Y=obj%val, X=REAL(val, DFP))
END PROCEDURE obj_Constructor2

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor3
CALL ALLOCATE (obj, SIZE(val))
CALL COPY(Y=obj%val, X=val)
END PROCEDURE obj_Constructor3

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (obj)
CALL ALLOCATE (obj, tSize)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_2
ALLOCATE (obj)
CALL ALLOCATE (obj, SIZE(val))
CALL COPY(Y=obj%val, X=REAL(val, DFP))
END PROCEDURE obj_Constructor_2

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_3
ALLOCATE (obj)
CALL ALLOCATE (obj, SIZE(val))
CALL COPY(Y=obj%val, X=REAL(val, DFP))
END PROCEDURE obj_Constructor_3

END SUBMODULE Methods
