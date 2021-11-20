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

!> authors: Vikas Sharma, Ph. D.
! date:         25 Feb 2021
! summary:         This submodule contains the contructor methods for [[IntVector_]]

SUBMODULE(IntVector_Method) ConstructorMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_shape
IF (ALLOCATED(obj%Val)) THEN
  Ans(1) = SIZE(obj%Val)
ELSE
  Ans = 0
END IF
END PROCEDURE intVec_shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_Size
IF (ALLOCATED(obj%Val)) THEN
  Ans = SIZE(obj%Val)
ELSE
  Ans = 0
END IF
END PROCEDURE intVec_Size

!----------------------------------------------------------------------------
!                                                         getTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_getTotalDimension
ans = obj%tDimension
END PROCEDURE IntVec_getTotalDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_AllocateData
CALL Reallocate(obj%Val, Dims)
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_AllocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_Deallocate
IF (ALLOCATED(obj%Val)) DEALLOCATE (obj%Val)
END PROCEDURE intVec_Deallocate

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_initiate1
CALL AllocateData(obj, tSize)
END PROCEDURE intVec_initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_initiate2
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
  CALL AllocateData(obj(i), tSize(i))
END DO

END PROCEDURE intVec_initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_initiate3
IF (ALLOCATED(obj%Val)) DEALLOCATE (obj%Val)
ALLOCATE (obj%Val(a:b))
obj%Val = 0
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate3

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_initiate4
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate4

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_initiate5
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate5

!----------------------------------------------------------------------------
!                                                       intVec_isAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_isAllocated
IF (ALLOCATED(obj%Val)) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE intVec_isAllocated

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Constructor1
CALL AllocateData(obj, tSize)
END PROCEDURE IntVec_Constructor1

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Constructor2
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE IntVec_Constructor2

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Constructor3
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE IntVec_Constructor3

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Constructor_1
ALLOCATE (obj)
CALL AllocateData(obj, tSize)
END PROCEDURE IntVec_Constructor_1

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Constructor_2
ALLOCATE (obj)
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE IntVec_Constructor_2

!----------------------------------------------------------------------------
!                                                                      Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Constructor_3
ALLOCATE (obj)
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE IntVec_Constructor_3

END SUBMODULE ConstructorMethods
