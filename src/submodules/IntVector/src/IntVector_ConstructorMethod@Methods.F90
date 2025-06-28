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
! summary: This submodule contains the contructor methods for [[IntVector_]]

SUBMODULE(IntVector_ConstructorMethod) Methods
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
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_Reallocate
IF (ALLOCATED(obj)) THEN
  IF (SIZE(obj) .NE. row) THEN
    DEALLOCATE (obj)
    ALLOCATE (obj(row))
  END IF
ELSE
  ALLOCATE (obj(row))
END IF
END PROCEDURE intVec_Reallocate

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
CALL ALLOCATE (obj, tSize)
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
  CALL ALLOCATE (obj(i), tSize(i))
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

MODULE PROCEDURE intVec_initiate4a
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate4a

MODULE PROCEDURE intVec_initiate4b
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate4b

MODULE PROCEDURE intVec_initiate4c
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate4c

MODULE PROCEDURE intVec_initiate4d
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate4d

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_initiate5a
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate5a

MODULE PROCEDURE intVec_initiate5b
obj%Val = Val
CALL setTotalDimension(obj, 1_I4B)
END PROCEDURE intVec_initiate5b

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Constructor1
CALL ALLOCATE (obj, tSize)
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
CALL ALLOCATE (obj, tSize)
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

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_assign_a
IF (ALLOCATED(obj%Val)) THEN
  Val = obj%Val
END IF
END PROCEDURE IntVec_assign_a

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_convert_int
IF (ALLOCATED(From%Val)) THEN
  To = From%Val
END IF
END PROCEDURE obj_convert_int

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy_Int8
INTEGER(I4B) :: tsize, ii
tsize = SIZE(y)
CALL Reallocate(x, tsize)
DO ii = 1, tsize
  x(ii) = y(ii)
END DO

END PROCEDURE obj_Copy_Int8

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy_Int16
INTEGER(I4B) :: tsize, ii
tsize = SIZE(y)
CALL Reallocate(x, tsize)
DO ii = 1, tsize
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy_Int16

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy_Int32
INTEGER(I4B) :: tsize, ii
tsize = SIZE(y)
CALL Reallocate(x, tsize)
DO ii = 1, tsize
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy_Int32

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy_Int64
INTEGER(I4B) :: tsize, ii
tsize = SIZE(y)
CALL Reallocate(x, tsize)
DO ii = 1, tsize
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy_Int64

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy1_
INTEGER(I4B) :: xx, yy

DO yy = y_start, y_end
  xx = x_start + yy - y_start
  x(xx) = y(yy)
END DO
END PROCEDURE obj_Copy1_

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy2_
INTEGER(I4B) :: tsize
tsize = SIZE(y)
CALL obj_Copy1_(x=x, y=y, x_start=1, y_start=1, y_end=tsize)
END PROCEDURE obj_Copy2_

END SUBMODULE Methods
