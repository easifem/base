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
USE IntVector_SetMethod, ONLY: SetTotalDimension
USE ReallocateUtility, ONLY: Util_Reallocate => Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_shape
LOGICAL(LGT) :: isok

ans = 0
isok = ALLOCATED(obj%val)
IF (isok) ans(1) = SIZE(obj%val)
END PROCEDURE obj_shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
LOGICAL(LGT) :: isok

ans = 0
isok = ALLOCATED(obj%val)
IF (isok) ans = SIZE(obj%val)
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                         getTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getTotalDimension
ans = obj%tDimension
END PROCEDURE obj_getTotalDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateData
CALL Util_Reallocate(obj%val, dims)
CALL SetTotalDimension(obj, 1_I4B)
END PROCEDURE obj_AllocateData

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Reallocate
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

isok = ALLOCATED(obj)
IF (.NOT. isok) THEN
  ALLOCATE (obj(row))
  RETURN
END IF

tsize = SIZE(obj)
isok = tsize .NE. row
IF (isok) THEN
  DEALLOCATE (obj)
  ALLOCATE (obj(row))
END IF
END PROCEDURE obj_Reallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
LOGICAL(LGT) :: isok
obj%tDimension = 0_I4B
isok = ALLOCATED(obj%val)
IF (isok) DEALLOCATE (obj%val)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate1
CALL obj_AllocateData(obj=obj, dims=tSize)
END PROCEDURE obj_initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate2
INTEGER(I4B) :: n, i
LOGICAL(LGT) :: isok

n = SIZE(tSize)
isok = ALLOCATED(obj)

IF (.NOT. isok) THEN
  ALLOCATE (obj(n))
  DO i = 1, n
    CALL obj_AllocateData(obj=obj(i), dims=tSize(i))
  END DO
  RETURN
END IF

i = SIZE(obj)
isok = i .NE. n
IF (isok) THEN
  DEALLOCATE (obj)
  ALLOCATE (obj(n))
END IF

DO i = 1, n
  CALL obj_AllocateData(obj=obj(i), dims=tSize(i))
END DO
END PROCEDURE obj_initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate3
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)
IF (isok) DEALLOCATE (obj%val)
ALLOCATE (obj%val(a:b))
obj%val(a:b) = 0
CALL SetTotalDimension(obj, 1_I4B)
END PROCEDURE obj_initiate3

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate4a
#include "./include/Initiate4.F90"
END PROCEDURE obj_initiate4a

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate4b
#include "./include/Initiate4.F90"
END PROCEDURE obj_initiate4b

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate4c
#include "./include/Initiate4.F90"
END PROCEDURE obj_initiate4c

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate4d
#include "./include/Initiate4.F90"
END PROCEDURE obj_initiate4d

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate5a
#include "./include/Initiate4.F90"
END PROCEDURE obj_initiate5a

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate5b
#include "./include/Initiate4.F90"
END PROCEDURE obj_initiate5b

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate6
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

obj%tDimension = obj2%tDimension
isok = ALLOCATED(obj2%val)
IF (isok) THEN
  tsize = SIZE(obj2%val)
  CALL Util_Reallocate(obj%val, tsize)
  CALL Copy_(x=obj%val, y=obj2%val)
END IF

END PROCEDURE obj_Initiate6

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL obj_AllocateData(obj=obj, dims=tSize)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor2
CALL Initiate(obj=obj, val=val)
END PROCEDURE obj_Constructor2

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor3
CALL Initiate(obj=obj, val=val)
END PROCEDURE obj_Constructor3

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (obj)
CALL Initiate(obj=obj, tsize=tsize)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_2
ALLOCATE (obj)
CALL Initiate(obj=obj, val=val)
END PROCEDURE obj_Constructor_2

!----------------------------------------------------------------------------
!                                                                      Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_3
ALLOCATE (obj)
CALL Initiate(obj=obj, val=val)
END PROCEDURE obj_Constructor_3

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign_a
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

isok = ALLOCATED(obj%val)
IF (.NOT. isok) RETURN

tsize = SIZE(obj%val)
CALL Util_Reallocate(val, tsize)
CALL Copy_(x=val, y=obj%val)
END PROCEDURE obj_assign_a

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_convert_int
CALL obj_assign_a(val=to, obj=from)
END PROCEDURE obj_convert_int

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy_Int8
INTEGER(I4B) :: tsize, ii

tsize = SIZE(y)
CALL Util_Reallocate(x, tsize)

DO CONCURRENT(ii=1:tsize)
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy_Int8

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy_Int16
INTEGER(I4B) :: tsize, ii

tsize = SIZE(y)
CALL Util_Reallocate(x, tsize)

DO CONCURRENT(ii=1:tsize)
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy_Int16

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy_Int32
INTEGER(I4B) :: tsize, ii

tsize = SIZE(y)
CALL Util_Reallocate(x, tsize)

DO CONCURRENT(ii=1:tsize)
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy_Int32

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy_Int64
INTEGER(I4B) :: tsize, ii

tsize = SIZE(y)
CALL Util_Reallocate(x, tsize)

DO CONCURRENT(ii=1:tsize)
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy_Int64

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy1_
INTEGER(I4B) :: yy

DO CONCURRENT(yy=y_start:y_end)
  x(x_start + yy - y_start) = y(yy)
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
