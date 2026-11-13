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
USE BaseType, ONLY: math => TypeMathOpt
USE IntVector_SetMethod, ONLY: SetTotalDimension
USE ReallocateUtility, ONLY: Util_Reallocate => Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "IntVector_ConstructorMethod@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape
ans(1) = obj%tsize
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = obj%tsize
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                   Capacity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Capacity
ans = obj%capacity
END PROCEDURE obj_Capacity

!----------------------------------------------------------------------------
!                                                         GetTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDimension
ans = obj%tDimension
END PROCEDURE obj_GetTotalDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateData
CALL Util_Reallocate(obj%val, dims)
obj%tsize = dims
obj%capacity = dims
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
obj%tsize = 0_I4B
obj%capacity = 0_I4B
isok = ALLOCATED(obj%val)
IF (isok) DEALLOCATE (obj%val)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CALL obj_AllocateData(obj=obj, dims=tSize)
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
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
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4a
#include "./include/Initiate4.F90"
END PROCEDURE obj_Initiate4a

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4b
#include "./include/Initiate4.F90"
END PROCEDURE obj_Initiate4b

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4c
#include "./include/Initiate4.F90"
END PROCEDURE obj_Initiate4c

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4d
#include "./include/Initiate4.F90"
END PROCEDURE obj_Initiate4d

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5a
#include "./include/Initiate4.F90"
END PROCEDURE obj_Initiate5a

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5b
#include "./include/Initiate4.F90"
END PROCEDURE obj_Initiate5b

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate6
LOGICAL(LGT) :: isok

obj%tDimension = obj2%tDimension

isok = ALLOCATED(obj2%val)
IF (.NOT. isok) THEN
  IF (ALLOCATED(obj%val)) DEALLOCATE (obj%val)
  obj%tsize = math%zero_i
  obj%capacity = math%zero_i
  RETURN
END IF

obj%tsize = obj2%tsize
CALL Util_Reallocate(obj%val, obj%tsize)
obj%capacity = obj%tsize

IF (obj%tsize .GT. math%zero_i) THEN
  CALL obj_Copy1_(x=obj%val, x_start=1_I4B, y=obj2%val, y_start=1_I4B, &
                  y_end=obj%tsize)
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

MODULE PROCEDURE obj_Assign1
IF (.NOT. ALLOCATED(obj%val)) THEN
  CALL Util_Reallocate(val, math%zero_i)
  RETURN
END IF

CALL Util_Reallocate(val, obj%tsize)

IF (obj%tsize .GT. math%zero_i) THEN
  CALL Copy_(x=val(1:obj%tsize), y=obj%val(1:obj%tsize))
END IF
END PROCEDURE obj_Assign1

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert1
to = from
END PROCEDURE obj_Convert1

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy1
INTEGER(I4B) :: tsize, ii
tsize = SIZE(y)
CALL Util_Reallocate(x, tsize)
DO CONCURRENT(ii=1:tsize)
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy1

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy2
INTEGER(I4B) :: tsize, ii
tsize = SIZE(y)
CALL Util_Reallocate(x, tsize)
DO CONCURRENT(ii=1:tsize)
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy2

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy3
INTEGER(I4B) :: tsize, ii
tsize = SIZE(y)
CALL Util_Reallocate(x, tsize)
DO CONCURRENT(ii=1:tsize)
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy3

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy4
INTEGER(I4B) :: tsize, ii
tsize = SIZE(y)
CALL Util_Reallocate(x, tsize)
DO CONCURRENT(ii=1:tsize)
  x(ii) = y(ii)
END DO
END PROCEDURE obj_Copy4

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
