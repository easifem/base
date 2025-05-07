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

MODULE IntVector_GetMethod
USE GlobalData, ONLY: DFP, I4B, LGT, INT8, INT16, INT32, INT64
USE BaseType, ONLY: IntVector_

PRIVATE

PUBLIC :: GET
PUBLIC :: GetPointer
PUBLIC :: GetIndex

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns IntVector instance

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_1(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    TYPE(IntVector_), INTENT(IN) :: datatype
    TYPE(IntVector_) :: val
  END FUNCTION intVec_get_1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns an instance of [[intvector_]], obj(indx)

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_2(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    TYPE(IntVector_), INTENT(IN) :: datatype
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    TYPE(IntVector_) :: val
  END FUNCTION intVec_get_2
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns the value using triplets.

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_3(obj, istart, iend, &
    & stride, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    TYPE(IntVector_), INTENT(IN) :: datatype
    !! an instance of [[IntVector_]]
    INTEGER(I4B), INTENT(IN) :: istart
    !! starting index value
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iend, stride
    !! iend is optional, default value is size(obj)
    !! stride is optional,  default value is 1.
    TYPE(IntVector_) :: val
    !! returned value
  END FUNCTION intVec_get_3
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: converts a vector of [[intvector_]] into a scalar instance.
!
!
!# Introduction
!
! Converts a vector of [[intvector_]] into a scalar instance.
! something like following is done.
!
! obj = obj(1) // obj(2) // obj(3) ...
!
! The size of val is size(obj(1)) + size(obj(2)) + ...

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_4(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    TYPE(IntVector_), INTENT(IN) :: datatype
    TYPE(IntVector_) :: val
  END FUNCTION intVec_get_4
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Serialized the vector of [[IntVector_]], select values by indx

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_5(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    TYPE(IntVector_), INTENT(IN) :: datatype
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    TYPE(IntVector_) :: val
  END FUNCTION intVec_get_5
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_6(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    TYPE(IntVector_), INTENT(IN) :: datatype
    TYPE(IntVector_) :: val
  END FUNCTION intVec_get_6
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_7a(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_7a
  MODULE PURE FUNCTION intVec_get_7b(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_7b
  MODULE PURE FUNCTION intVec_get_7c(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_7c
  MODULE PURE FUNCTION intVec_get_7d(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_7d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_8a(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_8a
  MODULE PURE FUNCTION intVec_get_8b(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_8b
  MODULE PURE FUNCTION intVec_get_8c(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_8c
  MODULE PURE FUNCTION intVec_get_8d(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_8d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_9a(obj, iStart, iEnd, Stride,&
    & datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_9a
  MODULE PURE FUNCTION intVec_get_9b(obj, iStart, iEnd, Stride,&
    & datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_9b
  MODULE PURE FUNCTION intVec_get_9c(obj, iStart, iEnd, Stride,&
    & datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_9c
  MODULE PURE FUNCTION intVec_get_9d(obj, iStart, iEnd, Stride,&
    & datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_9d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_10a(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_10a
  MODULE PURE FUNCTION intVec_get_10b(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_10b
  MODULE PURE FUNCTION intVec_get_10c(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_10c
  MODULE PURE FUNCTION intVec_get_10d(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_10d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_11a(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(INT8), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_11a
  MODULE PURE FUNCTION intVec_get_11b(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(INT16), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_11b
  MODULE PURE FUNCTION intVec_get_11c(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(INT32), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_11c
  MODULE PURE FUNCTION intVec_get_11d(obj, Indx, datatype) &
    & RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(INT64), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_11d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_12a(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_12a
  MODULE PURE FUNCTION intVec_get_12b(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_12b
  MODULE PURE FUNCTION intVec_get_12c(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_12c
  MODULE PURE FUNCTION intVec_get_12d(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64), ALLOCATABLE :: val(:)
  END FUNCTION intVec_get_12d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION intVec_get_13a(obj, indx, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: val
  END FUNCTION intVec_get_13a
  MODULE PURE FUNCTION intVec_get_13b(obj, indx, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: val
  END FUNCTION intVec_get_13b
  MODULE PURE FUNCTION intVec_get_13c(obj, indx, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: val
  END FUNCTION intVec_get_13c
  MODULE PURE FUNCTION intVec_get_13d(obj, indx, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: val
  END FUNCTION intVec_get_13d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                     getPointers@getMethod
!----------------------------------------------------------------------------

INTERFACE GetPointer
  MODULE FUNCTION intVec_getPointer_1(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN), TARGET :: obj
    TYPE(IntVector_), INTENT(IN) :: datatype
    TYPE(IntVector_), POINTER :: val
  END FUNCTION intVec_getPointer_1
END INTERFACE GetPointer

!----------------------------------------------------------------------------
!                                                    getPointers@getMethod
!----------------------------------------------------------------------------

INTERFACE GetPointer
  MODULE FUNCTION intVec_getPointer_2(obj, datatype) RESULT(val)
    CLASS(IntVector_), INTENT(IN), TARGET :: obj
    INTEGER(I4B), INTENT(IN) :: datatype
    INTEGER(I4B), POINTER :: val(:)
  END FUNCTION intVec_getPointer_2
END INTERFACE GetPointer

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE GetIndex
  MODULE PURE FUNCTION intVec_getIndex1(obj, val) RESULT(ans)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: val
    INTEGER(I4B) :: ans
  END FUNCTION intVec_getIndex1
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE GetIndex
  MODULE PURE FUNCTION intVec_getIndex2(obj, val) RESULT(ans)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: val(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION intVec_getIndex2
END INTERFACE GetIndex

END MODULE IntVector_GetMethod
