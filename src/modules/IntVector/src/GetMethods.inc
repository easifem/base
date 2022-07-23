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

PUBLIC :: GET
PUBLIC :: getPointer
PUBLIC :: getIndex

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns IntVector instance

INTERFACE
  MODULE PURE FUNCTION intVec_get_1(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    TYPE(IntVector_), INTENT(IN) :: DataType
    TYPE(IntVector_) :: Val
  END FUNCTION intVec_get_1
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_1
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns an instance of [[intvector_]], obj(indx)

INTERFACE
  MODULE PURE FUNCTION intVec_get_2(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    TYPE(IntVector_), INTENT(IN) :: DataType
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    TYPE(IntVector_) :: Val
  END FUNCTION intVec_get_2
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_2
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns the value using triplets.

INTERFACE
  MODULE PURE FUNCTION intVec_get_3(obj, istart, iend, &
    & stride, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    TYPE(IntVector_), INTENT(IN) :: DataType
    !! an instance of [[IntVector_]]
    INTEGER(I4B), INTENT(IN) :: istart
    !! starting index value
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iend, stride
    !! iend is optional, default value is size(obj)
    !! stride is optional,  default value is 1.
    TYPE(IntVector_) :: Val
    !! returned value
  END FUNCTION intVec_get_3
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_3
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

INTERFACE
  MODULE PURE FUNCTION intVec_get_4(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    TYPE(IntVector_), INTENT(IN) :: DataType
    TYPE(IntVector_) :: Val
  END FUNCTION intVec_get_4
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_4
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Serialized the vector of [[IntVector_]], select values by indx

INTERFACE
  MODULE PURE FUNCTION intVec_get_5(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    TYPE(IntVector_), INTENT(IN) :: DataType
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    TYPE(IntVector_) :: Val
  END FUNCTION intVec_get_5
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_5
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_get_6(obj, iStart, iEnd, &
    & Stride, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    TYPE(IntVector_), INTENT(IN) :: DataType
    TYPE(IntVector_) :: Val
  END FUNCTION intVec_get_6
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_6
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_get_7a(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(Int8), INTENT(IN) :: DataType
    INTEGER(Int8), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_7a
  MODULE PURE FUNCTION intVec_get_7b(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(Int16), INTENT(IN) :: DataType
    INTEGER(Int16), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_7b
  MODULE PURE FUNCTION intVec_get_7c(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(Int32), INTENT(IN) :: DataType
    INTEGER(Int32), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_7c
  MODULE PURE FUNCTION intVec_get_7d(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(Int64), INTENT(IN) :: DataType
    INTEGER(Int64), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_7d
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_7a, intVec_get_7b, intVec_get_7c, &
    & intVec_get_7d
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_get_8a(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(Int8), INTENT(IN) :: DataType
    INTEGER(Int8), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_8a
  MODULE PURE FUNCTION intVec_get_8b(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(Int16), INTENT(IN) :: DataType
    INTEGER(Int16), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_8b
  MODULE PURE FUNCTION intVec_get_8c(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(Int32), INTENT(IN) :: DataType
    INTEGER(Int32), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_8c
  MODULE PURE FUNCTION intVec_get_8d(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(Int64), INTENT(IN) :: DataType
    INTEGER(Int64), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_8d
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_8a, intVec_get_8b, intVec_get_8c, &
    & intVec_get_8d
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_get_9a(obj, iStart, iEnd, Stride,&
    & DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(Int8), INTENT(IN) :: DataType
    INTEGER(Int8), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_9a
  MODULE PURE FUNCTION intVec_get_9b(obj, iStart, iEnd, Stride,&
    & DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(Int16), INTENT(IN) :: DataType
    INTEGER(Int16), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_9b
  MODULE PURE FUNCTION intVec_get_9c(obj, iStart, iEnd, Stride,&
    & DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(Int32), INTENT(IN) :: DataType
    INTEGER(Int32), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_9c
  MODULE PURE FUNCTION intVec_get_9d(obj, iStart, iEnd, Stride,&
    & DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(Int64), INTENT(IN) :: DataType
    INTEGER(Int64), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_9d
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_9a, intVec_get_9b, intVec_get_9c, &
    & intVec_get_9d
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_get_10a(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(Int8), INTENT(IN) :: DataType
    INTEGER(Int8), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_10a
  MODULE PURE FUNCTION intVec_get_10b(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(Int16), INTENT(IN) :: DataType
    INTEGER(Int16), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_10b
  MODULE PURE FUNCTION intVec_get_10c(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(Int32), INTENT(IN) :: DataType
    INTEGER(Int32), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_10c
  MODULE PURE FUNCTION intVec_get_10d(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(Int64), INTENT(IN) :: DataType
    INTEGER(Int64), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_10d
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_10a, intVec_get_10b, intVec_get_10c, &
    & intVec_get_10d
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_get_11a(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(Int8), INTENT(IN) :: DataType
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(Int8), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_11a
  MODULE PURE FUNCTION intVec_get_11b(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(Int16), INTENT(IN) :: DataType
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(Int16), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_11b
  MODULE PURE FUNCTION intVec_get_11c(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(Int32), INTENT(IN) :: DataType
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(Int32), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_11c
  MODULE PURE FUNCTION intVec_get_11d(obj, Indx, DataType) &
    & RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(Int64), INTENT(IN) :: DataType
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(Int64), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_11d
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_11a, intVec_get_11b, intVec_get_11c, &
    & intVec_get_11d
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_get_12a(obj, iStart, iEnd, &
    & Stride, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(Int8), INTENT(IN) :: DataType
    INTEGER(Int8), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_12a
  MODULE PURE FUNCTION intVec_get_12b(obj, iStart, iEnd, &
    & Stride, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(Int16), INTENT(IN) :: DataType
    INTEGER(Int16), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_12b
  MODULE PURE FUNCTION intVec_get_12c(obj, iStart, iEnd, &
    & Stride, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(Int32), INTENT(IN) :: DataType
    INTEGER(Int32), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_12c
  MODULE PURE FUNCTION intVec_get_12d(obj, iStart, iEnd, &
    & Stride, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    INTEGER(Int64), INTENT(IN) :: DataType
    INTEGER(Int64), ALLOCATABLE :: Val(:)
  END FUNCTION intVec_get_12d
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_12a, intVec_get_12b, &
    & intVec_get_12c, intVec_get_12d
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_get_13a(obj, indx, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(Int8), INTENT(IN) :: DataType
    INTEGER(Int8) :: val
  END FUNCTION intVec_get_13a
  MODULE PURE FUNCTION intVec_get_13b(obj, indx, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(Int16), INTENT(IN) :: DataType
    INTEGER(Int16) :: val
  END FUNCTION intVec_get_13b
  MODULE PURE FUNCTION intVec_get_13c(obj, indx, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(Int32), INTENT(IN) :: DataType
    INTEGER(Int32) :: val
  END FUNCTION intVec_get_13c
  MODULE PURE FUNCTION intVec_get_13d(obj, indx, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(Int64), INTENT(IN) :: DataType
    INTEGER(Int64) :: val
  END FUNCTION intVec_get_13d
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_13a, intVec_get_13b, intVec_get_13c, &
    & intVec_get_13d
END INTERFACE get

!----------------------------------------------------------------------------
!                                                     getPointers@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION intVec_getPointer_1(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN), TARGET :: obj
    TYPE(IntVector_), INTENT(IN) :: DataType
    TYPE(IntVector_), POINTER :: Val
  END FUNCTION intVec_getPointer_1
END INTERFACE

INTERFACE getPointer
  MODULE PROCEDURE intVec_getPointer_1
END INTERFACE getPointer

!----------------------------------------------------------------------------
!                                                    getPointers@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION intVec_getPointer_2(obj, DataType) RESULT(Val)
    CLASS(IntVector_), INTENT(IN), TARGET :: obj
    INTEGER(I4B), INTENT(IN) :: DataType
    INTEGER(I4B), POINTER :: Val(:)
  END FUNCTION intVec_getPointer_2
END INTERFACE

INTERFACE getPointer
  MODULE PROCEDURE intVec_getPointer_2
END INTERFACE getPointer

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_getIndex1(obj, Val) RESULT(Ans)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Val
    INTEGER(I4B) :: Ans
  END FUNCTION intVec_getIndex1
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE intVec_getIndex1
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_getIndex2(obj, Val) RESULT(Ans)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:)
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION intVec_getIndex2
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE intVec_getIndex2
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_getIndex3(obj, Val) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: Val
    INTEGER(I4B) :: Ans
  END FUNCTION intVec_getIndex3
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE intVec_getIndex3
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION intVec_getIndex4(obj, Val) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: Val(:)
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION intVec_getIndex4
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE intVec_getIndex4
END INTERFACE getIndex
