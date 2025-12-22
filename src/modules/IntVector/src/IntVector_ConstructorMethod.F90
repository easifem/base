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

MODULE IntVector_ConstructorMethod
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B, DFP, LGT, INT8, INT16, INT32, INT64, &
                      REAL64, REAL32
PRIVATE

PUBLIC :: Shape
PUBLIC :: SIZE
PUBLIC :: GetTotalDimension
PUBLIC :: ALLOCATE
PUBLIC :: DEALLOCATE
PUBLIC :: Reallocate
PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: IntVector
PUBLIC :: IntVector_Pointer
PUBLIC :: Convert
PUBLIC :: Copy
PUBLIC :: Copy_

!----------------------------------------------------------------------------
!                                                         Shape@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: Returns shape of the vector

INTERFACE Shape
  MODULE PURE FUNCTION obj_shape(obj) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(1)
  END FUNCTION obj_shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         25 Feb 2021
! summary:         Returns size of the vector

INTERFACE Size
  MODULE PURE FUNCTION obj_Size(obj, dims) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         Returns the total dimension of an array
!
!# Introduction
!
! This function returns the total dimension (or rank) of an array,

INTERFACE GetTotalDimension
  MODULE PURE FUNCTION obj_getTotalDimension(obj) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_getTotalDimension
END INTERFACE GetTotalDimension

!----------------------------------------------------------------------------
!                                                   Allocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  25 Feb 2021
! summary:         Allocate memory for the vector

INTERFACE ALLOCATE
  MODULE PURE SUBROUTINE obj_AllocateData(obj, dims)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dims
  END SUBROUTINE obj_AllocateData
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                                   Reallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  25 Feb 2021
! summary: Allocate memory for the vector

INTERFACE Reallocate
  MODULE PURE SUBROUTINE obj_Reallocate(obj, row)
    TYPE(IntVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE obj_Reallocate
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: Deallocate memory occupied by IntVector

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(IntVector_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine initiates the [[IntVector_]]
!
!# Introduction
!
! This routine initiates an instance of IntVector
! Only the size of intvector is set.

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_initiate1(obj, tSize)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END SUBROUTINE obj_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine initiates the vector of [[IntVector_]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_initiate2(obj, tSize)
    TYPE(IntVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tSize(:)
  END SUBROUTINE obj_initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Initiates an instance on [[IntVector_]] with lower & upper bounds

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_initiate3(obj, a, b)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: a, b
  END SUBROUTINE obj_initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Initiates an instance by using a vector of integers
!
!# Introduction
! This routine initiates an instance of intvector by coping data
! from integer vector.
!
! This routine also define an assignment operator, obj=val

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_initiate4a(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    INTEGER(INT8), INTENT(IN) :: val(:)
  END SUBROUTINE obj_initiate4a
  !!
  MODULE PURE SUBROUTINE obj_initiate4b(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    INTEGER(INT16), INTENT(IN) :: val(:)
  END SUBROUTINE obj_initiate4b
  !!
  MODULE PURE SUBROUTINE obj_initiate4c(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    INTEGER(INT32), INTENT(IN) :: val(:)
  END SUBROUTINE obj_initiate4c
  !!
  MODULE PURE SUBROUTINE obj_initiate4d(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    INTEGER(INT64), INTENT(IN) :: val(:)
  END SUBROUTINE obj_initiate4d
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_initiate4a, obj_initiate4b, &
    obj_initiate4c, obj_initiate4d
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Initiates an instance by using a vector of real numbers
!
!# Introduction
!
! This routine initiates an instance of IntVector by copying data
! from a vector of reals. This routien also defines assignment operator,
! obj=val

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_initiate5a(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    REAL(REAL32), INTENT(IN) :: val(:)
  END SUBROUTINE obj_initiate5a
  !!
  MODULE PURE SUBROUTINE obj_initiate5b(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    REAL(REAL64), INTENT(IN) :: val(:)
  END SUBROUTINE obj_initiate5b
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_initiate5a, obj_initiate5b
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-25
! summary: Initiate an instance of IntVector by copying data from other

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_initiate6(obj, obj2)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    TYPE(IntVector_), INTENT(IN) :: obj2
  END SUBROUTINE obj_initiate6
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_initiate6
END INTERFACE ASSIGNMENT(=)

INTERFACE COPY
  MODULE PROCEDURE obj_initiate6
END INTERFACE COPY

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: IntVector returns an instance of [[IntVector_]] of given size

INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor1(tSize) RESULT(obj)
    TYPE(IntVector_) :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END FUNCTION obj_Constructor1
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Convert a integer vector into [[IntVector_]]

INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor2(Val) RESULT(obj)
    TYPE(IntVector_) :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:)
  END FUNCTION obj_Constructor2
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Converts a real vector into [[IntVector_]]
!
! TODO Implement IntVector method for Int4, Int8, Int16, Int32
! Real32, Real64
!
INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor3(Val) RESULT(obj)
    TYPE(IntVector_) :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
  END FUNCTION obj_Constructor3
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns the pointer to an instance of [[IntVector_]] of tsize

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_1(tSize) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END FUNCTION obj_Constructor_1
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Converts integer vector into [[intvector_]] and returns the pointer

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_2(Val) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:)
  END FUNCTION obj_Constructor_2
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Converts real vector into [[intvector_]] and returns the pointer

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_3(Val) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
  END FUNCTION obj_Constructor_3
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 assign@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_assign_a(Val, obj)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: Val(:)
    TYPE(IntVector_), INTENT(IN) :: obj
  END SUBROUTINE obj_assign_a
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                Convert@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_convert_int(From, To)
    TYPE(IntVector_), INTENT(IN) :: From
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: To(:)
  END SUBROUTINE obj_convert_int
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  Copy y into x, x will be reallocated
!
!# Introduction
!
! Get the size of y and reallocate x to the same size.
! If x is already allocated, it will be reallocated to the size of y.

INTERFACE Copy
  MODULE PURE SUBROUTINE obj_Copy_Int8(x, y)
    INTEGER(INT8), INTENT(INOUT), ALLOCATABLE :: x(:)
    INTEGER(INT8), INTENT(IN) :: y(:)
  END SUBROUTINE obj_Copy_Int8
END INTERFACE Copy

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  Copy y into x, x will be reallocated
!
!# Introduction
!
! Get the size of y and reallocate x to the same size.
! If x is already allocated, it will be reallocated to the size of y.

INTERFACE Copy
  MODULE PURE SUBROUTINE obj_Copy_Int16(x, y)
    INTEGER(INT16), INTENT(INOUT), ALLOCATABLE :: x(:)
    INTEGER(INT16), INTENT(IN) :: y(:)
  END SUBROUTINE obj_Copy_Int16
END INTERFACE Copy

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  Copy y into x, x will be reallocated
!
! Introduction
!
! Get the size of y and reallocate x to the same size.
! If x is already allocated, it will be reallocated to the size of y.

INTERFACE Copy
  MODULE PURE SUBROUTINE obj_Copy_Int32(x, y)
    INTEGER(INT32), INTENT(INOUT), ALLOCATABLE :: x(:)
    INTEGER(INT32), INTENT(IN) :: y(:)
  END SUBROUTINE obj_Copy_Int32
END INTERFACE Copy

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  Copy y into x, x will be reallocated
!
!# Introduction
!
! Get the size of y and reallocate x to the same size.
! If x is already allocated, it will be reallocated to the size of y.

INTERFACE Copy
  MODULE PURE SUBROUTINE obj_Copy_Int64(x, y)
    INTEGER(INT64), INTENT(INOUT), ALLOCATABLE :: x(:)
    INTEGER(INT64), INTENT(IN) :: y(:)
  END SUBROUTINE obj_Copy_Int64
END INTERFACE Copy

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  Copy portion of y into x

INTERFACE Copy_
  MODULE PURE SUBROUTINE obj_Copy1_(x, x_start, y, y_start, y_end)
    INTEGER(I4B), INTENT(INOUT) :: x(:)
    !! x vector should be allocated
    INTEGER(I4B), INTENT(IN) :: y(:)
    INTEGER(I4B), INTENT(IN) :: x_start
    INTEGER(I4B), INTENT(IN) :: y_start, y_end
  END SUBROUTINE obj_Copy1_
END INTERFACE Copy_

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  Copy y into x

INTERFACE Copy_
  MODULE PURE SUBROUTINE obj_Copy2_(x, y)
    INTEGER(I4B), INTENT(INOUT) :: x(:)
    INTEGER(I4B), INTENT(IN) :: y(:)
  END SUBROUTINE obj_Copy2_
END INTERFACE Copy_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE IntVector_ConstructorMethod
