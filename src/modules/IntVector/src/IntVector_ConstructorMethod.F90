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

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Constructor, size and copy methods for [[IntVector_]]
!
! [[IntVector_]] is an expandable vector of integers. Its storage
! `obj%val` holds `obj%capacity` entries, of which only the first
! `obj%tsize` are active data. `Size(obj)` returns `tsize` and
! `Capacity(obj)` returns `capacity`.
!
! Changes:
!   - 2026-11-03: Added tsize and capacity to IntVector_, added Capacity,
!     removed Initiate(obj, a, b)

MODULE IntVector_ConstructorMethod
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL64
USE GlobalData, ONLY: REAL32
IMPLICIT NONE
PRIVATE

PUBLIC :: Shape
PUBLIC :: SIZE
PUBLIC :: Capacity
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
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Returns the shape of the active data of [[IntVector_]]
!
! Returns the shape of the vector as an array with one element.
! The element is the number of active entries, `obj%tsize`.
! It is 0 for an empty or uninitialised vector.
!
! @note
! Unused capacity is not counted. See [[obj_Capacity]].
! @endnote
!
! Changes:
!   - 2026-11-03: Returns tsize instead of SIZE(obj%val)
!
! Returns:
!   ans : INTEGER(I4B)(1) - `[obj%tsize]`
!
! Examples
!
! - [IntVector test 1](|base|IntVector/examples/IntVector_test_1.F90)
!
! See also: [[obj_Size]], [[obj_Capacity]]

INTERFACE Shape
  MODULE PURE FUNCTION obj_Shape(obj) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    INTEGER(I4B) :: ans(1)
    !! shape of the active data, `[obj%tsize]`
  END FUNCTION obj_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Returns the number of active entries in [[IntVector_]]
!
! Returns the number of active entries of the vector, `obj%tsize`.
! It is 0 for an empty or uninitialised vector.
!
! @note
! The allocated length of `obj%val` can be larger than the size.
! See [[obj_Capacity]].
! @endnote
!
! Changes:
!   - 2026-11-03: Returns tsize instead of SIZE(obj%val)
!
! Returns:
!   ans : INTEGER(I4B) - number of active entries, `obj%tsize`
!
! Examples
!
! - [IntVector test 2](|base|IntVector/examples/IntVector_test_2.F90)
!
! See also: [[obj_Capacity]], [[obj_Shape]]

INTERFACE Size
  MODULE PURE FUNCTION obj_Size(obj, dims) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used; the vector always has rank 1
    INTEGER(I4B) :: ans
    !! number of active entries, `obj%tsize`
  END FUNCTION obj_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                        Capacity@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Returns the capacity of [[IntVector_]]
!
! Returns the number of entries that `obj%val` can hold without
! reallocation, `obj%capacity`.
! It is 0 for an empty or uninitialised vector.
! The capacity is never smaller than the size.
!
! Returns:
!   ans : INTEGER(I4B) - capacity, `obj%capacity`
!
! Examples
!
! - [IntVector test 3](|base|IntVector/examples/IntVector_test_3.F90)
!
! See also: [[obj_Size]]

INTERFACE Capacity
  MODULE PURE FUNCTION obj_Capacity(obj) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    INTEGER(I4B) :: ans
    !! capacity of `obj%val`, `obj%capacity`
  END FUNCTION obj_Capacity
END INTERFACE Capacity

!----------------------------------------------------------------------------
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the total dimension (rank) of [[IntVector_]]
!
! Returns the rank of the vector, stored in `obj%tDimension`.
! An initiated vector has rank 1.
! After `DEALLOCATE`, the rank is 0.
!
! Returns:
!   ans : INTEGER(I4B) - total dimension, `obj%tDimension`
!
! Examples
!
! - [IntVector test 4](|base|IntVector/examples/IntVector_test_4.F90)

INTERFACE GetTotalDimension
  MODULE PURE FUNCTION obj_GetTotalDimension(obj) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    INTEGER(I4B) :: ans
    !! total dimension, `obj%tDimension`
  END FUNCTION obj_GetTotalDimension
END INTERFACE GetTotalDimension

!----------------------------------------------------------------------------
!                                                   Allocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Allocates [[IntVector_]] with dims entries set to zero
!
! Allocates `obj%val` with exactly `dims` entries and sets them to zero.
! Afterwards the size and the capacity both equal `dims`.
! The total dimension is set to 1.
!
! @warning
! Any previous data in `obj` is lost.
! @endwarning
!
! Changes:
!   - 2026-11-03: Sets tsize and capacity to dims
!
! Examples
!
! - [IntVector test 5](|base|IntVector/examples/IntVector_test_5.F90)
!
! See also: [[obj_Deallocate]], [[obj_Initiate1]]

INTERFACE ALLOCATE
  MODULE PURE SUBROUTINE obj_AllocateData(obj, dims)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    INTEGER(I4B), INTENT(IN) :: dims
    !! number of entries, `dims >= 0`
  END SUBROUTINE obj_AllocateData
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                                   Reallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Reallocates an array of [[IntVector_]] to row entries
!
! Makes the array `obj` hold `row` empty vectors.
!
! 1. If `obj` is not allocated, it is allocated with `row` entries.
! 2. If `SIZE(obj)` differs from `row`, it is allocated again with
!    `row` entries. The old entries are lost.
! 3. If `SIZE(obj)` equals `row`, nothing changes. Each entry keeps
!    its data.
!
! Examples
!
! - [IntVector test 6](|base|IntVector/examples/IntVector_test_6.F90)
!
! See also: [[obj_Initiate2]]

INTERFACE Reallocate
  MODULE PURE SUBROUTINE obj_Reallocate(obj, row)
    TYPE(IntVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! array of integer vectors
    INTEGER(I4B), INTENT(IN) :: row
    !! required number of entries in obj
  END SUBROUTINE obj_Reallocate
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Deallocates the memory of [[IntVector_]]
!
! Frees `obj%val` if it is allocated.
! The size, the capacity and the total dimension are set to 0.
!
! Changes:
!   - 2026-11-03: Also resets tsize and capacity
!
! Examples
!
! - [IntVector test 5](|base|IntVector/examples/IntVector_test_5.F90)
!
! See also: [[obj_AllocateData]]

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Initiates [[IntVector_]] with tSize entries set to zero
!
! Allocates `obj%val` with `tSize` entries and sets them to zero.
! Afterwards the size and the capacity both equal `tSize`.
! The total dimension is set to 1.
! This works like [[obj_AllocateData]].
!
! @warning
! Any previous data in `obj` is lost.
! @endwarning
!
! Examples
!
! - [test 1](|base|IntVector/examples/Initiate_test_1.F90) shows
!   `Initiate` on a vector that holds data
! - [IntVector test 7](|base|IntVector/examples/IntVector_test_7.F90)
!
! See also: [[obj_AllocateData]], [[obj_Initiate2]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate1(obj, tSize)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    INTEGER(I4B), INTENT(IN) :: tSize
    !! number of entries, `tSize >= 0`
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Initiates an array of [[IntVector_]]
!
! Initiates each vector of the array `obj` with zero entries.
! Entry `obj(i)` gets `tSize(i)` entries.
!
! 1. If `obj` is not allocated, or `SIZE(obj)` differs from
!    `SIZE(tSize)`, it is allocated with `SIZE(tSize)` entries.
! 2. Each `obj(i)` is initiated as in [[obj_Initiate1]].
!
! Examples
!
! - [test 1](|base|IntVector/examples/Initiate_test_1.F90) shows
!   arrays of the same and of another size
! - [IntVector test 8](|base|IntVector/examples/IntVector_test_8.F90)
!
! See also: [[obj_Initiate1]], [[obj_Reallocate]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2(obj, tSize)
    TYPE(IntVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! array of integer vectors, `SIZE(obj) = SIZE(tSize)` on return
    INTEGER(I4B), INTENT(IN) :: tSize(:)
    !! `tSize(i)` is the number of entries of `obj(i)`
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Initiates [[IntVector_]] from a vector of integers
!
! Copies the integer array `val` into `obj`.
! Afterwards the size and the capacity both equal `SIZE(val)`.
! The total dimension is set to 1.
!
! The specific routines accept `val` of kind `INT8`, `INT16`, `INT32`
! or `INT64`. The values are converted to `INTEGER(I4B)`.
! The assignment `obj = val` calls the same routines.
!
! @warning
! Any previous data in `obj` is lost.
! @endwarning
!
! Changes:
!   - 2026-11-03: Sets tsize and capacity to SIZE(val)
!
! Examples
!
! - [test 1](|base|IntVector/examples/Initiate_test_1.F90) shows
!   `obj = val` with an empty array
! - [IntVector test 9](|base|IntVector/examples/IntVector_test_9.F90)
!
! See also: [[obj_Initiate5a]], [[obj_Constructor2]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate4a(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    INTEGER(INT8), INTENT(IN) :: val(:)
    !! values to copy
  END SUBROUTINE obj_Initiate4a

  MODULE PURE SUBROUTINE obj_Initiate4b(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    INTEGER(INT16), INTENT(IN) :: val(:)
    !! values to copy
  END SUBROUTINE obj_Initiate4b

  MODULE PURE SUBROUTINE obj_Initiate4c(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    INTEGER(INT32), INTENT(IN) :: val(:)
    !! values to copy
  END SUBROUTINE obj_Initiate4c

  MODULE PURE SUBROUTINE obj_Initiate4d(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    INTEGER(INT64), INTENT(IN) :: val(:)
    !! values to copy; each value must fit in `INTEGER(I4B)`
  END SUBROUTINE obj_Initiate4d
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Initiate4a, obj_Initiate4b, &
    obj_Initiate4c, obj_Initiate4d
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Initiates [[IntVector_]] from a vector of reals
!
! Copies the real vector `val` into `obj`, discarding the old data.
! Each value is converted with `INT`, which truncates toward zero.
! For example, 1.7 gives 1 and -1.7 gives -1.
! Afterwards `Size(obj) = Capacity(obj) = SIZE(val)`.
! The total dimension is set to 1.
!
! This routine also defines the assignment `obj = val` for `REAL32`
! and `REAL64` vectors.
!
! Changes:
!   - 2026-11-03: Sets tsize and capacity to SIZE(val)
!
! Examples
!
! - [IntVector test 10](|base|IntVector/examples/IntVector_test_10.F90)
!
! See also: [[obj_Initiate4a]], [[obj_Constructor3]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate5a(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    REAL(REAL32), INTENT(IN) :: val(:)
    !! values to convert, truncated toward zero
  END SUBROUTINE obj_Initiate5a

  MODULE PURE SUBROUTINE obj_Initiate5b(obj, val)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    REAL(REAL64), INTENT(IN) :: val(:)
    !! values to convert, truncated toward zero
  END SUBROUTINE obj_Initiate5b
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Initiate5a, obj_Initiate5b
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-25
! summary: Initiates [[IntVector_]] by copying another IntVector_
!
! Copies the active entries `obj2%val(1:obj2%tsize)` into `obj`.
! The old data of `obj` is discarded.
! Afterwards `Size(obj) = Capacity(obj) = Size(obj2)`.
! The total dimension of `obj` is the one of `obj2`.
!
! If `obj2%val` is not allocated, `obj%val` is deallocated.
! Then `obj` is empty.
!
! This routine also defines the assignment `obj = obj2` and
! `Copy(obj, obj2)`.
!
! Changes:
!   - 2026-11-03: Copy only the active part of obj2 and keep tsize and
!     capacity of obj consistent with its storage
!
! Examples
!
! - [test 1](|base|IntVector/examples/Copy_test_1.F90) copies a vector
!   with spare capacity and an empty vector
! - [IntVector test 11](|base|IntVector/examples/IntVector_test_11.F90)

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate6(obj, obj2)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! destination; its previous data is discarded
    TYPE(IntVector_), INTENT(IN) :: obj2
    !! source; only `obj2%val(1:obj2%tsize)` is copied
  END SUBROUTINE obj_Initiate6
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Initiate6
END INTERFACE ASSIGNMENT(=)

INTERFACE COPY
  MODULE PROCEDURE obj_Initiate6
END INTERFACE COPY

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns an [[IntVector_]] with tSize entries set to zero
!
! Returns an [[IntVector_]] with `tSize` entries, all set to zero.
! It is the function form of [[obj_Initiate1]].
!
! Returns:
!   obj : TYPE(IntVector_) - `Size(obj) = Capacity(obj) = tSize`
!
! Examples
!
! - [IntVector test 12](|base|IntVector/examples/IntVector_test_12.F90)
!
! See also: [[obj_Initiate1]], [[obj_Constructor_1]]

INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor1(tSize) RESULT(obj)
    TYPE(IntVector_) :: obj
    !! integer vector with `tSize` zero entries
    INTEGER(I4B), INTENT(IN) :: tSize
    !! number of entries, `tSize >= 0`
  END FUNCTION obj_Constructor1
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Converts an integer vector into [[IntVector_]]
!
! Returns an [[IntVector_]] that holds a copy of the integer vector
! `val`, as in [[obj_Initiate4c]].
!
! Returns:
!   obj : TYPE(IntVector_) - copy of `val`, with `Size(obj) = SIZE(val)`
!
! Examples
!
! - [IntVector test 13](|base|IntVector/examples/IntVector_test_13.F90)
!
! See also: [[obj_Initiate4c]], [[obj_Constructor_2]]

INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor2(val) RESULT(obj)
    TYPE(IntVector_) :: obj
    !! integer vector holding a copy of `val`
    INTEGER(I4B), INTENT(IN) :: val(:)
    !! values to copy
  END FUNCTION obj_Constructor2
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Converts a real vector into [[IntVector_]]
!
! Returns an [[IntVector_]] that holds the values of the real vector
! `val`. Each value is truncated toward zero, as in [[obj_Initiate5b]].
!
! Returns:
!   obj : TYPE(IntVector_) - `INT(val)`, with `Size(obj) = SIZE(val)`
!
! Examples
!
! - [IntVector test 14](|base|IntVector/examples/IntVector_test_14.F90)
!
! See also: [[obj_Initiate5b]], [[obj_Constructor_3]]

INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor3(val) RESULT(obj)
    TYPE(IntVector_) :: obj
    !! integer vector holding `INT(val)`
    REAL(DFP), INTENT(IN) :: val(:)
    !! values to convert, truncated toward zero
  END FUNCTION obj_Constructor3
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns a pointer to a new [[IntVector_]] of tSize entries
!
! Allocates a new [[IntVector_]] with `tSize` entries set to zero,
! as in [[obj_Initiate1]].
!
! @note
! The caller owns the result and must `DEALLOCATE` it.
! @endnote
!
! Returns:
!   obj : TYPE(IntVector_), POINTER - new vector with `tSize` zero entries
!
! Examples
!
! - [IntVector test 15](|base|IntVector/examples/IntVector_test_15.F90)
!
! See also: [[obj_Constructor1]]

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_1(tSize) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    !! newly allocated integer vector with `tSize` zero entries
    INTEGER(I4B), INTENT(IN) :: tSize
    !! number of entries, `tSize >= 0`
  END FUNCTION obj_Constructor_1
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns a pointer to a new [[IntVector_]] copied from val
!
! Allocates a new [[IntVector_]] and copies the integer vector `val`
! into it, as in [[obj_Initiate4c]].
!
! @note
! The caller owns the result and must `DEALLOCATE` it.
! @endnote
!
! Returns:
!   obj : TYPE(IntVector_), POINTER - new vector holding a copy of `val`
!
! Examples
!
! - [IntVector test 16](|base|IntVector/examples/IntVector_test_16.F90)
!
! See also: [[obj_Constructor2]]

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_2(val) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    !! newly allocated integer vector holding a copy of `val`
    INTEGER(I4B), INTENT(IN) :: val(:)
    !! values to copy
  END FUNCTION obj_Constructor_2
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns a pointer to a new [[IntVector_]] converted from reals
!
! Allocates a new [[IntVector_]] and fills it from the real vector
! `val`. Each value is truncated toward zero, as in [[obj_Initiate5b]].
!
! @note
! The caller owns the result and must `DEALLOCATE` it.
! @endnote
!
! Returns:
!   obj : TYPE(IntVector_), POINTER - new vector holding `INT(val)`
!
! Examples
!
! - [IntVector test 17](|base|IntVector/examples/IntVector_test_17.F90)
!
! See also: [[obj_Constructor3]]

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_3(val) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    !! newly allocated integer vector holding `INT(val)`
    REAL(DFP), INTENT(IN) :: val(:)
    !! values to convert, truncated toward zero
  END FUNCTION obj_Constructor_3
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 assign@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Assigns the active data of [[IntVector_]] to an integer array
!
! Defines the assignment `val = obj`.
! It reallocates `val` to `Size(obj)` entries.
! Then it copies the active entries `obj%val(1:obj%tsize)` into `val`.
! If `obj%val` is not allocated, `val` becomes a zero-size array.
!
! Changes:
!   - 2026-11-03: Copies only obj%val(1:obj%tsize); an empty obj gives
!     a zero-size val instead of leaving val unchanged
!
! Examples
!
! - [IntVector test 18](|base|IntVector/examples/IntVector_test_18.F90)
!
! See also: [[obj_Convert1]]

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_Assign1(val, obj)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: val(:)
    !! destination, `SIZE(val) = Size(obj)` on return
    TYPE(IntVector_), INTENT(IN) :: obj
    !! source integer vector
  END SUBROUTINE obj_Assign1
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                Convert@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Converts [[IntVector_]] into an integer array
!
! Copies the active entries of `from` into the integer array `to`.
! This does the same as the assignment `to = from`.
!
! Examples
!
! - [IntVector test 19](|base|IntVector/examples/IntVector_test_19.F90)
!
! See also: [[obj_Assign1]]

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert1(from, to)
    TYPE(IntVector_), INTENT(IN) :: from
    !! source integer vector
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: to(:)
    !! destination, `SIZE(to) = Size(from)` on return
  END SUBROUTINE obj_Convert1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary: Copies y into x, reallocating x to SIZE(y)
!
! Allocates `x` with exactly `SIZE(y)` entries, then sets `x = y`.
! This also holds when `x` is already allocated with another size.
! The routine exists for the integer kinds `INT8`, `INT16`, `INT32` and
! `INT64`. Both arguments must have the same kind.
!
! Examples
!
! - [IntVector test 20](|base|IntVector/examples/IntVector_test_20.F90)
!
! See also: [[obj_Copy2_]], [[obj_Initiate6]]

INTERFACE Copy
  MODULE PURE SUBROUTINE obj_Copy1(x, y)
    INTEGER(INT8), INTENT(INOUT), ALLOCATABLE :: x(:)
    !! destination, SIZE(x) = SIZE(y) on return
    INTEGER(INT8), INTENT(IN) :: y(:)
    !! source
  END SUBROUTINE obj_Copy1

  MODULE PURE SUBROUTINE obj_Copy2(x, y)
    INTEGER(INT16), INTENT(INOUT), ALLOCATABLE :: x(:)
    !! destination, SIZE(x) = SIZE(y) on return
    INTEGER(INT16), INTENT(IN) :: y(:)
    !! source
  END SUBROUTINE obj_Copy2

  MODULE PURE SUBROUTINE obj_Copy3(x, y)
    INTEGER(INT32), INTENT(INOUT), ALLOCATABLE :: x(:)
    !! destination, SIZE(x) = SIZE(y) on return
    INTEGER(INT32), INTENT(IN) :: y(:)
    !! source
  END SUBROUTINE obj_Copy3

  MODULE PURE SUBROUTINE obj_Copy4(x, y)
    INTEGER(INT64), INTENT(INOUT), ALLOCATABLE :: x(:)
    !! destination, SIZE(x) = SIZE(y) on return
    INTEGER(INT64), INTENT(IN) :: y(:)
    !! source
  END SUBROUTINE obj_Copy4
END INTERFACE Copy

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary: Copies y(y_start:y_end) into x starting at x_start
!
! Sets `x(x_start + i - y_start) = y(i)` for `i = y_start, ..., y_end`.
! Nothing is copied when `y_end < y_start`.
!
! @note
! `x` is not reallocated.
! It must have at least `x_start + y_end - y_start` entries.
! @endnote
!
! Examples
!
! - [IntVector test 21](|base|IntVector/examples/IntVector_test_21.F90)
!
! See also: [[obj_Copy2_]]

INTERFACE Copy_
  MODULE PURE SUBROUTINE obj_Copy1_(x, x_start, y, y_start, y_end)
    INTEGER(I4B), INTENT(INOUT) :: x(:)
    !! destination, `SIZE(x) >= x_start + y_end - y_start`
    INTEGER(I4B), INTENT(IN) :: y(:)
    !! source
    INTEGER(I4B), INTENT(IN) :: x_start
    !! index in x where copying starts
    INTEGER(I4B), INTENT(IN) :: y_start, y_end
    !! range `y(y_start:y_end)` to copy
  END SUBROUTINE obj_Copy1_
END INTERFACE Copy_

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary: Copies y into the first SIZE(y) entries of x
!
! Sets `x(1:SIZE(y)) = y`. The other entries of `x` do not change.
!
! @note
! `x` is not reallocated. It must have at least `SIZE(y)` entries.
! @endnote
!
! Examples
!
! - [IntVector test 22](|base|IntVector/examples/IntVector_test_22.F90)
!
! See also: [[obj_Copy1_]], [[obj_Copy1]]

INTERFACE Copy_
  MODULE PURE SUBROUTINE obj_Copy2_(x, y)
    INTEGER(I4B), INTENT(INOUT) :: x(:)
    !! destination, SIZE(x) >= SIZE(y)
    INTEGER(I4B), INTENT(IN) :: y(:)
    !! source
  END SUBROUTINE obj_Copy2_
END INTERFACE Copy_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE IntVector_ConstructorMethod
