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
! The shape is `[obj%tsize]`, the number of active entries. It is
! `[0]` for an empty or uninitialised vector. Unused capacity is not
! counted.
!
! Changes:
!   - 2026-11-03: Returns tsize instead of SIZE(obj%val)
!
! Returns:
!   ans : INTEGER(I4B)(1) - [obj%tsize]
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Shape
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
! INTEGER(I4B) :: s(1)
!
! s = Shape(obj)
! CALL OK(s(1) .EQ. 0, "Shape: empty IntVector_")
!
! obj = [1_I4B, 2_I4B, 3_I4B]
! s = Shape(obj)
! CALL OK(s(1) .EQ. 3, "Shape: [3] after obj = [1, 2, 3]")
!
! END PROGRAM main
!```
!
! See also: [[obj_Size]], [[obj_Capacity]]

INTERFACE Shape
  MODULE PURE FUNCTION obj_Shape(obj) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    INTEGER(I4B) :: ans(1)
    !! shape of the active data, [obj%tsize]
  END FUNCTION obj_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Returns the number of active entries in [[IntVector_]]
!
! Returns `obj%tsize`, the number of entries of `obj%val` that hold
! data. It is 0 for an empty or uninitialised vector. The allocated
! length of `obj%val` can be larger; see [[obj_Capacity]].
!
! Changes:
!   - 2026-11-03: Returns tsize instead of SIZE(obj%val)
!
! Returns:
!   ans : INTEGER(I4B) - obj%tsize
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! CALL OK(Size(obj) .EQ. 0, "Size: empty IntVector_")
!
! obj = [1_I4B, 2_I4B, 3_I4B]
! CALL OK(Size(obj) .EQ. 3, "Size: 3 after obj = [1, 2, 3]")
!
! END PROGRAM main
!```
!
! See also: [[obj_Capacity]], [[obj_Shape]]

INTERFACE Size
  MODULE PURE FUNCTION obj_Size(obj, dims) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used; IntVector_ is always rank 1
    INTEGER(I4B) :: ans
    !! number of active entries, obj%tsize
  END FUNCTION obj_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                        Capacity@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Returns the capacity of [[IntVector_]]
!
! Returns `obj%capacity`, the number of entries `obj%val` can hold
! without reallocation. `Capacity(obj) >= Size(obj)` always holds.
! It is 0 for an empty or uninitialised vector.
!
! Returns:
!   ans : INTEGER(I4B) - obj%capacity
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: Capacity
! USE IntVector_Method, ONLY: Initiate
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! CALL OK(Capacity(obj) .EQ. 0, "Capacity: empty IntVector_")
!
! CALL Initiate(obj=obj, tSize=5_I4B)
! CALL OK(Capacity(obj) .EQ. 5, "Capacity: 5 after Initiate")
! CALL OK(Capacity(obj) .GE. Size(obj), "Capacity: not less than Size")
!
! END PROGRAM main
!```
!
! See also: [[obj_Size]]

INTERFACE Capacity
  MODULE PURE FUNCTION obj_Capacity(obj) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    INTEGER(I4B) :: ans
    !! capacity of obj%val, obj%capacity
  END FUNCTION obj_Capacity
END INTERFACE Capacity

!----------------------------------------------------------------------------
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the total dimension (rank) of [[IntVector_]]
!
! Returns `obj%tDimension`. All constructors set it to 1, and
! [[obj_Deallocate]] resets it to 0.
!
! Returns:
!   ans : INTEGER(I4B) - obj%tDimension
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: GetTotalDimension
! USE IntVector_Method, ONLY: Initiate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! CALL Initiate(obj=obj, tSize=4_I4B)
! CALL OK(GetTotalDimension(obj) .EQ. 1, "GetTotalDimension: rank 1")
!
! END PROGRAM main
!```

INTERFACE GetTotalDimension
  MODULE PURE FUNCTION obj_GetTotalDimension(obj) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    INTEGER(I4B) :: ans
    !! total dimension, obj%tDimension
  END FUNCTION obj_GetTotalDimension
END INTERFACE GetTotalDimension

!----------------------------------------------------------------------------
!                                                   Allocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Allocates [[IntVector_]] with dims entries set to zero
!
! `obj%val` is reallocated to exactly `dims` entries and filled with
! zeros. Afterwards `Size(obj) = Capacity(obj) = dims` and the total
! dimension is 1. Previous data is lost.
!
! Changes:
!   - 2026-11-03: Sets tsize and capacity to dims
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ALLOCATE
! USE IntVector_Method, ONLY: Capacity
! USE IntVector_Method, ONLY: DEALLOCATE
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! CALL ALLOCATE(obj=obj, dims=4_I4B)
! CALL OK(Size(obj) .EQ. 4, "ALLOCATE: Size is 4")
! CALL OK(Capacity(obj) .EQ. 4, "ALLOCATE: Capacity is 4")
! CALL OK(ALL(obj%val .EQ. 0), "ALLOCATE: entries are zero")
!
! CALL DEALLOCATE(obj)
! CALL OK(Size(obj) .EQ. 0, "DEALLOCATE: Size is 0")
! CALL OK(Capacity(obj) .EQ. 0, "DEALLOCATE: Capacity is 0")
! CALL OK(.NOT. ALLOCATED(obj%val), "DEALLOCATE: val is deallocated")
!
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate]], [[obj_Initiate1]]

INTERFACE ALLOCATE
  MODULE PURE SUBROUTINE obj_AllocateData(obj, dims)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    INTEGER(I4B), INTENT(IN) :: dims
    !! number of entries, dims >= 0
  END SUBROUTINE obj_AllocateData
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                                   Reallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Reallocates an array of [[IntVector_]] to row entries
!
! If `obj` is not allocated, or `SIZE(obj) /= row`, `obj` is
! (re)allocated with `row` empty [[IntVector_]] entries. If
! `SIZE(obj) == row`, `obj` is left unchanged, including the data in
! each entry.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: Reallocate
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_), ALLOCATABLE :: obj(:)
!
! CALL Reallocate(obj=obj, row=3_I4B)
! CALL OK(SIZE(obj) .EQ. 3, "Reallocate: 3 IntVector_ entries")
! CALL OK(Size(obj(1)) .EQ. 0, "Reallocate: each entry is empty")
!
! CALL Reallocate(obj=obj, row=5_I4B)
! CALL OK(SIZE(obj) .EQ. 5, "Reallocate: resized to 5 entries")
!
! END PROGRAM main
!```
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
! Deallocates `obj%val` and sets `tsize`, `capacity` and `tDimension`
! to 0.
!
! Changes:
!   - 2026-11-03: Also resets tsize and capacity
!
! Examples
!
! See the example of [[obj_AllocateData]].

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
! Same as [[obj_AllocateData]]: `obj%val` is reallocated to `tSize`
! entries set to zero, and `Size(obj) = Capacity(obj) = tSize`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: Initiate
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! CALL Initiate(obj=obj, tSize=5_I4B)
! CALL OK(Size(obj) .EQ. 5, "Initiate: Size is 5")
! CALL OK(ALL(obj%val(1:5) .EQ. 0), "Initiate: entries are zero")
!
! END PROGRAM main
!```
!
! See also: [[obj_AllocateData]], [[obj_Initiate2]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate1(obj, tSize)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! integer vector; previous data is discarded
    INTEGER(I4B), INTENT(IN) :: tSize
    !! number of entries, tSize >= 0
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Initiates an array of [[IntVector_]]
!
! `obj` is (re)allocated to `SIZE(tSize)` entries if needed, and each
! `obj(i)` is initiated with `tSize(i)` zero entries as in
! [[obj_Initiate1]].
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: Initiate
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_), ALLOCATABLE :: obj(:)
!
! CALL Initiate(obj=obj, tSize=[2_I4B, 0_I4B, 3_I4B])
! CALL OK(SIZE(obj) .EQ. 3, "Initiate: 3 IntVector_ entries")
! CALL OK(Size(obj(1)) .EQ. 2, "Initiate: Size(obj(1)) is 2")
! CALL OK(Size(obj(2)) .EQ. 0, "Initiate: Size(obj(2)) is 0")
! CALL OK(Size(obj(3)) .EQ. 3, "Initiate: Size(obj(3)) is 3")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate1]], [[obj_Reallocate]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2(obj, tSize)
    TYPE(IntVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! array of integer vectors, SIZE(obj) = SIZE(tSize) on return
    INTEGER(I4B), INTENT(IN) :: tSize(:)
    !! tSize(i) is the number of entries of obj(i)
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Initiates [[IntVector_]] from a vector of integers
!
! `obj%val` is reallocated to `SIZE(val)` entries and `val` is copied
! into it, converted to `INTEGER(I4B)`. Afterwards
! `Size(obj) = Capacity(obj) = SIZE(val)`. The same routines define
! the assignment `obj = val` for integer kinds INT8, INT16, INT32 and
! INT64.
!
! Changes:
!   - 2026-11-03: Sets tsize and capacity to SIZE(val)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Initiate
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! CALL Initiate(obj=obj, val=[1_I4B, 2_I4B, 3_I4B])
! CALL OK(Size(obj) .EQ. 3, "Initiate: Size is 3")
! CALL OK(ALL(obj%val(1:3) .EQ. [1, 2, 3]), "Initiate: values")
!
! obj = [4_I4B, 5_I4B]
! CALL OK(Size(obj) .EQ. 2, "obj = val: Size is 2")
! CALL OK(ALL(obj%val(1:2) .EQ. [4, 5]), "obj = val: values")
!
! END PROGRAM main
!```
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
    !! values to copy; each value must fit in INTEGER(I4B)
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
! `obj%val` is reallocated to `SIZE(val)` entries and each value is
! converted with `INT`, which truncates toward zero (1.7 gives 1, -1.7
! gives -1). Afterwards `Size(obj) = Capacity(obj) = SIZE(val)`. The
! same routines define the assignment `obj = val` for REAL32 and
! REAL64.
!
! Changes:
!   - 2026-11-03: Sets tsize and capacity to SIZE(val)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! obj = [1.7_DFP, -1.7_DFP, 3.0_DFP]
! CALL OK(Size(obj) .EQ. 3, "obj = real val: Size is 3")
! CALL OK(ALL(obj%val(1:3) .EQ. [1, -1, 3]), &
!         "obj = real val: truncated toward zero")
!
! END PROGRAM main
!```
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
! Only the active data `obj2%val(1:obj2%tsize)` is copied. `obj%val` is
! reallocated to exactly `obj2%tsize` entries, so after the call
! `Size(obj) = Capacity(obj) = Size(obj2)`. If `obj2%val` is not
! allocated, `obj%val` is deallocated and `obj` becomes empty.
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
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Capacity
! USE IntVector_Method, ONLY: Copy
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj, obj2, empty
!
! obj2 = [1_I4B, 2_I4B, 3_I4B]
!
! obj = obj2
! CALL OK(Size(obj) .EQ. 3, "obj = obj2: Size is 3")
! CALL OK(Capacity(obj) .EQ. 3, "obj = obj2: Capacity is 3")
! CALL OK(ALL(obj%val(1:3) .EQ. [1, 2, 3]), "obj = obj2: values")
!
! CALL Copy(obj, empty)
! CALL OK(Size(obj) .EQ. 0, "Copy from empty: Size is 0")
! CALL OK(.NOT. ALLOCATED(obj%val), "Copy from empty: val deallocated")
!
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate6(obj, obj2)
    TYPE(IntVector_), INTENT(INOUT) :: obj
    !! destination; its previous data is discarded
    TYPE(IntVector_), INTENT(IN) :: obj2
    !! source; only obj2%val(1:obj2%tsize) is copied
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
! Function form of [[obj_Initiate1]].
!
! Returns:
!   obj : TYPE(IntVector_) - Size(obj) = Capacity(obj) = tSize
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: IntVector
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! obj = IntVector(tSize=4_I4B)
! CALL OK(Size(obj) .EQ. 4, "IntVector(tSize): Size is 4")
! CALL OK(ALL(obj%val(1:4) .EQ. 0), "IntVector(tSize): entries are zero")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate1]], [[obj_Constructor_1]]

INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor1(tSize) RESULT(obj)
    TYPE(IntVector_) :: obj
    !! integer vector with tSize zero entries
    INTEGER(I4B), INTENT(IN) :: tSize
    !! number of entries, tSize >= 0
  END FUNCTION obj_Constructor1
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Converts an integer vector into [[IntVector_]]
!
! Function form of [[obj_Initiate4c]].
!
! Returns:
!   obj : TYPE(IntVector_) - copy of val, Size(obj) = SIZE(val)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: IntVector
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! obj = IntVector([3_I4B, 1_I4B, 2_I4B])
! CALL OK(Size(obj) .EQ. 3, "IntVector(val): Size is 3")
! CALL OK(ALL(obj%val(1:3) .EQ. [3, 1, 2]), "IntVector(val): values")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate4c]], [[obj_Constructor_2]]

INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor2(val) RESULT(obj)
    TYPE(IntVector_) :: obj
    !! integer vector holding a copy of val
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
! Function form of [[obj_Initiate5b]]; values are truncated toward
! zero.
!
! Returns:
!   obj : TYPE(IntVector_) - INT(val), Size(obj) = SIZE(val)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: DFP
! USE IntVector_Method, ONLY: IntVector
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! obj = IntVector([2.9_DFP, -0.5_DFP])
! CALL OK(Size(obj) .EQ. 2, "IntVector(real val): Size is 2")
! CALL OK(ALL(obj%val(1:2) .EQ. [2, 0]), &
!         "IntVector(real val): truncated toward zero")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate5b]], [[obj_Constructor_3]]

INTERFACE IntVector
  MODULE PURE FUNCTION obj_Constructor3(val) RESULT(obj)
    TYPE(IntVector_) :: obj
    !! integer vector holding INT(val)
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
! Allocates a new [[IntVector_]] and initiates it as in
! [[obj_Initiate1]]. The caller owns the result and must
! `DEALLOCATE` it.
!
! Returns:
!   obj : TYPE(IntVector_), POINTER - new vector with tSize zero entries
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: IntVector_Pointer
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_), POINTER :: obj
!
! obj => IntVector_Pointer(tSize=4_I4B)
! CALL OK(ASSOCIATED(obj), "IntVector_Pointer(tSize): associated")
! CALL OK(Size(obj) .EQ. 4, "IntVector_Pointer(tSize): Size is 4")
! DEALLOCATE (obj)
!
! END PROGRAM main
!```
!
! See also: [[obj_Constructor1]]

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_1(tSize) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    !! newly allocated integer vector with tSize zero entries
    INTEGER(I4B), INTENT(IN) :: tSize
    !! number of entries, tSize >= 0
  END FUNCTION obj_Constructor_1
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns a pointer to a new [[IntVector_]] copied from val
!
! Allocates a new [[IntVector_]] and initiates it from `val` as in
! [[obj_Initiate4c]]. The caller owns the result and must
! `DEALLOCATE` it.
!
! Returns:
!   obj : TYPE(IntVector_), POINTER - new vector holding a copy of val
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: IntVector_Pointer
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_), POINTER :: obj
!
! obj => IntVector_Pointer([1_I4B, 2_I4B])
! CALL OK(Size(obj) .EQ. 2, "IntVector_Pointer(val): Size is 2")
! CALL OK(ALL(obj%val(1:2) .EQ. [1, 2]), "IntVector_Pointer(val): values")
! DEALLOCATE (obj)
!
! END PROGRAM main
!```
!
! See also: [[obj_Constructor2]]

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_2(val) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    !! newly allocated integer vector holding a copy of val
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
! Allocates a new [[IntVector_]] and initiates it from `val` as in
! [[obj_Initiate5b]]; values are truncated toward zero. The caller
! owns the result and must `DEALLOCATE` it.
!
! Returns:
!   obj : TYPE(IntVector_), POINTER - new vector holding INT(val)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: DFP
! USE IntVector_Method, ONLY: IntVector_Pointer
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_), POINTER :: obj
!
! obj => IntVector_Pointer([1.5_DFP, 2.5_DFP])
! CALL OK(Size(obj) .EQ. 2, "IntVector_Pointer(real val): Size is 2")
! CALL OK(ALL(obj%val(1:2) .EQ. [1, 2]), &
!         "IntVector_Pointer(real val): truncated toward zero")
! DEALLOCATE (obj)
!
! END PROGRAM main
!```
!
! See also: [[obj_Constructor3]]

INTERFACE IntVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_3(val) RESULT(obj)
    TYPE(IntVector_), POINTER :: obj
    !! newly allocated integer vector holding INT(val)
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
! Defines `val = obj`. `val` is reallocated to `Size(obj)` entries and
! receives `obj%val(1:obj%tsize)`. If `obj%val` is not allocated, `val`
! becomes a zero-size array.
!
! Changes:
!   - 2026-11-03: Copies only obj%val(1:obj%tsize); an empty obj gives
!     a zero-size val instead of leaving val unchanged
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj, empty
! INTEGER(I4B), ALLOCATABLE :: val(:)
!
! obj = [1_I4B, 2_I4B, 3_I4B]
! val = obj
! CALL OK(SIZE(val) .EQ. 3, "val = obj: SIZE(val) is 3")
! CALL OK(ALL(val .EQ. [1, 2, 3]), "val = obj: values")
!
! val = empty
! CALL OK(SIZE(val) .EQ. 0, "val = empty obj: SIZE(val) is 0")
!
! END PROGRAM main
!```
!
! See also: [[obj_Convert1]]

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_Assign1(val, obj)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: val(:)
    !! destination, SIZE(val) = Size(obj) on return
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
! Same as `to = from`; see [[obj_Assign1]].
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Convert
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
! INTEGER(I4B), ALLOCATABLE :: val(:)
!
! obj = [5_I4B, 6_I4B]
! CALL Convert(from=obj, to=val)
! CALL OK(ALL(val .EQ. [5, 6]), "Convert: values")
!
! END PROGRAM main
!```
!
! See also: [[obj_Assign1]]

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert1(from, to)
    TYPE(IntVector_), INTENT(IN) :: from
    !! source integer vector
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: to(:)
    !! destination, SIZE(to) = Size(from) on return
  END SUBROUTINE obj_Convert1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary: Copies y into x, reallocating x to SIZE(y)
!
! `x` is reallocated to exactly `SIZE(y)` entries (also when it is
! already allocated with a different size), then `x = y`. Available
! for integer kinds INT8, INT16, INT32 and INT64.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: Copy
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: x(:)
!
! ALLOCATE (x(10))
! CALL Copy(x=x, y=[1_I4B, 2_I4B, 3_I4B])
! CALL OK(SIZE(x) .EQ. 3, "Copy: x reallocated to SIZE(y)")
! CALL OK(ALL(x .EQ. [1, 2, 3]), "Copy: values")
!
! END PROGRAM main
!```
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
! `x` is not reallocated; it must have at least
! `x_start + y_end - y_start` entries. Nothing is copied when
! `y_end < y_start`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: Copy_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: x(5)
!
! x = 0
! CALL Copy_(x=x, x_start=2_I4B, y=[10_I4B, 20_I4B, 30_I4B, 40_I4B], &
!            y_start=2_I4B, y_end=3_I4B)
! CALL OK(ALL(x .EQ. [0, 20, 30, 0, 0]), "Copy_: x(2:3) = y(2:3)")
!
! END PROGRAM main
!```
!
! See also: [[obj_Copy2_]]

INTERFACE Copy_
  MODULE PURE SUBROUTINE obj_Copy1_(x, x_start, y, y_start, y_end)
    INTEGER(I4B), INTENT(INOUT) :: x(:)
    !! destination, SIZE(x) >= x_start + y_end - y_start
    INTEGER(I4B), INTENT(IN) :: y(:)
    !! source
    INTEGER(I4B), INTENT(IN) :: x_start
    !! index in x where copying starts
    INTEGER(I4B), INTENT(IN) :: y_start, y_end
    !! range y(y_start:y_end) to copy
  END SUBROUTINE obj_Copy1_
END INTERFACE Copy_

!----------------------------------------------------------------------------
!                                                           Copy@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary: Copies y into the first SIZE(y) entries of x
!
! Sets `x(1:SIZE(y)) = y`; the rest of `x` is unchanged. `x` is not
! reallocated; it must have at least `SIZE(y)` entries.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: Copy_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: x(4)
!
! x = 0
! CALL Copy_(x=x, y=[7_I4B, 8_I4B])
! CALL OK(ALL(x .EQ. [7, 8, 0, 0]), "Copy_: x(1:2) = y")
!
! END PROGRAM main
!```
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
