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
! date: 2021-11-11
! summary: Get methods for [[IntVector_]]
!
! The generic `Get` reads entries from one [[IntVector_]] or from a vector
! of them, selected as all entries, a list of positions or an index
! triplet. The `datatype` argument selects the type and kind of the
! result; its value is not used. `GetIndex` searches for values. All
! methods use only the active entries `obj%val(1:obj%tsize)`.
!
! Changes:
!   - 2026-11-03: Renamed intVec_get_* to obj_Get*, removed GetPointer
!   - 2026-11-05: Fixed empty triplets, Get11 and scalar GetIndex

MODULE IntVector_GetMethod
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: Get
PUBLIC :: Get_
PUBLIC :: GetIndex

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns a copy of [[IntVector_]]
!
! Returns the active entries `obj%val(1:obj%tsize)` as a new
! [[IntVector_]]. The type of the result is selected by `datatype`; its
! value is not used, so pass `TypeIntVector` from `BaseType`. An empty
! `obj` gives an empty result. For an integer array result see
! [[obj_Get7a]].
!
! Changes:
!   - 2026-11-03: Renamed from intVec_get_1; only the active entries
!     val(1:tsize) are read
!
! Returns:
!   val : TYPE(IntVector_) - new vector with Size(val) = Capacity(val)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE BaseType, ONLY: TypeIntVector
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Capacity
! USE IntVector_Method, ONLY: Get
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj, empty, ans
! INTEGER(I4B), ALLOCATABLE :: arr(:)
!
! obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
!
! ! Test 1: the result is an IntVector_ copy
! ans = Get(obj, TypeIntVector)
! CALL OK(Size(ans) .EQ. 4, "Get: IntVector_ result, Size")
! CALL OK(ALL(ans%val(1:4) .EQ. [10, 20, 30, 40]), &
!         "Get: IntVector_ result, values")
!
! ! Test 2: the result is an integer array
! arr = Get(obj, 1_I4B)
! CALL OK(SIZE(arr) .EQ. 4, "Get: array result, SIZE")
! CALL OK(ALL(arr .EQ. [10, 20, 30, 40]), "Get: array result, values")
!
! ! Test 3: an empty obj gives an empty result
! ans = Get(empty, TypeIntVector)
! CALL OK(Size(ans) .EQ. 0, "Get: empty obj, IntVector_ result")
! arr = Get(empty, 1_I4B)
! CALL OK(SIZE(arr) .EQ. 0, "Get: empty obj, array result")
!
! ! Test 4: entries beyond tsize are not returned
! obj%tsize = 2
! ans = Get(obj, TypeIntVector)
! CALL OK(Size(ans) .EQ. 2, "Get: spare capacity, Size")
! CALL OK(Capacity(ans) .EQ. 2, "Get: result capacity = tsize")
! arr = Get(obj, 1_I4B)
! CALL OK(ALL(arr .EQ. [10, 20]), "Get: spare capacity, array result")
!
! END PROGRAM main
!```
!
! See also: [[obj_Get7a]], [[obj_Initiate6]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get1(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    TYPE(IntVector_), INTENT(IN) :: datatype
    !! selects an IntVector_ result, value is not used
    TYPE(IntVector_) :: val
    !! returned entries
  END FUNCTION obj_Get1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Like [[obj_Get1]] but without allocation.
!
! This method returns the val without allocation. This method
! is similar to [[obj_Get1]]
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_1(obj, ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    TYPE(IntVector_), INTENT(INOUT) :: ans
    !! returned entries
  END SUBROUTINE obj_Get_1
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns obj(indx) as an [[IntVector_]]
!
! Returns the entries of `obj` at the positions `indx`, in the order of
! `indx`. A position can be repeated. Every position must satisfy
! `1 <= indx(i) <= Size(obj)`. If `obj%val` is not allocated, the result
! has `SIZE(indx)` zeros. For an integer array result see [[obj_Get8a]],
! and for one position see [[obj_Get13a]].
!
! Changes:
!   - 2026-11-03: Renamed from intVec_get_2; only the active entries
!     val(1:tsize) are read
!
! Returns:
!   val : TYPE(IntVector_) - Size(val) = SIZE(indx)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE BaseType, ONLY: TypeIntVector
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Get
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj, ans
! INTEGER(I4B), ALLOCATABLE :: arr(:)
!
! obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
!
! ! Test 1: vector of positions, IntVector_ result; order is kept
! ans = Get(obj, [4_I4B, 1_I4B, 4_I4B], TypeIntVector)
! CALL OK(Size(ans) .EQ. 3, "Get: indx, IntVector_ result, Size")
! CALL OK(ALL(ans%val(1:3) .EQ. [40, 10, 40]), &
!         "Get: indx, IntVector_ result, values")
!
! ! Test 2: vector of positions, array result
! arr = Get(obj, [2_I4B, 3_I4B], 1_I4B)
! CALL OK(ALL(arr .EQ. [20, 30]), "Get: indx, array result")
!
! ! Test 3: no positions gives an empty result
! BLOCK
!   INTEGER(I4B) :: noindx(0)
!   ans = Get(obj, noindx, TypeIntVector)
!   CALL OK(Size(ans) .EQ. 0, "Get: empty indx, IntVector_ result")
!   arr = Get(obj, noindx, 1_I4B)
!   CALL OK(SIZE(arr) .EQ. 0, "Get: empty indx, array result")
! END BLOCK
!
! ! Test 4: one position gives a scalar
! CALL OK(Get(obj, 3_I4B, 1_I4B) .EQ. 30, "Get: scalar indx")
!
! ! Test 5: a scalar position beyond tsize gives zero
! CALL OK(Get(obj, 5_I4B, 1_I4B) .EQ. 0, "Get: scalar indx > tsize")
!
! END PROGRAM main
!```
!
! See also: [[obj_Get8a]], [[obj_Get13a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get2(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    TYPE(IntVector_), INTENT(IN) :: datatype
    !! selects an IntVector_ result, value is not used
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    TYPE(IntVector_) :: val
    !! returned entries
  END FUNCTION obj_Get2
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get the values from IntVector in the form of IntVector using indx
!
! This method is similar to [[obj_Get2]], but it will reallocates
! only if it is necessary.
!
INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_2(obj, indx, ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    TYPE(IntVector_), INTENT(INOUT) :: ans
    !! returned entries
  END SUBROUTINE obj_Get_2
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns obj(istart:iend:stride) as an [[IntVector_]]
!
! Returns the entries of `obj` selected by an index triplet. `iend` and
! `stride` are optional, so pass `datatype` by keyword when any of them
! is left out. The stride can be negative.
!
! A triplet that selects no entries, such as 3:2:2, gives an empty result.
!
! Changes:
!   - 2026-11-03: Renamed from intVec_get_3; only the active entries
!     val(1:tsize) are read
!   - 2026-11-05: A triplet that selects no entries gives an empty result
!
! Returns:
!   val : TYPE(IntVector_) - new vector with Size(val) = Capacity(val)
!
! Formula:
!   $$ \text{count} = \max\left(
!     \frac{iend - istart + stride}{stride}, 0\right) $$
!   with integer division.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE BaseType, ONLY: TypeIntVector
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Get
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj, ans
! INTEGER(I4B), ALLOCATABLE :: arr(:)
!
! obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B, 50_I4B]
!
! ! Test 1: full triplet, IntVector_ result
! ans = Get(obj, 1_I4B, 5_I4B, 2_I4B, TypeIntVector)
! CALL OK(Size(ans) .EQ. 3, "Get: triplet, IntVector_ result, Size")
! CALL OK(ALL(ans%val(1:3) .EQ. [10, 30, 50]), &
!         "Get: triplet, IntVector_ result, values")
!
! ! Test 2: iend and stride default to Size(obj) and 1
! ans = Get(obj, istart=3_I4B, datatype=TypeIntVector)
! CALL OK(ALL(ans%val(1:Size(ans)) .EQ. [30, 40, 50]), &
!         "Get: default iend and stride")
!
! ! Test 3: stride alone
! ans = Get(obj, istart=2_I4B, stride=2_I4B, datatype=TypeIntVector)
! CALL OK(ALL(ans%val(1:Size(ans)) .EQ. [20, 40]), "Get: default iend")
!
! ! Test 4: negative stride walks backwards
! ans = Get(obj, 5_I4B, 1_I4B, -2_I4B, TypeIntVector)
! CALL OK(ALL(ans%val(1:Size(ans)) .EQ. [50, 30, 10]), &
!         "Get: negative stride, IntVector_ result")
!
! ! Test 5: array result
! arr = Get(obj, 2_I4B, 4_I4B, 1_I4B, 1_I4B)
! CALL OK(ALL(arr .EQ. [20, 30, 40]), "Get: triplet, array result")
! arr = Get(obj, 5_I4B, 1_I4B, -2_I4B, 1_I4B)
! CALL OK(ALL(arr .EQ. [50, 30, 10]), "Get: negative stride, array result")
!
! ! Test 6: a stride larger than the range gives one entry
! arr = Get(obj, 2_I4B, 3_I4B, 5_I4B, 1_I4B)
! CALL OK(ALL(arr .EQ. [20]), "Get: stride larger than the range")
!
! END PROGRAM main
!```
!
! See also: [[obj_Get9a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get3(obj, istart, iend, stride, datatype) &
    RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    TYPE(IntVector_), INTENT(IN) :: datatype
    !! selects an IntVector_ result, value is not used
    INTEGER(I4B), INTENT(IN) :: istart
    !! first index
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iend, stride
    !! last index, default Size(obj); step, default 1, nonzero
    TYPE(IntVector_) :: val
    !! returned entries
  END FUNCTION obj_Get3
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get values from IntVector_ in another IntVector_ using triplets
!
! This method works like [[obj_Get3]] but it will reallocates only when
! necessary.

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_3(obj, istart, iend, stride, ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart
    !! first index
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iend, stride
    !! last index, default Size(obj); step, default 1, nonzero
    TYPE(IntVector_), INTENT(INOUT) :: ans
    !! returned entries
  END SUBROUTINE obj_Get_3
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Joins a vector of [[IntVector_]] into one [[IntVector_]]
!
! Returns the active entries of `obj(1)`, `obj(2)`, ... one after the
! other. Empty items add nothing. For an integer array result see
! [[obj_Get10a]].
!
! Changes:
!   - 2026-11-03: Renamed from intVec_get_4; only the active entries
!     val(1:tsize) are read
!
! Returns:
!   val : TYPE(IntVector_) - Size(val) is the sum of Size(obj(i))
!
! Formula:
!   $$ val = obj(1) \,//\, obj(2) \,//\, \cdots $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE BaseType, ONLY: TypeIntVector
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Get
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj(3), ans
! INTEGER(I4B), ALLOCATABLE :: arr(:)
!
! obj(1) = [1_I4B, 2_I4B]
! obj(3) = [3_I4B, 4_I4B, 5_I4B]
!
! ! Test 1: entries are joined in order, an empty item is skipped
! ans = Get(obj, TypeIntVector)
! CALL OK(Size(ans) .EQ. 5, "Get: join, IntVector_ result, Size")
! CALL OK(ALL(ans%val(1:5) .EQ. [1, 2, 3, 4, 5]), &
!         "Get: join, IntVector_ result, values")
!
! ! Test 2: array result
! arr = Get(obj, 1_I4B)
! CALL OK(SIZE(arr) .EQ. 5, "Get: join, array result, SIZE")
! CALL OK(ALL(arr .EQ. [1, 2, 3, 4, 5]), "Get: join, array result, values")
!
! ! Test 3: entries beyond tsize are not joined
! obj(3)%tsize = 1
! arr = Get(obj, 1_I4B)
! CALL OK(ALL(arr .EQ. [1, 2, 3]), "Get: join, spare capacity, array")
! ans = Get(obj, TypeIntVector)
! CALL OK(Size(ans) .EQ. 3, "Get: join, spare capacity, IntVector_")
!
! END PROGRAM main
!```
!
! See also: [[obj_Get10a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get4(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    TYPE(IntVector_), INTENT(IN) :: datatype
    !! selects an IntVector_ result, value is not used
    TYPE(IntVector_) :: val
    !! returned entries
  END FUNCTION obj_Get4
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get IntVector from a vector of IntVector.
!
! This method works same as [[obj_Get4]], but it will reallocate only
! when it is necessary.
!
INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_4(obj, ans)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    TYPE(IntVector_), INTENT(INOUT) :: ans
    !! returned entries
  END SUBROUTINE obj_Get_4
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Reads the same positions from each item of a vector
!
! Returns `obj(1)%val(indx)`, `obj(2)%val(indx)`, ... one after the
! other as an [[IntVector_]]. Every position must satisfy
! `1 <= indx(j) <= Size(obj(i))` for each item. For an integer array
! result see [[obj_Get11a]].
!
! Changes:
!   - 2026-11-03: Renamed from intVec_get_5; only the active entries
!     val(1:tsize) are read
!
! Returns:
!   val : TYPE(IntVector_) - Size(val) = SIZE(obj) * SIZE(indx)
!
! Formula:
!   $$ val((i - 1) m + j) = obj(i)\%val(indx(j)),
!   \quad m = \text{SIZE}(indx) $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE BaseType, ONLY: TypeIntVector
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Get
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj(2), ans
! INTEGER(I4B), ALLOCATABLE :: arr(:)
!
! obj(1) = [1_I4B, 2_I4B, 3_I4B]
! obj(2) = [4_I4B, 5_I4B, 6_I4B]
!
! ! Test 1: the same positions are read from each item
! ans = Get(obj, [3_I4B, 1_I4B], TypeIntVector)
! CALL OK(Size(ans) .EQ. 4, "Get: vector obj, indx, IntVector_, Size")
! CALL OK(ALL(ans%val(1:4) .EQ. [3, 1, 6, 4]), &
!         "Get: vector obj, indx, IntVector_, values")
!
! ! Test 2: array result
! arr = Get(obj, [3_I4B, 1_I4B], 1_I4B)
! CALL OK(SIZE(arr) .EQ. 4, "Get: vector obj, indx, array, SIZE")
! CALL OK(ALL(arr .EQ. [3, 1, 6, 4]), "Get: vector obj, indx, array, values")
!
! END PROGRAM main
!```
!
! See also: [[obj_Get11a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get5(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    TYPE(IntVector_), INTENT(IN) :: datatype
    !! selects an IntVector_ result, value is not used
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    TYPE(IntVector_) :: val
    !! returned entries
  END FUNCTION obj_Get5
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get IntVector from vector of IntVector using indx.
!
! This method is like [[obj_Get5]] but it will reallocate only if it is
! necessary.
!
INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_5(obj, indx, ans)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    TYPE(IntVector_), INTENT(INOUT) :: ans
    !! returned entries
  END SUBROUTINE obj_Get_5
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Applies one index triplet to each item of a vector
!
! Returns `obj(1)%val(istart:iend:stride)`,
! `obj(2)%val(istart:iend:stride)`, ... one after the other as an
! [[IntVector_]]. Unlike [[obj_Get3]], `iend` and `stride` are both
! required. The stride can be negative.
!
! A triplet that selects no entries, such as 3:2:2, gives an empty result.
!
! Changes:
!   - 2026-11-05: A triplet that selects no entries gives an empty result
!
! Returns:
!   val : TYPE(IntVector_) - Size(val) = SIZE(obj) * count
!
! Formula:
!   $$ \text{count} = \max\left(
!     \frac{iend - istart + stride}{stride}, 0\right) $$
!   with integer division.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE BaseType, ONLY: TypeIntVector
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Get
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj(2), ans
! INTEGER(I4B), ALLOCATABLE :: arr(:)
!
! obj(1) = [1_I4B, 2_I4B, 3_I4B]
! obj(2) = [4_I4B, 5_I4B, 6_I4B]
!
! ! Test 1: the same triplet is applied to each item
! ans = Get(obj, 1_I4B, 3_I4B, 2_I4B, TypeIntVector)
! CALL OK(Size(ans) .EQ. 4, "Get: vector obj, triplet, IntVector_, Size")
! CALL OK(ALL(ans%val(1:4) .EQ. [1, 3, 4, 6]), &
!         "Get: vector obj, triplet, IntVector_, values")
!
! ! Test 2: array result
! arr = Get(obj, 1_I4B, 3_I4B, 2_I4B, 1_I4B)
! CALL OK(SIZE(arr) .EQ. 4, "Get: vector obj, triplet, array, SIZE")
! CALL OK(ALL(arr .EQ. [1, 3, 4, 6]), "Get: vector obj, triplet, array")
!
! ! Test 3: negative stride
! arr = Get(obj, 3_I4B, 2_I4B, -1_I4B, 1_I4B)
! CALL OK(ALL(arr .EQ. [3, 2, 6, 5]), "Get: vector obj, negative stride")
!
! END PROGRAM main
!```
!
! See also: [[obj_Get12a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get6(obj, istart, iend, stride, datatype) &
    RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    TYPE(IntVector_), INTENT(IN) :: datatype
    !! selects an IntVector_ result, value is not used
    TYPE(IntVector_) :: val
    !! returned entries
  END FUNCTION obj_Get6
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get IntVector from a vector of IntVector using triples
!
! This method is like [[obj_Get6]], but it will reallocate only if it
! is necessary.

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_6(obj, istart, iend, stride, ans)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    TYPE(IntVector_), INTENT(INOUT) :: ans
    !! returned entries
  END SUBROUTINE obj_Get_6
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns the active entries as an integer array
!
! Returns `obj%val(1:obj%tsize)` converted to the kind of `datatype`.
! The value of `datatype` is not used. Values that do not fit in that
! kind are not checked. An empty `obj` gives a zero-size array.
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! See also: [[obj_Get1]] for the example

INTERFACE Get
  MODULE PURE FUNCTION obj_Get7a(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get7a
  MODULE PURE FUNCTION obj_Get7b(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get7b
  MODULE PURE FUNCTION obj_Get7c(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get7c
  MODULE PURE FUNCTION obj_Get7d(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(INT64), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT64), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get7d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get integer vector from IntVector
!
! These methods are similar to [[obj_Get7a]], [[obj_Get7b]], [[obj_Get7c]],
! [[obj_Get7d]]. But this method will not allocate ans.
!
! `SIZE(ans)` must be at least `Size(obj)`. Only `ans(1:tsize)` is written.
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_7a(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! the size of data written to ans
  END SUBROUTINE obj_Get_7a
  MODULE PURE SUBROUTINE obj_Get_7b(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(INT16), INTENT(INOUT) ::  ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! the size of data written to ans
  END SUBROUTINE obj_Get_7b
  MODULE PURE SUBROUTINE obj_Get_7c(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans.
  END SUBROUTINE obj_Get_7c
  MODULE PURE SUBROUTINE obj_Get_7d(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(INT64), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans.
  END SUBROUTINE obj_Get_7d
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns obj(indx) as an integer array
!
! Returns the entries of `obj` at the positions `indx`, in the order of
! `indx`, converted to the kind of `datatype`. Every position must
! satisfy `1 <= indx(i) <= Size(obj)`.
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! See also: [[obj_Get2]] for the example, [[obj_Get13a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get8a(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get8a
  MODULE PURE FUNCTION obj_Get8b(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get8b
  MODULE PURE FUNCTION obj_Get8c(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get8c
  MODULE PURE FUNCTION obj_Get8d(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT64), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT64), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get8d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get integer vector from IntVector using indx
!
! These methods are similar to [[obj_Get8a]], [[obj_Get8b]], [[obj_Get8c]],
! [[obj_Get8d]]. But this method will not allocate ans.
!
! `SIZE(ans)` must be at least `SIZE(indx)`. Only `ans(1:tsize)` is written.
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_8a(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_8a
  MODULE PURE SUBROUTINE obj_Get_8b(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_8b
  MODULE PURE SUBROUTINE obj_Get_8c(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_8c
  MODULE PURE SUBROUTINE obj_Get_8d(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT64), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_8d
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns obj(istart:iend:stride) as an integer array
!
! Returns the entries of `obj` selected by an index triplet, converted
! to the kind of `datatype`. The stride can be negative.
!
! A triplet that selects no entries, such as 3:2:2, gives an empty result.
!
! Changes:
!   - 2026-11-05: A triplet that selects no entries gives an empty result
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! Formula:
!   $$ \text{count} = \max\left(
!     \frac{iend - istart + stride}{stride}, 0\right) $$
!   with integer division.
!
! See also: [[obj_Get3]] for the example

INTERFACE Get
  MODULE PURE FUNCTION obj_Get9a(obj, istart, iend, stride, datatype) &
    RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get9a
  MODULE PURE FUNCTION obj_Get9b(obj, istart, iend, stride, datatype) &
    RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get9b
  MODULE PURE FUNCTION obj_Get9c(obj, istart, iend, stride, datatype) &
    RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get9c
  MODULE PURE FUNCTION obj_Get9d(obj, istart, iend, stride, datatype) &
    RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT64), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT64), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get9d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get integer vector from IntVector using triplets
!
! These methods are similar to [[obj_Get9a]], [[obj_Get9b]],
! [[obj_Get9c]], [[obj_Get9d]]. But this method will not allocate ans.
!
! `SIZE(ans)` must be at least the number of entries selected by the
! triplet. Only `ans(1:tsize)` is written.
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_9a(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_9a
  MODULE PURE SUBROUTINE obj_Get_9b(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_9b
  MODULE PURE SUBROUTINE obj_Get_9c(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_9c
  MODULE PURE SUBROUTINE obj_Get_9d(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT64), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_9d
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Joins a vector of [[IntVector_]] into an integer array
!
! Returns the active entries of `obj(1)`, `obj(2)`, ... one after the
! other, converted to the kind of `datatype`. Empty items add nothing.
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! See also: [[obj_Get4]] for the example

INTERFACE Get
  MODULE PURE FUNCTION obj_Get10a(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get10a
  MODULE PURE FUNCTION obj_Get10b(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get10b
  MODULE PURE FUNCTION obj_Get10c(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get10c
  MODULE PURE FUNCTION obj_Get10d(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT64), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT64), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get10d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get integer vector from a vector of IntVector
!
! These methods are similar to [[obj_Get10a]], [[obj_Get10b]],
! [[obj_Get10c]], [[obj_Get10d]]. But this method will not allocate ans.
!
! `SIZE(ans)` must be at least the sum of `Size(obj(i))`. Only
! `ans(1:tsize)` is written.
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_10a(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_10a
  MODULE PURE SUBROUTINE obj_Get_10b(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_10b
  MODULE PURE SUBROUTINE obj_Get_10c(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_10c
  MODULE PURE SUBROUTINE obj_Get_10d(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT64), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_10d
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Reads the same positions from each item, as an array
!
! Returns `obj(1)%val(indx)`, `obj(2)%val(indx)`, ... one after the
! other, converted to the kind of `datatype`. Every position must
! satisfy `1 <= indx(j) <= Size(obj(i))` for each item.
!
! Changes:
!   - 2026-11-05: Fixed the crash from an uninitialised counter
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! See also: [[obj_Get5]] for the example

INTERFACE Get
  MODULE PURE FUNCTION obj_Get11a(obj, indx, datatype) &
    & RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    INTEGER(INT8), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get11a
  MODULE PURE FUNCTION obj_Get11b(obj, indx, datatype) &
    & RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    INTEGER(INT16), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get11b
  MODULE PURE FUNCTION obj_Get11c(obj, indx, datatype) &
    & RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    INTEGER(INT32), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get11c
  MODULE PURE FUNCTION obj_Get11d(obj, indx, datatype) &
    & RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(INT64), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    INTEGER(INT64), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get11d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get integer vector from a vector of IntVector using indx
!
! These methods are similar to [[obj_Get11a]], [[obj_Get11b]],
! [[obj_Get11c]], [[obj_Get11d]]. But this method will not allocate ans.
!
! `SIZE(ans)` must be at least `SIZE(obj) * SIZE(indx)`. Only
! `ans(1:tsize)` is written.
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_11a(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_11a
  MODULE PURE SUBROUTINE obj_Get_11b(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_11b
  MODULE PURE SUBROUTINE obj_Get_11c(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_11c
  MODULE PURE SUBROUTINE obj_Get_11d(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions read from each obj(i)
    INTEGER(INT64), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_11d
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Applies one index triplet to each item, as an array
!
! Returns `obj(1)%val(istart:iend:stride)`,
! `obj(2)%val(istart:iend:stride)`, ... one after the other, converted
! to the kind of `datatype`. The stride can be negative.
!
! A triplet that selects no entries, such as 3:2:2, gives an empty result.
!
! Changes:
!   - 2026-11-05: A triplet that selects no entries gives an empty result
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! Formula:
!   $$ \text{count} = \max\left(
!     \frac{iend - istart + stride}{stride}, 0\right) $$
!   with integer division.
!
! See also: [[obj_Get6]] for the example

INTERFACE Get
  MODULE PURE FUNCTION obj_Get12a(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    !! first index, last index, nonzero step
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get12a
  MODULE PURE FUNCTION obj_Get12b(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    !! first index, last index, nonzero step
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get12b
  MODULE PURE FUNCTION obj_Get12c(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    !! first index, last index, nonzero step
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get12c
  MODULE PURE FUNCTION obj_Get12d(obj, iStart, iEnd, &
    & Stride, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    !! first index, last index, nonzero step
    INTEGER(INT64), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT64), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get12d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                               Get_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Get integer vector from a vector of IntVector using triplets
!
! These methods are similar to [[obj_Get12a]], [[obj_Get12b]],
! [[obj_Get12c]], [[obj_Get12d]]. But this method will not allocate ans.
!
! `SIZE(ans)` must be at least `SIZE(obj)` times the number of entries
! selected by the triplet. Only `ans(1:tsize)` is written.
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_12a(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_12a
  MODULE PURE SUBROUTINE obj_Get_12b(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_12b
  MODULE PURE SUBROUTINE obj_Get_12c(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_12c
  MODULE PURE SUBROUTINE obj_Get_12d(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vector of integer vectors, read in order
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT64), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_12d
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns one entry of [[IntVector_]]
!
! Returns `obj%val(indx)` converted to the kind of `datatype`. If
! `indx > Size(obj)` the result is zero. `indx >= 1` is needed.
!
! Returns:
!   val : INTEGER - kind of datatype
!
! See also: [[obj_Get2]] for the example, [[obj_Get8a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get13a(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx
    !! position to read, indx >= 1
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8) :: val
    !! returned entry
  END FUNCTION obj_Get13a
  MODULE PURE FUNCTION obj_Get13b(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx
    !! position to read, indx >= 1
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16) :: val
    !! returned entry
  END FUNCTION obj_Get13b
  MODULE PURE FUNCTION obj_Get13c(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx
    !! position to read, indx >= 1
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32) :: val
    !! returned entry
  END FUNCTION obj_Get13c
  MODULE PURE FUNCTION obj_Get13d(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: indx
    !! position to read, indx >= 1
    INTEGER(INT64), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT64) :: val
    !! returned entry
  END FUNCTION obj_Get13d
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                         GetIndex@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns the first position of a value
!
! Returns the first position of `val` in `obj%val(1:obj%tsize)`, or zero
! if `val` is absent. An empty `obj` gives zero. The result is the same
! as that of [[obj_GetIndex2]] for one value.
!
! Changes:
!   - 2026-11-05: An absent value gives zero, not the nearest one
!
! Returns:
!   ans : INTEGER(I4B) - position in 1:Size(obj), 0 if val is absent
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: GetIndex
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj, empty
!
! obj = [5_I4B, 7_I4B, 5_I4B, 9_I4B]
!
! ! Test 1: scalar value, first position is returned
! CALL OK(GetIndex(obj, 7_I4B) .EQ. 2, "GetIndex: scalar value")
! CALL OK(GetIndex(obj, 5_I4B) .EQ. 1, "GetIndex: repeated value")
!
! ! Test 2: vector of values, 0 means not found
! CALL OK(ALL(GetIndex(obj, [9_I4B, 1_I4B, 5_I4B, 5_I4B]) .EQ. &
!             [4, 0, 1, 1]), "GetIndex: vector of values")
!
! ! Test 3: empty vector of values gives an empty result
! BLOCK
!   INTEGER(I4B) :: novalue(0)
!   CALL OK(SIZE(GetIndex(obj, novalue)) .EQ. 0, "GetIndex: empty val")
! END BLOCK
!
! ! Test 4: nothing is found in an empty obj
! CALL OK(GetIndex(empty, 1_I4B) .EQ. 0, "GetIndex: empty obj, scalar")
! CALL OK(ALL(GetIndex(empty, [1_I4B, 2_I4B]) .EQ. 0), &
!         "GetIndex: empty obj, vector")
!
! ! Test 5: entries beyond tsize are not searched
! obj%tsize = 3
! CALL OK(ALL(GetIndex(obj, [5_I4B, 9_I4B]) .EQ. [1, 0]), &
!         "GetIndex: vector of values with tsize = 3")
!
! END PROGRAM main
!```
!
! See also: [[obj_GetIndex2]]

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex1(obj, val) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: val
    !! value to search
    INTEGER(I4B) :: ans
    !! position of the value, 0 if absent
  END FUNCTION obj_GetIndex1
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns the positions of a vector of values
!
! For each `val(j)`, returns the first position of that value in
! `obj%val(1:obj%tsize)`, or zero if the value is absent. The cost is
! `SIZE(val) * Size(obj)`.
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE, ans(:) - SIZE(ans) = SIZE(val)
!
! See also: [[obj_GetIndex1]] for the example

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex2(obj, val) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only obj%val(1:obj%tsize) is read
    INTEGER(I4B), INTENT(IN) :: val(:)
    !! values to search
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! position of each value, 0 if absent
  END FUNCTION obj_GetIndex2
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE IntVector_GetMethod
