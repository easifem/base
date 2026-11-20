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
! The generic `Get` reads entries from one [[IntVector_]] or from an
! array of them. You select all entries, a list of positions or an index
! triplet. The `datatype` argument sets the type and kind of the result.
! Its value is not used.
!
! `GetIndex` searches for values. All methods use only the active
! entries `obj%val(1:obj%tsize)`.
!
! Changes:
!   - 2026-11-03: Renamed `intVec_get_*` to `obj_Get*`, removed GetPointer
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
! Returns the active entries `obj%val(1:tsize)` as a new [[IntVector_]].
! An empty `obj` gives an empty result.
!
! The type of the result is selected by `datatype`. Its value is not
! used, so pass `TypeIntVector` from `BaseType`.
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
! - [test 1](|base|IntVector/examples/Get_test_1.F90)
!
! See also: [[obj_Get_1]], [[obj_Get7a]], [[obj_Initiate6]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get1(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Copies the active entries `obj%val(1:tsize)` into `ans`.
!
! 1. If `ans%val` is not allocated, the routine allocates it.
! 2. If `capacity` of `ans` is smaller than `tsize` of `obj`, the
!    routine makes `ans` larger.
! 3. Otherwise `capacity` of `ans` does not change.
! 4. The routine sets `tsize` of `ans` to `tsize` of `obj`.
!
! Entries of `ans%val` beyond the new `tsize` do not change.
!
! Examples
!
! - [test 1](|base|IntVector/examples/Get__test_1.F90)
!
! See also: [[obj_Get1]]

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_1(obj, ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! `1 <= indx(i) <= Size(obj)`.
!
! If `obj%val` is not allocated, the result has `SIZE(indx)` zeros.
! The type of the result is selected by `datatype`. Its value is not
! used, so pass `TypeIntVector` from `BaseType`.
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
! - [test 2](|base|IntVector/examples/Get_test_2.F90)
!
! See also: [[obj_Get_2]], [[obj_Get8a]], [[obj_Get13a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get2(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Copies the entries of `obj` at the positions `indx` into `ans`, in the
! order of `indx`. Every position must satisfy
! `1 <= indx(i) <= Size(obj)`.
!
! 1. If `ans%val` is not allocated, the routine allocates it.
! 2. If `capacity` of `ans` is smaller than `SIZE(indx)`, the routine
!    makes `ans` larger.
! 3. Otherwise `capacity` of `ans` does not change.
! 4. The routine sets `tsize` of `ans` to `SIZE(indx)`.
!
! Old values in `ans%val(1:tsize)` are cleared first. If `obj%val` is not
! allocated, `ans` holds `SIZE(indx)` zeros.
!
! Examples
!
! - [test 2](|base|IntVector/examples/Get__test_2.F90)
!
! See also: [[obj_Get2]]

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_2(obj, indx, ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Returns the entries of `obj` selected by the triplet
! `istart:iend:stride` as a new [[IntVector_]].
! `iend` defaults to `Size(obj)` and `stride` defaults to 1.
! The stride can be negative, but not zero.
! Pass `datatype` by keyword when `iend` or `stride` is left out.
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
! - [test 3](|base|IntVector/examples/Get_test_3.F90)
!
! See also: [[obj_Get_3]], [[obj_Get9a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get3(obj, istart, iend, stride, datatype) &
    RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Copies the entries of `obj` selected by an index triplet into `ans`.
! The triplet works as in [[obj_Get3]].
!
! 1. If `ans%val` is not allocated, the routine allocates it.
! 2. If `capacity` of `ans` is smaller than the number of selected
!    entries, the routine makes `ans` larger.
! 3. Otherwise `capacity` of `ans` does not change.
! 4. The routine sets `tsize` of `ans` to the number of selected entries.
!
! Old values in `ans%val(1:tsize)` are cleared first, so a skipped entry is 0.
! A triplet that selects no entries gives `tsize = 0`.
!
! Examples
!
! - [test 3](|base|IntVector/examples/Get__test_3.F90)
!
! See also: [[obj_Get3]]

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_3(obj, istart, iend, stride, ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Joins the active entries of `obj(1)`, `obj(2)`, ... one after the
! other into a new [[IntVector_]]. Empty items add nothing.
!
! The type of the result is selected by `datatype`. Its value is not
! used, so pass `TypeIntVector` from `BaseType`.
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
! - [test 4](|base|IntVector/examples/Get_test_4.F90)
!
! See also: [[obj_Get_4]], [[obj_Get10a]]

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
! summary: Joins a vector of [[IntVector_]] into an existing [[IntVector_]]
!
! Copies the active entries of `obj(1)`, `obj(2)`, ... one after the
! other into `ans`. Empty items add nothing.
!
! 1. If `ans%val` is not allocated, the routine allocates it.
! 2. If `capacity` of `ans` is smaller than the total number of entries,
!    the routine makes `ans` larger.
! 3. Otherwise `capacity` of `ans` does not change.
! 4. The routine sets `tsize` of `ans` to the total number of entries.
!
! Examples
!
! - [test 4](|base|IntVector/examples/Get__test_4.F90)
!
! See also: [[obj_Get4]]

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
! - [test 5](|base|IntVector/examples/Get_test_5.F90)
!
! See also: [[obj_Get_5]], [[obj_Get11a]]

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
! summary: Reads the same positions from each item into an [[IntVector_]]
!
! Copies `obj(1)%val(indx)`, `obj(2)%val(indx)`, ... one after the other
! into `ans`. Every position must satisfy
! `1 <= indx(j) <= Size(obj(i))` for each item.
!
! 1. If `ans%val` is not allocated, the routine allocates it.
! 2. If `capacity` of `ans` is smaller than `SIZE(obj) * SIZE(indx)`,
!    the routine makes `ans` larger.
! 3. Otherwise `capacity` of `ans` does not change.
! 4. The routine sets `tsize` of `ans` to `SIZE(obj) * SIZE(indx)`.
!
! Old values in `ans%val(1:tsize)` are cleared first.
!
! Examples
!
! - [test 5](|base|IntVector/examples/Get__test_5.F90)
!
! See also: [[obj_Get5]]

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
! - [test 6](|base|IntVector/examples/Get_test_6.F90)
! - [test 7](|base|IntVector/examples/Get_test_7.F90) with an empty triplet
!
! See also: [[obj_Get_6]], [[obj_Get12a]]

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
! summary: Applies one index triplet to each item, into an [[IntVector_]]
!
! Copies `obj(1)%val(istart:iend:stride)`,
! `obj(2)%val(istart:iend:stride)`, ... one after the other into `ans`.
! The triplet works as in [[obj_Get6]]. The stride can be negative.
!
! 1. If `ans%val` is not allocated, the routine allocates it.
! 2. If `capacity` of `ans` is smaller than the number of copied entries,
!    the routine makes `ans` larger.
! 3. Otherwise `capacity` of `ans` does not change.
! 4. The routine sets `tsize` of `ans` to the number of copied entries.
!
! Old values in `ans%val(1:tsize)` are cleared first. A triplet that
! selects no entries gives `tsize = 0`.
!
! Examples
!
! - [test 6](|base|IntVector/examples/Get__test_6.F90)
!
! See also: [[obj_Get6]]

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
! Each kind has its own specific procedure: `INT8` (`obj_Get7a`),
! `INT16` (`obj_Get7b`), `INT32` (`obj_Get7c`) and `INT64` (`obj_Get7d`).
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! Examples
!
! - [test 1](|base|IntVector/examples/Get_test_1.F90) with an array result
!
! See also: [[obj_Get_7a]], [[obj_Get1]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get7a(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get7a
  MODULE PURE FUNCTION obj_Get7b(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get7b
  MODULE PURE FUNCTION obj_Get7c(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get7c
  MODULE PURE FUNCTION obj_Get7d(obj, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! summary: Writes the active entries into an integer array
!
! Writes `obj%val(1:obj%tsize)` into `ans(1:tsize)`, converted to the
! kind of `ans`. The routine never allocates `ans`.
!
! `SIZE(ans)` must be at least `Size(obj)`. Entries of `ans` beyond
! `tsize` do not change. Values that do not fit in the kind of `ans`
! are not checked.
!
! Each kind has its own specific procedure: `INT8` (`obj_Get_7a`),
! `INT16` (`obj_Get_7b`), `INT32` (`obj_Get_7c`) and `INT64`
! (`obj_Get_7d`).
!
! Examples
!
! - [test 7](|base|IntVector/examples/Get__test_7.F90)
!
! See also: [[obj_Get7a]]

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_7a(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! the size of data written to ans
  END SUBROUTINE obj_Get_7a
  MODULE PURE SUBROUTINE obj_Get_7b(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(INT16), INTENT(INOUT) ::  ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! the size of data written to ans
  END SUBROUTINE obj_Get_7b
  MODULE PURE SUBROUTINE obj_Get_7c(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans.
  END SUBROUTINE obj_Get_7c
  MODULE PURE SUBROUTINE obj_Get_7d(obj, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Each kind has its own specific procedure: `INT8` (`obj_Get8a`),
! `INT16` (`obj_Get8b`), `INT32` (`obj_Get8c`) and `INT64` (`obj_Get8d`).
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! Examples
!
! - [test 2](|base|IntVector/examples/Get_test_2.F90) with an array result
!
! See also: [[obj_Get_8a]], [[obj_Get2]], [[obj_Get13a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get8a(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get8a
  MODULE PURE FUNCTION obj_Get8b(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get8b
  MODULE PURE FUNCTION obj_Get8c(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32), ALLOCATABLE :: val(:)
    !! returned entries
  END FUNCTION obj_Get8c
  MODULE PURE FUNCTION obj_Get8d(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! summary: Writes the entries at given positions into an integer array
!
! Writes the entries of `obj` at the positions `indx` into `ans(1:tsize)`,
! in the order of `indx`, converted to the kind of `ans`. Every position
! must satisfy `1 <= indx(i) <= Size(obj)`. The routine never allocates
! `ans`.
!
! `SIZE(ans)` must be at least `SIZE(indx)`. The routine sets `tsize` to
! `SIZE(indx)`. It clears `ans(1:tsize)` first. Entries of `ans` beyond
! `tsize` do not change.
!
! Each kind has its own specific procedure: `INT8` (`obj_Get_8a`),
! `INT16` (`obj_Get_8b`), `INT32` (`obj_Get_8c`) and `INT64`
! (`obj_Get_8d`).
!
! Examples
!
! - [test 8](|base|IntVector/examples/Get__test_8.F90)
!
! See also: [[obj_Get8a]]

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_8a(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_8a
  MODULE PURE SUBROUTINE obj_Get_8b(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_8b
  MODULE PURE SUBROUTINE obj_Get_8c(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to read, 1 <= indx(i) <= Size(obj)
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_8c
  MODULE PURE SUBROUTINE obj_Get_8d(obj, indx, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Examples
!
! - [test 3](|base|IntVector/examples/Get_test_3.F90) with an array result
! - [test 7](|base|IntVector/examples/Get_test_7.F90) with an empty triplet
!
! See also: [[obj_Get_9a]], [[obj_Get3]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get9a(obj, istart, iend, stride, datatype) &
    RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! summary: Writes the entries of an index triplet into an integer array
!
! Writes the entries of `obj` selected by `istart:iend:stride` into
! `ans(1:tsize)`, converted to the kind of `ans`. The stride can be
! negative. The routine never allocates `ans`.
!
! `SIZE(ans)` must be at least the number of selected entries. The routine
! sets `tsize` to that number. It clears `ans(1:tsize)` first. Entries of
! `ans` beyond `tsize` do not change. A triplet that selects no entries,
! such as 3:2:2, gives `tsize = 0`.
!
! Each selected index must satisfy `1 <= index <= Size(obj)`.
!
! Each kind has its own specific procedure: `INT8` (`obj_Get_9a`),
! `INT16` (`obj_Get_9b`), `INT32` (`obj_Get_9c`) and `INT64`
! (`obj_Get_9d`).
!
! Formula:
!   $$ \text{tsize} = \max\left(
!     \frac{iend - istart + stride}{stride}, 0\right) $$
!   with integer division.
!
! Examples
!
! - [test 9](|base|IntVector/examples/Get__test_9.F90)
!
! See also: [[obj_Get9a]]

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_9a(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_9a
  MODULE PURE SUBROUTINE obj_Get_9b(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_9b
  MODULE PURE SUBROUTINE obj_Get_9c(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! first index, last index, nonzero step
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    !! returned entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written to ans
  END SUBROUTINE obj_Get_9c
  MODULE PURE SUBROUTINE obj_Get_9d(obj, istart, iend, stride, ans, tsize)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Each kind has its own specific procedure: `INT8` (`obj_Get10a`),
! `INT16` (`obj_Get10b`), `INT32` (`obj_Get10c`) and `INT64`
! (`obj_Get10d`).
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! Examples
!
! - [test 4](|base|IntVector/examples/Get_test_4.F90) with an array result
!
! See also: [[obj_Get_10a]], [[obj_Get4]]

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
! summary: Writes a vector of [[IntVector_]] into an integer array
!
! Writes the active entries of `obj(1)`, `obj(2)`, ... one after the
! other into `ans(1:tsize)`, converted to the kind of `ans`. Empty items
! add nothing. The routine never allocates `ans`.
!
! `SIZE(ans)` must be at least the sum of `Size(obj(i))`. The routine sets
! `tsize` to that sum. Entries of `ans` beyond `tsize` do not change.
!
! Each kind has its own specific procedure: `INT8` (`obj_Get_10a`),
! `INT16` (`obj_Get_10b`), `INT32` (`obj_Get_10c`) and `INT64`
! (`obj_Get_10d`).
!
! Examples
!
! - [test 10](|base|IntVector/examples/Get__test_10.F90)
!
! See also: [[obj_Get10a]]

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
! Each kind has its own specific procedure: `INT8` (`obj_Get11a`),
! `INT16` (`obj_Get11b`), `INT32` (`obj_Get11c`) and `INT64`
! (`obj_Get11d`).
!
! Returns:
!   val : INTEGER, ALLOCATABLE, val(:) - kind of datatype
!
! Examples
!
! - [test 5](|base|IntVector/examples/Get_test_5.F90) with an array result
!
! See also: [[obj_Get_11a]], [[obj_Get5]]

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
! summary: Writes the same positions of each item into an integer array
!
! Writes `obj(1)%val(indx)`, `obj(2)%val(indx)`, ... one after the other
! into `ans(1:tsize)`, converted to the kind of `ans`. The routine never
! allocates `ans`.
!
! `SIZE(ans)` must be at least `SIZE(obj) * SIZE(indx)`. The routine sets
! `tsize` to that product. It clears `ans(1:tsize)` first. Entries of
! `ans` beyond `tsize` do not change.
!
! Every position must satisfy `1 <= indx(j) <= Size(obj(i))` for each
! item.
!
! Each kind has its own specific procedure: `INT8` (`obj_Get_11a`),
! `INT16` (`obj_Get_11b`), `INT32` (`obj_Get_11c`) and `INT64`
! (`obj_Get_11d`).
!
! Examples
!
! - [test 11](|base|IntVector/examples/Get__test_11.F90)
!
! See also: [[obj_Get11a]]

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
! Each kind has its own specific procedure: `INT8` (`obj_Get12a`),
! `INT16` (`obj_Get12b`), `INT32` (`obj_Get12c`) and `INT64`
! (`obj_Get12d`).
!
! Examples
!
! - [test 6](|base|IntVector/examples/Get_test_6.F90) with an array result
! - [test 7](|base|IntVector/examples/Get_test_7.F90) with an empty triplet
!
! See also: [[obj_Get_12a]], [[obj_Get6]]

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
! summary: Writes one index triplet of each item into an integer array
!
! Writes `obj(1)%val(istart:iend:stride)`,
! `obj(2)%val(istart:iend:stride)`, ... one after the other into
! `ans(1:tsize)`, converted to the kind of `ans`. The stride can be
! negative. The routine never allocates `ans`.
!
! `SIZE(ans)` must be at least `SIZE(obj)` times the number of entries
! selected by the triplet. The routine sets `tsize` to that product. It
! clears `ans(1:tsize)` first. Entries of `ans` beyond `tsize` do not
! change. A triplet that selects no entries gives `tsize = 0`.
!
! Each selected index must satisfy `1 <= index <= Size(obj(i))` for each
! item.
!
! Each kind has its own specific procedure: `INT8` (`obj_Get_12a`),
! `INT16` (`obj_Get_12b`), `INT32` (`obj_Get_12c`) and `INT64`
! (`obj_Get_12d`).
!
! Formula:
!   $$ \text{tsize} = \text{SIZE}(obj) \max\left(
!     \frac{iend - istart + stride}{stride}, 0\right) $$
!   with integer division.
!
! Examples
!
! - [test 12](|base|IntVector/examples/Get__test_12.F90)
!
! See also: [[obj_Get12a]]

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
! `indx > Size(obj)`, the result is zero. The routine does not check
! `indx >= 1`, so the caller must make sure of it.
!
! Each kind has its own specific procedure: `INT8` (`obj_Get13a`),
! `INT16` (`obj_Get13b`), `INT32` (`obj_Get13c`) and `INT64`
! (`obj_Get13d`).
!
! Returns:
!   val : INTEGER - kind of datatype
!
! Examples
!
! - [test 2](|base|IntVector/examples/Get_test_2.F90) with a scalar `indx`
!
! See also: [[obj_Get2]], [[obj_Get8a]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get13a(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx
    !! position to read, indx >= 1
    INTEGER(INT8), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT8) :: val
    !! returned entry
  END FUNCTION obj_Get13a
  MODULE PURE FUNCTION obj_Get13b(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx
    !! position to read, indx >= 1
    INTEGER(INT16), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT16) :: val
    !! returned entry
  END FUNCTION obj_Get13b
  MODULE PURE FUNCTION obj_Get13c(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
    INTEGER(I4B), INTENT(IN) :: indx
    !! position to read, indx >= 1
    INTEGER(INT32), INTENT(IN) :: datatype
    !! selects the kind of the result, value is not used
    INTEGER(INT32) :: val
    !! returned entry
  END FUNCTION obj_Get13c
  MODULE PURE FUNCTION obj_Get13d(obj, indx, datatype) RESULT(val)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! - [test 1](|base|IntVector/examples/GetIndex_test_1.F90) with repeated values
! - [test 2](|base|IntVector/examples/GetIndex_test_2.F90) with absent values
!
! See also: [[obj_GetIndex2]]

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex1(obj, val) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
! Examples
!
! - [test 1](|base|IntVector/examples/GetIndex_test_1.F90)
!
! See also: [[obj_GetIndex1]]

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex2(obj, val) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, only `obj%val(1:obj%tsize)` is read
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
