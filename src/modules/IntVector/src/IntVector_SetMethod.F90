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
! date: 2021-02-25
! summary: Set methods for [[IntVector_]]
!
! `Set` writes entries of an [[IntVector_]]: at a list of positions, at one
! position, all active entries to a constant, or the leading active entries
! from an array. `SetTotalDimension` sets the rank stored in the vector.
! `RemoveDuplicates` sorts the vector and keeps the unique entries.
!
! None of the `Set` methods allocate or grow the vector, so `tsize` and
! `capacity` do not change. Allocate `obj` first, for example with
! [[Initiate]] or `obj = val`.
!
! Changes:
!   - 2026-11-05: Removed the generic Repeat from this module

MODULE IntVector_SetMethod
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: SetTotalDimension
PUBLIC :: Set
PUBLIC :: RemoveDuplicates

!----------------------------------------------------------------------------
!                                               SetTotalDimension@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: Set the total dimension (rank) of [[IntVector_]]
!
! Writes `tDimension` to `obj%tDimension`. The value is not checked, and
! `val`, `tsize` and `capacity` do not change, so `obj` may be unallocated.
! The default value is 1. To read it use [[obj_GetTotalDimension]].
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: GetTotalDimension
! USE IntVector_Method, ONLY: SetTotalDimension
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! ! Test 1: the default value is 1
! BLOCK
!   CALL OK(GetTotalDimension(obj) .EQ. 1, "SetTotalDimension: default")
! END BLOCK
!
! ! Test 2: an unallocated obj can be set
! BLOCK
!   CALL SetTotalDimension(obj, 3_I4B)
!   CALL OK(GetTotalDimension(obj) .EQ. 3, "SetTotalDimension: unallocated")
! END BLOCK
!
! ! Test 3: the data is not changed
! BLOCK
!   obj = [10_I4B, 20_I4B, 30_I4B]
!   CALL SetTotalDimension(obj, 2_I4B)
!   CALL OK(GetTotalDimension(obj) .EQ. 2, "SetTotalDimension: new value")
!   CALL OK(Size(obj) .EQ. 3, "SetTotalDimension: Size is not changed")
!   CALL OK(ALL(obj%val .EQ. [10, 20, 30]), &
!           "SetTotalDimension: values are not changed")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalDimension]]

INTERFACE SetTotalDimension
  MODULE PURE SUBROUTINE obj_SetTotalDimension(obj, tDimension)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! vector whose rank is set
    INTEGER(I4B), INTENT(IN) :: tDimension
    !! total dimension (rank)
  END SUBROUTINE obj_SetTotalDimension
END INTERFACE SetTotalDimension

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: Set the entries of [[IntVector_]] at a list of positions
!
! Writes `VALUE(ii)` to `obj%val(indx(ii))`. When `VALUE` has one entry,
! that entry is written to every position in `indx`. Otherwise `VALUE` and
! `indx` must have the same size. `tsize` and `capacity` do not change.
!
! Each position in `indx` should appear only once. In a debug build the
! routine returns without changing `obj` when `val` is not allocated or an
! entry of `indx` exceeds `tsize`. It gives no message, because the routine
! is pure. A release build does not check the bounds.
!
! Changes:
!   - 2026-11-05: Checks run only in debug builds; added a check against
!     tsize; val must be allocated in a release build
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Set
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
!
! ! Test 1: each position gets its own value
! BLOCK
!   CALL Set(obj, [1_I4B, 3_I4B], [-1_I4B, -3_I4B])
!   CALL OK(ALL(obj%val .EQ. [-1, 20, -3, 40]), "Set: list of values")
! END BLOCK
!
! ! Test 2: positions can be in any order
! BLOCK
!   CALL Set(obj, [4_I4B, 2_I4B], [44_I4B, 22_I4B])
!   CALL OK(ALL(obj%val .EQ. [-1, 22, -3, 44]), "Set: unsorted positions")
! END BLOCK
!
! ! Test 3: a single value is written to every position
! BLOCK
!   CALL Set(obj, [1_I4B, 2_I4B, 4_I4B], [7_I4B])
!   CALL OK(ALL(obj%val .EQ. [7, 7, -3, 7]), "Set: single value")
! END BLOCK
!
! ! Test 4: an empty list of positions changes nothing
! BLOCK
!   INTEGER(I4B) :: none(0)
!   CALL Set(obj, none, none)
!   CALL OK(ALL(obj%val .EQ. [7, 7, -3, 7]), "Set: no positions")
! END BLOCK
!
! ! Test 5: the size does not change
! BLOCK
!   CALL OK(Size(obj) .EQ. 4, "Set: Size is not changed")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[obj_Set2]], [[obj_Set3]], [[obj_Set4]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1(obj, indx, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! vector to change, `val` must be allocated
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to write, each one in the range 1 to `tsize`
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
    !! values to write, SIZE(VALUE) = 1 or SIZE(VALUE) = SIZE(indx)
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: Set one entry of [[IntVector_]]
!
! Writes `VALUE` to `obj%val(indx)`. `tsize` and `capacity` do not change.
!
! In a debug build the routine returns without changing `obj` when `val` is
! not allocated or `indx` exceeds `tsize`. It gives no message, because the
! routine is pure. A release build does not check the bounds.
!
! Changes:
!   - 2026-11-05: Checks run only in debug builds; added a check against
!     tsize; val must be allocated in a release build
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Set
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! obj = [10_I4B, 20_I4B, 30_I4B]
!
! ! Test 1: an interior entry
! BLOCK
!   CALL Set(obj, 2_I4B, -5_I4B)
!   CALL OK(ALL(obj%val .EQ. [10, -5, 30]), "Set: interior entry")
! END BLOCK
!
! ! Test 2: the first and the last entry
! BLOCK
!   CALL Set(obj, 1_I4B, 0_I4B)
!   CALL Set(obj, 3_I4B, 9_I4B)
!   CALL OK(ALL(obj%val .EQ. [0, -5, 9]), "Set: first and last entry")
! END BLOCK
!
! ! Test 3: the size does not change
! BLOCK
!   CALL OK(Size(obj) .EQ. 3, "Set: Size is not changed")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[obj_Set1]], [[obj_Set3]], [[obj_Set4]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set2(obj, indx, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! vector to change, `val` must be allocated
    INTEGER(I4B), INTENT(IN) :: indx
    !! position to write, in the range 1 to `tsize`
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! value to write
  END SUBROUTINE obj_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set all active entries of [[IntVector_]] to a constant value
!
! Writes `VALUE` to `obj%val(1:tsize)`. Entries beyond `tsize` do not
! change, and neither do `tsize` and `capacity`. An `obj` with `tsize = 0`
! is not changed, even if it is not allocated.
!
! Changes:
!   - 2026-11-05: Writes only val(1:tsize) instead of the whole of val
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Set
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
!
! ! Test 1: every active entry is set
! BLOCK
!   CALL Set(obj, 5_I4B)
!   CALL OK(ALL(obj%val .EQ. 5), "Set: constant value")
!   CALL OK(Size(obj) .EQ. 4, "Set: Size is not changed")
! END BLOCK
!
! ! Test 2: entries beyond tsize are not changed
! BLOCK
!   obj%tsize = 2
!   CALL Set(obj, 8_I4B)
!   CALL OK(ALL(obj%val .EQ. [8, 8, 5, 5]), "Set: spare capacity")
! END BLOCK
!
! ! Test 3: an unallocated obj stays unallocated
! BLOCK
!   TYPE(IntVector_) :: empty
!   CALL Set(empty, 5_I4B)
!   CALL OK(.NOT. ALLOCATED(empty%val), "Set: unallocated obj")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[obj_Set1]], [[obj_Set2]], [[obj_Set4]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set3(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! vector to change
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! value written to every active entry
  END SUBROUTINE obj_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set the leading active entries of [[IntVector_]] from an array
!
! Writes `VALUE(ii)` to `obj%val(ii)` for `ii` from 1 to
! `MIN(SIZE(VALUE), tsize)`. A shorter `VALUE` leaves the remaining entries
! unchanged, and a longer `VALUE` is cut at `tsize`. `tsize` and `capacity`
! do not change. An `obj` with `tsize = 0` is not changed, even if it is
! not allocated.
!
! Changes:
!   - 2026-11-05: VALUE is cut at tsize; it used to be written in full
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Set
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! obj = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
!
! ! Test 1: VALUE has the same size as obj
! BLOCK
!   CALL Set(obj, [1_I4B, 2_I4B, 3_I4B, 4_I4B])
!   CALL OK(ALL(obj%val .EQ. [1, 2, 3, 4]), "Set: same size")
! END BLOCK
!
! ! Test 2: a shorter VALUE sets only the leading entries
! BLOCK
!   CALL Set(obj, [7_I4B, 8_I4B])
!   CALL OK(ALL(obj%val .EQ. [7, 8, 3, 4]), "Set: shorter VALUE")
! END BLOCK
!
! ! Test 3: a longer VALUE is cut at the size of obj
! BLOCK
!   CALL Set(obj, [5_I4B, 6_I4B, 7_I4B, 8_I4B, 9_I4B, 10_I4B])
!   CALL OK(ALL(obj%val .EQ. [5, 6, 7, 8]), "Set: longer VALUE")
!   CALL OK(Size(obj) .EQ. 4, "Set: Size is not changed")
! END BLOCK
!
! ! Test 4: an empty VALUE changes nothing
! BLOCK
!   INTEGER(I4B) :: none(0)
!   CALL Set(obj, none)
!   CALL OK(ALL(obj%val .EQ. [5, 6, 7, 8]), "Set: empty VALUE")
! END BLOCK
!
! ! Test 5: entries beyond tsize are not changed
! BLOCK
!   obj%tsize = 2
!   CALL Set(obj, [1_I4B, 2_I4B, 3_I4B])
!   CALL OK(ALL(obj%val .EQ. [1, 2, 7, 8]), "Set: spare capacity")
! END BLOCK
!
! ! Test 6: an unallocated obj stays unallocated
! BLOCK
!   TYPE(IntVector_) :: empty
!   CALL Set(empty, [1_I4B, 2_I4B])
!   CALL OK(.NOT. ALLOCATED(empty%val), "Set: unallocated obj")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[obj_Set1]], [[obj_Set2]], [[obj_Set3]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set4(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! vector to change
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
    !! values to write, entries beyond `tsize` are ignored
  END SUBROUTINE obj_Set4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                   RemoveDuplicates@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: Sort [[IntVector_]] and remove its repeated entries
!
! Sorts `obj%val` in ascending order, keeps one copy of each value, and
! sets `tsize` to the new size of `val`. An unallocated
! `obj` is not changed. `capacity` remains the same.
!
! Changes:
!   - 2026-11-05: Updates tsize; an unallocated obj is not
!     changed
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Capacity
! USE IntVector_Method, ONLY: RemoveDuplicates
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
! ! Test 1: duplicates are removed and the result is sorted
!
! BLOCK
!   obj = [5_I4B, 4_I4B, 4_I4B, 3_I4B, 1_I4B, 1_I4B, 1_I4B, 2_I4B]
!   CALL RemoveDuplicates(obj)
!   CALL OK(SIZE(obj) .EQ. 5, "RemoveDuplicates: Size")
!   CALL OK(Capacity(obj) .EQ. 8, "RemoveDuplicates: capacity")
!   CALL OK(ALL(obj%val(1:obj%tsize) .EQ. [1, 2, 3, 4, 5]), &
!           "RemoveDuplicates: values")
! END BLOCK
!
! ! Test 2: no duplicates, the vector is only sorted
! BLOCK
!   obj = [3_I4B, 1_I4B, 2_I4B]
!   CALL RemoveDuplicates(obj)
!   CALL OK(SIZE(obj) .EQ. 3, "RemoveDuplicates: no duplicates, Size")
!   CALL OK(ALL(obj%val(1:obj%tsize) .EQ. [1, 2, 3]), &
!           "RemoveDuplicates: no duplicates, values")
! END BLOCK
!
! ! Test 3: all entries are the same
! BLOCK
!   obj = [7_I4B, 7_I4B, 7_I4B]
!   CALL RemoveDuplicates(obj)
!   CALL OK(SIZE(obj) .EQ. 1, "RemoveDuplicates: all same, Size")
!   CALL OK(obj%val(1) .EQ. 7, "RemoveDuplicates: all same, value")
! END BLOCK
!
! ! Test 4: one entry
! BLOCK
!   obj = [7_I4B]
!   CALL RemoveDuplicates(obj)
!   CALL OK(SIZE(obj) .EQ. 1, "RemoveDuplicates: one entry, Size")
!   CALL OK(obj%val(1) .EQ. 7, "RemoveDuplicates: one entry, value")
! END BLOCK
!
! ! Test 5: an unallocated obj stays unallocated
! BLOCK
!   TYPE(IntVector_) :: empty
!   CALL RemoveDuplicates(empty)
!   CALL OK(.NOT. ALLOCATED(empty%val), "RemoveDuplicates: unallocated")
!   CALL OK(SIZE(empty) .EQ. 0, "RemoveDuplicates: unallocated, Size")
! END BLOCK
! END PROGRAM main
!```
!
! Full example with spare capacity:
! src/modules/IntVector/examples/RemoveDuplicates_test_2.F90

INTERFACE RemoveDuplicates
  MODULE PURE SUBROUTINE obj_RemoveDuplicates1(obj)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! vector to sort and reduce to its unique entries
  END SUBROUTINE obj_RemoveDuplicates1
END INTERFACE RemoveDuplicates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE IntVector_SetMethod
