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
! `Set` writes entries of an [[IntVector_]]. It can write at a list of
! positions, at one position, or to all active entries as a constant.
! It can also copy an array into the leading active entries.
!
! `SetTotalDimension` sets the rank stored in the vector.
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
! date: 2021-02-23
! summary: Set the total dimension (rank) of [[IntVector_]]
!
! Writes `tDimension` to `obj%tDimension`.
! The value is not checked.
! The entries, `tsize` and `capacity` do not change.
! `obj%val` does not need to be allocated.
!
! The default value of `obj%tDimension` is 1.
!
! Examples
!
! - [test 1](|base|IntVector/examples/SetTotalDimension_test_1.F90)
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
! date: 2021-06-17
! summary: Set the entries of [[IntVector_]] at a list of positions
!
! Writes `VALUE(ii)` to `obj%val(indx(ii))` for each position in `indx`.
! If `VALUE` has one entry, the routine writes it to every position.
! Otherwise `VALUE` and `indx` must have the same size.
! `tsize` and `capacity` do not change.
!
! Each position in `indx` should appear only once.
!
! @note
! `obj%val` must be allocated and each position must lie in `1:tsize`.
! A debug build checks this and returns without changing `obj`.
! It prints no message. A release build does not check.
! @endnote
!
! Changes:
!   - 2026-11-05: Checks run only in debug builds; added a check against
!     tsize; val must be allocated in a release build
!
! Examples
!
! - [test 1](|base|IntVector/examples/Set_test_1.F90)
!
! See also: [[obj_Set2]], [[obj_Set3]], [[obj_Set4]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1(obj, indx, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! vector to change, `val` must be allocated
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! positions to write, each one in the range 1 to `tsize`
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
    !! values to write, `SIZE(VALUE)` is 1 or `SIZE(indx)`
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-17
! summary: Set one entry of [[IntVector_]]
!
! Writes `VALUE` to `obj%val(indx)`.
! `tsize` and `capacity` do not change.
!
! @note
! `obj%val` must be allocated and `indx` must lie in `1:tsize`.
! A debug build checks this and returns without changing `obj`.
! It prints no message. A release build does not check.
! @endnote
!
! Changes:
!   - 2026-11-05: Checks run only in debug builds; added a check against
!     tsize; val must be allocated in a release build
!
! Examples
!
! - [test 2](|base|IntVector/examples/Set_test_2.F90)
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
! Writes `VALUE` to `obj%val(1:tsize)`.
! Entries beyond `tsize` do not change.
! `tsize` and `capacity` do not change either.
!
! If `tsize` is 0, `obj` does not change, even when `obj%val` is not
! allocated.
!
! Changes:
!   - 2026-11-05: Writes only val(1:tsize) instead of the whole of val
!
! Examples
!
! - [test 3](|base|IntVector/examples/Set_test_3.F90)
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
! `MIN(SIZE(VALUE), tsize)`.
! A shorter `VALUE` leaves the remaining entries unchanged.
! A longer `VALUE` is cut at `tsize`.
! `tsize` and `capacity` do not change.
!
! If `tsize` is 0, `obj` does not change, even when `obj%val` is not
! allocated.
!
! Changes:
!   - 2026-11-05: VALUE is cut at tsize; it used to be written in full
!
! Examples
!
! - [test 4](|base|IntVector/examples/Set_test_4.F90)
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
! date: 2021-02-25
! summary: Sort [[IntVector_]] and remove its repeated entries
!
! Sorts the active entries `obj%val(1:tsize)` in ascending order.
! Keeps one copy of each value and sets `tsize` to the number of values
! kept. Entries beyond `tsize` are not used.
!
! If `obj%val` is not allocated, `obj` does not change.
! `capacity` does not change.
!
! Changes:
!   - 2026-11-05: Updates tsize; an unallocated obj is not
!     changed
!
! Examples
!
! - [test 1](|base|IntVector/examples/RemoveDuplicates_test_1.F90)
! - [test 2](|base|IntVector/examples/RemoveDuplicates_test_2.F90) with
!   spare capacity

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
