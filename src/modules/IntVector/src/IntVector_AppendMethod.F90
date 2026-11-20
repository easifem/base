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
! summary: Append and concatenate methods for [[IntVector_]]
!
! `Append` adds an integer, an integer array or another [[IntVector_]]
! after the active entries of an [[IntVector_]]. The vector grows when
! its storage is full. `H_CONCAT` returns a new [[IntVector_]] that joins
! two vectors end to end. Its inputs do not change.

MODULE IntVector_AppendMethod
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: Append
PUBLIC :: H_CONCAT

!----------------------------------------------------------------------------
!                                                         Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Append an integer to an [[IntVector_]]
!
! Adds `VALUE` after the active entries of `obj`.
! This increases `obj%tsize` by one.
! A new object, which has no storage yet, is allowed.
!
! If `obj%val` is full, it grows by 10 entries.
! A new object gets 10 entries on the first call.
! After the call, `obj%capacity` equals `SIZE(obj%val)`.
! It can be larger than `obj%tsize`.
! To read the active entries, use `obj%val(1:obj%tsize)`.
!
! Changes:
!   - 2026-11-05: Grow `val` in chunks and keep `tsize` and `capacity`
!     updated
!
! Examples
!
! - [test 1](|base|IntVector/examples/Append_test_1.F90) scalar, array and `IntVector_`
! - [test 3](|base|IntVector/examples/Append_test_3.F90) growth of the capacity
!
! See also: [[obj_Append2]], [[obj_Append3]]

INTERFACE Append
  MODULE PURE SUBROUTINE obj_Append1(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! object, it can be new
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! value to append
  END SUBROUTINE obj_Append1
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                         Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Append an integer array to an [[IntVector_]]
!
! Adds the entries of `VALUE` after the active entries of `obj`, in
! order. This increases `obj%tsize` by `SIZE(VALUE)`.
! A new object, which has no storage yet, is allowed.
!
! If `obj%val` is full, it grows by `SIZE(VALUE)` entries each time.
! A new object gets `SIZE(VALUE)` entries.
! After the call, `obj%capacity` equals `SIZE(obj%val)`.
! If `VALUE` is empty, `obj` does not change.
!
! @warning
! `VALUE` must not be a section of `obj%val`.
! The call can reallocate `obj%val`, which invalidates the section.
! @endwarning
!
! Changes:
!   - 2026-11-05: Grow `val` in chunks and keep `tsize` and `capacity`
!     updated
!
! Examples
!
! - [test 1](|base|IntVector/examples/Append_test_1.F90) array on an object with values and on a new object
! - [test 2](|base|IntVector/examples/Append_test_2.F90) empty array
! - [test 3](|base|IntVector/examples/Append_test_3.F90) growth of the capacity
!
! See also: [[obj_Append1]], [[obj_Append3]]

INTERFACE Append
  MODULE PURE SUBROUTINE obj_Append2(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! object, it can be new
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
    !! values to append, not a section of `obj%val`
  END SUBROUTINE obj_Append2
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                         Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Append an [[IntVector_]] to another [[IntVector_]]
!
! Adds the active entries `anotherObj%val(1:anotherObj%tsize)` after the
! active entries of `obj`.
! Spare capacity of `anotherObj` is not copied.
! If `anotherObj` is empty, `obj` does not change.
! Storage grows as in [[obj_Append2]].
!
! @warning
! `obj` and `anotherObj` must be different objects.
! The call can reallocate `obj%val`.
! To duplicate an object, use [[H_CONCAT]].
! @endwarning
!
! Changes:
!   - 2026-11-05: Copy only the active entries of `anotherObj`
!
! Examples
!
! - [test 1](|base|IntVector/examples/Append_test_1.F90) with spare capacity in `anotherObj`
! - [test 2](|base|IntVector/examples/Append_test_2.F90) empty `anotherObj`
!
! See also: [[obj_Append1]], [[obj_Append2]]

INTERFACE Append
  MODULE PURE SUBROUTINE obj_Append3(obj, anotherObj)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! object, it can be new
    CLASS(IntVector_), INTENT(IN) :: anotherObj
    !! object to append, it must not be obj
  END SUBROUTINE obj_Append3
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                         H_CONCAT@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-18
! summary: Horizontally concat two integer vectors
!
! Returns a new array with the entries of `vec1` followed by the entries
! of `vec2`. Either input can be empty.
!
! Returns:
!   ans : INTEGER(I4B) - array of size `SIZE(vec1) + SIZE(vec2)`
!
! Examples
!
! - [test 1](|base|IntVector/examples/H_CONCAT_test_1.F90)
!
! See also: [[obj_H_CONCAT2]], [[obj_H_CONCAT3]], [[obj_H_CONCAT4]]

INTERFACE H_CONCAT
  MODULE PURE FUNCTION obj_H_CONCAT1(vec1, vec2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: vec1(:)
    !! first array
    INTEGER(I4B), INTENT(IN) :: vec2(:)
    !! second array
    INTEGER(I4B) :: ans(SIZE(vec1) + SIZE(vec2))
    !! vec1 followed by vec2
  END FUNCTION obj_H_CONCAT1
END INTERFACE H_CONCAT

!----------------------------------------------------------------------------
!                                                         H_CONCAT@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-18
! summary: Horizontally concat two [[IntVector_]]
!
! Returns a new object with the active entries of `obj1` followed by the
! active entries of `obj2`. Spare capacity of the inputs is ignored.
! Either input can be empty.
! The result has `capacity = tsize`.
!
! Changes:
!   - 2026-11-05: Use only the active entries of the inputs
!
! Returns:
!   ans : TYPE(IntVector_) - `tsize = obj1%tsize + obj2%tsize`
!
! Examples
!
! - [test 1](|base|IntVector/examples/H_CONCAT_test_1.F90)
!
! See also: [[obj_H_CONCAT1]], [[obj_H_CONCAT3]], [[obj_H_CONCAT4]]

INTERFACE H_CONCAT
  MODULE PURE FUNCTION obj_H_CONCAT2(obj1, obj2) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj1
    !! first object
    TYPE(IntVector_), INTENT(IN) :: obj2
    !! second object
    TYPE(IntVector_) :: ans
    !! active entries of obj1 followed by those of obj2
  END FUNCTION obj_H_CONCAT2
END INTERFACE H_CONCAT

!----------------------------------------------------------------------------
!                                                         H_CONCAT@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-18
! summary: Horizontally concat an integer vector and an [[IntVector_]]
!
! Returns a new object with the entries of `vec1` followed by the active
! entries of `obj2`. Spare capacity of `obj2` is ignored.
! Either input can be empty.
! The result has `capacity = tsize`.
!
! Changes:
!   - 2026-11-05: Use only the active entries of `obj2`
!
! Returns:
!   ans : TYPE(IntVector_) - `tsize = SIZE(vec1) + obj2%tsize`
!
! Examples
!
! - [test 1](|base|IntVector/examples/H_CONCAT_test_1.F90)
!
! See also: [[obj_H_CONCAT1]], [[obj_H_CONCAT2]], [[obj_H_CONCAT4]]

INTERFACE H_CONCAT
  MODULE PURE FUNCTION obj_H_CONCAT3(vec1, obj2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: vec1(:)
    !! first array
    TYPE(IntVector_), INTENT(IN) :: obj2
    !! second object
    TYPE(IntVector_) :: ans
    !! vec1 followed by the active entries of obj2
  END FUNCTION obj_H_CONCAT3
END INTERFACE H_CONCAT

!----------------------------------------------------------------------------
!                                                         H_CONCAT@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-18
! summary: Horizontally concat an [[IntVector_]] and an integer vector
!
! Returns a new object with the active entries of `obj1` followed by the
! entries of `vec2`. Spare capacity of `obj1` is ignored.
! Either input can be empty.
! The result has `capacity = tsize`.
!
! Changes:
!   - 2026-11-05: Use only the active entries of `obj1`
!
! Returns:
!   ans : TYPE(IntVector_) - `tsize = obj1%tsize + SIZE(vec2)`
!
! Examples
!
! - [test 1](|base|IntVector/examples/H_CONCAT_test_1.F90)
!
! See also: [[obj_H_CONCAT1]], [[obj_H_CONCAT2]], [[obj_H_CONCAT3]]

INTERFACE H_CONCAT
  MODULE PURE FUNCTION obj_H_CONCAT4(obj1, vec2) RESULT(ans)
    TYPE(IntVector_), INTENT(IN) :: obj1
    !! first object
    INTEGER(I4B), INTENT(IN) :: vec2(:)
    !! second array
    TYPE(IntVector_) :: ans
    !! active entries of obj1 followed by vec2
  END FUNCTION obj_H_CONCAT4
END INTERFACE H_CONCAT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE IntVector_AppendMethod
