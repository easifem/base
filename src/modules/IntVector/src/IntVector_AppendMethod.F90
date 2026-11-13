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
! The value is stored after the active entries and `obj%tsize` increases
! by one. A new object, which has no storage yet, is allowed.
!
! When `obj%val` is full it grows by `MAX(obj%capacity, 10)` entries, so
! the storage doubles. After the call `obj%capacity` equals
! `SIZE(obj%val)` and can be larger than `obj%tsize`. Read the active
! entries through `obj%val(1:obj%tsize)` or `Size(obj)`.
!
! Changes:
!   - 2026-11-05: Grow `val` in chunks and keep `tsize` and `capacity`
!     updated
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Append
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: append a scalar to a new object
! BLOCK
!   TYPE(IntVector_) :: obj
!   CALL Append(obj, 5_I4B)
!   CALL OK(Size(obj) .EQ. 1, "Append: scalar, tsize")
!   CALL OK(obj%val(1) .EQ. 5, "Append: scalar, value")
!   CALL OK(obj%capacity .EQ. SIZE(obj%val), "Append: capacity")
! END BLOCK
!
! ! Test 2: repeated scalar append grows past the chunk size
! BLOCK
!   TYPE(IntVector_) :: obj
!   INTEGER(I4B) :: ii
!   LOGICAL :: isok
!   DO ii = 1, 25
!     CALL Append(obj, ii)
!   END DO
!   isok = ALL(obj%val(1:25) .EQ. [(ii, ii=1, 25)])
!   CALL OK(Size(obj) .EQ. 25, "Append: repeated scalar, tsize")
!   CALL OK(isok, "Append: repeated scalar, values")
!   CALL OK(obj%capacity .GE. 25, "Append: repeated scalar, capacity")
! END BLOCK
!
! ! Test 3: append an array to an object with values
! BLOCK
!   TYPE(IntVector_) :: obj
!   obj = [1_I4B, 2_I4B]
!   CALL Append(obj, [3_I4B, 4_I4B, 5_I4B])
!   CALL OK(Size(obj) .EQ. 5, "Append: array, tsize")
!   CALL OK(ALL(obj%val(1:5) .EQ. [1, 2, 3, 4, 5]), "Append: array, val")
!   CALL OK(obj%capacity .EQ. SIZE(obj%val), "Append: array capacity")
! END BLOCK
!
! ! Test 4: array longer than the chunk size, on a new object
! BLOCK
!   TYPE(IntVector_) :: obj
!   INTEGER(I4B) :: ii
!   CALL Append(obj, [(ii, ii=1, 37)])
!   CALL OK(Size(obj) .EQ. 37, "Append: long array, tsize")
!   CALL OK(ALL(obj%val(1:37) .EQ. [(ii, ii=1, 37)]), &
!           "Append: long array, values")
! END BLOCK
!
! ! Test 5: append an IntVector_, only its active entries are copied
! BLOCK
!   TYPE(IntVector_) :: obj, other
!   obj = [1_I4B]
!   CALL Append(other, 7_I4B)
!   CALL Append(other, 8_I4B)
!   CALL OK(other%capacity .GT. Size(other), "Append: other has spare room")
!   CALL Append(obj, other)
!   CALL OK(Size(obj) .EQ. 3, "Append: IntVector_, tsize")
!   CALL OK(ALL(obj%val(1:3) .EQ. [1, 7, 8]), "Append: IntVector_, values")
!   CALL OK(Size(other) .EQ. 2, "Append: IntVector_, other unchanged")
! END BLOCK
!
! END PROGRAM main
!```
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
! The entries of `VALUE` are stored after the active entries, in order,
! and `obj%tsize` increases by `SIZE(VALUE)`. Storage grows as in
! [[obj_Append1]].
!
! `VALUE` must not be a section of `obj%val`, because `obj%val` can be
! reallocated during the call.
!
! Changes:
!   - 2026-11-05: Grow `val` in chunks and keep `tsize` and `capacity`
!     updated
!
! Examples
!
! See the program in [[obj_Append1]], Test 3 and Test 4.
!
! See also: [[obj_Append1]], [[obj_Append3]]

INTERFACE Append
  MODULE PURE SUBROUTINE obj_Append2(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    !! object, it can be new
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
    !! values to append, not a section of obj%val
  END SUBROUTINE obj_Append2
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                         Append@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Append an [[IntVector_]] to another [[IntVector_]]
!
! Only the active entries `anotherObj%val(1:anotherObj%tsize)` are
! copied; spare capacity of `anotherObj` is ignored. If `anotherObj` is
! empty, `obj` does not change.
!
! `obj` and `anotherObj` must be different objects, because `obj%val`
! can be reallocated during the call. To duplicate an object use
! [[H_CONCAT]].
!
! Changes:
!   - 2026-11-05: Copy only the active entries of `anotherObj`
!
! Examples
!
! See the program in [[obj_Append1]], Test 5.
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
! date: 18 June 2021
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
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: Append
! USE IntVector_Method, ONLY: H_CONCAT
! USE IntVector_Method, ONLY: Size
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj1, obj2, ans
! INTEGER(I4B) :: empty(0)
!
! obj1 = [1_I4B, 2_I4B]
! obj2 = [3_I4B, 4_I4B, 5_I4B]
!
! ! Test 1: two integer arrays
! BLOCK
!   INTEGER(I4B), ALLOCATABLE :: arr(:)
!   arr = H_CONCAT([1_I4B, 2_I4B], [3_I4B])
!   CALL OK(SIZE(arr) .EQ. 3, "H_CONCAT: arrays, size")
!   CALL OK(ALL(arr .EQ. [1, 2, 3]), "H_CONCAT: arrays, values")
!   arr = H_CONCAT(empty, [3_I4B])
!   CALL OK(ALL(arr .EQ. [3]), "H_CONCAT: arrays, empty first")
!   arr = H_CONCAT([3_I4B], empty)
!   CALL OK(ALL(arr .EQ. [3]), "H_CONCAT: arrays, empty second")
! END BLOCK
!
! ! Test 2: two IntVector_
! ans = H_CONCAT(obj1, obj2)
! CALL OK(Size(ans) .EQ. 5, "H_CONCAT: obj obj, size")
! CALL OK(ALL(ans%val(1:5) .EQ. [1, 2, 3, 4, 5]), "H_CONCAT: obj obj, val")
! CALL OK(ans%capacity .EQ. 5, "H_CONCAT: obj obj, capacity")
!
! ! Test 3: array then IntVector_
! ans = H_CONCAT([9_I4B], obj2)
! CALL OK(Size(ans) .EQ. 4, "H_CONCAT: vec obj, size")
! CALL OK(ALL(ans%val(1:4) .EQ. [9, 3, 4, 5]), "H_CONCAT: vec obj, val")
!
! ! Test 4: IntVector_ then array
! ans = H_CONCAT(obj1, [9_I4B])
! CALL OK(Size(ans) .EQ. 3, "H_CONCAT: obj vec, size")
! CALL OK(ALL(ans%val(1:3) .EQ. [1, 2, 9]), "H_CONCAT: obj vec, val")
!
! ! Test 5: spare capacity of the inputs is ignored
! BLOCK
!   TYPE(IntVector_) :: spare
!   CALL Append(spare, 7_I4B)
!   CALL OK(spare%capacity .GT. Size(spare), "H_CONCAT: input has spare")
!   ans = H_CONCAT(spare, obj1)
!   CALL OK(Size(ans) .EQ. 3, "H_CONCAT: spare first, size")
!   CALL OK(ALL(ans%val(1:3) .EQ. [7, 1, 2]), "H_CONCAT: spare 1st, val")
!   ans = H_CONCAT(obj1, spare)
!   CALL OK(ALL(ans%val(1:3) .EQ. [1, 2, 7]), "H_CONCAT: spare 2nd, val")
!   ans = H_CONCAT([9_I4B], spare)
!   CALL OK(ALL(ans%val(1:2) .EQ. [9, 7]), "H_CONCAT: spare, vec obj")
!   ans = H_CONCAT(spare, [9_I4B])
!   CALL OK(ALL(ans%val(1:2) .EQ. [7, 9]), "H_CONCAT: spare, obj vec")
! END BLOCK
!
! ! Test 6: empty, never allocated IntVector_
! BLOCK
!   TYPE(IntVector_) :: none1, none2
!   ans = H_CONCAT(none1, none2)
!   CALL OK(Size(ans) .EQ. 0, "H_CONCAT: both empty, size")
!   ans = H_CONCAT(none1, obj1)
!   CALL OK(ALL(ans%val(1:2) .EQ. [1, 2]), "H_CONCAT: empty first, val")
!   ans = H_CONCAT(obj1, none1)
!   CALL OK(ALL(ans%val(1:2) .EQ. [1, 2]), "H_CONCAT: empty second, val")
!   ans = H_CONCAT(empty, none1)
!   CALL OK(Size(ans) .EQ. 0, "H_CONCAT: empty vec obj, size")
!   ans = H_CONCAT(none1, empty)
!   CALL OK(Size(ans) .EQ. 0, "H_CONCAT: empty obj vec, size")
! END BLOCK
!
! END PROGRAM main
!```
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
! date: 18 June 2021
! summary: Horizontally concat two [[IntVector_]]
!
! Returns a new object with the active entries of `obj1` followed by the
! active entries of `obj2`. Spare capacity of the inputs is ignored, and
! either input can be empty. The result has `capacity = tsize`.
!
! Changes:
!   - 2026-11-05: Use only the active entries of the inputs
!
! Returns:
!   ans : TYPE(IntVector_) - `tsize = obj1%tsize + obj2%tsize`
!
! Examples
!
! See the program in [[obj_H_CONCAT1]], Test 2, Test 5 and Test 6.
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
! date: 18 June 2021
! summary: Horizontally concat an integer vector and an [[IntVector_]]
!
! Returns a new object with the entries of `vec1` followed by the active
! entries of `obj2`. Spare capacity of `obj2` is ignored, and either
! input can be empty. The result has `capacity = tsize`.
!
! Changes:
!   - 2026-11-05: Use only the active entries of `obj2`
!
! Returns:
!   ans : TYPE(IntVector_) - `tsize = SIZE(vec1) + obj2%tsize`
!
! Examples
!
! See the program in [[obj_H_CONCAT1]], Test 3, Test 5 and Test 6.
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
! date: 18 June 2021
! summary: Horizontally concat an [[IntVector_]] and an integer vector
!
! Returns a new object with the active entries of `obj1` followed by the
! entries of `vec2`. Spare capacity of `obj1` is ignored, and either
! input can be empty. The result has `capacity = tsize`.
!
! Changes:
!   - 2026-11-05: Use only the active entries of `obj1`
!
! Returns:
!   ans : TYPE(IntVector_) - `tsize = obj1%tsize + SIZE(vec2)`
!
! Examples
!
! See the program in [[obj_H_CONCAT1]], Test 4, Test 5 and Test 6.
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
