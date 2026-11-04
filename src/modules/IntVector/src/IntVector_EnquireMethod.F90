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
! date: 2021-11-11
! summary: Enquire methods for [[IntVector_]]
!
! Methods that ask whether an [[IntVector_]] is allocated and whether
! values are present in it. Searches use only the active entries
! `obj%val(1:obj%tsize)`; spare capacity beyond `tsize` is ignored.
!
! Changes:
!   - 2026-11-03: Searches use val(1:tsize) instead of the whole val

MODULE IntVector_EnquireMethod
USE Basetype, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: LGT
PRIVATE

PUBLIC :: OPERATOR(.IN.)
PUBLIC :: IsPresent
PUBLIC :: IsAllocated
PUBLIC :: IsInitiated

!----------------------------------------------------------------------------
!                                                        IsAllocated@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns true if obj%val is allocated
!
! Returns `ALLOCATED(obj%val)`. A vector that was never initiated, or
! one that was deallocated, gives false. `IsInitiated` is the same
! function under another name.
!
! Changes:
!   - 2026-11-03: Renamed intVec_isAllocated to obj_IsAllocated
!
! Returns:
!   ans : LOGICAL(LGT) - true if obj%val is allocated
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: DEALLOCATE
! USE IntVector_Method, ONLY: IsAllocated
! USE IntVector_Method, ONLY: IsInitiated
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! ! Test 1: a new IntVector_ is not allocated
! CALL OK(.NOT. IsAllocated(obj), "IsAllocated: new obj")
! CALL OK(.NOT. IsInitiated(obj), "IsInitiated: new obj")
!
! ! Test 2: allocated after assignment
! obj = [1_I4B, 2_I4B, 3_I4B]
! CALL OK(IsAllocated(obj), "IsAllocated: after obj = [1, 2, 3]")
! CALL OK(IsInitiated(obj), "IsInitiated: after obj = [1, 2, 3]")
!
! ! Test 3: not allocated after Deallocate
! CALL DEALLOCATE (obj)
! CALL OK(.NOT. IsAllocated(obj), "IsAllocated: after Deallocate")
! CALL OK(.NOT. IsInitiated(obj), "IsInitiated: after Deallocate")
!
! END PROGRAM main
!```
!
! See also: [[obj_Size]], [[obj_Capacity]]

INTERFACE IsAllocated
  MODULE PURE FUNCTION obj_IsAllocated(obj) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    LOGICAL(LGT) :: ans
    !! true if obj%val is allocated
  END FUNCTION obj_IsAllocated
END INTERFACE IsAllocated

INTERFACE IsInitiated
  MODULE PROCEDURE obj_IsAllocated
END INTERFACE IsInitiated

!----------------------------------------------------------------------------
!                                                     Operator(.in.)@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns true if every entry of obj1 is in obj2
!
! Returns true when each active entry `obj1%val(1:obj1%tsize)` is found
! among the active entries `obj2%val(1:obj2%tsize)`. Order and repeated
! entries do not matter. Entries beyond `tsize` are not used.
!
! Changes:
!   - 2026-11-03: Uses val(1:tsize) of obj1 and obj2
!
! Returns:
!   ans : LOGICAL(LGT) - true if obj1 is a subset of obj2
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: OPERATOR(.IN.)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj1, obj2
!
! obj2 = [9_I4B, 10_I4B, 11_I4B, 12_I4B]
!
! ! Test 1: every entry of obj1 is in obj2
! obj1 = [12_I4B, 9_I4B]
! CALL OK(obj1 .IN. obj2, ".IN.: [12, 9] in [9, 10, 11, 12]")
!
! ! Test 2: obj2 has entries that are not in obj1
! CALL OK(.NOT. (obj2 .IN. obj1), ".IN.: [9, 10, 11, 12] not in [12, 9]")
!
! ! Test 3: one entry is missing
! obj1 = [9_I4B, 13_I4B]
! CALL OK(.NOT. (obj1 .IN. obj2), ".IN.: [9, 13] not in obj2")
!
! ! Test 4: a set is a subset of itself
! CALL OK(obj2 .IN. obj2, ".IN.: obj2 in obj2")
!
! ! Test 5: only the active entries val(1:tsize) of obj2 are searched
! obj1 = [12_I4B]
! obj2%tsize = 3
! CALL OK(.NOT. (obj1 .IN. obj2), ".IN.: 12 beyond tsize of obj2")
!
! ! Test 6: only the active entries val(1:tsize) of obj1 are checked
! obj1 = [9_I4B, 13_I4B]
! obj1%tsize = 1
! CALL OK(obj1 .IN. obj2, ".IN.: 13 beyond tsize of obj1")
!
! END PROGRAM main
!```
!
! See also: [[obj_In2c]], [[obj_IsPresent2]]

INTERFACE OPERATOR(.IN.)
  MODULE PURE FUNCTION obj_In1(obj1, obj2) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj1
    !! values to look for, active entries val(1:tsize)
    TYPE(IntVector_), INTENT(IN) :: obj2
    !! vector searched, active entries val(1:tsize)
    LOGICAL(LGT) :: ans
    !! true if every active entry of obj1 is in obj2
  END FUNCTION obj_In1
END INTERFACE OPERATOR(.IN.)

!----------------------------------------------------------------------------
!                                                     Operator(.in.)@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns true if an integer is in [[IntVector_]]
!
! Returns `ANY(a .EQ. obj%val(1:obj%tsize))`. Entries beyond `tsize`
! are not searched; an empty vector gives false. Overloaded for
! INT8, INT16, INT32 and INT64 values of `a`.
!
! Changes:
!   - 2026-11-03: Searches val(1:tsize) instead of the whole val
!
! Returns:
!   ans : LOGICAL(LGT) - true if a is an active entry of obj
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: OPERATOR(.IN.)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! ! Test 1: empty IntVector_ contains nothing
! CALL OK(.NOT. (1_I4B .IN. obj), ".IN.: 1 not in empty obj")
!
! obj = [1_I4B, 2_I4B, 3_I4B]
!
! ! Test 2: present entries
! CALL OK(1_I4B .IN. obj, ".IN.: 1 in [1, 2, 3]")
! CALL OK(3_I4B .IN. obj, ".IN.: 3 in [1, 2, 3]")
!
! ! Test 3: absent entries
! CALL OK(.NOT. (4_I4B .IN. obj), ".IN.: 4 not in [1, 2, 3]")
! CALL OK(.NOT. (-1_I4B .IN. obj), ".IN.: -1 not in [1, 2, 3]")
!
! ! Test 4: entries beyond tsize are not searched
! obj%tsize = 2
! CALL OK(.NOT. (3_I4B .IN. obj), ".IN.: 3 beyond tsize")
! CALL OK(2_I4B .IN. obj, ".IN.: 2 within tsize")
!
! END PROGRAM main
!```
!
! See also: [[obj_IsPresent1]], [[obj_In1]]

INTERFACE OPERATOR(.IN.)
  MODULE PURE FUNCTION obj_In2a(a, obj) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: a
    !! value to look for
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, active entries val(1:tsize)
    LOGICAL(LGT) :: ans
    !! true if a is in obj
  END FUNCTION obj_In2a

  MODULE PURE FUNCTION obj_In2b(a, obj) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: a
    !! value to look for
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, active entries val(1:tsize)
    LOGICAL(LGT) :: ans
    !! true if a is in obj
  END FUNCTION obj_In2b

  MODULE PURE FUNCTION obj_In2c(a, obj) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: a
    !! value to look for
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, active entries val(1:tsize)
    LOGICAL(LGT) :: ans
    !! true if a is in obj
  END FUNCTION obj_In2c

  MODULE PURE FUNCTION obj_In2d(a, obj) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: a
    !! value to look for
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, active entries val(1:tsize)
    LOGICAL(LGT) :: ans
    !! true if a is in obj
  END FUNCTION obj_In2d
END INTERFACE OPERATOR(.IN.)

!----------------------------------------------------------------------------
!                                                          IsPresent@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns true if a value is present in [[IntVector_]]
!
! Searches the active entries `obj%val(1:obj%tsize)` for `VALUE` and
! stops at the first match. Entries beyond `tsize` are not searched;
! an empty vector gives false.
!
! Changes:
!   - 2026-11-03: Searches val(1:tsize) instead of the whole val
!
! Returns:
!   ans : LOGICAL(LGT) - true if VALUE is an active entry of obj
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: IsPresent
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! ! Test 1: empty IntVector_ contains nothing
! CALL OK(.NOT. IsPresent(obj, 1_I4B), "IsPresent: empty obj, scalar")
! CALL OK(.NOT. ANY(IsPresent(obj, [1_I4B, 2_I4B])), &
!         "IsPresent: empty obj, vector")
!
! obj = [5_I4B, 7_I4B, 5_I4B, 9_I4B]
!
! ! Test 2: scalar value
! CALL OK(IsPresent(obj, 7_I4B), "IsPresent: 7 in [5, 7, 5, 9]")
! CALL OK(IsPresent(obj, 5_I4B), "IsPresent: repeated 5")
! CALL OK(.NOT. IsPresent(obj, 6_I4B), "IsPresent: 6 not present")
!
! ! Test 3: vector of values, one result per value
! BLOCK
!   LOGICAL(LGT), ALLOCATABLE :: ans(:)
!   ans = IsPresent(obj, [9_I4B, 1_I4B, 5_I4B, 5_I4B])
!   CALL OK(SIZE(ans) .EQ. 4, "IsPresent: SIZE(ans) = SIZE(value)")
!   CALL OK(ALL(ans .EQV. [.TRUE., .FALSE., .TRUE., .TRUE.]), &
!           "IsPresent: [9, 1, 5, 5]")
! END BLOCK
!
! ! Test 4: empty vector of values gives an empty result
! BLOCK
!   INTEGER(I4B) :: novalue(0)
!   CALL OK(SIZE(IsPresent(obj, novalue)) .EQ. 0, &
!           "IsPresent: empty value")
! END BLOCK
!
! ! Test 5: entries beyond tsize are not searched
! obj%tsize = 3
! CALL OK(.NOT. IsPresent(obj, 9_I4B), "IsPresent: 9 beyond tsize")
! CALL OK(ALL(IsPresent(obj, [5_I4B, 9_I4B]) .EQV. [.TRUE., .FALSE.]), &
!         "IsPresent: [5, 9] with tsize = 3")
!
! END PROGRAM main
!```
!
! See also: [[obj_IsPresent2]], [[obj_In2c]]

INTERFACE IsPresent
  MODULE PURE FUNCTION obj_IsPresent1(obj, VALUE) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, active entries val(1:tsize)
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! value to look for
    LOGICAL(LGT) :: Ans
    !! true if VALUE is in obj
  END FUNCTION obj_IsPresent1
END INTERFACE IsPresent

!----------------------------------------------------------------------------
!                                                          IsPresent@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns for each value whether it is present in [[IntVector_]]
!
! For each `VALUE(j)` sets `ans(j)` to true if it equals an active
! entry of `obj%val(1:obj%tsize)`. `SIZE(ans) = SIZE(VALUE)`; an empty
! `VALUE` gives an empty result. Entries beyond `tsize` are not
! searched.
!
! Changes:
!   - 2026-11-03: Searches val(1:tsize) instead of the whole val
!
! Returns:
!   ans : LOGICAL(LGT)(:) - ans(j) true if VALUE(j) is in obj
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: IntVector_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE IntVector_Method, ONLY: ASSIGNMENT(=)
! USE IntVector_Method, ONLY: IsPresent
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(IntVector_) :: obj
!
! ! Test 1: empty IntVector_ contains nothing
! CALL OK(.NOT. IsPresent(obj, 1_I4B), "IsPresent: empty obj, scalar")
! CALL OK(.NOT. ANY(IsPresent(obj, [1_I4B, 2_I4B])), &
!         "IsPresent: empty obj, vector")
!
! obj = [5_I4B, 7_I4B, 5_I4B, 9_I4B]
!
! ! Test 2: scalar value
! CALL OK(IsPresent(obj, 7_I4B), "IsPresent: 7 in [5, 7, 5, 9]")
! CALL OK(IsPresent(obj, 5_I4B), "IsPresent: repeated 5")
! CALL OK(.NOT. IsPresent(obj, 6_I4B), "IsPresent: 6 not present")
!
! ! Test 3: vector of values, one result per value
! BLOCK
!   LOGICAL(LGT), ALLOCATABLE :: ans(:)
!   ans = IsPresent(obj, [9_I4B, 1_I4B, 5_I4B, 5_I4B])
!   CALL OK(SIZE(ans) .EQ. 4, "IsPresent: SIZE(ans) = SIZE(value)")
!   CALL OK(ALL(ans .EQV. [.TRUE., .FALSE., .TRUE., .TRUE.]), &
!           "IsPresent: [9, 1, 5, 5]")
! END BLOCK
!
! ! Test 4: empty vector of values gives an empty result
! BLOCK
!   INTEGER(I4B) :: novalue(0)
!   CALL OK(SIZE(IsPresent(obj, novalue)) .EQ. 0, &
!           "IsPresent: empty value")
! END BLOCK
!
! ! Test 5: entries beyond tsize are not searched
! obj%tsize = 3
! CALL OK(.NOT. IsPresent(obj, 9_I4B), "IsPresent: 9 beyond tsize")
! CALL OK(ALL(IsPresent(obj, [5_I4B, 9_I4B]) .EQV. [.TRUE., .FALSE.]), &
!         "IsPresent: [5, 9] with tsize = 3")
!
! END PROGRAM main
!```
!
! See also: [[obj_IsPresent1]], [[obj_In1]]

INTERFACE IsPresent
  MODULE PURE FUNCTION obj_IsPresent2(obj, VALUE) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector, active entries val(1:tsize)
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
    !! values to look for
    LOGICAL(LGT), ALLOCATABLE :: Ans(:)
    !! Ans(j) true if VALUE(j) is in obj; SIZE(Ans) = SIZE(VALUE)
  END FUNCTION obj_IsPresent2
END INTERFACE IsPresent

END MODULE IntVector_EnquireMethod
