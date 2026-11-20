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
! summary: Returns true if `obj%val` is allocated
!
! Returns true when the array `obj%val` is allocated.
! A new vector, or one that was deallocated, gives false.
! `IsInitiated` is the same function under another name.
!
! Changes:
!   - 2026-11-03: Renamed intVec_isAllocated to obj_IsAllocated
!
! Returns:
!   ans : LOGICAL(LGT) - true if `obj%val` is allocated
!
! Examples
!
! - [test 1](|base|IntVector/examples/IsAllocated_test_1.F90)
!
! See also: [[obj_Size]], [[obj_Capacity]]

INTERFACE IsAllocated
  MODULE PURE FUNCTION obj_IsAllocated(obj) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! integer vector
    LOGICAL(LGT) :: ans
    !! true if `obj%val` is allocated
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
! summary: Returns true if every entry of `obj1` is in `obj2`
!
! Returns true when each active entry of `obj1` is also an active entry
! of `obj2`. The active entries are `val(1:tsize)`.
! Order and repeated entries do not matter.
! Entries beyond `tsize` are not used.
!
! Changes:
!   - 2026-11-03: Uses val(1:tsize) of obj1 and obj2
!
! Returns:
!   ans : LOGICAL(LGT) - true if `obj1` is a subset of `obj2`
!
! Examples
!
! - [test 1](|base|IntVector/examples/In_test_1.F90)
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
! Returns true when `a` equals one of the active entries `val(1:tsize)`
! of `obj`. Entries beyond `tsize` are not searched.
! An empty vector gives false.
!
! The operator exists for `a` of kind `INT8`, `INT16`, `INT32` and
! `INT64`. These are the procedures `obj_In2a` to `obj_In2d`.
!
! Changes:
!   - 2026-11-03: Searches val(1:tsize) instead of the whole val
!
! Returns:
!   ans : LOGICAL(LGT) - true if `a` is an active entry of `obj`
!
! Examples
!
! - [test 2](|base|IntVector/examples/In_test_2.F90) with an `INT32` value
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
! Returns true when `VALUE` equals one of the active entries
! `val(1:tsize)` of `obj`. Entries beyond `tsize` are not searched.
! An empty vector gives false.
!
! Changes:
!   - 2026-11-03: Searches val(1:tsize) instead of the whole val
!
! Returns:
!   ans : LOGICAL(LGT) - true if `VALUE` is an active entry of `obj`
!
! Examples
!
! - [test 1](|base|IntVector/examples/IsPresent_test_1.F90)
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
! Checks each entry of `VALUE` against the active entries `val(1:tsize)`
! of `obj`. The result `ans` has the same size as `VALUE`.
! `ans(j)` is true when `VALUE(j)` is an active entry of `obj`.
! An empty `VALUE` gives an empty result.
! Entries beyond `tsize` are not searched.
!
! Changes:
!   - 2026-11-03: Searches val(1:tsize) instead of the whole val
!
! Returns:
!   ans : LOGICAL(LGT)(:) - `ans(j)` is true if `VALUE(j)` is in `obj`
!
! Examples
!
! - [test 1](|base|IntVector/examples/IsPresent_test_1.F90)
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
