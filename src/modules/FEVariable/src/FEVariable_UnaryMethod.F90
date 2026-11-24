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
! date: 2021-12-12
! summary: Unary and comparison methods of FEVariable_
!
! Element-wise `ABS`, `SQRT` and integer power, the Euclidean norm of a
! vector variable, and the comparison operators `==` and `/=`. The `_`
! versions write into an existing `FEVariable_` without allocation.

MODULE FEVariable_UnaryMethod
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: ABS, ABS_
PUBLIC :: OPERATOR(**), POWER_
PUBLIC :: Sqrt, Sqrt_
PUBLIC :: Norm2, Norm2_
PUBLIC :: OPERATOR(.EQ.)
PUBLIC :: OPERATOR(.NE.)

!----------------------------------------------------------------------------
!                                                            ABS@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = ABS(obj)
!
! Returns a copy of `obj` with the absolute value of every value.
!
! Changes:
!   - 2026-11-14: the routine now computes all values, not only the last
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Abs_test_1.F90) with `ABS` and `ABS_`
!
! See also: [[obj_abs_]]

INTERFACE ABS
  MODULE PURE FUNCTION obj_abs(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariable_) :: ans
    !! ABS(obj)
  END FUNCTION obj_abs
END INTERFACE ABS

!----------------------------------------------------------------------------
!                                                           ABS_@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = ABS(obj) without allocation
!
! Stores the absolute value of every value of `obj` in `ans`.
! It sets `ans%val(1:len)` to `ABS(obj%val(1:len))`, where `len` is
! `obj%len`.
!
! `ans` must already have the rank, variable type and shape of `obj`.
! Its `val` must have room for `len` values. For example, use `ans = obj`
! first.
! This routine sets only `len`, `s` and `val` of `ans`.
!
! Changes:
!   - 2026-11-14: the routine now computes all values, not only the last
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Abs_test_1.F90)
!
! See also: [[obj_abs]]

INTERFACE ABS_
  MODULE PURE SUBROUTINE obj_abs_(obj, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! ABS(obj)
  END SUBROUTINE obj_abs_
END INTERFACE ABS_

!----------------------------------------------------------------------------
!                                                          Power@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = obj ** n
!
! Returns a copy of `obj` with every value raised to the integer power
! `n`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Power_test_1.F90) `**` and `POWER_`
!
! See also: [[obj_power_]]

INTERFACE OPERATOR(**)
  MODULE PURE FUNCTION obj_power(obj, n) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B), INTENT(IN) :: n
    !! integer power
    TYPE(FEVariable_) :: ans
    !! obj ** n
  END FUNCTION obj_power
END INTERFACE OPERATOR(**)

!----------------------------------------------------------------------------
!                                                         POWER_@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = obj ** n without allocation
!
! Raises every value of `obj` to the integer power `n` and stores it in
! `ans`. It sets `ans%val(1:len)` to `obj%val(1:len) ** n`, where `len`
! is `obj%len`.
!
! `ans` must already have the rank, variable type and shape of `obj`.
! Its `val` must have room for `len` values. For example, use `ans = obj`
! first.
! This routine sets only `len`, `s` and `val` of `ans`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Power_test_1.F90)
!
! See also: [[obj_power]]

INTERFACE POWER_
  MODULE PURE SUBROUTINE obj_power_(obj, n, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B), INTENT(IN) :: n
    !! integer power
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! obj ** n
  END SUBROUTINE obj_power_
END INTERFACE POWER_

!----------------------------------------------------------------------------
!                                                           SQRT@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = SQRT(obj)
!
! Returns a copy of `obj` with the square root of every value.
!
! @note
! All values must be non-negative. The routine does not check this.
! @endnote
!
! Changes:
!   - 2026-11-14: the routine now computes all values, not only the last
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Sqrt_test_1.F90) `SQRT` and `Sqrt_`
!
! See also: [[obj_sqrt_]]

INTERFACE Sqrt
  MODULE PURE FUNCTION obj_sqrt(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable, values >= 0
    TYPE(FEVariable_) :: ans
    !! SQRT(obj)
  END FUNCTION obj_sqrt
END INTERFACE Sqrt

!----------------------------------------------------------------------------
!                                                          SQRT_@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-01
! summary: ans = SQRT(obj) without allocation
!
! Stores the square root of every value of `obj` in `ans`.
! It sets `ans%val(1:len)` to `SQRT(obj%val(1:len))`, where `len` is
! `obj%len`.
!
! `ans` must already have the rank, variable type and shape of `obj`.
! Its `val` must have room for `len` values. For example, use `ans = obj`
! first.
! This routine sets only `len`, `s` and `val` of `ans`.
!
! @note
! All values must be non-negative. The routine does not check this.
! @endnote
!
! Changes:
!   - 2026-11-14: the routine now computes all values, not only the last
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Sqrt_test_1.F90)
!
! See also: [[obj_sqrt]]

INTERFACE Sqrt_
  MODULE PURE SUBROUTINE obj_sqrt_(obj, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable, values >= 0
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! SQRT(obj)
  END SUBROUTINE obj_sqrt_
END INTERFACE Sqrt_

!----------------------------------------------------------------------------
!                                                          NORM2@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = NORM2(obj)
!
! Returns the Euclidean norm of a vector variable at each point.
! The result is a scalar variable.
!
! The result has the same `defineOn` and variable type as `obj`.
! Its shape has no component dimension.
!
! @note
! `obj` must be a vector variable. The routine does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Norm2_test_1.F90)
! - [test 2](|base|FEVariable/examples/Norm2_test_2.F90) nodal, space-time
!
! See also: [[obj_norm2_]], [[DOT_PRODUCT]]

INTERFACE Norm2
  MODULE PURE FUNCTION obj_norm2(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! vector variable
    TYPE(FEVariable_) :: ans
    !! scalar variable, norm at each point
  END FUNCTION obj_norm2
END INTERFACE Norm2

!----------------------------------------------------------------------------
!                                                         NORM2_@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = NORM2(obj) without allocation
!
! Stores the Euclidean norm of a vector variable at each point in `ans`.
! It gives the same values as [[obj_norm2]].
!
! `ans` must already be a scalar variable with the variable type of
! `obj`. Its `val` must have room for the result.
! This routine sets only `len`, `s` and `val` of `ans`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Norm2_test_1.F90)
! - [test 2](|base|FEVariable/examples/Norm2_test_2.F90)
!
! See also: [[obj_norm2]]

INTERFACE Norm2_
  MODULE PURE SUBROUTINE obj_norm2_(obj, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! vector variable
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! scalar variable, norm at each point
  END SUBROUTINE obj_norm2_
END INTERFACE Norm2_

!----------------------------------------------------------------------------
!                                                          Equal@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = obj1 .eq. obj2
!
! Returns `.TRUE.` if `obj1` and `obj2` are equal.
!
! Two variables are equal when all of these hold.
!
! 1. They have the same `len`, `defineOn`, `rank`, `varType` and `tshape`.
! 2. They have the same shape `s`.
! 3. The values `val(1:len)` are approximately equal, see `.APPROXEQ.`
!    in ApproxUtility.
!
! The capacity of `val` is not compared.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Equal_test_1.F90)
! - [test 2](|base|FEVariable/examples/Equal_test_2.F90) rank or shape differ
!
! See also: [[obj_notEqual]]

INTERFACE OPERATOR(.EQ.)
  MODULE PURE FUNCTION obj_isEqual(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first variable
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second variable
    LOGICAL(LGT) :: ans
    !! true if obj1 and obj2 are equal
  END FUNCTION obj_isEqual
END INTERFACE OPERATOR(.EQ.)

!----------------------------------------------------------------------------
!                                                       NotEqual@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: ans = obj1 .ne. obj2
!
! Returns `.TRUE.` if `obj1` and `obj2` are not equal.
! It is the negation of `obj1 .EQ. obj2`.
!
! Changes:
!   - 2026-11-14: now the negation of `==`. Before, it compared the whole
!     allocated `val` arrays. It failed when their sizes differed.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Equal_test_1.F90)
! - [test 2](|base|FEVariable/examples/Equal_test_2.F90)
!
! See also: [[obj_isEqual]]

INTERFACE OPERATOR(.NE.)
  MODULE PURE FUNCTION obj_notEqual(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first variable
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second variable
    LOGICAL(LGT) :: ans
    !! true if obj1 and obj2 are not equal
  END FUNCTION obj_notEqual
END INTERFACE OPERATOR(.NE.)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_UnaryMethod
