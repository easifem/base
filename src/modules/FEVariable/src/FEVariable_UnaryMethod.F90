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
! versions write into an existing FEVariable_ without allocation.

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
!   - 2026-11-14: all values are computed; before only the last value was
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: ABS
! USE FEVariable_Method, ONLY: ABS_
! USE FEVariable_Method, ONLY: Get
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE FEVariable_Method, ONLY: SHAPE
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(3), vs(2, 2)
!
! a = [-1.0_DFP, 4.0_DFP, -9.0_DFP]
! vs = RESHAPE([-1.0_DFP, 2.0_DFP, -3.0_DFP, 4.0_DFP], [2, 2])
!
! ! Test 1: ABS of every value
! BLOCK
!   TYPE(FEVariable_) :: ans
!
!   ans = ABS(QuadratureVariable(a, Scalar, Space))
!   CALL OK(ALL(Get(ans, Scalar, Space) .EQ. [1.0_DFP, 4.0_DFP, 9.0_DFP]), &
!           "ABS scalar space")
!
!   ans = ABS(QuadratureVariable(vs, Vector, Space))
!   CALL OK(ALL(Get(ans, Vector, Space) .EQ. ABS(vs)), "ABS vector space")
!   CALL OK(ALL(SHAPE(ans) .EQ. [2, 2]), "ABS keeps the shape")
! END BLOCK
!
! ! Test 2: ABS_ writes into an existing variable
! BLOCK
!   TYPE(FEVariable_) :: obj, ans
!
!   obj = QuadratureVariable(a, Scalar, Space)
!   ans = obj
!   CALL ABS_(obj=obj, ans=ans)
!   CALL OK(ALL(ans%val(1:3) .EQ. [1.0_DFP, 4.0_DFP, 9.0_DFP]), "ABS_")
! END BLOCK
!
! END PROGRAM main
!```
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
! Sets `ans%val(1:len) = ABS(obj%val(1:len))` with `len = obj%len`.
!
! `ans` must already have the rank, variable type and total shape of
! `obj` and enough capacity, for example `ans = obj`. Only `len`, `s`
! and `val` of `ans` are set.
!
! Changes:
!   - 2026-11-14: all values are computed; before only the last value was
!
! Examples
!
! See [[obj_abs]] (Test 2).

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
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE FEVariable_Method, ONLY: Get
! USE FEVariable_Method, ONLY: OPERATOR(**)
! USE FEVariable_Method, ONLY: POWER_
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(3)
!
! a = [-1.0_DFP, 2.0_DFP, 3.0_DFP]
!
! ! Test 1: obj ** n for n = 2, 3 and 0
! BLOCK
!   TYPE(FEVariable_) :: ans
!
!   ans = QuadratureVariable(a, Scalar, Space)**2
!   CALL OK(ALL(ABS(Get(ans, Scalar, Space) - a**2) .LT. tol), "obj ** 2")
!   ans = QuadratureVariable(a, Scalar, Space)**3
!   CALL OK(ALL(ABS(Get(ans, Scalar, Space) - a**3) .LT. tol), "obj ** 3")
!   ans = QuadratureVariable(a, Scalar, Space)**0
!   CALL OK(ALL(ABS(Get(ans, Scalar, Space) - 1.0_DFP) .LT. tol), "obj ** 0")
! END BLOCK
!
! ! Test 2: POWER_ writes into an existing variable
! BLOCK
!   TYPE(FEVariable_) :: obj, ans
!
!   obj = QuadratureVariable(a, Scalar, Space)
!   ans = obj
!   CALL POWER_(obj=obj, n=2, ans=ans)
!   CALL OK(ALL(ABS(ans%val(1:3) - a**2) .LT. tol), "POWER_")
! END BLOCK
!
! END PROGRAM main
!```
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
! Sets `ans%val(1:len) = obj%val(1:len) ** n` with `len = obj%len`.
!
! `ans` must already have the rank, variable type and total shape of
! `obj` and enough capacity, for example `ans = obj`. Only `len`, `s`
! and `val` of `ans` are set.
!
! Examples
!
! See [[obj_power]] (Test 2).

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
! Returns a copy of `obj` with the square root of every value. Values
! must not be negative; this is not checked.
!
! Changes:
!   - 2026-11-14: all values are computed; before only the last value was
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
! USE BaseType, ONLY: Constant => TypeFEVariableConstant
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE FEVariable_Method, ONLY: Get
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE FEVariable_Method, ONLY: SQRT
! USE FEVariable_Method, ONLY: Sqrt_
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(3), m(2, 2)
!
! a = [1.0_DFP, 4.0_DFP, 9.0_DFP]
! m = RESHAPE([1.0_DFP, 4.0_DFP, 16.0_DFP, 25.0_DFP], [2, 2])
!
! ! Test 1: SQRT of every value
! BLOCK
!   TYPE(FEVariable_) :: ans
!
!   ans = SQRT(QuadratureVariable(a, Scalar, Space))
!   CALL OK(ALL(ABS(Get(ans, Scalar, Space) - [1.0_DFP, 2.0_DFP, 3.0_DFP]) &
!               .LT. tol), "SQRT scalar space")
!
!   ans = SQRT(QuadratureVariable(m, Matrix, Constant))
!   CALL OK(ALL(ABS(Get(ans, Matrix, Constant) - SQRT(m)) .LT. tol), &
!           "SQRT matrix constant")
! END BLOCK
!
! ! Test 2: Sqrt_ writes into an existing variable
! BLOCK
!   TYPE(FEVariable_) :: obj, ans
!
!   obj = QuadratureVariable(a, Scalar, Space)
!   ans = obj
!   CALL Sqrt_(obj=obj, ans=ans)
!   CALL OK(ALL(ABS(ans%val(1:3) - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), &
!           "Sqrt_")
! END BLOCK
!
! END PROGRAM main
!```
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
! Sets `ans%val(1:len) = SQRT(obj%val(1:len))` with `len = obj%len`.
!
! `ans` must already have the rank, variable type and total shape of
! `obj` and enough capacity, for example `ans = obj`. Only `len`, `s`
! and `val` of `ans` are set.
!
! Changes:
!   - 2026-11-14: all values are computed; before only the last value was
!
! Examples
!
! See [[obj_sqrt]] (Test 2).

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
! Returns a scalar FEVariable_ with the Euclidean norm of the vector at
! each space and time point. The variable type and `defineOn` are those
! of `obj`; the shape drops the component dimension.
!
! `obj` must be a vector variable. Scalar and matrix variables are not
! supported; this is not checked.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: Constant => TypeFEVariableConstant
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
! USE BaseType, ONLY: Time => TypeFEVariableTime
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: Get
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: NORM2
! USE FEVariable_Method, ONLY: Norm2_
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: vs(2, 2), vst(2, 2, 2)
!
! vs = RESHAPE([3.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], [2, 2])
! vst = RESHAPE([3.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP, 0.0_DFP, 1.0_DFP, &
!                5.0_DFP, 12.0_DFP], [2, 2, 2])
!
! ! Test 1: norm at each point gives a scalar variable
! BLOCK
!   TYPE(FEVariable_) :: ans
!
!   ans = NORM2(QuadratureVariable([3.0_DFP, 4.0_DFP], Vector, Constant))
!   CALL OK(ans%rank .EQ. fevaropt%scalar, "constant: scalar")
!   CALL OK(ABS(Get(ans, Scalar, Constant) - 5.0_DFP) .LT. tol, "constant")
!
!   ans = NORM2(QuadratureVariable(vs, Vector, Space))
!   CALL OK(ALL(ABS(Get(ans, Scalar, Space) - [5.0_DFP, 10.0_DFP]) &
!               .LT. tol), "space")
!
!   ans = NORM2(NodalVariable(vs, Vector, Time))
!   CALL OK(ans%defineOn .EQ. fevaropt%nodal, "time: nodal")
!   CALL OK(ALL(ABS(Get(ans, Scalar, Time) - [5.0_DFP, 10.0_DFP]) &
!               .LT. tol), "time")
!
!   ans = NORM2(QuadratureVariable(vst, Vector, SpaceTime))
!   CALL OK(ALL(ABS(Get(ans, Scalar, SpaceTime) - RESHAPE([5.0_DFP, &
!               10.0_DFP, 1.0_DFP, 13.0_DFP], [2, 2])) .LT. tol), "spaceTime")
! END BLOCK
!
! ! Test 2: Norm2_ writes into an existing scalar variable
! BLOCK
!   TYPE(FEVariable_) :: ans
!
!   ans = QuadratureVariable(tsize=2, rank=Scalar, vartype=Space)
!   CALL Norm2_(obj=QuadratureVariable(vs, Vector, Space), ans=ans)
!   CALL OK(ALL(ABS(ans%val(1:2) - [5.0_DFP, 10.0_DFP]) .LT. tol), "Norm2_")
! END BLOCK
!
! END PROGRAM main
!```
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
! Same as [[obj_norm2]], but writes into `ans`. `ans` must already be a
! scalar variable with the variable type of `obj` and enough capacity.
! Only `len`, `s` and `val` of `ans` are set.
!
! Examples
!
! See [[obj_norm2]] (Test 2).

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
! True if `obj1` and `obj2` have the same `len`, `defineOn`, `rank`,
! `varType`, `tshape` and shape, and `val(1:len)` are approximately equal
! (see `.APPROXEQ.` in ApproxUtility). Capacity is not compared.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: Time => TypeFEVariableTime
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: OPERATOR(.EQ.)
! USE FEVariable_Method, ONLY: OPERATOR(.NE.)
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE FEVariable_Method, ONLY: SetLen
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: a(3)
!
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
!
! ! Test 1: same data are equal
! BLOCK
!   TYPE(FEVariable_) :: obj1, obj2
!
!   obj1 = QuadratureVariable(a, Scalar, Space)
!   obj2 = obj1
!   CALL OK(obj1 .EQ. obj2, "same: ==")
!   CALL OK(.NOT. (obj1 .NE. obj2), "same: not /=")
! END BLOCK
!
! ! Test 2: different values, defineOn, varType or len are not equal
! BLOCK
!   TYPE(FEVariable_) :: obj1, obj2
!
!   obj1 = QuadratureVariable(a, Scalar, Space)
!   obj2 = QuadratureVariable(a + 1.0_DFP, Scalar, Space)
!   CALL OK(obj1 .NE. obj2, "values differ: /=")
!   obj2 = NodalVariable(a, Scalar, Space)
!   CALL OK(obj1 .NE. obj2, "defineOn differs: /=")
!   obj2 = QuadratureVariable(a, Scalar, Time)
!   CALL OK(obj1 .NE. obj2, "varType differs: /=")
!   obj2 = QuadratureVariable(a(1:2), Scalar, Space)
!   CALL OK(.NOT. (obj1 .EQ. obj2), "len differs: not ==")
!   CALL OK(obj1 .NE. obj2, "len differs: /=")
! END BLOCK
!
! ! Test 3: same data with a larger capacity are equal
! BLOCK
!   TYPE(FEVariable_) :: obj1, obj2
!
!   obj1 = QuadratureVariable(a, Scalar, Space)
!   obj2 = QuadratureVariable([a, 7.0_DFP], Scalar, Space)
!   CALL SetLen(obj2, 3)
!   obj2%s(1) = 3
!   obj2%val(1:3) = a
!   CALL OK(obj1 .EQ. obj2, "larger capacity: ==")
!   CALL OK(.NOT. (obj1 .NE. obj2), "larger capacity: not /=")
! END BLOCK
!
! END PROGRAM main
!```
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
! Returns `.NOT. (obj1 == obj2)`, see [[obj_isEqual]].
!
! Changes:
!   - 2026-11-14: defined as the negation of ==; before it compared the
!     whole allocated val arrays and failed when their sizes differed
!
! Examples
!
! See [[obj_isEqual]].

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
