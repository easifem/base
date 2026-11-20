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
! summary: Division of FEVariable_, operator(/) and Division_
!
! `obj1 / obj2`, `obj / val` and `val / obj` return a new FEVariable_.
! `Division_` writes the result into an existing FEVariable_ without
! extra allocation.

#define _OP_ /

MODULE FEVariable_DivisionMethod
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: OPERATOR(_OP_)
PUBLIC :: Division_

!----------------------------------------------------------------------------
!                                                   Division_@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Division of two FEVariables without allocation
!
! Computes `ans = obj1 / obj2`. It computes the rank case and variable
! case and calls [[fevar_Division_3]].
!
! `ans` must already be initiated with the rank and total shape of the
! result and with enough capacity, for example by a previous
! `ans = obj1 / obj2`. Only `s`, `len`, `val` and `varType` of `ans` are
! set; `rank`, `tshape` and `capacity` are not changed and `ans%val` is not
! reallocated. User should know what they are doing.
!
! The rank, variable type and shape of the result follow
! [[GetRankCase]], [[GetVarCase]], [[GetRank]], [[GetVarType]] and
! [[GetShape]]. The operation is element-wise: a scalar operand acts on
! every component of a vector or matrix, and two vectors or two
! matrices are combined entry by entry (`*` and `/` are not vector or
! matrix products). A space variable and a time variable give a
! space-time variable. `defineOn` of the result is that of the first operand.
! Vector / matrix and matrix / vector are not supported.
!
! When the space (or time) sizes of the two operands differ, only the
! first `MIN` of the two sizes is used; this is not checked.
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
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: Division_
! USE FEVariable_Method, ONLY: GetRankCase
! USE FEVariable_Method, ONLY: GetVarCase
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: a(3), c(3), vs(2, 3)
! INTEGER(I4B) :: ii
! TYPE(FEVariable_) :: obj1, obj2, ans
!
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! c = [100.0_DFP, 200.0_DFP, 300.0_DFP]
! vs = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
!
! ! ans is initiated with the rank and shape of the result
! ans = QuadratureVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)
!
! ! Test 1: Division_(obj1, obj2, ans)
! BLOCK
!   obj1 = QuadratureVariable(vs, Vector, Space)
!   obj2 = QuadratureVariable(vs + 1.0_DFP, Vector, Space)
!   CALL Division_(obj1=obj1, obj2=obj2, ans=ans)
!   CALL OK(ans%len .EQ. 6, "obj1 / obj2: len")
!   CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(vs / (vs + 1.0_DFP), [6])) &
!               .LT. tol), "obj1 / obj2: values")
! END BLOCK
!
! ! Test 2: Division_(obj, val, ans)
! BLOCK
!   obj1 = QuadratureVariable(vs, Vector, Space)
!   CALL Division_(obj=obj1, val=0.5_DFP, ans=ans)
!   CALL OK(ALL(ABS(ans%val(1:6) - (RESHAPE(vs, [6]) / 0.5_DFP)) &
!               .LT. tol), "obj / val: values")
!
!   CALL Division_(obj=obj1, val=0.5_DFP, ans=ans, objOnRHS=.TRUE.)
!   CALL OK(ALL(ABS(ans%val(1:6) - (0.5_DFP / RESHAPE(vs, [6]))) &
!               .LT. tol), "val / obj with objOnRHS: values")
! END BLOCK
!
! ! Test 3: Division_ with precomputed rankCase and varCase
! BLOCK
!   INTEGER(I4B) :: rankCase, varCase
!
!   obj1 = QuadratureVariable(a, Scalar, Space)
!   obj2 = QuadratureVariable(c, Scalar, Space)
!   rankCase = GetRankCase(obj1%rank, obj2%rank)
!   varCase = GetVarCase(obj1%varType, obj2%varType)
!   CALL Division_(obj1=obj1, obj2=obj2, ans=ans, varCase=varCase, &
!                  rankCase=rankCase)
!   CALL OK(ans%len .EQ. 3, "with cases: len = 3")
!   CALL OK(ans%varType .EQ. fevaropt%space, "with cases: space")
!   CALL OK(ALL(ABS(ans%val(1:3) - (a / c)) .LT. tol), &
!           "with cases: values")
! END BLOCK
!
! ! Test 4: scalar constant / vector constant into ans
! BLOCK
!   obj1 = QuadratureVariable(2.0_DFP, Scalar, Constant)
!   obj2 = QuadratureVariable(a, Vector, Constant)
!   CALL Division_(obj1=obj1, obj2=obj2, ans=ans)
!   CALL OK(ans%len .EQ. 3, "scalar / vector constant: len")
!   CALL OK(ALL(ABS(ans%val(1:3) - (2.0_DFP / a)) .LT. tol), &
!           "scalar / vector constant: values")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[fevar_Division1]], [[fevar_Division_3]]

INTERFACE Division_
  MODULE PURE SUBROUTINE fevar_Division_1(obj1, obj2, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj1 / obj2
  END SUBROUTINE fevar_Division_1
END INTERFACE Division_

!----------------------------------------------------------------------------
!                                                   Division_@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Division of a FEVariable and a real without allocation
!
! Computes `ans%val(1:len) = obj%val(1:len) / val` with `len = obj%len`,
! and copies `len` and `s` of `obj` into `ans`.
! If `objOnRHS` is true, computes `val / obj%val(1:len)` instead.
!
! `ans` must already have the rank, variable type and total shape of
! `obj` and enough capacity, for example `ans = obj`. `varType`, `rank`
! and `tshape` of `ans` are not changed.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj1, obj2, ans
! ! ans initiated with the rank and shape of the result
! CALL Division_(obj=obj1, val=0.5_DFP, ans=ans, objOnRHS=.TRUE.)
!```
!
! Full example: src/modules/FEVariable/examples/Division__test_1.F90
!
! See also: [[fevar_Division2]]

INTERFACE Division_
  MODULE PURE SUBROUTINE fevar_Division_2(obj, val, ans, objOnRHS)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value, applied to every value of obj
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj / val (or val / obj, see objOnRHS)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: objOnRHS
    !! By default obj is on LHS, i.e., ans = obj / val
    !! If objOnRHS = .TRUE. , then ans = val / obj
  END SUBROUTINE fevar_Division_2
END INTERFACE Division_

!----------------------------------------------------------------------------
!                                                   Division_@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Division of two FEVariables with given rank and variable case
!
! Same as [[fevar_Division_1]], but `rankCase` and `varCase` are
! given by the caller. This bypasses the overhead of computing them inside the
! method, which helps when the same operation is repeated many times.
!
! `ans` must already be initiated with the rank and total shape of the
! result and with enough capacity, for example by a previous
! `ans = obj1 / obj2`. Only `s`, `len`, `val` and `varType` of `ans` are
! set; `rank`, `tshape` and `capacity` are not changed and `ans%val` is not
! reallocated. User should know what they are doing.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj1, obj2, ans
! ! ans initiated with the rank and shape of the result
! CALL Division_(obj1=obj1, obj2=obj2, ans=ans, &
!               varCase=varCase, rankCase=rankCase)
!```
!
! Full example: src/modules/FEVariable/examples/Division__test_1.F90
!
! See also: [[fevar_Division_1]], [[GetRankCase]], [[GetVarCase]]

INTERFACE Division_
  MODULE PURE SUBROUTINE fevar_Division_3( &
    obj1, obj2, ans, varCase, rankCase)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj1 / obj2
    INTEGER(I4B), INTENT(IN) :: varCase
    !! varCase can be obtained from GetVarCase function
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rankCase can be obtained from GetRankCase function
  END SUBROUTINE fevar_Division_3
END INTERFACE Division_

!----------------------------------------------------------------------------
!                                                    Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable / FEVariable
!
! Returns a new FEVariable_ initiated with the rank, variable type and
! shape of the result, then filled by [[fevar_Division_3]].
!
! The rank, variable type and shape of the result follow
! [[GetRankCase]], [[GetVarCase]], [[GetRank]], [[GetVarType]] and
! [[GetShape]]. The operation is element-wise: a scalar operand acts on
! every component of a vector or matrix, and two vectors or two
! matrices are combined entry by entry (`*` and `/` are not vector or
! matrix products). A space variable and a time variable give a
! space-time variable. `defineOn` of the result is that of the first operand.
! Vector / matrix and matrix / vector are not supported.
!
! When the space (or time) sizes of the two operands differ, only the
! first `MIN` of the two sizes is used; this is not checked.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj1, obj2, ans
! ans = obj1 / obj2
!```
!
! Full example: src/modules/FEVariable/examples/Division_test_1.F90
!
! See also: [[fevar_Division_1]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Division1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    TYPE(FEVariable_) :: ans
    !! obj1 / obj2
  END FUNCTION fevar_Division1
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                                    Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable / Real
!
! Returns a copy of `obj` with `val` applied to every value:
! `ans%val = obj%val / val`.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj1, obj2, ans
! ans = obj1 / 1.0_DFP
!```
!
! Full example: src/modules/FEVariable/examples/Division_test_1.F90
!
! See also: [[fevar_Division_2]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Division2(obj, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value
    TYPE(FEVariable_) :: ans
    !! obj / val
  END FUNCTION fevar_Division2
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                                    Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = Real / FEVariable
!
! Returns a copy of `obj` with `val` applied to every value:
! `ans%val = val / obj%val`.
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj1, obj2, ans
! ans = 1.0_DFP / obj1
!```
!
! Full example: src/modules/FEVariable/examples/Division_test_1.F90
!
! See also: [[fevar_Division2]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Division3(val, obj) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    !! real value
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariable_) :: ans
    !! val / obj
  END FUNCTION fevar_Division3
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_DivisionMethod

#undef _OP_
