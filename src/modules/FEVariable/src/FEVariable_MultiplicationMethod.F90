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
! summary: Multiplication of FEVariable_, operator(*) and Multiplication_
!
! `obj1 * obj2`, `obj * val` and `val * obj` return a new `FEVariable_`.
! `Multiplication_` writes the result into an existing `FEVariable_`.
! It does not allocate.

#define _OP_ *

MODULE FEVariable_MultiplicationMethod
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: OPERATOR(_OP_)
PUBLIC :: Multiplication_

!----------------------------------------------------------------------------
!                                       Multiplication_@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Multiplication of two FEVariables without allocation
!
! Computes `ans = obj1 * obj2` without allocating memory.
!
! The operation is element-wise. A scalar operand acts on every
! component of a vector or matrix. Two vectors, or two matrices,
! multiply entry by entry. This is not a vector or matrix product.
!
! A space variable times a time variable gives a space-time variable.
! The rank, variable type and shape of the result follow [[GetRankCase]],
! [[GetVarCase]], [[GetRank]], [[GetVarType]] and [[GetShape]].
!
! The routine computes the rank case and the variable case itself.
! To pass them in, use [[fevar_Multiplication_3]].
!
! @note
! `ans` must already have the rank and total shape of the result. It
! must also have enough capacity. For example, use the result of a
! previous `obj1 * obj2`.
! @endnote
!
! The routine sets only `s`, `len`, `val` and `varType` of `ans`. It
! does not change `rank`, `tshape` or `capacity`. It does not reallocate
! `ans%val`.
!
! @warning
! If the space (or time) sizes of the operands differ, the routine uses
! only the smaller size. It does not check this.
! @endwarning
!
! @note
! A vector times a matrix, or a matrix times a vector, is not
! supported. In that case `ans` is not changed.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Multiplication__test_1.F90)
!
! See also: [[fevar_Multiplication1]], [[fevar_Multiplication_3]]

INTERFACE Multiplication_
  MODULE PURE SUBROUTINE fevar_Multiplication_1(obj1, obj2, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj1 * obj2
  END SUBROUTINE fevar_Multiplication_1
END INTERFACE Multiplication_

!----------------------------------------------------------------------------
!                                       Multiplication_@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Multiplication of a FEVariable and a real without allocation
!
! Multiplies every value of `obj` by `val` and stores the result in
! `ans`, without allocating memory.
!
! The routine copies `len` and `s` of `obj` into `ans`. Then it sets
! `ans%val(1:len) = obj%val(1:len) * val`.
!
! @note
! `ans` must already have the rank, variable type and total shape of
! `obj`. It must also have enough capacity. For example, use `ans = obj`.
! @endnote
!
! The routine does not change `varType`, `rank` or `tshape` of `ans`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Multiplication__test_1.F90)
!
! See also: [[fevar_Multiplication2]]

INTERFACE Multiplication_
  MODULE PURE SUBROUTINE fevar_Multiplication_2(obj, val, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value, applied to every value of obj
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj * val
  END SUBROUTINE fevar_Multiplication_2
END INTERFACE Multiplication_

!----------------------------------------------------------------------------
!                                       Multiplication_@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Multiplication of two FEVariables with given rank and variable case
!
! Computes `ans = obj1 * obj2` without allocating memory. The caller
! gives `rankCase` and `varCase`.
!
! This does the same as [[fevar_Multiplication_1]]. It saves the cost of
! computing the two cases, which helps when the same operation repeats
! many times.
!
! @note
! `ans` must already have the rank and total shape of the result. It
! must also have enough capacity. For example, use the result of a
! previous `obj1 * obj2`.
! @endnote
!
! The routine sets only `s`, `len`, `val` and `varType` of `ans`. It
! does not change `rank`, `tshape` or `capacity`. It does not reallocate
! `ans%val`.
!
! @note
! If `rankCase` is not a supported combination, `ans` is not changed.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Multiplication__test_1.F90)
!
! See also: [[fevar_Multiplication_1]], [[GetRankCase]], [[GetVarCase]]

INTERFACE Multiplication_
  MODULE PURE SUBROUTINE fevar_Multiplication_3( &
    obj1, obj2, ans, varCase, rankCase)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj1 * obj2
    INTEGER(I4B), INTENT(IN) :: varCase
    !! variable case, from [[GetVarCase]]
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rank case, from [[GetRankCase]]
  END SUBROUTINE fevar_Multiplication_3
END INTERFACE Multiplication_

!----------------------------------------------------------------------------
!                                        Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable * FEVariable
!
! Returns the product `obj1 * obj2` as a new `FEVariable_`.
!
! The operation is element-wise. A scalar operand acts on every
! component of a vector or matrix. Two vectors, or two matrices,
! multiply entry by entry. This is not a vector or matrix product.
!
! A space variable times a time variable gives a space-time variable.
! The rank, variable type and shape of the result follow [[GetRankCase]],
! [[GetVarCase]], [[GetRank]], [[GetVarType]] and [[GetShape]]. The
! `defineOn` of the result is that of `obj1`.
!
! @warning
! If the space (or time) sizes of the operands differ, the routine uses
! only the smaller size. It does not check this.
! @endwarning
!
! @note
! A vector times a matrix, or a matrix times a vector, is not
! supported. The values of `ans` are then not set.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Multiplication_test_1.F90)
! - [test 2](|base|FEVariable/examples/Multiplication_test_2.F90)
!
! See also: [[fevar_Multiplication_1]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Multiplication1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    TYPE(FEVariable_) :: ans
    !! obj1 * obj2
  END FUNCTION fevar_Multiplication1
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                        Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable * Real
!
! Returns a copy of `obj` with every value multiplied by `val`.
! The result has the same rank, variable type and shape as `obj`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Multiplication_test_1.F90)
!
! See also: [[fevar_Multiplication_2]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Multiplication2(obj, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value
    TYPE(FEVariable_) :: ans
    !! obj * val
  END FUNCTION fevar_Multiplication2
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                        Multiplication@MultiplicationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = Real * FEVariable
!
! Returns a copy of `obj` with every value multiplied by `val`.
! The result has the same rank, variable type and shape as `obj`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Multiplication_test_1.F90)
!
! See also: [[fevar_Multiplication2]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Multiplication3(val, obj) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    !! real value
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariable_) :: ans
    !! val * obj
  END FUNCTION fevar_Multiplication3
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_MultiplicationMethod

#undef _OP_
