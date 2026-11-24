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
! summary: Addition of FEVariable_, operator(+) and Addition_
!
! `obj1 + obj2`, `obj + val` and `val + obj` return a new `FEVariable_`.
! `Addition_` writes the result into an existing `FEVariable_` without
! extra allocation.

#define _OP_ +

MODULE FEVariable_AdditionMethod
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: OPERATOR(_OP_)
PUBLIC :: Addition_

!----------------------------------------------------------------------------
!                                                   Addition_@AdditionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Addition of two FEVariables without allocation
!
! Computes `ans = obj1 + obj2` without allocating memory.
! It finds the rank case and the variable case itself.
! To give them yourself, use [[fevar_Addition_3]].
!
! @warning
! You must initiate `ans` before the call. It needs the rank and total
! shape of the result and enough capacity. A previous
! `ans = obj1 + obj2` does this.
!
! The routine sets only `s`, `len`, `val` and `varType` of `ans`. It does
! not change `rank`, `tshape` or `capacity`. It does not reallocate
! `ans%val`.
! @endwarning
!
! The rank, variable type and shape of the result follow
! [[GetRankCase]], [[GetVarCase]], [[GetRank]], [[GetVarType]] and
! [[GetShape]]. The operation is element-wise. A scalar operand acts on
! every component of a vector or matrix. Two vectors or two matrices
! add entry by entry. A space variable and a time variable give a
! space-time variable. The `defineOn` of the result is that of the
! first operand. Vector + matrix and matrix + vector are not supported.
!
! @note
! If the space (or time) sizes of the two operands differ, the routine
! uses the smaller size. It does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Addition__test_1.F90)
!
! See also: [[fevar_Addition1]], [[fevar_Addition_3]]

INTERFACE Addition_
  MODULE PURE SUBROUTINE fevar_Addition_1(obj1, obj2, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj1 + obj2
  END SUBROUTINE fevar_Addition_1
END INTERFACE Addition_

!----------------------------------------------------------------------------
!                                                   Addition_@AdditionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Addition of a FEVariable and a real without allocation
!
! Computes `ans%val(1:len) = obj%val(1:len) + val` with `len = obj%len`,
! and copies `len` and `s` of `obj` into `ans`.
!
! `ans` must already have the rank, variable type and total shape of
! `obj` and enough capacity, for example `ans = obj`. `varType`, `rank`
! and `tshape` of `ans` are not changed.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Addition__test_1.F90)
!
! See also: [[fevar_Addition2]]

INTERFACE Addition_
  MODULE PURE SUBROUTINE fevar_Addition_2(obj, val, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value, applied to every value of obj
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj + val
  END SUBROUTINE fevar_Addition_2
END INTERFACE Addition_

!----------------------------------------------------------------------------
!                                                   Addition_@AdditionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Addition of two FEVariables with given rank and variable case
!
! Same as [[fevar_Addition_1]], but the caller gives `rankCase` and
! `varCase`. This saves the cost of finding them inside the routine.
! It helps when you repeat the same operation many times.
!
! @warning
! You must initiate `ans` before the call. It needs the rank and total
! shape of the result and enough capacity. A previous
! `ans = obj1 + obj2` does this.
!
! The routine sets only `s`, `len`, `val` and `varType` of `ans`. It does
! not change `rank`, `tshape` or `capacity`. It does not reallocate
! `ans%val`.
! @endwarning
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Addition__test_1.F90)
!
! See also: [[fevar_Addition_1]], [[GetRankCase]], [[GetVarCase]]

INTERFACE Addition_
  MODULE PURE SUBROUTINE fevar_Addition_3( &
    obj1, obj2, ans, varCase, rankCase)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj1 + obj2
    INTEGER(I4B), INTENT(IN) :: varCase
    !! variable case, from [[GetVarCase]]
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rank case, from [[GetRankCase]]
  END SUBROUTINE fevar_Addition_3
END INTERFACE Addition_

!----------------------------------------------------------------------------
!                                                    Addition@AdditionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable + FEVariable
!
! Returns a new `FEVariable_` with the rank, variable type and shape
! of the result. Its values are the sum of the two operands.
!
! The rank, variable type and shape of the result follow
! [[GetRankCase]], [[GetVarCase]], [[GetRank]], [[GetVarType]] and
! [[GetShape]]. The operation is element-wise. A scalar operand acts on
! every component of a vector or matrix. Two vectors or two matrices
! add entry by entry. A space variable and a time variable give a
! space-time variable. The `defineOn` of the result is that of the
! first operand. Vector + matrix and matrix + vector are not supported.
!
! @note
! If the space (or time) sizes of the two operands differ, the routine
! uses the smaller size. It does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Addition_test_1.F90)
! - [test 2](|base|FEVariable/examples/Addition_test_2.F90)
!
! See also: [[fevar_Addition_1]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Addition1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    TYPE(FEVariable_) :: ans
    !! obj1 + obj2
  END FUNCTION fevar_Addition1
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                                    Addition@AdditionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = FEVariable + Real
!
! Returns a copy of `obj` with `val` applied to every value:
! `ans%val = obj%val + val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Addition_test_1.F90)
!
! See also: [[fevar_Addition_2]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Addition2(obj, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value
    TYPE(FEVariable_) :: ans
    !! obj + val
  END FUNCTION fevar_Addition2
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                                    Addition@AdditionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: FEVariable = Real + FEVariable
!
! Returns a copy of `obj` with `val` applied to every value:
! `ans%val = val + obj%val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Addition_test_1.F90)
!
! See also: [[fevar_Addition2]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Addition3(val, obj) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    !! real value
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariable_) :: ans
    !! val + obj
  END FUNCTION fevar_Addition3
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_AdditionMethod

#undef _OP_
