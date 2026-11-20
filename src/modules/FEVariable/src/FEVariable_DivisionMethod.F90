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
! `obj1 / obj2`, `obj / val` and `val / obj` return a new `FEVariable_`.
! `Division_` writes the result into an existing `FEVariable_` without
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
! summary: Divides two FEVariable_ objects without allocation
!
! Computes `ans = obj1 / obj2` and writes the result into `ans`.
! The operation works value by value.
! A scalar operand acts on every component of a vector or matrix.
! The routine divides two vectors, or two matrices, entry by entry.
! These are not vector or matrix products.
!
! The rank of the result comes from [[GetRankCase]] and [[GetRank]].
! The variable type comes from [[GetVarCase]] and [[GetVarType]].
! The shape comes from [[GetShape]].
! A space variable and a time variable give a space-time variable.
!
! @note
! Vector / matrix and matrix / vector are not supported.
! @endnote
!
! @warning
! `ans` must already have the rank and total shape of the result.
! It must also have enough capacity, for example from an earlier
! `ans = obj1 / obj2`.
! The routine sets `s`, `len`, `val` and `varType` of `ans`.
! It does not change `rank`, `tshape` or `capacity`.
! It does not reallocate `ans%val`.
! @endwarning
!
! The operands must have the same number of space points and the same
! number of time points. The routine does not check this.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Division__test_1.F90)
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
! summary: Divides a FEVariable_ and a real value without allocation
!
! Computes `ans = obj / val` value by value.
! If `objOnRHS` is true, it computes `ans = val / obj` instead.
! It also copies `len` and `s` of `obj` into `ans`.
!
! @warning
! `ans` must already have the rank, variable type and total shape of
! `obj`, and enough capacity, for example `ans = obj`.
! The routine does not change `varType`, `rank` or `tshape` of `ans`.
! @endwarning
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Division__test_1.F90)
!
! See also: [[fevar_Division2]], [[fevar_Division3]]

INTERFACE Division_
  MODULE PURE SUBROUTINE fevar_Division_2(obj, val, ans, objOnRHS)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value, applied to every value of `obj`
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, `obj / val`, or `val / obj` if `objOnRHS` is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: objOnRHS
    !! If `.TRUE.`, computes `val / obj`. Default is `.FALSE.`,
    !! which computes `obj / val`.
  END SUBROUTINE fevar_Division_2
END INTERFACE Division_

!----------------------------------------------------------------------------
!                                                   Division_@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Divides two FEVariable_ objects with given rank and variable case
!
! Computes `ans = obj1 / obj2` without allocation.
! The caller gives `rankCase` and `varCase`.
! This avoids computing them on every call, which helps when the same
! operation repeats many times.
!
! Get `rankCase` from [[GetRankCase]] and `varCase` from [[GetVarCase]].
!
! @warning
! `ans` must already have the rank and total shape of the result.
! It must also have enough capacity, for example from an earlier
! `ans = obj1 / obj2`.
! The routine sets `s`, `len`, `val` and `varType` of `ans`.
! It does not change `rank`, `tshape` or `capacity`.
! It does not reallocate `ans%val`.
! @endwarning
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Division__test_1.F90)
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
    !! variable case of `obj1` and `obj2`, from `GetVarCase`
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rank case of `obj1` and `obj2`, from `GetRankCase`
  END SUBROUTINE fevar_Division_3
END INTERFACE Division_

!----------------------------------------------------------------------------
!                                                    Division@DivisionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: Divides one FEVariable_ by another, `obj1 / obj2`
!
! Returns a new `FEVariable_` that holds the result.
! The operation works value by value.
! A scalar operand acts on every component of a vector or matrix.
! The operator divides two vectors, or two matrices, entry by entry.
! These are not vector or matrix products.
!
! The rank of the result comes from [[GetRankCase]] and [[GetRank]].
! The variable type comes from [[GetVarCase]] and [[GetVarType]].
! The shape comes from [[GetShape]].
! A space variable and a time variable give a space-time variable.
! The `defineOn` of the result is that of `obj1`.
!
! @note
! Vector / matrix and matrix / vector are not supported.
! @endnote
!
! @warning
! The operands must have the same number of space points and the same
! number of time points. The operator does not check this.
! @endwarning
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Division_test_1.F90)
! - [test 2](|base|FEVariable/examples/Division_test_2.F90) with time
!   and space-time operands
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
! summary: Divides a FEVariable_ by a real value, `obj / val`
!
! Returns a new `FEVariable_` with the same rank, variable type and
! shape as `obj`.
! Each value of the result is the matching value of `obj` divided by `val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Division_test_1.F90)
!
! See also: [[fevar_Division3]], [[fevar_Division_2]]

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
! summary: Divides a real value by a FEVariable_, `val / obj`
!
! Returns a new `FEVariable_` with the same rank, variable type and
! shape as `obj`.
! Each value of the result is `val` divided by the matching value of `obj`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Division_test_1.F90)
!
! See also: [[fevar_Division2]], [[fevar_Division_2]]

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
