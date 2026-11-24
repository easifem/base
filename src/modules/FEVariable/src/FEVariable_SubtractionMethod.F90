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
! summary: Subtraction of `FEVariable_` objects
!
! This module provides `OPERATOR(-)` and `Subtraction_`.
! `obj1 - obj2`, `obj - val` and `val - obj` return a new `FEVariable_`.
! `Subtraction_` writes the result into an existing `FEVariable_`
! and does not allocate.

#define _OP_ -

MODULE FEVariable_SubtractionMethod
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: OPERATOR(_OP_)
PUBLIC :: Subtraction_

!----------------------------------------------------------------------------
!                                             Subtraction_@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Subtracts two `FEVariable_` objects without allocation
!
! Computes `ans = obj1 - obj2`.
! The routine finds the rank case and the variable case of the two
! operands, then calls [[fevar_Subtraction_3]].
!
! The operation works entry by entry.
! A scalar operand acts on every component of a vector or matrix.
! For two vectors, or two matrices, it subtracts entry by entry.
! A space variable and a time variable give a space-time variable.
! The rank, variable type and shape of the result follow
! [[GetRankCase]], [[GetVarCase]], [[GetRank]], [[GetVarType]] and
! [[GetShape]].
!
! The routine sets only `s`, `len`, `val` and `varType` of `ans`.
! It does not change `rank`, `tshape` or `capacity`.
!
! @warning
! `ans` must already have the rank and total shape of the result,
! and enough capacity. Initiate it first, for example with a previous
! `ans = obj1 - obj2`. The routine does not reallocate `ans%val`.
! @endwarning
!
! @note
! Vector - matrix and matrix - vector are not supported.
! @endnote
!
! @note
! If the space (or time) sizes of the two operands differ, the
! routine uses only the smaller size. It does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Subtraction__test_1.F90)
!
! See also: [[fevar_Subtraction1]], [[fevar_Subtraction_3]]

INTERFACE Subtraction_
  MODULE PURE SUBROUTINE fevar_Subtraction_1(obj1, obj2, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj1 - obj2
  END SUBROUTINE fevar_Subtraction_1
END INTERFACE Subtraction_

!----------------------------------------------------------------------------
!                                             Subtraction_@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Subtracts a real value from a `FEVariable_`, or the reverse
!
! Sets `ans%val(1:len) = obj%val(1:len) - val`, with `len = obj%len`.
! It also copies `len` and `s` of `obj` into `ans`.
!
! If `objOnRHS` is true, it sets `ans%val(1:len) = val - obj%val(1:len)`
! instead.
!
! @warning
! `ans` must already have the rank, variable type and total shape of
! `obj`, and enough capacity. For example, initiate it with `ans = obj`.
! The routine does not change `varType`, `rank` or `tshape` of `ans`.
! @endwarning
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Subtraction__test_1.F90)
!
! See also: [[fevar_Subtraction2]], [[fevar_Subtraction3]]

INTERFACE Subtraction_
  MODULE PURE SUBROUTINE fevar_Subtraction_2(obj, val, ans, objOnRHS)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value, applied to every value of `obj`
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, `obj - val`, or `val - obj` if `objOnRHS` is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: objOnRHS
    !! If `.TRUE.`, then `ans = val - obj`.
    !! Default is `.FALSE.`, which gives `ans = obj - val`.
  END SUBROUTINE fevar_Subtraction_2
END INTERFACE Subtraction_

!----------------------------------------------------------------------------
!                                             Subtraction_@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-28
! summary: Subtracts two `FEVariable_` objects with given cases
!
! Computes `ans = obj1 - obj2`, as [[fevar_Subtraction_1]] does.
! The caller gives `rankCase` and `varCase`.
! This avoids computing them again when the same operation repeats
! many times.
!
! Get `rankCase` from [[GetRankCase]] and `varCase` from [[GetVarCase]].
! If `rankCase` is not a supported rank pair, `ans` is not changed.
!
! @warning
! `ans` must already have the rank and total shape of the result,
! and enough capacity. Initiate it first, for example with a previous
! `ans = obj1 - obj2`. The routine does not reallocate `ans%val`.
! @endwarning
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Subtraction__test_1.F90)
!
! See also: [[fevar_Subtraction_1]], [[GetRankCase]], [[GetVarCase]]

INTERFACE Subtraction_
  MODULE PURE SUBROUTINE fevar_Subtraction_3( &
    obj1, obj2, ans, varCase, rankCase)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    CLASS(FEVariable_), INTENT(INOUT) :: ans
    !! result, obj1 - obj2
    INTEGER(I4B), INTENT(IN) :: varCase
    !! variable case, from `GetVarCase`
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rank case, from `GetRankCase`
  END SUBROUTINE fevar_Subtraction_3
END INTERFACE Subtraction_

!----------------------------------------------------------------------------
!                                              Subtraction@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: Subtracts one `FEVariable_` from another
!
! Returns `obj1 - obj2`, a new `FEVariable_`.
! The rank, variable type and shape of the result follow
! [[GetRankCase]], [[GetVarCase]], [[GetRank]], [[GetVarType]] and
! [[GetShape]].
!
! The operation works entry by entry.
! A scalar operand acts on every component of a vector or matrix.
! For two vectors, or two matrices, it subtracts entry by entry.
! A space variable and a time variable give a space-time variable.
! The `defineOn` of the result is that of `obj1`.
!
! @note
! Vector - matrix and matrix - vector are not supported.
! @endnote
!
! @note
! If the space (or time) sizes of the two operands differ, the
! operator uses only the smaller size. It does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Subtraction_test_1.F90)
!
! See also: [[fevar_Subtraction_1]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Subtraction1(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first operand
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second operand
    TYPE(FEVariable_) :: ans
    !! obj1 - obj2
  END FUNCTION fevar_Subtraction1
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                              Subtraction@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: Subtracts a real value from a `FEVariable_`
!
! Returns `obj - val`, a new `FEVariable_`.
! The result has the same rank, variable type and shape as `obj`.
! It subtracts `val` from every value of `obj`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Subtraction_test_1.F90)
!
! See also: [[fevar_Subtraction3]], [[fevar_Subtraction_2]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Subtraction2(obj, val) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    REAL(DFP), INTENT(IN) :: val
    !! real value
    TYPE(FEVariable_) :: ans
    !! obj - val
  END FUNCTION fevar_Subtraction2
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!                                              Subtraction@SubtractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: Subtracts a `FEVariable_` from a real value
!
! Returns `val - obj`, a new `FEVariable_`.
! The result has the same rank, variable type and shape as `obj`.
! It subtracts every value of `obj` from `val`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Subtraction_test_1.F90)
!
! See also: [[fevar_Subtraction2]], [[fevar_Subtraction_2]]

INTERFACE OPERATOR(_OP_)
  MODULE PURE FUNCTION fevar_Subtraction3(val, obj) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val
    !! real value
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariable_) :: ans
    !! val - obj
  END FUNCTION fevar_Subtraction3
END INTERFACE OPERATOR(_OP_)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_SubtractionMethod

#undef _OP_
