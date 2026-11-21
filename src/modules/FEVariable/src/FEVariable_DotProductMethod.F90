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
! summary: Dot product of FEVariable_

MODULE FEVariable_DotProductMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableScalar_, &
                    FEVariableVector_, &
                    FEVariableMatrix_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_, &
                    TypeFEVariableOpt

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                DOT_PRODUCT@DotProductMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-12
! summary: Dot product of two vector FEVariables
!
! Returns the scalar variable `obj1 . obj2`. It is the dot product of the
! two vectors at each space point and time point.
!
! 1. It multiplies `obj1` and `obj2` component by component, see
!    [[fevar_Multiplication1]].
! 2. It sums the products over the components.
!
! The variable type follows the product. For example, a space vector and
! a time vector give a space-time scalar. The result has the `defineOn`
! of `obj1`.
!
! @note
! Both variables must be vectors with the same number of components.
! The routine does not check this.
! @endnote
!
! Changes:
!   - 2026-11-14: implemented; it was a stub returning an uninitiated
!     variable
!
! Returns:
!   ans : `FEVariable_` - scalar variable with the dot product
!
! Examples
!
! - [test 1](|base|FEVariable/examples/DotProduct_test_1.F90)
! - [test 2](|base|FEVariable/examples/DotProduct_test_2.F90)
!
! See also: [[NORM2]]

INTERFACE DOT_PRODUCT
  MODULE PURE FUNCTION fevar_dot_product(obj1, obj2) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj1
    !! first vector variable
    CLASS(FEVariable_), INTENT(IN) :: obj2
    !! second vector variable
    TYPE(FEVariable_) :: ans
    !! scalar variable obj1 . obj2
  END FUNCTION fevar_dot_product
END INTERFACE DOT_PRODUCT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_DotProductMethod
