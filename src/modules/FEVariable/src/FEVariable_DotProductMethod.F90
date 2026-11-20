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
! Returns the scalar FEVariable `obj1 . obj2`, the dot product at each
! space and time point. It computes the element-wise product
! `obj1 * obj2` (see [[fevar_Multiplication1]]) and sums over the
! component index. The variable type and the space and time sizes follow
! the product: for example, a space and a time vector give a space-time
! scalar. `defineOn` is that of `obj1`.
!
! Both operands must be vector variables with the same number of
! components; this is not checked.
!
! Changes:
!   - 2026-11-14: implemented; it was a stub returning an uninitiated
!     variable
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
! USE FEVariable_Method, ONLY: DOT_PRODUCT
! USE FEVariable_Method, ONLY: Get
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE FEVariable_Method, ONLY: SHAPE
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: u(2), w(2), us(2, 3), wt(2, 2)
! INTEGER(I4B) :: ii
!
! u = [1.0_DFP, 2.0_DFP]
! w = [3.0_DFP, 4.0_DFP]
! us = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
! wt = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 2])
!
! ! Test 1: constant . constant gives a scalar constant
! BLOCK
!   TYPE(FEVariable_) :: ans
!
!   ans = DOT_PRODUCT(QuadratureVariable(u, Vector, Constant), &
!                     QuadratureVariable(w, Vector, Constant))
!   CALL OK(ans%isInit, "constant: initiated")
!   CALL OK(ans%rank .EQ. fevaropt%scalar, "constant: scalar")
!   CALL OK(ans%varType .EQ. fevaropt%constant, "constant: constant")
!   CALL OK(ABS(Get(ans, Scalar, Constant) - 11.0_DFP) .LT. tol, &
!           "constant: [1, 2] . [3, 4] = 11")
! END BLOCK
!
! ! Test 2: constant . space gives a scalar space
! BLOCK
!   TYPE(FEVariable_) :: ans
!
!   ans = DOT_PRODUCT(QuadratureVariable(w, Vector, Constant), &
!                     QuadratureVariable(us, Vector, Space))
!   CALL OK(ans%varType .EQ. fevaropt%space, "constant . space: space")
!   CALL OK(ALL(SHAPE(ans) .EQ. [3]), "constant . space: shape [3]")
!   CALL OK(ALL(ABS(Get(ans, Scalar, Space) - MATMUL(w, us)) .LT. tol), &
!           "constant . space: values")
! END BLOCK
!
! ! Test 3: space . space gives a scalar space
! BLOCK
!   TYPE(FEVariable_) :: ans
!
!   ans = DOT_PRODUCT(QuadratureVariable(us, Vector, Space), &
!                     QuadratureVariable(us, Vector, Space))
!   CALL OK(ALL(ABS(Get(ans, Scalar, Space) - SUM(us * us, dim=1)) &
!               .LT. tol), "space . space: values")
! END BLOCK
!
! ! Test 4: space . time gives a scalar spaceTime
! BLOCK
!   TYPE(FEVariable_) :: ans
!   REAL(DFP) :: ref(3, 2)
!   INTEGER(I4B) :: jj, kk
!
!   ans = DOT_PRODUCT(QuadratureVariable(us, Vector, Space), &
!                     QuadratureVariable(wt, Vector, Time))
!   DO kk = 1, 2
!     DO jj = 1, 3
!       ref(jj, kk) = DOT_PRODUCT(us(:, jj), wt(:, kk))
!     END DO
!   END DO
!   CALL OK(ans%varType .EQ. fevaropt%spacetime, "space . time: spaceTime")
!   CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "space . time: shape [3, 2]")
!   CALL OK(ALL(ABS(Get(ans, Scalar, SpaceTime) - ref) .LT. tol), &
!           "space . time: values")
! END BLOCK
!
! END PROGRAM main
!```
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
