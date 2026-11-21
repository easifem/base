!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Square root of a FEVariable by SQRT and Sqrt_

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SQRT
USE FEVariable_Method, ONLY: Sqrt_
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3), m(2, 2)

a = [1.0_DFP, 4.0_DFP, 9.0_DFP]
m = RESHAPE([1.0_DFP, 4.0_DFP, 16.0_DFP, 25.0_DFP], [2, 2])

! Test 1: SQRT of every value
BLOCK
  TYPE(FEVariable_) :: ans

  ans = SQRT(QuadratureVariable(a, Scalar, Space))
  CALL OK(ALL(ABS(Get(ans, Scalar, Space) - [1.0_DFP, 2.0_DFP, 3.0_DFP]) &
              .LT. tol), "SQRT scalar space")

  ans = SQRT(QuadratureVariable(m, Matrix, Constant))
  CALL OK(ALL(ABS(Get(ans, Matrix, Constant) - SQRT(m)) .LT. tol), &
          "SQRT matrix constant")
END BLOCK

! Test 2: Sqrt_ writes into an existing variable
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = QuadratureVariable(a, Scalar, Space)
  ans = obj
  CALL Sqrt_(obj=obj, ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), &
          "Sqrt_")
END BLOCK

END PROGRAM main
