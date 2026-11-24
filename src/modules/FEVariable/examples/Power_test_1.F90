!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Integer power of a FEVariable by ** and POWER_

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: OPERATOR(**)
USE FEVariable_Method, ONLY: POWER_
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3)

a = [-1.0_DFP, 2.0_DFP, 3.0_DFP]

! Test 1: obj ** n for n = 2, 3 and 0
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(a, Scalar, Space)**2
  CALL OK(ALL(ABS(Get(ans, Scalar, Space) - a**2) .LT. tol), "obj ** 2")
  ans = QuadratureVariable(a, Scalar, Space)**3
  CALL OK(ALL(ABS(Get(ans, Scalar, Space) - a**3) .LT. tol), "obj ** 3")
  ans = QuadratureVariable(a, Scalar, Space)**0
  CALL OK(ALL(ABS(Get(ans, Scalar, Space) - 1.0_DFP) .LT. tol), "obj ** 0")
END BLOCK

! Test 2: POWER_ writes into an existing variable
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = QuadratureVariable(a, Scalar, Space)
  ans = obj
  CALL POWER_(obj=obj, n=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - a**2) .LT. tol), "POWER_")
END BLOCK

END PROGRAM main
