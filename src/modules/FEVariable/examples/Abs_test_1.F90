!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Absolute value of a FEVariable by ABS and ABS_

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: ABS
USE FEVariable_Method, ONLY: ABS_
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SHAPE
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(3), vs(2, 2)

a = [-1.0_DFP, 4.0_DFP, -9.0_DFP]
vs = RESHAPE([-1.0_DFP, 2.0_DFP, -3.0_DFP, 4.0_DFP], [2, 2])

! Test 1: ABS of every value
BLOCK
  TYPE(FEVariable_) :: ans

  ans = ABS(QuadratureVariable(a, Scalar, Space))
  CALL OK(ALL(Get(ans, Scalar, Space) .EQ. [1.0_DFP, 4.0_DFP, 9.0_DFP]), &
          "ABS scalar space")

  ans = ABS(QuadratureVariable(vs, Vector, Space))
  CALL OK(ALL(Get(ans, Vector, Space) .EQ. ABS(vs)), "ABS vector space")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2]), "ABS keeps the shape")
END BLOCK

! Test 2: ABS_ writes into an existing variable
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = QuadratureVariable(a, Scalar, Space)
  ans = obj
  CALL ABS_(obj=obj, ans=ans)
  CALL OK(ALL(ans%val(1:3) .EQ. [1.0_DFP, 4.0_DFP, 9.0_DFP]), "ABS_")
END BLOCK

END PROGRAM main
