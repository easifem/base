!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Check if a FEVariable is defined on quadrature points

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE FEVariable_Method, ONLY: IsQuadratureVariable
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: quadrature variable is quadrature, nodal variable is not
BLOCK
  TYPE(FEVariable_) :: obj

  obj = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL OK(IsQuadratureVariable(obj), "quadrature variable: true")

  obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL OK(.NOT. IsQuadratureVariable(obj), "nodal variable: false")
END BLOCK

END PROGRAM main
