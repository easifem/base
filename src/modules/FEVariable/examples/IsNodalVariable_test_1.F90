!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Check if a FEVariable is defined on nodes

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE FEVariable_Method, ONLY: IsNodalVariable
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: nodal variable is nodal, quadrature variable is not
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL OK(IsNodalVariable(obj), "nodal variable: true")

  obj = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL OK(.NOT. IsNodalVariable(obj), "quadrature variable: false")
END BLOCK

END PROGRAM main
