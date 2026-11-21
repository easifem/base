!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Where a FEVariable is defined, by GetDefineOn and .defineon.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE FEVariable_Method, ONLY: GetDefineOn
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: OPERATOR(.defineon.)
USE FEVariable_Method, ONLY: QuadratureVariable
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: nodal and quadrature variables
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL OK(GetDefineOn(obj) .EQ. fevaropt%nodal, "GetDefineOn = nodal")
  CALL OK((.defineon.obj) .EQ. fevaropt%nodal, ".defineon. = nodal")

  obj = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL OK(GetDefineOn(obj) .EQ. fevaropt%quadrature, &
          "GetDefineOn = quadrature")
END BLOCK

END PROGRAM main
