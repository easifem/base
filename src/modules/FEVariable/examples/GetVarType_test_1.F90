!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Variable type of a FEVariable and from a variable case

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE FEVariable_Method, ONLY: GetVarType
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: OPERATOR(.vartype.)
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: variable type of space, time and spaceTime variables
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL OK(GetVarType(obj) .EQ. fevaropt%space, "GetVarType = space")
  CALL OK((.vartype.obj) .EQ. fevaropt%space, ".vartype. = space")

  obj = NodalVariable(tsize=2, rank=Scalar, vartype=Time)
  CALL OK(GetVarType(obj) .EQ. fevaropt%time, "GetVarType = time")

  obj = NodalVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
  CALL OK((.vartype.obj) .EQ. fevaropt%spacetime, ".vartype. = spaceTime")
END BLOCK

! Test 2: variable type of the result from a var case (see GetVarCase)
BLOCK
  CALL OK(GetVarType(varCase=00) .EQ. fevaropt%constant, "varCase 00")
  CALL OK(GetVarType(varCase=01) .EQ. fevaropt%space, "varCase 01")
  CALL OK(GetVarType(varCase=11) .EQ. fevaropt%space, "varCase 11")
  CALL OK(GetVarType(varCase=20) .EQ. fevaropt%time, "varCase 20")
  CALL OK(GetVarType(varCase=22) .EQ. fevaropt%time, "varCase 22")
  CALL OK(GetVarType(varCase=12) .EQ. fevaropt%spacetime, "varCase 12")
  CALL OK(GetVarType(varCase=21) .EQ. fevaropt%spacetime, "varCase 21")
  CALL OK(GetVarType(varCase=03) .EQ. fevaropt%spacetime, "varCase 03")
  CALL OK(GetVarType(varCase=33) .EQ. fevaropt%spacetime, "varCase 33")
END BLOCK

END PROGRAM main
