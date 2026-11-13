!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Variable case of two variables

PROGRAM main
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: GetVarCase
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: varCase = 10 * a + b, constant 0, space 1, time 2, spaceTime 3
BLOCK
  CALL OK(GetVarCase(fevaropt%constant, fevaropt%constant) .EQ. 00, "00")
  CALL OK(GetVarCase(fevaropt%constant, fevaropt%spacetime) .EQ. 03, "03")
  CALL OK(GetVarCase(fevaropt%space, fevaropt%time) .EQ. 12, "12")
  CALL OK(GetVarCase(fevaropt%time, fevaropt%space) .EQ. 21, "21")
  CALL OK(GetVarCase(fevaropt%time, fevaropt%time) .EQ. 22, "22")
  CALL OK(GetVarCase(fevaropt%spacetime, fevaropt%space) .EQ. 31, "31")
  CALL OK(GetVarCase(fevaropt%spacetime, fevaropt%spacetime) .EQ. 33, &
          "33")
END BLOCK

END PROGRAM main
