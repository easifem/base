!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Variable type of the result for all variable cases
!
! GetVarType(varCase) gives the variable type of the result of a binary
! operation between two variables (see GetVarCase):
!
! - 00: constant
! - 01, 10, 11: space
! - 02, 20, 22: time
! - all others (one is spaceTime, or space with time): spaceTime

PROGRAM main
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: GetVarCase
USE FEVariable_Method, ONLY: GetVarType
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: each variable case
BLOCK
  CALL OK(GetVarType(varCase=00) .EQ. fevaropt%constant, "00")
  CALL OK(GetVarType(varCase=01) .EQ. fevaropt%space, "01")
  CALL OK(GetVarType(varCase=10) .EQ. fevaropt%space, "10")
  CALL OK(GetVarType(varCase=11) .EQ. fevaropt%space, "11")
  CALL OK(GetVarType(varCase=02) .EQ. fevaropt%time, "02")
  CALL OK(GetVarType(varCase=20) .EQ. fevaropt%time, "20")
  CALL OK(GetVarType(varCase=22) .EQ. fevaropt%time, "22")
  CALL OK(GetVarType(varCase=12) .EQ. fevaropt%spacetime, "12")
  CALL OK(GetVarType(varCase=21) .EQ. fevaropt%spacetime, "21")
  CALL OK(GetVarType(varCase=03) .EQ. fevaropt%spacetime, "03")
  CALL OK(GetVarType(varCase=13) .EQ. fevaropt%spacetime, "13")
  CALL OK(GetVarType(varCase=23) .EQ. fevaropt%spacetime, "23")
  CALL OK(GetVarType(varCase=30) .EQ. fevaropt%spacetime, "30")
  CALL OK(GetVarType(varCase=31) .EQ. fevaropt%spacetime, "31")
  CALL OK(GetVarType(varCase=32) .EQ. fevaropt%spacetime, "32")
  CALL OK(GetVarType(varCase=33) .EQ. fevaropt%spacetime, "33")
END BLOCK

! Test 2: GetVarType of GetVarCase of two variable types
BLOCK
  INTEGER(I4B) :: vc

  vc = GetVarCase(fevaropt%time, fevaropt%constant)
  CALL OK(GetVarType(varCase=vc) .EQ. fevaropt%time, "time, constant")

  vc = GetVarCase(fevaropt%time, fevaropt%space)
  CALL OK(GetVarType(varCase=vc) .EQ. fevaropt%spacetime, &
          "time, space: spaceTime")
END BLOCK

END PROGRAM main
