!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Display the content of a FEVariable

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: Display
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: Display writes the message, rank, varType and sizes to a unit
BLOCK
  TYPE(FEVariable_) :: obj
  INTEGER(I4B) :: unitNo, ios
  CHARACTER(256) :: line, msg
  LOGICAL(LGT) :: hasMsg, hasRank, hasVarType, hasDefineOn, hasLen

  obj = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
                              [2, 2]), Vector, Space)

  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(obj, "my variable", unitNo=unitNo)
  REWIND (unitNo)

  hasMsg = .FALSE.
  hasRank = .FALSE.
  hasVarType = .FALSE.
  hasDefineOn = .FALSE.
  hasLen = .FALSE.
  readLoop: DO
    READ (unitNo, '(A)', iostat=ios, iomsg=msg) line
    IF (ios .NE. 0) EXIT readLoop
    hasMsg = hasMsg .OR. INDEX(line, "my variable") .GT. 0
    hasRank = hasRank .OR. INDEX(line, "rank: 1 (Vector)") .GT. 0
    hasVarType = hasVarType .OR. INDEX(line, "varType: Space") .GT. 0
    hasDefineOn = hasDefineOn .OR. INDEX(line, "defineOn: nodal") .GT. 0
    hasLen = hasLen .OR. INDEX(line, "len:") .GT. 0
  END DO readLoop
  CLOSE (unitNo)

  CALL OK(hasMsg, "message is written")
  CALL OK(hasRank, "rank is written")
  CALL OK(hasVarType, "varType is written")
  CALL OK(hasDefineOn, "defineOn is written")
  CALL OK(hasLen, "len is written")
END BLOCK

END PROGRAM main
