!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Rank of a FEVariable and rank from a rank case

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: GetRank
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: OPERATOR(.rank.)
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: rank of scalar, vector and matrix variables
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(1.0_DFP, Scalar, Constant)
  CALL OK(GetRank(obj) .EQ. fevaropt%scalar, "GetRank = scalar")
  CALL OK((.rank.obj) .EQ. fevaropt%scalar, ".rank. = scalar")

  obj = NodalVariable(tsize=3, rank=Vector, vartype=Constant)
  CALL OK(GetRank(obj) .EQ. fevaropt%vector, "GetRank = vector")

  obj = NodalVariable(nrow=3, ncol=3, rank=Matrix, vartype=Constant)
  CALL OK((.rank.obj) .EQ. fevaropt%matrix, ".rank. = matrix")
END BLOCK

! Test 2: rank of the result from a rank case (see GetRankCase)
BLOCK
  CALL OK(GetRank(rankCase=00) .EQ. fevaropt%scalar, "rankCase 00: scalar")
  CALL OK(GetRank(rankCase=01) .EQ. fevaropt%vector, "rankCase 01: vector")
  CALL OK(GetRank(rankCase=10) .EQ. fevaropt%vector, "rankCase 10: vector")
  CALL OK(GetRank(rankCase=11) .EQ. fevaropt%vector, "rankCase 11: vector")
  CALL OK(GetRank(rankCase=02) .EQ. fevaropt%matrix, "rankCase 02: matrix")
  CALL OK(GetRank(rankCase=20) .EQ. fevaropt%matrix, "rankCase 20: matrix")
  CALL OK(GetRank(rankCase=22) .EQ. fevaropt%matrix, "rankCase 22: matrix")
  CALL OK(GetRank(rankCase=12) .EQ. fevaropt%matrix, "rankCase 12: matrix")
  CALL OK(GetRank(rankCase=21) .EQ. fevaropt%matrix, "rankCase 21: matrix")
END BLOCK

END PROGRAM main
