!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Rank case of two variables

PROGRAM main
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: GetRankCase
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: rankCase = 10 * a + b, with scalar 0, vector 1, matrix 2
BLOCK
  CALL OK(GetRankCase(fevaropt%scalar, fevaropt%scalar) .EQ. 00, "00")
  CALL OK(GetRankCase(fevaropt%scalar, fevaropt%vector) .EQ. 01, "01")
  CALL OK(GetRankCase(fevaropt%scalar, fevaropt%matrix) .EQ. 02, "02")
  CALL OK(GetRankCase(fevaropt%vector, fevaropt%scalar) .EQ. 10, "10")
  CALL OK(GetRankCase(fevaropt%vector, fevaropt%vector) .EQ. 11, "11")
  CALL OK(GetRankCase(fevaropt%vector, fevaropt%matrix) .EQ. 12, "12")
  CALL OK(GetRankCase(fevaropt%matrix, fevaropt%scalar) .EQ. 20, "20")
  CALL OK(GetRankCase(fevaropt%matrix, fevaropt%vector) .EQ. 21, "21")
  CALL OK(GetRankCase(fevaropt%matrix, fevaropt%matrix) .EQ. 22, "22")
END BLOCK

END PROGRAM main
