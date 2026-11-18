!> author: Shion Shimizu
! date: 2026-11-17
! summary: ConvergenceIn_ToString(name, isUpper)

PROGRAM main
USE BaseType, ONLY: TypeConvergenceOpt
USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToString
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: same text as ConvergenceIn_ToChar, as a String
BLOCK
  TYPE(String) :: astr

  astr = ConvergenceIn_ToString(TypeConvergenceOpt%res)
  CALL OK(astr%Chars() .EQ. "Residual", "res: Residual")
  astr = ConvergenceIn_ToString(TypeConvergenceOpt%sol, isUpper=.TRUE.)
  CALL OK(astr%Chars() .EQ. "SOLUTION", "sol: SOLUTION")
  astr = ConvergenceIn_ToString(TypeConvergenceOpt%both)
  CALL OK(astr%Chars() .EQ. "Both", "both: Both")
END BLOCK

END PROGRAM main
