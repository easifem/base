!> author: Shion Shimizu
! date: 2026-11-17
! summary: ConvergenceIn_ToInt(name)

PROGRAM main
USE BaseType, ONLY: TypeConvergenceOpt
USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToInt
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: upper case names
BLOCK
  CALL OK(ConvergenceIn_ToInt("RESIDUAL") .EQ. TypeConvergenceOpt%res, &
          "RESIDUAL")
  CALL OK(ConvergenceIn_ToInt("SOLUTION") .EQ. TypeConvergenceOpt%sol, &
          "SOLUTION")
  CALL OK(ConvergenceIn_ToInt("BOTH") .EQ. TypeConvergenceOpt%both, &
          "BOTH")
END BLOCK

! Test 2: the case of the name does not matter
BLOCK
  CALL OK(ConvergenceIn_ToInt("residual") .EQ. TypeConvergenceOpt%res, &
          "residual")
  CALL OK(ConvergenceIn_ToInt("Solution") .EQ. TypeConvergenceOpt%sol, &
          "Solution")
  CALL OK(ConvergenceIn_ToInt("both") .EQ. TypeConvergenceOpt%both, &
          "both")
END BLOCK

END PROGRAM main
