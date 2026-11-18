!> author: Shion Shimizu
! date: 2026-11-17
! summary: ConvergenceIn_ToChar(name, isUpper)

PROGRAM main
USE BaseType, ONLY: TypeConvergenceOpt
USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToChar
USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToInt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: default names
BLOCK
  CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%res) .EQ. "Residual", &
          "res: Residual")
  CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%sol) .EQ. "Solution", &
          "sol: Solution")
  CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%both) .EQ. "Both", &
          "both: Both")
END BLOCK

! Test 2: upper case names
BLOCK
  CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%res, isUpper=.TRUE.) &
          .EQ. "RESIDUAL", "res: RESIDUAL")
  CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%sol, isUpper=.TRUE.) &
          .EQ. "SOLUTION", "sol: SOLUTION")
  CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%both, isUpper=.TRUE.) &
          .EQ. "BOTH", "both: BOTH")
END BLOCK

! Test 3: round trip through ConvergenceIn_ToInt
BLOCK
  INTEGER(I4B) :: values(3), ii

  values = [TypeConvergenceOpt%res, TypeConvergenceOpt%sol, &
            TypeConvergenceOpt%both]
  DO ii = 1, 3
    CALL OK(ConvergenceIn_ToInt(ConvergenceIn_ToChar(values(ii))) .EQ. &
            values(ii), "round trip: "//ConvergenceIn_ToChar(values(ii)))
  END DO
END BLOCK

END PROGRAM main
