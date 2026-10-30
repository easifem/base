!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return interior interpolation DOFs for an element.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE InterpolationUtility, ONLY: GetTotalInDOF
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: ndof

ndof = GetTotalInDOF(elemType=Line, order=2_I4B, &
  baseContinuity="H1", baseInterpolation="Lagrange")
CALL OK(ndof == 1, "Quadratic line degree count")
END PROGRAM main
