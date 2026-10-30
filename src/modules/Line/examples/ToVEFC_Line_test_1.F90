!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: ToVEFC_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p(3)

p = [-1.0_DFP,0.0_DFP,1.0_DFP]
CALL ToVEFC_Line(p)
CALL OK(MAXVAL(ABS(p-[-1.0_DFP,1.0_DFP,0.0_DFP])) &
  < 1.0E-12_DFP, "Endpoints precede interior points")
END PROGRAM main
