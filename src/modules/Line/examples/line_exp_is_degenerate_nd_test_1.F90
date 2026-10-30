!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Line_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Line_Method, ONLY: line_exp_is_degenerate_nd
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p1(2), p2(2)
LOGICAL(LGT) :: deg

p1 = [0.0_DFP, 0.0_DFP]
p2 = [2.0_DFP, 0.0_DFP]
deg = line_exp_is_degenerate_nd(2_I4B, p1, p1)
CALL OK(deg, "Coincident endpoints are degenerate")
END PROGRAM main
