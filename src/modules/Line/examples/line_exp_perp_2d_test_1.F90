!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Line_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Line_Method, ONLY: line_exp_perp_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p1(2), p2(2)
REAL(DFP) :: foot(2)
LOGICAL(LGT) :: failed

p1 = [0.0_DFP, 0.0_DFP]
p2 = [2.0_DFP, 0.0_DFP]
CALL line_exp_perp_2d(p1, p2, [1.0_DFP, 1.0_DFP], foot, failed)
CALL OK(.NOT. failed, "Projection succeeds")
CALL OK(MAXVAL(ABS(foot-[1.0_DFP, 0.0_DFP])) < 1.0E-12_DFP, &
  "Orthogonal projection onto infinite line")
END PROGRAM main
