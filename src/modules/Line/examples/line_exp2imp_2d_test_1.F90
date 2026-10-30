!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for Line_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Line_Method, ONLY: line_exp2imp_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p1(2), p2(2)
REAL(DFP) :: a, b, c

p1 = [0.0_DFP, 0.0_DFP]
p2 = [2.0_DFP, 0.0_DFP]
CALL line_exp2imp_2d(p1, p2, a, b, c)
CALL OK(ABS(a*p1(1)+b*p1(2)+c) < 1.0E-12_DFP .AND. &
  ABS(a*p2(1)+b*p2(2)+c) < 1.0E-12_DFP, "Both endpoints lie on line")
END PROGRAM main
