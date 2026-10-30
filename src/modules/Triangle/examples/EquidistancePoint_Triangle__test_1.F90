!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y(2, 6)
INTEGER(I4B) :: nr, nc

CALL EquidistancePoint_Triangle_(order=2_I4B, ans=y, nrow=nr, ncol=nc)
CALL OK(nr == 2 .AND. nc == 6, "Active coordinate dimensions")
CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
END PROGRAM main
