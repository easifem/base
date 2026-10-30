!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y(1, 1)
INTEGER(I4B) :: nr, nc

CALL EquidistanceInPoint_Line_(order=2_I4B, ans=y, nrow=nr, ncol=nc)
CALL OK(nr == 1 .AND. nc == 1, "Active coordinate dimensions")
CALL OK(MAXVAL(ABS(y-(0.0_DFP))) < 1.0E-11_DFP, &
  "Single interior point is the centroid")
END PROGRAM main
