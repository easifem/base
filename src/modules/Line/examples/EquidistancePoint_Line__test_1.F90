!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: EquidistancePoint_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y(1, 3)
INTEGER(I4B) :: nr, nc

CALL EquidistancePoint_Line_(order=2_I4B, ans=y, nrow=nr, ncol=nc)
CALL OK(nr == 1 .AND. nc == 3, "Active coordinate dimensions")
CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
END PROGRAM main
