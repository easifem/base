!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for Line_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Line_Method, ONLY: line_imp_is_degenerate_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p1(2), p2(2)
LOGICAL(LGT) :: deg

p1 = [0.0_DFP, 0.0_DFP]
p2 = [2.0_DFP, 0.0_DFP]
deg = line_imp_is_degenerate_2d(0.0_DFP, 0.0_DFP, 1.0_DFP)
CALL OK(deg, "Zero normal is degenerate")
END PROGRAM main
