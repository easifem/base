!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for Line_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Line_Method, ONLY: lines_imp_int_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p1(2), p2(2)
REAL(DFP) :: p(2)
INTEGER(I4B) :: status

p1 = [0.0_DFP, 0.0_DFP]
p2 = [2.0_DFP, 0.0_DFP]
CALL lines_imp_int_2d(1.0_DFP, 0.0_DFP, -1.0_DFP, &
  0.0_DFP, 1.0_DFP, -2.0_DFP, status, p)
CALL OK(status == 1, "Unique intersection")
CALL OK(MAXVAL(ABS(p-[1.0_DFP, 2.0_DFP])) < 1.0E-12_DFP, &
  "Intersection solves both line equations")
END PROGRAM main
