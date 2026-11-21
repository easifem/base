!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Status codes of implicit 2D line intersection.
!
! A line is a x + b y + c = 0. Status -1 or -2 marks a degenerate first or
! second line, 0 parallel lines, 1 one point, 2 coincident lines.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Line_Method, ONLY: lines_imp_int_2d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p(2)
INTEGER(I4B) :: ival

! Test 1: parallel lines x = 1 and x = 2
BLOCK
  CALL lines_imp_int_2d(1.0_DFP, 0.0_DFP, -1.0_DFP, &
                        1.0_DFP, 0.0_DFP, -2.0_DFP, ival, p)
  CALL OK(ival == 0, "Parallel lines give 0")
END BLOCK

! Test 2: coincident lines x = 1 and 2x = 2
BLOCK
  CALL lines_imp_int_2d(1.0_DFP, 0.0_DFP, -1.0_DFP, &
                        2.0_DFP, 0.0_DFP, -2.0_DFP, ival, p)
  CALL OK(ival == 2, "Coincident lines give 2")
END BLOCK

! Test 3: coincident lines y = 1 and 3y = 3 (a1 = 0 branch)
BLOCK
  CALL lines_imp_int_2d(0.0_DFP, 1.0_DFP, -1.0_DFP, &
                        0.0_DFP, 3.0_DFP, -3.0_DFP, ival, p)
  CALL OK(ival == 2, "Coincident horizontal lines give 2")
END BLOCK

! Test 4: degenerate first line
BLOCK
  CALL lines_imp_int_2d(0.0_DFP, 0.0_DFP, 1.0_DFP, &
                        1.0_DFP, 0.0_DFP, -2.0_DFP, ival, p)
  CALL OK(ival == -1, "Degenerate first line gives -1")
END BLOCK

! Test 5: degenerate second line
BLOCK
  CALL lines_imp_int_2d(1.0_DFP, 0.0_DFP, -1.0_DFP, &
                        0.0_DFP, 0.0_DFP, 1.0_DFP, ival, p)
  CALL OK(ival == -2, "Degenerate second line gives -2")
END BLOCK

END PROGRAM main
