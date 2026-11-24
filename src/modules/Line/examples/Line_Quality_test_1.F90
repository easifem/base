!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Line_Quality is a placeholder that returns zero.

PROGRAM main
USE BaseType, ONLY: ReferenceLine_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceLine_Method, ONLY: Line_Quality
USE ReferenceLine_Method, ONLY: Quality_Line
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceLine_) :: ref
REAL(DFP) :: xij(1, 2)

ref = ReferenceLine(nsd=1_I4B)
xij(1, :) = [0.0_DFP, 3.0_DFP]

! Test 1: the specific name returns zero
BLOCK
  CALL OK(Line_Quality(refelem=ref, xij=xij, measure=1_I4B) == 0.0_DFP, &
          "Line_Quality returns zero")
END BLOCK

! Test 2: the generic name returns zero
BLOCK
  CALL OK(Quality_Line(refelem=ref, xij=xij, measure=2_I4B) == 0.0_DFP, &
          "Quality_Line returns zero")
END BLOCK

END PROGRAM main
