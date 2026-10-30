!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTriangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: ReferenceTriangle_
USE ReferenceTriangle_Method, ONLY: ContainsLine
USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
TYPE(ReferenceTriangle_) :: ref
LOGICAL(LGT) :: inside
REAL(DFP) :: point(3)

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
CALL ContainsLine(ref, t3, [0.5_DFP,0.25_DFP,-1.0_DFP], &
  [0.0_DFP,0.0_DFP,1.0_DFP], .TRUE., inside, point)
CALL OK(inside .AND. ABS(point(3)) < 1.0E-11_DFP, &
  "Parametric line crosses triangle")
END PROGRAM main
