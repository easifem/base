!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTriangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: ReferenceTriangle_
USE ReferenceTriangle_Method, ONLY: Barycentric
USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
TYPE(ReferenceTriangle_) :: ref
REAL(DFP) :: y(3)

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
y = Barycentric(ref, t2, [0.5_DFP, 0.25_DFP])
CALL OK(MAXVAL(ABS(MATMUL(t2,y)- &
  [0.5_DFP, 0.25_DFP])) < 1.0E-11_DFP, "Barycentric reconstruction")
END PROGRAM main
