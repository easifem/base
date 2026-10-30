!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for ReferenceTriangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: ReferenceTriangle_
USE BaseType, ONLY: QualityMeasure
USE ReferenceTriangle_Method, ONLY: Triangle_Quality
USE ReferenceTriangle_Method, ONLY: Quality_Triangle
USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: t2(2, 3), t3(3, 3)
TYPE(ReferenceTriangle_) :: ref
REAL(DFP) :: y

t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
t3 = 0.0_DFP
t3(1:2, :) = t2
ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
y = Triangle_Quality(ref,t2,QualityMeasure%area)
CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP .AND. &
  ABS(Quality_Triangle(ref,t2,QualityMeasure%area)-y) < 1.0E-11_DFP, &
  "Area quality aliases agree")
END PROGRAM main
