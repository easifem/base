!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceLine_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE BaseType, ONLY: ReferenceLine_
USE BaseType, ONLY: ReferenceElement_
USE ReferenceLine_Method, ONLY: HighOrderElement_Line
USE ReferenceLine_Method, ONLY: Measure_Simplex_Line
USE ReferenceLine_Method, ONLY: Line_Quality
USE ReferenceLine_Method, ONLY: Quality_Line
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceLine_) :: ref
TYPE(ReferenceElement_) :: high
REAL(DFP) :: x(1, 2), q

ref = ReferenceLine(nsd=1_I4B, domainName="BIUNIT")
x(1, :) = [-1.0_DFP, 1.0_DFP]
CALL HighOrderElement_Line(refelem=ref, order=2_I4B, &
  obj=high, ipType=Equidistance)
CALL OK(SIZE(high%xij, 2) == 3, "Quadratic line has three nodes")
CALL OK(ABS(Measure_Simplex_Line(ref, x)-2.0_DFP) < 1.0E-12_DFP, &
  "Segment length")
q = Line_Quality(ref, x, measure=1_I4B)
CALL OK(q == 0.0_DFP .AND. Quality_Line(ref, x, 1_I4B) == q, &
  "Quality routine currently returns its placeholder value")
END PROGRAM main
