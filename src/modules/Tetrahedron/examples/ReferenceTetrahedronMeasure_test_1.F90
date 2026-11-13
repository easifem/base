!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTetrahedron_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: ReferenceTetrahedron_
USE ReferenceTetrahedron_Method, ONLY: Tetrahedron_Quality
USE ReferenceTetrahedron_Method, ONLY: Quality_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: Measure_Simplex_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceTetrahedron_) :: ref
REAL(DFP) :: y,q

ref = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
y = Measure_Simplex_Tetrahedron(ref,ref%xij)
CALL OK(ABS(y-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, "Reference measure")
q = Tetrahedron_Quality(ref,ref%xij,measure=1_I4B)
CALL OK(q == 0.0_DFP .AND. Quality_Tetrahedron(ref,ref%xij,1_I4B) == q, &
  "Quality aliases currently return a placeholder zero")
END PROGRAM main
