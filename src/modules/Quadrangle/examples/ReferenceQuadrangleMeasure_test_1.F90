!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for ReferenceQuadrangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: ReferenceQuadrangle_
USE ReferenceQuadrangle_Method, ONLY: Quadrangle_Quality
USE ReferenceQuadrangle_Method, ONLY: Quality_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: Measure_Simplex_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceQuadrangle_) :: ref
REAL(DFP) :: y,q

ref = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
y = Measure_Simplex_Quadrangle(ref,ref%xij)
CALL OK(ABS(y-(1.0_DFP)) < 1.0E-11_DFP, "Reference measure")
q = Quadrangle_Quality(ref,ref%xij,measure=1_I4B)
CALL OK(q == 0.0_DFP .AND. Quality_Quadrangle(ref,ref%xij,1_I4B) == q, &
  "Quality aliases currently return a placeholder zero")
END PROGRAM main
