!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for ReferenceQuadrangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceQuadrangle_Method, ONLY: QuadArea2D
USE ReferenceQuadrangle_Method, ONLY: QuadrangleArea2D
USE ReferenceQuadrangle_Method, ONLY: QuadArea3D
USE ReferenceQuadrangle_Method, ONLY: QuadrangleArea3D
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: q2(2,4),q3(3,4),a,b,c,d

q2 = RESHAPE([0.0_DFP,0.0_DFP,2.0_DFP,0.0_DFP, &
  2.0_DFP,1.0_DFP,0.0_DFP,1.0_DFP],[2,4])
q3 = 0.0_DFP
q3(1:2,:) = q2
CALL QuadArea2D(q2,a)
CALL QuadrangleArea2D(q2,b)
CALL QuadArea3D(q3,c)
CALL QuadrangleArea3D(q3,d)
CALL OK(MAXVAL(ABS([a,b,c,d]-2.0_DFP)) < 1.0E-11_DFP, &
  "Area aliases agree for a planar rectangle")
END PROGRAM main
