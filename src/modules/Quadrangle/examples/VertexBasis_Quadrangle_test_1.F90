!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: VertexBasis_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2,4),v(4,4)

x = RESHAPE([-1.0_DFP,-1.0_DFP,1.0_DFP,-1.0_DFP, &
  1.0_DFP,1.0_DFP,-1.0_DFP,1.0_DFP],[2,4])
v = VertexBasis_Quadrangle(xij=x)
CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
  < 1.0E-12_DFP, "Vertex partition of unity")
END PROGRAM main
