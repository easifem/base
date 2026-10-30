!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: VerticalEdgeBasis_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2,4),v(4,2)

x = RESHAPE([-1.0_DFP,-1.0_DFP,1.0_DFP,-1.0_DFP, &
  1.0_DFP,1.0_DFP,-1.0_DFP,1.0_DFP],[2,4])
v = VerticalEdgeBasis_Quadrangle(qe1=2_I4B,qe2=2_I4B,x=x(1,:),y=x(2,:))
CALL OK(MAXVAL(ABS(v)) &
  < 1.0E-12_DFP, "Interior entity modes vanish at vertices")
END PROGRAM main
