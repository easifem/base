!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: CellBasis_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2,4),v(4,1)

x = RESHAPE([-1.0_DFP,-1.0_DFP,1.0_DFP,-1.0_DFP, &
  1.0_DFP,1.0_DFP,-1.0_DFP,1.0_DFP],[2,4])
v = CellBasis_Quadrangle(pb=2_I4B,qb=2_I4B,x=x(1,:),y=x(2,:))
CALL OK(MAXVAL(ABS(v)) &
  < 1.0E-12_DFP, "Interior entity modes vanish at vertices")
END PROGRAM main
