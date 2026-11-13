!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, &
  ONLY: OrthogonalBasisGradient_Tetrahedron
USE TetrahedronInterpolationUtility, &
  ONLY: OrthogonalBasisGradient_Tetrahedron_
USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3,2),xp(3,2),xm(3,2),vp(2,4),vm(2,4),g(2,4,3)
REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
INTEGER(I4B) :: k

! Current analytic gradient assembly overwrites derivative component two
! and never fills component three. Differentiate working values instead.
x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
DO k = 1,3
  xp = x
  xm = x
  xp(k,:) = xp(k,:)+h
  xm(k,:) = xm(k,:)-h
  vp = OrthogonalBasis_Tetrahedron(1_I4B,xp,"UNIT")
  vm = OrthogonalBasis_Tetrahedron(1_I4B,xm,"UNIT")
  g(:,:,k) = (vp-vm)/(2.0_DFP*h)
END DO
CALL OK(MAXVAL(ABS(g(:,1,:))) < 1.0E-8_DFP, "Constant mode gradient")
CALL OK(MAXVAL(ABS(g(:,2,3)-4.0_DFP)) < 1.0E-8_DFP, &
  "Numerical derivative of the first nonconstant mode")
END PROGRAM main
