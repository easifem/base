!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing LagrangeCoeff_Tetrahedron with the orthogonal basis

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Orthogonal
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: EquidistancePoint_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, n = 10
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: nodes(:, :)
REAL(DFP) :: v(n, n), eye(n, n), x(3, 4)
INTEGER(I4B) :: ii

! Nodes of a smaller tetrahedron inside the unit one. The collapsed
! coordinates of the orthogonal basis are singular at the vertex (0,0,1).
x(:, 1) = [0.1_DFP, 0.1_DFP, 0.1_DFP]
x(:, 2) = [0.6_DFP, 0.1_DFP, 0.1_DFP]
x(:, 3) = [0.1_DFP, 0.6_DFP, 0.1_DFP]
x(:, 4) = [0.1_DFP, 0.1_DFP, 0.6_DFP]
nodes = EquidistancePoint_Tetrahedron(order=order, xij=x)
v = OrthogonalBasis_Tetrahedron(order=order, xij=nodes, &
                                refTetrahedron="UNIT")
eye = 0.0_DFP
DO ii = 1, n
  eye(ii, ii) = 1.0_DFP
END DO

! Test 1: refTetrahedron given, v * c = I
BLOCK
  REAL(DFP) :: c(n, n)
  c = LagrangeCoeff_Tetrahedron(order=order, xij=nodes, &
                                basisType=Orthogonal, refTetrahedron="UNIT")
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - eye)) .LT. tol, &
          "orthogonal basis with UNIT: v * c = I")
END BLOCK

! Test 2: refTetrahedron omitted, the default UNIT must be used
BLOCK
  REAL(DFP) :: c(n, n)
  c = LagrangeCoeff_Tetrahedron(order=order, xij=nodes, &
                                basisType=Orthogonal)
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - eye)) .LT. tol, &
          "orthogonal basis with default domain: v * c = I")
END BLOCK

END PROGRAM main
