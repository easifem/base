!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing HeirarchicalBasis_Tetrahedron for order 4 at the vertices

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron
IMPLICIT NONE

! Order 4: 4 vertex, 18 edge, 12 face and 1 cell functions
INTEGER(I4B), PARAMETER :: order = 4, n = 35
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(3, 4)
REAL(DFP), ALLOCATABLE :: v(:, :)

x = RefCoord_Tetrahedron("UNIT")
v = HeirarchicalBasis_Tetrahedron(order=order, xij=x, refTetrahedron="UNIT")

! Test 1: one row per point and one column per function
BLOCK
  CALL OK(SIZE(v, 1) .EQ. 4 .AND. SIZE(v, 2) .EQ. n, "shape is (4, 35)")
END BLOCK

! Test 2: vertex functions are 1 at their own vertex and 0 at the others
BLOCK
  INTEGER(I4B) :: ii
  REAL(DFP) :: eye(4, 4)
  eye = 0.0_DFP
  DO ii = 1, 4
    eye(ii, ii) = 1.0_DFP
  END DO
  CALL OK(MAXVAL(ABS(v(:, 1:4) - eye)) .LT. tol, "vertex functions")
END BLOCK

! Test 3: edge, face and cell functions vanish at all vertices
BLOCK
  CALL OK(MAXVAL(ABS(v(:, 5:n))) .LT. tol, "bubbles vanish at vertices")
END BLOCK

END PROGRAM main
