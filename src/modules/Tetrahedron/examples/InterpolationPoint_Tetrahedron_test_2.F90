!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing the VEFC layout of InterpolationPoint_Tetrahedron, order 3

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: InterpolationPoint_Tetrahedron
IMPLICIT NONE

! VEFC order 3: 4 vertices, then 2 nodes on each of the 6 edges
! (edges 12, 13, 14, 23, 24, 34), then 1 node on each of the 4 faces
! (z=0, y=0, x=0, x+y+z=1).
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP, t = 1.0_DFP / 3.0_DFP
REAL(DFP), ALLOCATABLE :: nodes(:, :)

nodes = InterpolationPoint_Tetrahedron(order=3_I4B, ipType=Equidistance, &
                                       layout="VEFC")

! Test 1: 20 nodes, vertices first
BLOCK
  CALL OK(SIZE(nodes, 1) .EQ. 3 .AND. SIZE(nodes, 2) .EQ. 20, "shape")
  CALL OK(MAXVAL(ABS(nodes(:, 1:4) - RefCoord_Tetrahedron("UNIT"))) &
          .LT. tol, "vertices")
END BLOCK

! Test 2: nodes on edge 1 (vertex 1 to 2) and edge 6 (vertex 3 to 4)
BLOCK
  CALL OK(MAXVAL(ABS(nodes(:, 5) - [t, 0.0_DFP, 0.0_DFP])) .LT. tol .AND. &
          MAXVAL(ABS(nodes(:, 6) - [2 * t, 0.0_DFP, 0.0_DFP])) .LT. tol, &
          "edge 1 nodes go from vertex 1 to vertex 2")
  CALL OK(MAXVAL(ABS(nodes(:, 15) - [0.0_DFP, 2 * t, t])) .LT. tol .AND. &
          MAXVAL(ABS(nodes(:, 16) - [0.0_DFP, t, 2 * t])) .LT. tol, &
          "edge 6 nodes go from vertex 3 to vertex 4")
END BLOCK

! Test 3: one node at the centroid of each face
BLOCK
  CALL OK(MAXVAL(ABS(nodes(:, 17) - [t, t, 0.0_DFP])) .LT. tol, "face z=0")
  CALL OK(MAXVAL(ABS(nodes(:, 18) - [t, 0.0_DFP, t])) .LT. tol, "face y=0")
  CALL OK(MAXVAL(ABS(nodes(:, 19) - [0.0_DFP, t, t])) .LT. tol, "face x=0")
  CALL OK(MAXVAL(ABS(nodes(:, 20) - [t, t, t])) .LT. tol, "face x+y+z=1")
END BLOCK

! Test 4: xij maps the nodes to a physical tetrahedron
BLOCK
  REAL(DFP), ALLOCATABLE :: mapped(:, :)
  REAL(DFP) :: xij(3, 4)
  xij = 2.0_DFP * RefCoord_Tetrahedron("UNIT")
  mapped = InterpolationPoint_Tetrahedron(order=3_I4B, &
                                          ipType=Equidistance, &
                                          layout="VEFC", xij=xij)
  CALL OK(MAXVAL(ABS(mapped - 2.0_DFP * nodes)) .LT. tol, &
          "mapped nodes are scaled by 2")
END BLOCK

END PROGRAM main
