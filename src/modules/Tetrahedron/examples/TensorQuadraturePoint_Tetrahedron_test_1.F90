!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing TensorQuadraturePoint_Tetrahedron with a given order

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: &
  TensorQuadraturePoint_Tetrahedron
IMPLICIT NONE

! On the unit tetrahedron,
! int x^a y^b z^c = a! b! c! / (a + b + c + 3)!
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B), PARAMETER :: order = 4

! Test 1: order 4 gives n = 3, so 3 * 4 * 3 = 36 points
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = TensorQuadraturePoint_Tetrahedron(order=order, &
                                        quadType=GaussLegendreQP, &
                                        refTetrahedron="UNIT")
  CALL OK(SIZE(q, 1) .EQ. 4 .AND. SIZE(q, 2) .EQ. 36, "order 4: shape")
END BLOCK

! Test 2: UNIT rule, weights sum to 1/6 and the points lie inside
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = TensorQuadraturePoint_Tetrahedron(order=order, &
                                        quadType=GaussLegendreQP, &
                                        refTetrahedron="UNIT")
  CALL OK(ABS(SUM(q(4, :)) - 1.0_DFP / 6.0_DFP) .LT. tol, &
          "UNIT: weights sum to the volume")
  CALL OK(ALL(q(1:3, :) .GE. -tol) .AND. &
          ALL(SUM(q(1:3, :), dim=1) .LE. 1.0_DFP + tol), &
          "UNIT: points inside the tetrahedron")
END BLOCK

! Test 3: UNIT rule is exact up to degree 4
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  REAL(DFP) :: v
  q = TensorQuadraturePoint_Tetrahedron(order=order, &
                                        quadType=GaussLegendreQP, &
                                        refTetrahedron="UNIT")
  v = SUM(q(4, :) * q(1, :) * q(2, :) * q(3, :))
  CALL OK(ABS(v - 1.0_DFP / 720.0_DFP) .LT. tol, "UNIT: int x*y*z")
  v = SUM(q(4, :) * q(1, :)**2 * q(2, :)**2)
  CALL OK(ABS(v - 1.0_DFP / 1260.0_DFP) .LT. tol, "UNIT: int x^2*y^2")
  v = SUM(q(4, :) * q(1, :)**4)
  CALL OK(ABS(v - 1.0_DFP / 210.0_DFP) .LT. tol, "UNIT: int x^4")
  v = SUM(q(4, :) * q(3, :)**4)
  CALL OK(ABS(v - 1.0_DFP / 210.0_DFP) .LT. tol, "UNIT: int z^4")
END BLOCK

! Test 4: BIUNIT rule, weights sum to 8/6 and int x = -(8/6)/2
! The centroid of the BIUNIT tetrahedron is (-1/2, -1/2, -1/2).
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = TensorQuadraturePoint_Tetrahedron(order=order, &
                                        quadType=GaussLegendreQP, &
                                        refTetrahedron="BIUNIT")
  CALL OK(ABS(SUM(q(4, :)) - 8.0_DFP / 6.0_DFP) .LT. tol, &
          "BIUNIT: weights sum to the volume")
  CALL OK(ABS(SUM(q(4, :) * q(1, :)) + 4.0_DFP / 6.0_DFP) .LT. tol, &
          "BIUNIT: int x")
END BLOCK

! Test 5: xij maps the rule to the tetrahedron 2 * UNIT
! Volume 8/6, centroid (1/2, 1/2, 1/2)
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  REAL(DFP) :: xij(3, 4), vol
  xij = 2.0_DFP * RefCoord_Tetrahedron("UNIT")
  q = TensorQuadraturePoint_Tetrahedron(order=order, &
                                        quadType=GaussLegendreQP, &
                                        refTetrahedron="UNIT", xij=xij)
  vol = SUM(q(4, :))
  CALL OK(ABS(vol - 8.0_DFP / 6.0_DFP) .LT. tol, "xij: volume")
  CALL OK(ABS(SUM(q(4, :) * q(3, :)) / vol - 0.5_DFP) .LT. tol, &
          "xij: centroid z")
END BLOCK

END PROGRAM main
