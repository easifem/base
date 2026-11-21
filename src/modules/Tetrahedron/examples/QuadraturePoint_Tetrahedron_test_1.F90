!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing QuadraturePoint_Tetrahedron with nips, BIUNIT and xij

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: nips = 4 selects the 4 point rule of degree 2
! On the unit tetrahedron, int x*y = 1/120.
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = QuadraturePoint_Tetrahedron(nips=[4_I4B], quadType=GaussLegendreQP, &
                                  refTetrahedron="UNIT")
  CALL OK(SIZE(q, 1) .EQ. 4 .AND. SIZE(q, 2) .EQ. 4, "nips=4: shape")
  CALL OK(ABS(SUM(q(4, :)) - 1.0_DFP / 6.0_DFP) .LT. tol, &
          "nips=4: weights sum to the volume")
  CALL OK(ABS(SUM(q(4, :) * q(1, :) * q(2, :)) - 1.0_DFP / 120.0_DFP) &
          .LT. tol, "nips=4: integrates x*y")
END BLOCK

! Test 2: nips with no rule returns zero rows and columns
BLOCK
  REAL(DFP) :: q(4, 7)
  INTEGER(I4B) :: nrow, ncol
  CALL QuadraturePoint_Tetrahedron_(nips=[7_I4B], quadType=GaussLegendreQP, &
                                    refTetrahedron="UNIT", ans=q, &
                                    nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 0, "nips=7: no rule")
END BLOCK

! Test 3: BIUNIT tetrahedron has volume 8/6
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = QuadraturePoint_Tetrahedron(order=2_I4B, quadType=GaussLegendreQP, &
                                  refTetrahedron="BIUNIT")
  CALL OK(ABS(SUM(q(4, :)) - 8.0_DFP / 6.0_DFP) .LT. tol, &
          "BIUNIT: weights sum to the volume")
  CALL OK(ALL(q(1:3, :) .GE. -1.0_DFP) .AND. ALL(q(1:3, :) .LE. 1.0_DFP), &
          "BIUNIT: points inside [-1,1]")
END BLOCK

! Test 4: xij maps the points to a physical tetrahedron
! Tetrahedron 2*UNIT: volume 8/6, centroid (1/2,1/2,1/2)
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  REAL(DFP) :: xij(3, 4), vol
  xij = 2.0_DFP * RefCoord_Tetrahedron("UNIT")
  q = QuadraturePoint_Tetrahedron(order=2_I4B, quadType=GaussLegendreQP, &
                                  refTetrahedron="UNIT", xij=xij)
  vol = SUM(q(4, :))
  CALL OK(ABS(vol - 8.0_DFP / 6.0_DFP) .LT. tol, "xij: volume")
  CALL OK(ABS(SUM(q(4, :) * q(3, :)) / vol - 0.5_DFP) .LT. tol, &
          "xij: centroid z")
END BLOCK

END PROGRAM main
