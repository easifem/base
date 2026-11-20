!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing QuadraturePoint_Quadrangle for anisotropic orders, given
! point counts, the unit square and a general quadrangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: GaussLegendreQP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP

! Test 1: anisotropic orders p=2, q=4 integrate x^2 y^4 exactly
BLOCK
  REAL(DFP), ALLOCATABLE :: qp(:, :)

  qp = QuadraturePoint_Quadrangle(p=2_I4B, q=4_I4B, &
                                  quadType1=GaussLegendreQP, &
                                  quadType2=GaussLegendreQP, &
                                  refQuadrangle="BIUNIT")
  CALL OK(SIZE(qp, 1) .EQ. 3, "p=2, q=4: three rows (x, y, weight)")
  CALL OK(ABS(SUM(qp(3, :)) - 4.0_DFP) .LT. tol, &
          "p=2, q=4: weights sum to 4")
  CALL OK(ABS(SUM(qp(3, :) * qp(1, :)**2 * qp(2, :)**4) - &
          (2.0_DFP / 3.0_DFP) * (2.0_DFP / 5.0_DFP)) .LT. tol, &
          "p=2, q=4: integral of x^2 y^4")
END BLOCK

! Test 2: given number of points in each direction
BLOCK
  REAL(DFP), ALLOCATABLE :: qp(:, :)

  qp = QuadraturePoint_Quadrangle(nips=[3_I4B], quadType=GaussLegendreQP, &
                                  refQuadrangle="BIUNIT")
  CALL OK(SIZE(qp, 2) .EQ. 9, "nips=3: 9 points")
  CALL OK(ABS(SUM(qp(3, :)) - 4.0_DFP) .LT. tol, &
          "nips=3: weights sum to 4")

  qp = QuadraturePoint_Quadrangle(nipsx=[2_I4B], nipsy=[3_I4B], &
                                  quadType1=GaussLegendreQP, &
                                  quadType2=GaussLegendreQP, &
                                  refQuadrangle="BIUNIT")
  CALL OK(SIZE(qp, 2) .EQ. 6, "nipsx=2, nipsy=3: 6 points")
  CALL OK(ABS(SUM(qp(3, :) * qp(2, :)**4) - 4.0_DFP / 5.0_DFP) .LT. tol, &
          "nipsx=2, nipsy=3: integral of y^4")
END BLOCK

! Test 3: unit square
BLOCK
  REAL(DFP), ALLOCATABLE :: qp(:, :)

  qp = QuadraturePoint_Quadrangle(order=2_I4B, quadType=GaussLegendreQP, &
                                  refQuadrangle="UNIT")
  CALL OK(ABS(SUM(qp(3, :)) - 1.0_DFP) .LT. tol, &
          "unit square: weights sum to 1")
  CALL OK(ABS(SUM(qp(3, :) * qp(1, :)) - 0.5_DFP) .LT. tol, &
          "unit square: integral of x is 1/2")
  CALL OK(MINVAL(qp(1:2, :)) .GT. 0.0_DFP .AND. &
          MAXVAL(qp(1:2, :)) .LT. 1.0_DFP, &
          "unit square: points lie inside")
END BLOCK

! Test 4: trapezoid (0,0), (2,0), (1,1), (0,1), area 3/2
! The integral of x over it is 7/6
BLOCK
  REAL(DFP), ALLOCATABLE :: qp(:, :)
  REAL(DFP) :: xij(2, 4)

  xij(:, 1) = [0.0_DFP, 0.0_DFP]
  xij(:, 2) = [2.0_DFP, 0.0_DFP]
  xij(:, 3) = [1.0_DFP, 1.0_DFP]
  xij(:, 4) = [0.0_DFP, 1.0_DFP]

  qp = QuadraturePoint_Quadrangle(order=3_I4B, quadType=GaussLegendreQP, &
                                  refQuadrangle="BIUNIT", xij=xij)
  CALL OK(ABS(SUM(qp(3, :)) - 1.5_DFP) .LT. tol, &
          "trapezoid: weights sum to the area")
  CALL OK(ABS(SUM(qp(3, :) * qp(1, :)) - 7.0_DFP / 6.0_DFP) .LT. tol, &
          "trapezoid: integral of x is 7/6")
END BLOCK

END PROGRAM main
