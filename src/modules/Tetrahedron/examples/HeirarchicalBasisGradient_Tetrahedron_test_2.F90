!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing HeirarchicalBasisGradient_Tetrahedron for orders 2 to 4

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: &
  HeirarchicalBasisGradient_Tetrahedron
IMPLICIT NONE

! The gradient must match the central difference of
! HeirarchicalBasis_Tetrahedron. Order 2 has edge modes, order 3 adds
! facet modes and order 4 adds one cell mode.
INTEGER(I4B), PARAMETER :: np = 2
REAL(DFP), PARAMETER :: tol = 1.0E-6_DFP
REAL(DFP) :: xu(3, np), xb(3, np)

xu(:, 1) = [0.1_DFP, 0.2_DFP, 0.3_DFP]
xu(:, 2) = [0.3_DFP, 0.15_DFP, 0.25_DFP]
xb = 2.0_DFP * xu - 1.0_DFP

! Test 1: order 2 on UNIT, 10 modes
BLOCK
  REAL(DFP) :: g(np, 10, 3), gfd(np, 10, 3)
  g = HeirarchicalBasisGradient_Tetrahedron(order=2_I4B, xij=xu, &
                                            refTetrahedron="UNIT")
  CALL CentralDiff(2_I4B, xu, "UNIT", gfd)
  CALL OK(MAXVAL(ABS(g - gfd)) .LT. tol, "order 2, UNIT")
END BLOCK

! Test 2: order 3 on UNIT, 20 modes with 4 facet modes
BLOCK
  REAL(DFP) :: g(np, 20, 3), gfd(np, 20, 3)
  g = HeirarchicalBasisGradient_Tetrahedron(order=3_I4B, xij=xu, &
                                            refTetrahedron="UNIT")
  CALL CentralDiff(3_I4B, xu, "UNIT", gfd)
  CALL OK(MAXVAL(ABS(g - gfd)) .LT. tol, "order 3, UNIT")
END BLOCK

! Test 3: order 4 on UNIT, 35 modes with 1 cell mode
BLOCK
  REAL(DFP) :: g(np, 35, 3), gfd(np, 35, 3)
  g = HeirarchicalBasisGradient_Tetrahedron(order=4_I4B, xij=xu, &
                                            refTetrahedron="UNIT")
  CALL CentralDiff(4_I4B, xu, "UNIT", gfd)
  CALL OK(MAXVAL(ABS(g - gfd)) .LT. tol, "order 4, UNIT")
END BLOCK

! Test 4: order 3 on BIUNIT
BLOCK
  REAL(DFP) :: g(np, 20, 3), gfd(np, 20, 3)
  g = HeirarchicalBasisGradient_Tetrahedron(order=3_I4B, xij=xb, &
                                            refTetrahedron="BIUNIT")
  CALL CentralDiff(3_I4B, xb, "BIUNIT", gfd)
  CALL OK(MAXVAL(ABS(g - gfd)) .LT. tol, "order 3, BIUNIT")
END BLOCK

CONTAINS

SUBROUTINE CentralDiff(order, x, ref, ans)
  INTEGER(I4B), INTENT(IN) :: order
  REAL(DFP), INTENT(IN) :: x(:, :)
  CHARACTER(*), INTENT(IN) :: ref
  REAL(DFP), INTENT(OUT) :: ans(:, :, :)
  REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
  REAL(DFP) :: xp(3, SIZE(x, 2)), xm(3, SIZE(x, 2))
  REAL(DFP) :: vp(SIZE(ans, 1), SIZE(ans, 2)), vm(SIZE(ans, 1), SIZE(ans, 2))
  INTEGER(I4B) :: k
  DO k = 1, 3
    xp = x
    xm = x
    xp(k, :) = xp(k, :) + h
    xm(k, :) = xm(k, :) - h
    vp = HeirarchicalBasis_Tetrahedron(order=order, xij=xp, &
                                       refTetrahedron=ref)
    vm = HeirarchicalBasis_Tetrahedron(order=order, xij=xm, &
                                       refTetrahedron=ref)
    ans(:, :, k) = (vp - vm) / (2.0_DFP * h)
  END DO
END SUBROUTINE CentralDiff

END PROGRAM main
