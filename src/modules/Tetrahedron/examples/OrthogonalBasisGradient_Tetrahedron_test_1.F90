!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing OrthogonalBasisGradient_Tetrahedron against differences

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: &
  OrthogonalBasisGradient_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: &
  OrthogonalBasisGradient_Tetrahedron_
IMPLICIT NONE

! The gradient must match the central difference of
! OrthogonalBasis_Tetrahedron at interior points.
INTEGER(I4B), PARAMETER :: order = 2, n = 10, np = 2
REAL(DFP), PARAMETER :: tol = 1.0E-6_DFP
REAL(DFP) :: xu(3, np), xb(3, np)

xu(:, 1) = [0.1_DFP, 0.2_DFP, 0.3_DFP]
xu(:, 2) = [0.3_DFP, 0.15_DFP, 0.25_DFP]
xb = 2.0_DFP * xu - 1.0_DFP

! Test 1: function form on the UNIT tetrahedron
BLOCK
  REAL(DFP) :: g(np, n, 3), gfd(np, n, 3)
  g = OrthogonalBasisGradient_Tetrahedron(order=order, xij=xu, &
                                          refTetrahedron="UNIT")
  CALL CentralDiff(xu, "UNIT", gfd)
  CALL OK(MAXVAL(ABS(g - gfd)) .LT. tol, "UNIT: function form")
END BLOCK

! Test 2: function form on the BIUNIT tetrahedron
BLOCK
  REAL(DFP) :: g(np, n, 3), gfd(np, n, 3)
  g = OrthogonalBasisGradient_Tetrahedron(order=order, xij=xb, &
                                          refTetrahedron="BIUNIT")
  CALL CentralDiff(xb, "BIUNIT", gfd)
  CALL OK(MAXVAL(ABS(g - gfd)) .LT. tol, "BIUNIT: function form")
END BLOCK

! Test 3: subroutine form, sizes and values
BLOCK
  REAL(DFP) :: g(np, n, 3), gfd(np, n, 3)
  INTEGER(I4B) :: dim1, dim2, dim3
  CALL OrthogonalBasisGradient_Tetrahedron_(order=order, xij=xu, &
                                            refTetrahedron="UNIT", ans=g, &
                                            dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(dim1 .EQ. np .AND. dim2 .EQ. n .AND. dim3 .EQ. 3, &
          "subroutine form: sizes")
  CALL CentralDiff(xu, "UNIT", gfd)
  CALL OK(MAXVAL(ABS(g - gfd)) .LT. tol, "subroutine form: values")
END BLOCK

CONTAINS

SUBROUTINE CentralDiff(x, ref, ans)
  REAL(DFP), INTENT(IN) :: x(:, :)
  CHARACTER(*), INTENT(IN) :: ref
  REAL(DFP), INTENT(OUT) :: ans(:, :, :)
  REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
  REAL(DFP) :: xp(3, SIZE(x, 2)), xm(3, SIZE(x, 2))
  REAL(DFP) :: vp(SIZE(x, 2), n), vm(SIZE(x, 2), n)
  INTEGER(I4B) :: k
  DO k = 1, 3
    xp = x
    xm = x
    xp(k, :) = xp(k, :) + h
    xm(k, :) = xm(k, :) - h
    vp = OrthogonalBasis_Tetrahedron(order=order, xij=xp, &
                                     refTetrahedron=ref)
    vm = OrthogonalBasis_Tetrahedron(order=order, xij=xm, &
                                     refTetrahedron=ref)
    ans(:, :, k) = (vp - vm) / (2.0_DFP * h)
  END DO
END SUBROUTINE CentralDiff

END PROGRAM main
