!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing TensorProdBasis_Quadrangle with a different basis in each
! direction and with points given as a grid

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: xij(2, 3)

xij(:, 1) = [0.2_DFP, -0.4_DFP]
xij(:, 2) = [-0.3_DFP, 0.5_DFP]
xij(:, 3) = [0.7_DFP, 0.9_DFP]

! Test 1: monomial in x and Legendre in y, p=1, q=2
! Column k2*(p+1)+k1+1 holds x^k1 * P_k2(y)
BLOCK
  REAL(DFP) :: v(3, 6), px(3), py(3)

  v = TensorProdBasis_Quadrangle(p=1_I4B, q=2_I4B, xij=xij, &
                                 basisType1=poly%monomial, &
                                 basisType2=poly%legendre)
  px = xij(1, :)
  py = 0.5_DFP * (3.0_DFP * xij(2, :)**2 - 1.0_DFP)
  CALL OK(MAXVAL(ABS(v(:, 2) - px)) .LT. tol, &
          "mode (1, 0) is x")
  CALL OK(MAXVAL(ABS(v(:, 5) - py)) .LT. tol, &
          "mode (0, 2) is the Legendre polynomial P2(y)")
  CALL OK(MAXVAL(ABS(v(:, 6) - px * py)) .LT. tol, &
          "mode (1, 2) is x * P2(y)")
END BLOCK

! Test 2: grid form agrees with the point form, y varies fastest
BLOCK
  REAL(DFP) :: x(2), y(3), pts(2, 6), vg(6, 4), vp(6, 4)
  INTEGER(I4B) :: ii, jj

  x = [-0.5_DFP, 0.25_DFP]
  y = [-0.8_DFP, 0.1_DFP, 0.6_DFP]
  DO ii = 1, 2
    DO jj = 1, 3
      pts(1, 3 * (ii - 1) + jj) = x(ii)
      pts(2, 3 * (ii - 1) + jj) = y(jj)
    END DO
  END DO

  vg = TensorProdBasis_Quadrangle(p=1_I4B, q=1_I4B, x=x, y=y, &
                                  basisType1=poly%legendre, &
                                  basisType2=poly%legendre)
  vp = TensorProdBasis_Quadrangle(p=1_I4B, q=1_I4B, xij=pts, &
                                  basisType1=poly%legendre, &
                                  basisType2=poly%legendre)
  CALL OK(MAXVAL(ABS(vg - vp)) .LT. tol, "grid form equals point form")
END BLOCK

END PROGRAM main
