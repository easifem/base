!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing Dubiner_Quadrangle with points given as a grid

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: Dubiner_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: grid form agrees with the point form, y varies fastest
BLOCK
  REAL(DFP) :: x(2), y(3), pts(2, 6), vg(6, 6), vp(6, 6)
  INTEGER(I4B) :: ii, jj

  x = [-0.5_DFP, 0.25_DFP]
  y = [-0.8_DFP, 0.1_DFP, 0.6_DFP]
  DO ii = 1, 2
    DO jj = 1, 3
      pts(1, 3 * (ii - 1) + jj) = x(ii)
      pts(2, 3 * (ii - 1) + jj) = y(jj)
    END DO
  END DO

  vg = Dubiner_Quadrangle(order=2_I4B, x=x, y=y)
  vp = Dubiner_Quadrangle(order=2_I4B, xij=pts)
  CALL OK(MAXVAL(ABS(vg - vp)) .LT. tol, "grid form equals point form")
END BLOCK

! Test 2: the first mode is the constant 1
BLOCK
  REAL(DFP) :: vg(6, 6)

  vg = Dubiner_Quadrangle(order=2_I4B, x=[-0.5_DFP, 0.25_DFP], &
                          y=[-0.8_DFP, 0.1_DFP, 0.6_DFP])
  CALL OK(MAXVAL(ABS(vg(:, 1) - 1.0_DFP)) .LT. tol, "constant mode")
END BLOCK

END PROGRAM main
