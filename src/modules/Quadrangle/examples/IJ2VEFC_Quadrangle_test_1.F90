!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing IJ2VEFC_Quadrangle for a square grid and for an
! anisotropic grid

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: IJ2VEFC_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: p=2, q=2 grid gives vertices, edge midpoints, then the centre
BLOCK
  REAL(DFP) :: x(3), xi(3, 3), eta(3, 3), temp(2, 9), expected(2, 9)
  INTEGER(I4B) :: ii, jj

  x = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
  DO jj = 1, 3
    DO ii = 1, 3
      xi(ii, jj) = x(ii)
      eta(ii, jj) = x(jj)
    END DO
  END DO

  expected = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
                      1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, &
                      0.0_DFP, -1.0_DFP, 1.0_DFP, 0.0_DFP, &
                      0.0_DFP, 1.0_DFP, -1.0_DFP, 0.0_DFP, &
                      0.0_DFP, 0.0_DFP], [2, 9])

  CALL IJ2VEFC_Quadrangle(xi=xi, eta=eta, temp=temp, p=2_I4B, q=2_I4B)
  CALL OK(MAXVAL(ABS(temp - expected)) .LT. tol, &
          "p=2, q=2: vertices, edges, then centre")
END BLOCK

! Test 2: p=2, q=3 grid, the two interior points come last
BLOCK
  REAL(DFP) :: x(3), y(4), xi(3, 4), eta(3, 4), temp(2, 12)
  REAL(DFP) :: inner(2, 2)
  INTEGER(I4B) :: ii, jj

  x = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
  y = [-1.0_DFP, -1.0_DFP / 3.0_DFP, 1.0_DFP / 3.0_DFP, 1.0_DFP]
  DO jj = 1, 4
    DO ii = 1, 3
      xi(ii, jj) = x(ii)
      eta(ii, jj) = y(jj)
    END DO
  END DO

  inner(:, 1) = [0.0_DFP, -1.0_DFP / 3.0_DFP]
  inner(:, 2) = [0.0_DFP, 1.0_DFP / 3.0_DFP]

  CALL IJ2VEFC_Quadrangle(xi=xi, eta=eta, temp=temp, p=2_I4B, q=3_I4B)
  CALL OK(MAXVAL(ABS(temp(:, 11:12) - inner)) .LT. tol, &
          "p=2, q=3: interior points come last, in increasing y")
END BLOCK

END PROGRAM main
