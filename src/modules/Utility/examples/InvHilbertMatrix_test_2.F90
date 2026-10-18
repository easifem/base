!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: HilbertMatrix times InvHilbertMatrix is the identity

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LinearAlgebraUtility, ONLY: HilbertMatrix
USE LinearAlgebraUtility, ONLY: InvHilbertMatrix
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: h(:, :), invh(:, :), eye(:, :)
INTEGER(I4B) :: n, i
CHARACTER(1) :: sn

DO n = 2, 5
  h = HilbertMatrix(n)
  invh = InvHilbertMatrix(n)

  ALLOCATE (eye(n, n))
  eye = 0.0_DFP
  DO i = 1, n
    eye(i, i) = 1.0_DFP
  END DO

  WRITE (sn, "(I1)") n
  CALL OK(ALL(ABS(MATMUL(h, invh) - eye) .LE. tol), &
          "H * inv(H) = I, n = "//sn)
  CALL OK(ALL(ABS(MATMUL(invh, h) - eye) .LE. tol), &
          "inv(H) * H = I, n = "//sn)

  DEALLOCATE (eye)
END DO

END PROGRAM main
