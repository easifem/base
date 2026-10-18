!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap_ the indices of a rank-3 array without allocation

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE SwapUtility, ONLY: Swap
USE SwapUtility, ONLY: Swap_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: b(2, 3, 4), a(3, 4, 2)
REAL(DFP), ALLOCATABLE :: c(:, :, :)
INTEGER(I4B) :: i, j, k
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! b(i, j, k) = 100*i + 10*j + k
DO k = 1, 4
  DO j = 1, 3
    DO i = 1, 2
      b(i, j, k) = REAL(100 * i + 10 * j + k, DFP)
    END DO
  END DO
END DO

! (2, 3, 1): a(j, k, i) = b(i, j, k), a has the right shape
CALL Swap_(a, b, 2, 3, 1)
CALL OK(ABS(a(3, 4, 2) - 234.0_DFP) .LT. tol, "a(3,4,2)")
CALL OK(ABS(a(1, 2, 2) - 212.0_DFP) .LT. tol, "a(1,2,2)")

! Swap_ gives the same result as Swap
CALL Swap(c, b, 2, 3, 1)
CALL OK(ALL(ABS(a - c) .LT. tol), "same as Swap")

END PROGRAM main
