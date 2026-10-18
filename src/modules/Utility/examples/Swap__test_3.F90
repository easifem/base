!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap_ the indices of a rank-4 array without allocation
!
! For rank 4 the argument `a` of Swap_ is ALLOCATABLE, so the actual
! argument must be ALLOCATABLE too, and it must be allocated before
! the call.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE SwapUtility, ONLY: Swap
USE SwapUtility, ONLY: Swap_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: b(2, 3, 4, 5), a(5, 4, 3, 2)
REAL(DFP), ALLOCATABLE :: c(:, :, :, :)
INTEGER(I4B) :: i, j, k, l
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! b(i, j, k, l) = 1000*i + 100*j + 10*k + l
DO l = 1, 5
  DO k = 1, 4
    DO j = 1, 3
      DO i = 1, 2
        b(i, j, k, l) = REAL(1000 * i + 100 * j + 10 * k + l, DFP)
      END DO
    END DO
  END DO
END DO

! (4, 3, 2, 1): a(l, k, j, i) = b(i, j, k, l)
! a does not have to be allocatable
CALL Swap_(a, b, 4, 3, 2, 1)
CALL OK(ALL(SHAPE(a) .EQ. [5, 4, 3, 2]), "shape is not changed")
CALL OK(ABS(a(5, 4, 3, 2) - 2345.0_DFP) .LT. tol, "a(5,4,3,2)")
CALL OK(ABS(a(1, 2, 3, 1) - 1321.0_DFP) .LT. tol, "a(1,2,3,1)")

! Swap_ gives the same result as Swap
CALL Swap(c, b, 4, 3, 2, 1)
CALL OK(ALL(ABS(a - c) .LT. tol), "same as Swap")

END PROGRAM main
