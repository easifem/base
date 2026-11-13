!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap the indices of a rank-3 array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: b(2, 3, 4)
REAL(DFP), ALLOCATABLE :: a(:, :, :)
INTEGER(I4B) :: i, j, k
LOGICAL(LGT) :: isok
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! b(i, j, k) = 100*i + 10*j + k
DO k = 1, 4
  DO j = 1, 3
    DO i = 1, 2
      b(i, j, k) = REAL(100 * i + 10 * j + k, DFP)
    END DO
  END DO
END DO

! (1, 2, 3): a is a copy of b
CALL Swap(a, b, 1, 2, 3)
CALL OK(ALL(SHAPE(a) .EQ. [2, 3, 4]), "(1,2,3): shape")
CALL OK(ALL(ABS(a - b) .LT. tol), "(1,2,3): a = b")

! (1, 3, 2): a(i, k, j) = b(i, j, k)
CALL Swap(a, b, 1, 3, 2)
CALL OK(ALL(SHAPE(a) .EQ. [2, 4, 3]), "(1,3,2): shape")
isok = .TRUE.
DO k = 1, 4
  DO j = 1, 3
    DO i = 1, 2
      isok = isok .AND. ABS(a(i, k, j) - b(i, j, k)) .LT. tol
    END DO
  END DO
END DO
CALL OK(isok, "(1,3,2): a(i,k,j) = b(i,j,k)")
CALL OK(ABS(a(2, 4, 1) - 214.0_DFP) .LT. tol, "(1,3,2): a(2,4,1)")

! (2, 3, 1): a(j, k, i) = b(i, j, k)
CALL Swap(a, b, 2, 3, 1)
CALL OK(ALL(SHAPE(a) .EQ. [3, 4, 2]), "(2,3,1): shape")
CALL OK(ABS(a(3, 4, 2) - 234.0_DFP) .LT. tol, "(2,3,1): a(3,4,2)")
CALL OK(ABS(a(1, 2, 2) - 212.0_DFP) .LT. tol, "(2,3,1): a(1,2,2)")

! (3, 1, 2): a(k, i, j) = b(i, j, k)
CALL Swap(a, b, 3, 1, 2)
CALL OK(ALL(SHAPE(a) .EQ. [4, 2, 3]), "(3,1,2): shape")
CALL OK(ABS(a(4, 1, 2) - 124.0_DFP) .LT. tol, "(3,1,2): a(4,1,2)")

! an index out of range: a is a copy of b
CALL Swap(a, b, 4, 1, 2)
CALL OK(ALL(ABS(a - b) .LT. tol), "out of range: a = b")

END PROGRAM main
