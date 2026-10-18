!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap the indices of a rank-4 array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: b(2, 3, 4, 5)
REAL(DFP), ALLOCATABLE :: a(:, :, :, :)
INTEGER(I4B) :: i, j, k, l
LOGICAL(LGT) :: isok
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

! (1, 2, 3, 4): a is a copy of b
CALL Swap(a, b, 1, 2, 3, 4)
CALL OK(ALL(SHAPE(a) .EQ. [2, 3, 4, 5]), "(1,2,3,4): shape")
CALL OK(ALL(ABS(a - b) .LT. tol), "(1,2,3,4): a = b")

! (1, 3, 2, 4): a(i, k, j, l) = b(i, j, k, l)
CALL Swap(a, b, 1, 3, 2, 4)
CALL OK(ALL(SHAPE(a) .EQ. [2, 4, 3, 5]), "(1,3,2,4): shape")
isok = .TRUE.
DO l = 1, 5
  DO k = 1, 4
    DO j = 1, 3
      DO i = 1, 2
        isok = isok .AND. ABS(a(i, k, j, l) - b(i, j, k, l)) .LT. tol
      END DO
    END DO
  END DO
END DO
CALL OK(isok, "(1,3,2,4): a(i,k,j,l) = b(i,j,k,l)")

! (4, 3, 2, 1): a(l, k, j, i) = b(i, j, k, l)
CALL Swap(a, b, 4, 3, 2, 1)
CALL OK(ALL(SHAPE(a) .EQ. [5, 4, 3, 2]), "(4,3,2,1): shape")
CALL OK(ABS(a(5, 4, 3, 2) - 2345.0_DFP) .LT. tol, "(4,3,2,1): a(5,4,3,2)")
CALL OK(ABS(a(1, 2, 3, 1) - 1321.0_DFP) .LT. tol, "(4,3,2,1): a(1,2,3,1)")

END PROGRAM main
