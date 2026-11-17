!> author: Shion Shimizu
! date: 2026-11-17
! summary: LinSolve with SuperLU, reuse of the factors (isFactored)
!
! Description:
!   A small non-symmetric matrix is solved with a new right-hand side
!   and the stored factors (isFactored = .TRUE.), then with new values
!   (isFactored = .FALSE.), then again with the stored factors. The
!   residual |A x - b| is checked after each solve. The versions with
!   two right-hand sides and with the solution returned in B are
!   checked as well, and freeing the factors with isFreeFactors.

PROGRAM main
USE BaseType, ONLY: CSRMatrix_
USE CSRMatrix_Method, ONLY: DEALLOCATE
USE CSRMatrix_Method, ONLY: Initiate
USE CSRMatrix_Method, ONLY: LinSolve
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 6
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: Ad(n, n) = RESHAPE(REAL([ &
                        4, 1, 0, 0, 3, 0, &
                        2, 1, 1, 0, 0, 0, &
                        0, 5, 2, 1, 0, 0, &
                        0, 0, 1, 6, 2, 0, &
                        1, 0, 0, 1, 4, 1, &
                        0, 0, 0, 0, 2, 3], DFP), [n, n])
!! non-symmetric matrix, given column by column

! Test 1: one right-hand side (LinSolve1)
CALL OneRhs()

! Test 2: two right-hand sides (LinSolve2)
CALL TwoRhs()

! Test 3: solution returned in B (LinSolve3)
CALL InPlace()

! Test 4: the factors are freed after the solve (isFreeFactors)
CALL FreeFactors()

CONTAINS

! Test 1: one right-hand side (LinSolve1)
SUBROUTINE OneRhs()
  TYPE(CSRMatrix_) :: mat
  REAL(DFP) :: x(n), b(n), Ad2(n, n)
  INTEGER(I4B) :: info

  CALL InitiateCSR(mat, Ad)

  ! first solve, the matrix is factored
  b = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  x = 0.0_DFP
  CALL LinSolve(X=x, A=mat, B=b, isFactored=.FALSE._LGT, info=info)
  CALL OK(info .EQ. 0, "first solve: A x = b: info")
  CALL OK(Residual(Ad, x, b) .LT. tol, "first solve: A x = b")

  ! new right-hand side, the stored factors are used
  b = [-1.0_DFP, 0.5_DFP, 2.0_DFP, 0.0_DFP, 1.0_DFP, -3.0_DFP]
  x = 0.0_DFP
  CALL LinSolve(X=x, A=mat, B=b, isFactored=.TRUE._LGT, info=info)
  CALL OK(info .EQ. 0, "stored factors: A x = b: info")
  CALL OK(Residual(Ad, x, b) .LT. tol, "stored factors: A x = b")

  ! new values with the same pattern, the matrix is factored again
  Ad2 = Ad
  Ad2(1, 1) = 7.0_DFP
  Ad2(4, 5) = -1.0_DFP
  CALL SetValues(mat, Ad2)
  x = 0.0_DFP
  CALL LinSolve(X=x, A=mat, B=b, isFactored=.FALSE._LGT, info=info)
  CALL OK(info .EQ. 0, "new values: A x = b: info")
  CALL OK(Residual(Ad2, x, b) .LT. tol, "new values: A x = b")

  ! the new factors are used
  b = [2.0_DFP, 1.0_DFP, 0.0_DFP, -1.0_DFP, 3.0_DFP, 1.0_DFP]
  x = 0.0_DFP
  CALL LinSolve(X=x, A=mat, B=b, isFactored=.TRUE._LGT, info=info)
  CALL OK(info .EQ. 0, "new stored factors: A x = b: info")
  CALL OK(Residual(Ad2, x, b) .LT. tol, "new stored factors: A x = b")

  CALL DEALLOCATE(mat)
END SUBROUTINE OneRhs

! Test 2: two right-hand sides (LinSolve2)
SUBROUTINE TwoRhs()
  TYPE(CSRMatrix_) :: mat
  REAL(DFP) :: x(n, 2), b(n, 2)
  INTEGER(I4B) :: info

  CALL InitiateCSR(mat, Ad)

  b(:, 1) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  b(:, 2) = [6.0_DFP, 5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, 1.0_DFP]
  x = 0.0_DFP
  CALL LinSolve(X=x, A=mat, B=b, isFactored=.FALSE._LGT, info=info)
  CALL OK(info .EQ. 0, "two rhs, first solve: info")
  CALL OK(MAX(Residual(Ad, x(:, 1), b(:, 1)), &
              Residual(Ad, x(:, 2), b(:, 2))) .LT. tol, &
          "two rhs, first solve")

  b = -2.0_DFP * b + 1.0_DFP
  x = 0.0_DFP
  CALL LinSolve(X=x, A=mat, B=b, isFactored=.TRUE._LGT, info=info)
  CALL OK(info .EQ. 0, "two rhs, stored factors: info")
  CALL OK(MAX(Residual(Ad, x(:, 1), b(:, 1)), &
              Residual(Ad, x(:, 2), b(:, 2))) .LT. tol, &
          "two rhs, stored factors")

  CALL DEALLOCATE(mat)
END SUBROUTINE TwoRhs

! Test 3: solution returned in B (LinSolve3)
SUBROUTINE InPlace()
  TYPE(CSRMatrix_) :: mat
  REAL(DFP) :: b(n), b0(n)
  INTEGER(I4B) :: info

  CALL InitiateCSR(mat, Ad)

  b0 = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  b = b0
  CALL LinSolve(A=mat, B=b, isFactored=.FALSE._LGT, info=info)
  CALL OK(info .EQ. 0, "in place, first solve: info")
  CALL OK(Residual(Ad, b, b0) .LT. tol, "in place, first solve")

  b0 = [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP]
  b = b0
  CALL LinSolve(A=mat, B=b, isFactored=.TRUE._LGT, info=info)
  CALL OK(info .EQ. 0, "in place, stored factors: info")
  CALL OK(Residual(Ad, b, b0) .LT. tol, "in place, stored factors")

  CALL DEALLOCATE(mat)
END SUBROUTINE InPlace

! Test 4: the factors are freed after the solve (isFreeFactors)
SUBROUTINE FreeFactors()
  TYPE(CSRMatrix_) :: mat
  REAL(DFP) :: x(n), b(n)
  INTEGER(I4B) :: info

  CALL InitiateCSR(mat, Ad)

  b = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  x = 0.0_DFP
  CALL LinSolve(X=x, A=mat, B=b, isFactored=.FALSE._LGT, info=info, &
                isFreeFactors=.TRUE._LGT)
  CALL OK(info .EQ. 0, "free factors, first solve: info")
  CALL OK(Residual(Ad, x, b) .LT. tol, "free factors, first solve")
  CALL OK(.NOT. mat%slu%isLInitiated, "free factors: L is freed")
  CALL OK(.NOT. mat%slu%isUInitiated, "free factors: U is freed")

  ! the matrix is factored again
  b = [-1.0_DFP, 0.5_DFP, 2.0_DFP, 0.0_DFP, 1.0_DFP, -3.0_DFP]
  x = 0.0_DFP
  CALL LinSolve(X=x, A=mat, B=b, isFactored=.FALSE._LGT, info=info)
  CALL OK(info .EQ. 0, "free factors, second solve: info")
  CALL OK(Residual(Ad, x, b) .LT. tol, "free factors, second solve")
  CALL OK(mat%slu%isLInitiated, "free factors: L is kept by default")

  CALL DEALLOCATE(mat)
END SUBROUTINE FreeFactors

! CSR matrix with the nonzero entries of a dense matrix
SUBROUTINE InitiateCSR(mat, Ad)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  REAL(DFP), INTENT(IN) :: Ad(:, :)

  REAL(DFP) :: A(SIZE(Ad))
  INTEGER(I4B) :: IA(SIZE(Ad, 1) + 1), JA(SIZE(Ad)), ii, jj, nnz

  nnz = 0
  IA(1) = 1
  DO ii = 1, SIZE(Ad, 1)
    DO jj = 1, SIZE(Ad, 2)
      IF (Ad(ii, jj) .NE. 0.0_DFP) THEN
        nnz = nnz + 1
        A(nnz) = Ad(ii, jj)
        JA(nnz) = jj
      END IF
    END DO
    IA(ii + 1) = nnz + 1
  END DO
  CALL Initiate(obj=mat, A=A(1:nnz), IA=IA, JA=JA(1:nnz))
END SUBROUTINE InitiateCSR

! New values of the nonzero entries, the pattern is not changed
SUBROUTINE SetValues(mat, Ad)
  TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  REAL(DFP), INTENT(IN) :: Ad(:, :)

  INTEGER(I4B) :: ii, kk

  DO ii = 1, SIZE(Ad, 1)
    DO kk = mat%csr%IA(ii), mat%csr%IA(ii + 1) - 1
      mat%A(kk) = Ad(ii, mat%csr%JA(kk))
    END DO
  END DO
END SUBROUTINE SetValues

! Largest entry of |A x - b|
FUNCTION Residual(Ad, x, b) RESULT(ans)
  REAL(DFP), INTENT(IN) :: Ad(:, :)
  REAL(DFP), INTENT(IN) :: x(:)
  REAL(DFP), INTENT(IN) :: b(:)
  REAL(DFP) :: ans

  ans = MAXVAL(ABS(MATMUL(Ad, x) - b))
END FUNCTION Residual

END PROGRAM main
