!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Eigenvalues and eigenvectors of symmetric and non-symmetric
!   tensors

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Rank2Tensor_Method, ONLY: Eigen
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

! Test 1: symmetric tensor, eigenvalues 5, 10, -15
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3), QR(3, 3), WR(3), QI(3, 3), WI(3)
  INTEGER(I4B) :: k

  mat = RESHAPE([5.0_DFP, 0.0_DFP, 0.0_DFP, &
                 0.0_DFP, -6.0_DFP, -12.0_DFP, &
                 0.0_DFP, -12.0_DFP, 1.0_DFP], [3, 3])
  obj = Rank2Tensor(mat, .TRUE.)
  CALL OK(obj%isSym, "symmetric: isSym is true")
  CALL Eigen(obj, QR, WR, QI, WI)
  DO k = 1, 3
    CALL OK(MAXVAL(ABS(MATMUL(mat, QR(:, k)) - WR(k) * QR(:, k))) &
            .LT. tol, "symmetric: A q = w q")
  END DO
  CALL OK(ABS(SUM(WR)) .LT. tol, "symmetric: sum of w = 0")
  CALL OK(ABS(PRODUCT(WR) + 750.0_DFP) .LT. tol, &
          "symmetric: product of w = -750")
  CALL OK(MAXVAL(ABS(WI)) .LT. tol, "symmetric: WI = 0")
  CALL OK(MAXVAL(ABS(QI)) .LT. tol, "symmetric: QI = 0")
END BLOCK

! Test 2: non-symmetric tensor with a complex pair, w = 1, 2i, -2i
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3), QR(3, 3), WR(3), QI(3, 3), WI(3)
  COMPLEX(DFP) :: q(3), r(3)
  INTEGER(I4B) :: k

  mat = 0.0_DFP
  mat(1, 2) = -2.0_DFP
  mat(2, 1) = 2.0_DFP
  mat(3, 3) = 1.0_DFP
  obj = Rank2Tensor(mat)
  CALL Eigen(obj, QR, WR, QI, WI)
  DO k = 1, 3
    q = CMPLX(QR(:, k), QI(:, k), DFP)
    r = MATMUL(mat, q) - CMPLX(WR(k), WI(k), DFP) * q
    CALL OK(MAXVAL(ABS(r)) .LT. tol, "non-symmetric: A q = w q")
  END DO
  CALL OK(ABS(SUM(WR) - 1.0_DFP) .LT. tol, "non-symmetric: sum WR = 1")
  CALL OK(ABS(MAXVAL(WI) - 2.0_DFP) .LT. tol .AND. &
          ABS(MINVAL(WI) + 2.0_DFP) .LT. tol, "non-symmetric: WI = +-2")
END BLOCK

END PROGRAM main
