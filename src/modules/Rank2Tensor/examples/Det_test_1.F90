!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Determinant of a tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: Det
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
                                            -1.0_DFP, 3.0_DFP, 1.0_DFP, &
                                            0.0_DFP, 1.0_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: determinant of a non-symmetric tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(A0)
  CALL OK(ABS(Det(obj) - 26.0_DFP) .LT. tol, "det(A) = 26")
END BLOCK

! Test 2: determinant of the identity and of a scaled identity
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(Eye3, .TRUE.)
  CALL OK(ABS(Det(obj) - 1.0_DFP) .LT. tol, "det(I) = 1")
  obj = Rank2Tensor(2.0_DFP * Eye3, .TRUE.)
  CALL OK(ABS(Det(obj) - 8.0_DFP) .LT. tol, "det(2I) = 8")
END BLOCK

! Test 3: det(A^T) = det(A) and a singular tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3)

  obj = Rank2Tensor(TRANSPOSE(A0))
  CALL OK(ABS(Det(obj) - 26.0_DFP) .LT. tol, "det(A^T) = 26")
  mat = A0
  mat(:, 3) = mat(:, 1) + mat(:, 2)
  obj = Rank2Tensor(mat)
  CALL OK(ABS(Det(obj)) .LT. tol, "det of singular tensor = 0")
END BLOCK

END PROGRAM main
