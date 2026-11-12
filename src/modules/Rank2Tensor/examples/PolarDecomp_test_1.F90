!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Polar decomposition and rotation part of a tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: Det
USE Rank2Tensor_Method, ONLY: PolarDecomp
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: RotationPart
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([1.0_DFP, -0.333_DFP, &
                                            0.959_DFP, 0.495_DFP, &
                                            1.0_DFP, 0.0_DFP, 0.5_DFP, &
                                            -0.247_DFP, 1.5_DFP], [3, 3])

! Test 1: A = R U = V R with R proper orthogonal, U and V symmetric
BLOCK
  TYPE(Rank2Tensor_) :: obj, R, U, V

  obj = Rank2Tensor(A0)
  CALL PolarDecomp(obj, R, U, V)
  CALL OK(MAXVAL(ABS(MATMUL(TRANSPOSE(R%T), R%T) - Eye3)) .LT. tol, &
          "R^T R = I")
  CALL OK(ABS(Det(R) - 1.0_DFP) .LT. tol, "det R = 1")
  CALL OK(MAXVAL(ABS(MATMUL(R%T, U%T) - A0)) .LT. tol, "R U = A")
  CALL OK(MAXVAL(ABS(MATMUL(V%T, R%T) - A0)) .LT. tol, "V R = A")
  CALL OK(MAXVAL(ABS(U%T - TRANSPOSE(U%T))) .LT. tol, "U symmetric")
  CALL OK(MAXVAL(ABS(V%T - TRANSPOSE(V%T))) .LT. tol, "V symmetric")
END BLOCK

! Test 2: rotation by 90 deg about z times U = diag(2, 1, 3)
BLOCK
  TYPE(Rank2Tensor_) :: obj, R, U, V
  REAL(DFP) :: mat(3, 3), R0(3, 3), U0(3, 3), V0(3, 3)

  R0 = RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, -1.0_DFP, 0.0_DFP, 0.0_DFP, &
                0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3])
  U0 = 0.0_DFP
  U0(1, 1) = 2.0_DFP
  U0(2, 2) = 1.0_DFP
  U0(3, 3) = 3.0_DFP
  V0 = 0.0_DFP
  V0(1, 1) = 1.0_DFP
  V0(2, 2) = 2.0_DFP
  V0(3, 3) = 3.0_DFP
  mat = MATMUL(R0, U0)
  obj = Rank2Tensor(mat)
  CALL PolarDecomp(obj, R, U, V)
  CALL OK(MAXVAL(ABS(R%T - R0)) .LT. tol, "rotation: R")
  CALL OK(MAXVAL(ABS(U%T - U0)) .LT. tol, "rotation: U = diag(2,1,3)")
  CALL OK(MAXVAL(ABS(V%T - V0)) .LT. tol, "rotation: V = diag(1,2,3)")
END BLOCK

! Test 3: RotationPart is the R of PolarDecomp
BLOCK
  TYPE(Rank2Tensor_) :: obj, R, U, V, R2

  obj = Rank2Tensor(A0)
  CALL PolarDecomp(obj, R, U, V)
  R2 = RotationPart(obj)
  CALL OK(MAXVAL(ABS(R2%T - R%T)) .LT. tol, "RotationPart = R")
END BLOCK

END PROGRAM main
