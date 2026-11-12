!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Exponential of a tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: Exp
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([0.1_DFP, 0.2_DFP, -0.3_DFP, &
                                            0.4_DFP, -0.5_DFP, 0.6_DFP, &
                                            0.7_DFP, 0.0_DFP, 0.2_DFP], &
                                           [3, 3])

! Test 1: exp(0) = I
BLOCK
  TYPE(Rank2Tensor_) :: E

  E = Exp(Rank2Tensor(0.0_DFP * Eye3, .TRUE.))
  CALL OK(MAXVAL(ABS(E%T - Eye3)) .LT. tol, "exp(0) = I")
END BLOCK

! Test 2: exponential of a diagonal tensor
BLOCK
  TYPE(Rank2Tensor_) :: E
  REAL(DFP) :: mat(3, 3), expected(3, 3)

  mat = 0.0_DFP
  mat(1, 1) = 1.0_DFP
  mat(2, 2) = -2.0_DFP
  mat(3, 3) = 5.0_DFP
  expected = 0.0_DFP
  expected(1, 1) = EXP(1.0_DFP)
  expected(2, 2) = EXP(-2.0_DFP)
  expected(3, 3) = EXP(5.0_DFP)
  E = Exp(Rank2Tensor(mat, .TRUE.))
  CALL OK(MAXVAL(ABS(E%T - expected)) .LT. tol * EXP(5.0_DFP), &
          "exp of diagonal")
  CALL OK(E%isSym, "exp of symmetric is symmetric")
END BLOCK

! Test 3: exp(4A) exp(6A) = exp(10A)
BLOCK
  TYPE(Rank2Tensor_) :: E, E1, E2

  E1 = Exp(Rank2Tensor(4.0_DFP * A0))
  E2 = Exp(Rank2Tensor(6.0_DFP * A0))
  E = Exp(Rank2Tensor(10.0_DFP * A0))
  CALL OK(MAXVAL(ABS(MATMUL(E1%T, E2%T) - E%T)) &
          .LT. tol * MAXVAL(ABS(E%T)), "exp(4A) exp(6A) = exp(10A)")
END BLOCK

! Test 4: exp(A) exp(-A) = I
BLOCK
  TYPE(Rank2Tensor_) :: E1, E2

  E1 = Exp(Rank2Tensor(A0))
  E2 = Exp(Rank2Tensor(-A0))
  CALL OK(MAXVAL(ABS(MATMUL(E1%T, E2%T) - Eye3)) .LT. tol, &
          "exp(A) exp(-A) = I")
END BLOCK

END PROGRAM main
