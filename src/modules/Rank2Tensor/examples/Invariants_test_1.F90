!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Principal invariants of a tensor and the Cayley-Hamilton
!   theorem

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: Invariants
USE Rank2Tensor_Method, ONLY: J2
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
                                            -1.0_DFP, 3.0_DFP, 1.0_DFP, &
                                            0.0_DFP, 1.0_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: invariants of a non-symmetric tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: inv(3)

  obj = Rank2Tensor(A0)
  inv = Invariants(obj)
  CALL OK(MAXVAL(ABS(inv - [9.0_DFP, 26.0_DFP, 26.0_DFP])) .LT. tol, &
          "I1, I2, I3 = 9, 26, 26")
END BLOCK

! Test 2: Cayley-Hamilton, A^3 - I1 A^2 + I2 A - I3 I = 0
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: inv(3), A2(3, 3), CH(3, 3)

  obj = Rank2Tensor(A0)
  inv = Invariants(obj)
  A2 = MATMUL(A0, A0)
  CH = MATMUL(A2, A0) - inv(1) * A2 + inv(2) * A0 - inv(3) * Eye3
  CALL OK(MAXVAL(ABS(CH)) .LT. tol, "Cayley-Hamilton")
END BLOCK

! Test 3: deviatoric branch gives the same invariants, I2 = -J2
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: inv(3)

  obj = Dev(Rank2Tensor(A0))
  inv = Invariants(obj, isDeviatoric=.TRUE.)
  CALL OK(ABS(inv(1)) .LT. tol, "deviatoric: I1 = 0")
  CALL OK(MAXVAL(ABS(inv - Invariants(obj))) .LT. tol, &
          "deviatoric: same as general branch")
  CALL OK(ABS(inv(2) + J2(obj, isDeviatoric=.TRUE.)) .LT. tol, &
          "deviatoric: I2 = -J2")
END BLOCK

END PROGRAM main
