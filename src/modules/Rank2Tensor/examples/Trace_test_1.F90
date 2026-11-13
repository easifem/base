!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Trace of a tensor and of its powers

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Trace
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
                                            -1.0_DFP, 3.0_DFP, 1.0_DFP, &
                                            0.0_DFP, 1.0_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: trace without power
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(A0)
  CALL OK(ABS(Trace(obj) - 9.0_DFP) .LT. tol, "tr(A) = 9")
END BLOCK

! Test 2: trace of the square and of the cube
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(A0)
  CALL OK(ABS(Trace(obj, power=2) - 29.0_DFP) .LT. tol, "tr(A^2) = 29")
  CALL OK(ABS(Trace(obj, power=3) - 105.0_DFP) .LT. tol, &
          "tr(A^3) = 105")
END BLOCK

! Test 3: power less than 1 gives tr(I) = 3
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(A0)
  CALL OK(ABS(Trace(obj, power=0) - 3.0_DFP) .LT. tol, "tr(A^0) = 3")
END BLOCK

END PROGRAM main
