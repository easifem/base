!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Inverse of a Rank2Tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: Inv
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
                                            -1.0_DFP, 3.0_DFP, 0.25_DFP, &
                                            0.0_DFP, 1.5_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: inverse of a non-symmetric tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj, invObj

  obj = Rank2Tensor(A0)
  CALL Inv(obj, invObj)
  CALL OK(MAXVAL(ABS(MATMUL(invObj%T, obj%T) - Eye3)) .LT. tol, &
          "Inv: inv(A) A = I")
  CALL OK(MAXVAL(ABS(MATMUL(obj%T, invObj%T) - Eye3)) .LT. tol, &
          "Inv: A inv(A) = I")
  CALL OK(.NOT. invObj%isSym, "Inv: not symmetric")
END BLOCK

! Test 2: inverse of a diagonal symmetric tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj, invObj
  REAL(DFP) :: expected(3, 3)

  expected = 0.0_DFP
  expected(1, 1) = 0.5_DFP
  expected(2, 2) = 0.25_DFP
  expected(3, 3) = 0.125_DFP
  obj = Rank2Tensor(RESHAPE([2.0_DFP, 0.0_DFP, 0.0_DFP, &
                             0.0_DFP, 4.0_DFP, 0.0_DFP, &
                             0.0_DFP, 0.0_DFP, 8.0_DFP], [3, 3]), .TRUE.)
  CALL Inv(obj, invObj)
  CALL OK(MAXVAL(ABS(invObj%T - expected)) .LT. tol, "Inv: diagonal")
  CALL OK(invObj%isSym, "Inv: symmetric")
END BLOCK

END PROGRAM main
