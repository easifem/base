!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Transpose of a Rank2Tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: TRANSPOSE
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
                                            -1.0_DFP, 3.0_DFP, 0.25_DFP, &
                                            0.0_DFP, 1.5_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: transpose of a non-symmetric tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj, ans

  obj = Rank2Tensor(A0)
  ans = TRANSPOSE(obj)
  CALL OK(ABS(ans%T(1, 2) - 1.0_DFP) .LT. tol, "TRANSPOSE: T12")
  CALL OK(ABS(ans%T(2, 1) + 1.0_DFP) .LT. tol, "TRANSPOSE: T21")
  CALL OK(MAXVAL(ABS(ans%T - TRANSPOSE(A0))) .LT. tol, "TRANSPOSE: T")
  CALL OK(.NOT. ans%isSym, "TRANSPOSE: isSym is kept")
END BLOCK

! Test 2: transpose twice gives the tensor back
BLOCK
  TYPE(Rank2Tensor_) :: obj, ans

  obj = Rank2Tensor(A0)
  ans = TRANSPOSE(TRANSPOSE(obj))
  CALL OK(MAXVAL(ABS(ans%T - A0)) .LT. tol, "TRANSPOSE: twice")
END BLOCK

END PROGRAM main
