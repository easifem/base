!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Dyadic product of two vectors and of two Rank2Tensors

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: Otimes
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: dyadic product of two vectors
BLOCK
  TYPE(Rank2Tensor_) :: ans
  REAL(DFP) :: a(3), b(3)

  a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
  b = [4.0_DFP, 5.0_DFP, 6.0_DFP]
  ans = Otimes(a, b)
  CALL OK(ABS(ans%T(1, 2) - 5.0_DFP) .LT. tol, "vec vec: T12")
  CALL OK(ABS(ans%T(3, 1) - 12.0_DFP) .LT. tol, "vec vec: T31")
  CALL OK(MAXVAL(ABS(ans%T - SPREAD(a, 2, 3) * SPREAD(b, 1, 3))) &
          .LT. tol, "vec vec: T")
  CALL OK(.NOT. ans%isSym, "vec vec: isSym")
END BLOCK

! Test 2: dyadic product of two tensors
BLOCK
  REAL(DFP) :: ans(3, 3, 3, 3), B0(3, 3)

  B0 = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
                6.0_DFP, 7.0_DFP, 8.0_DFP, 9.0_DFP], [3, 3])
  ans = Otimes(Rank2Tensor(Eye3), Rank2Tensor(B0))
  CALL OK(ABS(ans(1, 1, 2, 3) - 8.0_DFP) .LT. tol, "obj obj: 1123")
  CALL OK(ABS(ans(1, 2, 2, 3)) .LT. tol, "obj obj: 1223")
  CALL OK(ABS(ans(3, 3, 3, 1) - 3.0_DFP) .LT. tol, "obj obj: 3331")
  CALL OK(ABS(SUM(ans) - 3.0_DFP * SUM(B0)) .LT. tol, "obj obj: sum")
END BLOCK

END PROGRAM main
