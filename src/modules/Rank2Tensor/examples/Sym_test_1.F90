!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Symmetric and skew-symmetric parts of a Rank2Tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: SkewSym
USE Rank2Tensor_Method, ONLY: Sym
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
                                            -1.0_DFP, 3.0_DFP, 0.25_DFP, &
                                            0.0_DFP, 1.5_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: symmetric part
BLOCK
  TYPE(Rank2Tensor_) :: obj, ans

  obj = Rank2Tensor(A0)
  ans = Sym(obj)
  CALL OK(ABS(ans%T(1, 2)) .LT. tol, "Sym: T12")
  CALL OK(ABS(ans%T(2, 3) - 0.875_DFP) .LT. tol, "Sym: T23")
  CALL OK(MAXVAL(ABS(ans%T - TRANSPOSE(ans%T))) .LT. tol, &
          "Sym: symmetric")
  CALL OK(ans%isSym, "Sym: isSym")
END BLOCK

! Test 2: skew-symmetric part
BLOCK
  TYPE(Rank2Tensor_) :: obj, ans

  obj = Rank2Tensor(A0)
  ans = SkewSym(obj)
  CALL OK(ABS(ans%T(2, 1) - 1.0_DFP) .LT. tol, "SkewSym: T21")
  CALL OK(MAXVAL(ABS(ans%T + TRANSPOSE(ans%T))) .LT. tol, &
          "SkewSym: skew-symmetric")
  CALL OK(.NOT. ans%isSym, "SkewSym: isSym")
END BLOCK

! Test 3: the two parts add up to the tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj, symPart, skewPart

  obj = Rank2Tensor(A0)
  symPart = Sym(obj)
  skewPart = SkewSym(obj)
  CALL OK(MAXVAL(ABS(symPart%T + skewPart%T - A0)) .LT. tol, &
          "Sym + SkewSym = A")
END BLOCK

END PROGRAM main
