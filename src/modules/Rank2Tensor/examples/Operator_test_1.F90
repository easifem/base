!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Element-wise +, -, * and / of Rank2Tensors and scalars

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: OPERATOR(*)
USE Rank2Tensor_Method, ONLY: OPERATOR(+)
USE Rank2Tensor_Method, ONLY: OPERATOR(-)
USE Rank2Tensor_Method, ONLY: OPERATOR(/)
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: s = 2.0_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
                                            -1.0_DFP, 3.0_DFP, 0.25_DFP, &
                                            0.5_DFP, 1.5_DFP, 4.0_DFP], &
                                           [3, 3])
REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
                                            2.0_DFP, 5.0_DFP, 0.5_DFP, &
                                            4.0_DFP, 0.5_DFP, 8.0_DFP], &
                                           [3, 3])

! Test 1: addition
BLOCK
  TYPE(Rank2Tensor_) :: a, b, c

  a = Rank2Tensor(A0)
  b = Rank2Tensor(B0, .TRUE.)
  c = a + b
  CALL OK(MAXVAL(ABS(c%T - (A0 + B0))) .LT. tol, "obj + obj")
  CALL OK(.NOT. c%isSym, "obj + obj: isSym")
  c = b + s
  CALL OK(MAXVAL(ABS(c%T - (B0 + s))) .LT. tol, "obj + scalar")
  CALL OK(c%isSym, "obj + scalar: isSym")
  c = s + a
  CALL OK(MAXVAL(ABS(c%T - (s + A0))) .LT. tol, "scalar + obj")
END BLOCK

! Test 2: subtraction
BLOCK
  TYPE(Rank2Tensor_) :: a, b, c

  a = Rank2Tensor(A0)
  b = Rank2Tensor(B0, .TRUE.)
  c = a - b
  CALL OK(MAXVAL(ABS(c%T - (A0 - B0))) .LT. tol, "obj - obj")
  c = a - s
  CALL OK(MAXVAL(ABS(c%T - (A0 - s))) .LT. tol, "obj - scalar")
  c = s - a
  CALL OK(MAXVAL(ABS(c%T - (s - A0))) .LT. tol, "scalar - obj")
END BLOCK

! Test 3: element-wise product, not the matrix product
BLOCK
  TYPE(Rank2Tensor_) :: a, b, c

  a = Rank2Tensor(A0)
  b = Rank2Tensor(B0, .TRUE.)
  c = a * b
  CALL OK(ABS(c%T(1, 2) + 2.0_DFP) .LT. tol, "obj * obj: T12")
  CALL OK(MAXVAL(ABS(c%T - A0 * B0)) .LT. tol, "obj * obj")
  c = a * s
  CALL OK(MAXVAL(ABS(c%T - A0 * s)) .LT. tol, "obj * scalar")
  c = s * b
  CALL OK(MAXVAL(ABS(c%T - s * B0)) .LT. tol, "scalar * obj")
  CALL OK(c%isSym, "scalar * obj: isSym")
END BLOCK

! Test 4: element-wise division
BLOCK
  TYPE(Rank2Tensor_) :: a, b, c

  a = Rank2Tensor(A0)
  b = Rank2Tensor(B0, .TRUE.)
  c = a / b
  CALL OK(ABS(c%T(1, 2) + 0.5_DFP) .LT. tol, "obj / obj: T12")
  CALL OK(MAXVAL(ABS(c%T - A0 / B0)) .LT. tol, "obj / obj")
  c = a / s
  CALL OK(MAXVAL(ABS(c%T - A0 / s)) .LT. tol, "obj / scalar")
  c = s / b
  CALL OK(MAXVAL(ABS(c%T - s / B0)) .LT. tol, "scalar / obj")
END BLOCK

END PROGRAM main
