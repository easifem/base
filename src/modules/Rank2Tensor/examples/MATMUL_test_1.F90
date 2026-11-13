!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Single contraction of Rank2Tensors and vectors

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: MATMUL
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
                                            -1.0_DFP, 3.0_DFP, 0.25_DFP, &
                                            0.0_DFP, 1.5_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: tensor times tensor
BLOCK
  TYPE(Rank2Tensor_) :: a, b, c

  a = Rank2Tensor(A0)
  b = Rank2Tensor(TRANSPOSE(A0))
  c = MATMUL(a, b)
  CALL OK(ABS(c%T(1, 1) - 5.0_DFP) .LT. tol, "obj obj: T11")
  CALL OK(MAXVAL(ABS(c%T - MATMUL(A0, TRANSPOSE(A0)))) .LT. tol, &
          "obj obj: T")
  CALL OK(.NOT. c%isSym, "obj obj: isSym")
END BLOCK

! Test 2: tensor times vector
BLOCK
  TYPE(Rank2Tensor_) :: a
  REAL(DFP) :: v(3)

  a = Rank2Tensor(A0)
  v = MATMUL(a, [1.0_DFP, 1.0_DFP, 1.0_DFP])
  CALL OK(MAXVAL(ABS(v - [1.0_DFP, 5.5_DFP, 4.75_DFP])) .LT. tol, &
          "obj vec")
END BLOCK

! Test 3: vector times tensor
BLOCK
  TYPE(Rank2Tensor_) :: a
  REAL(DFP) :: v(3)

  a = Rank2Tensor(A0)
  v = MATMUL([1.0_DFP, 1.0_DFP, 1.0_DFP], a)
  CALL OK(MAXVAL(ABS(v - [3.5_DFP, 2.25_DFP, 5.5_DFP])) .LT. tol, &
          "vec obj")
END BLOCK

END PROGRAM main
