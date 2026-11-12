!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Left and right Cauchy-Green tensors from F or from the
!   stretch tensors

PROGRAM main
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: LeftCauchyGreen_
USE BaseType, ONLY: RightCauchyGreen_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: LeftCauchyGreen
USE Rank2Tensor_Method, ONLY: LeftCauchyGreen_Pointer
USE Rank2Tensor_Method, ONLY: LeftStretch
USE Rank2Tensor_Method, ONLY: RightCauchyGreen
USE Rank2Tensor_Method, ONLY: RightCauchyGreen_Pointer
USE Rank2Tensor_Method, ONLY: RightStretch
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
                        [1.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, 0.9_DFP, 0.2_DFP, &
                         0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])

! Test 1: b = F F^T and b = V^2
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(LeftCauchyGreen_) :: b

  F = DeformationGradient(F0)
  b = LeftCauchyGreen(F=F)
  CALL OK(MAXVAL(ABS(b%T - MATMUL(F0, TRANSPOSE(F0)))) .LT. tol, &
          "b from F")
  CALL OK(b%isSym, "b from F: symmetric")
  b = LeftCauchyGreen(V=LeftStretch(F))
  CALL OK(MAXVAL(ABS(b%T - MATMUL(F0, TRANSPOSE(F0)))) .LT. tol, &
          "b from V")
END BLOCK

! Test 2: C = F^T F and C = U^2
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(RightCauchyGreen_) :: C

  F = DeformationGradient(F0)
  C = RightCauchyGreen(F=F)
  CALL OK(MAXVAL(ABS(C%T - MATMUL(TRANSPOSE(F0), F0))) .LT. tol, &
          "C from F")
  CALL OK(C%isSym, "C from F: symmetric")
  C = RightCauchyGreen(U=RightStretch(F))
  CALL OK(MAXVAL(ABS(C%T - MATMUL(TRANSPOSE(F0), F0))) .LT. tol, &
          "C from U")
END BLOCK

! Test 3: with no argument b and C are the identity
BLOCK
  TYPE(LeftCauchyGreen_) :: b
  TYPE(RightCauchyGreen_) :: C

  b = LeftCauchyGreen()
  C = RightCauchyGreen()
  CALL OK(MAXVAL(ABS(b%T - Eye3)) .LT. tol, "no argument: b = I")
  CALL OK(MAXVAL(ABS(C%T - Eye3)) .LT. tol, "no argument: C = I")
END BLOCK

! Test 4: pointers to b and C
BLOCK
  TYPE(DeformationGradient_) :: F
  CLASS(LeftCauchyGreen_), POINTER :: pb
  CLASS(RightCauchyGreen_), POINTER :: pc

  F = DeformationGradient(F0)
  pb => LeftCauchyGreen_Pointer(F=F)
  CALL OK(MAXVAL(ABS(pb%T - MATMUL(F0, TRANSPOSE(F0)))) .LT. tol, &
          "pointer: b from F")
  CALL OK(pb%isSym, "pointer: b symmetric")
  DEALLOCATE (pb)
  pc => RightCauchyGreen_Pointer(U=RightStretch(F))
  CALL OK(MAXVAL(ABS(pc%T - MATMUL(TRANSPOSE(F0), F0))) .LT. tol, &
          "pointer: C from U")
  CALL OK(pc%isSym, "pointer: C symmetric")
  DEALLOCATE (pc)
END BLOCK

END PROGRAM main
