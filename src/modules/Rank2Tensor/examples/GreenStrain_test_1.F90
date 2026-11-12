!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Green-Lagrange strain from F or from C

PROGRAM main
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: GreenStrain_
USE BaseType, ONLY: RightCauchyGreen_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: GreenStrain
USE Rank2Tensor_Method, ONLY: GreenStrain_Pointer
USE Rank2Tensor_Method, ONLY: RightCauchyGreen
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
                        [1.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, 0.9_DFP, 0.2_DFP, &
                         0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])

! Test 1: uniaxial stretch, E11 = (2^2 - 1) / 2
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(GreenStrain_) :: E
  REAL(DFP) :: mat(3, 3), expected(3, 3)

  mat = Eye3
  mat(1, 1) = 2.0_DFP
  F = DeformationGradient(mat)
  E = GreenStrain(F)
  expected = 0.0_DFP
  expected(1, 1) = 1.5_DFP
  CALL OK(MAXVAL(ABS(E%T - expected)) .LT. tol, "uniaxial: E11 = 1.5")
  CALL OK(E%isSym, "uniaxial: symmetric")
END BLOCK

! Test 2: E = (F^T F - I) / 2 and E(C) = E(F)
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(RightCauchyGreen_) :: C
  TYPE(GreenStrain_) :: E
  REAL(DFP) :: expected(3, 3)

  expected = 0.5_DFP * (MATMUL(TRANSPOSE(F0), F0) - Eye3)
  F = DeformationGradient(F0)
  E = GreenStrain(F)
  CALL OK(MAXVAL(ABS(E%T - expected)) .LT. tol, "general: E from F")
  C = RightCauchyGreen(F=F)
  E = GreenStrain(C)
  CALL OK(MAXVAL(ABS(E%T - expected)) .LT. tol, "general: E from C")
  CALL OK(E%isSym, "general: E from C symmetric")
END BLOCK

! Test 3: pointers from F and from C
BLOCK
  TYPE(DeformationGradient_) :: F
  CLASS(GreenStrain_), POINTER :: ptr
  REAL(DFP) :: expected(3, 3)

  expected = 0.5_DFP * (MATMUL(TRANSPOSE(F0), F0) - Eye3)
  F = DeformationGradient(F0)
  ptr => GreenStrain_Pointer(F)
  CALL OK(MAXVAL(ABS(ptr%T - expected)) .LT. tol, "pointer: from F")
  CALL OK(ptr%isSym, "pointer: from F symmetric")
  DEALLOCATE (ptr)
  ptr => GreenStrain_Pointer(RightCauchyGreen(F=F))
  CALL OK(MAXVAL(ABS(ptr%T - expected)) .LT. tol, "pointer: from C")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
