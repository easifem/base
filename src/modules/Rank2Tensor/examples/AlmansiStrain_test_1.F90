!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Euler-Almansi strain from F or from b
!
! The Almansi strain is the push forward of the Green strain,
! e = F^-T E F^-1, which is checked here in the form E = F^T e F.

PROGRAM main
USE BaseType, ONLY: AlmansiStrain_
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: GreenStrain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: AlmansiStrain
USE Rank2Tensor_Method, ONLY: AlmansiStrain_Pointer
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: GreenStrain
USE Rank2Tensor_Method, ONLY: LeftCauchyGreen
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
                        [1.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, 0.9_DFP, 0.2_DFP, &
                         0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])

! Test 1: uniaxial stretch, e11 = (1 - 1 / 2^2) / 2
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(AlmansiStrain_) :: e
  REAL(DFP) :: mat(3, 3), expected(3, 3)

  mat = Eye3
  mat(1, 1) = 2.0_DFP
  F = DeformationGradient(mat)
  e = AlmansiStrain(F)
  expected = 0.0_DFP
  expected(1, 1) = 0.375_DFP
  CALL OK(MAXVAL(ABS(e%T - expected)) .LT. tol, "uniaxial: e11")
  CALL OK(e%isSym, "uniaxial: symmetric")
END BLOCK

! Test 2: e = F^-T E F^-1, that is E = F^T e F
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(AlmansiStrain_) :: e
  TYPE(GreenStrain_) :: E0

  F = DeformationGradient(F0)
  e = AlmansiStrain(F)
  E0 = GreenStrain(F)
  CALL OK(MAXVAL(ABS(MATMUL(TRANSPOSE(F0), MATMUL(e%T, F0)) - E0%T)) &
          .LT. tol, "general: E = F^T e F")
END BLOCK

! Test 3: e(b) = e(F)
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(AlmansiStrain_) :: e1, e2

  F = DeformationGradient(F0)
  e1 = AlmansiStrain(F)
  e2 = AlmansiStrain(LeftCauchyGreen(F=F))
  CALL OK(MAXVAL(ABS(e2%T - e1%T)) .LT. tol, "general: e(b) = e(F)")
  CALL OK(e2%isSym, "general: e(b) symmetric")
END BLOCK

! Test 4: pointers from F and from b
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(AlmansiStrain_) :: e
  CLASS(AlmansiStrain_), POINTER :: ptr

  F = DeformationGradient(F0)
  e = AlmansiStrain(F)
  ptr => AlmansiStrain_Pointer(F)
  CALL OK(MAXVAL(ABS(ptr%T - e%T)) .LT. tol, "pointer: from F")
  CALL OK(ptr%isSym, "pointer: from F symmetric")
  DEALLOCATE (ptr)
  ptr => AlmansiStrain_Pointer(LeftCauchyGreen(F=F))
  CALL OK(MAXVAL(ABS(ptr%T - e%T)) .LT. tol, "pointer: from b")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
