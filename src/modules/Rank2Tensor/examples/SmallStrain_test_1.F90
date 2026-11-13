!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Infinitesimal strain from a displacement gradient

PROGRAM main
USE BaseType, ONLY: SmallStrain_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: SmallStrain
USE Rank2Tensor_Method, ONLY: SmallStrain_Pointer
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: gradU(3, 3) = RESHAPE( &
                        [0.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, -0.1_DFP, 0.2_DFP, &
                         0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
REAL(DFP), PARAMETER :: eps(3, 3) = RESHAPE( &
                        [0.2_DFP, 0.2_DFP, 0.2_DFP, &
                         0.2_DFP, -0.1_DFP, 0.05_DFP, &
                         0.2_DFP, 0.05_DFP, 0.1_DFP], [3, 3])

! Test 1: symmetric part of the displacement gradient
BLOCK
  TYPE(SmallStrain_) :: e

  e = SmallStrain(gradU)
  CALL OK(MAXVAL(ABS(e%T - eps)) .LT. tol, "SmallStrain: components")
  CALL OK(e%isSym, "SmallStrain: symmetric")
END BLOCK

! Test 2: a pure rotation gradient gives zero strain
BLOCK
  TYPE(SmallStrain_) :: e
  REAL(DFP) :: skew(3, 3)

  skew = gradU - TRANSPOSE(gradU)
  e = SmallStrain(skew)
  CALL OK(MAXVAL(ABS(e%T)) .LT. tol, "skew gradient: zero strain")
END BLOCK

! Test 3: pointer
BLOCK
  CLASS(SmallStrain_), POINTER :: ptr

  ptr => SmallStrain_Pointer(gradU)
  CALL OK(MAXVAL(ABS(ptr%T - eps)) .LT. tol, "pointer: components")
  CALL OK(ptr%isSym, "pointer: symmetric")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
