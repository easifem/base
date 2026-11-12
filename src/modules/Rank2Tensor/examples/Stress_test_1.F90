!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Construct a Stress_ from a matrix, a Rank2Tensor_ or a
!   VoigtRank2Tensor_, as a value and as a pointer

PROGRAM main
USE BaseType, ONLY: Stress_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasureKirchhoff
USE GlobalData, ONLY: StressMeasureMandel
USE GlobalData, ONLY: StressMeasurePK1
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressTypeVoigt
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Stress
USE Rank2Tensor_Method, ONLY: Stress_Pointer
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
                        [10.0_DFP, 2.0_DFP, 3.0_DFP, &
                         2.0_DFP, 20.0_DFP, 4.0_DFP, &
                         3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])

! Test 1: from a symmetric matrix
BLOCK
  TYPE(Stress_) :: sig

  sig = Stress(s0, StressMeasureCauchy)
  CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "mat: components")
  CALL OK(sig%isSym, "mat: isSym is true")
  CALL OK(sig%stressType .EQ. StressMeasureCauchy, "mat: stressType")
END BLOCK

! Test 2: from a non-symmetric matrix
BLOCK
  TYPE(Stress_) :: sig
  REAL(DFP) :: p0(3, 3)

  p0 = s0
  p0(1, 2) = 7.0_DFP
  sig = Stress(p0, StressMeasurePK1)
  CALL OK(.NOT. sig%isSym, "non-symmetric mat: isSym is false")
  CALL OK(ABS(sig%T(1, 2) - 7.0_DFP) .LT. tol, &
          "non-symmetric mat: T(1,2) kept")
  CALL OK(sig%stressType .EQ. StressMeasurePK1, &
          "non-symmetric mat: stressType")
END BLOCK

! Test 3: from a Rank2Tensor_
BLOCK
  TYPE(Stress_) :: sig

  sig = Stress(Rank2Tensor(s0, .TRUE.), StressMeasurePK2)
  CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "Rank2Tensor: components")
  CALL OK(sig%isSym, "Rank2Tensor: isSym is true")
  CALL OK(sig%stressType .EQ. StressMeasurePK2, "Rank2Tensor: stressType")
END BLOCK

! Test 4: from a stress-like and a strain-like VoigtRank2Tensor_
BLOCK
  TYPE(Stress_) :: sig

  sig = Stress(VoigtRank2Tensor(s0, StressTypeVoigt), StressMeasureMandel)
  CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "Voigt stress: components")
  CALL OK(sig%isSym, "Voigt stress: isSym is true")
  CALL OK(sig%stressType .EQ. StressMeasureMandel, "Voigt: stressType")
  sig = Stress(VoigtRank2Tensor(s0, StrainTypeVoigt), StressMeasureCauchy)
  CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "Voigt strain: components")
END BLOCK

! Test 5: Stress_Pointer from a matrix
BLOCK
  CLASS(Stress_), POINTER :: ptr

  ptr => Stress_Pointer(s0, StressMeasureKirchhoff)
  CALL OK(ASSOCIATED(ptr), "pointer mat: associated")
  CALL OK(MAXVAL(ABS(ptr%T - s0)) .LT. tol, "pointer mat: components")
  CALL OK(ptr%isSym, "pointer mat: isSym is true")
  CALL OK(ptr%stressType .EQ. StressMeasureKirchhoff, &
          "pointer mat: stressType")
  DEALLOCATE (ptr)
END BLOCK

! Test 6: Stress_Pointer from a Rank2Tensor_
BLOCK
  CLASS(Stress_), POINTER :: ptr

  ptr => Stress_Pointer(Rank2Tensor(s0, .TRUE.), StressMeasurePK2)
  CALL OK(MAXVAL(ABS(ptr%T - s0)) .LT. tol, "pointer T: components")
  CALL OK(ptr%stressType .EQ. StressMeasurePK2, "pointer T: stressType")
  DEALLOCATE (ptr)
END BLOCK

! Test 7: Stress_Pointer from a VoigtRank2Tensor_
BLOCK
  CLASS(Stress_), POINTER :: ptr

  ptr => Stress_Pointer(VoigtRank2Tensor(s0, StressTypeVoigt), &
                        StressMeasurePK1)
  CALL OK(MAXVAL(ABS(ptr%T - s0)) .LT. tol, "pointer Voigt: components")
  CALL OK(ptr%isSym, "pointer Voigt: isSym is true")
  CALL OK(ptr%stressType .EQ. StressMeasurePK1, &
          "pointer Voigt: stressType")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
