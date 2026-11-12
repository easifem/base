!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: VoigtRank2Tensor_Pointer from a Voigt vector or a matrix

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor_Pointer
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: pointer from a stress-like Voigt vector
BLOCK
  CLASS(VoigtRank2Tensor_), POINTER :: ptr
  REAL(DFP) :: vec(6)

  vec = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  ptr => VoigtRank2Tensor_Pointer(vec, StressTypeVoigt)
  CALL OK(ASSOCIATED(ptr), "vector: associated")
  CALL OK(MAXVAL(ABS(ptr%V - vec)) .LT. tol, "vector: V")
  DEALLOCATE (ptr)
END BLOCK

! Test 2: pointer from a strain-like matrix
BLOCK
  CLASS(VoigtRank2Tensor_), POINTER :: ptr
  REAL(DFP) :: T(3, 3)

  T = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
               4.0_DFP, 2.0_DFP, 5.0_DFP, &
               6.0_DFP, 5.0_DFP, 3.0_DFP], [3, 3])
  ptr => VoigtRank2Tensor_Pointer(T, StrainTypeVoigt)
  CALL OK(MAXVAL(ABS(ptr%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
                              10.0_DFP, 12.0_DFP])) .LT. tol, &
          "matrix: engineering shear")
  CALL OK(ABS(ptr%scale - 0.5_DFP) .LT. tol, "matrix: scale is 0.5")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
