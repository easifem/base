!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: VoigtRank2Tensor_Pointer with a strain-like vector and an
!   unknown Voigt type

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor_Pointer
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
                                 10.0_DFP, 12.0_DFP]

! Test 1: strain-like vector, the vector is stored as it is
BLOCK
  CLASS(VoigtRank2Tensor_), POINTER :: ptr
  REAL(DFP) :: T(3, 3)

  ptr => VoigtRank2Tensor_Pointer(v6, StrainTypeVoigt)
  CALL OK(MAXVAL(ABS(ptr%V - v6)) .LT. tol, "strain vector: V")
  CALL OK(ABS(ptr%scale - 0.5_DFP) .LT. tol, "strain vector: scale")
  T = ptr
  CALL OK(ABS(T(1, 2) - 4.0_DFP) .LT. tol, "strain vector: T12")
  DEALLOCATE (ptr)
END BLOCK

! Test 2: an unknown Voigt type is kept but treated as stress-like
BLOCK
  CLASS(VoigtRank2Tensor_), POINTER :: ptr

  ptr => VoigtRank2Tensor_Pointer(v6, 99)
  CALL OK(ptr%voigtType .EQ. 99, "unknown: voigtType is stored")
  CALL OK(ABS(ptr%scale - 1.0_DFP) .LT. tol, "unknown: scale is 1")
  DEALLOCATE (ptr)
END BLOCK

! Test 3: two calls give two independent objects
BLOCK
  CLASS(VoigtRank2Tensor_), POINTER :: p1, p2

  p1 => VoigtRank2Tensor_Pointer(v6, StressTypeVoigt)
  p2 => VoigtRank2Tensor_Pointer(2.0_DFP * v6, StressTypeVoigt)
  CALL OK(.NOT. ASSOCIATED(p1, p2), "two calls: different targets")
  CALL OK(ABS(p1%V(1) - 1.0_DFP) .LT. tol, "two calls: p1 unchanged")
  DEALLOCATE (p1, p2)
END BLOCK

END PROGRAM main
