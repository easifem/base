!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Construct a Rank2Tensor from a matrix or a Voigt tensor, as a
!   value or a pointer, and assign it to a matrix or a Voigt tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE Rank2Tensor_Method, ONLY: Display
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: Rank2Tensor_Pointer
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
                                            4.0_DFP, 2.0_DFP, 5.0_DFP, &
                                            6.0_DFP, 5.0_DFP, 3.0_DFP], &
                                           [3, 3])

! Test 1: from a matrix, with and without the symmetry flag
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(S0)
  CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "matrix: T")
  CALL OK(.NOT. obj%isSym, "matrix: isSym defaults to false")
  obj = Rank2Tensor(S0, .TRUE.)
  CALL OK(obj%isSym, "matrix: isSym is true")
  CALL Display(obj, "obj: ")
END BLOCK

! Test 2: from a stress-like and a strain-like Voigt tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(VoigtRank2Tensor(S0, StressTypeVoigt))
  CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "Voigt stress-like: T")
  CALL OK(obj%isSym, "Voigt stress-like: isSym")
  obj = Rank2Tensor(VoigtRank2Tensor(S0, StrainTypeVoigt))
  CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "Voigt strain-like: T")
  CALL OK(obj%isSym, "Voigt strain-like: isSym")
END BLOCK

! Test 3: pointer from a matrix and from a Voigt tensor
BLOCK
  CLASS(Rank2Tensor_), POINTER :: ptr

  ptr => Rank2Tensor_Pointer(S0, .TRUE.)
  CALL OK(MAXVAL(ABS(ptr%T - S0)) .LT. tol, "pointer, matrix: T")
  CALL OK(ptr%isSym, "pointer, matrix: isSym")
  DEALLOCATE (ptr)
  ptr => Rank2Tensor_Pointer(VoigtRank2Tensor(S0, StrainTypeVoigt))
  CALL OK(MAXVAL(ABS(ptr%T - S0)) .LT. tol, "pointer, Voigt: T")
  CALL OK(ptr%isSym, "pointer, Voigt: isSym")
  DEALLOCATE (ptr)
END BLOCK

! Test 4: obj = mat and mat = obj
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3)

  obj = S0
  CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "obj = mat: T")
  CALL OK(.NOT. obj%isSym, "obj = mat: not symmetric")
  mat = 0.0_DFP
  mat = obj
  CALL OK(MAXVAL(ABS(mat - S0)) .LT. tol, "mat = obj")
END BLOCK

! Test 5: VoigtRank2Tensor_ = obj is stress-like
BLOCK
  TYPE(VoigtRank2Tensor_) :: v

  v = Rank2Tensor(S0, .TRUE.)
  CALL OK(MAXVAL(ABS(v%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                            5.0_DFP, 6.0_DFP])) .LT. tol, "Voigt = obj: V")
  CALL OK(v%voigtType .EQ. StressTypeVoigt, "Voigt = obj: voigtType")
END BLOCK

END PROGRAM main
