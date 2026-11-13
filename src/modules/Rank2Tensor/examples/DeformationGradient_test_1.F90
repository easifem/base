!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Construct a deformation gradient and a pointer to one

PROGRAM main
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: DeformationGradient_Pointer
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
                        [1.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, 0.9_DFP, 0.2_DFP, &
                         0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])

! Test 1: from a matrix
BLOCK
  TYPE(DeformationGradient_) :: F

  F = DeformationGradient(F0)
  CALL OK(MAXVAL(ABS(F%T - F0)) .LT. tol, "mat: components")
END BLOCK

! Test 2: from a Rank2Tensor_
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(F0)
  F = DeformationGradient(obj)
  CALL OK(MAXVAL(ABS(F%T - F0)) .LT. tol, "obj: components")
  CALL OK(.NOT. F%isSym, "obj: not symmetric")
END BLOCK

! Test 3: with no argument F is zero
BLOCK
  TYPE(DeformationGradient_) :: F

  F = DeformationGradient()
  CALL OK(MAXVAL(ABS(F%T)) .LT. tol, "no argument: F is zero")
END BLOCK

! Test 4: pointer from a matrix
BLOCK
  CLASS(DeformationGradient_), POINTER :: ptr

  ptr => DeformationGradient_Pointer(F0)
  CALL OK(ASSOCIATED(ptr), "pointer mat: associated")
  CALL OK(MAXVAL(ABS(ptr%T - F0)) .LT. tol, "pointer mat: components")
  DEALLOCATE (ptr)
END BLOCK

! Test 5: pointer from a Rank2Tensor_ and with no argument
BLOCK
  CLASS(DeformationGradient_), POINTER :: ptr

  ptr => DeformationGradient_Pointer(Rank2Tensor(F0))
  CALL OK(MAXVAL(ABS(ptr%T - F0)) .LT. tol, "pointer obj: components")
  DEALLOCATE (ptr)
  ptr => DeformationGradient_Pointer()
  CALL OK(MAXVAL(ABS(ptr%T)) .LT. tol, "pointer no argument: zero")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
