!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Construct a velocity gradient and a pointer to one

PROGRAM main
USE BaseType, ONLY: VelocityGradient_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: VelocityGradient
USE Rank2Tensor_Method, ONLY: VelocityGradient_Pointer
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: L0(3, 3) = RESHAPE( &
                        [0.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, -0.1_DFP, 0.2_DFP, &
                         0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])

! Test 1: from a matrix
BLOCK
  TYPE(VelocityGradient_) :: L

  L = VelocityGradient(L0)
  CALL OK(MAXVAL(ABS(L%T - L0)) .LT. tol, "VelocityGradient: components")
END BLOCK

! Test 2: pointer from a matrix
BLOCK
  CLASS(VelocityGradient_), POINTER :: ptr

  ptr => VelocityGradient_Pointer(L0)
  CALL OK(ASSOCIATED(ptr), "pointer: associated")
  CALL OK(MAXVAL(ABS(ptr%T - L0)) .LT. tol, "pointer: components")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
