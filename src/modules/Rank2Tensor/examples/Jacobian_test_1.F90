!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Volume ratio J = det F of a deformation gradient

PROGRAM main
USE BaseType, ONLY: DeformationGradient_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: Jacobian
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: identity has J = 1
BLOCK
  TYPE(DeformationGradient_) :: F

  F = DeformationGradient(Eye3)
  CALL OK(ABS(Jacobian(F) - 1.0_DFP) .LT. tol, "identity: J = 1")
END BLOCK

! Test 2: upper triangular F, J is the product of the diagonal
BLOCK
  TYPE(DeformationGradient_) :: F
  REAL(DFP) :: mat(3, 3)

  mat = RESHAPE([2.0_DFP, 0.0_DFP, 0.0_DFP, &
                 1.0_DFP, 3.0_DFP, 0.0_DFP, &
                 0.5_DFP, -1.0_DFP, 4.0_DFP], [3, 3])
  F = DeformationGradient(mat)
  CALL OK(ABS(Jacobian(F) - 24.0_DFP) .LT. tol, "triangular: J = 24")
END BLOCK

! Test 3: a general F
BLOCK
  TYPE(DeformationGradient_) :: F
  REAL(DFP) :: mat(3, 3)

  mat = RESHAPE([1.2_DFP, 0.1_DFP, 0.0_DFP, &
                 0.3_DFP, 0.9_DFP, 0.2_DFP, &
                 0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
  F = DeformationGradient(mat)
  CALL OK(ABS(Jacobian(F) - 1.179_DFP) .LT. tol, "general: J = 1.179")
END BLOCK

END PROGRAM main
