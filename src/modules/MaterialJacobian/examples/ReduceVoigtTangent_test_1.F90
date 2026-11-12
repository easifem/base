!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: ReduceVoigtTangent of an anisotropic tangent
!
! For plane stress the reduced tangent must give the in-plane stress
! of a strain state whose out-of-plane stresses vanish.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: C6(6, 6), B(6, 6)
INTEGER(I4B) :: i, j

! symmetric positive definite anisotropic tangent
DO j = 1, 6
  DO i = 1, 6
    B(i, j) = REAL(MOD(3 * i + 5 * j, 7), DFP) / 7.0_DFP
  END DO
END DO
C6 = MATMUL(TRANSPOSE(B), B)
DO i = 1, 6
  C6(i, i) = C6(i, i) + 5.0_DFP
END DO

! Test 1: plane strain is the sub-matrix [1, 2, 4]
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)

  C2 = ReduceVoigtTangent(C6, VoigtFormPlaneStrain)
  CALL OK(MAXVAL(ABS(C2 - C6([1, 2, 4], [1, 2, 4]))) .LT. tol, &
          "plane strain: sub-matrix")
END BLOCK

! Test 2: plane stress, out-of-plane stresses vanish
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)
  REAL(DFP) :: ei(3), eo(3), e6(6), s6(6)

  C2 = ReduceVoigtTangent(C6, VoigtFormPlaneStress)
  ei = [1.0E-3_DFP, -2.0E-3_DFP, 0.5E-3_DFP]
  eo = -MATMUL(Inverse3(C6([3, 5, 6], [3, 5, 6])), &
               MATMUL(C6([3, 5, 6], [1, 2, 4]), ei))
  e6([1, 2, 4]) = ei
  e6([3, 5, 6]) = eo
  s6 = MATMUL(C6, e6)
  CALL OK(MAXVAL(ABS(s6([3, 5, 6]))) .LT. tol, &
          "plane stress: s33 = s23 = s13 = 0")
  CALL OK(MAXVAL(ABS(s6([1, 2, 4]) - MATMUL(C2, ei))) .LT. tol, &
          "plane stress: condensed tangent")
END BLOCK

CONTAINS

! Inverse of a 3x3 matrix by cofactors
FUNCTION Inverse3(A) RESULT(X)
  REAL(DFP), INTENT(IN) :: A(3, 3)
  REAL(DFP) :: X(3, 3)

  X(1, 1) = A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)
  X(1, 2) = A(1, 3) * A(3, 2) - A(1, 2) * A(3, 3)
  X(1, 3) = A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2)
  X(2, 1) = A(2, 3) * A(3, 1) - A(2, 1) * A(3, 3)
  X(2, 2) = A(1, 1) * A(3, 3) - A(1, 3) * A(3, 1)
  X(2, 3) = A(1, 3) * A(2, 1) - A(1, 1) * A(2, 3)
  X(3, 1) = A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1)
  X(3, 2) = A(1, 2) * A(3, 1) - A(1, 1) * A(3, 2)
  X(3, 3) = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
  X = X / (A(1, 1) * X(1, 1) + A(1, 2) * X(2, 1) + A(1, 3) * X(3, 1))
END FUNCTION Inverse3

END PROGRAM main
