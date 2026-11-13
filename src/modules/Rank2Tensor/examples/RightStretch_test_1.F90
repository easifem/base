!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Polar decomposition F = R U = V R and principal stretches

PROGRAM main
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: LeftStretch
USE Rank2Tensor_Method, ONLY: PrincipalStretch
USE Rank2Tensor_Method, ONLY: RightStretch
USE Rank2Tensor_Method, ONLY: Rotation
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
                        [1.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, 0.9_DFP, 0.2_DFP, &
                         0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])

! Test 1: F = R U with R a 90 degree rotation about z
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(Rank2Tensor_) :: U, V, R
  REAL(DFP) :: rot(3, 3), stretch(3, 3)

  rot = RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, &
                 -1.0_DFP, 0.0_DFP, 0.0_DFP, &
                 0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3])
  stretch = 0.0_DFP
  stretch(1, 1) = 2.0_DFP
  stretch(2, 2) = 3.0_DFP
  stretch(3, 3) = 1.0_DFP
  F = DeformationGradient(MATMUL(rot, stretch))
  U = RightStretch(F)
  V = LeftStretch(F)
  R = Rotation(F)
  CALL OK(MAXVAL(ABS(U%T - stretch)) .LT. tol, "rotated: U")
  CALL OK(MAXVAL(ABS(R%T - rot)) .LT. tol, "rotated: R")
  CALL OK(MAXVAL(ABS(V%T - MATMUL(rot, MATMUL(stretch, &
          TRANSPOSE(rot))))) .LT. tol, "rotated: V = R U R^T")
END BLOCK

! Test 2: general F, U^2 = F^T F, V^2 = F F^T, F = R U = V R
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(Rank2Tensor_) :: U, V, R

  F = DeformationGradient(F0)
  U = RightStretch(F)
  V = LeftStretch(F)
  R = Rotation(F)
  CALL OK(MAXVAL(ABS(MATMUL(U%T, U%T) - MATMUL(TRANSPOSE(F0), F0))) &
          .LT. tol, "general: U^2 = F^T F")
  CALL OK(MAXVAL(ABS(MATMUL(V%T, V%T) - MATMUL(F0, TRANSPOSE(F0)))) &
          .LT. tol, "general: V^2 = F F^T")
  CALL OK(MAXVAL(ABS(MATMUL(R%T, U%T) - F0)) .LT. tol, "general: F = R U")
  CALL OK(MAXVAL(ABS(MATMUL(V%T, R%T) - F0)) .LT. tol, "general: F = V R")
  CALL OK(MAXVAL(ABS(MATMUL(TRANSPOSE(R%T), R%T) - Eye3)) .LT. tol, &
          "general: R^T R = I")
  CALL OK(MAXVAL(ABS(U%T - TRANSPOSE(U%T))) .LT. tol, &
          "general: U symmetric")
END BLOCK

! Test 3: principal stretches are sorted in descending order
BLOCK
  TYPE(DeformationGradient_) :: F
  REAL(DFP) :: mat(3, 3), lam(3)

  mat = 0.0_DFP
  mat(1, 1) = 1.0_DFP
  mat(2, 2) = 3.0_DFP
  mat(3, 3) = 2.0_DFP
  F = DeformationGradient(mat)
  lam = PrincipalStretch(F)
  CALL OK(MAXVAL(ABS(lam - [3.0_DFP, 2.0_DFP, 1.0_DFP])) .LT. tol, &
          "diagonal: stretches 3, 2, 1")
END BLOCK

! Test 4: principal stretches of a general F
BLOCK
  TYPE(DeformationGradient_) :: F
  REAL(DFP) :: lam(3)

  F = DeformationGradient(F0)
  lam = PrincipalStretch(F)
  CALL OK(lam(1) .GE. lam(2) .AND. lam(2) .GE. lam(3), &
          "general: descending")
  CALL OK(ABS(PRODUCT(lam) - 1.179_DFP) .LT. tol, &
          "general: product is det F")
  CALL OK(ABS(SUM(lam**2) - SUM(F0**2)) .LT. tol, &
          "general: sum of squares is tr(F^T F)")
END BLOCK

END PROGRAM main
