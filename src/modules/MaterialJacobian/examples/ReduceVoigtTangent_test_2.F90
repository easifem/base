!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: ReduceVoigtTangent of an isotropic tangent, closed forms
!
! For an isotropic material the plane stress tangent is known in
! closed form, with lambda* = 2 lambda mu / (lambda + 2 mu).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStress
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lam = 1.5_DFP
REAL(DFP), PARAMETER :: mu = 0.5_DFP
REAL(DFP) :: C(6, 6)

C = IsotropicTangent(lam, mu)

! Test 1: plane stress of an isotropic tangent
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)
  REAL(DFP) :: lamStar, expected(3, 3)

  lamStar = 2.0_DFP * lam * mu / (lam + 2.0_DFP * mu)
  expected = 0.0_DFP
  expected(1:2, 1:2) = lamStar
  expected(1, 1) = lamStar + 2.0_DFP * mu
  expected(2, 2) = lamStar + 2.0_DFP * mu
  expected(3, 3) = mu
  C2 = ReduceVoigtTangent(C, VoigtFormPlaneStress)
  CALL OK(SIZE(C2, 1) .EQ. 3 .AND. SIZE(C2, 2) .EQ. 3, &
          "plane stress: shape 3x3")
  CALL OK(MAXVAL(ABS(C2 - expected)) .LT. tol, "plane stress: values")
END BLOCK

! Test 2: axisymmetric uses the order [11, 22, 12, 33]
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)
  REAL(DFP) :: Ca(6, 6)
  INTEGER(I4B) :: i, j

  DO j = 1, 6
    DO i = 1, 6
      Ca(i, j) = REAL(10 * i + j, DFP)
    END DO
  END DO
  C2 = ReduceVoigtTangent(Ca, VoigtFormAxisymmetric)
  CALL OK(SIZE(C2, 1) .EQ. 4 .AND. SIZE(C2, 2) .EQ. 4, &
          "axisymmetric: shape 4x4")
  CALL OK(MAXVAL(ABS(C2 - Ca([1, 2, 4, 3], [1, 2, 4, 3]))) .LT. tol, &
          "axisymmetric: order [1, 2, 4, 3]")
END BLOCK

! Test 3: VoigtForm3D returns the full tangent
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)

  C2 = ReduceVoigtTangent(C, VoigtForm3D)
  CALL OK(SIZE(C2, 1) .EQ. 6 .AND. SIZE(C2, 2) .EQ. 6, "3D: shape 6x6")
  CALL OK(MAXVAL(ABS(C2 - C)) .LT. tol, "3D: values")
END BLOCK

END PROGRAM main
