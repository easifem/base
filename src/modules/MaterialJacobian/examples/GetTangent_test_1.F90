!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: GetTangent of an isotropic tangent in each Voigt form
!
! The plane strain and plane stress matrices are compared with the
! closed forms in terms of Young's modulus E and Poisson's ratio nu.

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE MaterialJacobian_Method, ONLY: GetTangent
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: E = 200.0_DFP
REAL(DFP), PARAMETER :: nu = 0.3_DFP
REAL(DFP), PARAMETER :: lam = E * nu / ((1.0_DFP + nu) &
                                        * (1.0_DFP - 2.0_DFP * nu))
REAL(DFP), PARAMETER :: mu = E / (2.0_DFP * (1.0_DFP + nu))
TYPE(MaterialJacobian_) :: obj

obj = MaterialJacobian(IsotropicTangent(lam, mu))

! Test 1: the default form is 3D
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)

  C2 = GetTangent(obj)
  CALL OK(SIZE(C2, 1) .EQ. 6 .AND. SIZE(C2, 2) .EQ. 6, "3D: 6x6")
END BLOCK

! Test 2: plane strain
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)
  REAL(DFP) :: a

  a = E / ((1.0_DFP + nu) * (1.0_DFP - 2.0_DFP * nu))
  C2 = GetTangent(obj, VoigtFormPlaneStrain)
  CALL OK(SIZE(C2, 1) .EQ. 3, "plane strain: 3x3")
  CALL OK(ABS(C2(1, 1) - a * (1.0_DFP - nu)) .LT. tol, &
          "plane strain: C11")
  CALL OK(ABS(C2(1, 2) - a * nu) .LT. tol, "plane strain: C12")
  CALL OK(ABS(C2(3, 3) - a * (1.0_DFP - 2.0_DFP * nu) / 2.0_DFP) &
          .LT. tol, "plane strain: C33")
END BLOCK

! Test 3: plane stress
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)
  REAL(DFP) :: a

  a = E / (1.0_DFP - nu * nu)
  C2 = GetTangent(obj, VoigtFormPlaneStress)
  CALL OK(ABS(C2(1, 1) - a) .LT. tol, "plane stress: C11")
  CALL OK(ABS(C2(1, 2) - a * nu) .LT. tol, "plane stress: C12")
  CALL OK(ABS(C2(3, 3) - mu) .LT. tol, "plane stress: C33")
  CALL OK(ABS(C2(1, 3)) .LT. tol, "plane stress: C13 is zero")
END BLOCK

! Test 4: axisymmetric [11, 22, 12, 33]
BLOCK
  REAL(DFP), ALLOCATABLE :: C2(:, :)

  C2 = GetTangent(obj, VoigtFormAxisymmetric)
  CALL OK(SIZE(C2, 1) .EQ. 4, "axisymmetric: 4x4")
  CALL OK(ABS(C2(4, 4) - (lam + 2.0_DFP * mu)) .LT. tol, &
          "axisymmetric: C44")
  CALL OK(ABS(C2(1, 4) - lam) .LT. tol, "axisymmetric: C14")
  CALL OK(ABS(C2(3, 3) - mu) .LT. tol, "axisymmetric: C33")
END BLOCK

END PROGRAM main
