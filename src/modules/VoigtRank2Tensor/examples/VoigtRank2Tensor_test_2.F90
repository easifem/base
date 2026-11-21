!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: VoigtRank2Tensor from reduced vectors and non-symmetric matrices

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: plane strain stress-like vector [11,22,12]
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T(3, 3)

  obj = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 4.0_DFP], StressTypeVoigt, &
                         VoigtFormPlaneStrain)
  T = obj
  CALL OK(ABS(T(1, 2) - 4.0_DFP) .LT. tol, "plane strain: T12")
  CALL OK(ABS(T(3, 3)) .LT. tol, "plane strain: T33 is zero")
  CALL OK(ABS(T(1, 3)) .LT. tol .AND. ABS(T(2, 3)) .LT. tol, &
          "plane strain: T13 and T23 are zero")
END BLOCK

! Test 2: plane stress strain-like vector, scale is 0.5
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  obj = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 8.0_DFP], StrainTypeVoigt, &
                         VoigtFormPlaneStress)
  CALL OK(obj%voigtType .EQ. StrainTypeVoigt, "plane stress: voigtType")
  CALL OK(ABS(obj%scale - 0.5_DFP) .LT. tol, "plane stress: scale")
  CALL OK(ABS(obj%V(4) - 8.0_DFP) .LT. tol, "plane stress: V4")
END BLOCK

! Test 3: 3D form gives the same result as the 2-argument constructor
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj1, obj2
  REAL(DFP) :: v6(6)

  v6 = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  obj1 = VoigtRank2Tensor(v6, StressTypeVoigt)
  obj2 = VoigtRank2Tensor(v6, StressTypeVoigt, VoigtForm3D)
  CALL OK(MAXVAL(ABS(obj1%V - obj2%V)) .LT. tol, "3D: same V")
END BLOCK

! Test 4: a non-symmetric matrix uses its symmetric part
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T(3, 3), T2(3, 3)

  T = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
               2.0_DFP, 2.0_DFP, 0.0_DFP, &
               4.0_DFP, 6.0_DFP, 3.0_DFP], [3, 3])
  obj = VoigtRank2Tensor(T, StressTypeVoigt)
  CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 1.0_DFP, &
                              3.0_DFP, 2.0_DFP])) .LT. tol, &
          "non-symmetric: V")
  T2 = obj
  CALL OK(MAXVAL(ABS(T2 - 0.5_DFP * (T + TRANSPOSE(T)))) .LT. tol, &
          "non-symmetric: T = sym(T)")
END BLOCK

END PROGRAM main
