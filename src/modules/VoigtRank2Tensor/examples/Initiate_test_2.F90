!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Initiate a VoigtRank2Tensor from reduced Voigt vectors

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
USE VoigtRank2Tensor_Method, ONLY: Initiate
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                 5.0_DFP, 6.0_DFP]

! Test 1: plane stress vector [11,22,12], out-of-plane entries are zero
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, [1.0_DFP, 2.0_DFP, 4.0_DFP], StressTypeVoigt, &
                VoigtFormPlaneStress)
  CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 0.0_DFP, 4.0_DFP, &
                              0.0_DFP, 0.0_DFP])) .LT. tol, &
          "plane stress: V")
  CALL OK(obj%voigtType .EQ. StressTypeVoigt, "plane stress: voigtType")
END BLOCK

! Test 2: axisymmetric vector [11,22,12,33]
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP], &
                StressTypeVoigt, VoigtFormAxisymmetric)
  CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                              0.0_DFP, 0.0_DFP])) .LT. tol, &
          "axisymmetric: V")
END BLOCK

! Test 3: 3D form copies the 6 entries
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, v6, StressTypeVoigt, VoigtForm3D)
  CALL OK(MAXVAL(ABS(obj%V - v6)) .LT. tol, "3D: V")
END BLOCK

! Test 4: an unknown form is treated as 3D
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, v6, StressTypeVoigt, 99)
  CALL OK(MAXVAL(ABS(obj%V - v6)) .LT. tol, "unknown form: V")
END BLOCK

! Test 5: only the first VoigtSize entries are read
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, v6, StressTypeVoigt, VoigtFormPlaneStrain)
  CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 0.0_DFP, 3.0_DFP, &
                              0.0_DFP, 0.0_DFP])) .LT. tol, &
          "longer vec: first 3 entries used")
END BLOCK

! Test 6: strain-like plane strain, engineering shear and T33 = 0
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T(3, 3)

  CALL Initiate(obj, [1.0_DFP, 2.0_DFP, 8.0_DFP], StrainTypeVoigt, &
                VoigtFormPlaneStrain)
  CALL OK(ABS(obj%scale - 0.5_DFP) .LT. tol, "strain: scale is 0.5")
  T = obj
  CALL OK(ABS(T(1, 2) - 4.0_DFP) .LT. tol, "strain: T12")
  CALL OK(ABS(T(3, 3)) .LT. tol, "strain: T33 is zero")
END BLOCK

! Test 7: round trip with GetVoigtVector
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP), ALLOCATABLE :: v(:)

  CALL Initiate(obj, [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP], &
                StrainTypeVoigt, VoigtFormAxisymmetric)
  v = GetVoigtVector(obj, VoigtFormAxisymmetric)
  CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
          .LT. tol, "round trip: axisymmetric")
END BLOCK

END PROGRAM main
