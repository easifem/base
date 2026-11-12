!> author: Shion Shimizu
! date: 2026-11-02
! summary: GetVoigtVector_ writes the reduced vector in a given array

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector_
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                 5.0_DFP, 6.0_DFP]
TYPE(VoigtRank2Tensor_) :: obj

obj = VoigtRank2Tensor(v6, StressTypeVoigt)

! Test 1: 3D form writes 6 entries
BLOCK
  REAL(DFP) :: ans(6)
  INTEGER(I4B) :: tsize

  CALL GetVoigtVector_(obj, VoigtForm3D, ans, tsize)
  CALL OK(tsize .EQ. 6, "3D: tsize")
  CALL OK(MAXVAL(ABS(ans - v6)) .LT. tol, "3D: values")
END BLOCK

! Test 2: plane strain writes [11,22,12], the rest of ans is untouched
BLOCK
  REAL(DFP) :: ans(6)
  INTEGER(I4B) :: tsize

  ans = -1.0_DFP
  CALL GetVoigtVector_(obj, VoigtFormPlaneStrain, ans, tsize)
  CALL OK(tsize .EQ. 3, "plane strain: tsize")
  CALL OK(MAXVAL(ABS(ans(1:3) - [1.0_DFP, 2.0_DFP, 4.0_DFP])) .LT. tol, &
          "plane strain: values")
  CALL OK(ALL(ans(4:6) .EQ. -1.0_DFP), "plane strain: rest untouched")
END BLOCK

! Test 3: axisymmetric writes [11,22,12,33]
BLOCK
  REAL(DFP) :: ans(4)
  INTEGER(I4B) :: tsize

  CALL GetVoigtVector_(obj, VoigtFormAxisymmetric, ans, tsize)
  CALL OK(tsize .EQ. 4, "axisymmetric: tsize")
  CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
          .LT. tol, "axisymmetric: values")
END BLOCK

END PROGRAM main
