!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: GetVoigtVector_ for plane stress and an unknown form

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStress
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector_
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
                                 10.0_DFP, 12.0_DFP]
TYPE(VoigtRank2Tensor_) :: obj

obj = VoigtRank2Tensor(v6, StrainTypeVoigt)

! Test 1: plane stress writes [11,22,12] in an exact-size array
BLOCK
  REAL(DFP) :: ans(3)
  INTEGER(I4B) :: tsize

  CALL GetVoigtVector_(obj, VoigtFormPlaneStress, ans, tsize)
  CALL OK(tsize .EQ. 3, "plane stress: tsize")
  CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 8.0_DFP])) .LT. tol, &
          "plane stress: values")
END BLOCK

! Test 2: an unknown form writes the 3D vector
BLOCK
  REAL(DFP) :: ans(6)
  INTEGER(I4B) :: tsize

  CALL GetVoigtVector_(obj, 99, ans, tsize)
  CALL OK(tsize .EQ. 6, "unknown: tsize")
  CALL OK(MAXVAL(ABS(ans - v6)) .LT. tol, "unknown: values")
END BLOCK

! Test 3: same result as GetVoigtVector, extra entries are untouched
BLOCK
  REAL(DFP) :: ans(10)
  REAL(DFP), ALLOCATABLE :: ref(:)
  INTEGER(I4B) :: tsize

  ans = -1.0_DFP
  ref = GetVoigtVector(obj, VoigtFormAxisymmetric)
  CALL GetVoigtVector_(obj, VoigtFormAxisymmetric, ans, tsize)
  CALL OK(tsize .EQ. SIZE(ref), "axisymmetric: tsize")
  CALL OK(MAXVAL(ABS(ans(1:tsize) - ref)) .LT. tol, &
          "axisymmetric: same as GetVoigtVector")
  CALL OK(ALL(ans(tsize + 1:) .EQ. -1.0_DFP), "axisymmetric: rest untouched")
END BLOCK

END PROGRAM main
