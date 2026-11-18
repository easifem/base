!> author: Shion Shimizu
! date: 2026-11-17
! summary: Display(obj, msg, unitNo, orient)
!
! Description:
!   Writes the tensor (and an array of tensors) to a scratch unit and
!   looks for the expected text in the lines.

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: Display
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: vec(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, &
                                  4.0_DFP, 5.0_DFP, 6.0_DFP]

! Test 1: stress-like tensor
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  INTEGER(I4B) :: unitNo

  obj = VoigtRank2Tensor(vec, StressTypeVoigt)
  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(obj, "my tensor", unitNo=unitNo)
  CALL OK(Found(unitNo, "my tensor"), "stress: message")
  CALL OK(Found(unitNo, "voigtType: stress-like"), "stress: voigtType")
  CALL OK(Found(unitNo, "scale: "), "stress: scale")
  CALL OK(Found(unitNo, "V: "), "stress: V")
  CLOSE (unitNo)
END BLOCK

! Test 2: strain-like tensor, column orientation
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  INTEGER(I4B) :: unitNo

  obj = VoigtRank2Tensor(vec, StrainTypeVoigt)
  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(obj, "my strain", unitNo=unitNo, orient="col")
  CALL OK(Found(unitNo, "voigtType: strain-like"), "strain: voigtType")
  CLOSE (unitNo)
END BLOCK

! Test 3: array of tensors
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj(2)
  INTEGER(I4B) :: unitNo

  obj(1) = VoigtRank2Tensor(vec, StressTypeVoigt)
  obj(2) = VoigtRank2Tensor(vec, StrainTypeVoigt)
  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(obj, "my tensors", unitNo=unitNo)
  CALL OK(Found(unitNo, "my tensors"), "array: message")
  CALL OK(Found(unitNo, "size: 2"), "array: size")
  CALL OK(Found(unitNo, "(1)"), "array: first entry")
  CALL OK(Found(unitNo, "(2)"), "array: second entry")
  CLOSE (unitNo)
END BLOCK

CONTAINS

! Looks for text in the lines written to unitNo
FUNCTION Found(unitNo, text) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: unitNo
  CHARACTER(*), INTENT(IN) :: text
  LOGICAL :: ans

  CHARACTER(256) :: line, errmsg
  INTEGER(I4B) :: ios

  REWIND (unitNo)
  ans = .FALSE.
  scan: DO
    READ (unitNo, "(A)", iostat=ios, iomsg=errmsg) line
    IF (ios .NE. 0) EXIT scan
    ans = ans .OR. (INDEX(line, text) .GT. 0)
  END DO scan
END FUNCTION Found

END PROGRAM main
