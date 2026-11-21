!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Display writes the message and the Voigt type to a unit

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: Display
USE VoigtRank2Tensor_Method, ONLY: Initiate
IMPLICIT NONE

REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                 5.0_DFP, 6.0_DFP]

! Test 1: stress-like tensor on stdout (default unit)
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, v6, StressTypeVoigt)
  CALL Display(obj, "stress-like tensor")
  CALL OK(.TRUE., "stdout: no crash")
END BLOCK

! Test 2: stress-like tensor, header lines written to a given unit
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  CHARACTER(80) :: line1, line2

  CALL Initiate(obj, v6, StressTypeVoigt)
  CALL ReadHeader(obj, "sigma", line1, line2)
  CALL OK(TRIM(line1) .EQ. "# sigma", "stress: message line")
  CALL OK(TRIM(line2) .EQ. "Stress Like Voigt Type", "stress: type line")
END BLOCK

! Test 3: strain-like tensor
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  CHARACTER(80) :: line1, line2

  CALL Initiate(obj, v6, StrainTypeVoigt)
  CALL ReadHeader(obj, "eps", line1, line2)
  CALL OK(TRIM(line1) .EQ. "# eps", "strain: message line")
  CALL OK(TRIM(line2) .EQ. "Strain Like Voigt Type", "strain: type line")
END BLOCK

! Test 4: an unknown Voigt type is reported as treated stress like
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  CHARACTER(80) :: line1, line2

  CALL Initiate(obj, v6, 99)
  CALL ReadHeader(obj, "unknown", line1, line2)
  CALL OK(TRIM(line2) .EQ. "Unknown Voigt Type (treated as stress like)", &
          "unknown: type line")
END BLOCK

CONTAINS

! Displays obj on a scratch unit and reads back the first two lines.
SUBROUTINE ReadHeader(obj, msg, line1, line2)
  TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  CHARACTER(80), INTENT(OUT) :: line1
  CHARACTER(80), INTENT(OUT) :: line2
  INTEGER(I4B) :: unitNo

  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(obj, msg, unitNo)
  REWIND (unitNo)
  READ (unitNo, "(A)") line1
  READ (unitNo, "(A)") line2
  CLOSE (unitNo)
END SUBROUTINE ReadHeader

END PROGRAM main
