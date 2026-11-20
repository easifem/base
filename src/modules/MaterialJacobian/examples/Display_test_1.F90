!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Display writes the tangent and the names of the measure tags
!
! The output is written to a scratch file. The last three lines are
! read back and compared with the expected tag names.

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StrainMeasureGreen
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressRateTruesdell
USE MaterialJacobian_Method, ONLY: Display
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: names of known tags
BLOCK
  TYPE(MaterialJacobian_) :: obj
  CHARACTER(128) :: lines(3)

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP), &
                         stressType=StressMeasurePK2, &
                         strainType=StrainMeasureGreen, &
                         rateType=StressRateTruesdell)
  CALL DisplayTail(obj, lines)
  CALL OK(TRIM(lines(1)) .EQ. "stress measure: second Piola-Kirchhoff", &
          "known: stress measure")
  CALL OK(TRIM(lines(2)) .EQ. "strain measure: Green-Lagrange", &
          "known: strain measure")
  CALL OK(TRIM(lines(3)) .EQ. "stress rate: Truesdell", &
          "known: stress rate")
END BLOCK

! Test 2: a tag outside the constants is shown as unknown
BLOCK
  TYPE(MaterialJacobian_) :: obj
  CHARACTER(128) :: lines(3)

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP), &
                         stressType=-7_I4B, strainType=99_I4B, &
                         rateType=42_I4B)
  CALL DisplayTail(obj, lines)
  CALL OK(TRIM(lines(1)) .EQ. "stress measure: unknown", &
          "unknown: stress measure")
  CALL OK(TRIM(lines(2)) .EQ. "strain measure: unknown", &
          "unknown: strain measure")
  CALL OK(TRIM(lines(3)) .EQ. "stress rate: unknown", &
          "unknown: stress rate")
END BLOCK

CONTAINS

! Display obj to a scratch file and return its last three lines
SUBROUTINE DisplayTail(obj, lines)
  TYPE(MaterialJacobian_), INTENT(IN) :: obj
  CHARACTER(128), INTENT(OUT) :: lines(3)
  INTEGER(I4B) :: unitNo, ios
  CHARACTER(128) :: line, iomsg

  lines = ""
  OPEN (newunit=unitNo, status="scratch", form="formatted", &
        action="readwrite")
  CALL Display(obj, "tangent", unitNo=unitNo)
  REWIND (unitNo)
  readLoop: DO
    READ (unitNo, "(A)", iostat=ios, iomsg=iomsg) line
    IF (ios .NE. 0) EXIT readLoop
    lines(1:2) = lines(2:3)
    lines(3) = line
  END DO readLoop
  CLOSE (unitNo)
END SUBROUTINE DisplayTail

END PROGRAM main
