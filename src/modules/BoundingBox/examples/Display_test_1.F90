!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Display of a BoundingBox_

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: Display
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(BoundingBox_) :: obj

obj = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                  2.0_DFP, 0.0_DFP, 0.0_DFP])

! Test 1: Display to stdout
BLOCK
  CALL Display(obj, msg="BoundingBox: ")
  CALL OK(.TRUE., "Display: stdout")
END BLOCK

! Test 2: Display to a file writes the message and the 10 fields
BLOCK
  INTEGER(I4B) :: unitNo, ios, nlines
  CHARACTER(256) :: line
  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(obj, msg="BoundingBox: ", unitNo=unitNo)
  REWIND (unitNo)
  nlines = 0
  DO
    READ (unitNo, "(A)", iostat=ios) line
    IF (ios .NE. 0) EXIT
    IF (LEN_TRIM(line) .GT. 0) nlines = nlines + 1
  END DO
  CLOSE (unitNo)
  CALL OK(nlines .GE. 11, "Display: unitNo, message and 10 fields")
END BLOCK

END PROGRAM main
