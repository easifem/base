!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetUnitNo returns a free unit number

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MiscUtility, ONLY: GetUnitNo
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: u, v
LOGICAL(LGT) :: isOpen

u = GetUnitNo()
CALL OK(u .GE. 10_I4B .AND. u .LE. 1000_I4B, "unit is in [10, 1000]")

INQUIRE (UNIT=u, OPENED=isOpen)
CALL OK(.NOT. isOpen, "unit is not connected")

! once u is open, the next call returns another unit
OPEN (UNIT=u, STATUS="SCRATCH", ACTION="READWRITE")
v = GetUnitNo()
CALL OK(v .NE. u, "an open unit is not returned")
INQUIRE (UNIT=v, OPENED=isOpen)
CALL OK(.NOT. isOpen, "second unit is not connected")
CLOSE (u)

END PROGRAM main
