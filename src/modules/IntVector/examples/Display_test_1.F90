!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Display of IntVector_
!
! Display writes to a scratch file. The test reads the file back and
! looks for the values. Only the active entries `val(1:tsize)` should be
! shown.

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Display
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: an allocated IntVector_ shows its values
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: unitNo
  obj = [111_I4B, 222_I4B, 999_I4B]
  OPEN (newunit=unitNo, status="scratch", action="readwrite")
  CALL Display(obj, "obj", unitNo=unitNo)
  CALL OK(HasText(unitNo, "222"), "Display: shows 222")
  CALL OK(HasText(unitNo, "999"), "Display: shows 999")
  CLOSE (unitNo)
END BLOCK

! Test 2: entries beyond tsize are not shown
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: unitNo
  obj = [111_I4B, 222_I4B, 999_I4B]
  obj%tsize = 2
  OPEN (newunit=unitNo, status="scratch", action="readwrite")
  CALL Display(obj, "obj", unitNo=unitNo)
  CALL OK(HasText(unitNo, "222"), "Display: spare, shows 222")
  CALL OK(.NOT. HasText(unitNo, "999"), "Display: spare, hides 999")
  CLOSE (unitNo)
END BLOCK

! Test 3: an unallocated IntVector_ shows no values
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: unitNo
  OPEN (newunit=unitNo, status="scratch", action="readwrite")
  CALL Display(obj, "empty", unitNo=unitNo)
  CALL OK(HasText(unitNo, "empty"), "Display: empty, shows msg")
  CLOSE (unitNo)
END BLOCK

! Test 4: a vector of IntVector_ shows every item
BLOCK
  TYPE(IntVector_) :: obj(2)
  INTEGER(I4B) :: unitNo
  obj(1) = [111_I4B]
  obj(2) = [222_I4B]
  OPEN (newunit=unitNo, status="scratch", action="readwrite")
  CALL Display(obj, "vec", unitNo=unitNo)
  CALL OK(HasText(unitNo, "111"), "Display: vector, shows obj(1)")
  CALL OK(HasText(unitNo, "222"), "Display: vector, shows obj(2)")
  CLOSE (unitNo)
END BLOCK

CONTAINS

!> Returns true if any line of the file on unitNo contains txt
FUNCTION HasText(unitNo, txt) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: unitNo
  !! unit of an open scratch file
  CHARACTER(*), INTENT(IN) :: txt
  !! text to look for
  LOGICAL :: ans
  !! true if txt is found
  CHARACTER(256) :: line, iomsg
  INTEGER(I4B) :: ios

  ans = .FALSE.
  REWIND (unitNo)
  readLoop: DO
    READ (unitNo, "(A)", iostat=ios, iomsg=iomsg) line
    IF (ios .NE. 0) EXIT readLoop
    IF (INDEX(line, txt) .GT. 0) THEN
      ans = .TRUE.
      EXIT readLoop
    END IF
  END DO readLoop
END FUNCTION HasText

END PROGRAM main
