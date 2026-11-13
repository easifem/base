!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Display(obj, Msg, UnitNo)
!
! Description:
!   Writes the object to a scratch unit and reads the text back. Covers
!   a finished pattern and an initiated object with no JA.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: Display
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: finished pattern
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=2_I4B, nrow=2_I4B)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B])
  CALL SetSparsity(csr)
  CALL OK(Contains_(csr, "my title"), "finished: Msg")
  CALL OK(Contains_(csr, "# NNZ"), "finished: nnz")
  CALL OK(Contains_(csr, "# NROW"), "finished: nrow")
  CALL OK(Contains_(csr, "# NCOL"), "finished: ncol")
  CALL OK(Contains_(csr, "# IA"), "finished: IA")
  CALL OK(Contains_(csr, "# JA"), "finished: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: JA is not allocated
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=2_I4B, nrow=2_I4B)
  CALL OK(Contains_(csr, "# JA is not allocated"), "no JA: message")
  CALL DEALLOCATE (csr)
END BLOCK

CONTAINS

! Displays obj on a scratch unit and looks for text in the lines
FUNCTION Contains_(obj, text) RESULT(ans)
  TYPE(CSRSparsity_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: text
  LOGICAL :: ans

  CHARACTER(256) :: line, errmsg
  INTEGER(I4B) :: unitNo, ios

  OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
  CALL Display(obj=obj, Msg="my title", UnitNo=unitNo)
  REWIND (unitNo)

  ans = .FALSE.
  scan: DO
    READ (unitNo, "(A)", iostat=ios, iomsg=errmsg) line
    IF (ios .NE. 0) EXIT scan
    ans = ans .OR. (INDEX(line, text) .GT. 0)
  END DO scan

  CLOSE (unitNo)
END FUNCTION Contains_

END PROGRAM main
