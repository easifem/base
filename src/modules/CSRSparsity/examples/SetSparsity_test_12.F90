!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: SetSparsity(obj) on a pattern with no entries
!
! Description:
!   The final SetSparsity call is made without any earlier
!   SetSparsity(obj, row, col) call, so the pattern has no entries.
!   The result should be a valid empty CSR pattern: nnz is zero and
!   IA is [1, 1, ..., 1].
!
!   Test 2 adds entries to one row only, so the other rows are empty.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: GetNNZ
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: no entries at all
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr)
  CALL OK(csr%isSparsityLock, "empty: isSparsityLock")
  CALL OK(GetNNZ(obj=csr) .EQ. 0_I4B, "empty: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 1, 1, 1]), "empty: IA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: only row 2 has entries
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr, row=2_I4B, col=[3_I4B, 1_I4B])
  CALL SetSparsity(obj=csr)
  CALL OK(GetNNZ(obj=csr) .EQ. 2_I4B, "one row: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 1, 3, 3]), "one row: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 3]), "one row: JA")
  CALL DEALLOCATE (csr)
END BLOCK

END PROGRAM main
