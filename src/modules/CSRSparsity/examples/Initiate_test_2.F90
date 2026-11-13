!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Copying with Initiate(obj, obj2) and assignment, and DEALLOCATE
!
! Description:
!   Copies a finished pattern and one still being built, checks that the
!   copy is independent of the source, and that DEALLOCATE resets every
!   component so that the object can be initiated with a new size.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: ASSIGNMENT(=)
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: copy of a finished pattern with Initiate
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  CALL Initiate(obj=csr1, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr1, row=1_I4B, col=[2_I4B, 1_I4B])
  CALL SetSparsity(csr1)
  CALL Initiate(obj=csr2, obj2=csr1)
  CALL OK(csr2%isSparsityLock, "Initiate: locked")
  CALL OK(csr2%nnz .EQ. 2_I4B, "Initiate: nnz")
  CALL OK(ALL(csr2%IA .EQ. [1, 3, 3, 3]), "Initiate: IA")
  CALL OK(ALL(csr2%JA .EQ. [1, 2]), "Initiate: JA")
  CALL DEALLOCATE (csr1)
  CALL OK(ALL(csr2%JA .EQ. [1, 2]), "Initiate: independent of source")
  CALL DEALLOCATE (csr2)
END BLOCK

! Test 2: assignment replaces the content of the left side
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  CALL Initiate(obj=csr1, ncol=2_I4B, nrow=2_I4B)
  CALL SetSparsity(obj=csr1, row=2_I4B, col=[1_I4B])
  CALL SetSparsity(csr1)
  CALL Initiate(obj=csr2, ncol=5_I4B, nrow=5_I4B, nnz=9_I4B)
  csr2 = csr1
  CALL OK(csr2%nrow .EQ. 2_I4B, "assignment: nrow")
  CALL OK(ALL(csr2%IA .EQ. [1, 1, 2]), "assignment: IA")
  CALL OK(ALL(csr2%JA .EQ. [1]), "assignment: JA")
  CALL DEALLOCATE (csr1)
  CALL DEALLOCATE (csr2)
END BLOCK

! Test 3: copy of a pattern that is still being built
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  CALL Initiate(obj=csr1, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr1, row=1_I4B, col=[2_I4B, 1_I4B])
  csr2 = csr1
  CALL OK(ALLOCATED(csr2%row), "partial copy: row storage is copied")
  IF (ALLOCATED(csr2%row)) THEN
    CALL SetSparsity(csr2)
    CALL OK(csr2%nnz .EQ. 2_I4B, "partial copy: nnz")
    CALL OK(ALL(csr2%IA .EQ. [1, 3, 3, 3]), "partial copy: IA")
    CALL OK(ALL(csr2%JA .EQ. [1, 2]), "partial copy: JA")
  END IF
  CALL DEALLOCATE (csr1)
  CALL DEALLOCATE (csr2)
END BLOCK

! Test 4: DEALLOCATE resets every component
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B])
  CALL DEALLOCATE (csr)
  CALL OK(.NOT. csr%isInitiated, "DEALLOCATE: isInitiated")
  CALL OK(.NOT. csr%isSparsityLock, "DEALLOCATE: isSparsityLock")
  CALL OK(csr%nnz .EQ. 0_I4B, "DEALLOCATE: nnz")
  CALL OK(csr%nrow .EQ. 0_I4B, "DEALLOCATE: nrow")
  CALL OK(csr%ncol .EQ. 0_I4B, "DEALLOCATE: ncol")
  CALL OK(.NOT. ALLOCATED(csr%IA), "DEALLOCATE: IA")
  CALL OK(.NOT. ALLOCATED(csr%JA), "DEALLOCATE: JA")
  CALL OK(.NOT. ALLOCATED(csr%idiag), "DEALLOCATE: idiag")
  CALL OK(.NOT. ALLOCATED(csr%row), "DEALLOCATE: row")
END BLOCK

! Test 5: DEALLOCATE of an object that was never initiated
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL DEALLOCATE (csr)
  CALL OK(.NOT. csr%isInitiated, "DEALLOCATE: empty object")
END BLOCK

END PROGRAM main
