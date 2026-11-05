!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Initiate(obj, IA, JA, ncol) and the matching CSRSparsity and
!   CSRSparsityPointer constructors
!
! Description:
!   The number of rows comes from SIZE(IA) and nnz from SIZE(JA). Checks
!   the default and given ncol, an empty row, a pattern with no entries,
!   and both constructors.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: ASSIGNMENT(=)
USE CSRSparsity_Method, ONLY: CSRSparsity
USE CSRSparsity_Method, ONLY: CSRSparsityPointer
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: Initiate
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: ncol defaults to the number of rows
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, IA=[1_I4B, 3_I4B, 3_I4B, 4_I4B], &
                JA=[1_I4B, 3_I4B, 2_I4B])
  CALL OK(csr%nrow .EQ. 3_I4B, "default ncol: nrow")
  CALL OK(csr%ncol .EQ. 3_I4B, "default ncol: ncol")
  CALL OK(csr%nnz .EQ. 3_I4B, "default ncol: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 3, 3, 4]), "default ncol: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 3, 2]), "default ncol: JA")
  CALL OK(csr%isInitiated, "default ncol: isInitiated")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: rectangular pattern with ncol
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, IA=[1_I4B, 2_I4B, 3_I4B], &
                JA=[5_I4B, 1_I4B], ncol=5_I4B)
  CALL OK(csr%nrow .EQ. 2_I4B, "ncol: nrow")
  CALL OK(csr%ncol .EQ. 5_I4B, "ncol: ncol")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 3: no entries
BLOCK
  TYPE(CSRSparsity_) :: csr
  INTEGER(I4B) :: empty(0)
  CALL Initiate(obj=csr, IA=[1_I4B, 1_I4B, 1_I4B], JA=empty)
  CALL OK(csr%nrow .EQ. 2_I4B, "empty: nrow")
  CALL OK(csr%nnz .EQ. 0_I4B, "empty: nnz")
  CALL OK(SIZE(csr%JA) .EQ. 0, "empty: SIZE(JA)")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 4: constructors
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(CSRSparsity_), POINTER :: ptr
  csr = CSRSparsity(IA=[1_I4B, 2_I4B, 3_I4B], JA=[2_I4B, 1_I4B])
  CALL OK(csr%nnz .EQ. 2_I4B, "CSRSparsity: nnz")
  CALL OK(ALL(csr%JA .EQ. [2, 1]), "CSRSparsity: JA")
  ptr => CSRSparsityPointer(IA=[1_I4B, 2_I4B, 3_I4B], JA=[2_I4B, 1_I4B])
  CALL OK(ptr%nnz .EQ. 2_I4B, "CSRSparsityPointer: nnz")
  CALL OK(ALL(ptr%JA .EQ. [2, 1]), "CSRSparsityPointer: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (ptr)
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
