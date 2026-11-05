!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: GetSym(obj, symObj, from)
!
! Description:
!   Builds the symmetric pattern from the upper or the lower triangle.
!   Both inputs below give the same result
!
!     row 1: columns 1, 2, 3
!     row 2: columns 1, 2
!     row 3: columns 1, 3
!
!   Also covers a triangle with no entry on the diagonal of one row.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: GetSym
USE CSRSparsity_Method, ONLY: Initiate
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: symIA(4) = [1, 4, 6, 8]
INTEGER(I4B), PARAMETER :: symJA(7) = [1, 2, 3, 1, 2, 1, 3]
INTEGER(I4B), PARAMETER :: symDiag(3) = [1, 5, 7]

! Test 1: from the upper triangle
BLOCK
  TYPE(CSRSparsity_) :: csr, sym
  CALL Initiate(obj=csr, IA=[1_I4B, 4_I4B, 5_I4B, 6_I4B], &
                JA=[1_I4B, 2_I4B, 3_I4B, 2_I4B, 3_I4B])
  CALL GetSym(obj=csr, symObj=sym, from="U")
  CALL OK(sym%nnz .EQ. 7_I4B, "U: nnz")
  CALL OK(sym%nrow .EQ. 3_I4B, "U: nrow")
  CALL OK(sym%isDiagStored, "U: isDiagStored")
  CALL OK(ALL(sym%IA .EQ. symIA), "U: IA")
  CALL OK(ALL(sym%JA .EQ. symJA), "U: JA")
  CALL OK(ALL(sym%idiag .EQ. symDiag), "U: idiag")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (sym)
END BLOCK

! Test 2: from the lower triangle
BLOCK
  TYPE(CSRSparsity_) :: csr, sym
  CALL Initiate(obj=csr, IA=[1_I4B, 2_I4B, 4_I4B, 6_I4B], &
                JA=[1_I4B, 1_I4B, 2_I4B, 1_I4B, 3_I4B])
  CALL GetSym(obj=csr, symObj=sym, from="L")
  CALL OK(sym%nnz .EQ. 7_I4B, "L: nnz")
  CALL OK(sym%isDiagStored, "L: isDiagStored")
  CALL OK(ALL(sym%IA .EQ. symIA), "L: IA")
  CALL OK(ALL(sym%JA .EQ. symJA), "L: JA")
  CALL OK(ALL(sym%idiag .EQ. symDiag), "L: idiag")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (sym)
END BLOCK

! Test 3: from accepts lower case letters
BLOCK
  TYPE(CSRSparsity_) :: csr, sym
  CALL Initiate(obj=csr, IA=[1_I4B, 3_I4B, 4_I4B], &
                JA=[1_I4B, 2_I4B, 2_I4B])
  CALL GetSym(obj=csr, symObj=sym, from="u")
  CALL OK(sym%nnz .EQ. 4_I4B, "lower case: nnz")
  CALL OK(ALL(sym%JA .EQ. [1, 2, 1, 2]), "lower case: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (sym)
END BLOCK

! Test 4: row 2 has no diagonal entry, idiag(2) is zero
BLOCK
  TYPE(CSRSparsity_) :: csr, sym
  CALL Initiate(obj=csr, IA=[1_I4B, 3_I4B, 4_I4B, 5_I4B], &
                JA=[1_I4B, 2_I4B, 3_I4B, 3_I4B])
  CALL GetSym(obj=csr, symObj=sym, from="U")
  CALL OK(sym%nnz .EQ. 6_I4B, "no diagonal: nnz")
  CALL OK(ALL(sym%IA .EQ. [1, 3, 5, 7]), "no diagonal: IA")
  CALL OK(ALL(sym%JA .EQ. [1, 2, 1, 3, 2, 3]), "no diagonal: JA")
  CALL OK(ALL(sym%idiag .EQ. [1, 0, 6]), "no diagonal: idiag")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (sym)
END BLOCK

END PROGRAM main
