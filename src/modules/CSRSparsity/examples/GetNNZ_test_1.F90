!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: GetNNZ(obj), GetNNZ(obj, from) and GetNNZ(obj, from(1))
!
! Description:
!   Uses the pattern
!
!     row 1: columns 1, 2, 3
!     row 2: column  2
!     row 3: columns 1, 3
!
!   which has 2 entries above the diagonal, 1 below and 3 on it.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: GetNNZ
USE CSRSparsity_Method, ONLY: Initiate
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(CSRSparsity_) :: csr

CALL Initiate(obj=csr, IA=[1_I4B, 4_I4B, 5_I4B, 7_I4B], &
              JA=[1_I4B, 2_I4B, 3_I4B, 2_I4B, 1_I4B, 3_I4B])

! Test 1: total number of entries
BLOCK
  CALL OK(GetNNZ(csr) .EQ. 6_I4B, "total")
END BLOCK

! Test 2: one part, upper and lower case
BLOCK
  CALL OK(GetNNZ(obj=csr, from="U") .EQ. 2_I4B, "from: U")
  CALL OK(GetNNZ(obj=csr, from="u") .EQ. 2_I4B, "from: u")
  CALL OK(GetNNZ(obj=csr, from="L") .EQ. 1_I4B, "from: L")
  CALL OK(GetNNZ(obj=csr, from="l") .EQ. 1_I4B, "from: l")
  CALL OK(GetNNZ(obj=csr, from="D") .EQ. 3_I4B, "from: D")
  CALL OK(GetNNZ(obj=csr, from="d") .EQ. 3_I4B, "from: d")
  CALL OK(GetNNZ(obj=csr, from="A") .EQ. 6_I4B, "from: A")
  CALL OK(GetNNZ(obj=csr, from="?") .EQ. 6_I4B, "from: other value")
END BLOCK

! Test 3: all the parts in one call
BLOCK
  CALL OK(ALL(GetNNZ(obj=csr, from=[" "]) .EQ. [2, 1, 3]), "parts")
END BLOCK

! Test 4: an object that is not initiated
BLOCK
  TYPE(CSRSparsity_) :: empty
  CALL OK(GetNNZ(empty) .EQ. 0_I4B, "not initiated: total")
  CALL OK(GetNNZ(obj=empty, from="U") .EQ. 0_I4B, "not initiated: from")
  CALL OK(ALL(GetNNZ(obj=empty, from=[" "]) .EQ. 0), "not initiated: parts")
END BLOCK

CALL DEALLOCATE (csr)

END PROGRAM main
