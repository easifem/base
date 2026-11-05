!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Accessors of a pattern, Shape, Size, GetColIndex, GetColNumber,
!   .startColumn., .endColumn., GetIA, GetJA, SetIA and SetJA
!
! Description:
!   Uses a 3 by 4 pattern with an empty second row
!
!     row 1: columns 2, 3
!     row 2: no columns
!     row 3: column  1

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: CsrShape => Shape
USE CSRSparsity_Method, ONLY: CsrSize => Size
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: GetColIndex
USE CSRSparsity_Method, ONLY: GetColNumber
USE CSRSparsity_Method, ONLY: GetIA
USE CSRSparsity_Method, ONLY: GetJA
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: OPERATOR(.endColumn.)
USE CSRSparsity_Method, ONLY: OPERATOR(.startColumn.)
USE CSRSparsity_Method, ONLY: SetIA
USE CSRSparsity_Method, ONLY: SetJA
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(CSRSparsity_) :: csr

CALL Initiate(obj=csr, IA=[1_I4B, 3_I4B, 3_I4B, 4_I4B], &
              JA=[2_I4B, 3_I4B, 1_I4B], ncol=4_I4B)

! Test 1: Shape and Size
BLOCK
  CALL OK(ALL(CsrShape(csr) .EQ. [3, 4]), "Shape")
  CALL OK(CsrSize(csr, 1_I4B) .EQ. 3_I4B, "Size: Dims=1")
  CALL OK(CsrSize(csr, 2_I4B) .EQ. 4_I4B, "Size: Dims=2")
  CALL OK(CsrSize(csr) .EQ. 12_I4B, "Size: no Dims")
END BLOCK

! Test 2: range of a row with columns
BLOCK
  CALL OK(ALL(GetColIndex(obj=csr, irow=1_I4B) .EQ. [1, 2]), &
          "GetColIndex: row 1")
  CALL OK((csr.startColumn.1_I4B) .EQ. 1_I4B, ".startColumn.: row 1")
  CALL OK((csr.endColumn.1_I4B) .EQ. 2_I4B, ".endColumn.: row 1")
END BLOCK

! Test 3: the end of an empty row comes before its start
BLOCK
  CALL OK(ALL(GetColIndex(obj=csr, irow=2_I4B) .EQ. [3, 2]), &
          "GetColIndex: empty row")
  CALL OK((csr.startColumn.2_I4B) .EQ. 3_I4B, ".startColumn.: empty row")
  CALL OK((csr.endColumn.2_I4B) .EQ. 2_I4B, ".endColumn.: empty row")
END BLOCK

! Test 4: entries of IA and JA
BLOCK
  CALL OK(GetIA(obj=csr, irow=4_I4B) .EQ. 4_I4B, "GetIA: last entry")
  CALL OK(GetJA(obj=csr, indx=3_I4B) .EQ. 1_I4B, "GetJA")
  CALL OK(GetColNumber(obj=csr, indx=2_I4B) .EQ. 3_I4B, "GetColNumber")
END BLOCK

! Test 5: SetIA and SetJA
BLOCK
  CALL SetIA(obj=csr, irow=2_I4B, VALUE=2_I4B)
  CALL OK(GetIA(obj=csr, irow=2_I4B) .EQ. 2_I4B, "SetIA")
  CALL SetJA(obj=csr, indx=1_I4B, VALUE=4_I4B)
  CALL OK(GetJA(obj=csr, indx=1_I4B) .EQ. 4_I4B, "SetJA")
END BLOCK

CALL DEALLOCATE (csr)

END PROGRAM main
