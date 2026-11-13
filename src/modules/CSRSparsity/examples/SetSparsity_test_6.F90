!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: SetSparsity(obj, row(:), col(:)) with IntVector_ columns
!
! Description:
!   Sets the sparsity of several rows in one call, with unsorted,
!   repeated and empty column lists and repeated rows, for a scalar DOF
!   and a 2-component DOF in FMT_NODES. Checks IA, JA and nnz after the
!   final SetSparsity call, that the result does not depend on the order
!   of the rows, and that it equals one SetSparsity(obj, row, col) call
!   per row (FMT_DOF).

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: IntVector_
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE DOF_Method, ONLY: Initiate
USE DOF_Method, ONLY: DEALLOCATE
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: FMT_NODES
USE GlobalData, ONLY: FMT_DOF
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: scalar DOF, repeated row, unsorted and empty col
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(IntVector_) :: col(5)
  col(1) = [3_I4B, 1_I4B, 1_I4B]
  col(2) = [2_I4B, 4_I4B]
  col(3) = [2_I4B, 3_I4B]
  ALLOCATE (col(4)%val(0))
  col(5) = [4_I4B, 1_I4B, 4_I4B]
  CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
  CALL SetSparsity(obj=csr, row=[1_I4B, 2_I4B, 1_I4B, 3_I4B, 4_I4B], &
                   col=col)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 7_I4B, "scalar: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 4, 6, 6, 8]), "scalar: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 2, 4, 1, 4]), "scalar: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: same rows in reverse order give the same pattern
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(IntVector_) :: col(5)
  col(5) = [3_I4B, 1_I4B, 1_I4B]
  col(4) = [2_I4B, 4_I4B]
  col(3) = [2_I4B, 3_I4B]
  ALLOCATE (col(2)%val(0))
  col(1) = [4_I4B, 1_I4B, 4_I4B]
  CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
  CALL SetSparsity(obj=csr, row=[4_I4B, 3_I4B, 1_I4B, 2_I4B, 1_I4B], &
                   col=col)
  CALL SetSparsity(csr)
  CALL OK(ALL(csr%IA .EQ. [1, 4, 6, 6, 8]), "reversed rows: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 2, 4, 1, 4]), "reversed rows: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 3: 3 nodes, 2 components, FMT_NODES
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: dof
  TYPE(IntVector_) :: col(3)
  col(1) = [2_I4B, 1_I4B]
  col(2) = [3_I4B]
  col(3) = [1_I4B, 3_I4B]
  CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr, row=[1_I4B, 3_I4B, 1_I4B], col=col)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 16_I4B, "FMT_NODES: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 7, 13, 13, 13, 15, 17]), "FMT_NODES: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, &
                           5, 6, 5, 6]), "FMT_NODES: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (dof)
END BLOCK

! Test 4: FMT_DOF, same result as one call per row
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  TYPE(DOF_) :: dof
  TYPE(IntVector_) :: col(3)
  INTEGER(I4B) :: row(3), ii
  row = [1_I4B, 3_I4B, 1_I4B]
  col(1) = [2_I4B, 1_I4B]
  col(2) = [3_I4B]
  col(3) = [1_I4B, 3_I4B]
  CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr1, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL Initiate(obj=csr2, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr1, row=row, col=col)
  DO ii = 1, SIZE(row)
    CALL SetSparsity(obj=csr2, row=row(ii), col=col(ii)%val)
  END DO
  CALL SetSparsity(csr1)
  CALL SetSparsity(csr2)
  CALL OK(csr1%nnz .EQ. csr2%nnz, "FMT_DOF: nnz equals row-wise")
  CALL OK(ALL(csr1%IA .EQ. csr2%IA), "FMT_DOF: IA equals row-wise")
  CALL OK(ALL(csr1%JA .EQ. csr2%JA), "FMT_DOF: JA equals row-wise")
  CALL DEALLOCATE (csr1)
  CALL DEALLOCATE (csr2)
  CALL DEALLOCATE (dof)
END BLOCK

END PROGRAM main
! ok 1 - scalar: nnz
! ok 2 - scalar: IA
! ok 3 - scalar: JA
! ok 4 - reversed rows: IA
! ok 5 - reversed rows: JA
! ok 6 - FMT_NODES: nnz
! ok 7 - FMT_NODES: IA
! ok 8 - FMT_NODES: JA
! ok 9 - FMT_DOF: nnz equals row-wise
! ok 10 - FMT_DOF: IA equals row-wise
! ok 11 - FMT_DOF: JA equals row-wise
