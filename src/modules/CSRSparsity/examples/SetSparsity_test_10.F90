!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: SetSparsity(obj, graph) with an integer graph
!
! Description:
!   graph(i, j) .NE. 0 connects row node i with column node j. Checks
!   IA, JA and nnz after the final SetSparsity call for a square scalar
!   graph (with values other than 1, and an empty row), a rectangular
!   graph, a 2-component DOF in FMT_NODES, and an all-zero graph.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE DOF_Method, ONLY: Initiate
USE DOF_Method, ONLY: DEALLOCATE
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: FMT_NODES
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: square scalar graph, any nonzero value counts, row 3 empty
BLOCK
  TYPE(CSRSparsity_) :: csr
  INTEGER(I4B) :: graph(4, 4)
  graph = 0
  graph(1, [1, 2, 3]) = [1, 2, -1]
  graph(2, [2, 4]) = [5, 1]
  graph(4, [1, 4]) = [1, 1]
  CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
  CALL SetSparsity(obj=csr, graph=graph)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 7_I4B, "square: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 4, 6, 6, 8]), "square: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 2, 4, 1, 4]), "square: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: rectangular graph, 2 rows and 3 columns
BLOCK
  TYPE(CSRSparsity_) :: csr
  INTEGER(I4B) :: graph(2, 3)
  graph = 0
  graph(1, [2, 3]) = 1
  graph(2, 1) = 1
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=2_I4B)
  CALL SetSparsity(obj=csr, graph=graph)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 3_I4B, "rectangular: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 3, 4]), "rectangular: IA")
  CALL OK(ALL(csr%JA .EQ. [2, 3, 1]), "rectangular: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 3: node graph with 3 nodes, 2 components, FMT_NODES
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: dof
  INTEGER(I4B) :: graph(3, 3)
  graph = 0
  graph(1, [1, 2, 3]) = 1
  graph(3, 3) = 1
  CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr, graph=graph)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 16_I4B, "FMT_NODES: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 7, 13, 13, 13, 15, 17]), "FMT_NODES: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, &
                           5, 6, 5, 6]), "FMT_NODES: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (dof)
END BLOCK

! Test 4: all-zero graph gives an empty pattern
BLOCK
  TYPE(CSRSparsity_) :: csr
  INTEGER(I4B) :: graph(3, 3)
  graph = 0
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr, graph=graph)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 0_I4B, "all zero: nnz")
  CALL OK(ALL(csr%IA .EQ. 1), "all zero: IA")
  CALL OK(SIZE(csr%JA) .EQ. 0, "all zero: JA is empty")
  CALL DEALLOCATE (csr)
END BLOCK

END PROGRAM main
! ok 1 - square: nnz
! ok 2 - square: IA
! ok 3 - square: JA
! ok 4 - rectangular: nnz
! ok 5 - rectangular: IA
! ok 6 - rectangular: JA
! ok 7 - FMT_NODES: nnz
! ok 8 - FMT_NODES: IA
! ok 9 - FMT_NODES: JA
! ok 10 - all zero: nnz
! ok 11 - all zero: IA
! ok 12 - all zero: JA is empty
