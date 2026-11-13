!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: SetSparsity(obj, graph) with a logical graph
!
! Description:
!   graph(i, j) = .TRUE. connects row node i with column node j. Checks
!   IA, JA and nnz after the final SetSparsity call for a square scalar
!   graph with an empty row, a rectangular graph, a 2-component DOF in
!   FMT_NODES, and an all-false graph. Also checks that the logical
!   graph gives the same pattern as the matching integer graph.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE DOF_Method, ONLY: Initiate
USE DOF_Method, ONLY: DEALLOCATE
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: FMT_NODES
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: square scalar graph, row 3 empty
BLOCK
  TYPE(CSRSparsity_) :: csr
  LOGICAL(LGT) :: graph(4, 4)
  graph = .FALSE.
  graph(1, [1, 2, 3]) = .TRUE.
  graph(2, [2, 4]) = .TRUE.
  graph(4, [1, 4]) = .TRUE.
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
  LOGICAL(LGT) :: graph(2, 3)
  graph = .FALSE.
  graph(1, [2, 3]) = .TRUE.
  graph(2, 1) = .TRUE.
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
  LOGICAL(LGT) :: graph(3, 3)
  graph = .FALSE.
  graph(1, [1, 2, 3]) = .TRUE.
  graph(3, 3) = .TRUE.
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

! Test 4: all-false graph gives an empty pattern
BLOCK
  TYPE(CSRSparsity_) :: csr
  LOGICAL(LGT) :: graph(3, 3)
  graph = .FALSE.
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr, graph=graph)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 0_I4B, "all false: nnz")
  CALL OK(ALL(csr%IA .EQ. 1), "all false: IA")
  CALL OK(SIZE(csr%JA) .EQ. 0, "all false: JA is empty")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 5: logical graph equals the matching integer graph
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  LOGICAL(LGT) :: lgraph(5, 5)
  INTEGER(I4B) :: igraph(5, 5), ii
  lgraph = .FALSE.
  DO ii = 1, 5
    lgraph(ii, ii) = .TRUE.
    lgraph(ii, MOD(ii, 5) + 1) = .TRUE.
  END DO
  lgraph(3, 1) = .TRUE.
  igraph = MERGE(1, 0, lgraph)
  CALL Initiate(obj=csr1, ncol=5_I4B, nrow=5_I4B)
  CALL Initiate(obj=csr2, ncol=5_I4B, nrow=5_I4B)
  CALL SetSparsity(obj=csr1, graph=lgraph)
  CALL SetSparsity(obj=csr2, graph=igraph)
  CALL SetSparsity(csr1)
  CALL SetSparsity(csr2)
  CALL OK(csr1%nnz .EQ. csr2%nnz, "nnz equals integer graph")
  CALL OK(ALL(csr1%IA .EQ. csr2%IA), "IA equals integer graph")
  CALL OK(ALL(csr1%JA .EQ. csr2%JA), "JA equals integer graph")
  CALL DEALLOCATE (csr1)
  CALL DEALLOCATE (csr2)
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
! ok 10 - all false: nnz
! ok 11 - all false: IA
! ok 12 - all false: JA is empty
! ok 13 - nnz equals integer graph
! ok 14 - IA equals integer graph
! ok 15 - JA equals integer graph
