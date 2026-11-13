!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: SetSparsity with different DOF for rows and columns
!
! Description:
!   Uses a different DOF for rows and columns, so that the matrix is
!   rectangular. Covers scalar rows with 2-component columns and the
!   reverse, FMT_NODES rows with FMT_DOF columns and the reverse, a
!   block with SetSparsity(obj, row, col, ivar, jvar), and graphs on a
!   2-component DOF in FMT_DOF. Checks IA, JA and nnz after the final
!   SetSparsity call.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE DOF_Method, ONLY: DEALLOCATE
USE DOF_Method, ONLY: Initiate
USE GlobalData, ONLY: FMT_DOF
USE GlobalData, ONLY: FMT_NODES
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: scalar rows, 2-component columns, FMT_NODES
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: idof, jdof
  CALL Initiate(obj=idof, tNodes=[2_I4B], names=["P"], &
                spacecompo=[1_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=jdof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=csr, ncol=6_I4B, nrow=2_I4B, idof=idof, jdof=jdof)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[3_I4B, 1_I4B])
  CALL SetSparsity(obj=csr, row=2_I4B, col=[2_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 6_I4B, "scalar rows: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 5, 7]), "scalar rows: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 5, 6, 3, 4]), "scalar rows: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (idof)
  CALL DEALLOCATE (jdof)
END BLOCK

! Test 2: 2-component rows, scalar columns, FMT_NODES
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: idof, jdof
  CALL Initiate(obj=idof, tNodes=[2_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=jdof, tNodes=[3_I4B], names=["P"], &
                spacecompo=[1_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=4_I4B, idof=idof, jdof=jdof)
  CALL SetSparsity(obj=csr, row=2_I4B, col=[3_I4B, 1_I4B])
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 6_I4B, "scalar columns: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 2, 3, 5, 7]), "scalar columns: IA")
  CALL OK(ALL(csr%JA .EQ. [2, 2, 1, 3, 1, 3]), "scalar columns: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (idof)
  CALL DEALLOCATE (jdof)
END BLOCK

! Test 3: rows in FMT_NODES, columns in FMT_DOF
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: idof, jdof
  CALL Initiate(obj=idof, tNodes=[2_I4B], names=["P"], &
                spacecompo=[1_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=jdof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr, ncol=6_I4B, nrow=2_I4B, idof=idof, jdof=jdof)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B])
  CALL SetSparsity(obj=csr, row=2_I4B, col=[3_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 6_I4B, "FMT_DOF columns: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 5, 7]), "FMT_DOF columns: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 4, 5, 3, 6]), "FMT_DOF columns: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (idof)
  CALL DEALLOCATE (jdof)
END BLOCK

! Test 4: rows in FMT_DOF, columns in FMT_NODES
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: idof, jdof
  CALL Initiate(obj=idof, tNodes=[2_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=jdof, tNodes=[3_I4B], names=["P"], &
                spacecompo=[1_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=4_I4B, idof=idof, jdof=jdof)
  ! node 1 gives rows 1 and 3
  CALL SetSparsity(obj=csr, row=1_I4B, col=[3_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 2_I4B, "FMT_DOF rows: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 2, 2, 3, 3]), "FMT_DOF rows: IA")
  CALL OK(ALL(csr%JA .EQ. [3, 3]), "FMT_DOF rows: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (idof)
  CALL DEALLOCATE (jdof)
END BLOCK

! Test 5: rectangular block, one row variable and two column variables
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: idof, jdof
  CALL Initiate(obj=idof, tNodes=[2_I4B], names=["Q"], &
                spacecompo=[1_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=jdof, tNodes=[3_I4B, 2_I4B], names=["V", "P"], &
                spacecompo=[2_I4B, 1_I4B], timecompo=[1_I4B, 1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr, ncol=8_I4B, nrow=2_I4B, idof=idof, jdof=jdof)
  ! Q node 1 with P node 2: column 8
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B], ivar=1_I4B, &
                   jvar=2_I4B)
  ! Q node 2 with V node 3: columns 3 and 6
  CALL SetSparsity(obj=csr, row=2_I4B, col=[3_I4B], ivar=1_I4B, &
                   jvar=1_I4B)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 3_I4B, "rectangular block: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 2, 4]), "rectangular block: IA")
  CALL OK(ALL(csr%JA .EQ. [8, 3, 6]), "rectangular block: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (idof)
  CALL DEALLOCATE (jdof)
END BLOCK

! Test 6: integer and logical graph on 2 components, FMT_DOF
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  TYPE(DOF_) :: dof
  INTEGER(I4B) :: igraph(3, 3)
  LOGICAL(LGT) :: lgraph(3, 3)
  lgraph = .FALSE.
  lgraph(1, [1, 2, 3]) = .TRUE.
  lgraph(3, 3) = .TRUE.
  igraph = MERGE(1, 0, lgraph)
  CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr1, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL Initiate(obj=csr2, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr1, graph=igraph)
  CALL SetSparsity(obj=csr2, graph=lgraph)
  CALL SetSparsity(csr1)
  CALL SetSparsity(csr2)
  CALL OK(csr1%nnz .EQ. 16_I4B, "integer graph, FMT_DOF: nnz")
  CALL OK(ALL(csr1%IA .EQ. [1, 7, 7, 9, 15, 15, 17]), &
          "integer graph, FMT_DOF: IA")
  CALL OK(ALL(csr1%JA .EQ. [1, 2, 3, 4, 5, 6, 3, 6, &
                            1, 2, 3, 4, 5, 6, 3, 6]), &
          "integer graph, FMT_DOF: JA")
  CALL OK(csr2%nnz .EQ. csr1%nnz, "logical graph, FMT_DOF: nnz")
  CALL OK(ALL(csr2%IA .EQ. csr1%IA), "logical graph, FMT_DOF: IA")
  CALL OK(ALL(csr2%JA .EQ. csr1%JA), "logical graph, FMT_DOF: JA")
  CALL DEALLOCATE (csr1)
  CALL DEALLOCATE (csr2)
  CALL DEALLOCATE (dof)
END BLOCK

END PROGRAM main
