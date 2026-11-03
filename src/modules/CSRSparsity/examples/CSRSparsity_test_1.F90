!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: SetSparsity(obj, row, col) followed by SetSparsity(obj)
!
! Description:
!   Builds sparsity row by row with unsorted, repeated and empty col,
!   for a scalar DOF and for a 2-component DOF in FMT_NODES and FMT_DOF,
!   and checks IA, JA and nnz after the final SetSparsity call.

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
USE GlobalData, ONLY: FMT_DOF
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: empty(0)

! Test 1: scalar DOF, unsorted and repeated col, one empty row
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[3_I4B, 1_I4B, 1_I4B])
  CALL SetSparsity(obj=csr, row=2_I4B, col=[2_I4B, 4_I4B])
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 3_I4B])
  CALL SetSparsity(obj=csr, row=3_I4B, col=empty)
  CALL SetSparsity(obj=csr, row=4_I4B, col=[4_I4B, 1_I4B, 4_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 7_I4B, "scalar: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 4, 6, 6, 8]), "scalar: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 2, 4, 1, 4]), "scalar: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: 3 nodes, 2 components, FMT_NODES
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: dof
  CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_NODES)
  CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B])
  CALL SetSparsity(obj=csr, row=3_I4B, col=[3_I4B])
  CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 3_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 16_I4B, "FMT_NODES: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 7, 13, 13, 13, 15, 17]), "FMT_NODES: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, &
                           5, 6, 5, 6]), "FMT_NODES: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (dof)
END BLOCK

! Test 3: 3 nodes, 2 components, FMT_DOF
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: dof
  CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B])
  CALL SetSparsity(obj=csr, row=3_I4B, col=[3_I4B])
  CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 3_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 16_I4B, "FMT_DOF: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 7, 7, 9, 15, 15, 17]), "FMT_DOF: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 4, 5, 6, 3, 6, &
                           1, 2, 3, 4, 5, 6, 3, 6]), "FMT_DOF: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (dof)
END BLOCK

END PROGRAM main
! ok 1 - scalar: nnz
! ok 2 - scalar: IA
! ok 3 - scalar: JA
! ok 4 - FMT_NODES: nnz
! ok 5 - FMT_NODES: IA
! ok 6 - FMT_NODES: JA
! ok 7 - FMT_DOF: nnz
! ok 8 - FMT_DOF: IA
! ok 9 - FMT_DOF: JA
