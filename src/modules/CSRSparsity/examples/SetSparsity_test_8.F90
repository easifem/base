!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: SetSparsity(obj, row, col, ivar, jvar) for block sparsity
!
! Description:
!   Uses a DOF in FMT_DOF with two physical variables: V (3 nodes,
!   2 components) and P (2 nodes, 1 component), so the matrix has 8 rows
!   and columns ordered as V1(1:3), V2(4:6), P(7:8). Fills the blocks
!   (V,V), (V,P), (P,V) and an empty (P,P) call, then checks IA, JA and
!   nnz after the final SetSparsity call. Also checks that for a single
!   variable, ivar = jvar = 1 gives the same pattern as
!   SetSparsity(obj, row, col).

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE DOF_Method, ONLY: Initiate
USE DOF_Method, ONLY: DEALLOCATE
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: FMT_DOF
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: empty(0)

! Test 1: two variables, all four blocks
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: dof
  CALL Initiate(obj=dof, tNodes=[3_I4B, 2_I4B], names=["V", "P"], &
                spacecompo=[2_I4B, 1_I4B], timecompo=[1_I4B, 1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr, ncol=8_I4B, nrow=8_I4B, idof=dof, jdof=dof)
  ! V node 1 with P nodes 2, 1: rows 1, 4 and columns 8, 7
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B], &
                   ivar=1_I4B, jvar=2_I4B)
  ! P node 2 with V node 3: row 8 and columns 3, 6
  CALL SetSparsity(obj=csr, row=2_I4B, col=[3_I4B], &
                   ivar=2_I4B, jvar=1_I4B)
  ! V node 1 with V node 1 (repeated): rows 1, 4 and columns 1, 4
  CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 1_I4B], &
                   ivar=1_I4B, jvar=1_I4B)
  ! P node 1 with no columns: nothing added
  CALL SetSparsity(obj=csr, row=1_I4B, col=empty, &
                   ivar=2_I4B, jvar=2_I4B)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 10_I4B, "two variables: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 5, 5, 5, 9, 9, 9, 9, 11]), &
          "two variables: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 4, 7, 8, 1, 4, 7, 8, 3, 6]), &
          "two variables: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (dof)
END BLOCK

! Test 2: one variable, ivar = jvar = 1 equals SetSparsity(obj, row, col)
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  TYPE(DOF_) :: dof
  CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr1, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL Initiate(obj=csr2, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr1, row=1_I4B, col=[2_I4B, 1_I4B], &
                   ivar=1_I4B, jvar=1_I4B)
  CALL SetSparsity(obj=csr1, row=3_I4B, col=[3_I4B], &
                   ivar=1_I4B, jvar=1_I4B)
  CALL SetSparsity(obj=csr2, row=1_I4B, col=[2_I4B, 1_I4B])
  CALL SetSparsity(obj=csr2, row=3_I4B, col=[3_I4B])
  CALL SetSparsity(csr1)
  CALL SetSparsity(csr2)
  CALL OK(csr1%nnz .EQ. csr2%nnz, "one variable: nnz equals row-wise")
  CALL OK(ALL(csr1%IA .EQ. csr2%IA), "one variable: IA equals row-wise")
  CALL OK(ALL(csr1%JA .EQ. csr2%JA), "one variable: JA equals row-wise")
  CALL DEALLOCATE (csr1)
  CALL DEALLOCATE (csr2)
  CALL DEALLOCATE (dof)
END BLOCK

END PROGRAM main
! ok 1 - two variables: nnz
! ok 2 - two variables: IA
! ok 3 - two variables: JA
! ok 4 - one variable: nnz equals row-wise
! ok 5 - one variable: IA equals row-wise
! ok 6 - one variable: JA equals row-wise
