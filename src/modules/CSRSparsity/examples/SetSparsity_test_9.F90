!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: SetSparsity(obj, row(:), col(:), ivar, jvar) for block
!   sparsity
!
! Description:
!   Uses a DOF in FMT_DOF with two physical variables: V (3 nodes,
!   2 components) and P (2 nodes, 1 component), so the matrix has 8 rows
!   and columns ordered as V1(1:3), V2(4:6), P(7:8). Sets several rows
!   per call for the blocks (V,P), (P,V), (V,V) and an empty (P,P) call,
!   with repeated rows and columns, then checks IA, JA and nnz after the
!   final SetSparsity call. Also checks that the result equals one
!   SetSparsity(obj, row, col, ivar, jvar) call per row.

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
USE GlobalData, ONLY: FMT_DOF
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(DOF_) :: dof
TYPE(IntVector_) :: colVP(3), colPV(2), colVV(1), colPP(1)
INTEGER(I4B) :: rowVP(3), rowPV(2), rowVV(1), rowPP(1)

CALL Initiate(obj=dof, tNodes=[3_I4B, 2_I4B], names=["V", "P"], &
              spacecompo=[2_I4B, 1_I4B], timecompo=[1_I4B, 1_I4B], &
              storagefmt=FMT_DOF)

! block (V,P): V nodes 1, 3, 1 with P nodes
rowVP = [1_I4B, 3_I4B, 1_I4B]
colVP(1) = [2_I4B]
colVP(2) = [1_I4B, 2_I4B]
colVP(3) = [1_I4B]
! block (P,V): P node 2 (twice) with V nodes
rowPV = [2_I4B, 2_I4B]
colPV(1) = [3_I4B]
colPV(2) = [3_I4B, 1_I4B]
! block (V,V): V node 1 with itself
rowVV = [1_I4B]
colVV(1) = [1_I4B]
! block (P,P): P node 1 with no columns
rowPP = [1_I4B]
ALLOCATE (colPP(1)%val(0))

! Test 1: expected IA, JA and nnz
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=8_I4B, nrow=8_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr, row=rowVP, col=colVP, ivar=1_I4B, &
                   jvar=2_I4B)
  CALL SetSparsity(obj=csr, row=rowPV, col=colPV, ivar=2_I4B, &
                   jvar=1_I4B)
  CALL SetSparsity(obj=csr, row=rowVV, col=colVV, ivar=1_I4B, &
                   jvar=1_I4B)
  CALL SetSparsity(obj=csr, row=rowPP, col=colPP, ivar=2_I4B, &
                   jvar=2_I4B)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 16_I4B, "several rows: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 5, 5, 7, 11, 11, 13, 13, 17]), &
          "several rows: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 4, 7, 8, 7, 8, 1, 4, 7, 8, 7, 8, &
                           1, 3, 4, 6]), "several rows: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: same result as one SetSparsity(row, col, ivar, jvar) per row
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  INTEGER(I4B) :: ii
  CALL Initiate(obj=csr1, ncol=8_I4B, nrow=8_I4B, idof=dof, jdof=dof)
  CALL Initiate(obj=csr2, ncol=8_I4B, nrow=8_I4B, idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr1, row=rowVP, col=colVP, ivar=1_I4B, &
                   jvar=2_I4B)
  CALL SetSparsity(obj=csr1, row=rowPV, col=colPV, ivar=2_I4B, &
                   jvar=1_I4B)
  DO ii = 1, SIZE(rowVP)
    CALL SetSparsity(obj=csr2, row=rowVP(ii), col=colVP(ii)%val, &
                     ivar=1_I4B, jvar=2_I4B)
  END DO
  DO ii = 1, SIZE(rowPV)
    CALL SetSparsity(obj=csr2, row=rowPV(ii), col=colPV(ii)%val, &
                     ivar=2_I4B, jvar=1_I4B)
  END DO
  CALL SetSparsity(csr1)
  CALL SetSparsity(csr2)
  CALL OK(csr1%nnz .EQ. csr2%nnz, "nnz equals row-wise")
  CALL OK(ALL(csr1%IA .EQ. csr2%IA), "IA equals row-wise")
  CALL OK(ALL(csr1%JA .EQ. csr2%JA), "JA equals row-wise")
  CALL DEALLOCATE (csr1)
  CALL DEALLOCATE (csr2)
END BLOCK

CALL DEALLOCATE (dof)

END PROGRAM main
! ok 1 - several rows: nnz
! ok 2 - several rows: IA
! ok 3 - several rows: JA
! ok 4 - nnz equals row-wise
! ok 5 - IA equals row-wise
! ok 6 - JA equals row-wise
