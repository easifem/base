!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: SetSparsity with IntVector_ columns and their active size
!
! Description:
!   Builds col with the IntVector_ assignment, so that col(i)%tsize is
!   set. Checks SetSparsity(obj, row(:), col(:)) and
!   SetSparsity(obj, row(:), col(:), ivar, jvar): only
!   col(i)%val(1:col(i)%tsize) is used, and an empty col(i) adds nothing.
!   Also covers the block version with a rectangular matrix.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: IntVector_
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE DOF_Method, ONLY: DEALLOCATE
USE DOF_Method, ONLY: Initiate
USE GlobalData, ONLY: FMT_DOF
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: noval(0)

! Test 1: scalar DOF, repeated row, unsorted and repeated col
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(IntVector_) :: col(3)
  col(1) = [3_I4B, 1_I4B, 1_I4B]
  col(2) = [2_I4B, 4_I4B]
  col(3) = [2_I4B, 3_I4B]
  CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
  CALL SetSparsity(obj=csr, row=[1_I4B, 2_I4B, 1_I4B], col=col)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 5_I4B, "assigned col: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 4, 6, 6, 6]), "assigned col: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 2, 4]), "assigned col: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: entries of col beyond tsize are not used
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(IntVector_) :: col(2)
  col(1) = [3_I4B, 1_I4B, 4_I4B]
  col(1)%tsize = 2
  col(2) = [2_I4B, 4_I4B]
  col(2)%tsize = 1
  CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
  CALL SetSparsity(obj=csr, row=[1_I4B, 2_I4B], col=col)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 3_I4B, "col with spare capacity: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 3, 4, 4, 4]), "col with spare capacity: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 3, 2]), "col with spare capacity: JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 3: an empty col(i) adds nothing
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(IntVector_) :: col(2)
  col(1) = [2_I4B]
  col(2) = noval
  CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
  CALL SetSparsity(obj=csr, row=[1_I4B, 2_I4B], col=col)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 1_I4B, "empty col(i): nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 2, 2, 2, 2]), "empty col(i): IA")
  CALL OK(ALL(csr%JA .EQ. [2]), "empty col(i): JA")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 4: block (V,P), entries of col beyond tsize are not used
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: dof
  TYPE(IntVector_) :: col(2)
  col(1) = [2_I4B, 1_I4B]
  col(1)%tsize = 1
  col(2) = [1_I4B, 2_I4B]
  CALL Initiate(obj=dof, tNodes=[3_I4B, 2_I4B], names=["V", "P"], &
                spacecompo=[2_I4B, 1_I4B], timecompo=[1_I4B, 1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr, ncol=8_I4B, nrow=8_I4B, idof=dof, jdof=dof)
  ! V nodes 1 and 3 give rows 1, 4 and 3, 6; P nodes 1, 2 give columns 7, 8
  CALL SetSparsity(obj=csr, row=[1_I4B, 3_I4B], col=col, ivar=1_I4B, &
                   jvar=2_I4B)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 6_I4B, "block: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 2, 2, 4, 5, 5, 7, 7, 7]), "block: IA")
  CALL OK(ALL(csr%JA .EQ. [8, 7, 8, 8, 7, 8]), "block: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (dof)
END BLOCK

! Test 5: rectangular block, one row variable and two column variables
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: idof, jdof
  TYPE(IntVector_) :: col(2)
  col(1) = [2_I4B]
  col(2) = [3_I4B]
  CALL Initiate(obj=idof, tNodes=[2_I4B], names=["Q"], &
                spacecompo=[1_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=jdof, tNodes=[3_I4B, 2_I4B], names=["V", "P"], &
                spacecompo=[2_I4B, 1_I4B], timecompo=[1_I4B, 1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr, ncol=8_I4B, nrow=2_I4B, idof=idof, jdof=jdof)
  ! V node 2 gives columns 2, 5 and V node 3 gives columns 3, 6
  CALL SetSparsity(obj=csr, row=[1_I4B, 2_I4B], col=col, ivar=1_I4B, &
                   jvar=1_I4B)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 4_I4B, "rectangular block: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 3, 5]), "rectangular block: IA")
  CALL OK(ALL(csr%JA .EQ. [2, 5, 3, 6]), "rectangular block: JA")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (idof)
  CALL DEALLOCATE (jdof)
END BLOCK

END PROGRAM main
