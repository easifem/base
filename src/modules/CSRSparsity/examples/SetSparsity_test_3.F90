!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Building a sparsity pattern up to the final SetSparsity call
!
! Description:
!   Checks that nnz is an upper bound before the final SetSparsity call
!   and exact after it, that different SetSparsity methods can be mixed
!   on one object, that the final call locks the object and frees the
!   row storage, that a second final call changes nothing, that empty
!   input gives an empty pattern, and that an object initiated from IA
!   and JA keeps them.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeDOFOpt
USE CSRSparsity_Method, ONLY: CsrSparsity_Deallocate => DEALLOCATE
USE CSRSparsity_Method, ONLY: CsrSparsity_Initiate => Initiate
USE CSRSparsity_Method, ONLY: SetSparsity
USE DOF_Method, ONLY: Dof_Deallocate => DEALLOCATE
USE DOF_Method, ONLY: Dof_Initiate => Initiate
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: scalar DOF, repeats count in nnz until the final call
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL CsrSparsity_Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
  CALL OK(csr%nnz .EQ. 0_I4B, "scalar: nnz after Initiate")
  CALL SetSparsity(obj=csr, row=1_I4B, col=[3_I4B, 1_I4B, 1_I4B])
  CALL OK(csr%nnz .EQ. 3_I4B, "scalar: nnz after first call")
  CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 2_I4B])
  CALL OK(csr%nnz .EQ. 5_I4B, "scalar: nnz after second call")
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 3_I4B, "scalar: nnz after final call")
  CALL CsrSparsity_Deallocate(csr)
  CALL OK(math%yes, "Deallocating:")
END BLOCK

! Test 2: nnz counts each row and column component
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  TYPE(DOF_) :: dof
  TYPE(IntVector_) :: col(1)
  col(1) = [2_I4B, 1_I4B]
  CALL Dof_Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                    spacecompo=[2_I4B], timecompo=[1_I4B], &
                    storagefmt=TypeDOFOpt%storageFormatNodes)
  CALL CsrSparsity_Initiate(obj=csr1, ncol=6_I4B, nrow=6_I4B, &
                            idof=dof, jdof=dof)
  CALL CsrSparsity_Initiate(obj=csr2, ncol=6_I4B, nrow=6_I4B, &
                            idof=dof, jdof=dof)
  CALL SetSparsity(obj=csr1, row=1_I4B, col=col(1)%val)
  CALL SetSparsity(obj=csr2, row=[1_I4B], col=col)
  CALL OK(csr1%nnz .EQ. 8_I4B, "components: nnz, one row")
  CALL OK(csr2%nnz .EQ. 8_I4B, "components: nnz, several rows")
  CALL CsrSparsity_Deallocate(csr1)
  CALL CsrSparsity_Deallocate(csr2)
  CALL Dof_Deallocate(dof)
END BLOCK

! Test 3: different SetSparsity methods on one object
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(IntVector_) :: col(2)
  INTEGER(I4B) :: graph(3, 3)
  col(1) = [3_I4B, 2_I4B]
  col(2) = [1_I4B]
  graph = 0
  graph(1, 1) = 1
  graph(2, 2) = 1
  CALL CsrSparsity_Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B])
  CALL SetSparsity(obj=csr, row=[1_I4B, 3_I4B], col=col)
  CALL SetSparsity(obj=csr, graph=graph)
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 5_I4B, "mixed methods: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 4, 5, 6]), "mixed methods: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 2, 1]), "mixed methods: JA")
  CALL CsrSparsity_Deallocate(csr)
END BLOCK

! Test 4: a larger nnz given to Initiate is replaced by the exact value
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL CsrSparsity_Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B, nnz=10_I4B)
  CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 1_I4B, "nnz from Initiate: nnz")
  CALL OK(SIZE(csr%JA) .EQ. 1, "nnz from Initiate: SIZE(JA)")
  CALL OK(ALL(csr%IA .EQ. [1, 2, 2, 2]), "nnz from Initiate: IA")
  CALL OK(csr%JA(1) .EQ. 2_I4B, "nnz from Initiate: JA")
  CALL CsrSparsity_Deallocate(csr)
END BLOCK

! Test 5: the final call locks the object, a second one changes nothing
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL CsrSparsity_Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr, row=2_I4B, col=[3_I4B, 1_I4B])
  CALL OK(.NOT. csr%isSparsityLock, "lock: not locked before final call")
  CALL SetSparsity(csr)
  CALL OK(csr%isSparsityLock, "lock: locked after final call")
  CALL OK(.NOT. ALLOCATED(csr%row), "lock: row storage is freed")
  ! this call prints a warning and does nothing
  ! warning is suppress in release mode.
  CALL SetSparsity(csr)
  CALL OK(csr%nnz .EQ. 2_I4B, "lock: nnz after second final call")
  CALL OK(ALL(csr%IA .EQ. [1, 1, 3, 3]), "lock: IA after second final call")
  CALL OK(ALL(csr%JA .EQ. [1, 3]), "lock: JA after second final call")
  CALL CsrSparsity_Deallocate(csr)
END BLOCK

! Test 6: only empty input gives an empty pattern
BLOCK
  TYPE(CSRSparsity_) :: csr1, csr2
  TYPE(IntVector_) :: nocol(0)
  INTEGER(I4B) :: empty(0)
  CALL CsrSparsity_Initiate(obj=csr1, ncol=3_I4B, nrow=3_I4B)
  CALL CsrSparsity_Initiate(obj=csr2, ncol=3_I4B, nrow=3_I4B)
  CALL SetSparsity(obj=csr1, row=1_I4B, col=empty)
  CALL SetSparsity(obj=csr2, row=empty, col=nocol)
  CALL SetSparsity(csr1)
  CALL SetSparsity(csr2)
  CALL OK(csr1%nnz .EQ. 0_I4B, "empty col: nnz")
  CALL OK(ALL(csr1%IA .EQ. 1), "empty col: IA")
  CALL OK(SIZE(csr1%JA) .EQ. 0, "empty col: JA is empty")
  CALL OK(csr2%nnz .EQ. 0_I4B, "empty row and col: nnz")
  CALL OK(ALL(csr2%IA .EQ. 1), "empty row and col: IA")
  CALL OK(SIZE(csr2%JA) .EQ. 0, "empty row and col: JA is empty")
  CALL CsrSparsity_Deallocate(csr1)
  CALL CsrSparsity_Deallocate(csr2)
END BLOCK

! Test 7: an object initiated from IA and JA keeps them
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL CsrSparsity_Initiate(obj=csr, IA=[1_I4B, 3_I4B, 4_I4B], &
                            JA=[1_I4B, 2_I4B, 2_I4B])
  CALL SetSparsity(csr)
  CALL OK(csr%isSparsityLock, "from IA and JA: locked")
  CALL OK(csr%nnz .EQ. 3_I4B, "from IA and JA: nnz")
  CALL OK(ALL(csr%IA .EQ. [1, 3, 4]), "from IA and JA: IA")
  CALL OK(ALL(csr%JA .EQ. [1, 2, 2]), "from IA and JA: JA")
  CALL CsrSparsity_Deallocate(csr)
END BLOCK

END PROGRAM main
