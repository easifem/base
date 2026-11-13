!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Initiate(obj, ncol, nrow, idof, jdof, nnz) and the matching
!   CSRSparsity and CSRSparsityPointer constructors
!
! Description:
!   Checks the state of the object after Initiate with the internal
!   scalar DOF, with given DOF objects and with nnz, that a second
!   Initiate replaces the first one, and that both constructors give the
!   same object.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE CSRSparsity_Method, ONLY: ASSIGNMENT(=)
USE CSRSparsity_Method, ONLY: CSRSparsity
USE CSRSparsity_Method, ONLY: CSRSparsityPointer
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: Initiate
USE DOF_Method, ONLY: DEALLOCATE
USE DOF_Method, ONLY: Initiate
USE GlobalData, ONLY: FMT_DOF
USE GlobalData, ONLY: FMT_NODES
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: internal scalar DOF, rectangular shape
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=4_I4B, nrow=3_I4B)
  CALL OK(csr%isInitiated, "default: isInitiated")
  CALL OK(.NOT. csr%isSparsityLock, "default: not locked")
  CALL OK(csr%nrow .EQ. 3_I4B, "default: nrow")
  CALL OK(csr%ncol .EQ. 4_I4B, "default: ncol")
  CALL OK(csr%nnz .EQ. 0_I4B, "default: nnz")
  CALL OK(SIZE(csr%IA) .EQ. 4, "default: SIZE(IA)")
  CALL OK(SIZE(csr%idiag) .EQ. 3, "default: SIZE(idiag)")
  CALL OK(.NOT. ALLOCATED(csr%JA), "default: JA is not allocated")
  CALL OK(csr%idof%storageFMT .EQ. FMT_NODES, "default: idof format")
  CALL OK(csr%jdof%storageFMT .EQ. FMT_NODES, "default: jdof format")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 2: nnz allocates JA
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B, nnz=5_I4B)
  CALL OK(csr%nnz .EQ. 5_I4B, "nnz: nnz")
  CALL OK(SIZE(csr%JA) .EQ. 5, "nnz: SIZE(JA)")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 3: given DOF objects are copied
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(DOF_) :: dof
  CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
                spacecompo=[2_I4B], timecompo=[1_I4B], &
                storagefmt=FMT_DOF)
  CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
  CALL DEALLOCATE (dof)
  CALL OK(csr%idof%storageFMT .EQ. FMT_DOF, "dof: idof format")
  CALL OK(csr%jdof%storageFMT .EQ. FMT_DOF, "dof: jdof format")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 4: a second Initiate replaces the first one
BLOCK
  TYPE(CSRSparsity_) :: csr
  CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B, nnz=5_I4B)
  CALL Initiate(obj=csr, ncol=2_I4B, nrow=2_I4B)
  CALL OK(csr%nrow .EQ. 2_I4B, "again: nrow")
  CALL OK(csr%nnz .EQ. 0_I4B, "again: nnz")
  CALL OK(SIZE(csr%IA) .EQ. 3, "again: SIZE(IA)")
  CALL OK(.NOT. ALLOCATED(csr%JA), "again: JA is not allocated")
  CALL DEALLOCATE (csr)
END BLOCK

! Test 5: constructors
BLOCK
  TYPE(CSRSparsity_) :: csr
  TYPE(CSRSparsity_), POINTER :: ptr
  csr = CSRSparsity(nrow=3_I4B, ncol=4_I4B)
  CALL OK(csr%isInitiated, "CSRSparsity: isInitiated")
  CALL OK(csr%nrow .EQ. 3_I4B, "CSRSparsity: nrow")
  CALL OK(csr%ncol .EQ. 4_I4B, "CSRSparsity: ncol")
  ptr => CSRSparsityPointer(nrow=3_I4B, ncol=4_I4B)
  CALL OK(ptr%isInitiated, "CSRSparsityPointer: isInitiated")
  CALL OK(ptr%nrow .EQ. 3_I4B, "CSRSparsityPointer: nrow")
  CALL OK(ptr%ncol .EQ. 4_I4B, "CSRSparsityPointer: ncol")
  CALL DEALLOCATE (csr)
  CALL DEALLOCATE (ptr)
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
