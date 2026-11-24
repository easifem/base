!> author: Vikas Sharma, Ph. D.
! date: 2026-11-24
! summary: Space-time matrix divergence when nips differs in time
!
! `obj(1)` has 2 quadrature points and `obj(2)` has 3. The result has
! shape `[nrow, 3, 2]`. The entry for time point 1 and quadrature
! point 3 does not exist, so it must be zero.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetDivergence
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(STElemShapeData_) :: stelemsd(2)
! nodal values val(nrow, nsd, nns, nnt) = 1
REAL(DFP) :: mst(1, 1, 2, 2)
INTEGER(I4B) :: ipt

! dNTdXt = 1, so the divergence is the sum of all nodal values: 4
CALL ALLOCATE (stelemsd(1), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
CALL ALLOCATE (stelemsd(2), nsd=1, xidim=1, nns=2, nips=3, nnt=2)
DO ipt = 1, 2
  stelemsd(ipt)%dNTdXt = 1.0_DFP
END DO
mst = 1.0_DFP

! Test 1: missing quadrature points are zero
BLOCK
  TYPE(FEVariable_) :: d
  REAL(DFP), ALLOCATABLE :: junk(:)

  ! leave non-zero values in freed heap memory of the result size
  ALLOCATE (junk(6))
  junk = 99.0_DFP
  DEALLOCATE (junk)

  CALL GetDivergence(obj=stelemsd, val=NodalVariable(mst, Matrix, &
                                                     SpaceTime), ans=d)
  CALL OK(ALL(d%s(1:3) .EQ. [1, 3, 2]), "shape [nrow, nips, nipt]")
  CALL OK(ALL(ABS(d%val(1:2) - 4.0_DFP) .LT. tol), &
          "time point 1: computed values")
  CALL OK(d%val(3) .EQ. 0.0_DFP, "time point 1: missing point is zero")
  CALL OK(ALL(ABS(d%val(4:6) - 4.0_DFP) .LT. tol), &
          "time point 2: computed values")
END BLOCK

END PROGRAM main
