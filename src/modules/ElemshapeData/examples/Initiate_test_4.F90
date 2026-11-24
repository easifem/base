!> author: Vikas Sharma, Ph. D.
! date: 2026-11-24
! summary: Copy ElemShapeData_ keeps NLaplacian

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: ASSIGNMENT(=)
USE ElemShapeData_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(ElemShapeData_) :: src

CALL ALLOCATE (src, nsd=2, xidim=2, nns=4, nips=3)
CALL RANDOM_NUMBER(src%NLaplacian)

! Test 1: Initiate copies NLaplacian
BLOCK
  TYPE(ElemShapeData_) :: obj

  CALL Initiate(obj, src)
  CALL OK(ALL(SHAPE(obj%NLaplacian) .EQ. [4, 3]), &
          "Initiate: NLaplacian(nns, nips)")
  CALL OK(ALL(ABS(obj%NLaplacian - src%NLaplacian) .LT. tol), &
          "Initiate: NLaplacian")
END BLOCK

! Test 2: assignment copies NLaplacian
BLOCK
  TYPE(ElemShapeData_) :: obj

  obj = src
  CALL OK(ALL(ABS(obj%NLaplacian - src%NLaplacian) .LT. tol), &
          "assignment: NLaplacian")
END BLOCK

END PROGRAM main
