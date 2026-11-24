!> author: Vikas Sharma, Ph. D.
! date: 2026-11-24
! summary: Copy STElemShapeData_ whose source has no dNTdt or dNTdXt
!
! The source has the time part `nnt`, `T` and `dTdTheta`, as set by
! `Initiate(obj(:), elemsd)`, and a space part allocated without `nnt`.
! So `dNTdt` and `dNTdXt` of the source are not allocated.
! The copy skips them and keeps the zeros set by the allocation.

PROGRAM main
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: ASSIGNMENT(=)
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(STElemShapeData_) :: src

! space part: nsd=2, xidim=2, nns=4, nips=4; no nnt
CALL ALLOCATE (src, nsd=2, xidim=2, nns=4, nips=4)
CALL RANDOM_NUMBER(src%N)
! time part: nnt=2 with T and dTdTheta only
src%nnt = 2
src%T = [0.25_DFP, 0.75_DFP]
src%dTdTheta = [-0.5_DFP, 0.5_DFP]

! Test 1: assignment skips the time arrays that are not allocated
BLOCK
  TYPE(STElemShapeData_) :: obj

  obj = src
  CALL OK(obj%nnt .EQ. 2, "nnt")
  CALL OK(ALL(ABS(obj%N - src%N) .LT. tol), "N")
  CALL OK(ALL(ABS(obj%T - src%T) .LT. tol), "T")
  CALL OK(ALL(ABS(obj%dTdTheta - src%dTdTheta) .LT. tol), "dTdTheta")
  CALL OK(ALL(obj%dNTdt .EQ. 0.0_DFP), "dNTdt is zero")
  CALL OK(ALL(obj%dNTdXt .EQ. 0.0_DFP), "dNTdXt is zero")
END BLOCK

END PROGRAM main
