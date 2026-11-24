!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Copy STElemShapeData_ with Initiate and assignment

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: ASSIGNMENT(=)
USE ElemShapeData_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(STElemShapeData_) :: src

! Space-time shape data with nsd=2, xidim=2, nns=4, nips=3, nnt=2
CALL ALLOCATE (src, nsd=2, xidim=2, nns=4, nips=3, nnt=2)
CALL RANDOM_NUMBER(src%N)
CALL RANDOM_NUMBER(src%dNdXi)
CALL RANDOM_NUMBER(src%dNdXt)
CALL RANDOM_NUMBER(src%jacobian)
CALL RANDOM_NUMBER(src%js)
CALL RANDOM_NUMBER(src%ws)
CALL RANDOM_NUMBER(src%coord)
CALL RANDOM_NUMBER(src%T)
CALL RANDOM_NUMBER(src%dTdTheta)
CALL RANDOM_NUMBER(src%dNTdt)
CALL RANDOM_NUMBER(src%dNTdXt)
src%thickness = 2.0_DFP
src%wt = 0.5_DFP
src%jt = 0.25_DFP

! Test 1: Initiate copies the space part of STElemShapeData_
BLOCK
  TYPE(STElemShapeData_) :: obj

  CALL Initiate(obj, src)
  CALL OK(obj%nsd .EQ. 2 .AND. obj%xidim .EQ. 2, "nsd, xidim")
  CALL OK(obj%nns .EQ. 4 .AND. obj%nips .EQ. 3, "nns, nips")
  CALL OK(ALL(ABS(obj%N - src%N) .LT. tol), "N")
  CALL OK(ALL(ABS(obj%dNdXi - src%dNdXi) .LT. tol), "dNdXi")
  CALL OK(ALL(ABS(obj%dNdXt - src%dNdXt) .LT. tol), "dNdXt")
  CALL OK(ALL(ABS(obj%jacobian - src%jacobian) .LT. tol), "jacobian")
  CALL OK(ALL(ABS(obj%js - src%js) .LT. tol), "js")
  CALL OK(ALL(ABS(obj%ws - src%ws) .LT. tol), "ws")
  CALL OK(ALL(ABS(obj%coord - src%coord) .LT. tol), "coord")
  CALL OK(ALL(ABS(obj%thickness - 2.0_DFP) .LT. tol), "thickness")
END BLOCK

! Test 2: Initiate copies the time part of STElemShapeData_
BLOCK
  TYPE(STElemShapeData_) :: obj

  CALL Initiate(obj, src)
  CALL OK(obj%nnt .EQ. 2, "nnt")
  CALL OK(ABS(obj%wt - 0.5_DFP) .LT. tol, "wt")
  CALL OK(ABS(obj%jt - 0.25_DFP) .LT. tol, "jt")
  CALL OK(ALL(ABS(obj%T - src%T) .LT. tol), "T")
  CALL OK(ALL(ABS(obj%dTdTheta - src%dTdTheta) .LT. tol), "dTdTheta")
  CALL OK(ALL(ABS(obj%dNTdt - src%dNTdt) .LT. tol), "dNTdt")
  CALL OK(ALL(ABS(obj%dNTdXt - src%dNTdXt) .LT. tol), "dNTdXt")
END BLOCK

! Test 3: assignment reallocates an object of other sizes
BLOCK
  TYPE(STElemShapeData_) :: obj

  CALL ALLOCATE (obj, nsd=3, xidim=3, nns=8, nips=8, nnt=3)
  obj = src
  CALL OK(ALL(SHAPE(obj%N) .EQ. [4, 3]), "assignment: N(nns, nips)")
  CALL OK(ALL(SHAPE(obj%dNTdXt) .EQ. [4, 2, 2, 3]), &
          "assignment: dNTdXt(nns, nnt, nsd, nips)")
  CALL OK(ALL(ABS(obj%dNTdXt - src%dNTdXt) .LT. tol), &
          "assignment: dNTdXt")
END BLOCK

! Test 4: copy STElemShapeData_ into ElemShapeData_ (space part only)
BLOCK
  TYPE(ElemShapeData_) :: obj

  CALL Initiate(obj, src)
  CALL OK(obj%nns .EQ. 4 .AND. obj%nips .EQ. 3, "to space: sizes")
  CALL OK(ALL(ABS(obj%N - src%N) .LT. tol), "to space: N")
  CALL OK(ALL(ABS(obj%jacobian - src%jacobian) .LT. tol), &
          "to space: jacobian")
END BLOCK

END PROGRAM main
