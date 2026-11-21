!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Allocate and deallocate the data of ElemShapeData_

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: DEALLOCATE
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: ALLOCATE sets the sizes of all arrays
BLOCK
  TYPE(ElemShapeData_) :: obj

  CALL ALLOCATE (obj, nsd=3, xidim=2, nns=4, nips=5)
  CALL OK(obj%nsd .EQ. 3 .AND. obj%xidim .EQ. 2, "nsd, xidim")
  CALL OK(obj%nns .EQ. 4 .AND. obj%nips .EQ. 5, "nns, nips")
  CALL OK(ALL(SHAPE(obj%N) .EQ. [4, 5]), "N(nns, nips)")
  CALL OK(ALL(SHAPE(obj%dNdXi) .EQ. [4, 2, 5]), "dNdXi(nns, xidim, nips)")
  CALL OK(ALL(SHAPE(obj%dNdXt) .EQ. [4, 3, 5]), "dNdXt(nns, nsd, nips)")
  CALL OK(ALL(SHAPE(obj%jacobian) .EQ. [3, 2, 5]), &
          "jacobian(nsd, xidim, nips)")
  CALL OK(ALL(SHAPE(obj%normal) .EQ. [3, 5]), "normal(3, nips)")
  CALL OK(ALL(SHAPE(obj%coord) .EQ. [3, 5]), "coord(nsd, nips)")
  CALL OK(SIZE(obj%js) .EQ. 5 .AND. SIZE(obj%ws) .EQ. 5, "js, ws(nips)")
  CALL OK(ALL(obj%thickness .EQ. 1.0_DFP), "thickness = 1")
END BLOCK

! Test 2: ALLOCATE with nnt for space-time shape data
BLOCK
  TYPE(STElemShapeData_) :: obj

  CALL ALLOCATE (obj, nsd=2, xidim=2, nns=4, nips=4, nnt=2)
  CALL OK(obj%nnt .EQ. 2, "space-time: nnt = 2")
  CALL OK(SIZE(obj%T) .EQ. 2 .AND. SIZE(obj%dTdTheta) .EQ. 2, &
          "space-time: T, dTdTheta(nnt)")
  CALL OK(ALL(SHAPE(obj%dNTdt) .EQ. [4, 2, 4]), &
          "space-time: dNTdt(nns, nnt, nips)")
  CALL OK(ALL(SHAPE(obj%dNTdXt) .EQ. [4, 2, 2, 4]), &
          "space-time: dNTdXt(nns, nnt, nsd, nips)")
END BLOCK

! Test 3: DEALLOCATE frees all arrays and resets the sizes
BLOCK
  TYPE(STElemShapeData_) :: obj

  CALL ALLOCATE (obj, nsd=2, xidim=2, nns=4, nips=4, nnt=2)
  CALL DEALLOCATE (obj)
  CALL OK(.NOT. ALLOCATED(obj%N), "N is deallocated")
  CALL OK(.NOT. ALLOCATED(obj%dNdXi), "dNdXi is deallocated")
  CALL OK(.NOT. ALLOCATED(obj%T), "T is deallocated")
  CALL OK(obj%nns .EQ. 0 .AND. obj%nips .EQ. 0 .AND. obj%nnt .EQ. 0, &
          "sizes are zero")
END BLOCK

END PROGRAM main
