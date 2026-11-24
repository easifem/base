!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing .Nptrs., the nodes inside a bounding box

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: OPERATOR(.Nptrs.)
USE BoundingBox_Method, ONLY: BoundingBox
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(BoundingBox_) :: obj

! unit square; lim = [xmin, xmax, ymin, ymax, zmin, zmax]
obj = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                  1.0_DFP, 0.0_DFP, 0.0_DFP])

! Test 1: nodes inside or on the boundary are returned in order
BLOCK
  REAL(DFP) :: xij(2, 5)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  xij = RESHAPE([0.5_DFP, 0.5_DFP, &
                 2.0_DFP, 0.5_DFP, &
                 1.0_DFP, 1.0_DFP, &
                 -0.1_DFP, 0.5_DFP, &
                 0.0_DFP, 0.2_DFP], [2, 5])
  nptrs = obj.Nptrs.xij
  CALL OK(SIZE(nptrs) .EQ. 3, ".Nptrs.: number of nodes inside")
  CALL OK(ALL(nptrs .EQ. [1, 3, 5]), ".Nptrs.: node numbers")
END BLOCK

! Test 2: no node inside gives an empty list
BLOCK
  REAL(DFP) :: xij(2, 2)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  xij = RESHAPE([5.0_DFP, 5.0_DFP, -5.0_DFP, 0.5_DFP], [2, 2])
  nptrs = obj.Nptrs.xij
  CALL OK(SIZE(nptrs) .EQ. 0, ".Nptrs.: no node inside")
END BLOCK

END PROGRAM main
