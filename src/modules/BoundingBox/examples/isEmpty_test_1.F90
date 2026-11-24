!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing isEmpty

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: OPERATOR(.Intersection.)
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: isEmpty
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: a default box is empty
BLOCK
  TYPE(BoundingBox_) :: obj
  CALL OK(isEmpty(obj), "isEmpty: default box")
END BLOCK

! Test 2: a unit square and a unit cube are not empty
BLOCK
  TYPE(BoundingBox_) :: sq, cube
  ! lim = [xmin, xmax, ymin, ymax, zmin, zmax]
  sq = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                   1.0_DFP, 0.0_DFP, 0.0_DFP])
  cube = BoundingBox(nsd=3_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                     1.0_DFP, 0.0_DFP, 1.0_DFP])
  CALL OK(.NOT. isEmpty(sq), "isEmpty: unit square")
  CALL OK(.NOT. isEmpty(cube), "isEmpty: unit cube")
END BLOCK

! Test 3: the intersection of two boxes apart in x is empty
BLOCK
  TYPE(BoundingBox_) :: a, b, c
  a = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                  1.0_DFP, 0.0_DFP, 0.0_DFP])
  b = BoundingBox(nsd=2_I4B, lim=[2.0_DFP, 3.0_DFP, 0.0_DFP, &
                                  1.0_DFP, 0.0_DFP, 0.0_DFP])
  c = a.Intersection.b
  CALL OK(isEmpty(c), "isEmpty: intersection of disjoint boxes")
END BLOCK

END PROGRAM main
