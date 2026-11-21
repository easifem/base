!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing isIntersectInX, isIntersectInY, isIntersectInZ,
! isIntersect and .isIntersect.

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: OPERATOR(.isIntersect.)
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: isIntersect
USE BoundingBox_Method, ONLY: isIntersectInX
USE BoundingBox_Method, ONLY: isIntersectInY
USE BoundingBox_Method, ONLY: isIntersectInZ
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(BoundingBox_) :: unitBox

! unit cube [0,1]^3; lim = [xmin, xmax, ymin, ymax, zmin, zmax]
unitBox = BoundingBox(nsd=3_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                      1.0_DFP, 0.0_DFP, 1.0_DFP])

! Test 1: partly overlapping boxes intersect in every direction
BLOCK
  TYPE(BoundingBox_) :: obj
  obj = BoundingBox(nsd=3_I4B, lim=[0.5_DFP, 1.5_DFP, 0.5_DFP, &
                                    1.5_DFP, 0.5_DFP, 1.5_DFP])
  CALL OK(isIntersectInX(unitBox, obj), "isIntersectInX: overlap")
  CALL OK(isIntersectInY(unitBox, obj), "isIntersectInY: overlap")
  CALL OK(isIntersectInZ(unitBox, obj), "isIntersectInZ: overlap")
  CALL OK(isIntersect(unitBox, obj), "isIntersect: overlap")
  CALL OK(unitBox.isIntersect.obj, ".isIntersect.: overlap")
END BLOCK

! Test 2: boxes apart in x only
BLOCK
  TYPE(BoundingBox_) :: obj
  obj = BoundingBox(nsd=3_I4B, lim=[2.0_DFP, 3.0_DFP, 0.0_DFP, &
                                    1.0_DFP, 0.0_DFP, 1.0_DFP])
  CALL OK(.NOT. isIntersectInX(unitBox, obj), "isIntersectInX: apart")
  CALL OK(isIntersectInY(unitBox, obj), "isIntersectInY: same range")
  CALL OK(.NOT. isIntersect(unitBox, obj), "isIntersect: apart in x")
END BLOCK

! Test 3: boxes that touch at a face intersect
BLOCK
  TYPE(BoundingBox_) :: obj
  obj = BoundingBox(nsd=3_I4B, lim=[1.0_DFP, 2.0_DFP, 0.0_DFP, &
                                    1.0_DFP, 0.0_DFP, 1.0_DFP])
  CALL OK(isIntersectInX(unitBox, obj), "isIntersectInX: touching")
  CALL OK(isIntersect(unitBox, obj), "isIntersect: touching")
END BLOCK

! Test 4: a box inside the other intersects, in both argument orders
BLOCK
  TYPE(BoundingBox_) :: small
  small = BoundingBox(nsd=3_I4B, lim=[0.25_DFP, 0.75_DFP, 0.25_DFP, &
                                      0.75_DFP, 0.25_DFP, 0.75_DFP])
  CALL OK(isIntersect(unitBox, small), "isIntersect: big, small")
  CALL OK(isIntersectInX(small, unitBox), "isIntersectInX: small, big")
  CALL OK(isIntersectInY(small, unitBox), "isIntersectInY: small, big")
  CALL OK(isIntersectInZ(small, unitBox), "isIntersectInZ: small, big")
  CALL OK(isIntersect(small, unitBox), "isIntersect: small, big")
END BLOCK

END PROGRAM main
