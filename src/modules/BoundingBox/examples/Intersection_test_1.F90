!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Intersection and .Intersection.

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: OPERATOR(.Intersection.)
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: Intersection
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(BoundingBox_) :: a, b

! lim = [xmin, xmax, ymin, ymax, zmin, zmax]
a = BoundingBox(nsd=3_I4B, lim=[0.0_DFP, 2.0_DFP, 0.0_DFP, &
                                2.0_DFP, 0.0_DFP, 2.0_DFP])
b = BoundingBox(nsd=2_I4B, lim=[1.0_DFP, 3.0_DFP, -1.0_DFP, &
                                1.0_DFP, 0.5_DFP, 1.5_DFP])

! Test 1: Intersection of two overlapping boxes
BLOCK
  TYPE(BoundingBox_) :: c
  c = Intersection(a, b)
  CALL OK(c%nsd .EQ. 3, "Intersection: nsd is the larger nsd")
  CALL OK(ALL(ABS(c%box(1, :) - [1.0_DFP, 0.0_DFP, 0.5_DFP]) &
              .LT. tol), "Intersection: min corner")
  CALL OK(ALL(ABS(c%box(2, :) - [2.0_DFP, 1.0_DFP, 1.5_DFP]) &
              .LT. tol), "Intersection: max corner")
  CALL OK(ALL(ABS(c%l - [1.0_DFP, 1.0_DFP, 1.0_DFP]) .LT. tol), &
          "Intersection: lengths")
END BLOCK

! Test 2: the operator gives the same box, in both orders
BLOCK
  TYPE(BoundingBox_) :: c, d, e
  c = Intersection(a, b)
  d = a.Intersection.b
  e = b.Intersection.a
  CALL OK(ALL(ABS(c%box - d%box) .LT. tol), ".Intersection.: a, b")
  CALL OK(ALL(ABS(c%box - e%box) .LT. tol), ".Intersection.: b, a")
END BLOCK

! Test 3: intersection with itself is the same box
BLOCK
  TYPE(BoundingBox_) :: c
  c = a.Intersection.a
  CALL OK(ALL(ABS(c%box - a%box) .LT. tol), ".Intersection.: a, a")
END BLOCK

END PROGRAM main
