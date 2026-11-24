!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Union and .UNION.

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: OPERATOR(.UNION.)
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: Union
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(BoundingBox_) :: a, b

! lim = [xmin, xmax, ymin, ymax, zmin, zmax]
a = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                1.0_DFP, 0.0_DFP, 0.0_DFP])
b = BoundingBox(nsd=3_I4B, lim=[2.0_DFP, 3.0_DFP, -1.0_DFP, &
                                0.5_DFP, 1.0_DFP, 2.0_DFP])

! Test 1: Union of two disjoint boxes
BLOCK
  TYPE(BoundingBox_) :: c
  c = Union(a, b)
  CALL OK(c%nsd .EQ. 3, "Union: nsd is the larger nsd")
  CALL OK(ALL(ABS(c%box(1, :) - [0.0_DFP, -1.0_DFP, 0.0_DFP]) &
              .LT. tol), "Union: min corner")
  CALL OK(ALL(ABS(c%box(2, :) - [3.0_DFP, 1.0_DFP, 2.0_DFP]) &
              .LT. tol), "Union: max corner")
  CALL OK(ALL(ABS(c%l - [3.0_DFP, 2.0_DFP, 2.0_DFP]) .LT. tol), &
          "Union: lengths")
END BLOCK

! Test 2: the operator gives the same box, in both orders
BLOCK
  TYPE(BoundingBox_) :: c, d, e
  c = Union(a, b)
  d = a.UNION.b
  e = b.UNION.a
  CALL OK(ALL(ABS(c%box - d%box) .LT. tol), ".UNION.: a, b")
  CALL OK(ALL(ABS(c%box - e%box) .LT. tol), ".UNION.: b, a")
END BLOCK

END PROGRAM main
