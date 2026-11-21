!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing the BoundingBox constructor functions

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: OPERATOR(.Xmax.)
USE BoundingBox_Method, ONLY: OPERATOR(.Xmin.)
USE BoundingBox_Method, ONLY: OPERATOR(.Ymax.)
USE BoundingBox_Method, ONLY: OPERATOR(.Ymin.)
USE BoundingBox_Method, ONLY: OPERATOR(.Zmax.)
USE BoundingBox_Method, ONLY: OPERATOR(.Zmin.)
USE BoundingBox_Method, ONLY: BoundingBox
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: BoundingBox(nsd, lim), lim = [xmin, xmax, ymin, ymax, zmin, zmax]
BLOCK
  TYPE(BoundingBox_) :: obj
  obj = BoundingBox(nsd=3_I4B, lim=[0.0_DFP, 1.0_DFP, 2.0_DFP, &
                                    4.0_DFP, 5.0_DFP, 8.0_DFP])
  CALL OK(obj%nsd .EQ. 3, "BoundingBox(nsd, lim): nsd")
  CALL OK(ABS((.Xmin.obj) - 0.0_DFP) .LT. tol .AND. &
          ABS((.Xmax.obj) - 1.0_DFP) .LT. tol, &
          "BoundingBox(nsd, lim): x range")
  CALL OK(ABS((.Ymin.obj) - 2.0_DFP) .LT. tol .AND. &
          ABS((.Ymax.obj) - 4.0_DFP) .LT. tol, &
          "BoundingBox(nsd, lim): y range")
  CALL OK(ABS((.Zmin.obj) - 5.0_DFP) .LT. tol .AND. &
          ABS((.Zmax.obj) - 8.0_DFP) .LT. tol, &
          "BoundingBox(nsd, lim): z range")
  CALL OK(ALL(ABS(obj%l - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), &
          "BoundingBox(nsd, lim): lengths")
END BLOCK

! Test 2: BoundingBox(anotherobj) copies the box
BLOCK
  TYPE(BoundingBox_) :: obj, obj2
  obj = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 1.0_DFP, 2.0_DFP, &
                                    4.0_DFP, 0.0_DFP, 0.0_DFP])
  obj2 = BoundingBox(obj)
  CALL OK(obj2%nsd .EQ. 2, "BoundingBox(anotherobj): nsd")
  CALL OK(ALL(ABS(obj2%box - obj%box) .LT. tol), &
          "BoundingBox(anotherobj): box")
  CALL OK(ALL(ABS(obj2%l - obj%l) .LT. tol), &
          "BoundingBox(anotherobj): lengths")
END BLOCK

! Test 3: BoundingBox(xij) for 2D points; z range stays zero
BLOCK
  TYPE(BoundingBox_) :: obj
  REAL(DFP) :: xij(2, 4)
  xij = RESHAPE([0.5_DFP, -1.0_DFP, 2.0_DFP, 3.0_DFP, &
                 -0.5_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP], [2, 4])
  obj = BoundingBox(xij)
  CALL OK(obj%nsd .EQ. 2, "BoundingBox(xij) 2D: nsd")
  CALL OK(ABS((.Xmin.obj) + 0.5_DFP) .LT. tol .AND. &
          ABS((.Xmax.obj) - 2.0_DFP) .LT. tol, &
          "BoundingBox(xij) 2D: x range")
  CALL OK(ABS((.Ymin.obj) + 1.0_DFP) .LT. tol .AND. &
          ABS((.Ymax.obj) - 3.0_DFP) .LT. tol, &
          "BoundingBox(xij) 2D: y range")
  CALL OK(ABS(.Zmin.obj) .LT. tol .AND. ABS(.Zmax.obj) .LT. tol, &
          "BoundingBox(xij) 2D: z range is zero")
END BLOCK

! Test 4: BoundingBox(xij) for 3D points
BLOCK
  TYPE(BoundingBox_) :: obj
  REAL(DFP) :: xij(3, 2)
  xij = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, &
                 -1.0_DFP, 5.0_DFP, 4.0_DFP], [3, 2])
  obj = BoundingBox(xij)
  CALL OK(obj%nsd .EQ. 3, "BoundingBox(xij) 3D: nsd")
  CALL OK(ALL(ABS(obj%box(1, :) - [-1.0_DFP, 2.0_DFP, 3.0_DFP]) &
              .LT. tol), "BoundingBox(xij) 3D: min corner")
  CALL OK(ALL(ABS(obj%box(2, :) - [1.0_DFP, 5.0_DFP, 4.0_DFP]) &
              .LT. tol), "BoundingBox(xij) 3D: max corner")
END BLOCK

! Test 5: BoundingBox(xij) for 1D points
BLOCK
  TYPE(BoundingBox_) :: obj
  REAL(DFP) :: xij(1, 3)
  xij(1, :) = [3.0_DFP, -2.0_DFP, 1.0_DFP]
  obj = BoundingBox(xij)
  CALL OK(obj%nsd .EQ. 1, "BoundingBox(xij) 1D: nsd")
  CALL OK(ABS((.Xmin.obj) + 2.0_DFP) .LT. tol .AND. &
          ABS((.Xmax.obj) - 3.0_DFP) .LT. tol, &
          "BoundingBox(xij) 1D: x range")
END BLOCK

END PROGRAM main
