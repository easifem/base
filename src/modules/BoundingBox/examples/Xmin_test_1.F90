!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing the .Xmin., .Xmax., .Ymin., .Ymax., .Zmin., .Zmax.
! operators

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
TYPE(BoundingBox_) :: obj

! lim = [xmin, xmax, ymin, ymax, zmin, zmax]
obj = BoundingBox(nsd=3_I4B, lim=[-1.0_DFP, 1.0_DFP, -2.0_DFP, &
                                  2.0_DFP, -3.0_DFP, 3.0_DFP])

! Test 1: x limits
BLOCK
  CALL OK(ABS((.Xmin.obj) + 1.0_DFP) .LT. tol, ".Xmin.")
  CALL OK(ABS((.Xmax.obj) - 1.0_DFP) .LT. tol, ".Xmax.")
END BLOCK

! Test 2: y limits
BLOCK
  CALL OK(ABS((.Ymin.obj) + 2.0_DFP) .LT. tol, ".Ymin.")
  CALL OK(ABS((.Ymax.obj) - 2.0_DFP) .LT. tol, ".Ymax.")
END BLOCK

! Test 3: z limits
BLOCK
  CALL OK(ABS((.Zmin.obj) + 3.0_DFP) .LT. tol, ".Zmin.")
  CALL OK(ABS((.Zmax.obj) - 3.0_DFP) .LT. tol, ".Zmax.")
END BLOCK

END PROGRAM main
