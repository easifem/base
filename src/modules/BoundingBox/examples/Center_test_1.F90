!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Center and .Center.

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: OPERATOR(.Center.)
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: Center
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: center of a 3D box
BLOCK
  TYPE(BoundingBox_) :: obj
  ! lim = [xmin, xmax, ymin, ymax, zmin, zmax]
  obj = BoundingBox(nsd=3_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                    2.0_DFP, -4.0_DFP, 0.0_DFP])
  CALL OK(ALL(ABS(Center(obj) - [0.5_DFP, 1.0_DFP, -2.0_DFP]) &
              .LT. tol), "Center: 3D box")
  CALL OK(ALL(ABS((.Center.obj) - Center(obj)) .LT. tol), &
          ".Center.: same as Center")
END BLOCK

! Test 2: center of a 2D box has zero z
BLOCK
  TYPE(BoundingBox_) :: obj
  obj = BoundingBox(nsd=2_I4B, lim=[-1.0_DFP, 1.0_DFP, 2.0_DFP, &
                                    4.0_DFP, 0.0_DFP, 0.0_DFP])
  CALL OK(ALL(ABS(Center(obj) - [0.0_DFP, 3.0_DFP, 0.0_DFP]) &
              .LT. tol), "Center: 2D box")
END BLOCK

END PROGRAM main
