!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetDiameter, GetDiameterSqr, GetRadius and
! GetRadiusSqr

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: GetDiameter
USE BoundingBox_Method, ONLY: GetDiameterSqr
USE BoundingBox_Method, ONLY: GetRadius
USE BoundingBox_Method, ONLY: GetRadiusSqr
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(BoundingBox_) :: obj

! box with sides 1, 2, 2, so the diagonal is 3
! lim = [xmin, xmax, ymin, ymax, zmin, zmax]
obj = BoundingBox(nsd=3_I4B, lim=[0.0_DFP, 1.0_DFP, 1.0_DFP, &
                                  3.0_DFP, -1.0_DFP, 1.0_DFP])

! Test 1: diameter is the length of the diagonal
BLOCK
  CALL OK(ABS(GetDiameterSqr(obj) - 9.0_DFP) .LT. tol, "GetDiameterSqr")
  CALL OK(ABS(GetDiameter(obj) - 3.0_DFP) .LT. tol, "GetDiameter")
END BLOCK

! Test 2: radius is half the diameter
BLOCK
  CALL OK(ABS(GetRadius(obj) - 1.5_DFP) .LT. tol, "GetRadius")
  CALL OK(ABS(GetRadiusSqr(obj) - 2.25_DFP) .LT. tol, "GetRadiusSqr")
END BLOCK

! Test 3: a 2D unit square
BLOCK
  TYPE(BoundingBox_) :: sq
  sq = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                   1.0_DFP, 0.0_DFP, 0.0_DFP])
  CALL OK(ABS(GetDiameter(sq) - SQRT(2.0_DFP)) .LT. tol, &
          "GetDiameter: unit square")
END BLOCK

END PROGRAM main
