!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing isInside and .isInside.

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: OPERATOR(.isInside.)
USE BoundingBox_Method, ONLY: BoundingBox
USE BoundingBox_Method, ONLY: isInside
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(BoundingBox_) :: obj

! unit cube; lim = [xmin, xmax, ymin, ymax, zmin, zmax]
obj = BoundingBox(nsd=3_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                  1.0_DFP, 0.0_DFP, 1.0_DFP])

! Test 1: points inside and outside a 3D box
BLOCK
  CALL OK(isInside(obj, [0.5_DFP, 0.5_DFP, 0.5_DFP]), &
          "isInside: center point")
  CALL OK(.NOT. isInside(obj, [1.5_DFP, 0.5_DFP, 0.5_DFP]), &
          "isInside: outside in x")
  CALL OK(.NOT. isInside(obj, [0.5_DFP, 0.5_DFP, -0.5_DFP]), &
          "isInside: outside in z")
END BLOCK

! Test 2: points on the boundary are inside
BLOCK
  CALL OK(isInside(obj, [0.0_DFP, 0.0_DFP, 0.0_DFP]), &
          "isInside: corner (0,0,0)")
  CALL OK(isInside(obj, [1.0_DFP, 1.0_DFP, 1.0_DFP]), &
          "isInside: corner (1,1,1)")
END BLOCK

! Test 3: a 2D point checks x and y only
BLOCK
  CALL OK(isInside(obj, [0.2_DFP, 0.8_DFP]), "isInside: 2D point")
  CALL OK(.NOT. isInside(obj, [0.2_DFP, 1.8_DFP]), &
          "isInside: 2D point outside in y")
END BLOCK

! Test 4: the operator gives the same result
BLOCK
  CALL OK(obj.isInside. [0.5_DFP, 0.5_DFP, 0.5_DFP], &
          ".isInside.: inside")
  CALL OK(.NOT. (obj.isInside. [2.0_DFP, 0.5_DFP, 0.5_DFP]), &
          ".isInside.: outside")
END BLOCK

END PROGRAM main
