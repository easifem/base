!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Equidistant points of a 1D segment given by two ends.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: EquidistancePoint_Line
USE LineInterpolationUtility, ONLY: EquidistancePoint_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: order 3; end points first, then interior points
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = EquidistancePoint_Line(order=3_I4B, xij=[0.0_DFP, 3.0_DFP])
  CALL OK(SIZE(y) == 4, "Order 3 has four points")
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 3.0_DFP, 1.0_DFP, 2.0_DFP])) < tol, &
          "Points in vertex-first order")
END BLOCK

! Test 2: order 0 gives the midpoint
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = EquidistancePoint_Line(order=0_I4B, xij=[0.0_DFP, 3.0_DFP])
  CALL OK(SIZE(y) == 1, "Order 0 has one point")
  CALL OK(ABS(y(1) - 1.5_DFP) < tol, "Order 0 point is the midpoint")
END BLOCK

! Test 3: order 1 gives the two end points
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = EquidistancePoint_Line(order=1_I4B, xij=[0.0_DFP, 3.0_DFP])
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 3.0_DFP])) < tol, &
          "Order 1 points are the end points")
END BLOCK

! Test 4: subroutine, order 2 on [-1, 1]
BLOCK
  REAL(DFP) :: buf(5)
  INTEGER(I4B) :: tsize
  CALL EquidistancePoint_Line_(order=2_I4B, xij=[-1.0_DFP, 1.0_DFP], &
                               ans=buf, tsize=tsize)
  CALL OK(tsize == 3, "Order 2 writes three points")
  CALL OK(MAXVAL(ABS(buf(1:3) - [-1.0_DFP, 1.0_DFP, 0.0_DFP])) < tol, &
          "Subroutine points in vertex-first order")
END BLOCK

END PROGRAM main
