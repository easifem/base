!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Interior equidistant points of a 1D segment given by two ends.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line
USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: function, order 4 on [0, 2]
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = EquidistanceInPoint_Line(order=4_I4B, xij=[0.0_DFP, 2.0_DFP])
  CALL OK(SIZE(y) == 3, "Order 4 has three interior points")
  CALL OK(MAXVAL(ABS(y - [0.5_DFP, 1.0_DFP, 1.5_DFP])) < tol, &
          "Interior points are equally spaced")
END BLOCK

! Test 2: function, order 1 has no interior point
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = EquidistanceInPoint_Line(order=1_I4B, xij=[0.0_DFP, 2.0_DFP])
  CALL OK(SIZE(y) == 0, "Order 1 returns an empty array")
END BLOCK

! Test 3: subroutine, order 3 on [-1, 1]
BLOCK
  REAL(DFP) :: buf(4)
  INTEGER(I4B) :: tsize
  CALL EquidistanceInPoint_Line_(order=3_I4B, xij=[-1.0_DFP, 1.0_DFP], &
                                 ans=buf, tsize=tsize)
  CALL OK(tsize == 2, "Order 3 writes two interior points")
  CALL OK(MAXVAL(ABS(buf(1:2) - [-1.0_DFP, 1.0_DFP] / 3.0_DFP)) < tol, &
          "Interior points at -1/3 and 1/3")
END BLOCK

! Test 4: subroutine, order 1 writes nothing
BLOCK
  REAL(DFP) :: buf(4)
  INTEGER(I4B) :: tsize
  CALL EquidistanceInPoint_Line_(order=1_I4B, xij=[-1.0_DFP, 1.0_DFP], &
                                 ans=buf, tsize=tsize)
  CALL OK(tsize == 0, "Order 1 writes no point")
END BLOCK

END PROGRAM main
