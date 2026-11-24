!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing QuadraturePoint_Tetrahedron above the Solin order limit

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: QuadratureNumber_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron
IMPLICIT NONE

! Order 22 is above the largest Solin rule (21), so a collapsed tensor
! rule is used. On the unit tetrahedron int x**a = 1/((a+1)(a+2)(a+3)).
INTEGER(I4B), PARAMETER :: order = 22
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: q(:, :)
REAL(DFP) :: exact

q = QuadraturePoint_Tetrahedron(order=order, quadType=GaussLegendreQP, &
                                refTetrahedron="UNIT")
exact = 1.0_DFP / REAL((order + 1) * (order + 2) * (order + 3), DFP)

! Test 1: number of points agrees with QuadratureNumber_Tetrahedron
BLOCK
  INTEGER(I4B) :: n
  n = QuadratureNumber_Tetrahedron(order=order, quadType=GaussLegendreQP)
  CALL OK(SIZE(q, 2) .EQ. n, "number of points")
END BLOCK

! Test 2: weights sum to the volume 1/6
BLOCK
  CALL OK(ABS(SUM(q(4, :)) - 1.0_DFP / 6.0_DFP) .LT. tol, "volume")
END BLOCK

! Test 3: the rule integrates x**22, y**22 and z**22 exactly
BLOCK
  INTEGER(I4B) :: ii
  REAL(DFP) :: relErr
  DO ii = 1, 3
    relErr = ABS(SUM(q(4, :) * q(ii, :)**order) - exact) / exact
    CALL OK(relErr .LT. tol, "degree 22 monomial in direction "// &
            CHAR(ICHAR('0') + ii))
  END DO
END BLOCK

END PROGRAM main
