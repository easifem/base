!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing OrthogonalBasis_Tetrahedron on a collapsed (x,y,z) grid

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron_
IMPLICIT NONE

! Grid in the collapsed cube [-1,1]^3 with 2*2*1 = 4 points.
! The point index is (ii-1)*2 + jj, x varies slowest.
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: a(2) = [-0.5_DFP, 0.5_DFP]
REAL(DFP), PARAMETER :: b(2) = [0.2_DFP, -0.4_DFP]
REAL(DFP), PARAMETER :: c(1) = [0.3_DFP]
REAL(DFP) :: expected(4, 4)
INTEGER(I4B) :: ii, jj, kk

! Order 1 modes in the order p, q, r with r fastest:
! 1, P_1^(2,0)(c), P_1^(1,0)(b)(1-c)/2, a(1-b)(1-c)/4
DO ii = 1, 2
  DO jj = 1, 2
    kk = (ii - 1) * 2 + jj
    expected(kk, 1) = 1.0_DFP
    expected(kk, 2) = 2.0_DFP * c(1) + 1.0_DFP
    expected(kk, 3) = 0.5_DFP * (3.0_DFP * b(jj) + 1.0_DFP) &
                      * 0.5_DFP * (1.0_DFP - c(1))
    expected(kk, 4) = a(ii) * 0.25_DFP * (1.0_DFP - b(jj)) * (1.0_DFP - c(1))
  END DO
END DO

! Test 1: function form on a BIUNIT grid
BLOCK
  REAL(DFP) :: v(4, 4)
  v = OrthogonalBasis_Tetrahedron(order=1_I4B, x=a, y=b, z=c, &
                                  refTetrahedron="BIUNIT")
  CALL OK(MAXVAL(ABS(v - expected)) .LT. tol, "function form, 4 points")
END BLOCK

! Test 2: subroutine form gives the same values and sizes
BLOCK
  REAL(DFP) :: v(4, 4)
  INTEGER(I4B) :: nrow, ncol
  CALL OrthogonalBasis_Tetrahedron_(order=1_I4B, x=a, y=b, z=c, &
                                    refTetrahedron="BIUNIT", ans=v, &
                                    nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 4, "subroutine form, sizes")
  CALL OK(MAXVAL(ABS(v - expected)) .LT. tol, "subroutine form, values")
END BLOCK

! Test 3: UNIT grid coordinates are mapped from [0,1] to [-1,1]
BLOCK
  REAL(DFP) :: v(4, 4)
  v = OrthogonalBasis_Tetrahedron(order=1_I4B, x=0.5_DFP * (1.0_DFP + a), &
                                  y=0.5_DFP * (1.0_DFP + b), &
                                  z=0.5_DFP * (1.0_DFP + c), &
                                  refTetrahedron="UNIT")
  CALL OK(MAXVAL(ABS(v - expected)) .LT. tol, "UNIT grid, 4 points")
END BLOCK

END PROGRAM main
