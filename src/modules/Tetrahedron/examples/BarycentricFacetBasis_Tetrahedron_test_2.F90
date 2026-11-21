!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing BarycentricFacetBasis_Tetrahedron with mixed face orders

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: BarycentricFacetBasis_Tetrahedron
IMPLICIT NONE

! A face of order p has (p-1)(p-2)/2 functions:
! order 3 gives 1 function, order 4 gives 3 functions.
! The functions of one face depend only on that face and its local index,
! so a face of order 3 gives the first function of the same face of
! order 4.
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: lambda(4, 2)
REAL(DFP) :: vMixed(2, 6), vAll4(2, 12)

lambda(:, 1) = [0.1_DFP, 0.2_DFP, 0.3_DFP, 0.4_DFP]
lambda(:, 2) = [0.4_DFP, 0.3_DFP, 0.2_DFP, 0.1_DFP]

! Face orders (3, 4, 3, 3): columns are 1 | 2 3 4 | 5 | 6
vMixed = BarycentricFacetBasis_Tetrahedron(ps1=3_I4B, ps2=4_I4B, &
                                           ps3=3_I4B, ps4=3_I4B, &
                                           lambda=lambda)

! Face orders (4, 4, 4, 4): columns are 1 2 3 | 4 5 6 | 7 8 9 | 10 11 12
vAll4 = BarycentricFacetBasis_Tetrahedron(ps1=4_I4B, ps2=4_I4B, &
                                          ps3=4_I4B, ps4=4_I4B, &
                                          lambda=lambda)

! Test 1: face 1 of order 3
BLOCK
  CALL OK(MAXVAL(ABS(vMixed(:, 1) - vAll4(:, 1))) .LT. tol, &
          "face 1, order 3")
END BLOCK

! Test 2: face 2 of order 4 has three functions
BLOCK
  CALL OK(MAXVAL(ABS(vMixed(:, 2:4) - vAll4(:, 4:6))) .LT. tol, &
          "face 2, order 4")
END BLOCK

! Test 3: faces 3 and 4 of order 3
BLOCK
  CALL OK(MAXVAL(ABS(vMixed(:, 5) - vAll4(:, 7))) .LT. tol, &
          "face 3, order 3")
  CALL OK(MAXVAL(ABS(vMixed(:, 6) - vAll4(:, 10))) .LT. tol, &
          "face 4, order 3")
END BLOCK

END PROGRAM main
