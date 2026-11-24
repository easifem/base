!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing LagrangeDegree_Quadrangle and LagrangeDegree_Quadrangle_

PROGRAM main
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: isotropic exponent table, the x exponent varies fastest
BLOCK
  INTEGER(I4B), ALLOCATABLE :: degree(:, :)

  degree = LagrangeDegree_Quadrangle(order=1_I4B)
  CALL OK(ALL(SHAPE(degree) .EQ. [4, 2]), "order=1: table shape")
  CALL OK(ALL(degree(:, 1) .EQ. [0, 1, 0, 1]) .AND. &
          ALL(degree(:, 2) .EQ. [0, 0, 1, 1]), &
          "order=1: x exponent varies fastest")
END BLOCK

! Test 2: anisotropic exponent table
BLOCK
  INTEGER(I4B), ALLOCATABLE :: degree(:, :)

  degree = LagrangeDegree_Quadrangle(p=2_I4B, q=1_I4B)
  CALL OK(ALL(SHAPE(degree) .EQ. [6, 2]), "p=2, q=1: table shape")
  CALL OK(ALL(degree(:, 1) .EQ. [0, 1, 2, 0, 1, 2]) .AND. &
          ALL(degree(:, 2) .EQ. [0, 0, 0, 1, 1, 1]), &
          "p=2, q=1: exponents")
END BLOCK

! Test 3: subroutine form with order
BLOCK
  INTEGER(I4B) :: ans(9, 2), nrow, ncol

  CALL LagrangeDegree_Quadrangle_(order=2_I4B, ans=ans, nrow=nrow, &
                                  ncol=ncol)
  CALL OK(nrow .EQ. 9 .AND. ncol .EQ. 2, "order=2: nrow and ncol")
  CALL OK(ALL(ans(9, :) .EQ. [2, 2]), "order=2: last row is x^2 y^2")
END BLOCK

! Test 4: subroutine form with p and q
BLOCK
  INTEGER(I4B) :: ans(9, 2), nrow, ncol

  CALL LagrangeDegree_Quadrangle_(p=1_I4B, q=2_I4B, ans=ans, nrow=nrow, &
                                  ncol=ncol)
  CALL OK(nrow .EQ. 6 .AND. ncol .EQ. 2, "p=1, q=2: nrow and ncol")
  CALL OK(ALL(ans(1:nrow, 2) .EQ. [0, 0, 1, 1, 2, 2]), &
          "p=1, q=2: y exponents")
END BLOCK

END PROGRAM main
