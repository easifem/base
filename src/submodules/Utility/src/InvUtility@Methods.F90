! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary: Methods for determining determinent and inverse of small matrix

SUBMODULE(InvUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS
!----------------------------------------------------------------------------
!                                                                        DET
!----------------------------------------------------------------------------

MODULE PROCEDURE det_2D
SELECT CASE (SIZE(A, 1))
CASE (1)
  Ans = A(1, 1)
CASE (2)
  Ans = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
CASE (3)
  Ans = A(1, 1) * (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)) &
    & - A(1, 2) * (A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1)) &
    & + A(1, 3) * (A(2, 1) * A(3, 2) - A(3, 1) * A(2, 2))
CASE (4)
  Ans = A(1, 1) * (A(2, 2) * (A(3, 3) * A(4, 4) - A(3, 4) * A(4, 3))&
    & + A(2, 3) * (A(3, 4) * A(4, 2) - A(3, 2) * A(4, 4)) &
    & + A(2, 4) * (A(3, 2) * A(4, 3) &
    & - A(3, 3) * A(4, 2))) - A(1, 2) * (A(2, 1) * (A(3, 3) * A(4, 4) &
& - A(3, 4) * A(4, 3)) + A(2, 3) * (A(3, 4) * A(4, 1) - A(3, 1) * A(4, 4)) &
    & + A(2, 4) * (A(3, 1) * A(4, 3) - A(3, 3) * A(4, 1))) &
    & + A(1, 3) * (A(2, 1) * (A(3, 2) * A(4, 4) - A(3, 4) * A(4, 2)) &
    & + A(2, 2) * (A(3, 4) * A(4, 1) &
& - A(3, 1) * A(4, 4)) + A(2, 4) * (A(3, 1) * A(4, 2) - A(3, 2) * A(4, 1))) &
    & - A(1, 4) * (A(2, 1) * (A(3, 2) * A(4, 3) - A(3, 3) * A(4, 2)) &
    & + A(2, 2) * (A(3, 3) * A(4, 1) - A(3, 1) * A(4, 3)) &
    & + A(2, 3) * (A(3, 1) * A(4, 2) - A(3, 2) * A(4, 1)))
CASE (5)
  Ans = det_2d_5(a)
CASE DEFAULT
  ! size 0 or larger than 5: Gaussian elimination
  Ans = det_general(A)
END SELECT
END PROCEDURE det_2D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION det_2d_5(a) RESULT(ans)
  REAL(DFP), INTENT(IN) :: a(5, 5)
  REAL(DFP) :: ans
  !!
  REAL(DFP) :: b(4, 4)
  INTEGER(I4B) :: i
  INTEGER(I4B) :: inc
  INTEGER(I4B) :: j
  INTEGER(I4B) :: k
  !
  !  Expand the determinant into the sum of the determinants of the
  !  five 4 by 4 matrices created by dropping row 1, and column k.
  !
  ans = 0.0D+00
  !
  DO k = 1, 5
    DO i = 1, 4
      DO j = 1, 4
        IF (j < k) THEN
          inc = 0
        ELSE
          inc = 1
        END IF
        b(i, j) = a(i + 1, j + inc)
      END DO
    END DO
    !!
    ans = ans + (-1)**(k + 1) * a(1, k) * det_2D(b)
    !!
  END DO
END FUNCTION det_2d_5

!----------------------------------------------------------------------------
!                                                                        DET
!----------------------------------------------------------------------------

MODULE PROCEDURE det_3D
INTEGER(I4B) :: i, n
n = SIZE(A, 3)
ALLOCATE (Ans(n))
DO i = 1, n
  Ans(i) = Det(A(:, :, i))
END DO
END PROCEDURE det_3D

!----------------------------------------------------------------------------
!                                                                        Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE Inv_2D
!Define internal variables
REAL(DFP) :: d, co(4, 4)

SELECT CASE (SIZE(A, 1))
CASE (1)
  d = det(A)
  IF (ABS(d) .LT. ZERO) THEN
    invA = 0.0_DFP
  ELSE
    invA = 1.0_DFP / d
  END IF

CASE (2)
  d = det(A)
  IF (ABS(d) .LT. ZERO) THEN
    invA = 0.0_DFP
  ELSE
    invA(1, 1) = A(2, 2) / d
    invA(1, 2) = -A(1, 2) / d
    invA(2, 1) = -A(2, 1) / d
    invA(2, 2) = A(1, 1) / d
  END IF
CASE (3)
  d = det(A)
  IF (ABS(d) .LT. ZERO) THEN
    invA = 0.0_DFP
  ELSE
    co(1, 1) = (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2))
    co(1, 2) = -(A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1))
    co(1, 3) = +(A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1))
    co(2, 1) = -(A(1, 2) * A(3, 3) - A(1, 3) * A(3, 2))
    co(2, 2) = +(A(1, 1) * A(3, 3) - A(1, 3) * A(3, 1))
    co(2, 3) = -(A(1, 1) * A(3, 2) - A(1, 2) * A(3, 1))
    co(3, 1) = +(A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2))
    co(3, 2) = -(A(1, 1) * A(2, 3) - A(1, 3) * A(2, 1))
    co(3, 3) = +(A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1))
    invA = TRANSPOSE(co(1:3, 1:3)) / d
  END IF

CASE (4)

  d = det(A)
  IF (ABS(d) .LT. ZERO) THEN
    invA = 0.0_DFP
  ELSE
    co(1, 1) = A(2, 2) * (A(3, 3) * A(4, 4) - A(3, 4) * A(4, 3)) + &
               A(2, 3) * (A(3, 4) * A(4, 2) - A(3, 2) * A(4, 4)) + &
               A(2, 4) * (A(3, 2) * A(4, 3) - A(3, 3) * A(4, 2))
    co(1, 2) = A(2, 1) * (A(3, 4) * A(4, 3) - A(3, 3) * A(4, 4)) + &
               A(2, 3) * (A(3, 1) * A(4, 4) - A(3, 4) * A(4, 1)) + &
               A(2, 4) * (A(3, 3) * A(4, 1) - A(3, 1) * A(4, 3))
    co(1, 3) = A(2, 1) * (A(3, 2) * A(4, 4) - A(3, 4) * A(4, 2)) + &
               A(2, 2) * (A(3, 4) * A(4, 1) - A(3, 1) * A(4, 4)) + &
               A(2, 4) * (A(3, 1) * A(4, 2) - A(3, 2) * A(4, 1))
    co(1, 4) = A(2, 1) * (A(3, 3) * A(4, 2) - A(3, 2) * A(4, 3)) + &
               A(2, 2) * (A(3, 1) * A(4, 3) - A(3, 3) * A(4, 1)) + &
               A(2, 3) * (A(3, 2) * A(4, 1) - A(3, 1) * A(4, 2))
    co(2, 1) = A(1, 2) * (A(3, 4) * A(4, 3) - A(3, 3) * A(4, 4)) + &
               A(1, 3) * (A(3, 2) * A(4, 4) - A(3, 4) * A(4, 2)) + &
               A(1, 4) * (A(3, 3) * A(4, 2) - A(3, 2) * A(4, 3))
    co(2, 2) = A(1, 1) * (A(3, 3) * A(4, 4) - A(3, 4) * A(4, 3)) + &
               A(1, 3) * (A(3, 4) * A(4, 1) - A(3, 1) * A(4, 4)) + &
               A(1, 4) * (A(3, 1) * A(4, 3) - A(3, 3) * A(4, 1))
    co(2, 3) = A(1, 1) * (A(3, 4) * A(4, 2) - A(3, 2) * A(4, 4)) + &
               A(1, 2) * (A(3, 1) * A(4, 4) - A(3, 4) * A(4, 1)) + &
               A(1, 4) * (A(3, 2) * A(4, 1) - A(3, 1) * A(4, 2))
    co(2, 4) = A(1, 1) * (A(3, 2) * A(4, 3) - A(3, 3) * A(4, 2)) + &
               A(1, 2) * (A(3, 3) * A(4, 1) - A(3, 1) * A(4, 3)) + &
               A(1, 3) * (A(3, 1) * A(4, 2) - A(3, 2) * A(4, 1))
    co(3, 1) = A(1, 2) * (A(2, 3) * A(4, 4) - A(2, 4) * A(4, 3)) + &
               A(1, 3) * (A(2, 4) * A(4, 2) - A(2, 2) * A(4, 4)) + &
               A(1, 4) * (A(2, 2) * A(4, 3) - A(2, 3) * A(4, 2))
    co(3, 2) = A(1, 1) * (A(2, 4) * A(4, 3) - A(2, 3) * A(4, 4)) + &
               A(1, 3) * (A(2, 1) * A(4, 4) - A(2, 4) * A(4, 1)) + &
               A(1, 4) * (A(2, 3) * A(4, 1) - A(2, 1) * A(4, 3))
    co(3, 3) = A(1, 1) * (A(2, 2) * A(4, 4) - A(2, 4) * A(4, 2)) + &
               A(1, 2) * (A(2, 4) * A(4, 1) - A(2, 1) * A(4, 4)) + &
               A(1, 4) * (A(2, 1) * A(4, 2) - A(2, 2) * A(4, 1))
    co(3, 4) = A(1, 1) * (A(2, 3) * A(4, 2) - A(2, 2) * A(4, 3)) + &
               A(1, 2) * (A(2, 1) * A(4, 3) - A(2, 3) * A(4, 1)) + &
               A(1, 3) * (A(2, 2) * A(4, 1) - A(2, 1) * A(4, 2))
    co(4, 1) = A(1, 2) * (A(2, 4) * A(3, 3) - A(2, 3) * A(3, 4)) + &
               A(1, 3) * (A(2, 2) * A(3, 4) - A(2, 4) * A(3, 2)) + &
               A(1, 4) * (A(2, 3) * A(3, 2) - A(2, 2) * A(3, 3))
    co(4, 2) = A(1, 1) * (A(2, 3) * A(3, 4) - A(2, 4) * A(3, 3)) + &
               A(1, 3) * (A(2, 4) * A(3, 1) - A(2, 1) * A(3, 4)) + &
               A(1, 4) * (A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1))
    co(4, 3) = A(1, 1) * (A(2, 4) * A(3, 2) - A(2, 2) * A(3, 4)) + &
               A(1, 2) * (A(2, 1) * A(3, 4) - A(2, 4) * A(3, 1)) + &
               A(1, 4) * (A(2, 2) * A(3, 1) - A(2, 1) * A(3, 2))
    co(4, 4) = A(1, 1) * (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)) + &
               A(1, 2) * (A(2, 3) * A(3, 1) - A(2, 1) * A(3, 3)) + &
               A(1, 3) * (A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1))
    invA = TRANSPOSE(co) / d
  END IF

CASE DEFAULT
  ! larger than 4: Gauss-Jordan elimination
  CALL inv_general(A, invA)

END SELECT

END PROCEDURE Inv_2D

!----------------------------------------------------------------------------
!                                                                        Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE Inv_3D
! define internal variables
INTEGER(I4B) :: i, n
n = SIZE(A, 3)
DO i = 1, n
  CALL Inv(invA=invA(:, :, i), A=A(:, :, i))
END DO
END PROCEDURE Inv_3D

!----------------------------------------------------------------------------
!                                                               det_general
!----------------------------------------------------------------------------

! Determinant of the n x n matrix a(1:n, 1:n), n = SIZE(a, 1), by
! Gaussian elimination with partial pivoting. For n = 0 it is 1.

PURE FUNCTION det_general(a) RESULT(ans)
  REAL(DFP), INTENT(IN) :: a(:, :)
  REAL(DFP) :: ans
  REAL(DFP) :: lu(SIZE(a, 1), SIZE(a, 1)), tmp(SIZE(a, 1)), f
  INTEGER(I4B) :: n, i, j, p

  n = SIZE(a, 1)
  lu = a(1:n, 1:n)
  ans = 1.0_DFP

  DO j = 1, n
    p = j - 1 + MAXLOC(ABS(lu(j:n, j)), 1)
    IF (lu(p, j) .EQ. 0.0_DFP) THEN
      ans = 0.0_DFP
      RETURN
    END IF
    IF (p .NE. j) THEN
      tmp = lu(j, :)
      lu(j, :) = lu(p, :)
      lu(p, :) = tmp
      ans = -ans
    END IF
    ans = ans * lu(j, j)
    DO i = j + 1, n
      f = lu(i, j) / lu(j, j)
      lu(i, j:n) = lu(i, j:n) - f * lu(j, j:n)
    END DO
  END DO
END FUNCTION det_general

!----------------------------------------------------------------------------
!                                                               inv_general
!----------------------------------------------------------------------------

! Inverse of the n x n matrix a(1:n, 1:n) by Gauss-Jordan elimination
! with partial pivoting. As for the small sizes, the matrix is treated
! as singular when ABS(det) < Zero, and then invA(1:n, 1:n) is zero.

PURE SUBROUTINE inv_general(a, invA)
  REAL(DFP), INTENT(IN) :: a(:, :)
  REAL(DFP), INTENT(INOUT) :: invA(:, :)
  REAL(DFP) :: w(SIZE(a, 1), 2 * SIZE(a, 1)), tmp(2 * SIZE(a, 1))
  INTEGER(I4B) :: n, i, j, p

  n = SIZE(a, 1)

  IF (ABS(det_general(a)) .LT. ZERO) THEN
    invA(1:n, 1:n) = 0.0_DFP
    RETURN
  END IF

  w = 0.0_DFP
  w(:, 1:n) = a(1:n, 1:n)
  DO i = 1, n
    w(i, n + i) = 1.0_DFP
  END DO

  DO j = 1, n
    p = j - 1 + MAXLOC(ABS(w(j:n, j)), 1)
    IF (p .NE. j) THEN
      tmp = w(j, :)
      w(j, :) = w(p, :)
      w(p, :) = tmp
    END IF
    w(j, :) = w(j, :) / w(j, j)
    DO i = 1, n
      IF (i .NE. j) w(i, :) = w(i, :) - w(i, j) * w(j, :)
    END DO
  END DO

  invA(1:n, 1:n) = w(:, n + 1:2 * n)
END SUBROUTINE inv_general

END SUBMODULE Methods
