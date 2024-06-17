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
    invA = 1.0 / d
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

END SUBMODULE Methods
