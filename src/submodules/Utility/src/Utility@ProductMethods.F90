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

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This submodule contains outerprod

SUBMODULE(Utility) ProductMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 VectorProd
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorProduct_1
c(1) = a(2) * b(3) - a(3) * b(2)
c(2) = a(3) * b(1) - a(1) * b(3)
c(3) = a(1) * b(2) - a(2) * b(1)
END PROCEDURE vectorProduct_1

!----------------------------------------------------------------------------
!                                                                 VectorProd
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorProduct_2
c(1) = a(2) * b(3) - a(3) * b(2)
c(2) = a(3) * b(1) - a(1) * b(3)
c(3) = a(1) * b(2) - a(2) * b(1)
END PROCEDURE vectorProduct_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1
ans = 0.0_DFP
ans = SPREAD(a, dim=2, ncopies=SIZE(b)) * &
      & SPREAD(b, dim=1, ncopies=SIZE(a))
END PROCEDURE outerprod_r1r1

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1s
ans = 0.0_DFP
IF (Sym) THEN
  ans = SPREAD(0.5_DFP * a, dim=2, ncopies=SIZE(b)) &
      & * SPREAD(b, dim=1, ncopies=SIZE(a)) &
      & + SPREAD(0.5_DFP * b, dim=2, ncopies=SIZE(a)) &
      & * SPREAD(a, dim=1, ncopies=SIZE(b))
ELSE
  ans = SPREAD(a, dim=2, ncopies=SIZE(b)) * &
          & SPREAD(b, dim=1, ncopies=SIZE(a))
END IF
END PROCEDURE outerprod_r1r1s

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r2
INTEGER(I4B) :: ii
do ii = 1, size(b, 2)
  ans(:, :, ii) = outerprod(a, b(:, ii))
end do
END PROCEDURE outerprod_r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r3
INTEGER(I4B) :: ii
do ii = 1, size(b, 3)
  ans(:, :, :, ii) = outerprod(a, b(:, :, ii))
end do
END PROCEDURE outerprod_r1r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r4
INTEGER(I4B) :: ii
do ii = 1, size(b, 4)
  ans(:, :, :, :, ii) = outerprod(a, b(:, :, :, ii))
end do
END PROCEDURE outerprod_r1r4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r5
INTEGER(I4B) :: ii
do ii = 1, size(b, 5)
  ans(:, :, :, :, :, ii) = outerprod(a, b(:, :, :, :, ii))
end do
END PROCEDURE outerprod_r1r5

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r1
INTEGER(I4B) :: ii
do ii = 1, size(b, 1)
  ans(:, :, ii) = a * b(ii)
end do
END PROCEDURE outerprod_r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 2)
  ans(:, :, :, ii) = outerprod(a, b(:, ii))
END DO
END PROCEDURE outerprod_r2r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r3
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 3)
  ans(:, :, :, :, ii) = outerprod(a, b(:, :, ii))
END DO
END PROCEDURE outerprod_r2r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r4
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 4)
  ans(:, :, :, :, :, ii) = outerprod(a, b(:, :, :, ii))
END DO
END PROCEDURE outerprod_r2r4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r3r1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 1)
  ans(:, :, :, ii) = a(:, :, :) * b(ii)
END DO
END PROCEDURE outerprod_r3r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r3r2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 2)
  ans(:, :, :, :, ii) = outerprod(a, b(:, ii))
END DO
END PROCEDURE outerprod_r3r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r3r3
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 3)
  ans(:, :, :, :, :, ii) = outerprod(a, b(:, :, ii))
END DO
END PROCEDURE outerprod_r3r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r4r1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 1)
  ans(:, :, :, :, ii) = a * b(ii)
END DO
END PROCEDURE outerprod_r4r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r4r2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 2)
  ans(:, :, :, :, :, ii) = outerprod(a, b(:, ii))
END DO
END PROCEDURE outerprod_r4r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r5r1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b)
  ans(:, :, :, :, :, ii) = a * b(ii)
END DO
END PROCEDURE outerprod_r5r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r2
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r3
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r1r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r4
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r1r4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r2r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r2r2
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r2r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r2r3
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r2r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r3r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r3r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r3r2
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r3r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r4r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r1r4r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r1r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r2r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r1r2
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r2r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r1r3
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r2r1r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r2r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r2r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r2r2
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r2r2r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r3r1r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r3r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r3r1r2
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r3r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r3r2r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r3r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r4r1r1
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_r4r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r1r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r1r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r1r2
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r1r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r1r3
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r1r1r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r2r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r1r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r2r2
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r1r2r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r1r3r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r1r3r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r2r1r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r2r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r2r1r2
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r2r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r2r2r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r2r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r1r3r1r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r1r3r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r1r1r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r2r1r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r1r1r2
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r2r1r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r1r2r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r2r1r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r2r2r1r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r2r2r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_r3r1r1r1
ans = outerprod(outerprod(a, outerprod(b, c)), d)
END PROCEDURE outerprod_r3r1r1r1

END SUBMODULE ProductMethods
