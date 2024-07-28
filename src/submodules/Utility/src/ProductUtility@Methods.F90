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
! date: 22 March 2021
! summary: This submodule contains OuterProd

SUBMODULE(ProductUtility) Methods
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

MODULE PROCEDURE OuterProd_r1r1
ans = 0.0_DFP
ans = SPREAD(a, dim=2, ncopies=SIZE(b)) * &
      SPREAD(b, dim=1, ncopies=SIZE(a))
END PROCEDURE OuterProd_r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1_
INTEGER(I4B) :: ii, jj

nrow = SIZE(a)
ncol = SIZE(b)
DO CONCURRENT(ii=1:nrow, jj=1:ncol)
  ans(ii, jj) = anscoeff * ans(ii, jj) + scale * a(ii) * b(jj)
END DO
END PROCEDURE OuterProd_r1r1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1s_
INTEGER(I4B) :: ii, jj
REAL(DFP) :: s

IF (sym) THEN
  nrow = SIZE(a)
  ncol = SIZE(b)
  s = 0.5_DFP * scale

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    ans(ii, jj) = anscoeff * ans(ii, jj) + s * a(ii) * b(jj) + &
                  s * b(ii) * a(jj)
  END DO

  RETURN
END IF

CALL OuterProd_(a=a, b=b, ans=ans, anscoeff=anscoeff, scale=scale, &
                nrow=nrow, ncol=ncol)

END PROCEDURE OuterProd_r1r1s_

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1s
ans = 0.0_DFP
IF (Sym) THEN
  ans = SPREAD(0.5_DFP * a, dim=2, ncopies=SIZE(b)) &
        * SPREAD(b, dim=1, ncopies=SIZE(a)) &
        + SPREAD(0.5_DFP * b, dim=2, ncopies=SIZE(a)) &
        * SPREAD(a, dim=1, ncopies=SIZE(b))
ELSE
  ans = SPREAD(a, dim=2, ncopies=SIZE(b)) * &
        SPREAD(b, dim=1, ncopies=SIZE(a))
END IF
END PROCEDURE OuterProd_r1r1s

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 2)
  ans(:, :, ii) = OuterProd(a, b(:, ii))
END DO
END PROCEDURE OuterProd_r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r3
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 3)
  ans(:, :, :, ii) = OuterProd(a, b(:, :, ii))
END DO
END PROCEDURE OuterProd_r1r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r4
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 4)
  ans(:, :, :, :, ii) = OuterProd(a, b(:, :, :, ii))
END DO
END PROCEDURE OuterProd_r1r4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r5
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 5)
  ans(:, :, :, :, :, ii) = OuterProd(a, b(:, :, :, :, ii))
END DO
END PROCEDURE OuterProd_r1r5

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 1)
  ans(:, :, ii) = a * b(ii)
END DO
END PROCEDURE OuterProd_r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 2)
  ans(:, :, :, ii) = OuterProd(a, b(:, ii))
END DO
END PROCEDURE OuterProd_r2r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r3
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 3)
  ans(:, :, :, :, ii) = OuterProd(a, b(:, :, ii))
END DO
END PROCEDURE OuterProd_r2r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r4
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 4)
  ans(:, :, :, :, :, ii) = OuterProd(a, b(:, :, :, ii))
END DO
END PROCEDURE OuterProd_r2r4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r3r1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 1)
  ans(:, :, :, ii) = a(:, :, :) * b(ii)
END DO
END PROCEDURE OuterProd_r3r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r3r2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 2)
  ans(:, :, :, :, ii) = OuterProd(a, b(:, ii))
END DO
END PROCEDURE OuterProd_r3r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r3r3
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 3)
  ans(:, :, :, :, :, ii) = OuterProd(a, b(:, :, ii))
END DO
END PROCEDURE OuterProd_r3r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r4r1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 1)
  ans(:, :, :, :, ii) = a * b(ii)
END DO
END PROCEDURE OuterProd_r4r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r4r2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b, 2)
  ans(:, :, :, :, :, ii) = OuterProd(a, b(:, ii))
END DO
END PROCEDURE OuterProd_r4r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r5r1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(b)
  ans(:, :, :, :, :, ii) = a * b(ii)
END DO
END PROCEDURE OuterProd_r5r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r2
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r3
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r1r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r4
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r1r4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r2r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r2r2
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r2r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r2r3
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r2r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r3r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r3r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r3r2
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r3r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r4r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r4r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r1r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r2r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r1r2
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r2r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r1r3
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r2r1r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r2r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r2r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r2r2
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r2r2r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r3r1r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r3r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r3r1r2
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r3r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r3r2r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r3r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r4r1r1
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r4r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r1r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r1r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r1r2
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r1r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r1r3
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r1r1r3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r2r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r1r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r2r2
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r1r2r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r3r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r1r3r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r2r1r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r2r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r2r1r2
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r2r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r2r2r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r2r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r3r1r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r1r3r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r1r1r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r2r1r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r1r1r2
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r2r1r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r1r2r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r2r1r2r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r2r2r1r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r2r2r1r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r3r1r1r1
ans = OuterProd(OuterProd(a, OuterProd(b, c)), d)
END PROCEDURE OuterProd_r3r1r1r1

END SUBMODULE Methods
