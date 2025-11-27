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
!                                                               OTimesTilda
!----------------------------------------------------------------------------

MODULE PROCEDURE OTimesTilda1
INTEGER(I4B) :: sa(2), sb(2)
INTEGER(I4B) :: ii, jj, pp, qq

sa = SHAPE(a)
sb = SHAPE(b)

nrow = sa(1) * sb(1)
ncol = sa(2) * sb(2)

DO CONCURRENT(ii=1:sa(1), jj=1:sa(2), pp=1:sb(1), qq=1:sb(2))
  ans((ii - 1) * sb(1) + pp, (jj - 1) * sb(2) + qq) = &
    anscoeff * ans((ii - 1) * sb(1) + pp, (jj - 1) * sb(2) + qq) + &
    scale * a(ii, jj) * b(pp, qq)
END DO

END PROCEDURE OTimesTilda1

!----------------------------------------------------------------------------
!                                                                OTimesTilda
!----------------------------------------------------------------------------

MODULE PROCEDURE OTimesTilda2
INTEGER(I4B) :: sa, sb
INTEGER(I4B) :: ii, jj

sa = SIZE(a)
sb = SIZE(b)

tsize = sa * sb

DO CONCURRENT(ii=1:sa, jj=1:sb)
  ans((ii - 1) * sb + jj) = &
    anscoeff * ans((ii - 1) * sb + jj) + scale * a(ii) * b(jj)
END DO

END PROCEDURE OTimesTilda2

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

MODULE PROCEDURE OuterProd_r1r2_
INTEGER(I4B) :: ii
dim1 = SIZE(a)
dim2 = SIZE(b, 1)
dim3 = SIZE(b, 2)
DO ii = 1, dim3
  CALL OuterProd_(a=a, b=b(1:dim2, ii), ans=ans(1:dim1, 1:dim2, ii), &
                  anscoeff=anscoeff, scale=scale, &
                  nrow=dim1, ncol=dim2)
END DO
END PROCEDURE OuterProd_r1r2_

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

MODULE PROCEDURE OuterProd_r2r1_
INTEGER(I4B) :: ii
dim1 = SIZE(a, 1)
dim2 = SIZE(a, 2)
dim3 = SIZE(b)

DO ii = 1, dim3
  ans(1:dim1, 1:dim2, ii) = anscoeff * ans(1:dim1, 1:dim2, ii) + &
                            scale * a(1:dim1, 1:dim2) * b(ii)
END DO
END PROCEDURE OuterProd_r2r1_

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

MODULE PROCEDURE OuterProd_r2r2_
INTEGER(I4B) :: ii

dim4 = SIZE(b, 2)

DO ii = 1, dim4
  CALL OuterProd_( &
    a=a, b=b(:, ii), ans=ans(:, :, :, ii), anscoeff=anscoeff, &
    scale=scale, dim1=dim1, dim2=dim2, dim3=dim3)
END DO
END PROCEDURE OuterProd_r2r2_

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

! ans(i, j, k) = anscoeff * ans + scale * (a(i) * b(j)) * c(k))
MODULE PROCEDURE OuterProd_r1r1r1_
REAL(DFP) :: scale0
INTEGER(I4B) :: kk

dim1 = SIZE(a)
dim2 = SIZE(b)
dim3 = SIZE(c)

DO kk = 1, dim3
  scale0 = scale * c(kk)
  CALL OuterProd_(a=a, b=b, ans=ans(:, :, kk), nrow=dim1, ncol=dim2, &
                  anscoeff=anscoeff, scale=scale0)
END DO
END PROCEDURE OuterProd_r1r1r1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r2
ans = OuterProd(OuterProd(a, b), c)
END PROCEDURE OuterProd_r1r1r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OuterProd_r1r1r2_
INTEGER(I4B) :: ii

dim4 = SIZE(c, 2)

DO ii = 1, dim4
  CALL OuterProd_(a=a, b=b, c=c(:, ii), ans=ans(:, :, :, ii), &
                  dim1=dim1, dim2=dim2, dim3=dim3, anscoeff=anscoeff, &
                  scale=scale)
END DO
END PROCEDURE OuterProd_r1r1r2_

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

! ans = OuterProd(OuterProd(a, b), c)
MODULE PROCEDURE OuterProd_r2r1r1_
REAL(DFP) :: scale0
INTEGER(I4B) :: kk

dim1 = SIZE(a, 1)
dim2 = SIZE(a, 2)
dim3 = SIZE(b)
dim4 = SIZE(c)

DO kk = 1, dim4
  scale0 = scale * c(kk)
  CALL OuterProd_(a=a, b=b, ans=ans(:, :, :, kk), dim1=dim1, dim2=dim2, &
                  dim3=dim3, anscoeff=anscoeff, scale=scale0)
END DO
END PROCEDURE OuterProd_r2r1r1_

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
