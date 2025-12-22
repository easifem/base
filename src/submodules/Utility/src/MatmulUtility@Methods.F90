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
! summary:         Methods for matrix multiplication

SUBMODULE(MatmulUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r1
INTEGER(I4B) :: dim1, dim2, dim3
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE matmul_r4_r1

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r1_
INTEGER(I4B) :: ii, jj, kk, ll

dim1 = SIZE(a1, 1)
dim2 = SIZE(a1, 2)
dim3 = SIZE(a1, 3)

ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

DO ll = 2, SIZE(a2)
  DO kk = 1, dim3
    DO jj = 1, dim2
      DO ii = 1, dim1
        ans(ii, jj, kk) = ans(ii, jj, kk) + a2(ll) * a1(ii, jj, kk, ll)
      END DO
    END DO
  END DO
END DO
END PROCEDURE matmul_r4_r1_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r2
INTEGER(I4B) :: dim1, dim2, dim3, dim4
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
             dim4=dim4)
END PROCEDURE matmul_r4_r2

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r2_
INTEGER(I4B) :: ii

dim4 = SIZE(a2, 2)

DO ii = 1, dim4
  call Matmul_(a1=a1, a2=a2(:, ii), ans=ans(:,:,:,ii), dim1=dim1, dim2=dim2, &
               dim3=dim3)
END DO
END PROCEDURE matmul_r4_r2_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r3
INTEGER(I4B) :: dim1, dim2, dim3, dim4, dim5
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
             dim4=dim4, dim5=dim5)
END PROCEDURE matmul_r4_r3

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r3_
INTEGER(I4B) :: ii

dim5 = SIZE(a2, 3)

DO ii = 1, dim5
  CALL Matmul_(a1=a1, a2=a2(:, :, ii), ans=ans(:, :, :, :, ii), dim1=dim1, &
               dim2=dim2, dim3=dim3, dim4=dim4)
END DO
END PROCEDURE matmul_r4_r3_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r4
INTEGER(I4B) :: dim1, dim2, dim3, dim4, dim5, dim6
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
             dim4=dim4, dim5=dim5, dim6=dim6)
END PROCEDURE matmul_r4_r4

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r4_
INTEGER(I4B) :: ii

dim6 = SIZE(a2, 4)

DO ii = 1, dim6
  CALL Matmul_(a1=a1, a2=a2(:, :, :, ii), ans=ans(:, :, :, :, :, ii), &
               dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4, dim5=dim5)
END DO
END PROCEDURE matmul_r4_r4_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r1
INTEGER(I4B) :: nrow, ncol
CALL Matmul_(a1=a1, a2=a2, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE matmul_r3_r1

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r1_
INTEGER(I4B) :: ii, jj, kk, tsize

nrow = SIZE(a1, 1)
ncol = SIZE(a1, 2)
tsize = MIN(SIZE(a2), SIZE(a1, 3))

ans(1:nrow, 1:ncol) = 0.0_DFP

DO kk = 1, tsize
  DO jj = 1, ncol
    DO ii = 1, nrow
      ans(ii, jj) = ans(ii, jj) + a2(kk) * a1(ii, jj, kk)
    END DO
  END DO
END DO
END PROCEDURE matmul_r3_r1_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r2
INTEGER(I4B) :: dim1, dim2, dim3
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE matmul_r3_r2

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r2_
INTEGER(I4B) :: ii

dim3 = SIZE(a2, 2)

DO ii = 1, dim3
  CALL Matmul_(a1=a1, a2=a2(:, ii), ans=ans(:, :, ii), nrow=dim1, ncol=dim2)
END DO
END PROCEDURE matmul_r3_r2_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r3
INTEGER(I4B) :: dim1, dim2, dim3, dim4
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
             dim4=dim4)
END PROCEDURE matmul_r3_r3

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r3_
INTEGER(I4B) :: ii

dim4 = SIZE(a2, 3)

DO ii = 1, dim4
  CALL Matmul_(a1=a1, a2=a2(:, :, ii), ans=ans(:, :, :, ii), &
               dim1=dim1, dim2=dim2, dim3=dim3)
END DO
END PROCEDURE matmul_r3_r3_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r4
INTEGER(I4B) :: dim1, dim2, dim3, dim4, dim5
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
             dim4=dim4, dim5=dim5)
END PROCEDURE matmul_r3_r4

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r4_
INTEGER(I4B) :: ii

dim5 = SIZE(a2, 4)

DO ii = 1, dim5
  CALL Matmul_(a1=a1, a2=a2(:, :, :, ii), ans=ans(:, :, :, :, ii), &
               dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
END DO
END PROCEDURE matmul_r3_r4_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r2_r3
INTEGER(I4B) :: dim1, dim2, dim3
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE matmul_r2_r3

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r2_r3_
INTEGER(I4B) :: ii

dim1 = SIZE(a1, 1)
dim2 = SIZE(a2, 2)
dim3 = SIZE(a2, 3)

DO ii = 1, dim3
  ans(1:dim1, 1:dim2, ii) = MATMUL(a1, a2(:, :, ii))
END DO
END PROCEDURE matmul_r2_r3_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r2_r4
INTEGER(I4B) :: dim1, dim2, dim3, dim4
CALL Matmul_(a1=a1, a2=a2, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
             dim4=dim4)
END PROCEDURE matmul_r2_r4

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r2_r4_
INTEGER(I4B) :: ii

dim4 = SIZE(a2, 4)
DO ii = 1, dim4
  CALL Matmul_(a1=a1, a2=a2(:, :, :, ii), ans=ans(:, :, :, ii), &
               dim1=dim1, dim2=dim2, dim3=dim3)
END DO
END PROCEDURE matmul_r2_r4_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r1_r1
ans = DOT_PRODUCT(a1, a2)
END PROCEDURE matmul_r1_r1

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r1_r1_
ans = DOT_PRODUCT(a1, a2)
END PROCEDURE matmul_r1_r1_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r1_r3
INTEGER(I4B) :: nrow, ncol
CALL Matmul_(a1=a1, a2=a2, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE matmul_r1_r3

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r1_r3_
INTEGER(I4B) :: ii, jj, kk, tsize

nrow = SIZE(a2, 2)
ncol = SIZE(a2, 3)
tsize = SIZE(a1)

ans(1:nrow, 1:ncol) = 0.0_DFP

DO kk = 1, ncol
  DO jj = 1, nrow
    DO ii = 1, tsize
      ans(jj, kk) = ans(jj, kk) + a1(ii) * a2(ii, jj, kk)
    END DO
  END DO
END DO
END PROCEDURE matmul_r1_r3_

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r1_r4
INTEGER(I4B) :: ii
ans = a1(1) * a2(1, :, :, :)
DO ii = 2, SIZE(a1)
  ans = ans + a1(ii) * a2(ii, :, :, :)
END DO
END PROCEDURE matmul_r1_r4

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r1_r4_
INTEGER(I4B) :: ii, jj, kk, ll, tsize

dim1 = SIZE(a2, 2)
dim2 = SIZE(a2, 3)
dim3 = SIZE(a2, 4)
tsize = SIZE(a1)

ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

DO ll = 1, dim3
  DO kk = 1, dim2
    DO jj = 1, dim1
      DO ii = 1, tsize
        ans(jj, kk, ll) = ans(jj, kk, ll) + a1(ii) * a2(ii, jj, kk, ll)
      END DO
    END DO
  END DO
END DO

END PROCEDURE matmul_r1_r4_

END SUBMODULE Methods
