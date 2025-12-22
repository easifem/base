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

MODULE MatmulUtility
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: MATMUL
PUBLIC :: MATMUL_

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j,k) = a1(i,j,k,l)*a2(l)`

INTERFACE
  MODULE PURE FUNCTION matmul_r4_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r4_r1
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j,k) = a1(i,j,k,l)*a2(l)`

INTERFACE
  MODULE PURE SUBROUTINE matmul_r4_r1_(a1, a2, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matmul_r4_r1_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r4_r1_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j,k,m) = a1(i,j,k,l)*a2(l,m)`

INTERFACE
  MODULE PURE FUNCTION matmul_r4_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), SIZE(a2, 2))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r4_r2
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r4_r2_(a1, a2, ans, dim1, dim2, dim3, dim4)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE matmul_r4_r2_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r4_r2_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j,k,m,n) = a1(i,j,k,l)*a2(l,m,n)`

INTERFACE
  MODULE PURE FUNCTION matmul_r4_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), &
      & SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r4_r3
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r4_r3_(a1, a2, ans, dim1, dim2, dim3, dim4, &
                                       dim5)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4, dim5
  END SUBROUTINE matmul_r4_r3_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r4_r3_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j,k,m,n,o) = a1(i,j,k,l)*a2(l,m,n,o)`

INTERFACE
  MODULE PURE FUNCTION matmul_r4_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3), &
      & SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r4_r4
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r4_r4_(a1, a2, ans, dim1, dim2, dim3, &
                                       dim4, dim5, dim6)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4, dim5, dim6
  END SUBROUTINE matmul_r4_r4_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r4_r4_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                             Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank3  and rank1 array
!
!# Introduction
!
! This fuction performs following task
! `ans(i,j) = a1(i,j,k)*a2(k)`

INTERFACE
  MODULE PURE FUNCTION matmul_r3_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r3_r1
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                   Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r3_r1_(a1, a2, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE matmul_r3_r1_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r3_r1_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         matmul for rank3 and rank2
!
!# Introduction
! This fuction performs following task
! `ans(i,j,l) = a1(i,j,k)*a2(k,l)`

INTERFACE
  MODULE PURE FUNCTION matmul_r3_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), &
      & SIZE(a2, 2))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r3_r2
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r3_r2_(a1, a2, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matmul_r3_r2_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r3_r2_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                              Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j,l,m) = a1(i,j,k) * a2(k,l,m)`

INTERFACE
  MODULE PURE FUNCTION matmul_r3_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), &
      & SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r3_r3
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                    Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r3_r3_(a1, a2, ans, dim1, dim2, dim3, &
                                       dim4)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE matmul_r3_r3_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r3_r3_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                              Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j,l,m,n) = a1(i,j,k) * a2(k,l,m,n)`

INTERFACE
  MODULE PURE FUNCTION matmul_r3_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), &
      & SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r3_r4
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                     Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r3_r4_(a1, a2, ans, dim1, dim2, dim3, &
                                       dim4, dim5)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4, dim5
  END SUBROUTINE matmul_r3_r4_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r3_r4_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank2 and rank3 array
!
!# Introduction
!
! This fuction performs following task
! `ans(i,k,l) = a1(i,j)*a2(j,k,l)`

INTERFACE
  MODULE PURE FUNCTION matmul_r2_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r2_r3
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                    Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r2_r3_(a1, a2, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matmul_r2_r3_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r2_r3_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank2 and rank3 array
!
!# Introduction
!
! This fuction performs following task
! `ans(i,k,l,m) = a1(i,j)*a2(j,k,l,m)`

INTERFACE
  MODULE PURE FUNCTION matmul_r2_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a2, 2), &
      & SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r2_r4
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                 Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r2_r4_(a1, a2, ans, dim1, dim2, dim3, dim4)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE matmul_r2_r4_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r2_r4_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                              Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank1 and rank3 array
!
!# Introduction
!
! This fuction performs following task
! `ans = a1(i)*a2(i)`

INTERFACE
  MODULE PURE FUNCTION matmul_r1_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r1_r1
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                    Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r1_r1_(a1, a2, ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP), INTENT(INOUT) :: ans
  END SUBROUTINE matmul_r1_r1_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r1_r1_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                              Matmul@Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank1 and rank3 array
!
!# Introduction
!
! This fuction performs following task
! `ans(j,k) = a1(i)*a2(i,j,k)`

INTERFACE
  MODULE PURE FUNCTION matmul_r1_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r1_r3
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                    Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r1_r3_(a1, a2, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE matmul_r1_r3_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r1_r3_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!                                                                      Matmul
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank4 and rank1 array
!
!# Introduction
!
! `ans(j,k,l) = a1(i) * a2(i,j,k,l)`

INTERFACE
  MODULE PURE FUNCTION matmul_r1_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r1_r4
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                                    Matmul_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matmul_r1_r4_(a1, a2, ans, dim1, dim2, dim3)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matmul_r1_r4_
END INTERFACE

INTERFACE MATMUL_
  MODULE PROCEDURE matmul_r1_r4_
END INTERFACE MATMUL_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MatmulUtility

