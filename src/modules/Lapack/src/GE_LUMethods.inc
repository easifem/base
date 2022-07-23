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

PUBLIC :: getLU

!----------------------------------------------------------------------------
!                                                                   getLU@LU
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: DGETF2 computes an LU factorization of a general m-by-n matrix A
!
!# Introduction
!
!  Using partial pivoting with row interchanges.
!
! DGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm.

INTERFACE
MODULE SUBROUTINE getLU_1(A, LU, IPIV, RCOND, NORM)
  REAL( DFP ), INTENT( IN ) :: A( :, : )
    !! Matrix to be factored
  REAL( DFP ), INTENT( OUT ) :: LU( :, : )
    !! LU factorization, the unit diagonal elements of L are not stored.
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: IPIV( : )
    !! IPIV is INTEGER array,row i of the matrix was interchanged with row
    !! IPIV(i).
  REAL( DFP ), OPTIONAL, INTENT( OUT ) :: RCOND
  CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: NORM
END SUBROUTINE getLU_1
END INTERFACE

INTERFACE getLU
  MODULE PROCEDURE getLU_1
END INTERFACE getLU

