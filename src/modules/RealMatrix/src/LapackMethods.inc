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

PUBLIC :: JacobiMethod

!----------------------------------------------------------------------------
!                                                         JacobiMethod@LAPACK
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Returns all the eigenvalues of symmetric matrix
!
!# Introduction
!
! This subroutine computes all eigenvalues and eigenvectors of a real
! symmetric N × N matrix `Mat`.
! -  On output, elements of `Mat` above the diagonal are destroyed.
! -  `eigenvalues` is a vector of length N that returns the eigenvalues of
! `Mat`.
! -  `EigenVectors` is an `N × N` matrix whose columns contain on output,
! the normalized eigenvectors (directions) of `Mat`.
! - `maxIter` returns the number of Jacobi rotations that were required.
!
! ### Reference:: Numerical Reciepe in Fortran, Page 1225
!

INTERFACE
MODULE PURE SUBROUTINE eig_jacobi_method( mat, eigenValues, eigenVectors, &
  & maxIter )
  REAL( DFP ), INTENT( INOUT ) :: mat( :, : )
  REAL( DFP ), INTENT( INOUT ) :: eigenValues( : )
  REAL( DFP ), INTENT( INOUT ) :: eigenVectors( :, : )
  INTEGER( I4B ), INTENT( IN ) :: maxIter
END SUBROUTINE eig_jacobi_method
END INTERFACE

INTERFACE JacobiMethod
  MODULE PROCEDURE eig_jacobi_method
END INTERFACE JacobiMethod
