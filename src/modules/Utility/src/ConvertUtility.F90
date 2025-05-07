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

MODULE ConvertUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Convert
PUBLIC :: Convert_
PUBLIC :: ConvertSafe

!----------------------------------------------------------------------------
!                                                   Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Rearrange the degrees of freedom in a finite element matrix
!
!# Introduction
!
! This subroutine changes the storage pattern of a two-d matrix
!  - Usually element matrix in easifem are stored in `FMT_DOF`
!  - Global matrices/tanmat, however, are stored in `FMT_Nodes`
!  - This subroutine is, therefore, in settings or adding values in
! [[SparseMatrix_]].
!
! > This subroutine converts changes the storage format of dense matrix.
! Usually, elemental finite element matrix is stored in `DOF_FMT`, and global
! matrix/ tanmat, may be stored in `Nodes_FMT`.
!
!@note
! All dof should have the same order of interpolation, therefore,
! this routine works when matrix is square.
!@endnote

INTERFACE Convert
  MODULE PURE SUBROUTINE convert_1(From, To, Conversion, nns, tdof)
    REAL(DFP), INTENT(IN) :: From(:, :)
    !! Matrix in one format
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: To(:, :)
    !! Matrix is desired format
    INTEGER(I4B), INTENT(IN) :: Conversion
    !! `Conversion` can be `NodesToDOF` or `DOFToNodes`
    INTEGER(I4B), INTENT(IN) :: nns, tdof
  END SUBROUTINE convert_1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                   Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Rearrange the degrees of freedom in a finite element matrix
!
!# Introduction
!
! This subroutine changes the storage pattern of a two-d matrix
!  - Usually element matrix in easifem are stored in `FMT_DOF`
!  - Global matrices/tanmat, however, are stored in `FMT_Nodes`
!  - This subroutine is, therefore, in settings or adding values in
! [[SparseMatrix_]].
!
! > This subroutine converts changes the storage format of dense matrix.
! Usually, elemental finite element matrix is stored in `DOF_FMT`, and global
! matrix/ tanmat, may be stored in `Nodes_FMT`.
!
!@note
! All dof should have the same order of interpolation, therefore,
! this routine works when matrix is square.
!@endnote

INTERFACE ConvertSafe
  MODULE PURE SUBROUTINE convert_1_safe(From, To, Conversion, nns, tdof)
    REAL(DFP), INTENT(IN) :: From(:, :)
    !! Matrix in one format
    REAL(DFP), INTENT(INOUT) :: To(:, :)
    !! Matrix is desired format
    INTEGER(I4B), INTENT(IN) :: Conversion
    !! `Conversion` can be `NodesToDOF` or `DOFToNodes`
    INTEGER(I4B), INTENT(IN) :: nns, tdof
  END SUBROUTINE convert_1_safe
END INTERFACE ConvertSafe

!----------------------------------------------------------------------------
!                                                    Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine converts rank4  matrix to rank2 matrix
!
!# Introduction
!
!  This subroutine converts rank4 matrix to rank2 matrix
! This routine can be used in Space-Time FEM
!
! - The first and second dimension of From is spatial nodes
! - The third and forth dimension of From is temporal nodes
!
! - In this way `From(:, :, a, b)` denotes the `a,b` block matrix
!
! Format of To  matrix
!
! Contains the block matrix structure in 2D.

INTERFACE Convert
  MODULE PURE SUBROUTINE convert_2(From, To)
    REAL(DFP), INTENT(IN) :: From(:, :, :, :)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: To(:, :)
  END SUBROUTINE convert_2
END INTERFACE Convert

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  convert without allocation

INTERFACE Convert_
  MODULE PURE SUBROUTINE convert2_(From, To, nrow, ncol)
    REAL(DFP), INTENT(IN) :: From(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: To(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE convert2_
END INTERFACE Convert_

!----------------------------------------------------------------------------
!                                                    Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine converts rank4  matrix to rank2 matrix
!

INTERFACE Convert
  MODULE PURE SUBROUTINE convert_3(From, To)
    REAL(DFP), INTENT(IN) :: From(:, :, :, :, :, :)
  !! I, J, ii, jj, a, b
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: To(:, :, :, :)
  !! I, J, a, b
  END SUBROUTINE convert_3
END INTERFACE Convert

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  convert without allocation

INTERFACE Convert_
  MODULE PURE SUBROUTINE convert3_(From, To, dim1, dim2, dim3, dim4)
    REAL(DFP), INTENT(IN) :: From(:, :, :, :, :, :)
    REAL(DFP), INTENT(INOUT) :: To(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE convert3_
END INTERFACE Convert_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConvertUtility
