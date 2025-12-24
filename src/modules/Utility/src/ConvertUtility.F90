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
  MODULE PURE SUBROUTINE obj_Convert1(from, to, conversion, nns, tdof)
    REAL(DFP), INTENT(IN) :: from(:, :)
    !! Matrix in one format
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: to(:, :)
    !! Matrix is desired format
    INTEGER(I4B), INTENT(IN) :: conversion
    !! `conversion` can be `NodestoDOF` or `DOFtoNodes`
    INTEGER(I4B), INTENT(IN) :: nns, tdof
  END SUBROUTINE obj_Convert1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                                 Convert_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-20
! summary: Like Convert_1, but no allocation

INTERFACE Convert_
 MODULE PURE SUBROUTINE obj_Convert_1(from, to, conversion, nns, tdof, nrow, &
                                       ncol)
    REAL(DFP), INTENT(IN) :: from(:, :)
    !! Matrix in one format
    REAL(DFP), INTENT(INOUT) :: to(:, :)
    !! Matrix is desired format
    INTEGER(I4B), INTENT(IN) :: conversion
    !! `conversion` can be `NodestoDOF` or `DOFtoNodes`
    INTEGER(I4B), INTENT(IN) :: nns, tdof
    !! number of nodes in space and tdod
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of data written in to
  END SUBROUTINE obj_Convert_1
END INTERFACE Convert_

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
  MODULE PURE SUBROUTINE obj_ConvertSafe1(from, to, conversion, nns, tdof)
    REAL(DFP), INTENT(IN) :: from(:, :)
    !! Matrix in one format
    REAL(DFP), INTENT(INOUT) :: to(:, :)
    !! Matrix is desired format
    INTEGER(I4B), INTENT(IN) :: conversion
    !! `conversion` can be `NodestoDOF` or `DOFtoNodes`
    INTEGER(I4B), INTENT(IN) :: nns, tdof
  END SUBROUTINE obj_ConvertSafe1
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
! - The first and second dimension of from is spatial nodes
! - The third and forth dimension of from is temporal nodes
!
! - In this way `from(:, :, a, b)` denotes the `a,b` block matrix
!
! Format of to  matrix
!
! Contains the block matrix structure in 2D.

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert2(from, to)
    REAL(DFP), INTENT(IN) :: from(:, :, :, :)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: to(:, :)
  END SUBROUTINE obj_Convert2
END INTERFACE Convert

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  convert without allocation

INTERFACE Convert_
  MODULE PURE SUBROUTINE obj_Convert_2(from, to, nrow, ncol)
    REAL(DFP), INTENT(IN) :: from(:, :, :, :)
    REAL(DFP), INTENT(INOUT) :: to(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_Convert_2
END INTERFACE Convert_

!----------------------------------------------------------------------------
!                                                    Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine converts rank4  matrix to rank2 matrix
!

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert3(from, to)
    REAL(DFP), INTENT(IN) :: from(:, :, :, :, :, :)
  !! I, J, ii, jj, a, b
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: to(:, :, :, :)
  !! I, J, a, b
  END SUBROUTINE obj_Convert3
END INTERFACE Convert

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  convert without allocation

INTERFACE Convert_
  MODULE PURE SUBROUTINE obj_Convert_3(from, to, dim1, dim2, dim3, dim4)
    REAL(DFP), INTENT(IN) :: from(:, :, :, :, :, :)
    REAL(DFP), INTENT(INOUT) :: to(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE obj_Convert_3
END INTERFACE Convert_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConvertUtility
