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

INTERFACE
MODULE PURE SUBROUTINE convert_1( From, To, Conversion, nns, tdof )
  REAL( DFP ), INTENT( IN ) :: From( :, : )
    !! Matrix in one format
  REAL( DFP ), INTENT( INOUT ), ALLOCATABLE :: To( :, : )
    !! Matrix is desired format
  INTEGER( I4B ), INTENT( IN ) :: Conversion
    !! `Conversion` can be `NodesToDOF` or `DOFToNodes`
  INTEGER( I4B ), INTENT( IN ) :: nns, tdof
END SUBROUTINE convert_1
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE convert_1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                    Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine converts rank4  matrix to rank2 matrix
!

INTERFACE
MODULE PURE SUBROUTINE convert_2( From, To )
  REAL( DFP ), INTENT( IN ) :: From( :, :, :, : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: To( :, : )
END SUBROUTINE convert_2
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE convert_2
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                    Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine converts rank4  matrix to rank2 matrix
!

INTERFACE
MODULE PURE SUBROUTINE convert_3( From, To )
  REAL( DFP ), INTENT( IN ) :: From( :, :, :, :, :, : )
  !! I, J, ii, jj, a, b
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: To( :, :, :, : )
  !! I, J, a, b
END SUBROUTINE convert_3
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE convert_3
END INTERFACE Convert

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConvertUtility