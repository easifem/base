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

MODULE CSRMatrix_MatrixMarketIO
USE GlobalData, ONLY: DFPC, I4B, DFP, LGT, stdout, stderr, stdin
IMPLICIT NONE
PRIVATE

INTERFACE
  MODULE SUBROUTINE ParseHeader(aline, h1, h2, h3, h4, h5, ierr, errmsg)
    CHARACTER(*), INTENT(IN) :: aline
    CHARACTER(*), INTENT(OUT) :: h1
    CHARACTER(*), INTENT(OUT) :: h2
    CHARACTER(*), INTENT(OUT) :: h3
    CHARACTER(*), INTENT(OUT) :: h4
    CHARACTER(*), INTENT(OUT) :: h5
    INTEGER(I4B), INTENT(OUT) :: ierr
    CHARACTER(*), INTENT(OUT) :: errmsg
  END SUBROUTINE ParseHeader
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 MMRead
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  23-01-19
! summary: Read sparse matrix from matrix market format.
!
!# Introduction
!
! This routine reads the sparse matrix from matrix market format.
!
! The matrix market format is described here:
! https://math.nist.gov/MatrixMarket/formats.html
!
! - Matrix should contains real values
! - The forth argumnet of header should be real

INTERFACE
  MODULE SUBROUTINE MMRead(unitno, rep, field, symm, rows, cols, nnz, &
      & indx, jndx, rval, ival, cval)
    INTEGER(I4B), INTENT(IN) :: unitno
    !! unit number of file
    CHARACTER(*), INTENT(OUT) :: rep
    !! coordinate <-- sparse array in COO format
    !! array <-- dense array
    CHARACTER(*), INTENT(OUT) :: field
    !! real
    !! integer
    !! pattern
    !! complex
    CHARACTER(*), INTENT(OUT) :: symm
    !! symmetric <-- if the matrix is symmetric
    !! skew-symmetric <-- if the matrix is skew-symmetric
    !! general <-- if the matrix is general
    !! hermitian <-- if the matrix is complex and symmetric
    INTEGER(I4B), INTENT(OUT) :: rows
    !! number of rows in matrix
    INTEGER(I4B), INTENT(OUT) :: cols
    !! number of columns in matrix
    INTEGER(I4B), INTENT(OUT) :: nnz
    !! number of nonzero elements
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: indx(:)
    !! row number (index)
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: jndx(:)
    !! col number (index)
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(OUT) :: rval(:)
    !! real value needed when field is `real`
    INTEGER(I4B), OPTIONAL, ALLOCATABLE, INTENT(OUT) :: ival(:)
    !! integer value needed when field is `integer`
    COMPLEX(DFPC), OPTIONAL, ALLOCATABLE, INTENT(OUT) :: cval(:)
    !! complex value needed when field is `complex`
  END SUBROUTINE MMRead
END INTERFACE

PUBLIC :: MMRead

!----------------------------------------------------------------------------
!                                                                 MMRead
!----------------------------------------------------------------------------

END MODULE CSRMatrix_MatrixMarketIO
