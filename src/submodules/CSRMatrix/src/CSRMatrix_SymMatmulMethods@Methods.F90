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

SUBMODULE(CSRMatrix_SymMatmulMethods) Methods
USE Display_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           SymMatSquare
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SymMatSquare
CALL Display("obj_SymMatSquare in "//__FILE__//" is under development.")
STOP

! INTEGER(I4B) :: ii, jj, nrow, ncol, c1, c2, c, nnz_irow
! REAL(DFP) :: irow(A%max_nnz_row, A%max_nnz_col)
!
! ASSOCIATE (csr2 => obj%csr, csr1 => A%csr, A2 => obj%A, A1 => A%A)
!   DO ii = 1, nrow
!     c1 = csr1.startColumn.ii
!     c2 = csr1.endColumn.ii
!
!     ! nnz_row = c2 - c1
!     ! DO jj = 1, nnz_row
!     !   tempRow(jj) = A1(c1 + jj - 1)
!     ! END DO
!     CALL GetCompactRow(obj=obj, VALUE=irow, irow=ii, n=nnz_irow)
!
!     DO c = c1, c2
!       jj = csr2%JA(c)
!       CALL GetCompactRow(obj=obj, VALUE=jrow, irow=jj, n=nnz_jrow)
!
!       A(c) = DOT_PRODUCT()
!
!     END DO
!   END DO
! END ASSOCIATE

END PROCEDURE obj_SymMatSquare

END SUBMODULE Methods
