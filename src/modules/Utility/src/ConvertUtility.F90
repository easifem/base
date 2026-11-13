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
! date: 2021-03-06
! summary: Methods for converting the layout of dense matrices.
!
! Description:
!   This module provides methods for
!
!   - changing the storage format of a square dense matrix between
!     `FMT_DOF` and `FMT_NODES`,
!   - converting a 4D array (a block matrix) to a 2D array,
!   - converting a 6D array to a 4D array.
!
!   The following generic interfaces are provided:
!
!   - `Convert`: the output `to` is allocated by the routine.
!   - `Convert_`: the output `to` is allocated by the caller, and the
!     routine only writes into the leading part of it. The size of the
!     written part is returned through `nrow`, `ncol` (or `dim1`, ...).
!   - `ConvertSafe`: like `Convert_`, but `to` must already have the
!     exact shape of the result.
MODULE ConvertUtility
USE GlobalData, ONLY: DFP, LGT, I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: Convert
PUBLIC :: Convert_
PUBLIC :: ConvertSafe

!----------------------------------------------------------------------------
!                                                   Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-06
! summary: Change the storage format of a square dense matrix.
!
! Description:
!   Rearranges the rows and columns of the square matrix `from` so that
!   the degrees of freedom are stored in the format selected by
!   `conversion`. The matrix `to` is (re)allocated with the shape
!   `(nns*tdof, nns*tdof)`.
!
!   - Element matrices in easifem are usually stored in `FMT_DOF`
!     (all nodes of dof 1, then all nodes of dof 2, ...).
!   - Global matrices (tangent matrices) are usually stored in
!     `FMT_NODES` (all dofs of node 1, then all dofs of node 2, ...).
!   - This routine is therefore used when setting or adding values in
!     [[SparseMatrix_]].
!
!   All dof should have the same order of interpolation, therefore
!   this routine works when the matrix is square.
!
!   If `conversion` is neither `DofToNodes` nor `NodesToDOF`, then
!   `to` is a copy of `from`.
!
! Arguments:
!   from : REAL(DFP), rank(2) - square matrix `(nns*tdof, nns*tdof)`
!   to : REAL(DFP), ALLOCATABLE, rank(2) - converted matrix
!   conversion : INTEGER(I4B) - `NodesToDOF` or `DofToNodes`
!   nns : INTEGER(I4B) - number of nodes in space
!   tdof : INTEGER(I4B) - number of degrees of freedom per node
!
! Formula:
!   Let $N$ = nns, $t$ = tdof, $1 \le n \le N$ and $1 \le d \le t$.
!   The position of the pair $(n, d)$ in the two formats is
!
!   $$j_{\mathrm{node}}(n,d) = (n-1)\,t + d, \qquad
!   j_{\mathrm{dof}}(n,d) = (d-1)\,N + n$$
!
!   For `conversion = DofToNodes`
!
!$$
! \mathrm{to}\big(j_{\mathrm{node}}(n,d),\, j_{\mathrm{node}}(n',d')\big)
!   = \mathrm{from}\big(j_{\mathrm{dof}}(n,d),\, j_{\mathrm{dof}}(n',d')\big)
!$$
!
!   For `conversion = NodesToDOF` the roles of $j_{\mathrm{node}}$ and
!   $j_{\mathrm{dof}}$ are exchanged. In matrix form
!   $\mathrm{to} = T^{\top}\,\mathrm{from}\,T$, where $T$ is a
!   permutation matrix.

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
! summary: Like `obj_Convert1`, but without allocation.
!
! Description:
!   Same conversion as `obj_Convert1`, but `to` is provided by the
!   caller. Only the block `to(1:nrow, 1:ncol)` is written, the rest of
!   `to` is left untouched. `to` must be at least
!   `(nns*tdof, nns*tdof)` in size.
!
! Arguments:
!   from : REAL(DFP), rank(2) - square matrix `(nns*tdof, nns*tdof)`
!   to : REAL(DFP), rank(2) - output buffer, size >= `(nrow, ncol)`
!   conversion : INTEGER(I4B) - `NodesToDOF` or `DofToNodes`
!   nns : INTEGER(I4B) - number of nodes in space
!   tdof : INTEGER(I4B) - number of degrees of freedom per node
!   nrow : INTEGER(I4B), OUT - number of rows written in `to`
!   ncol : INTEGER(I4B), OUT - number of columns written in `to`
!
! Formula:
!   $$\mathrm{nrow} = \mathrm{ncol} = N\,t$$
!
!   and `to(1:nrow, 1:ncol)` is given by the formula of `obj_Convert1`.

INTERFACE Convert_
  MODULE PURE SUBROUTINE obj_Convert_1( &
    from, to, conversion, nns, tdof, nrow, ncol)
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
! date: 2021-03-06
! summary: Change the storage format of a dense matrix, no allocation.
!
! Description:
!   Same conversion as `obj_Convert1`, but `to` is not allocated
!   by this routine. It must already have the shape
!   `(nns*tdof, nns*tdof)`.
!
!   - Element matrices in easifem are usually stored in `FMT_DOF`.
!   - Global matrices (tangent matrices) are usually stored in
!     `FMT_NODES`.
!
!   All dof should have the same order of interpolation, therefore
!   this routine works when the matrix is square.
!
! Arguments:
!   from : REAL(DFP), rank(2) - square matrix `(nns*tdof, nns*tdof)`
!   to : REAL(DFP), rank(2) - converted matrix, same shape as `from`
!   conversion : INTEGER(I4B) - `NodesToDOF` or `DofToNodes`
!   nns : INTEGER(I4B) - number of nodes in space
!   tdof : INTEGER(I4B) - number of degrees of freedom per node
!
! Formula:
!   $$\mathrm{to} = T^{\top}\,\mathrm{from}\,T$$
!
!   where $T$ is the permutation matrix described in `obj_Convert1`.

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
! date: 2021-03-06
! summary: Convert a rank-4 array to a rank-2 block matrix.
!
! Description:
!   Converts a rank-4 array to a rank-2 matrix. This routine can be
!   used in Space-Time FEM.
!
!   - The first and second dimensions of `from` are spatial nodes.
!   - The third and fourth dimensions of `from` are temporal nodes.
!   - `from(:, :, a, b)` is the `(a, b)` block of the matrix `to`.
!
!   The matrix `to` is (re)allocated with the shape
!   `(n1*n3, n2*n4)`, where `nk = SIZE(from, k)`.
!
! Arguments:
!   from : REAL(DFP), rank(4) - array with indices `(i, j, a, b)`
!   to : REAL(DFP), ALLOCATABLE, rank(2) - block matrix
!
! Formula:
!   $$\mathrm{to}\big((a-1)\,n_1 + i,\ (b-1)\,n_2 + j\big)
!   = \mathrm{from}(i,j,a,b)$$

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert2(from, to)
    REAL(DFP), INTENT(IN) :: from(:, :, :, :)
    !! `(i, j, a, b)`
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: to(:, :)
    !! block matrix of shape `(n1*n3, n2*n4)`
  END SUBROUTINE obj_Convert2
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                                 Convert_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-03
! summary: Like `obj_Convert2`, but without allocation.
!
! Description:
!   Same conversion as `obj_Convert2`, but `to` is provided by the
!   caller. Only the block `to(1:nrow, 1:ncol)` is written, the rest of
!   `to` is left untouched.
!
! Arguments:
!   from : REAL(DFP), rank(4) - array with indices `(i, j, a, b)`
!   to : REAL(DFP), rank(2) - output buffer, size >= `(nrow, ncol)`
!   nrow : INTEGER(I4B), OUT - number of rows written in `to`
!   ncol : INTEGER(I4B), OUT - number of columns written in `to`
!
! Formula:
!   $$\mathrm{nrow} = n_1 n_3, \qquad \mathrm{ncol} = n_2 n_4$$
!
!   $$\mathrm{to}\big((a-1)\,n_1 + i,\ (b-1)\,n_2 + j\big)
!   = \mathrm{from}(i,j,a,b)$$

INTERFACE Convert_
  MODULE PURE SUBROUTINE obj_Convert_2(from, to, nrow, ncol)
    REAL(DFP), INTENT(IN) :: from(:, :, :, :)
    !! `(i, j, a, b)`
    REAL(DFP), INTENT(INOUT) :: to(:, :)
    !! output buffer
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `to`
  END SUBROUTINE obj_Convert_2
END INTERFACE Convert_

!----------------------------------------------------------------------------
!                                                    Convert@ConvertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-06
! summary: Convert a rank-6 array to a rank-4 array.
!
! Description:
!   For each pair `(a, b)` of the last two indices, the rank-4 slice
!   `from(:, :, :, :, a, b)` is converted to a rank-2 block matrix by
!   `obj_Convert2` and stored in `to(:, :, a, b)`.
!
!   The array `to` is (re)allocated with the shape
!   `(n1*n3, n2*n4, n5, n6)`, where `nk = SIZE(from, k)`.
!
! Arguments:
!   from : REAL(DFP), rank(6) - array with indices `(i, j, k, l, a, b)`
!   to : REAL(DFP), ALLOCATABLE, rank(4) - array with indices
!        `(I, J, a, b)`
!
! Formula:
!   $$\mathrm{to}\big((k-1)\,n_1 + i,\ (l-1)\,n_2 + j,\ a,\ b\big)
!   = \mathrm{from}(i,j,k,l,a,b)$$

INTERFACE Convert
  MODULE PURE SUBROUTINE obj_Convert3(from, to)
    REAL(DFP), INTENT(IN) :: from(:, :, :, :, :, :)
    !! I, J, ii, jj, a, b
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: to(:, :, :, :)
    !! I, J, a, b
  END SUBROUTINE obj_Convert3
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                                 Convert_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-03
! summary: Like `obj_Convert3`, but without allocation.
!
! Description:
!   Same conversion as `obj_Convert3`, but `to` is provided by the
!   caller. Only the block `to(1:dim1, 1:dim2, 1:dim3, 1:dim4)` is
!   written, the rest of `to` is left untouched.
!
! Arguments:
!   from : REAL(DFP), rank(6) - array with indices `(i, j, k, l, a, b)`
!   to : REAL(DFP), rank(4) - output buffer, size >= `(dim1, ..., dim4)`
!   dim1 : INTEGER(I4B), OUT - extent written in the first dimension
!   dim2 : INTEGER(I4B), OUT - extent written in the second dimension
!   dim3 : INTEGER(I4B), OUT - extent written in the third dimension
!   dim4 : INTEGER(I4B), OUT - extent written in the fourth dimension
!
! Formula:
!   $$\mathrm{dim1} = n_1 n_3, \quad \mathrm{dim2} = n_2 n_4, \quad
!   \mathrm{dim3} = n_5, \quad \mathrm{dim4} = n_6$$
!
!   $$\mathrm{to}\big((k-1)\,n_1 + i,\ (l-1)\,n_2 + j,\ a,\ b\big)
!   = \mathrm{from}(i,j,k,l,a,b)$$

INTERFACE Convert_
  MODULE PURE SUBROUTINE obj_Convert_3(from, to, dim1, dim2, dim3, dim4)
    REAL(DFP), INTENT(IN) :: from(:, :, :, :, :, :)
    !! `(i, j, k, l, a, b)`
    REAL(DFP), INTENT(INOUT) :: to(:, :, :, :)
    !! output buffer
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    !! extents written in `to`
  END SUBROUTINE obj_Convert_3
END INTERFACE Convert_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConvertUtility
