---
title: Sparse matrix in easifem
author: Vikas Sharma
language: fortran
---

# Documentation of `SparseMatrix_`

## Status

- [x] Completed task
- [ ] Incomplete task
  - [ ] Extend `SparseMatrix_Method@Unary.f90`
  - [ ] Implement `SparseMatrix_Method@BLASSM.f90`
  - [ ] Improve `SparseMatrix_Method@MatVec.f90`
  - [ ] Improve `SparseMatrix_Method@ILUT.f90`
  - [ ] Add `SparseMatrix_Method@Ordering.f90`
  - [ ] Add `SparseMatrix_Method@Info.f90`

For solving system of linear equation `easifem` represents global tangent matrix by sparse matrix. Sparse matrix is stored in compressed sparse row (CSR) format.

## Structure

```fortran
TYPE, EXTENDS( AbstractMatrix_ ) :: SparseMatrix_
  TYPE( IntVector_ ), ALLOCATABLE :: Row( : )
  INTEGER( I4B ), ALLOCATABLE :: IA( : )
  INTEGER( I4B ), ALLOCATABLE :: JA( : )
  INTEGER( I4B ), ALLOCATABLE :: ColSize( : )
  INTEGER( I4B ), ALLOCATABLE :: RowSize( : )
  INTEGER( I4B ), ALLOCATABLE :: DiagIndx( : )
  REAL( DFP ), ALLOCATABLE :: A( : ), Diag( : )
  INTEGER( I4B ) :: tDOF = 1
  INTEGER( I4B ) :: tNodes=0
  INTEGER( I4B ) :: nnz = 0
  INTEGER( I4B ) :: ncol = 0
  INTEGER( I4B ) :: nrow=0
  CHARACTER( LEN = 5 ) :: MatrixProp='UNSYM'
END TYPE SparseMatrix_
```

- `tdof` is total number of degrees of freedom
- `tNodes` is total number of spatial nodes in each dof
- `nnz` number of nonzero entries
- `ncol` total number of columns
- `nrow` total number of rows

## Modules and submodules

- `SparseMatrix_Method@Constructor.f90`
- `SparseMatrix_Method@IO.f90`
- `SparseMatrix_Method@setMethod.f90`
- `SparseMatrix_Method@BLASSM.f90`
- `SparseMatrix_Method@MatVec.f90`
- `SparseMatrix_Method@Unary.f90`
- `SparseMatrix_Method@ILUT.f90`

## Getting started

### Object creation

```fortran
call initiate( SparseMatrix_::obj, INTEGER::tdof, INTEGER::tnodes(:) )
```

here,

- `tdof` is the degree of freedom
- `tnodes(:)` total number of nodes for each degree of freedom

### Setting sparsity pattern

After initiating the object we are ready to set the sparsity pattern in it. Following are the methods for setting sparsity pattern in the object.

```fortran
call setSparsity( SparseMatrix_::Obj, INTEGER::Row, INTEGER::Col(:) )
```

```fortran
call setsparsity( MeshData_::Obj, Mesh_::MeshObj, SparseMatrix_::mat)
```

After setting up sparsity pattern one should make a call to following routine.

```fortran
call setsparsity( SparseMatrix_:: obj )
```

This will finally set the data into `Obj % A(:)`, `Obj % IA(:)`, and `Obj % JA(:)`, in CSR format. This routine also set data inside `Obj % ColSize(:)` and `Obj % RowSize(:) `, and `Obj % DiagIndx(:)`

### Setting values in `sparsematrix_`


## Examples

### Example 1

```fortran
!<----------------------------------------------------------------------------
!<--- Author : Vikas
!<--- This program shows how to initiate sparsematrix
!<--- and how to set its sparsity pattern
!<----------------------------------------------------------------------------

program main
  use easifem
  implicit none

  type( sparsematrix_ ) :: obj
  real( dfp ), allocatable :: val( :, : )

  !<--- initiate the size...
  call initiate( obj = obj, tdof = 2, tnodes = [8], storageFMT=DOF_FMT )
  !<--- set sparsity
  call setsparsity( obj = obj, row = 1, col = [1,2,7] )
  call setsparsity( obj = obj, row = 2, col = [2,1,3,6,7,8] )
  call setsparsity( obj = obj, row = 3, col = [3, 2, 4, 8] )
  call setsparsity( obj = obj, row = 4, col = [4,3,5,8] )
  call setsparsity( obj = obj, row = 5, col = [5,4,6,8] )
  call setsparsity( obj = obj, row = 6, col = [6,2,5,7,8] )
  call setsparsity( obj = obj, row = 7, col = [7,1,2,6] )
  call setsparsity( obj = obj, row = 8, col = [8,2,3,4,5,6] )
  !<--- the following call to setsparsity is important as it removes
  !<--- extra spaces
  call setsparsity( obj = obj )
  call display( obj, 'obj' )
end program main
```