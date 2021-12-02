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

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This module contains methods for [[CSRMatrix_]]

MODULE CSRMatrix_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

INTEGER( I4B ), PARAMETER, PUBLIC :: SPARSE_FMT_CSR = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: SPARSE_FMT_COO = 1

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	14 July 2021
! summary: This subroutine construct the `CSRMatrix_` object
!
!# Introduction
!
! This routine constructs an instance of [[CSRMatrix_]].
!
!### Usage
!
!```fortran
! type( csrmatrix_ ) :: obj
! type( dof_ ) :: dofobj
! integer( i4b ) :: i, unitNo
! call initiate( obj=dofobj, tNodes=[12], names=['K'], &
!   & spaceCompo=[1], timeCompo=[1], storageFMT=NODES_FMT )
! call initiate( obj, ncol=12, nrow=12, dof=dofobj )
!```

INTERFACE
MODULE SUBROUTINE csrMat_initiate1( obj, ncol, nrow, dof, matrixProp )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ncol
    !! number of columns in sparse matrix
  INTEGER( I4B ), INTENT( IN ) :: nrow
    !! number of rows in sparse matrix
  TYPE( DOF_ ), OPTIONAL, INTENT( IN ) :: dof
    !! degree of freedom object; It contains information like
    !! storage format (NODES_FMT, DOF_FMT), and names of physical variable
    !! space-time component in each physical variables
    !! Total number of nodes used for these physical variables
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
    !! Matrix is `SYM`, `UNSYM`
END SUBROUTINE csrMat_initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	14 July 2021
! summary: This subroutine construct the `CSRMatrix_` object
!
!# Introduction
! This subroutine initiates an instance of [[CSRMatrix_]]. The object so
! created does not own the ownership of `obj%csr`. Instead it points to a
! [[CSRSparsity_]] object which is supplied by the user.
!
!@note
! The object `csr` should be initiated by the user before sending it to
! CSR matrix via this routine. This is because this routine uses information
! such as ncol, nrow, nnz from the csr.
!@endnote
!
!### Usage
!
!```fortran
! type( csrmatrix_ ) :: obj
! type( csrSparsity_ ) :: csr
! type( dof_ ) :: dofobj
! integer( i4b ) :: i, unitNo
! !
! call initiate( obj=dofobj, tNodes=[12], names=['K'], &
!   & spaceCompo=[1], timeCompo=[1], storageFMT=NODES_FMT )
! call initiate( csr, ncol=12, nrow=12, dof=dofobj )
!```

INTERFACE
MODULE SUBROUTINE csrMat_initiate2( obj, csr, matrixProp )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRSparsity_ ), TARGET, INTENT( IN ) :: csr
    !! number of columns in sparse matrix
    !! number of rows in sparse matrix
    !! degree of freedom object; It contains information like
    !! storage format (NODES_FMT, DOF_FMT), and names of physical variable
    !! space-time component in each physical variables
    !! Total number of nodes used for these physical variables
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
    !! Matrix is `SYM`, `UNSYM`
END SUBROUTINE csrMat_initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine constructs `sparsematrix_` object from IA, JA, A

INTERFACE
MODULE SUBROUTINE csrMat_initiate3( obj, A, IA, JA, MatrixProp )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: IA( : ), JA( : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE csrMat_initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: Initiate by copying
!
!# Introduction
! This routine initiates obj by copying contents from obj2
! This routine uses `obj2%csr => obj%csr`
! Also, csrOwenrsip is set to false.
! This routine is used in defining the assignment operator.

INTERFACE
MODULE SUBROUTINE csrMat_initiate4( obj, obj2 )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj2
END SUBROUTINE csrMat_initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Initiates a submatrix
!
!# Introduction
!
! This routine initiates a submatrix.

INTERFACE
MODULE SUBROUTINE csrMat_initiate5( obj, obj2, i1, i2, j1, j2 )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj2
  INTEGER( I4B ), INTENT( IN ) :: i1, i2
  INTEGER( I4B ), INTENT( IN ) :: j1, j2
END SUBROUTINE csrMat_initiate5
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine initiates an instance of sparse matrix by copying
! the content of another object obj2

INTERFACE
MODULE SUBROUTINE csrMat_initiate6( obj, obj2, hardCopy )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj2
  LOGICAL( LGT ), INTENT( IN ) :: hardCopy
END SUBROUTINE csrMat_initiate6
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csrMat_initiate1, csrMat_initiate2, csrMat_initiate3, &
    & csrMat_initiate4, csrMat_initiate5, csrMat_initiate6
END INTERFACE Initiate

PUBLIC :: Initiate

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE csrMat_initiate4
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This function returns the shape of sparse matrix

INTERFACE
MODULE PURE FUNCTION csrMat_Shape( obj ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans( 2 )
END FUNCTION csrMat_Shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE csrMat_Shape
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                           Size@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This function returns the size of sparse matrix
!
!# Introduction
!
! This function returns the size of sparse matrix
! If Dims equal to 1 then total number of rows are returned
! If Dims is equal to 2 then total number of columns are return
! If Dims is absent then nrow*ncol are returned

INTERFACE
MODULE PURE FUNCTION csrMat_Size( obj, Dims ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION csrMat_Size
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE csrMat_Size
END INTERFACE Size

PUBLIC :: Size

!----------------------------------------------------------------------------
!                                                 TotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the total dimension of an array

INTERFACE
MODULE PURE FUNCTION csrMat_TotalDimension( obj ) RESULT( Ans )
  CLASS( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION csrMat_TotalDimension
END INTERFACE

INTERFACE TotalDimension
  MODULE PROCEDURE csrMat_TotalDimension
END INTERFACE TotalDimension

PUBLIC :: TotalDimension

!----------------------------------------------------------------------------
!                                              setTotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This subroutine set the total dimension (rank) of an array

INTERFACE
MODULE PURE SUBROUTINE csrMat_setTotalDimension( obj, tDimension )
  CLASS( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tDimension
END SUBROUTINE csrMat_setTotalDimension
END INTERFACE

INTERFACE setTotalDimension
  MODULE PROCEDURE csrMat_setTotalDimension
END INTERFACE setTotalDimension

PUBLIC :: setTotalDimension

!----------------------------------------------------------------------------
!                                                         getNNZ@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Return the total number of non zero entry in the matrix

INTERFACE
MODULE PURE FUNCTION csrMat_getNNZ( obj ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION csrMat_getNNZ
END INTERFACE

INTERFACE getNNZ
  MODULE PROCEDURE csrMat_getNNZ
END INTERFACE getNNZ

PUBLIC :: getNNZ

!----------------------------------------------------------------------------
!                                                   AllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This subroutine creates memeory space for the sparse matrix object
!
!# Introduction
!
! This subroutine creates memory space for the sparse matrix
!
! Dims(1) denotes total number of rows
! Dims(2) denotes total number of columns
! tDOF is set to 1
! tNodes is set to Dims(1)
! nnz is set to to 0

INTERFACE
MODULE SUBROUTINE csrMat_AllocateData( obj, Dims, MatrixProp )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ),  INTENT( IN ) :: Dims( 2 )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE csrMat_AllocateData
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE csrMat_AllocateData
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine deallocates the data

INTERFACE
MODULE PURE SUBROUTINE csrMat_Deallocate( obj )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
END SUBROUTINE csrMat_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE csrMat_Deallocate
END INTERFACE Deallocate

PUBLIC :: Deallocate

!----------------------------------------------------------------------------
!                                                        Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine display the content of sparse matrix
!
!# Introduction
!
! This subroutine display the content of sparse matrix
! - In this subroutine `dump` routine from sparsekit lib is called

INTERFACE
MODULE SUBROUTINE csrMat_Display( obj, Msg, UnitNo )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE csrMat_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE csrMat_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                       Spy@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This subroutine prints the structure of sparse matrix in pdf format.

INTERFACE
MODULE SUBROUTINE csrMat_SPY( obj, filename, ext )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  CHARACTER( LEN = * ), INTENT( IN ) :: ext
END SUBROUTINE csrMat_SPY
END INTERFACE

INTERFACE SPY
  MODULE PROCEDURE csrMat_SPY
END INTERFACE SPY

PUBLIC :: SPY

!----------------------------------------------------------------------------
!                                                          IMPORT@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Jul 2021
! summary: Import sparse matrix from a file

INTERFACE
MODULE SUBROUTINE csrMat_Import( obj, fileName, matFormat )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), INTENT( IN ) :: matFormat
END SUBROUTINE csrMat_Import
END INTERFACE

INTERFACE Import
  MODULE PROCEDURE csrMat_Import
END INTERFACE Import

PUBLIC :: Import

!----------------------------------------------------------------------------
!                                                    setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
!  - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.
!
!### Usage
!```fortran
! ! [[CSRMatrix_]], [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( CSRMatrix_ ) :: obj
!   TYPE( DOF_ ) :: dofobj
!   INTEGER( I4B ) :: i, unitNo
!   !> main
!   CALL Initiate( obj=dofobj, tNodes=[5], names=['K'], &
!     & spaceCompo=[2], timeCompo=[1], storageFMT=FMT_DOF )
!   CALL Initiate( obj, ncol=(.tnodes. dofobj), &
!     & nrow=(.tnodes. dofobj), dof=dofobj )
!   CALL Setsparsity( obj, 1, [1,2,3,5] )
!   CALL Setsparsity( obj, 2, [2,1,3] )
!   CALL Setsparsity( obj, 3, [3,1,2,4,5] )
!   CALL Setsparsity( obj, 4, [4,3,5] )
!   CALL Setsparsity( obj, 5, [5,1,3,4] )
!   CALL SetSparsity(obj)
!   obj = 2.0_DFP
!   CALL SPY( obj, "test_6", ".svg" )
!   CALL Deallocate( obj )
! END PROGRAM main
!```

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity1( obj, row, col )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row
  INTEGER( I4B ), INTENT( IN ) :: col( : )
END SUBROUTINE csrMat_setSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine sets the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
! This subroutine calls `csrMat_setSparsity1`
!
!### Usage
!```fortran
! ! [[CSRMatrix_]], [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( CSRMatrix_ ) :: obj
!   TYPE( DOF_ ) :: dofobj
!   INTEGER( I4B ) :: i, unitNo
!   !> main
!   CALL Initiate( obj=dofobj, tNodes=[5], names=['K'], &
!     & spaceCompo=[2], timeCompo=[1], storageFMT=FMT_DOF )
!   CALL Initiate( obj, ncol=(.tnodes. dofobj), &
!     & nrow=(.tnodes. dofobj), dof=dofobj )
!   CALL Setsparsity( obj, 1, [1,2,3,5] )
!   CALL Setsparsity( obj, 2, [2,1,3] )
!   CALL Setsparsity( obj, 3, [3,1,2,4,5] )
!   CALL Setsparsity( obj, 4, [4,3,5] )
!   CALL Setsparsity( obj, 5, [5,1,3,4] )
!   CALL SetSparsity(obj)
!   obj = 2.0_DFP
!   CALL SPY( obj, "test_6", ".svg" )
!   CALL Deallocate( obj )
! END PROGRAM main
!```

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity2( obj, row, col )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row( : )
  TYPE( IntVector_ ), INTENT( IN ) :: col( : )
END SUBROUTINE csrMat_setSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
!  - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.
!
!## Usage
!
!```fortran
! ! [[CSRMatrix_]], [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( CSRMatrix_ ) :: obj
!   TYPE( DOF_ ) :: dofobj
!   INTEGER( I4B ) :: i, unitNo
!   !> main
!   ! #DOF/Initiate
!   CALL Initiate( obj=dofobj, tNodes=[12, 5], names=['V', 'P'], &
!     & spaceCompo=[2, 1], timeCompo=[1,1], storageFMT=FMT_DOF )
!   ! #CSRMatrix/Initiate
!   CALL Initiate( obj, ncol=(.tnodes. dofobj), &
!     & nrow=(.tnodes. dofobj), dof=dofobj )
!   ! #CSRMatrix/SetSparsity
!   CALL SetSparsity( obj, 1, [1,2,3,5,6,7,10,11,12], 1, 1 )
!   CALL SetSparsity( obj, 2, [2,1,3,6,7,11], 1, 1 )
!   CALL SetSparsity( obj, 3, [3,1,2,4,5,6,7,11,8,9,12,10], 1, 1 )
!   CALL SetSparsity( obj, 4, [4,3,5,8,9,12], 1, 1 )
!   CALL SetSparsity( obj, 5, [5,1,3,4,10,11,8,9,12], 1, 1 )
!   CALL SetSparsity( obj, 6, [6,1,2,3,7,11], 1, 1 )
!   CALL SetSparsity( obj, 7, [7,1,2,3,6,7,11], 1, 1 )
!   CALL SetSparsity( obj, 8, [8,3,4,5,8,9,12], 1, 1 )
!   CALL SetSparsity( obj, 9, [9,3,4,5,8,9,12], 1, 1 )
!   CALL SetSparsity( obj, 10, [10,1,3,5,11,12], 1, 1 )
!   CALL SetSparsity( obj, 11, [11,1,2,3,5,6,7,12,10], 1, 1 )
!   CALL SetSparsity( obj, 12, [12,1,3,4,5,10,11,8,9], 1, 1 )
!   !>
!   CALL SetSparsity( obj, 1, [1,2,3,5], 1, 2 )
!   CALL SetSparsity( obj, 2, [2,1,3], 1, 2 )
!   CALL SetSparsity( obj, 3, [3,1,2,4,5], 1, 2 )
!   CALL SetSparsity( obj, 4, [4,3,5], 1, 2 )
!   CALL SetSparsity( obj, 5, [5,1,3,4], 1, 2 )
!   CALL SetSparsity( obj, 6, [1,2,3], 1, 2 )
!   CALL SetSparsity( obj, 7, [1,2,3], 1, 2 )
!   CALL SetSparsity( obj, 8, [3,4,5], 1, 2 )
!   CALL SetSparsity( obj, 9, [3,4,5], 1, 2 )
!   CALL SetSparsity( obj, 10, [1,3,5], 1, 2 )
!   CALL SetSparsity( obj, 11, [1,2,3,5], 1, 2 )
!   CALL SetSparsity( obj, 12, [1,3,4,5], 1, 2 )
!   !>
!   CALL SetSparsity( obj, 1, [1,2,3,5,6,7,10,11,12], 2, 1 )
!   CALL SetSparsity( obj, 2, [2,1,3,6,7,11], 2, 1 )
!   CALL SetSparsity( obj, 3, [3,1,2,4,5,6,7,11,8,9,12,10], 2, 1 )
!   CALL SetSparsity( obj, 4, [4,3,5,12,8,9], 2, 1 )
!   CALL SetSparsity( obj, 5, [5,1,3,4,10,11,8,9,12], 2, 1 )
!   !>
!   CALL SetSparsity( obj, 1, [1,2,3,5], 2, 2 )
!   CALL SetSparsity( obj, 2, [2,1,3], 2, 2 )
!   CALL SetSparsity( obj, 3, [3,1,2,4,5], 2, 2 )
!   CALL SetSparsity( obj, 4, [4,3,5], 2, 2 )
!   CALL SetSparsity( obj, 5, [5,1,3,4], 2, 2 )
!   !>
!   CALL SetSparsity(obj)
!   obj = 2.0_DFP
!   CALL SPY( obj, "test_7", ".svg" )
!   CALL Deallocate( obj )
! END PROGRAM main
!```

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity3( obj, row, col, ivar, jvar )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row
  INTEGER( I4B ), INTENT( IN ) :: col( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
END SUBROUTINE csrMat_setSparsity3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine sets the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
! This subroutine calls `csrMat_setSparsity1`
!
!
!## Usage
!
!```fortran
! ! [[CSRMatrix_]], [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( CSRMatrix_ ) :: obj
!   TYPE( DOF_ ) :: dofobj
!   INTEGER( I4B ) :: i, unitNo
!   !> main
!   ! #DOF/Initiate
!   CALL Initiate( obj=dofobj, tNodes=[12, 5], names=['V', 'P'], &
!     & spaceCompo=[2, 1], timeCompo=[1,1], storageFMT=FMT_DOF )
!   ! #CSRMatrix/Initiate
!   CALL Initiate( obj, ncol=(.tnodes. dofobj), &
!     & nrow=(.tnodes. dofobj), dof=dofobj )
!   ! #CSRMatrix/SetSparsity
!   CALL SetSparsity( obj, 1, [1,2,3,5,6,7,10,11,12], 1, 1 )
!   CALL SetSparsity( obj, 2, [2,1,3,6,7,11], 1, 1 )
!   CALL SetSparsity( obj, 3, [3,1,2,4,5,6,7,11,8,9,12,10], 1, 1 )
!   CALL SetSparsity( obj, 4, [4,3,5,8,9,12], 1, 1 )
!   CALL SetSparsity( obj, 5, [5,1,3,4,10,11,8,9,12], 1, 1 )
!   CALL SetSparsity( obj, 6, [6,1,2,3,7,11], 1, 1 )
!   CALL SetSparsity( obj, 7, [7,1,2,3,6,7,11], 1, 1 )
!   CALL SetSparsity( obj, 8, [8,3,4,5,8,9,12], 1, 1 )
!   CALL SetSparsity( obj, 9, [9,3,4,5,8,9,12], 1, 1 )
!   CALL SetSparsity( obj, 10, [10,1,3,5,11,12], 1, 1 )
!   CALL SetSparsity( obj, 11, [11,1,2,3,5,6,7,12,10], 1, 1 )
!   CALL SetSparsity( obj, 12, [12,1,3,4,5,10,11,8,9], 1, 1 )
!   !>
!   CALL SetSparsity( obj, 1, [1,2,3,5], 1, 2 )
!   CALL SetSparsity( obj, 2, [2,1,3], 1, 2 )
!   CALL SetSparsity( obj, 3, [3,1,2,4,5], 1, 2 )
!   CALL SetSparsity( obj, 4, [4,3,5], 1, 2 )
!   CALL SetSparsity( obj, 5, [5,1,3,4], 1, 2 )
!   CALL SetSparsity( obj, 6, [1,2,3], 1, 2 )
!   CALL SetSparsity( obj, 7, [1,2,3], 1, 2 )
!   CALL SetSparsity( obj, 8, [3,4,5], 1, 2 )
!   CALL SetSparsity( obj, 9, [3,4,5], 1, 2 )
!   CALL SetSparsity( obj, 10, [1,3,5], 1, 2 )
!   CALL SetSparsity( obj, 11, [1,2,3,5], 1, 2 )
!   CALL SetSparsity( obj, 12, [1,3,4,5], 1, 2 )
!   !>
!   CALL SetSparsity( obj, 1, [1,2,3,5,6,7,10,11,12], 2, 1 )
!   CALL SetSparsity( obj, 2, [2,1,3,6,7,11], 2, 1 )
!   CALL SetSparsity( obj, 3, [3,1,2,4,5,6,7,11,8,9,12,10], 2, 1 )
!   CALL SetSparsity( obj, 4, [4,3,5,12,8,9], 2, 1 )
!   CALL SetSparsity( obj, 5, [5,1,3,4,10,11,8,9,12], 2, 1 )
!   !>
!   CALL SetSparsity( obj, 1, [1,2,3,5], 2, 2 )
!   CALL SetSparsity( obj, 2, [2,1,3], 2, 2 )
!   CALL SetSparsity( obj, 3, [3,1,2,4,5], 2, 2 )
!   CALL SetSparsity( obj, 4, [4,3,5], 2, 2 )
!   CALL SetSparsity( obj, 5, [5,1,3,4], 2, 2 )
!   !>
!   CALL SetSparsity(obj)
!   obj = 2.0_DFP
!   CALL SPY( obj, "test_7", ".svg" )
!   CALL Deallocate( obj )
! END PROGRAM main
!```

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity4( obj, row, col, ivar, jvar )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row( : )
  TYPE( IntVector_ ), INTENT( IN ) :: col( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
END SUBROUTINE csrMat_setSparsity4
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set sparsity pattern of `sparsematrix_`
!
!# Introduction
!
! authors: Dr. Vikas Sharma
!
! This subroutine set sparsity pattern of `sparsematrix_`
! This will finally set the data into
!
! - `obj%A(:)`
! - `obj%IA(:)`
! - `obj%JA(:)`
! in CSR format. This routine also set data inside `obj%ColSize(:)` and
! `obj%RowSize(:) `, and `obj%DiagIndx(:)`
!
!## Usage
!
!```fortran
! program main
!   use easifem
!   implicit none
!   type( sparsematrix_ ) :: obj
!   real( dfp ), allocatable :: val( :, : )
!   call initiate( obj = obj, tdof = 2, tnodes = [8], storageFMT=DOF_FMT )
!   call setsparsity( obj = obj, row = 1, col = [1,2,7] )
!   call setsparsity( obj = obj, row = 2, col = [2,1,3,6,7,8] )
!   call setsparsity( obj = obj, row = 3, col = [3, 2, 4, 8] )
!   call setsparsity( obj = obj, row = 4, col = [4,3,5,8] )
!   call setsparsity( obj = obj, row = 5, col = [5,4,6,8] )
!   call setsparsity( obj = obj, row = 6, col = [6,2,5,7,8] )
!   call setsparsity( obj = obj, row = 7, col = [7,1,2,6] )
!   call setsparsity( obj = obj, row = 8, col = [8,2,3,4,5,6] )
!   call setsparsity( obj = obj )
!   allocate( val( 6, 6 ) )
!   call RANDOM_NUMBER( val )
!   call display( val, "val")
!   call setValue( obj=obj, nptrs=[1,2,7], val=val, storageFMT=DOF_FMT )
!   call display( obj, "obj" )
! end program main
!```

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity_final( obj )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
END SUBROUTINE csrMat_setSparsity_final
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE setSparsity
  MODULE PROCEDURE csrMat_setSparsity1, csrMat_setSparsity2,  &
    & csrMat_setSparsity3, csrMat_setSparsity4, &
    & csrMat_setSparsity_final
END INTERFACE setSparsity

PUBLIC :: setSparsity

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in sparse matrix
!
!# Introduction
!
! This subroutine sets the values in sparse matrix.
!
!$$
! obj(Nptrs,Nptrs)=Val(:,:)
!$$
!
! - Usually `Val(:,:)` represents the element finite element matrix
! - `StorageFMT` denotes the storage format of Val; `Nodes_FMT` or `DOF_FMT`
! - Usually, element matrix is stored with `DOF_FMT`

INTERFACE
MODULE PURE SUBROUTINE csrMat_set1( obj, nptrs, val, storageFMT )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
  REAL( DFP ), INTENT( IN ) :: val( :, : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
END SUBROUTINE csrMat_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets all values of sparse matrix to given scalar value
!
!# Introduction
! This routine sets all values of sparse matrix to given value.
! This routine is used to define an assignment operator. Therefore, we can
! call this routine by `obj=val`.

INTERFACE
MODULE PURE SUBROUTINE csrMat_set2( obj, Val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE csrMat_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: Sets a single entry of sparse matrix
!
!# Introduction
!
! - This subroutine sets a single entry of sparse matrix.
! - Before using this routine the user should be aware of the storage
! pattern of degree of freedom.
! - However, if total number of degrees of freedom is one then there is not
! need to worry.
!
!@warning
! This routine should be avoided by general user.
!@endwarning

INTERFACE
MODULE PURE SUBROUTINE csrMat_set3( obj, irow, icolumn, Val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: irow
  INTEGER( I4B ), INTENT( IN ) :: icolumn
  REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE csrMat_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine sets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (rowNodeNum, rowDOF) and (colNodeNum, colDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `csrMat_set3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:csrMat_set3]]
!@endnote

INTERFACE
MODULE PURE SUBROUTINE csrMat_set4( obj, rowNodeNum, colNodeNum, rowDOF, &
  & colDOF, Val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: rowNodeNum
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: colNodeNum
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: rowDOF
    !! row degree of freedom
  INTEGER( I4B ), INTENT( IN ) :: colDOF
    !! col degree of freedom
  REAL( DFP ), INTENT( IN ) :: Val
    !! scalar value to be set
END SUBROUTINE csrMat_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine sets selected values in sparse matrix
!
!# Introduction
!
! This subroutine sets selected values of the sparse matrix to the scalar
! value `val`
!
! This routine corresponds to `obj(nptrs) = val`

INTERFACE
MODULE PURE SUBROUTINE csrMat_set5( obj, nptrs, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE csrMat_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in sparse matrix
!
!# Introduction
!
! This subroutine sets the values in block sparse matrix. The storage pattern
! of both sparse matrix and val (the element matrix) should be
! in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=Val(:,:)
!$$
!

INTERFACE
MODULE PURE SUBROUTINE csrMat_set6( obj, iNptrs, jNptrs,  &
  & ivar, jvar, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: jNptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  REAL( DFP ), INTENT( IN ) :: val( :, : )
END SUBROUTINE csrMat_set6
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set1, csrMat_set2, csrMat_set3, csrMat_set4, &
    & csrMat_set5, csrMat_set6
END INTERFACE set

PUBLIC :: set

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE csrMat_set2
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                             Add@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 Marach 2021
! summary: This subroutine add contribution

INTERFACE
MODULE PURE SUBROUTINE csrMat_add1( obj, nptrs, val, scale, storageFMT )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
    !! Node numbers
  REAL( DFP ), INTENT( IN ) :: val( :, : )
    !! Element finite element matrix
  REAL( DFP ), INTENT( IN ) :: scale
    !! Scale is used to scale the Val before adding it to the obj
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
    !! Storage format of element finite matrix
END SUBROUTINE csrMat_add1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             add@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Adds all values of sparse matrix to given scalar value
!
!# Introduction
! This routine adds all values of sparse matrix to given value.
! This routine signifies `obj=obj+scale*val`.

INTERFACE
MODULE PURE SUBROUTINE csrMat_add2( obj, val, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: val
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             add@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This subroutine adds a single entry of sparse matrix
!
!# Introduction
!
! This subroutine adds a single entry of sparse matrix.
! Before using this subroutien the user should be aware of the storage
! pattern of degree of freedom. However, if total number of degrees of
! freedom is one then there is not need to worry. In my opinion, this routine
! should be avoided by general user.

INTERFACE
MODULE PURE SUBROUTINE csrMat_add3( obj, irow, icolumn, val, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: irow
  INTEGER( I4B ), INTENT( IN ) :: icolumn
  REAL( DFP ), INTENT( IN ) :: val
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add3
END INTERFACE

!----------------------------------------------------------------------------
!                                                             add@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: Sets the specific row and column entry to a given value
!
!# Introduction
!
! This routine sets the specific row and column entry to a given value.
! The row and column index is calculated by using (rowNodeNum, rowDOF) and
! (colNodeNum, colDOF), respectively.
! After computing the irow and icolumn (internally) this routine calls,
! `csrMat_set3`.

INTERFACE
MODULE PURE SUBROUTINE csrMat_add4( obj, rowNodeNum, colNodeNum, rowDOF, &
  & colDOF, val, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: rowNodeNum
  INTEGER( I4B ), INTENT( IN ) :: colNodeNum
  INTEGER( I4B ), INTENT( IN ) :: rowDOF
  INTEGER( I4B ), INTENT( IN ) :: colDOF
  REAL( DFP ), INTENT( IN ) :: val
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            add@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine add the selected value in sparse matrix

INTERFACE
MODULE PURE SUBROUTINE csrMat_add5( obj, nptrs, val, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
  REAL( DFP ), INTENT( IN ) :: val
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add5
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE csrMat_add1, csrMat_add2, csrMat_add3, csrMat_add4, &
    & csrMat_add5
END INTERFACE Add

PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the the row of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setRow1( obj, irow, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: irow
  REAL( DFP ), INTENT( IN ) :: val( : )
END SUBROUTINE csrMat_setRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is
! calculated using the inode and idof.
! - `inode` is the node number
! - `idof` is the degree of freedom number
! - `irow` calculated from inode and idof depends upon the storageFMT.

INTERFACE
MODULE SUBROUTINE csrMat_setRow2( obj, inode, idof, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( IN ) :: val( : )
END SUBROUTINE csrMat_setRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the the row of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setRow3( obj, irow, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: irow
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE csrMat_setRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the inode and idof.
! - inode is the node number
! - idof is the degree of freedom number
! - irow calculated from inode and idof depends upon the storageFMT.

INTERFACE
MODULE SUBROUTINE csrMat_setRow4( obj, inode, idof, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE csrMat_setRow4
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow1, csrMat_setRow2, csrMat_setRow3, &
    & csrMat_setRow4
END INTERFACE setRow

PUBLIC :: setRow

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setColumn1( obj, iColumn, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iColumn
  REAL( DFP ), INTENT( IN ) :: val( : )
END SUBROUTINE csrMat_setColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated using the inode and idof.
! - inode is the node number
! - idof is the degree of freedom number
! - iColumn calculated from inode and idof depends upon the storageFMT.

INTERFACE
MODULE SUBROUTINE csrMat_setColumn2( obj, inode, idof, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( IN ) :: val( : )
END SUBROUTINE csrMat_setColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setColumn3( obj, iColumn, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iColumn
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE csrMat_setColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated using the inode and idof.
! - inode is the node number
! - idof is the degree of freedom number
! - iColumn calculated from inode and idof depends upon the storageFMT.

INTERFACE
MODULE SUBROUTINE csrMat_setColumn4( obj, inode, idof, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE csrMat_setColumn4
END INTERFACE

INTERFACE setColumn
  MODULE PROCEDURE csrMat_setColumn1, csrMat_setColumn2, csrMat_setColumn3,&
    & csrMat_setColumn4
END INTERFACE setColumn

PUBLIC :: setColumn

!----------------------------------------------------------------------------
!                                                  getStorageFMT@getMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION csrMat_getStorageFMT( obj ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION csrMat_getStorageFMT
END INTERFACE

INTERFACE getStorageFMT
  MODULE PROCEDURE csrMat_getStorageFMT
END INTERFACE getStorageFMT

PUBLIC :: getStorageFMT

INTERFACE OPERATOR(.storageFMT. )
  MODULE PROCEDURE csrMat_getStorageFMT
END INTERFACE OPERATOR(.storageFMT. )

PUBLIC :: OPERATOR(.storageFMT. )

!----------------------------------------------------------------------------
!                                                   getMatrixProp@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION csrMat_getMatrixProp( obj ) RESULT( Ans )
  TYPE( CSRMatrix_ ), TARGET, INTENT( IN ) :: obj
  CHARACTER( LEN = 5 ) :: ans
END FUNCTION csrMat_getMatrixProp
END INTERFACE

INTERFACE OPERATOR( .MatrixProp. )
  MODULE PROCEDURE csrMat_getMatrixProp
END INTERFACE OPERATOR( .MatrixProp. )

PUBLIC :: OPERATOR( .MatrixProp. )

INTERFACE getMatrixProp
  MODULE PROCEDURE csrMat_getMatrixProp
END INTERFACE getMatrixProp

PUBLIC :: getMatrixProp

!----------------------------------------------------------------------------
!                                                   getDOFPointer@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION csrMat_getDOFPointer( obj ) RESULT( Ans )
  TYPE( CSRMatrix_ ), TARGET, INTENT( IN ) :: obj
  CLASS( DOF_ ), POINTER :: Ans
END FUNCTION csrMat_getDOFPointer
END INTERFACE

INTERFACE getDOFPointer
  MODULE PROCEDURE csrMat_getDOFPointer
END INTERFACE getDOFPointer

PUBLIC :: getDOFPointer

!----------------------------------------------------------------------------
!                                                          isSquare@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION csrMat_isSquare( obj ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION csrMat_isSquare
END INTERFACE

INTERFACE isSquare
  MODULE PROCEDURE csrMat_isSquare
END INTERFACE isSquare

PUBLIC :: isSquare

!----------------------------------------------------------------------------
!                                                       isRectangle@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION csrMat_isRectangle( obj ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION csrMat_isRectangle
END INTERFACE

INTERFACE isRectangle
  MODULE PROCEDURE csrMat_isRectangle
END INTERFACE isRectangle

PUBLIC :: isRectangle

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_getRow1( obj, irow, val, scale, addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: irow
  REAL( DFP ), INTENT( INOUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of a sparse matrix. The row index is
! calculated using the inode and idof.
! - `inode` is the node number
! - `idof` is the degree of freedom number
! - `irow` calculated from inode and idof depends upon the storageFMT.

INTERFACE
MODULE SUBROUTINE csrMat_getRow2( obj, inode, idof, val, scale, &
  & addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( INOUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getRow2
END INTERFACE

INTERFACE getRow
  MODULE PROCEDURE csrMat_getRow1, csrMat_getRow2
END INTERFACE getRow

PUBLIC :: getRow

!----------------------------------------------------------------------------
!                                                     getBlockRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
! This routine returns the row of given block
! This routineis designed to handle block matrices
! jvar is the column number for the block address.

INTERFACE
MODULE SUBROUTINE csrMat_getBlockRow1( obj, jvar, irow, val, scale, &
  & addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: irow
  REAL( DFP ), INTENT( INOUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getBlockRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of a sparse matrix. The row index is
! calculated using the inode and idof.
! - `inode` is the node number
! - `idof` is the degree of freedom number of ivar
! - `irow` calculated from inode and idof depends upon the storageFMT.

INTERFACE
MODULE SUBROUTINE csrMat_getBlockRow2( obj, ivar, jvar, inode, idof, &
  & val, scale, addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( INOUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockRow2
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow1, csrMat_getBlockRow2
END INTERFACE getBlockRow

PUBLIC :: getBlockRow

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_getColumn1( obj, iColumn, val, scale, &
  & addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iColumn
  REAL( DFP ), INTENT( INOUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the inode and idof.
! - `inode` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from inode and idof depends upon the storageFMT.

INTERFACE
MODULE SUBROUTINE csrMat_getColumn2( obj, inode, idof, val, scale,  &
  & addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( INOUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getColumn2
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn1, csrMat_getColumn2
END INTERFACE getColumn

PUBLIC :: getColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn1( obj, ivar, iColumn, &
  & val, scale, addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: iColumn
  REAL( DFP ), INTENT( INOUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the inode and idof.
! - `inode` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from inode and idof depends upon the storageFMT.

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn2( obj, ivar, jvar, inode, idof, &
  & val, scale,  addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( INOUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn2
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn1, csrMat_getBlockColumn2
END INTERFACE getBlockColumn

PUBLIC :: getBlockColumn

!----------------------------------------------------------------------------
!                                                              Convert@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine converts sparsematrix to dense storage
!
!# Introduction
!
! This subroutine converts sparsematrix into a dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.
! This subroutine can be used for debuggin purpose.

INTERFACE
MODULE PURE SUBROUTINE crsMat_Convert1( A, IA, JA, mat )
  REAL( DFP ), INTENT( IN )    :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: IA( : )
  INTEGER( I4B ), INTENT( IN ) :: JA( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: mat( :, : )
END SUBROUTINE crsMat_Convert1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Convert@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine converts sparsematrix to dense storage
!
!# Introduction
!
! This subroutine converts sparsematrix to dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.

INTERFACE
MODULE PURE SUBROUTINE crsMat_Convert2( To, From )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: To( :, : )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: From
END SUBROUTINE crsMat_Convert2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Convert@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine converts sparsematrix to dense storage
!
!# Introduction
!
! This subroutine converts sparsematrix to dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.

INTERFACE
MODULE PURE SUBROUTINE crsMat_Convert3( To, From )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: To
  TYPE( CSRMatrix_ ), INTENT( IN ) :: From
END SUBROUTINE crsMat_Convert3
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE crsMat_Convert1, crsMat_Convert2, crsMat_Convert3
END INTERFACE Convert

PUBLIC :: Convert

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE crsMat_Convert2, crsMat_Convert3
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                           ColumnSORT@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 March 2021
! summary: Sort column of row
!
!# Introduction
!
! - This routine sorts the elements of a matrix (stored in Compressed
! Sparse Row Format) in increasing order of their column indices within
! each row. It uses insertion sort algorithm
!
! - `values`= logical indicating whether or not the real values a(*) must
! also be permuted. IF (.not. values) then the array a is not
! touched by csort and can be a dummy array.
!
! - Default value of `SortValue` is true.

INTERFACE
MODULE SUBROUTINE csrMat_ColumnSORT( obj, isValues )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: isValues
END SUBROUTINE csrMat_ColumnSORT
END INTERFACE

INTERFACE ColumnSORT
  MODULE PROCEDURE csrMat_ColumnSORT
END INTERFACE ColumnSORT

PUBLIC :: ColumnSORT

!----------------------------------------------------------------------------
!                                                     RemoveDuplicates@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Removes duplicate entries from the sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_RemoveDuplicates( obj, isValues )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: isValues
END SUBROUTINE csrMat_RemoveDuplicates
END INTERFACE

INTERFACE RemoveDuplicates
  MODULE PROCEDURE csrMat_RemoveDuplicates
END INTERFACE RemoveDuplicates

PUBLIC :: RemoveDuplicates

!----------------------------------------------------------------------------
!                                                                Clean@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Performs different tasks related to cleaning of sparse matrix
!
!# Introduction
! This routine performs tasks related to the cleaning of sparse matrix.

INTERFACE
MODULE SUBROUTINE csrMat_Clean( obj, isValues, ExtraOption )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isValues
    !! If .TRUE. then values will be touched, otherwise they remain
    !! untouched by this subroutine
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ExtraOption
    !! If it is 0, then do nothing
    !! If 1, then remove duplicates and zeros, if any
    !! If 2, then remove duplicates and perform partial ordering
    !! If 3, then remove duplicates, sort entries in increasing order of col
END SUBROUTINE csrMat_Clean
END INTERFACE

INTERFACE Clean
  MODULE PROCEDURE csrMat_Clean
END INTERFACE Clean

PUBLIC :: Clean

!----------------------------------------------------------------------------
!                                                                 Copy@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 March 2021
! summary: 	Copy sparse matrix into each other

INTERFACE
MODULE SUBROUTINE csrMat_Copy( From, To )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: From
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: To
END SUBROUTINE csrMat_Copy
END INTERFACE

INTERFACE Copy
  MODULE PROCEDURE csrMat_Copy
END INTERFACE Copy

PUBLIC :: Copy

!----------------------------------------------------------------------------
!                                                                 get@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This returns a sigle value from the matrix

INTERFACE
MODULE FUNCTION csrMat_Get1( obj, i, j ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: i, j
  REAL( DFP ) :: Ans
END FUNCTION csrMat_Get1
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE csrMat_Get1
END INTERFACE Get

PUBLIC :: Get

!----------------------------------------------------------------------------
!                                                              Filter@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July, 2021
! summary: This routine removes any elements whose absolute value is small from an input matrix A and puts the resulting matrix in B.
!
!# Introduction
!
! - `option` = integer. used to determine strategy chosen by caller to drop
!  elements from matrix A.
! - `option` = 1, Elements whose absolute value is less than the drop
! tolerance are removed.
! - `option` = 2, Elements whose absolute value is less than the product of
! the drop tolerance and the Euclidean norm of the row are removed.
! - `option` = 3, Elements whose absolute value is less that the product of
! the drop tolerance and the largest element in the row are removed.
! - `droptol` = real. drop tolerance used for dropping strategy.
!
!
!## Usage
!
!```fortran
! type( csrmatrix_ ) :: obj, obj2
! integer( i4b ) :: i
! type( IntVector_ ) :: IA, JA
! type( RealVector_ ) :: A
! CALL display( 'testing DropEntry IA, JA, A')
! IA = [1,3,6,9,10,13]
! JA = [1,4,1,2,4,2,3,5,4,5,3,2]
! A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
! call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
! ! call setSparsity(obj) !! Not required
! call DropEntry(objIn=obj, objOut=obj2, dropTol=4.0_DFP)
! call display( obj2, "obj2=" )
! call DropEntry(objIn=obj, objOut=obj, dropTol=4.0_DFP)
! call display( obj, "obj=" )
! call Deallocate( obj )
! call Deallocate( obj2 )
!```

INTERFACE
MODULE SUBROUTINE csrMat_DropEntry( objIn, objOut, droptol, option )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: objIn
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: objOut
  REAL( DFP ), INTENT( IN ) :: droptol
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: option
END SUBROUTINE csrMat_DropEntry
END INTERFACE

INTERFACE DropEntry
  MODULE PROCEDURE csrMat_DropEntry
END INTERFACE DropEntry

PUBLIC :: DropEntry

!----------------------------------------------------------------------------
!                                                          Transpose@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Transpose of the sparse matrix
!
!# Introduction
!
! In-place transposition routine. This subroutine transposes a matrix stored
! in compressed sparse row format. the transposition is done in place in that
! the arrays a,ja,ia c of the transpose are overwritten onto the original
! arrays.
!
!## Usage
!
!```fortran
! type( csrmatrix_ ) :: obj
! integer( i4b ) :: i
! type( IntVector_ ) :: IA, JA
! type( RealVector_ ) :: A
! CALL display( 'testing TRANSPOSE')
! IA = [1,3,6,9,10,13]
! JA = [1,4,1,2,4,2,3,5,4,5,3,2]
! A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
! call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
! call getTranspose(obj)
! call display( obj, "obj=" )
! call Deallocate( obj )
!```

INTERFACE
MODULE SUBROUTINE csrMat_Transpose( obj )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
END SUBROUTINE csrMat_Transpose
END INTERFACE

INTERFACE getTRANSPOSE
  MODULE PROCEDURE csrMat_Transpose
END INTERFACE getTRANSPOSE

PUBLIC :: getTRANSPOSE

!----------------------------------------------------------------------------
!                                                         getDiagonal@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the diagonal of sparse matrix
!
!# Introduction
!
! This subroutine returns the diagonal entries of sparse matrix.
!
! - offset: containing the offset of the wanted diagonal the diagonal
! extracted is the one corresponding to the entries `a(i,j)` with `j-i =
! ioff`. thus `ioff = 0` means the main diagonal
! - `diag` : real*8 array of length nrow containing the wanted diagonal. diag
! contains the diagonal (`a(i,j),j-i = ioff`) as defined above.
! - `idiag` = integer array of  length `len`, containing the poisitions in
! the original arrays `a` and `ja` of the diagonal elements collected in
! `diag`. A zero entry in `idiag(i)` means that there was no entry found in
! row i belonging to the diagonal.
!
!## Usage
!
!```fortran
! type( csrmatrix_ ) :: obj
! integer( i4b ) :: i
! type( IntVector_ ) :: IA, JA
! type( RealVector_ ) :: A
! CALL display( 'testing getDiagonal')
! IA = [1,3,6,9,10,13]
! JA = [1,4,1,2,4,2,3,5,4,5,3,2]
! A = 1.0_DFP*[10.0,-1.0,-2.0,11.0,-3.0,-4.0,12.0,-5.0,13.0,14.0,-9.0,-8.0]
! call initiate( obj=obj, A=A%val, IA=IA%val, JA=JA%val )
! call getDiagonal( obj=obj, diag=A%val, idiag=IA%val, offset=0 )
! call display( A, "diag=")
! call display( IA, "idiag=")
! call getDiagonal( obj=obj, diag=A%val, idiag=IA%val, offset=-1 )
! call display( A, "diag=")
! call display( IA, "idiag=")
! call getDiagonal( obj=obj, diag=A%val, idiag=IA%val, offset=2 )
! call display( A, "diag=")
! call display( IA, "idiag=")
! call Deallocate( obj )
!```

INTERFACE
MODULE SUBROUTINE csrMat_getDiagonal( obj, diag, idiag, offset )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: diag( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: idiag( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset
END SUBROUTINE csrMat_getDiagonal
END INTERFACE

INTERFACE getDiagonal
  MODULE PROCEDURE csrMat_getDiagonal
END INTERFACE getDiagonal

PUBLIC :: getDiagonal

!----------------------------------------------------------------------------
!                                                    getLowerTriangle@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the lower part of the sparse matrix
!
!# Introduction
!
! This subroutine returns the lower part of the sparse matrix.

INTERFACE
MODULE SUBROUTINE csrMat_getLowerTriangle( obj, L )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: L
END SUBROUTINE csrMat_getLowerTriangle
END INTERFACE

INTERFACE getLowerTriangle
  MODULE PROCEDURE csrMat_getLowerTriangle
END INTERFACE getLowerTriangle

PUBLIC :: getLowerTriangle

!----------------------------------------------------------------------------
!                                                    getUpperTriangle@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the Upper part of the sparse matrix
!
!# Introduction
!
! This subroutine returns the Upper part of the sparse matrix.

INTERFACE
MODULE SUBROUTINE csrMat_getUpperTriangle( obj, U )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: U
END SUBROUTINE csrMat_getUpperTriangle
END INTERFACE

INTERFACE getUpperTriangle
  MODULE PROCEDURE csrMat_getUpperTriangle
END INTERFACE getUpperTriangle

PUBLIC :: getUpperTriangle

!----------------------------------------------------------------------------
!                                                         PermuteRow@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Permute the rows of sparse matrix

INTERFACE
MODULE FUNCTION csrMat_permuteRow( obj, PERM, isValues ) &
  & RESULT( ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: PERM( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isValues
  TYPE( CSRMatrix_ ) :: ans
END FUNCTION csrMat_permuteRow
END INTERFACE

INTERFACE PermuteRow
  MODULE PROCEDURE csrMat_permuteRow
END INTERFACE PermuteRow

PUBLIC :: PermuteRow

!----------------------------------------------------------------------------
!                                                        PermuteColumn@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Permute the columns of sparse matrix

INTERFACE
MODULE FUNCTION csrMat_permuteColumn( obj, PERM, isValues ) &
  & RESULT( ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: PERM( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isValues
  TYPE( CSRMatrix_ ) :: ans
END FUNCTION csrMat_permuteColumn
END INTERFACE

INTERFACE PermuteColumn
  MODULE PROCEDURE csrMat_permuteColumn
END INTERFACE PermuteColumn

PUBLIC :: PermuteColumn

!----------------------------------------------------------------------------
!                                                             Permute@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Permute the columns of sparse matrix

INTERFACE
MODULE FUNCTION csrMat_permute( obj, rowPERM, colPERM, &
  & isValues, symPERM ) RESULT( ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: rowPERM( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: colPERM( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isValues
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: symPERM
  TYPE( CSRMatrix_ ) :: ans
END FUNCTION csrMat_permute
END INTERFACE

INTERFACE Permute
  MODULE PROCEDURE csrMat_permute
END INTERFACE Permute

PUBLIC :: Permute

!----------------------------------------------------------------------------
!                                                       getILUT@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
!
! This routine builds the ILUT precondition. Incomplete LU factorization with
! dual truncation mechanism.
!
! - `obj` matrix stored in Compressed Sparse Row format.
! - `lfil` = integer. The fill-in parameter. Each row of L and each row of U
! will have a maximum of lfil elements (excluding the diagonal element). lfil
! must be .ge. 0.
! - `droptol` = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! The diagonal elements of the input matrix must be nonzero (at least
! 'structurally'). Dual drop strategy works as follows:
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.
! - Keeping only the largest `lfil` elements in the ith row of L and the
! largest `lfil` elements in the ith row of `U` (excluding diagonal elements).
! - Flexibility: one  can use  `droptol=0` to get  a strategy  based on
! keeping the largest elements in each row  of `L` and `U`.
! - Taking `droptol .ne. 0` but `lfil=n` will give the usual threshold
! strategy (however, fill-in is then mpredictible).

INTERFACE
MODULE SUBROUTINE csrMat_getILUT1( obj, ALU, JLU, JU, lfil, droptol )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  INTEGER( I4B ), INTENT( IN ) :: lfil
  REAL( DFP ), INTENT( IN ) :: droptol
END SUBROUTINE csrMat_getILUT1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getILUT@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
! This routine builds the ILUT precondition. Incomplete LU factorization with
! dual truncation mechanism.
!
! - `obj` matrix stored in Compressed Sparse Row format.
! - `lfil` = integer. The fill-in parameter. Each row of L and each row of U
! will have a maximum of lfil elements (excluding the diagonal element). lfil
! must be .ge. 0.
! - `droptol` = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
!
! - `ALU`,`JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - `JU` = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! The diagonal elements of the input matrix must be nonzero (at least
! 'structurally'). Dual drop strategy works as follows:
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.
! - Keeping only the largest `lfil` elements in the ith row of L and the
! largest `lfil` elements in the ith row of `U` (excluding diagonal elements).
! - Flexibility: one  can use  `droptol=0` to get  a strategy  based on
! keeping the largest elements in each row  of `L` and `U`.
! - Taking `droptol .ne. 0` but `lfil=n` will give the usual threshold
! strategy (however, fill-in is then mpredictible).

INTERFACE
MODULE SUBROUTINE csrMat_getILUT2( obj, Pmat, lfil, droptol )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  INTEGER( I4B ), INTENT( IN ) :: lfil
  REAL( DFP ), INTENT( IN ) :: droptol
END SUBROUTINE csrMat_getILUT2
END INTERFACE

INTERFACE getILUT
  MODULE PROCEDURE csrMat_getILUT1, csrMat_getILUT2
END INTERFACE getILUT

PUBLIC :: getILUT

!----------------------------------------------------------------------------
!                                                      getILUTP@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
! This routine builds the ILUTP precondition. ILUT with pivoting, incomplete
! LU factorization with dual truncation mechanism
!
! - `obj` matrix stored in Compressed Sparse Row format.
! - `lfil` = integer. The fill-in parameter. Each row of L and each row of U
! will have a maximum of lfil elements (excluding the diagonal element). lfil
! must be .ge. 0.
! - `droptol` = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
! - `permtol` = tolerance ratio used to determine whether or not to permute
! two columns.  At step i columns i and j are permuted when
!
! `abs(a(i,j))*permtol .gt. abs(a(i,i))`.
!
! - permtol=0 implies never permute; good values 0.1 to 0.01
!
! - `mbloc` = if desired, permuting can be done only within the diagonal
! blocks of size mbloc. Useful for PDE problems with several degrees of
! freedom.. If feature not wanted take mbloc=n.
!
! `iperm` = contains the permutation arrays. iperm(1:n) = old numbers of
! unknowns iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH LU-SOLVE, THE
! MATRIX A IS PERMUTED ON RETURN. All column indices are changed. SIMILARLY
! FOR THE U MATRIX. To permute the matrix back to its original state use the
! loop:
!
!```fortran
!  do k=ia(1), ia(n+1)-1
!    ja(k) = iperm(ja(k))
!  enddo
!```
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.
! - Keeping only the largest `lfil` elements in the ith row of L and the
! largest `lfil` elements in the ith row of `U` (excluding diagonal elements).
! - Flexibility: one  can use  `droptol=0` to get  a strategy  based on
! keeping the largest elements in each row  of `L` and `U`.
! - Taking `droptol .ne. 0` but `lfil=n` will give the usual threshold
! strategy (however, fill-in is then mpredictible).

INTERFACE
MODULE SUBROUTINE csrMat_getILUTP1( obj, ALU, JLU, JU, lfil, droptol, &
  & permtol, mbloc, IPERM )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  INTEGER( I4B ), INTENT( IN ) :: lfil
  REAL( DFP ), INTENT( IN ) :: droptol
  REAL( DFP ), INTENT( IN ) :: permtol
  INTEGER( I4B ), INTENT( IN ) :: mbloc
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IPERM( : )
END SUBROUTINE csrMat_getILUTP1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getILUTP@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
! This routine builds the ILUTP precondition. ILUT with pivoting, incomplete
! LU factorization with dual truncation mechanism
!
! - `obj` matrix stored in Compressed Sparse Row format.
! - `lfil` = integer. The fill-in parameter. Each row of L and each row of U
! will have a maximum of lfil elements (excluding the diagonal element). lfil
! must be .ge. 0.
! - `droptol` = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
! - `permtol` = tolerance ratio used to determine whether or not to permute
! two columns.  At step i columns i and j are permuted when
!
! `abs(a(i,j))*permtol .gt. abs(a(i,i))`.
!
! - permtol=0 implies never permute; good values 0.1 to 0.01
!
! - `mbloc` = if desired, permuting can be done only within the diagonal
! blocks of size mbloc. Useful for PDE problems with several degrees of
! freedom.. If feature not wanted take mbloc=n.
!
! `iperm` = contains the permutation arrays. iperm(1:n) = old numbers of
! unknowns iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH LU-SOLVE, THE
! MATRIX A IS PERMUTED ON RETURN. All column indices are changed. SIMILARLY
! FOR THE U MATRIX. To permute the matrix back to its original state use the
! loop:
!
!```fortran
!  do k=ia(1), ia(n+1)-1
!     ja(k) = iperm(ja(k))
!  enddo
!```
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.
! - Keeping only the largest `lfil` elements in the ith row of L and the
! largest `lfil` elements in the ith row of `U` (excluding diagonal elements).
! - Flexibility: one  can use  `droptol=0` to get  a strategy  based on
! keeping the largest elements in each row  of `L` and `U`.
! - Taking `droptol .ne. 0` but `lfil=n` will give the usual threshold
! strategy (however, fill-in is then mpredictible).

INTERFACE
MODULE SUBROUTINE csrMat_getILUTP2( obj, Pmat, lfil, droptol, permtol, &
  & mbloc, IPERM )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  INTEGER( I4B ), INTENT( IN ) :: lfil
  REAL( DFP ), INTENT( IN ) :: droptol
  REAL( DFP ), INTENT( IN ) :: permtol
  INTEGER( I4B ), INTENT( IN ) :: mbloc
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IPERM( : )
END SUBROUTINE csrMat_getILUTP2
END INTERFACE

INTERFACE getILUTP
  MODULE PROCEDURE csrMat_getILUTP1, csrMat_getILUTP2
END INTERFACE getILUTP

PUBLIC :: getILUTP

!----------------------------------------------------------------------------
!                                                       getILUTD@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
! This routine computes the ILU factorization with standard threshold
! dropping: at ith step of elimination, an element a(i,j) in row i is dropped
! if it satisfies the criterion:
!
! - abs(a(i,j)) < tol, that is, average magnitude of elements in row i of A
! - There is no control on memory size required for the factors as is done in
! ILUT.
! - This routines computes also various diagonal compensation ILU's such
! MILU. These are defined through the parameter `alph`
!
! - alph = diagonal compensation parameter, alph*(sum of all dropped out
! elements in a given row) is added to the diagonal element of U of the
! factorization
!   - alph = 0 means the scheme is ILU with threshold,
!   - alph = 1 means the scheme is MILU with threshold.
! - droptol = Threshold parameter for dropping small terms in the
! factorization. During the elimination, a term a(i,j) is dropped whenever abs
! (a(i,j)) .lt. tol * [weighted norm of row i]. Here weighted norm = 1-norm /
! number of nnz elements in the row.
! - `obj` matrix stored in Compressed Sparse Row format.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.

INTERFACE
MODULE SUBROUTINE csrMat_getILUD1( obj, ALU, JLU, JU, alpha, droptol )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: droptol
END SUBROUTINE csrMat_getILUD1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          ILUD@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
!
! This routine computes the ILU factorization with standard threshold
! dropping: at ith step of elimination, an element a(i,j) in row i is dropped
! if it satisfies the criterion:
!
! - abs(a(i,j)) < tol, that is, average magnitude of elements in row i of A
! - There is no control on memory size required for the factors as is done in
! ILUT.
! - This routines computes also various diagonal compensation ILU's such
! MILU. These are defined through the parameter `alph`
!
! - alph = diagonal compensation parameter, alph*(sum of all dropped out
! elements in a given row) is added to the diagonal element of U of the
! factorization
!   - alph = 0 means the scheme is ILU with threshold,
!   - alph = 1 means the scheme is MILU with threshold.
! - droptol = Threshold parameter for dropping small terms in the
! factorization. During the elimination, a term a(i,j) is dropped whenever abs
! (a(i,j)) .lt. tol * [weighted norm of row i]. Here weighted norm = 1-norm /
! number of nnz elements in the row.
! - `obj` matrix stored in Compressed Sparse Row format.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.

INTERFACE
MODULE SUBROUTINE csrMat_getILUD2( obj, Pmat, alpha, droptol )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: droptol
END SUBROUTINE csrMat_getILUD2
END INTERFACE

INTERFACE getILUD
  MODULE PROCEDURE csrMat_getILUD1, csrMat_getILUD2
END INTERFACE getILUD

PUBLIC :: getILUD

!----------------------------------------------------------------------------
!                                                       getILUDP@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUDP precondition
!
!# Introduction
!
! This routine computes ILUDP preconditioner, incomplete LU factorization
! with standard droppoing strategy.
!
! - `droptol` = tolerance used for dropping elements in L and U. elements are
! dropped if they are .lt. norm(row) x droptol row = row being eliminated
! - `permtol` = tolerance ratio used for determning whether to permute two
! columns.  Two columns are permuted only when abs(a(i,j))*permtol .gt. abs(a
! (i,i)) [0 --> never permute; good values 0.1 to 0.01]
! - `mbloc` = if desired, permuting can be done only within the diagonal
! blocks of size mbloc. Useful for PDE problems with several degrees of
! freedom.. If feature not wanted take mbloc=n.
!
! - iperm   = contains the permutation arrays, iperm(1:n) = old numbers of
! unknowns, iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! - abs(a(i,j)) < droptol, that is, average magnitude of elements in row i
! of A
! - alph = diagonal compensation parameter, alph*(sum of all dropped out
! elements in a given row) is added to the diagonal element of U of the
! factorization
!   - alph = 0 means the scheme is ILU with threshold,
!   - alph = 1 means the scheme is MILU with threshold.
! - droptol = Threshold parameter for dropping small terms in the
! factorization. During the elimination, a term a(i,j) is dropped whenever abs
! (a(i,j)) .lt. droptol * [weighted norm of row i]. Here weighted norm =
! 1-norm / number of nnz elements in the row.
! - `obj` matrix stored in Compressed Sparse Row format.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.

INTERFACE
MODULE SUBROUTINE csrMat_getILUDP1( obj, ALU, JLU, JU, alpha, droptol, &
  & permtol, mbloc, IPERM )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: droptol
  REAL( DFP ), INTENT( IN ) :: permtol
  INTEGER( I4B ), INTENT( IN ) :: mbloc
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IPERM( : )
END SUBROUTINE csrMat_getILUDP1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getILUTDP@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUDP precondition
!
!# Introduction
!
! This routine computes ILUDP preconditioner, incomplete LU factorization
! with standard droppoing strategy.
!
! - `droptol` = tolerance used for dropping elements in L and U. elements are
! dropped if they are .lt. norm(row) x droptol row = row being eliminated
! - `permtol` = tolerance ratio used for determning whether to permute two
! columns.  Two columns are permuted only when abs(a(i,j))*permtol .gt. abs(a
! (i,i)) [0 --> never permute; good values 0.1 to 0.01]
! - `mbloc` = if desired, permuting can be done only within the diagonal
! blocks of size mbloc. Useful for PDE problems with several degrees of
! freedom.. If feature not wanted take mbloc=n.
!
! - iperm   = contains the permutation arrays, iperm(1:n) = old numbers of
! unknowns, iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! - abs(a(i,j)) < droptol, that is, average magnitude of elements in row i of
! A
! - alph = diagonal compensation parameter, alph*(sum of all dropped out
! elements in a given row) is added to the diagonal element of U of the
! factorization
!   - alph = 0 means the scheme is ILU with threshold,
!   - alph = 1 means the scheme is MILU with threshold.
! - droptol = Threshold parameter for dropping small terms in the
! factorization. During the elimination, a term a(i,j) is dropped whenever abs
! (a(i,j)) .lt. droptol * [weighted norm of row i]. Here weighted norm =
! 1-norm / number of nnz elements in the row.
! - `obj` matrix stored in Compressed Sparse Row format.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.

INTERFACE
MODULE SUBROUTINE csrMat_getILUDP2( obj, Pmat, alpha, droptol, &
  & permtol, mbloc, IPERM )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: droptol
  REAL( DFP ), INTENT( IN ) :: permtol
  INTEGER( I4B ), INTENT( IN ) :: mbloc
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IPERM( : )
END SUBROUTINE csrMat_getILUDP2
END INTERFACE

INTERFACE getILUDP
  MODULE PROCEDURE csrMat_getILUDP1, csrMat_getILUDP2
END INTERFACE getILUDP

PUBLIC :: getILUDP

!----------------------------------------------------------------------------
!                                                       getILUK@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUK precondition
!
!# Introduction
!

INTERFACE
MODULE SUBROUTINE csrMat_getILUK1( obj, ALU, JLU, JU, lfil, LEVS )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  INTEGER( I4B ), INTENT( IN ) :: lfil
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: LEVS( : )
END SUBROUTINE csrMat_getILUK1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getILUK@ILUTMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUK precondition
!
!# Introduction
!
! This routine returns the ILU WITH LEVEL OF FILL-IN OF K (ILU(k))
!
! - `lfil` = integer. The fill-in parameter. Each element whose leve-of-fill
! exceeds lfil during the ILU process is dropped. lfil must be .ge. 0
! - droptol = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
! - `ALU,JLU` = matrix stored in Modified Sparse Row (MSR) format containing
! the L and U factors together. The diagonal (stored in alu(1:n) ) is
! inverted. Each i-th row of the `ALU,JLU` matrix contains the i-th row of L
! (excluding the diagonal entry=1) followed by the i-th row of `U`.
! - `JU` = integer array of length n containing the pointers to the beginning
! of each row of `U` in the matrix `ALU,JLU`.
! - `LEVS` = integer (work) array of size `IWK`, which contains the levels of
! each element in `ALU, JLU`.
!
!@note
! This is not implemented efficiently storage-wise. For example: Only the
! part of the array levs(*) associated with the U-matrix is needed in the
! routine.. So some storage can be saved if needed. The levels of fills in
! the LU matrix are output for information only -- they are not needed by
! LU-solve.
!@endnote

INTERFACE
MODULE SUBROUTINE csrMat_getILUK2( obj, Pmat, lfil, LEVS )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  INTEGER( I4B ), INTENT( IN ) :: lfil
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: LEVS( : )
END SUBROUTINE csrMat_getILUK2
END INTERFACE

INTERFACE getILUK
  MODULE PROCEDURE csrMat_getILUK1, csrMat_getILUK2
END INTERFACE getILUK

PUBLIC :: getILUK

!----------------------------------------------------------------------------
!                                                    LUSOLVE@LUsolveMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: This routine solves the LU x = y
!
! This routine solves the system `LU x = y`, given an LU decomposition of a
! matrix stored in (`ALU, JLU, JU`) modified sparse row format (MSR).
! This ALU, JLU, JU are created by calling ILUT methods described above

INTERFACE
MODULE SUBROUTINE csrMat_LUSOLVE( sol, rhs, alu, jlu, ju )
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
  REAL( DFP ), INTENT( IN ) :: rhs( : )
  REAL( DFP ), INTENT( IN ) :: alu( : )
  INTEGER( I4B ), INTENT( IN ) :: jlu( : )
  INTEGER( I4B ), INTENT( IN ) :: ju( : )
END SUBROUTINE csrMat_LUSOLVE
END INTERFACE

INTERFACE LUSOLVE
MODULE PROCEDURE csrMat_LUSOLVE
END INTERFACE LUSOLVE

PUBLIC :: LUSOLVE

!----------------------------------------------------------------------------
!                                                             LUTSOLVE@ILUT
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: This routine solves the (LU)^T x = y
!
! This routine solves the system `(LU)^T x = y`, given an LU decomposition of
! a matrix stored in (`ALU, JLU, JU`) modified sparse row format (MSR).
! This ALU, JLU, JU are created by calling ILUT methods described above

INTERFACE
MODULE SUBROUTINE csrMat_LUTSOLVE( sol, rhs, alu, jlu, ju )
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
  REAL( DFP ), INTENT( IN ) :: rhs( : )
  REAL( DFP ), INTENT( IN ) :: alu( : )
  INTEGER( I4B ), INTENT( IN ) :: jlu( : )
  INTEGER( I4B ), INTENT( IN ) :: ju( : )
END SUBROUTINE csrMat_LUTSOLVE
END INTERFACE

INTERFACE LUTSOLVE
  MODULE PROCEDURE csrMat_LUTSOLVE
END INTERFACE LUTSOLVE

PUBLIC :: LUTSOLVE

!----------------------------------------------------------------------------
!                                                    AMatVec1@MatvecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_AMatVec1( obj, x, y )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
END SUBROUTINE csrMat_AMatVec1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    AMatVec2@MatvecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x, A is in MSR format

INTERFACE
MODULE SUBROUTINE csrMat_AMatVec2( A, JA, x, y )
  REAL( DFP ) , INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: JA( : )
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
END SUBROUTINE csrMat_AMatVec2
END INTERFACE

INTERFACE AMatVec
  MODULE PROCEDURE csrMat_AMatVec1, csrMat_AMatVec2
END INTERFACE AMatVec

PUBLIC :: AMatVec

!----------------------------------------------------------------------------
!                                                     AtMatvec@MatvecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_AtMatvec( obj, x, y )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
END SUBROUTINE csrMat_AtMatvec
END INTERFACE

INTERFACE AtMatvec
  MODULE PROCEDURE csrMat_AtMatvec
END INTERFACE AtMatvec

PUBLIC :: AtMatvec

!----------------------------------------------------------------------------
!                                                             Matvec@MatVec
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine performs matrix-vector multiplication
!
!# Introduction
! y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_MatVec1( obj, x, y, transp )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
END SUBROUTINE csrMat_MatVec1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Matvec@MatVec
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine performs matrix-vector multiplication
!
!# Introduction
! y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_MatVec2( A, JA, x, y )
  REAL( DFP ) , INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: JA( : )
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( INOUT ) :: y( : )
END SUBROUTINE csrMat_MatVec2
END INTERFACE

INTERFACE MatVec
  MODULE PROCEDURE csrMat_MatVec1, csrMat_MatVec2
END INTERFACE MatVec

PUBLIC :: MatVec

! !----------------------------------------------------------------------------
! !                                                           Matmul@MatVec
! !----------------------------------------------------------------------------

! !> authors: Vikas Sharma, Ph. D.
! ! date: 14 July 2021
! ! summary: Matrix vector multiplication

! INTERFACE
! MODULE FUNCTION csrMat_Matmul( obj, x, matvectype ) RESULT( Ans )
!   TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
!   REAL( DFP ), INTENT( IN ) :: x( : )
!   CHARACTER( LEN = * ), INTENT( IN ) :: matvectype
!   REAL( DFP ) :: Ans( SIZE( x ) )
! END FUNCTION csrMat_Matmul
! END INTERFACE

! INTERFACE Matmul
!   MODULE PROCEDURE csrMat_Matmul
! END INTERFACE Matmul

! PUBLIC :: Matmul

! !----------------------------------------------------------------------------
! !                                                          LSolve@LinAlg
! !----------------------------------------------------------------------------

! !> authors: Vikas Sharma, Ph. D.
! ! date: 14 July 2021
! ! summary: Solve Lx = y by forward elimination technique will be used
! !
! !# Introduction
! ! This subroutine Solve Lx = y by forward elimination technique will be used
! ! Here L is lower triangular matrix with unit diag in CSR format
!
! INTERFACE
! MODULE SUBROUTINE csrMat_LSolve( obj, x, y )
!   TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
!     !! Sparse matrix
!   REAL( DFP ), INTENT( IN ) :: y( : )
!     !! This contains RHS
!   REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: x( : )
!     !! This contains solution
! END SUBROUTINE csrMat_LSolve
! END INTERFACE
!
! INTERFACE LSolve
!   MODULE PROCEDURE csrMat_LSolve
! END INTERFACE LSolve
!
! PUBLIC :: LSolve

!----------------------------------------------------------------------------
! !                                                           USolve@LinAlg
! !----------------------------------------------------------------------------

! !> authors: Vikas Sharma, Ph. D.
! ! date: 14 July 2021
! ! summary: Solve Ux = y by backward elimination technique will be used
! !
! !# Introduction
! !- This subroutine solve Ux = y by backward elimination technique will be
! ! used
! ! - Here U is upper triangular matrix with unit diag in CSR format

! INTERFACE
! MODULE SUBROUTINE csrMat_USolve( obj, x, y )
!   TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
!     !! Sparse matrix in upper triangle form
!   REAL( DFP ), INTENT( IN ) :: y( : )
!     !! RHS
!   REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: x( : )
!     !! Solution
! END SUBROUTINE csrMat_USolve
! END INTERFACE

! INTERFACE USolve
!   MODULE PROCEDURE csrMat_USolve
! END INTERFACE USolve

! PUBLIC :: USolve

!----------------------------------------------------------------------------
!                                             NestedDissect@ReoderingMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Nested dissection using Metis library
INTERFACE
MODULE SUBROUTINE csrMat_NestedDissect( reorder, csrMat )
  TYPE( SparseMatrixReOrdering_ ), INTENT( INOUT ) :: reorder
  TYPE( CSRMatrix_ ), INTENT( IN ) :: csrMat
END SUBROUTINE csrMat_NestedDissect
END INTERFACE

INTERFACE NestedDissect
  MODULE PROCEDURE csrMat_NestedDissect
END INTERFACE NestedDissect

PUBLIC :: NestedDissect

!----------------------------------------------------------------------------
!                                                   Display@ReorderingMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Display the content of SparseMatrixReordering
INTERFACE
MODULE SUBROUTINE csrMat_reorderDisplay( obj, msg, unitNo )
  TYPE( SparseMatrixReOrdering_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE csrMat_reorderDisplay
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE csrMat_reorderDisplay
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                   Permute@ReorderingMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION csrMat_Permute2( obj, rowPERM, colPERM ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  TYPE( SparseMatrixReOrdering_ ), INTENT( IN ) :: rowPERM
  TYPE( SparseMatrixReOrdering_ ), INTENT( IN ) :: colPERM
  TYPE( CSRMatrix_ ) :: ans
END FUNCTION csrMat_Permute2
END INTERFACE

INTERFACE Permute
  MODULE PROCEDURE csrMat_Permute2
END INTERFACE Permute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


END MODULE CSRMatrix_Method
