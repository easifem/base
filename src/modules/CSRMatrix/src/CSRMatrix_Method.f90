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
! date: 	22 March 2021
! summary: 	This module contains methods for [[CSRMatrix_]]

MODULE CSRMatrix_Method
USE GlobalData
USE BaseType
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	14 July 2021
! summary: This subroutine construct the `CSRMatrix_` object
!
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
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
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
!### Introduction
! This subroutine initiates an instance of [[CSRMatrix_]]. The object so created does not own the ownership of `obj%csr`. Instead it points to a [[CSRSparsity_]] object which is supplied by the user.
!
!@note
! 	The object `csr` should be initiated by the user before sending it to CSR matrix via this routine. This is because this routine uses information such as ncol, nrow, nnz from the csr.
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
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
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
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: IA( : ), JA( : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE csrMat_initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

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
! summary: This returns a sigle value from the matrix

INTERFACE
MODULE SUBROUTINE csrMat_initiate5( obj, obj2, i1, i2, j1, j2 )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj2
  INTEGER( I4B ), INTENT( IN ) :: i1, i2
  INTEGER( I4B ), INTENT( IN ) :: j1, j2
END SUBROUTINE csrMat_initiate5
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csrMat_initiate1, csrMat_initiate2, csrMat_initiate3, &
    & csrMat_initiate4, csrMat_initiate5
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
!### Introduction
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
!### Introduction
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
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ),  INTENT( IN ) :: Dims( 2 )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE csrMat_AllocateData
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE csrMat_AllocateData
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine deallocates the data

INTERFACE
MODULE PURE SUBROUTINE csrMat_DeallocateData( obj )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
END SUBROUTINE csrMat_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE csrMat_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine display the content of sparse matrix
!
!### Introduction
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
!                                                                     Spy@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This subroutine prints the structure of sparse matrix in pdf format.

INTERFACE
MODULE SUBROUTINE csrMat_SPY( obj, msg, unitNo )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ) ::  unitNo
END SUBROUTINE csrMat_SPY
END INTERFACE

INTERFACE SPY
  MODULE PROCEDURE csrMat_SPY
END INTERFACE SPY

PUBLIC :: SPY

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!### Introduction
!
! This subroutine sets the sparsity pattern of a given row
!  - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.
!
!### Usage
!```fortran
! program main
!   use easifem
!   implicit none
!
!   type( sparsematrix_ ) :: obj
!   real( dfp ), allocatable :: val( :, : )
!
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
MODULE SUBROUTINE csrMat_setSparsity1( obj, row, col )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
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
!### Introduction
!
! This subroutine sets the sparsity pattern of a given row
! This subroutine calls `csrMat_setSparsity1`
!
!### Usage
!```fortran
! program main
!   use easifem
!   implicit none
!
!   type( sparsematrix_ ) :: obj
!   real( dfp ), allocatable :: val( :, : )
!
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
MODULE SUBROUTINE csrMat_setSparsity2( obj, row, col )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row( : )
  TYPE( IntVector_ ), INTENT( IN ) :: col( : )
END SUBROUTINE csrMat_setSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set sparsity pattern of `sparsematrix_`
!
!### Introduction
!
! authors: Dr. Vikas Sharma
!
! This subroutine set sparsity pattern of `sparsematrix_`
! This will finally set the data into
! - `obj%A(:)`
! - `obj%IA(:)`,
! - `obj%JA(:)`
! in CSR format. This routine also set data inside `obj%ColSize(:)` and
! `obj%RowSize(:) `, and `obj%DiagIndx(:)`
!
!### Usage
!```fortran
! program main
!   use easifem
!   implicit none
!
!   type( sparsematrix_ ) :: obj
!   real( dfp ), allocatable :: val( :, : )
!
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
MODULE SUBROUTINE csrMat_setSparsity3( obj )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
END SUBROUTINE csrMat_setSparsity3
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csrMat_setSparsity1, csrMat_setSparsity2, &
    & csrMat_setSparsity3
END INTERFACE setSparsity

PUBLIC :: setSparsity

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in sparse matrix
!
!### Introduction
!
! This subroutine sets the values in sparse matrix
! - Usually `Val(:,:)` represents the element finite element matrix
! - `StorageFMT` denotes the storage format of Val; `Nodes_FMT` or `DOF_FMT`
! - Commonly element finite element matrix is stored with `DOF_FMT`

INTERFACE
MODULE PURE SUBROUTINE csrMat_set1( obj, nptrs, val, storageFMT )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
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
! summary: 	This subroutine set all values of [[sparsematrix_]] to given scalar value

INTERFACE
MODULE PURE SUBROUTINE csrMat_set2( obj, Val )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
  REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE csrMat_set2
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set1, csrMat_set2
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
MODULE PURE SUBROUTINE csrMat_Add( obj, Nptrs, Val, Scale, StorageFMT )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
    !! Node numbers
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! Element finite element matrix
  REAL( DFP ), INTENT( IN ) :: Scale
    !! Scale is used to scale the Val before adding it to the obj
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
    !! Storage format of element finite matrix
END SUBROUTINE csrMat_Add
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE csrMat_Add
END INTERFACE Add

PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                              Convert@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine converts sparsematrix to dense storage
!
!### Introduction
!
! This subroutine converts sparsematrix into a dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.
! This subroutine can be used for debuggin purpose.

INTERFACE
MODULE PURE SUBROUTINE crsMat_convert_internally( A, IA, JA, mat )
  REAL( DFP ), INTENT( IN )    :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: IA( : )
  INTEGER( I4B ), INTENT( IN ) :: JA( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: mat( :, : )
END SUBROUTINE crsMat_convert_internally
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Convert@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine converts sparsematrix to dense storage
!
!### Introduction
!
! This subroutine converts sparsematrix to dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.

INTERFACE
MODULE PURE SUBROUTINE crsMat_Convert( From, To )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: From
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: To( :, : )
END SUBROUTINE crsMat_Convert
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE crsMat_Convert
END INTERFACE Convert

PUBLIC :: Convert

!----------------------------------------------------------------------------
!                                                           ColumnSORT@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 March 2021
! summary: Sort column of row
!
!### Introduction
! This routine sorts the elements of a matrix (stored in Compressed
! Sparse Row Format) in increasing order of their column indices within
! each row. It uses insertion sort algorithm
!
! `values`= logical indicating whether or not the real values a(*) must
! also be permuted. IF (.not. values) then the array a is not
! touched by csort and can be a dummy array.
!
! Default value of `SortValue` is true.

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
!### Introduction
! This routine performs tasks related to the cleaning of sparse matrix.

INTERFACE
MODULE SUBROUTINE csrMat_Clean( obj, isValues, ExtraOption )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
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
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: To
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
!### Introduction
!
! - `option` = integer. used to determine strategy chosen by caller to drop elements from matrix A.
! - `option` = 1, Elements whose absolute value is less than the drop tolerance are removed.
! - `option` = 2, Elements whose absolute value is less than the product of the drop tolerance and the Euclidean norm of the row are removed.
! - `option` = 3, Elements whose absolute value is less that the product of the drop tolerance and the largest element in the row are removed.
! - `droptol` = real. drop tolerance used for dropping strategy.
!
!
!### Usage
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
  ! call deallocateData( obj )
  ! call deallocateData( obj2 )
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
!### Introduction
! In-place transposition routine. This subroutine transposes a matrix stored in compressed sparse row format. the transposition is done in place in that the arrays a,ja,ia c of the transpose are overwritten onto the original arrays.
!
!### Usage
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
  ! call deallocateData( obj )
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
!### Introduction
! This subroutine returns the diagonal entries of sparse matrix.
!
! - offset: containing the offset of the wanted diagonal the diagonal extracted is the one corresponding to the entries `a(i,j)` with `j-i = ioff`. thus `ioff = 0` means the main diagonal
! - `diag` : real*8 array of length nrow containing the wanted diagonal. diag contains the diagonal (`a(i,j),j-i = ioff`) as defined above.
! - `idiag` = integer array of  length `len`, containing the poisitions in the original arrays `a` and `ja` of the diagonal elements collected in `diag`. A zero entry in `idiag(i)` means that there was no entry found in row i belonging to the diagonal.
!
!### Usage
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
  ! call deallocateData( obj )
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
!### Introduction
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
!### Introduction
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
!                                                              Matvec@MatVec
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine performs matrix-vector multiplication
!
!### Introduction
! y = A*x

INTERFACE
MODULE SUBROUTINE csrMat_MatVec( obj, x, y, matvectype )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: y( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: matvectype
END SUBROUTINE csrMat_MatVec
END INTERFACE

INTERFACE MatVec
  MODULE PROCEDURE csrMat_MatVec
END INTERFACE MatVec

PUBLIC :: MatVec

!----------------------------------------------------------------------------
!                                                              Matmul@MatVec
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Matrix vector multiplication

INTERFACE
MODULE FUNCTION csrMat_Matmul( obj, x, matvectype ) RESULT( Ans )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: matvectype
  REAL( DFP ) :: Ans( SIZE( x ) )
END FUNCTION csrMat_Matmul
END INTERFACE

INTERFACE Matmul
  MODULE PROCEDURE csrMat_Matmul
END INTERFACE Matmul

PUBLIC :: Matmul

!----------------------------------------------------------------------------
!                                                              LSolve@LinAlg
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Solve Lx = y by forward elimination technique will be used
!
!### Introduction
! This subroutine Solve Lx = y by forward elimination technique will be used
! Here L is lower triangular matrix with unit diag in CSR format

INTERFACE
MODULE SUBROUTINE csrMat_LSolve( obj, x, y )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
    !! Sparse matrix
  REAL( DFP ), INTENT( IN ) :: y( : )
    !! This contains RHS
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: x( : )
    !! This contains solution
END SUBROUTINE csrMat_LSolve
END INTERFACE

INTERFACE LSolve
  MODULE PROCEDURE csrMat_LSolve
END INTERFACE LSolve

PUBLIC :: LSolve

!----------------------------------------------------------------------------
!                                                              USolve@LinAlg
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Solve Ux = y by backward elimination technique will be used
!
!### Introduction
!- This subroutine solve Ux = y by backward elimination technique will be used
! - Here U is upper triangular matrix with unit diag in CSR format

INTERFACE
MODULE SUBROUTINE csrMat_USolve( obj, x, y )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
    !! Sparse matrix in upper triangle form
  REAL( DFP ), INTENT( IN ) :: y( : )
    !! RHS
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: x( : )
    !! Solution
END SUBROUTINE csrMat_USolve
END INTERFACE

INTERFACE USolve
  MODULE PROCEDURE csrMat_USolve
END INTERFACE USolve

PUBLIC :: USolve

!----------------------------------------------------------------------------
!                                                                ILUT@LinAlg
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Returns incomplete LU decomposition

INTERFACE
MODULE SUBROUTINE csrMat_ILUT( obj, alu, jlu, ju, ierr, droptol, lfil )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: alu( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: jlu( : ), ju( : )
  INTEGER( I4B ), INTENT( INOUT) :: ierr
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: droptol
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: lfil
END SUBROUTINE csrMat_ILUT
END INTERFACE

INTERFACE ILUT
  MODULE PROCEDURE csrMat_ILUT
END INTERFACE ILUT

PUBLIC :: ILUT

!----------------------------------------------------------------------------
!                                                              ILUTP@LinAlg
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE csrMat_ILUTP( obj, alu, jlu, ju, iperm, ierr, droptol, &
    & permtol, lfil, mbloc )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: alu( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: jlu( : ), ju( : ), iperm( : )
  INTEGER( I4B ), INTENT( INOUT) :: ierr
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: droptol, permtol
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: lfil, mbloc
END SUBROUTINE csrMat_ILUTP
END INTERFACE

INTERFACE ILUTP
  MODULE PROCEDURE csrMat_ILUTP
END INTERFACE ILUTP

PUBLIC :: ILUTP

!----------------------------------------------------------------------------
!                                                               ILUD@LinAlg
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE csrMat_ILUD( obj, alu, jlu, ju, ierr, alpha, droptol )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: alu( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: jlu( : ), ju( : )
  INTEGER( I4B ), INTENT( INOUT) :: ierr
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: droptol, alpha
END SUBROUTINE csrMat_ILUD
END INTERFACE

INTERFACE ILUD
  MODULE PROCEDURE csrMat_ILUD
END INTERFACE ILUD

PUBLIC :: ILUD

!----------------------------------------------------------------------------
!                                                              ILUDP@LinAlg
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE csrMat_ILUDP( obj, alu, jlu, ju, iperm, ierr, droptol, &
    & permtol, alpha, mbloc )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: alu( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: jlu( : ), ju( : ), iperm( : )
  INTEGER( I4B ), INTENT( INOUT) :: ierr
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: droptol, permtol, alpha
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: mbloc
END SUBROUTINE csrMat_ILUDP
END INTERFACE

INTERFACE ILUDP
  MODULE PROCEDURE csrMat_ILUDP
END INTERFACE ILUDP

PUBLIC :: ILUDP

!----------------------------------------------------------------------------
!                                                            LUSOLVE@LinAlg
!----------------------------------------------------------------------------

! LUx = y
INTERFACE
MODULE SUBROUTINE csrMat_LUSOLVE( x, y, alu, jlu, ju )
  REAL( DFP ), INTENT( IN ) :: y ( : ), alu( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: x( : )
  INTEGER( I4B ), INTENT( IN ) :: jlu( : ), ju( : )
END SUBROUTINE csrMat_LUSOLVE
END INTERFACE

INTERFACE LUSOLVE
MODULE PROCEDURE csrMat_LUSOLVE
END INTERFACE LUSOLVE

PUBLIC :: LUSOLVE

!----------------------------------------------------------------------------
!                                                     Sparsekit_LUTSOLVE@ILUT
!----------------------------------------------------------------------------

!(LU)^T x = y
INTERFACE
MODULE SUBROUTINE lutsol_alu( x, y, alu, jlu, ju )
  REAL( DFP ), INTENT( IN ) :: y( : ), alu( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: x( : )
  INTEGER( I4B ), INTENT( IN ) :: jlu( : ), ju( : )
END SUBROUTINE lutsol_alu
END INTERFACE

INTERFACE Sparsekit_LUTSOLVE
  MODULE PROCEDURE lutsol_alu
END INTERFACE Sparsekit_LUTSOLVE

PUBLIC :: Sparsekit_LUTSOLVE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSRMatrix_Method