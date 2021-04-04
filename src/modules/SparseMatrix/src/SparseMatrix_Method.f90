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
! summary: 	This module contains methods for [[SparseMatrix_]]

MODULE SparseMatrix_Method
USE GlobalData
USE BaseType
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine construct the `SparseMatrix_` object
!
!### Introduction
!
! This subroutine initiate the instance of [[sparsematrix_]] object
!
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE SUBROUTINE initiate_obj1( Obj, tDOF, tNodes, MatrixProp, &
  & StorageFMT )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tDOF
    !! Total number of degrees of freedom
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : )
    !! Total number of spatial nodes (size of vec) of each dof
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
    !! Matrix is `SYM`, `UNSYM`
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: StorageFMT
END SUBROUTINE initiate_obj1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine construct `sparsematrix_` object from IA, JA, A

INTERFACE
MODULE SUBROUTINE initiate_obj2( Obj, A, IA, JA, MatrixProp )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: IA( : ), JA( : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE initiate_obj2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE initiate_obj3( Obj, IA, JA, MatrixProp )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: IA( : ), JA( : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE initiate_obj3
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj1, initiate_obj2, initiate_obj3
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This function returns the shape of sparse matrix
!
!### Introduction
!
! This function returns the shape of sparse matrix

INTERFACE
MODULE PURE FUNCTION get_shape( Obj ) RESULT( Ans )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans( 2 )
END FUNCTION get_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE get_shape
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
MODULE PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION get_size
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE get_size
END INTERFACE Size

PUBLIC :: Size

!----------------------------------------------------------------------------
!                                                         getNNZ@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Return the total number of non zero entry in the matrix
!
!### Introduction
!
! This function return the total number of non-zero entry in the sparse matrix

INTERFACE
MODULE PURE FUNCTION get_nnz( Obj ) RESULT( Ans )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION get_nnz
END INTERFACE

INTERFACE getNNZ
  MODULE PROCEDURE get_nnz
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
MODULE SUBROUTINE Allocate_data( Obj, Dims, MatrixProp )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ),  INTENT( IN ) :: Dims( 2 )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE Allocate_data
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE Allocate_Data
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine deallocates the data
!
!### Introduction
!
! This subroutine deallocates the data stored inside [[SparseMatrix_]]

INTERFACE
MODULE PURE SUBROUTINE Deallocate_Data( Obj )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
END SUBROUTINE Deallocate_Data
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
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
MODULE SUBROUTINE Display_obj( Obj, Msg, UnitNo )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine displays the content of sparse matrix
!
!### Introduction
!
! This subroutine displays the content of sparse matrix
! - options( 1 ) = i1
! - options( 2 ) = i2
! - options( 3 ) = 1 if values need to be printed else dont print values
! - options( 4 ) = unitno

INTERFACE
MODULE SUBROUTINE Display_CSR_2( Obj, msg, options )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ) :: options( : )
END SUBROUTINE Display_CSR_2
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE Display_obj, Display_CSR_2
END INTERFACE Display

PUBLIC :: Display

! !----------------------------------------------------------------------------
! !                                                                     Spy@IO
! !----------------------------------------------------------------------------

! !> authors: Vikas Sharma, Ph. D.
! ! date: 	22 March 2021
! ! summary: 	This subroutine prints the structure of sparse matrix in pdf format.

! INTERFACE
! MODULE SUBROUTINE obj_spy( Obj, Path, File, Extension, ScriptLang, Values )
!   TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
!   TYPE( String ), INTENT( IN ) :: Path, File, Extension
!   CHARACTER( LEN = * ), INTENT( IN ) :: ScriptLang
!   LOGICAL( LGT ), INTENT( IN ) :: Values
! END SUBROUTINE obj_spy
! END INTERFACE

! INTERFACE Spy
!   MODULE PROCEDURE obj_spy
! END INTERFACE Spy

! PUBLIC :: Spy

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
! and appended to `Obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.

INTERFACE
MODULE PURE SUBROUTINE setSparsity_1( Obj, Row, Col )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Row
  INTEGER( I4B ), INTENT( IN ) :: Col( : )
END SUBROUTINE setSparsity_1
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
! This subroutine calls `setSparsity_1`

INTERFACE
MODULE PURE SUBROUTINE setSparsity_2( Obj, Row, Col )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Row( : )
  TYPE( IntVector_ ), INTENT( IN ) :: Col( : )
END SUBROUTINE setSparsity_2
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
! - `Obj % A(:)`
! - `Obj % IA(:)`,
! - `Obj % JA(:)`
! in CSR format. This routine also set data inside `Obj % ColSize(:)` and
! `Obj % RowSize(:) `, and `Obj % DiagIndx(:)`


INTERFACE
MODULE PURE SUBROUTINE setSparsity_3( Obj )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
END SUBROUTINE setSparsity_3
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE setSparsity_1, setSparsity_2, setSparsity_3
END INTERFACE setSparsity

PUBLIC :: setSparsity

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in sparse matrix
!
!### Introduction
!
! This subroutine sets the values in sparse matrix
! - Usually `Val(:,:)` represents the elemental finite element matrix
! - `StorageFMT` denotes the storage format of Val; `Nodes_FMT` or `DOF_FMT`
! - Usually finite element matrix is stored with `DOF_FMT`
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
MODULE PURE SUBROUTINE setValue_1( Obj, Nptrs, Val, StorageFMT )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE setValue_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This subroutine set all values of [[sparsematrix_]] to given scalar value

INTERFACE
MODULE PURE SUBROUTINE setValue_2( Obj, Val )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE setValue_2
END INTERFACE

INTERFACE setValue
  MODULE PROCEDURE setValue_1, setValue_2
END INTERFACE setValue

PUBLIC :: setValue

!----------------------------------------------------------------------------
!                                                  addContribution@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 Marach 2021
! summary: This subroutine add contribution

INTERFACE
MODULE PURE SUBROUTINE addContribution_1( Obj, Nptrs, Val, Scale, StorageFMT )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), StorageFMT
  REAL( DFP ), INTENT( IN ) :: Val( :, : ), Scale
END SUBROUTINE addContribution_1
END INTERFACE

INTERFACE addContribution
  MODULE PROCEDURE addContribution_1
END INTERFACE addContribution

PUBLIC :: addContribution

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
MODULE PURE SUBROUTINE aij_convert_dns( A, IA, JA, mat )
  REAL( DFP ), INTENT( IN )    :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: IA( : )
  INTEGER( I4B ), INTENT( IN ) :: JA( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: mat( :, : )
END SUBROUTINE aij_convert_dns
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
MODULE PURE SUBROUTINE obj_convert_dns( From, To )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: From
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: To( :, : )
END SUBROUTINE obj_convert_dns
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE aij_convert_dns, obj_convert_dns
END INTERFACE Convert

PUBLIC :: Convert

!----------------------------------------------------------------------------
!                                                                 SORT@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 March 2021
! summary: Sort column of row
!
!### Introduction
! This routine sorts the elements of  a matrix (stored in Compressed
! Sparse Row Format) in increasing order of their column indices within
! each row. It uses insertion sort
!
! `values`= logical indicating whether or not the real values a(*) must
! also be permuted. IF (.not. values) then the array a is not
! touched by csort and can be a dummy array.
!
! Default value of `SortValue` is true.

INTERFACE
MODULE PURE SUBROUTINE csort_CSR( Obj, isValues )
  TYPE( SparseMatrix_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: isValues
END SUBROUTINE csort_CSR
END INTERFACE

INTERFACE CSORT
  MODULE PROCEDURE csort_CSR
END INTERFACE CSORT

PUBLIC :: CSORT

!----------------------------------------------------------------------------
!                                                      RemoveDuplicates@Unary
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
!<--- If values = .true. then remove the corresponding values too

INTERFACE
MODULE SUBROUTINE remove_duplicates_csr( Obj, isValues )
  TYPE( SparseMatrix_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: isValues
END SUBROUTINE remove_duplicates_csr
END INTERFACE

INTERFACE RemoveDuplicates
  MODULE PROCEDURE remove_duplicates_csr
END INTERFACE RemoveDuplicates

PUBLIC :: RemoveDuplicates

!----------------------------------------------------------------------------
!                                                                Clean@Unary
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
!<--- extraoption 0 do nothing
!<---    1 eliminate duplicates and zeros
!<---    2 eliminate duplicates and perform partial ordering
!<---    3 eliminate duplicates and sort entries in increasing order of
!<---    col indices
!<--- Values  .true. work on Obj % A too, otherwise dont touch Obj % A
!<--- INDU contains pointers to upper triangle

INTERFACE
MODULE SUBROUTINE clean_CSR( Obj, isValues, ExtraOption )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isValues
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ExtraOption
END SUBROUTINE clean_CSR
END INTERFACE

INTERFACE Clean
  MODULE PROCEDURE clean_CSR
END INTERFACE Clean

PUBLIC :: Clean

!----------------------------------------------------------------------------
!                                                                 Copy@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 March 2021
! summary: 	Copy sparsematrix
!
!@todo
! 	Add shallow copy, move it it constructor
!@endtodo

INTERFACE
MODULE SUBROUTINE copy_CSR_CSR( From, To )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: From
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: To
END SUBROUTINE copy_CSR_CSR
END INTERFACE

INTERFACE Copy
  MODULE PROCEDURE copy_CSR_CSR
END INTERFACE Copy

PUBLIC :: Copy

!----------------------------------------------------------------------------
!                                                          ArrayValues@Unary
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
INTERFACE
MODULE PURE FUNCTION get_scalar_value( Obj, i, j, Sorted ) RESULT( Ans )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: i, j
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: Sorted
  REAL( DFP ) :: Ans
END FUNCTION get_scalar_value
END INTERFACE

INTERFACE ArrayValues
  MODULE PROCEDURE get_scalar_value
END INTERFACE ArrayValues

PUBLIC :: ArrayValues

!----------------------------------------------------------------------------
!                                                              Matvec@MatVec
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
!<--- call to AMUX and ATMUXR
! y = Ax

INTERFACE
MODULE SUBROUTINE matvec_CSR_amux( Obj, x, y, matvectype )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: y( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: matvectype
END SUBROUTINE matvec_CSR_amux
END INTERFACE

INTERFACE MatVec
  MODULE PROCEDURE matvec_CSR_amux
END INTERFACE MatVec

PUBLIC :: MatVec

!----------------------------------------------------------------------------
!                                                              Matmul@MatVec
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
!<--- call to AMUX and ATMUXR
! y = Ax
INTERFACE
MODULE FUNCTION matmul_CSR( Obj, x, matvectype ) RESULT( Ans )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: matvectype
  REAL( DFP ) :: Ans( SIZE( x ) )
END FUNCTION matmul_CSR
END INTERFACE

INTERFACE Matmul
  MODULE PROCEDURE matmul_CSR
END INTERFACE Matmul

PUBLIC :: Matmul

!----------------------------------------------------------------------------
!                                                    Sparsekit_LSolve@MatVec
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
!<--- Here L is lower triangular matrix with unit diag in CSR format
!<--- Solve Lx = y by forward elimination technique will be used

INTERFACE
MODULE SUBROUTINE lsol_csr( Obj, x, y )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: y( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: x( : )
END SUBROUTINE lsol_csr
END INTERFACE

INTERFACE Sparsekit_LSolve
  MODULE PROCEDURE lsol_csr
END INTERFACE Sparsekit_LSolve

PUBLIC :: Sparsekit_LSolve

!----------------------------------------------------------------------------
!                                                    Sparsekit_USolve@MatVec
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
!<--- Here U is upper triangular matrix with unit diag in CSR format
!<--- Solve Ux = y by backward elimination technique will be used
INTERFACE
MODULE SUBROUTINE usol_csr( Obj, x, y )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: y( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: x( : )
END SUBROUTINE usol_csr
END INTERFACE

INTERFACE Sparsekit_USolve
  MODULE PROCEDURE usol_csr
END INTERFACE Sparsekit_USolve

PUBLIC :: Sparsekit_USolve

!----------------------------------------------------------------------------
!                                                                   ILUT@ILUT
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE ilut_csr( Obj, alu, jlu, ju, ierr, droptol, lfil )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: alu( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: jlu( : ), ju( : )
  INTEGER( I4B ), INTENT( INOUT) :: ierr
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: droptol
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: lfil
END SUBROUTINE ilut_csr
END INTERFACE

INTERFACE SparseKit_ILUT
  MODULE PROCEDURE ilut_csr
END INTERFACE SparseKit_ILUT

PUBLIC :: Sparsekit_ILUT

!----------------------------------------------------------------------------
!                                                                  ILUTP@ILUT
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE ilutp_csr( Obj, alu, jlu, ju, iperm, ierr, droptol, &
    & permtol, lfil, mbloc )
  TYPE( SparseMatrix_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: alu( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: jlu( : ), ju( : ), iperm( : )
  INTEGER( I4B ), INTENT( INOUT) :: ierr
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: droptol, permtol
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: lfil, mbloc
END SUBROUTINE ilutp_csr
END INTERFACE

INTERFACE SparseKit_ilutp
  MODULE PROCEDURE ilutp_csr
END INTERFACE SparseKit_ilutp

PUBLIC :: Sparsekit_ilutp

!----------------------------------------------------------------------------
!                                                                   ILUD@ILUT
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE ilud_csr( Obj, alu, jlu, ju, ierr, alpha, droptol )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: alu( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: jlu( : ), ju( : )
  INTEGER( I4B ), INTENT( INOUT) :: ierr
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: droptol, alpha
END SUBROUTINE ilud_csr
END INTERFACE

INTERFACE SparseKit_ilud
  MODULE PROCEDURE ilud_csr
END INTERFACE SparseKit_ilud

PUBLIC :: Sparsekit_ilud

!----------------------------------------------------------------------------
!                                                                 ILUdP@ILUT
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE iludp_csr( Obj, alu, jlu, ju, iperm, ierr, droptol, &
    & permtol, alpha, mbloc )
  TYPE( SparseMatrix_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: alu( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: jlu( : ), ju( : ), iperm( : )
  INTEGER( I4B ), INTENT( INOUT) :: ierr
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: droptol, permtol, alpha
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: mbloc
END SUBROUTINE iludp_csr
END INTERFACE

INTERFACE SparseKit_iludp
  MODULE PROCEDURE iludp_csr
END INTERFACE SparseKit_iludp

PUBLIC :: Sparsekit_iludp

!----------------------------------------------------------------------------
!                                                      Sparsekit_LUSOLVE@ILUT
!----------------------------------------------------------------------------

! LUx = y
INTERFACE
MODULE SUBROUTINE lusol_alu( x, y, alu, jlu, ju )
  REAL( DFP ), INTENT( IN ) :: y ( : ), alu( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: x( : )
  INTEGER( I4B ), INTENT( IN ) :: jlu( : ), ju( : )
END SUBROUTINE lusol_alu
END INTERFACE

INTERFACE Sparsekit_LUSOLVE
MODULE PROCEDURE lusol_alu
END INTERFACE Sparsekit_LUSOLVE

PUBLIC :: Sparsekit_LUSOLVE

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

END MODULE SparseMatrix_Method