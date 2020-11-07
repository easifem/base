MODULE SparseMatrix_Method
USE GlobalData
USE BaseType
IMPLICIT NONE

PRIVATE

#include "./SparseMatrix_Interface.inc"
!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine construct the `SparseMatrix_` object

!> authors: Dr. Vikas Sharma
!
! This subroutine construct the `sparsematrix_` object
!
MODULE PURE SUBROUTINE initiate_obj1( Obj, tDOF, tNodes, MatrixProp, &
  & StorageFMT )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tDOF
    !! Total number of degrees of freedom
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : )
    !! Total nunber of spatial nodes (size of vec) of each dof
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
    !! Matrix is `SYM`, `UNSYM`
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: StorageFMT
END SUBROUTINE initiate_obj1
END INTERFACE

INTERFACE
!! This subroutine converts a dense matrix into sparse matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine converts a dense matrix into a sparse matrix
MODULE PURE SUBROUTINE initiate_obj2( Obj, Val, MatrixProp )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE initiate_obj2
END INTERFACE

! todo
INTERFACE
!! This matrix converts a dense matrix into a sparse matrix

!> authors: Dr. Vikas Sharma
!
! This matrix converts a dense matrix into a sparse matrix
MODULE PURE SUBROUTINE initiate_obj3( Obj, Val, MatrixProp )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  TYPE( RealMatrix_ ), INTENT( IN ) :: Val
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE initiate_obj3
END INTERFACE

INTERFACE
!! This subroutine construct `sparsematrix_` object from IA, JA, A

!> authors: Dr. Vikas Sharma
!
! This subroutine constructs `sparsematrix_` object from IA, JA, and A
MODULE PURE SUBROUTINE initiate_obj4( Obj, A, IA, JA, MatrixProp )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: IA( : ), JA( : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE initiate_obj4
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE initiate_obj5( Obj, IA, JA, MatrixProp )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: IA( : ), JA( : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: MatrixProp
END SUBROUTINE initiate_obj5
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj1, initiate_obj2, initiate_obj3, &
    & initiate_obj4, initiate_obj5
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the shape of sparse matrix

!> authors: Dr. Vikas Sharma
!
! This function returns the shape of sparse matrix

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

INTERFACE
!! This function returns the size of sparse matrix

!> authors: Dr. Vikas Sharma
!
! This function returns the size of sparse matrix
! If Dims equal to 1 then total number of rows are returned
! If Dims is equal to 2 then total number of columns are return
! If Dims is absent then nrow*ncol are returned

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

INTERFACE
!! Return the total number of non zero entry in the matrix

!> authors: Dr. Vikas Sharma
!
! This function return the total number of non-zero entry in the sparse matrix
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

INTERFACE
!! This subroutine creates memeory space for the sparse matrix object

!> authors: Dr. Vikas Sharma
!
! This subroutine creates memory space for the sparse matrix
!
! Dims(1) denotes total number of rows
! Dims(2) denotes total number of columns
! tDOF is set to 1
! tNodes is set to Dims(1)
! nnz is set to to 0

MODULE PURE SUBROUTINE Allocate_data( Obj, Dims, MatrixProp )
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

INTERFACE
!! This subroutine deallocates the data

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocates the data stored
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

INTERFACE
!! This subroutine display the content of sparse matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine display the content of sparse matrix
! - In this subroutine `dump` routine from sparsekit lib is called
MODULE SUBROUTINE Display_obj( Obj, Msg, UnitNo )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE


INTERFACE
!! This subroutine displays the content of sparse matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine displays the content of sparse matrix
! - options( 1 ) = i1
! - options( 2 ) = i2
! - options( 3 ) = 1 if values need to be printed else dont print values
! - options( 4 ) = unitno

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

!----------------------------------------------------------------------------
!                                                                     Spy@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE obj_spy( Obj, PFE, ScriptLang, Values )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: Obj
  TYPE( String ), INTENT( IN ) :: PFE( 3 )
  CHARACTER( LEN = * ), INTENT( IN ) :: ScriptLang
  LOGICAL( LGT ), INTENT( IN ) :: Values
END SUBROUTINE obj_spy
END INTERFACE

INTERFACE Spy
  MODULE PROCEDURE obj_spy
END INTERFACE Spy

PUBLIC :: Spy

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the sparsity pattern of a given row

!> authors: Dr. Vikas Sharma
!
! This subroutine set the sparsity pattern of a given row
!  - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `Obj%Row(Row)`
! - If `obj%tdof` is not equal  to 1, then based on the storage format and
! `Col` connectivity information is generated.

MODULE PURE SUBROUTINE setSparsity_1( Obj, Row, Col )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Row, Col( : )
END SUBROUTINE setSparsity_1
END INTERFACE

INTERFACE
!! This subroutine set the sparsity pattern of a given row

!> authors: Dr. Vikas Sharma
!
! This subroutine sets the sparsity pattern of a given row
! This subroutine calls `setSparsity_1`

MODULE PURE SUBROUTINE setSparsity_2( Obj, Row, Col )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Row( : )
  TYPE( IntVector_ ), INTENT( IN ) :: Col( : )
END SUBROUTINE setSparsity_2
END INTERFACE

INTERFACE
!! This subroutine set sparsity pattern of `sparsematrix_`

!> authors: Dr. Vikas Sharma
!
! This subroutine set sparsity pattern of `sparsematrix_`
! This will finally set the data into
! - `Obj % A(:)`
! - `Obj % IA(:)`,
! - `Obj % JA(:)`
! in CSR format. This routine also set data inside `Obj % ColSize(:)` and
! `Obj % RowSize(:) `, and `Obj % DiagIndx(:)`

MODULE PURE SUBROUTINE setSparsity_3( Obj )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
END SUBROUTINE setSparsity_3
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE setSparsity_1, setSparsity_2, setSparsity_3
END INTERFACE setSparsity

PUBLIC :: setSparsity

!----------------------------------------------------------------------------
!                                                         setValue@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the value in `sparsematrix_`

!> authors: Dr. Vikas Sharma
!
! This subroutine sets the value in `sparsematrix_`
! - Shape( Val ) = [SIZE(Nptrs)*tdof, SIZE(Nptrs)*tdof]
! - Usually `Val` denotes the element matrix
! - Symbolic we are performing following task `Obj(Nptrs, Nptrs)=Val`
MODULE PURE SUBROUTINE setValueInternally( Obj, Nptrs, Val )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE setValueInternally
END INTERFACE

INTERFACE
!! This subroutine set the value in sparse matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine sets the values in sparse matrix
! - Usually `Val(:,:)` represents the elemental finite element matrix
! - `StorageFMT` denotes the storage format of Val; `Nodes_FMT` or `DOF_FMT`
! - Usually finite element matrix is stored with `DOF_FMT`
!
! ### Usage
! ```fortran
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
! ```

MODULE PURE SUBROUTINE setValue_1( Obj, Nptrs, Val, StorageFMT )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE setValue_1
END INTERFACE

INTERFACE
!! This subroutine set all values of [[sparsematrix_]] to given scalar value

!> authors: Dr. Vikas Sharma
!
! This subroutine set all values of [[sparsematrix_]] to a given scalar value

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

INTERFACE
MODULE PURE SUBROUTINE addContributionInternally( Obj, Nptrs, Val, Scale )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( :, : ), Scale
END SUBROUTINE addContributionInternally
END INTERFACE

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

INTERFACE
!! This subroutine converts sparsematrix to dense storage

!> authors: Dr. Vikas Sharma
!
! This subroutine converts sparsematrix to dense storage format
! `A(:), IA(:), JA(:)` denotes CSR format.
!
MODULE PURE SUBROUTINE aij_convert_dns( A, IA, JA, mat )
  REAL( DFP ), INTENT( IN ) :: A( : )
  INTEGER( I4B ), INTENT( IN ) :: IA( : ), JA( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: mat( :, : )
END SUBROUTINE aij_convert_dns
END INTERFACE

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

!<--- From sparsekit lib of SAAD
!<--- If Sortvalues = .true. Obj % A will also be sorted in increasing order
INTERFACE
MODULE SUBROUTINE csort_CSR( Obj, Values )
  TYPE( SparseMatrix_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: Values
END SUBROUTINE csort_CSR
END INTERFACE

INTERFACE ColSORT
  MODULE PROCEDURE csort_CSR
END INTERFACE ColSORT

PUBLIC :: ColSORT

!----------------------------------------------------------------------------
!                                                      RemoveDuplicates@Unary
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
!<--- If values = .true. then remove the corresponding values too
INTERFACE
MODULE SUBROUTINE remove_duplicates_csr( Obj, Values )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: Values
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
MODULE SUBROUTINE clean_CSR( Obj, Values, ExtraOption, INDU )
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: Values
  INTEGER( I4B ), INTENT( IN ) :: ExtraOption
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: INDU( : )
END SUBROUTINE clean_CSR
END INTERFACE

INTERFACE Clean
  MODULE PROCEDURE clean_CSR
END INTERFACE Clean

PUBLIC :: Clean

!----------------------------------------------------------------------------
!                                                                 Copy@Unary
!----------------------------------------------------------------------------

!<--- From sparsekit lib of SAAD
!<--- If Values .true. then copy Values also
INTERFACE
MODULE SUBROUTINE copy_CSR_CSR( From, To, Values )
  TYPE( SparseMatrix_ ), INTENT( IN ) :: From
  TYPE( SparseMatrix_ ), INTENT( INOUT) :: To
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: Values
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
MODULE FUNCTION get_scalar_value( Obj, i, j, Sorted ) RESULT( Ans )
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

CONTAINS
#include "contains.part"

END MODULE SparseMatrix_Method