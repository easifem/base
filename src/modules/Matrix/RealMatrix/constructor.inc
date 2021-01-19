
!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return shape of [[RealMatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function return space of [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!	s = Shape( Obj )
! ```

MODULE PURE FUNCTION get_shape( Obj ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans( 2 )
END FUNCTION get_shape
END INTERFACE

!>
! Generic interface to get shape of [[RealMatrix_]]
INTERFACE Shape
  MODULE PROCEDURE get_shape
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                           Size@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return size of [[RealMatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function return size of `RealMatrix_`
! - If `Dims` is present and equal to 1 then total number of rows (m)
! - If `Dims` is present and equal to 2 then total number of cols (n)
! - If `Dimes` is absent then Ans = m * n
!### Usage
!
! ```fortran
!	trow = SIZE( Obj, 1 )
! tcol = SIZE( Obj, 2 )
! t = SIZE( Obj )
! ```

MODULE PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION get_size
END INTERFACE

!>
! Generic interface to get size of [[RealMatrix_]]
INTERFACE Size
  MODULE PROCEDURE get_size
END INTERFACE Size

PUBLIC :: Size

!----------------------------------------------------------------------------
!                                                    AllocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Allocate memory for [[RealMatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine allocate memory for [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!	call allocateData( Obj, Dims )
! ```

MODULE PURE SUBROUTINE Allocate_Data( Obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Dims(2)
END SUBROUTINE Allocate_Data

END INTERFACE

!>
! Generic subroutine to allocate memory for [[realmatrix_]]
INTERFACE AllocateData
  MODULE PROCEDURE Allocate_Data
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                         Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Display content of [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine displays content of [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!	call display( obj, 'mat', stdout )
! ```

MODULE SUBROUTINE Display_obj( Obj, Msg, UnitNo )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE


INTERFACE
!! Display content of [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine displays content of [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!	call display( obj, 'mat', stdout )
! ```

MODULE SUBROUTINE Display_obj_vec( Obj, Msg, UnitNo )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj_vec
END INTERFACE

!>
! Generic interface to display content of [[realmatrix_]]
INTERFACE Display
  MODULE PROCEDURE Display_obj, Display_obj_vec
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                  DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Deallocate data in [[RealMatrix_]]

!> authors: Dr. Vikas Sharma
!
! This routine deallocates data stored in Obj
!
!### Usage
!
! ```fortran
!	call deallocateData( Obj )
! ```

MODULE PURE SUBROUTINE Deallocate_Data( Obj )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Deallocate_Data
END INTERFACE

!>
! Generic interface to deallocate [[RealMatrix_]]
INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Initiate [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate `Obj` with shape `Dims`
!
!### Usage
!
! ```fortran
!	call initiate( obj, [2,3] )
! ```
! The above call will initiate a matrix of shape (2,3)

MODULE PURE SUBROUTINE initiate_obj( Obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
END SUBROUTINE initiate_obj
END INTERFACE

INTERFACE
!! Initiate [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate vector of [[realmatrix_]] with shape `Dims`
!
!### Usage
!
!```fortran
! type( realmatrix_ ) :: obj( 4 )
!	call initiate( obj, [2,3] )
!```
! The above call will initiate `obj` vector of matrices of shape (2,3)

MODULE PURE SUBROUTINE initiate_obj_vector_a( Obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
END SUBROUTINE initiate_obj_vector_a
END INTERFACE

INTERFACE
!! Initiate [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate vector of [[realmatrix_]] with matrices of
! different shapes given in `Dims`
! - `Dims` has two columns; the first column denotes the number of rows, and
! second column denotes the number of columns in a matrix
! - irow of `Dims` corresponds to the shape of `Obj(irow)`
! - in this way `SIZE(obj)` should be equal to the SIZE(Dims, 1)
!
!### Usage
!
!```fortran
! type( realmatrix_ ) :: obj( 3 )
! integer( i4b ) :: Dims( 3, 2  )
!
! Dims( 1, : ) = [2,2]
! Dims( 2, : ) = [4,4]
! Dims( 3, : ) = [4,4]
!	call initiate( obj, Dims )
!```
!
! - The above call will initiate a obj( 1 ) with shape (2,2)
! - The above call will initiate a obj( 2 ) with shape (4,4)
! - The above call will initiate a obj( 3 ) with shape (4,4)

MODULE PURE SUBROUTINE initiate_obj_vector_b( Obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Dims( :, : )
END SUBROUTINE initiate_obj_vector_b
END INTERFACE

INTERFACE
!! Initiate [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine performs `Obj % Val = Val`, i.e., initiate `Obj` with `Val`
!
!### Usage
!
! ```fortran
!	call initiate( obj, val )
! ```

MODULE PURE SUBROUTINE initiate_obj_val( Obj, Val )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE initiate_obj_val
END INTERFACE

!> Generic interface to initiate [[realmatrix_]]
INTERFACE Initiate
  MODULE PROCEDURE initiate_obj_vector_a, initiate_obj_vector_b, &
    & initiate_obj, initiate_obj_val
END INTERFACE Initiate

PUBLIC :: Initiate

INTERFACE ASSIGNMENT( = )
    MODULE PROCEDURE initiate_obj_val
END INTERFACE

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                          Matrix@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Fucntion that will initiate [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! function that return [[realmatrix_]] of specified dimensions
!
!### Usage
!
! ```fortran
!	obj = RealMatrix( [2,2] )
! ```

  MODULE PURE FUNCTION Constructor1( Dims ) RESULT( Obj )
    TYPE( RealMatrix_ ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
  END FUNCTION Constructor1
END INTERFACE

!> Generic function to construct [[realmatrix_]]
INTERFACE RealMatrix
  MODULE PROCEDURE Constructor1
END INTERFACE RealMatrix

PUBLIC :: RealMatrix

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return identity matrix of integers

!> authors: Dr. Vikas Sharma
!
! This function return identity matrix of integer
!
!### Usage
!
! ```fortran
!	i = eye( 2, 1_I4B )
! ```

MODULE PURE FUNCTION eye_int( m, DataType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m, DataType
  INTEGER( I4B ) :: Ans( m, m )
END FUNCTION eye_int
END INTERFACE

INTERFACE
!! Return identity matrix of real numbers

!> authors: Dr. Vikas Sharma
!
! This function returns identity matrux of reals
!
!### Usage
!
! ```fortran
!	e = eye( 5, 1.0_dfp )
! ```

MODULE PURE FUNCTION eye_real( m, DataType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m
  REAL( DFP ) :: Ans( m, m )
  REAL( DFP ), INTENT( IN ) :: DataType
END FUNCTION eye_real
END INTERFACE

INTERFACE
!! Return identity matrix of type [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function returns identity matrix of type [[realmatrix_]]
!
!### Usage
!
! ```fortran
!	obj = eye( 3, typeRealMatrix )
! ```

MODULE PURE FUNCTION eye_obj( m, DataType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m
  TYPE( RealMatrix_ ) :: Ans
  TYPE( RealMatrix_ ), INTENT( IN ) ::  DataType
END FUNCTION eye_obj
END INTERFACE

INTERFACE
!! Return identity matrix of real number

!> authors: Dr. Vikas Sharma
!
! This function returns the identity matrix of real numbers
!
!### Usage
!
! ```fortran
!	e = eye( 4 )
! ```

MODULE PURE FUNCTION eye_real_b( m ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m
  REAL( DFP ) :: Ans( m, m )
END FUNCTION eye_real_b
END INTERFACE

!> Generic interface for obtaining identity matrix in array or [[realmatrix_]]
INTERFACE Eye
  MODULE PROCEDURE eye_int, eye_obj, eye_real, eye_real_b
END INTERFACE Eye

PUBLIC :: Eye

!----------------------------------------------------------------------------
!                                                        Convert@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Rearrange the dofs in finite element matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine changes the storage pattern of a two-d matrix
!  - Usually element matrix in easifem are stored in `FMT_DOF`
!  - Global matrices/tanmat, however, are stored in `FMT_Nodes`
!  - This subroutine is, therefore, in settings or adding values in
! [[SparseMatrix_]].
!
! > This subroutine converts changes the storage format of dense matrix.
! Usually, elemental finite element matrix is stored in `DOF_FMT`, and global ! matrix/ tanmat, may be stored in `Nodes_FMT`.
!
!### Usage
!
! ```fortran
!	call Convert( From, To, DOFToNodes, nns, tdof )
! ```

MODULE PURE SUBROUTINE convert_DofToNodes( From, To, Conversion, nns, tdof )
  REAL( DFP ), INTENT( IN ) :: From( :, : )
    !! Matrix in one format
  REAL( DFP ), INTENT( INOUT ), ALLOCATABLE :: To( :, : )
    !! Matrix is desired format
  INTEGER( I4B ), INTENT( IN ) :: Conversion
    !! `Conversion` can be `NodesToDOF` or `DOFToNodes`
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nns, tdof
END SUBROUTINE convert_DofToNodes
END INTERFACE

INTERFACE
!! Rearrange the dofs in finite element matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine changes the storage pattern of a two-d matrix
!  - Usually element matrix in easifem are stored in `FMT_DOF`
!  - Global matrices/tanmat, however, are stored in `FMT_Nodes`
!  - This subroutine is, therefore, in settings or adding values in
! [[SparseMatrix_]].
!
! > This subroutine converts changes the storage format of dense matrix.
! Usually, elemental finite element matrix is stored in `DOF_FMT`, and global ! matrix/ tanmat, may be stored in `Nodes_FMT`.
!
!### Usage
!
! ```fortran
!	call Convert( From, To, DOFToNodes, nns, tdof )
! ```

MODULE PURE SUBROUTINE realmat_convert_doftonodes(  From, To, Conversion, &
  & nns, tdof )
  TYPE( RealMatrix_ ), INTENT( IN ) :: From
    !! Matrix in one format
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: To
    !! Matrix in one format
  INTEGER( I4B ), INTENT( IN ) :: Conversion
    !! `Conversion` can be `NodesToDOF` or `DOFToNodes`
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nns, tdof
END SUBROUTINE realmat_convert_doftonodes
END INTERFACE

!>  Generic subroutine for covertsion
INTERFACE Convert
  MODULE PROCEDURE convert_DofToNodes, realmat_convert_doftonodes
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                                     Convert
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine converts rank4  matrix to rank2 matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine converts a rank4 matrix to rank2 matrix
MODULE PURE SUBROUTINE convert_mat4_to_mat2( From, To )
  REAL( DFP ), INTENT( IN ) :: From( :, :, :, : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: To( :, : )
END SUBROUTINE convert_mat4_to_mat2
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE convert_mat4_to_mat2
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                 Matrix_Pointer@Constructor
!----------------------------------------------------------------------------

!> Generic function to get pointer to [[RealMatrix_]]
INTERFACE RealMatrix_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE RealMatrix_Pointer

PUBLIC :: RealMatrix_Pointer

!----------------------------------------------------------------------------
!                                                            Sym@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return sym(Obj) = 0.5*(Obj + transpose( Obj ) )

!> authors: Dr. Vikas Sharma
!
! Return symmetric part of Obj
!
!### Usage
!
! ```fortran
!	realMat = Sym( Obj )
! ```

MODULE PURE FUNCTION sym_obj( Obj ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
    !! Real matrix
  TYPE( RealMatrix_ ) :: Ans
    !! Symmetric real matrix
END FUNCTION sym_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Sym@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return sym(Obj) = 0.5*(Obj + transpose( Obj ) )

!> authors: Dr. Vikas Sharma
!
! Return symmetric part of Obj
!
!### Usage
!
! ```fortran
!	realMat = Sym( Obj )
! ```

MODULE PURE FUNCTION sym_array( Obj ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj( :, : )
    !! Two dimensiona array
  REAL( DFP ) :: Ans( SIZE( Obj, 1 ), SIZE( Obj, 2 ) )
    !! Symmetric array
END FUNCTION sym_array
END INTERFACE

!>
! Generic interface for getting symmetric part of [[RealMatrix_]]
INTERFACE SYM
  MODULE PROCEDURE sym_obj, sym_array
END INTERFACE SYM

PUBLIC :: SYM


!----------------------------------------------------------------------------
!                                                        SkewSym@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return SkewSym(Obj) = 0.5*(Obj + transpose( Obj ) )

!> authors: Dr. Vikas Sharma
!
! Return SkewSymmetric part of Obj
!
!### Usage
!
! ```fortran
!	realMat = SkewSym( Obj )
! ```

MODULE PURE FUNCTION SkewSym_obj( Obj ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
    !! Real matrix
  TYPE( RealMatrix_ ) :: Ans
    !! SkewSymmetric real matrix
END FUNCTION SkewSym_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SkewSym@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return SkewSym(Obj) = 0.5*(Obj + transpose( Obj ) )

!> authors: Dr. Vikas Sharma
!
! Return SkewSymmetric part of Obj
!
!### Usage
!
! ```fortran
!	realMat = SkewSym( Obj )
! ```

MODULE PURE FUNCTION SkewSym_array( Obj ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj( :, : )
    !! Two dimensiona array
  REAL( DFP ) :: Ans( SIZE( Obj, 1 ), SIZE( Obj, 2 ) )
    !! SkewSymmetric array
END FUNCTION SkewSym_array
END INTERFACE

!>
! Generic interface for getting SkewSymmetric part of [[RealMatrix_]]
INTERFACE SkewSym
  MODULE PROCEDURE SkewSym_obj, SkewSym_array
END INTERFACE SkewSym

PUBLIC :: SkewSym

!----------------------------------------------------------------------------
!                                              MakeDiagonalCopies@Construct
!----------------------------------------------------------------------------

INTERFACE
!! Make diagonal copies of Matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine makes `ncopy` diagonal copies of `Mat`
! - The size of `Mat` on return is nCopy * SIZE( Mat, 1 )
!
!### Usage
!
! ```fortran
!	call MakeDiagonalCopies( Mat, nCopy )
! ```

MODULE PURE SUBROUTINE realmat_make_diag_copy1( Mat, nCopy )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) ::  Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: nCopy
END SUBROUTINE realmat_make_diag_copy1
END INTERFACE

INTERFACE
!! Make diagonal copies of Matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine makes `ncopy` diagonal copies of `Mat`
!
!### Usage
!
! ```fortran
!	call MakeDiagonalCopies( From = Mat, To = anotherMat, nCopy = nCopy )
! ```

MODULE PURE SUBROUTINE realmat_make_diag_copy2( From, To, nCopy )
  REAL( DFP ),  INTENT( IN ) :: From( :, : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: To( :, : )
  INTEGER( I4B ), INTENT( IN ) :: nCopy
END SUBROUTINE realmat_make_diag_copy2
END INTERFACE


INTERFACE
!! Make diagonal copies of [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine makes `ncopy` diagonal copies of `Mat`
! - The size of `Mat` on return is nCopy * SIZE( Mat, 1 )
!
!### Usage
!
! ```fortran
!	call MakeDiagonalCopies( Mat, nCopy )
! ```

MODULE PURE SUBROUTINE realmat_make_diag_copy3(  Mat, ncopy )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Mat
  INTEGER( I4B ), INTENT( IN ) :: ncopy
END SUBROUTINE realmat_make_diag_copy3
END INTERFACE

INTERFACE
!! Make diagonal copies of Matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine makes `ncopy` diagonal copies of `Mat`
!
!### Usage
!
! ```fortran
!	call MakeDiagonalCopies( From = Mat, To = anotherMat, nCopy = nCopy )
! ```

MODULE PURE SUBROUTINE realmat_make_diag_copy4( From, To, nCopy )
  TYPE( RealMatrix_ ),  INTENT( IN ) :: From
  TYPE( RealMatrix_ ), INTENT( INOUT) :: To
  INTEGER( I4B ), INTENT( IN ) :: nCopy
END SUBROUTINE realmat_make_diag_copy4
END INTERFACE

!> Generic subroutine to make diagonal copies of [[RealMatrix_]] and 2d arrays
INTERFACE MakeDiagonalCopies
  MODULE PROCEDURE realmat_make_diag_copy1, realmat_make_diag_copy2, &
      & realmat_make_diag_copy3, &
      & realmat_make_diag_copy4
END INTERFACE MakeDiagonalCopies

PUBLIC :: MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                                         Random@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Set a values in [[realmatrix_]] obj to random values

!> authors: Dr. Vikas Sharma
!
! This subroutine set values in `Obj%Val` to random
! - This subroutine calls `RANDOM_NUMBER()` function from Fortran

MODULE SUBROUTINE realmat_random_number( Obj, m, n )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: m, n
END SUBROUTINE realmat_random_number
END INTERFACE

INTERFACE RANDOM_NUMBER
  MODULE PROCEDURE realmat_random_number
END INTERFACE RANDOM_NUMBER

PUBLIC :: RANDOM_NUMBER

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------