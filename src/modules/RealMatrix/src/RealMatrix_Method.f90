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
! date: 	6 March 2021
! summary: This module contains methods for [[RealMatrix_]] data type

MODULE RealMatrix_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE


!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Display content of [[realmatrix_]]
!
!### Introduction
!
! This subroutine displays content of [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!	call display( obj, 'mat', stdout )
! ```

INTERFACE
MODULE SUBROUTINE Display_obj( Obj, Msg, UnitNo )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Display content of [[realmatrix_]]
!
!### Introduction
!
! This subroutine displays content of [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!	call display( obj, 'mat', stdout )
! ```

INTERFACE
MODULE SUBROUTINE Display_obj_vec( Obj, Msg, UnitNo )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj_vec
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE Display_obj, Display_obj_vec
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Return shape of [[RealMatrix_]]
!
!### Introduction
!
! This function return shape of [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!	s = Shape( Obj )
! ```

INTERFACE
MODULE PURE FUNCTION get_shape( Obj ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
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
! date: 6 March 2021
! summary: Return size of [[RealMatrix_]]
!
!### Introduction
!
! This function return size of `RealMatrix_`
! - If `Dims` is present and equal to 1 then total number of rows (m)
! - If `Dims` is present and equal to 2 then total number of cols (n)
! - If `Dimes` is absent then Ans = m * n
!
!### Usage
!
!```fortran
!	trow = SIZE( Obj, 1 )
! tcol = SIZE( Obj, 2 )
! t = SIZE( Obj )
!```

INTERFACE
MODULE PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION get_size
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE get_size
END INTERFACE Size

PUBLIC :: Size

!----------------------------------------------------------------------------
!                                                  AllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	This subroutine allocate memory for [[RealMatrix_]]
!
!
!### Usage
!
! ```fortran
!	call allocateData( Obj, Dims )
! ```


INTERFACE
MODULE PURE SUBROUTINE Allocate_Data( Obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Dims(2)
END SUBROUTINE Allocate_Data

END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE Allocate_Data
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                  DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Deallocate data in [[RealMatrix_]]
!
!### Introduction
!
! This routine deallocates data stored in Obj
!
!### Usage
!
! ```fortran
!	call deallocateData( Obj )
! ```

INTERFACE
MODULE PURE SUBROUTINE Deallocate_Data( Obj )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Deallocate_Data
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine initiate `Obj` with shape `Dims`
!
!### Usage
!
!```fortran
!	call initiate( obj, [2,3] )
!```
! The above call will initiate a matrix of shape (2,3)

INTERFACE
MODULE PURE SUBROUTINE initiate_obj( Obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
END SUBROUTINE initiate_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	This subroutine initiate vector of [[realmatrix_]] with shape `Dims`
!
!### Usage
!
!```fortran
! type( realmatrix_ ) :: obj( 4 )
!	call initiate( obj, [2,3] )
!```
! The above call will initiate `obj` vector of matrices of shape (2,3)

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_vector_a( Obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
END SUBROUTINE initiate_obj_vector_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Initiate an instance of [[RealMatrix_]]
!
!### Introduction
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

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_vector_b( Obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Dims( :, : )
END SUBROUTINE initiate_obj_vector_b
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: This subroutine performs `Obj % Val = Val`, i.e., initiate `Obj` with `Val`
!
!### Usage
!
! ```fortran
!	call initiate( obj, val )
! ```

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_val( Obj, Val )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE initiate_obj_val
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

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

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Constructor function for [[RealMatrix_]]
!
!### Introduction
!
! This function returns an instance of [[realmatrix_]]
!
!### Usage
!
!```fortran
!	obj = RealMatrix( [2,2] )
!```

INTERFACE
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

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Return an identity matrix of an integers
!
! This function returns an identity matrix of type integer
!
!### Usage
!
!```fortran
!	i = eye( 2, 1_I4B )
!```

INTERFACE
MODULE PURE FUNCTION eye_int( m, DataType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m, DataType
  INTEGER( I4B ) :: Ans( m, m )
END FUNCTION eye_int
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

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

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> Generic interface for obtaining identity matrix in array or [[realmatrix_]]
INTERFACE Eye
  MODULE PROCEDURE eye_int, eye_obj, eye_real, eye_real_b
END INTERFACE Eye

PUBLIC :: Eye

!----------------------------------------------------------------------------
!                                                        Convert@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Rearrange the degrees of freedom in a finite element matrix
!
!### Introduction
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
!```fortran
!	call Convert( From, To, DOFToNodes, nns, tdof )
!```

INTERFACE
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

!----------------------------------------------------------------------------
!                                                        Convert@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Rearrange the dofs in finite element matrix
!
!### Introduction
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
!```fortran
!```

INTERFACE
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

!----------------------------------------------------------------------------
!                                                        Convert@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine converts rank4  matrix to rank2 matrix
!
!### Introduction
!
! This subroutine converts a rank4 matrix to rank2 matrix
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE convert_mat4_to_mat2( From, To )
  REAL( DFP ), INTENT( IN ) :: From( :, :, :, : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: To( :, : )
END SUBROUTINE convert_mat4_to_mat2
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Convert@Constructor
!----------------------------------------------------------------------------

INTERFACE Convert
  MODULE PROCEDURE convert_DofToNodes, realmat_convert_doftonodes, &
    & convert_mat4_to_mat2
END INTERFACE Convert

PUBLIC :: Convert

!----------------------------------------------------------------------------
!                                                 Matrix_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor_1( Dims ) RESULT( Obj )
  CLASS( RealMatrix_ ), POINTER :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
END FUNCTION Constructor_1
END INTERFACE

!> Generic function to get pointer to [[RealMatrix_]]
INTERFACE RealMatrix_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE RealMatrix_Pointer

PUBLIC :: RealMatrix_Pointer

!----------------------------------------------------------------------------
!                                                            Sym@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Return sym(Obj) = 0.5*(Obj + transpose( Obj ) )
!
!### Introduction
!
! Return symmetric part of Obj
!
!### Usage
!
! ```fortran
!	realMat = Sym( Obj )
! ```

INTERFACE
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

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Return sym(Obj) = 0.5*(Obj + transpose( Obj ) )
!
!### Introduction
!
! Return symmetric part of Obj
!
!### Usage
!
!```fortran
!	realMat = Sym( Obj )
!```

INTERFACE
MODULE PURE FUNCTION sym_array( Obj ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj( :, : )
    !! Two dimensiona array
  REAL( DFP ) :: Ans( SIZE( Obj, 1 ), SIZE( Obj, 2 ) )
    !! Symmetric array
END FUNCTION sym_array
END INTERFACE

INTERFACE SYM
  MODULE PROCEDURE sym_obj, sym_array
END INTERFACE SYM

PUBLIC :: SYM

!----------------------------------------------------------------------------
!                                                        SkewSym@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Return SkewSymmetric part of Obj
!
!### Usage
!
!```fortran
!	realMat = SkewSym( Obj )
!```

INTERFACE
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

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	Return SkewSym(Obj) = 0.5*(Obj + transpose( Obj ) )
!
!### Introduction
!
! Return SkewSymmetric part of Obj
!
!### Usage
!
!```fortran
!	realMat = SkewSym( Obj )
!```

INTERFACE
MODULE PURE FUNCTION SkewSym_array( Obj ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj( :, : )
    !! Two dimensiona array
  REAL( DFP ) :: Ans( SIZE( Obj, 1 ), SIZE( Obj, 2 ) )
    !! SkewSymmetric array
END FUNCTION SkewSym_array
END INTERFACE

INTERFACE SkewSym
  MODULE PROCEDURE SkewSym_obj, SkewSym_array
END INTERFACE SkewSym

PUBLIC :: SkewSym

!----------------------------------------------------------------------------
!                                              MakeDiagonalCopies@Construct
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Make diagonal copies of Matrix
!
!### Introduction
!
! This subroutine makes `ncopy` diagonal copies of `Mat` The size of `Mat` on return is nCopy * SIZE( Mat, 1 )
!
!### Usage
!
!```fortran
!	call MakeDiagonalCopies( Mat, nCopy )
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_make_diag_copy1( Mat, nCopy )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) ::  Mat( :, : )
  INTEGER( I4B ), INTENT( IN ) :: nCopy
END SUBROUTINE realmat_make_diag_copy1
END INTERFACE

!----------------------------------------------------------------------------
!                                              MakeDiagonalCopies@Construct
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Make diagonal copies of Matrix
!
! This subroutine makes `ncopy` diagonal copies of `Mat`
!
!### Usage
!
!```fortran
!	call MakeDiagonalCopies( From = Mat, To = anotherMat, nCopy = nCopy )
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_make_diag_copy2( From, To, nCopy )
  REAL( DFP ),  INTENT( IN ) :: From( :, : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: To( :, : )
  INTEGER( I4B ), INTENT( IN ) :: nCopy
END SUBROUTINE realmat_make_diag_copy2
END INTERFACE

!----------------------------------------------------------------------------
!                                              MakeDiagonalCopies@Construct
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Make diagonal copies of [[realmatrix_]]
!
! This subroutine makes `ncopy` diagonal copies of `Mat`, The size of `Mat` on return is nCopy * SIZE( Mat, 1 )
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( Mat, nCopy )
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_make_diag_copy3(  Mat, ncopy )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Mat
  INTEGER( I4B ), INTENT( IN ) :: ncopy
END SUBROUTINE realmat_make_diag_copy3
END INTERFACE

!----------------------------------------------------------------------------
!                                              MakeDiagonalCopies@Construct
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Make diagonal copies of Matrix
!
!### Introduction
!
! This subroutine makes `ncopy` diagonal copies of `Mat`
!
!### Usage
!
!```fortran
!	call MakeDiagonalCopies( From = Mat, To = anotherMat, nCopy = nCopy )
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_make_diag_copy4( From, To, nCopy )
  TYPE( RealMatrix_ ),  INTENT( IN ) :: From
  TYPE( RealMatrix_ ), INTENT( INOUT) :: To
  INTEGER( I4B ), INTENT( IN ) :: nCopy
END SUBROUTINE realmat_make_diag_copy4
END INTERFACE

INTERFACE MakeDiagonalCopies
  MODULE PROCEDURE realmat_make_diag_copy1, realmat_make_diag_copy2, &
      & realmat_make_diag_copy3, &
      & realmat_make_diag_copy4
END INTERFACE MakeDiagonalCopies

PUBLIC :: MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                                 Random_number@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set a values in [[realmatrix_]] obj to random values
!
!### Introduction
!
! This subroutine set values in `Obj%Val` to random
! - This subroutine calls `RANDOM_NUMBER()` function from Fortran
!
!### Usage
!
!```fortran
!
!```

INTERFACE
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
!                                                  TestMatrix@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 March 2021
! summary: This function returns the example matrix

INTERFACE
MODULE FUNCTION TestMatrix( matNo ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: matNo
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION TestMatrix
END INTERFACE

PUBLIC :: TestMatrix

!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!### Introduction
!
! This function returns the value stored in `Obj % Val` in a 2D fortran array
!
!### Usage
!
!```fortran
!	Val = ArrayValues( Obj, 1.0_dfp )
!```

INTERFACE
MODULE PURE FUNCTION f_getValues_Real( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION f_getValues_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Returns the values of [[RealMatrix_]] obj in 2D array
!
!### Introduction
!
! This function returns a section of `Obj % Val` in a 2D fortran array. This
! is equivalent to `Val = Obj % Val(RIndx, CIndx)`
!
!### Usage
!
!```fortran
! integer( i4b ) :: r( 2 ), c( 2 )
! type( RealMatrix_ ) :: Obj
! call initiate( Obj, [4,4] )
! call random_number( obj ); r=[1,2]; c=[2,3]
! Val = ArrayValues( Obj, R, C, 1.0_dfp )
!```
!
! The above call will return `Obj%Val[1:2, 2:3]`


INTERFACE
MODULE PURE FUNCTION f_getSectionValues_Real( Obj, RIndx, CIndx, DataType ) &
  & RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: RIndx( : ), CIndx( : )
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION f_getSectionValues_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!### Introduction
!
! This function returns a section of `Obj % Val` in a 2D fortran array. This
! is equivalent to `Val = Obj % Val(is:ie:s, is:ie:s)`
!
!### Usage
!
!```fortran
! integer( i4b ) :: r( 2 ), c( 2 )
! type( RealMatrix_ ) :: Obj
! call initiate( Obj, [4,4] )
! call random_number( obj )
! Val = ArrayValues( Obj, 1, 2, 1, 1.0_dfp )
!```
!
! The above call will return `Obj%Val[1:2:1, 1:2:1]`

INTERFACE
MODULE PURE FUNCTION f_getValuesFromTriplet_Real( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION f_getValuesFromTriplet_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns [[RealMatrix_]] obj from [[realmatrix_]]
!
!### Introduction
!
! This function is essentially copy method `Ans=Obj`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION f_getValues_self( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION f_getValues_self
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]
!
!### Introduction
!
! This function is essentially copy method `Ans=Obj(RIndx, CIndx)`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION f_getSectionValues_Self( Obj, RIndx, CIndx, DataType ) &
  & RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: RIndx( : ), CIndx( : )
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION f_getSectionValues_Self
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]
!
!### Introduction
! 	This function is essentially copy method `Ans=Obj(is:ie, is:ie)`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION f_getValuesFromTriplet_self( Obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION f_getValuesFromTriplet_self
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns values in 2D fortran array from [[realmatrix_]]
!
!### Introduction
!
! This function combines all [[realmatrix_]] value of `Obj` and
! returns a 2D fortrn array
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION f_getValues_1( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj( :, : )
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION f_getValues_1

!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns [[realmatrix_]] object from a 2D array of [[realmatrix_]]
!
!### Introduction
!
! This function combines all [[realmatrix_]] value of `Obj` and
! returns a [[realmatrix_]] object
!
!### Usage
!
!```fortran
!
!```

MODULE PURE FUNCTION f_getValues_2( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj( :, : )
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION f_getValues_2
END INTERFACE

INTERFACE ArrayValues
  MODULE PROCEDURE f_getValues_Real, f_getSectionValues_Real, &
    & f_getValuesFromTriplet_Real, f_getValues_self, f_getSectionValues_Self,&
    & f_getValuesFromTriplet_self, f_getValues_1, f_getValues_2
END INTERFACE ArrayValues

PUBLIC :: ArrayValues

!----------------------------------------------------------------------------
!                                                             Copy@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Copy from [[realmatrix_]] to 2D fortran array
!
!### Introduction
!
! This subroutine copy the contents of [[realmatrix_]] object into a 2D
! fortran array
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE Copy_Obj_to_Val( From, To )
  TYPE( RealMatrix_ ), INTENT( IN ) :: From
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: To( :, : )
END SUBROUTINE Copy_Obj_to_Val
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Copy@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Copy from [[realmatrix_]] to another [[realmatrix_]]
!
!### Introduction
!
! This subroutine copy the contents of [[realmatrix_]] object to another
! [[realmatrix_]] object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE Copy_Obj_to_Obj( From, To )
  TYPE( RealMatrix_ ), INTENT( IN ) :: From
  TYPE( RealMatrix_ ), INTENT( INOUT) :: To
END SUBROUTINE Copy_Obj_to_Obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Copy@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  6 March 2021
! summary: Copy from 2D fortran array to [[realmatrix_]]
!
!### Introduction
!
! This subroutine copy the contents of a 2D fortran array to [[realmatrix_]] object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE Copy_Val_to_Obj( From, To )
  REAL( DFP ), INTENT( IN ) :: From( :, : )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: To
END SUBROUTINE Copy_Val_to_Obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Copy@getValues
!----------------------------------------------------------------------------

INTERFACE COPY
  MODULE PROCEDURE Copy_Obj_to_Val, Copy_Obj_to_Obj, Copy_Val_to_Obj
END INTERFACE COPY

PUBLIC :: COPY

INTERFACE CONVERT
  MODULE PROCEDURE Copy_Obj_to_Val, Copy_Obj_to_Obj, Copy_Val_to_Obj
END INTERFACE CONVERT

!----------------------------------------------------------------------------
!                                                     ArrayPointer@getValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Get pointer to the values stored inside [[realmatrix_]]
!
!### Introduction
!
! This function returns the pointer to the values stored inside the
! [[realmatrix_]]
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE FUNCTION f_getPointer_Real( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ), TARGET :: Obj
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), POINTER :: Ans( :, : )
END FUNCTION f_getPointer_Real
END INTERFACE

INTERFACE ArrayPointer
  MODULE PROCEDURE f_getPointer_Real
END INTERFACE ArrayPointer

PUBLIC :: ArrayPointer

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add values in [[realmatrix_]]
!
!### Introduction
!
! This subroutine set `Obj % Val` to `Val`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_setValues_1( Obj, Val )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE realmat_setValues_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

INTERFACE
!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set values in [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine set values in `Obj % Val`
! `Obj % Val( i, j ) = Val`
!
!### Usage
!
!```fortran
!
!```

MODULE PURE SUBROUTINE realmat_setValues_2( Obj, Val, Row, Col )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val
  INTEGER( I4B ), INTENT( IN ) :: Col, Row
END SUBROUTINE realmat_setValues_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Set values in [[realmatrix_]]
!
!### Introduction
!
! This subroutine set values in `Obj % Val`
!

INTERFACE
MODULE PURE SUBROUTINE realmat_setValues_3( Obj, Val, Row, Col )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  INTEGER( I4B ), INTENT( IN ) :: Col( : ), Row( : )
END SUBROUTINE realmat_setValues_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set values in [[realmatrix_]]
!
!### Introduction
!
! This subroutine set values in `Obj % val`
! - If `ExtraOption=MATRIX_DIAGONAL` then diagonal values are set; and `Indx` denotes diagonal number with `0` being the main diagonal
! - If `Extraoption=MATRIX_ROW` then row values are set; `Indx` then denotes row number
! - If `Extraoption=MATRIX_COLUMN` then col values are set; `Indx` then denotes col number
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_setValues_4( Obj, Val, Indx, ExtraOption )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: Indx
  INTEGER( I4B ), INTENT( IN ) :: ExtraOption
END SUBROUTINE realmat_setValues_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setValues@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Set values in [[realmatrix_]]
!
!### Introduction
!
! This subroutine set values in `Obj % val`
! - If `ExtraOption=MATRIX_DIAGONAL` then diagonal values are set; and `Indx` denotes the diagonal number with `0` being the main diagonal
! - If `Extraoption=ROW` then row values are set; `Indx` then denotes row number
! - If `Extraoption=COLUMN` then col values are set; `Indx` then denotes col number
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_setValues_5( Obj, Val, Indx, ExtraOption )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  INTEGER( I4B ), INTENT( IN ) :: Indx ( : ), ExtraOption
END SUBROUTINE realmat_setValues_5
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
INTERFACE setValues
  MODULE PROCEDURE realmat_setValues_1, realmat_setValues_2, &
    & realmat_setValues_3, realmat_setValues_4, &
    & realmat_setValues_5
END INTERFACE setValues

PUBLIC :: setValues

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Add contribution in values of [[realmatrix_]]
!
!### Introduction
!
! This subroutine adds contribution in values of `Obj % Val`. This subroutine performs following task:
!
! $$obj = obj <op> scale * val $$
!
! Here `op` can be `+, -, *, /`.
!
!@todo
! 	Use Blas routines or OpenMP support?
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE realmat_addVal_1( Obj, Val, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  REAL( DFP ), INTENT( IN ) :: Scale
    !! Scaling for `Val`
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
    !! operator symbol; `+, -, *, /`
END SUBROUTINE realmat_addVal_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add contribution in values of [[Realmatrix_]]
!
!### Introduction
!
! This subroutine adds contribution in values of `Obj % Val`
!
!```fortran
! obj % val = obj % val *Op* scale * val
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_addVal_2( Obj, Val, Row, Col, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val
  REAL( DFP ), INTENT( IN ) :: Scale
  INTEGER( I4B ), INTENT( IN ) :: Row
  INTEGER( I4B ), INTENT( IN ) :: Col
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
END SUBROUTINE realmat_addVal_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Add contribution in values of [[realmatrix_]]
!
!### Introduction
!
! This subroutine adds contribution in values of `Obj % Val`
!
!```fortran
! obj % val = obj % val *Op* scale * val
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_addVal_3( Obj, Val, Row, Col, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  REAL( DFP ), INTENT( IN ) :: Scale
  INTEGER( I4B ), INTENT( IN ) :: Row( : )
  INTEGER( I4B ), INTENT( IN ) :: Col( : )
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
END SUBROUTINE realmat_addVal_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add contribution in values of [[Realmatrix_]]
!
!### Introduction
!
! This subroutine adds contribution in values of `Obj % Val`
!
! ```fortran
! obj % val = obj % val *Op* scale * val
! ```

INTERFACE
MODULE PURE SUBROUTINE realmat_addVal_4( Obj, Val, Indx, ExtraOption, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : ), Scale
  INTEGER( I4B ), INTENT( IN ) :: Indx
  INTEGER( I4B ), INTENT( IN ) :: ExtraOption
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
END SUBROUTINE realmat_addVal_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                  addContribution@setValues
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Add contribution in values of [[realmatrix_]]
!
!### Introduction
!
! This subroutine adds contribution in values of `Obj % Val`
!
!```fortran
! obj % val = obj % val *Op* scale * val
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_addVal_5( Obj, Val, Indx, ExtraOption, Scale, Op )
  TYPE( RealMatrix_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : ), Scale
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER( I4B ), INTENT( IN ) :: ExtraOption
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Op
END SUBROUTINE realmat_addVal_5
END INTERFACE

INTERFACE addContribution
  MODULE PROCEDURE realmat_addVal_1, realmat_addVal_2, realmat_addVal_3, &
    & realmat_addVal_4, realmat_addVal_5
END INTERFACE addContribution

PUBLIC :: addContribution

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION MatMul_1( Obj1, Obj2 ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj1, Obj2
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION MatMul_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION MatMul_2( Obj, Vec ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  REAL( DFP ), ALLOCATABLE :: Ans( : )
END FUNCTION MatMul_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION MatMul_3( Obj, Vec ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  TYPE( RealVector_ ), INTENT( IN ) :: Vec
  TYPE( RealVector_ ) :: Ans
END FUNCTION MatMul_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE Matmul
  MODULE PROCEDURE MatMul_1, MatMul_2, MatMul_3
END INTERFACE Matmul

PUBLIC :: Matmul

!----------------------------------------------------------------------------
!                                                         JacobiMethod@LAPACK
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Returns all the eigenvalues of symmetric matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine computes all eigenvalues and eigenvectors of a real
! symmetric N × N matrix A.
! -  On output, elements of `A` above the diagonal are destroyed.
! -  `d` is a vector of length N that returns the eigenvalues of `A`.
! -  `V` is an `N × N` matrix whose columns contain on output, the normalized
!  eigenvectors of A.
! -  `tRot` returns the number of Jacobi rotations that were required.
!
! ### Reference:: Numerical Reciepe in Fortran, Page 1225
!

INTERFACE
MODULE PURE SUBROUTINE eig_jacobi_method( Mat, EigenValues, EigenVectors, &
  & MaxIter )
  INTEGER( I4B ), INTENT( IN ) :: MaxIter
  REAL( DFP ), INTENT( INOUT ) :: EigenValues( : )
  REAL( DFP ), INTENT( INOUT ) :: Mat( :, : )
  REAL( DFP ), INTENT( INOUT ) :: EigenVectors( :, : )
END SUBROUTINE eig_jacobi_method
END INTERFACE

!> Generic subroutine for computing eigen value of a symmetric matrix
INTERFACE JacobiMethod
  MODULE PROCEDURE eig_jacobi_method
END INTERFACE JacobiMethod

PUBLIC :: JacobiMethod

END MODULE RealMatrix_Method