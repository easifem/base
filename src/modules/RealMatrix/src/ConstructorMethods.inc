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

PUBLIC :: Shape
PUBLIC :: Size
PUBLIC :: TotalDimension
PUBLIC :: setTotalDimension
PUBLIC :: Allocate
PUBLIC :: Deallocate
PUBLIC :: Initiate
PUBLIC :: RealMatrix
PUBLIC :: Eye
PUBLIC :: Convert
PUBLIC :: RealMatrix_Pointer
PUBLIC :: SYM
PUBLIC :: SkewSym
PUBLIC :: MakeDiagonalCopies
PUBLIC :: RANDOM_NUMBER
PUBLIC :: TestMatrix
PUBLIC :: ASSIGNMENT( = )


!----------------------------------------------------------------------------
!                                                 Shape@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Return shape of [[RealMatrix_]]
!
!# Introduction
!
! This function return shape of [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!	s = Shape( obj )
! ```

INTERFACE
MODULE PURE FUNCTION get_shape( obj ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans( 2 )
END FUNCTION get_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE get_shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                  Size@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Return size of [[RealMatrix_]]
!
!# Introduction
!
! This function return size of `RealMatrix_`
! - If `Dims` is present and equal to 1 then total number of rows (m)
! - If `Dims` is present and equal to 2 then total number of cols (n)
! - If `Dimes` is absent then Ans = m * n
!
!### Usage
!
!```fortran
!	trow = SIZE( obj, 1 )
! tcol = SIZE( obj, 2 )
! t = SIZE( obj )
!```

INTERFACE
MODULE PURE FUNCTION get_size( obj, Dims ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION get_size
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE get_size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                         TotalDimension@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the total dimension of an array
!
!# Introduction
!
! This function returns the total dimension (or rank) of an array,

INTERFACE
MODULE PURE FUNCTION get_tdimension( obj ) RESULT( Ans )
  CLASS( RealMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION get_tdimension
END INTERFACE

INTERFACE TotalDimension
  MODULE PROCEDURE get_tdimension
END INTERFACE TotalDimension

!----------------------------------------------------------------------------
!                                               SetTotalDimension@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This subroutine set the total dimension (rank) of an array
!
!# Introduction
!
! This subroutine sets the rank(total dimension) of an array

INTERFACE
MODULE PURE SUBROUTINE set_tdimension( obj, tDimension )
  CLASS( RealMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tDimension
END SUBROUTINE set_tdimension
END INTERFACE

INTERFACE setTotalDimension
  MODULE PROCEDURE set_tdimension
END INTERFACE setTotalDimension

!----------------------------------------------------------------------------
!                                           Allocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	This subroutine allocate memory for [[RealMatrix_]]
!
!
!### Usage
!
! ```fortran
!	call Allocate( obj, Dims )
! ```


INTERFACE
MODULE PURE SUBROUTINE Allocate_Data( obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Dims(2)
END SUBROUTINE Allocate_Data

END INTERFACE

INTERFACE Allocate
  MODULE PROCEDURE Allocate_Data
END INTERFACE Allocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Deallocate data in [[RealMatrix_]]
!
!# Introduction
!
! This routine deallocates data stored in obj
!
!### Usage
!
! ```fortran
!	call Deallocate( obj )
! ```

INTERFACE
MODULE PURE SUBROUTINE Deallocate_Data( obj )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Deallocate_Data
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE Deallocate_Data
END INTERFACE Deallocate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine initiate `obj` with shape `Dims`
!
!### Usage
!
!```fortran
!	call initiate( obj, [2,3] )
!```
! The above call will initiate a matrix of shape (2,3)

INTERFACE
MODULE PURE SUBROUTINE realmat_initiate1( obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
END SUBROUTINE realmat_initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realmat_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine initiate `obj` with shape `Dims`
!
!### Usage
!
!```fortran
!	call initiate( obj, [2,3] )
!```
! The above call will initiate a matrix of shape (2,3)

INTERFACE
MODULE PURE SUBROUTINE realmat_initiate2( obj, nrow, ncol )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nrow
  INTEGER( I4B ), INTENT( IN ) :: ncol
END SUBROUTINE realmat_initiate2
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realmat_initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	Initiate vector of [[realmatrix_]] with shape `Dims`
!
!### Usage
!
!```fortran
! type( realmatrix_ ) :: obj( 4 )
!	call initiate( obj, [2,3] )
!```
! The above call will initiate `obj` vector of matrices of shape (2,3)

INTERFACE
MODULE PURE SUBROUTINE realmat_initiate3( obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
END SUBROUTINE realmat_initiate3
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realmat_initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Initiate an instance of [[RealMatrix_]]
!
!# Introduction
!
! This subroutine initiate vector of [[realmatrix_]] with matrices of
! different shapes given in `Dims`
! - `Dims` has two columns; the first column denotes the number of rows, and
! second column denotes the number of columns in a matrix
! - irow of `Dims` corresponds to the shape of `obj(irow)`
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
MODULE PURE SUBROUTINE realmat_initiate4( obj, Dims )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Dims( :, : )
END SUBROUTINE realmat_initiate4
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realmat_initiate4
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: This subroutine performs `obj%l = Val`, i.e., initiate `obj` with `Val`
!
!### Usage
!
! ```fortran
!	call initiate( obj, val )
! ```

INTERFACE
MODULE PURE SUBROUTINE realmat_initiate5( obj, Val )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE realmat_initiate5
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realmat_initiate5
END INTERFACE Initiate

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realmat_initiate5
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Matrix@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Constructor function for [[RealMatrix_]]
!
!# Introduction
!
! This function returns an instance of [[realmatrix_]]
!
!### Usage
!
!```fortran
!	obj = RealMatrix( [2,2] )
!```

INTERFACE
  MODULE PURE FUNCTION Constructor1( Dims ) RESULT( obj )
    TYPE( RealMatrix_ ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
  END FUNCTION Constructor1
END INTERFACE

!> Generic function to construct [[realmatrix_]]
INTERFACE RealMatrix
  MODULE PROCEDURE Constructor1
END INTERFACE RealMatrix

!----------------------------------------------------------------------------
!                                                    Eye@ConstructorMethods
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

MODULE PURE FUNCTION realMat_eye1( m, DataType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m
  TYPE( RealMatrix_ ), INTENT( IN ) ::  DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION realMat_eye1
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE realMat_eye1
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                 Convert@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Rearrange the dofs in finite element matrix
!
!# Introduction
!
! This subroutine changes the storage pattern of a two-d matrix
!  - Usually element matrix in easifem are stored in `FMT_DOF`
!  - Global matrices/tanmat, however, are stored in `FMT_Nodes`
!  - This subroutine is, therefore, in settings or adding values in
! [[SparseMatrix_]].
!
! > This subroutine converts changes the storage format of dense matrix.
! Usually, elemental finite element matrix is stored in `DOF_FMT`, and global
! matrix/ tanmat, may be stored in `Nodes_FMT`.
!

INTERFACE
MODULE PURE SUBROUTINE realmat_convert_1(  From, To, Conversion, &
  & nns, tdof )
  TYPE( RealMatrix_ ), INTENT( IN ) :: From
    !! Matrix in one format
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: To
    !! Matrix in one format
  INTEGER( I4B ), INTENT( IN ) :: Conversion
    !! `Conversion` can be `NodesToDOF` or `DOFToNodes`
  INTEGER( I4B ), INTENT( IN ) :: nns, tdof
END SUBROUTINE realmat_convert_1
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE realmat_convert_1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                         Matrix_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor_1( Dims ) RESULT( obj )
  CLASS( RealMatrix_ ), POINTER :: obj
  INTEGER( I4B ), INTENT( IN ) :: Dims( 2 )
END FUNCTION Constructor_1
END INTERFACE

!> Generic function to get pointer to [[RealMatrix_]]
INTERFACE RealMatrix_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE RealMatrix_Pointer

!----------------------------------------------------------------------------
!                                                    Sym@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Return sym(obj) = 0.5*(obj + transpose( obj ) )
!
!# Introduction
!
! Return symmetric part of obj
!
!### Usage
!
! ```fortran
!	realMat = Sym( obj )
! ```

INTERFACE
MODULE PURE FUNCTION sym_obj( obj ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
    !! Real matrix
  TYPE( RealMatrix_ ) :: Ans
    !! Symmetric real matrix
END FUNCTION sym_obj
END INTERFACE

INTERFACE SYM
  MODULE PROCEDURE sym_obj
END INTERFACE SYM

!----------------------------------------------------------------------------
!                                                    Sym@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Return sym(obj) = 0.5*(obj + transpose( obj ) )
!
!# Introduction
!
! Return symmetric part of obj
!
!### Usage
!
!```fortran
!	realMat = Sym( obj )
!```

INTERFACE
MODULE PURE FUNCTION sym_array( obj ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: obj( :, : )
    !! Two dimensiona array
  REAL( DFP ) :: Ans( SIZE( obj, 1 ), SIZE( obj, 2 ) )
    !! Symmetric array
END FUNCTION sym_array
END INTERFACE

INTERFACE SYM
  MODULE PROCEDURE sym_array
END INTERFACE SYM

!----------------------------------------------------------------------------
!                                                SkewSym@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Return SkewSymmetric part of obj
!
!### Usage
!
!```fortran
!	realMat = SkewSym( obj )
!```

INTERFACE
MODULE PURE FUNCTION SkewSym_obj( obj ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
    !! Real matrix
  TYPE( RealMatrix_ ) :: Ans
    !! SkewSymmetric real matrix
END FUNCTION SkewSym_obj
END INTERFACE

INTERFACE SkewSym
  MODULE PROCEDURE SkewSym_obj
END INTERFACE SkewSym

!----------------------------------------------------------------------------
!                                                 SkewSym@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	Return SkewSym(obj) = 0.5*(obj + transpose( obj ) )
!
!# Introduction
!
! Return SkewSymmetric part of obj
!
!### Usage
!
!```fortran
!	realMat = SkewSym( obj )
!```

INTERFACE
MODULE PURE FUNCTION SkewSym_array( obj ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: obj( :, : )
    !! Two dimensiona array
  REAL( DFP ) :: Ans( SIZE( obj, 1 ), SIZE( obj, 2 ) )
    !! SkewSymmetric array
END FUNCTION SkewSym_array
END INTERFACE

INTERFACE SkewSym
  MODULE PROCEDURE SkewSym_array
END INTERFACE SkewSym

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Make diagonal copies of Matrix
!
!# Introduction
!
! This subroutine makes `ncopy` diagonal copies of `Mat` The size of `Mat` on
! return is nCopy * SIZE( Mat, 1 )
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

INTERFACE MakeDiagonalCopies
  MODULE PROCEDURE realmat_make_diag_copy1
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
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
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: To( :, : )
  INTEGER( I4B ), INTENT( IN ) :: nCopy
END SUBROUTINE realmat_make_diag_copy2
END INTERFACE

INTERFACE MakeDiagonalCopies
  MODULE PROCEDURE realmat_make_diag_copy2
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                       MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Make diagonal copies of [[realmatrix_]]
!
! This subroutine makes `ncopy` diagonal copies of `Mat`, The size of `Mat`
! on return is nCopy * SIZE( Mat, 1 )
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( Mat, nCopy )
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_make_diag_copy3( Mat, ncopy )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: Mat
  INTEGER( I4B ), INTENT( IN ) :: ncopy
END SUBROUTINE realmat_make_diag_copy3
END INTERFACE

INTERFACE MakeDiagonalCopies
  MODULE PROCEDURE realmat_make_diag_copy3
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Make diagonal copies of Matrix
!
!# Introduction
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
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: To
  INTEGER( I4B ), INTENT( IN ) :: nCopy
END SUBROUTINE realmat_make_diag_copy4
END INTERFACE

INTERFACE MakeDiagonalCopies
  MODULE PROCEDURE realmat_make_diag_copy4
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                          Random_number@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set a values in [[realmatrix_]] obj to random values
!
!# Introduction
!
! This subroutine set values in `obj%Val` to random
! - This subroutine calls `RANDOM_NUMBER()` function from Fortran
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE SUBROUTINE realmat_random_number( obj, m, n )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: m, n
END SUBROUTINE realmat_random_number
END INTERFACE

INTERFACE RANDOM_NUMBER
  MODULE PROCEDURE realmat_random_number
END INTERFACE RANDOM_NUMBER

!----------------------------------------------------------------------------
!                                             TestMatrix@ConstructorMethods
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
