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
! You should have received a Copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: This module contains methods for [[RealMatrix_]] data type

MODULE RealMatrix_Method
USE GlobalData
USE BaSetype
IMPLICIT NONE
PRIVATE

PUBLIC :: Shape
PUBLIC :: Size
PUBLIC :: TotalDimension
PUBLIC :: SetTotalDimension
PUBLIC :: ALLOCATE
PUBLIC :: DEALLOCATE
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
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: GetPointer
PUBLIC :: Copy
PUBLIC :: Get
PUBLIC :: Display
PUBLIC :: LinearSolver_CG
PUBLIC :: Matmul
PUBLIC :: Set
PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                 Shape@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Return Shape of [[RealMatrix_]]
!
!# Introduction
!
! This function return Shape of [[RealMatrix_]]
!
!### Usage
!
! ```fortran
!        s = Shape( obj )
! ```

INTERFACE Shape
  MODULE PURE FUNCTION Get_Shape(obj) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans(2)
  END FUNCTION Get_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                  Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
!        trow = SIZE( obj, 1 )
! tcol = SIZE( obj, 2 )
! t = SIZE( obj )
!```

INTERFACE Size
  MODULE PURE FUNCTION Get_size(obj, Dims) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: Dims
    INTEGER(I4B) :: Ans
  END FUNCTION Get_size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                         TotalDimension@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         Returns the total dimension of an array
!
!# Introduction
!
! This function returns the total dimension (or rank) of an array,

INTERFACE TotalDimension
  MODULE PURE FUNCTION Get_tdimension(obj) RESULT(Ans)
    CLASS(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans
  END FUNCTION Get_tdimension
END INTERFACE TotalDimension

!----------------------------------------------------------------------------
!                                               SetTotalDimension@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine Set the total dimension (rank) of an array
!
!# Introduction
!
! This subroutine Sets the rank(total dimension) of an array

INTERFACE SetTotalDimension
  MODULE PURE SUBROUTINE Set_tdimension(obj, tDimension)
    CLASS(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tDimension
  END SUBROUTINE Set_tdimension
END INTERFACE SetTotalDimension

!----------------------------------------------------------------------------
!                                           Allocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary:         This subroutine allocate memory for [[RealMatrix_]]
!
!
!### Usage
!
! ```fortran
!        call Allocate( obj, Dims )
! ```

INTERFACE ALLOCATE
  MODULE PURE SUBROUTINE Allocate_Data(obj, Dims)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Dims(2)
  END SUBROUTINE Allocate_Data
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
!        call Deallocate( obj )
! ```

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE Deallocate_Data(obj)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE Deallocate_Data
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine Initiate `obj` with Shape `Dims`
!
!### Usage
!
!```fortran
!        call Initiate( obj, [2,3] )
!```
! The above call will Initiate a matrix of Shape (2,3)

INTERFACE Initiate
  MODULE PURE SUBROUTINE realmat_Initiate1(obj, Dims)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Dims(2)
  END SUBROUTINE realmat_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine Initiate `obj` with Shape `Dims`
!
!### Usage
!
!```fortran
!        call Initiate( obj, [2,3] )
!```
! The above call will Initiate a matrix of Shape (2,3)

INTERFACE Initiate
  MODULE PURE SUBROUTINE realmat_Initiate2(obj, nrow, ncol)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nrow
    INTEGER(I4B), INTENT(IN) :: ncol
  END SUBROUTINE realmat_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary:         Initiate vector of [[realmatrix_]] with Shape `Dims`
!
!### Usage
!
!```fortran
! type( realmatrix_ ) :: obj( 4 )
!        call Initiate( obj, [2,3] )
!```
! The above call will Initiate `obj` vector of matrices of Shape (2,3)

INTERFACE Initiate
  MODULE PURE SUBROUTINE realmat_Initiate3(obj, Dims)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: Dims(2)
  END SUBROUTINE realmat_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Initiate an instance of [[RealMatrix_]]
!
!# Introduction
!
! This subroutine Initiate vector of [[realmatrix_]] with matrices of
! different Shapes given in `Dims`
! - `Dims` has two columns; the first column denotes the number of rows, and
! second column denotes the number of columns in a matrix
! - irow of `Dims` corresponds to the Shape of `obj(irow)`
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
!        call Initiate( obj, Dims )
!```
!
! - The above call will Initiate a obj( 1 ) with Shape (2,2)
! - The above call will Initiate a obj( 2 ) with Shape (4,4)
! - The above call will Initiate a obj( 3 ) with Shape (4,4)

INTERFACE Initiate
  MODULE PURE SUBROUTINE realmat_Initiate4(obj, Dims)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: Dims(:, :)
  END SUBROUTINE realmat_Initiate4
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: This subroutine performs `obj%l = Val`
!
!### Usage
!
! ```fortran
!        call Initiate( obj, val )
! ```

INTERFACE Initiate
  MODULE PURE SUBROUTINE realmat_Initiate5(obj, Val)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
  END SUBROUTINE realmat_Initiate5
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE realmat_Initiate5
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Matrix@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
!        obj = RealMatrix( [2,2] )
!```

INTERFACE RealMatrix
  MODULE PURE FUNCTION Constructor1(Dims) RESULT(obj)
    TYPE(RealMatrix_) :: obj
    INTEGER(I4B), INTENT(IN) :: Dims(2)
  END FUNCTION Constructor1
END INTERFACE RealMatrix

!----------------------------------------------------------------------------
!                                                    Eye@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary: Return identity matrix of type [[realmatrix_]]
!
!# Introduction
!
! This function returns identity matrix of type [[realmatrix_]]
!
!### Usage
!
! ```fortran
!        obj = eye( 3, typeRealMatrix )
! ```

INTERFACE

  MODULE PURE FUNCTION realMat_eye1(m, dataType) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: m
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: Ans
  END FUNCTION realMat_eye1
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE realMat_eye1
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                 Convert@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Rearrange the dofs in finite element matrix
!
!# Introduction
!
! This subroutine changes the storage pattern of a two-d matrix
!  - Usually element matrix in easifem are stored in `FMT_DOF`
!  - Global matrices/tanmat, however, are stored in `FMT_Nodes`
!  - This subroutine is, therefore, in Settings or Adding values in
! [[SparseMatrix_]].
!
! > This subroutine converts changes the storage format of dense matrix.
! Usually, elemental finite element matrix is stored in `DOF_FMT`, and global
! matrix/ tanmat, may be stored in `Nodes_FMT`.
!

INTERFACE Convert
  MODULE PURE SUBROUTINE realmat_convert_1(From, To, Conversion, &
    & nns, tdof)
    TYPE(RealMatrix_), INTENT(IN) :: From
    !! Matrix in one format
    TYPE(RealMatrix_), INTENT(INOUT) :: To
    !! Matrix in one format
    INTEGER(I4B), INTENT(IN) :: Conversion
    !! `Conversion` can be `NodesToDOF` or `DOFToNodes`
    INTEGER(I4B), INTENT(IN) :: nns, tdof
  END SUBROUTINE realmat_convert_1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                         Matrix_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE RealMatrix_Pointer
  MODULE PURE FUNCTION Constructor_1(Dims) RESULT(obj)
    CLASS(RealMatrix_), POINTER :: obj
    INTEGER(I4B), INTENT(IN) :: Dims(2)
  END FUNCTION Constructor_1
END INTERFACE RealMatrix_Pointer

!----------------------------------------------------------------------------
!                                                    Sym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Return sym(obj) = 0.5*(obj + transpose( obj ) )
!
!# Introduction
!
! Return symmetric part of obj
!
!### Usage
!
! ```fortran
!        realMat = Sym( obj )
! ```

INTERFACE Sym
  MODULE PURE FUNCTION sym_obj(obj) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    !! Real matrix
    TYPE(RealMatrix_) :: Ans
    !! Symmetric real matrix
  END FUNCTION sym_obj
END INTERFACE Sym

!----------------------------------------------------------------------------
!                                                    Sym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary:          Return sym(obj) = 0.5*(obj + transpose( obj ) )
!
!# Introduction
!
! Return symmetric part of obj
!
!### Usage
!
!```fortran
!        realMat = Sym( obj )
!```

INTERFACE Sym
  MODULE PURE FUNCTION sym_array(obj) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: obj(:, :)
    !! Two dimensiona array
    REAL(DFP) :: Ans(SIZE(obj, 1), SIZE(obj, 2))
    !! Symmetric array
  END FUNCTION sym_array
END INTERFACE Sym

!----------------------------------------------------------------------------
!                                                SkewSym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Return SkewSymmetric part of obj
!
!### Usage
!
!```fortran
!        realMat = SkewSym( obj )
!```

INTERFACE SkewSym
  MODULE PURE FUNCTION SkewSym_obj(obj) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    !! Real matrix
    TYPE(RealMatrix_) :: Ans
    !! SkewSymmetric real matrix
  END FUNCTION SkewSym_obj
END INTERFACE SkewSym

!----------------------------------------------------------------------------
!                                                 SkewSym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary:         Return SkewSym(obj) = 0.5*(obj + transpose( obj ) )
!
!# Introduction
!
! Return SkewSymmetric part of obj
!
!### Usage
!
!```fortran
!        realMat = SkewSym( obj )
!```

INTERFACE SkewSym
  MODULE PURE FUNCTION SkewSym_array(obj) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: obj(:, :)
    !! Two dimensiona array
    REAL(DFP) :: Ans(SIZE(obj, 1), SIZE(obj, 2))
    !! SkewSymmetric array
  END FUNCTION SkewSym_array
END INTERFACE SkewSym

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Make diagonal copies of Matrix
!
!# Introduction
!
! This subroutine makes `nCopy` diagonal copies of `Mat` The size of `Mat` on
! return is nCopy * SIZE( Mat, 1 )
!
!### Usage
!
!```fortran
!        call MakeDiagonalCopies( Mat, nCopy )
!```

INTERFACE MakeDiagonalCopies
  MODULE PURE SUBROUTINE realmat_make_diag_Copy1(Mat, nCopy)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: nCopy
  END SUBROUTINE realmat_make_diag_Copy1
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Make diagonal copies of Matrix
!
! This subroutine makes `nCopy` diagonal copies of `Mat`
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( From = Mat, To = anotherMat, nCopy = nCopy )
!```

INTERFACE MakeDiagonalCopies
  MODULE PURE SUBROUTINE realmat_make_diag_Copy2(From, To, nCopy)
    REAL(DFP), INTENT(IN) :: From(:, :)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: To(:, :)
    INTEGER(I4B), INTENT(IN) :: nCopy
  END SUBROUTINE realmat_make_diag_Copy2
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                       MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Make diagonal copies of [[realmatrix_]]
!
! This subroutine makes `nCopy` diagonal copies of `Mat`, The size of `Mat`
! on return is nCopy * SIZE( Mat, 1 )
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( Mat, nCopy )
!```

INTERFACE MakeDiagonalCopies
  MODULE PURE SUBROUTINE realmat_make_diag_Copy3(Mat, nCopy)
    TYPE(RealMatrix_), INTENT(INOUT) :: Mat
    INTEGER(I4B), INTENT(IN) :: nCopy
  END SUBROUTINE realmat_make_diag_Copy3
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Make diagonal copies of Matrix
!
!# Introduction
!
! This subroutine makes `nCopy` diagonal copies of `Mat`
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( From = Mat, To = anotherMat, nCopy = nCopy )
!```

INTERFACE MakeDiagonalCopies
  MODULE PURE SUBROUTINE realmat_make_diag_Copy4(From, To, nCopy)
    TYPE(RealMatrix_), INTENT(IN) :: From
    TYPE(RealMatrix_), INTENT(INOUT) :: To
    INTEGER(I4B), INTENT(IN) :: nCopy
  END SUBROUTINE realmat_make_diag_Copy4
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                          Random_number@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set a values in [[realmatrix_]] obj to random values
!
!# Introduction
!
! This subroutine Set values in `obj%Val` to random
! - This subroutine calls `RANDOM_NUMBER()` function from Fortran

INTERFACE Random_number
  MODULE SUBROUTINE realmat_random_number(obj, m, n)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: m, n
  END SUBROUTINE realmat_random_number
END INTERFACE Random_number

!----------------------------------------------------------------------------
!                                             TestMatrix@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         7 March 2021
! summary: This function returns the example matrix

INTERFACE
  MODULE FUNCTION TestMatrix(matNo) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: matNo
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION TestMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                             Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Introduction
!
! This function returns the value stored in `obj%l` in a 2D fortran array
!
!### Usage
!
!```fortran
!        Val = Get( obj, 1.0_dfp )
!```

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get1(obj, dataType) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION realmat_Get1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                             Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Introduction
!
! This function returns the value stored in `obj%l` in a 2D fortran array
!
!### Usage
!
!```fortran
!        Val = Get( obj, 1.0_dfp )
!```

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get1b(obj) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION realmat_Get1b
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary:          Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Introduction
!
! This function returns a section of `obj%l` in a 2D fortran array. This
! is equivalent to `Val = obj%l(RIndx, CIndx)`
!
!### Usage
!
!```fortran
! integer( i4b ) :: r( 2 ), c( 2 )
! type( RealMatrix_ ) :: obj
! call Initiate( obj, [4,4] )
! call random_number( obj ); r=[1,2]; c=[2,3]
! Val = Get( obj, R, C, 1.0_dfp )
!```
!
! The above call will return `obj%Val[1:2, 2:3]`

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get2(obj, RIndx, CIndx, dataType) &
    & RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: RIndx(:), CIndx(:)
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION realmat_Get2
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Introduction
!
! This function returns a section of `obj%l` in a 2D fortran array. This
! is equivalent to `Val = obj%l(is:ie:s, is:ie:s)`
!
!### Usage
!
!```fortran
! integer( i4b ) :: r( 2 ), c( 2 )
! type( RealMatrix_ ) :: obj
! call Initiate( obj, [4,4] )
! call random_number( obj )
! Val = Get( obj, 1, 2, 1, 1.0_dfp )
!```
!
! The above call will return `obj%Val[1:2:1, 1:2:1]`

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get3(obj, iStart, iEnd, Stride, &
    & dataType) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION realmat_Get3
END INTERFACE Get

!----------------------------------------------------------------------------
!                                               Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Returns [[RealMatrix_]] obj from [[realmatrix_]]

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get4(obj, dataType) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: Ans
  END FUNCTION realmat_Get4
END INTERFACE Get

!----------------------------------------------------------------------------
!                                               Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]
!
!# Introduction
!
! This function is essentially Copy method `Ans=obj(RIndx, CIndx)`

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get5(obj, RIndx, CIndx, dataType) &
    & RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: RIndx(:), CIndx(:)
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: Ans
  END FUNCTION realmat_Get5
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]
!
!# Introduction
!         This function is essentially Copy method `Ans=obj(is:ie, is:ie)`

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get6(obj, iStart, iEnd, &
    & Stride, dataType) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: Ans
  END FUNCTION realmat_Get6
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Returns values in 2D fortran array from [[realmatrix_]]
!
!# Introduction
!
! This function combines all [[realmatrix_]] value of `obj` and
! returns a 2D fortrn array

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get7(obj, dataType) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj(:, :)
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION realmat_Get7
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Returns [[realmatrix_]] object from a 2D array of [[realmatrix_]]
!
!# Introduction
!
! This function combines all [[realmatrix_]] value of `obj` and
! returns a [[realmatrix_]] object

INTERFACE Get
  MODULE PURE FUNCTION realmat_Get8(obj, dataType) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj(:, :)
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: Ans
  END FUNCTION realmat_Get8
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                      Copy@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Copy from [[realmatrix_]] to 2D fortran array
!
!# Introduction
!
! This subroutine Copy the contents of [[realmatrix_]] object into a 2D
! fortran array

INTERFACE Copy
  MODULE PURE SUBROUTINE realmat_Copy1(From, To)
    TYPE(RealMatrix_), INTENT(IN) :: From
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: To(:, :)
  END SUBROUTINE realmat_Copy1
END INTERFACE Copy

INTERFACE Convert
  MODULE PROCEDURE realmat_Copy1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                      Copy@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Copy from RealMatrix to another RealMatrix
!
!# Introduction
!
! This subroutine Copy the contents of RealMatrix object to another
! RealMatrix object

INTERFACE Copy
  MODULE PURE SUBROUTINE realmat_Copy2(From, To)
    TYPE(RealMatrix_), INTENT(IN) :: From
    TYPE(RealMatrix_), INTENT(INOUT) :: To
  END SUBROUTINE realmat_Copy2
END INTERFACE Copy

INTERFACE Convert
  MODULE PROCEDURE realmat_Copy2
END INTERFACE Convert

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE realmat_Copy2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Copy@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  6 March 2021
! summary: Copy from 2D fortran array to RealMatrix
!
!# Introduction
!
! This subroutine Copy the contents of a 2D fortran array to RealMatrix
! object

INTERFACE Copy
  MODULE PURE SUBROUTINE realmat_Copy3(From, To)
    REAL(DFP), INTENT(IN) :: From(:, :)
    TYPE(RealMatrix_), INTENT(INOUT) :: To
  END SUBROUTINE realmat_Copy3
END INTERFACE Copy

INTERFACE Convert
  MODULE PROCEDURE realmat_Copy3
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                             ArrayPointer@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Get pointer to the values stored inside [[realmatrix_]]
!
!# Introduction
!
! This function returns the pointer to the values stored inside the
! [[realmatrix_]]

INTERFACE GetPointer
  MODULE FUNCTION realmat_GetPointer(obj, dataType) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN), TARGET :: obj
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), POINTER :: Ans(:, :)
  END FUNCTION realmat_GetPointer
END INTERFACE GetPointer

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Display content of [[realmatrix_]]

INTERFACE Display
  MODULE SUBROUTINE realmat_Display1(obj, Msg, UnitNo)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE realmat_Display1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Display content of [[realmatrix_]]

INTERFACE Display
  MODULE SUBROUTINE realmat_Display2(obj, Msg, UnitNo)
    TYPE(RealMatrix_), INTENT(IN) :: obj(:)
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE realmat_Display2
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                 CG@IterativeSolverMethods
!----------------------------------------------------------------------------

INTERFACE LinearSolver_CG
  MODULE PURE SUBROUTINE realmat_CG_1(mat, rhs, sol, maxIter, &
      & rtol, atol, convergenceIn, relativeToRHS, &
      & restartAfter)
    REAL(DFP), INTENT(IN) :: mat(:, :)
  !! Symmetric matrix
    REAL(DFP), INTENT(IN) :: rhs(:)
    REAL(DFP), INTENT(INOUT) :: sol(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
  !! maximum number of iteration
  !! if maxIter < 0 then maxIter=infinite
  !! if maxIter is absent then min( size(mat,1), 10 )
    REAL(DFP), OPTIONAL, INTENT(IN) :: rtol
  !! relative tolerance, default is 1.0E-6
    REAL(DFP), OPTIONAL, INTENT(IN) :: atol
  !! absolute tolerance, default is 0.0
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: convergenceIn
  !! convergenceInRes <-- default
  !! convergenceInSol
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativeToRHS
  !! FALSE <--- relative converfence is checked with respect to ||res||
  !! TRUE Convergence is checked with respect to ||rhs||
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: restartAfter
  !! recompute residual by using b-Ax
  END SUBROUTINE realmat_CG_1
END INTERFACE LinearSolver_CG

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE Matmul
  MODULE PURE FUNCTION realmat_MatMul1(obj1, obj2) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj1, obj2
    TYPE(RealMatrix_) :: Ans
  END FUNCTION realmat_MatMul1
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE Matmul
  MODULE PURE FUNCTION realmat_MatMul2(obj, Vec) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Vec(:)
    REAL(DFP), ALLOCATABLE :: Ans(:)
  END FUNCTION realmat_MatMul2
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE Matmul
  MODULE PURE FUNCTION realmat_MatMul3(obj, Vec) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    TYPE(RealVector_), INTENT(IN) :: Vec
    TYPE(RealVector_) :: Ans
  END FUNCTION realmat_MatMul3
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                        SetValues@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Add values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine Set `obj%val` to `Val`

INTERFACE Set
  MODULE PURE SUBROUTINE realmat_Set_1(obj, Val)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
  END SUBROUTINE realmat_Set_1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                        SetValues@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set values in [[realmatrix_]]

!> author: Dr. Vikas Sharma
!
! This subroutine Set values in `obj%l`
! `obj%l( i, j ) = Val`

INTERFACE Set
  MODULE PURE SUBROUTINE realmat_Set_2(obj, Val, Row, Col)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
    INTEGER(I4B), INTENT(IN) :: Col, Row
  END SUBROUTINE realmat_Set_2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                        SetValues@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary:          Set values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine Set values in `obj%l`
!

INTERFACE Set
  MODULE PURE SUBROUTINE realmat_Set_3(obj, Val, Row, Col)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    INTEGER(I4B), INTENT(IN) :: Col(:), Row(:)
  END SUBROUTINE realmat_Set_3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                        SetValues@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Set values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine Set values in `obj%l`
! - If `ExtraOption=MATRIX_DIAGONAL` then diagonal values are Set; and `Indx`
! denotes diagonal number with `0` being the main diagonal
! - If `Extraoption=MATRIX_ROW` then row values are Set; `Indx` then denotes
! row number
! - If `Extraoption=MATRIX_COLUMN` then col values are Set; `Indx` then
! denotes col number

INTERFACE Set
  MODULE PURE SUBROUTINE realmat_Set_4(obj, Val, Indx, ExtraOption)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: Indx
    INTEGER(I4B), INTENT(IN) :: ExtraOption
  END SUBROUTINE realmat_Set_4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                        SetValues@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Set values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine Set values in `obj%l`
! - If `ExtraOption=MATRIX_DIAGONAL` then diagonal values are Set; and `Indx`
! denotes the diagonal number with `0` being the main diagonal
! - If `Extraoption=ROW` then row values are Set; `Indx` then denotes row
! number
! - If `Extraoption=COLUMN` then col values are Set; `Indx` then denotes col
! number

INTERFACE Set
  MODULE PURE SUBROUTINE realmat_Set_5(obj, Val, Indx, ExtraOption)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    INTEGER(I4B), INTENT(IN) :: Indx(:), ExtraOption
  END SUBROUTINE realmat_Set_5
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                        SetValues@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Add values in [[realmatrix_]]
!
!# Introduction
!
! This subroutine Set `obj%l` to `Val`

INTERFACE Set
  MODULE PURE SUBROUTINE realmat_Set_6(obj, Val)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
  END SUBROUTINE realmat_Set_6
END INTERFACE Set

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE realmat_Set_6
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                  AddContribution@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary:          Add contribution in values of [[realmatrix_]]
!
!# Introduction
!
! This subroutine Adds contribution in values of `obj%l`. This subroutine
! performs following task:
!
! $$obj = obj <op> scale * val $$
!
! Here `op` can be `+, -, *, /`.
!
!@todo
! Use Blas routines or OpenMP support?
!@endtodo

INTERFACE Add
  MODULE PURE SUBROUTINE realmat_Add_1(obj, Val, Scale, Op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    REAL(DFP), INTENT(IN) :: Scale
    !! Scaling for `Val`
    CHARACTER(1), INTENT(IN) :: Op
    !! operator symbol; `+, -, *, /`
  END SUBROUTINE realmat_Add_1
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                  AddContribution@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Add contribution in values of [[Realmatrix_]]
!
!# Introduction
!
! This subroutine Adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE Add
  MODULE PURE SUBROUTINE realmat_Add_2(obj, Val, Row, Col, Scale, Op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
    REAL(DFP), INTENT(IN) :: Scale
    INTEGER(I4B), INTENT(IN) :: Row
    INTEGER(I4B), INTENT(IN) :: Col
    CHARACTER(1), INTENT(IN) :: Op
  END SUBROUTINE realmat_Add_2
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                  AddContribution@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Add contribution in values of [[realmatrix_]]
!
!# Introduction
!
! This subroutine Adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE Add
  MODULE PURE SUBROUTINE realmat_Add_3(obj, Val, Row, Col, Scale, Op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    REAL(DFP), INTENT(IN) :: Scale
    INTEGER(I4B), INTENT(IN) :: Row(:)
    INTEGER(I4B), INTENT(IN) :: Col(:)
    CHARACTER(1), INTENT(IN) :: Op
  END SUBROUTINE realmat_Add_3
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                  AddContribution@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Add contribution in values of [[Realmatrix_]]
!
!# Introduction
!
! This subroutine Adds contribution in values of `obj%l`
!
! ```fortran
! obj%l = obj%v%*Op* scale * val
! ```

INTERFACE Add
  MODULE PURE SUBROUTINE realmat_Add_4(obj, Val, Indx, ExtraOption, Scale, Op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:), Scale
    INTEGER(I4B), INTENT(IN) :: Indx
    INTEGER(I4B), INTENT(IN) :: ExtraOption
    CHARACTER(1), INTENT(IN) :: Op
  END SUBROUTINE realmat_Add_4
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                  AddContribution@SetValues
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         6 March 2021
! summary: Add contribution in values of [[realmatrix_]]
!
!# Introduction
!
! This subroutine Adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE Add
  MODULE PURE SUBROUTINE realmat_Add_5(obj, Val, Indx, ExtraOption, &
    & Scale, Op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :), Scale
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(I4B), INTENT(IN) :: ExtraOption
    CHARACTER(1), INTENT(IN) :: Op
  END SUBROUTINE realmat_Add_5
END INTERFACE Add

END MODULE RealMatrix_Method
