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
! date: 2026-03-19
! summary: This module contains methods for RealMatrix_ data type

MODULE RealMatrix_Method
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: RealMatrix_
USE BaseType, ONLY: RealMatrixPointer_
USE BaseType, ONLY: RealVector_
IMPLICIT NONE

PRIVATE
PUBLIC :: Shape
PUBLIC :: Size
PUBLIC :: totalDimension
PUBLIC :: SettotalDimension
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
PUBLIC :: MakeDiagonalCopies_
PUBLIC :: RANDOM_NUMBER
PUBLIC :: TestMatrix
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: GetPointer
PUBLIC :: Copy
PUBLIC :: Get
PUBLIC :: Get_
PUBLIC :: GetColumn_
PUBLIC :: Display
PUBLIC :: LinearSolver_CG
PUBLIC :: Matmul
PUBLIC :: Set
PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                  Shape@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Return Shape of RealMatrix_
!
!# Shape
!
! This function return Shape of RealMatrix_.
!
!### Usage
!
!```fortran
!s = Shape( obj )
!```

INTERFACE Shape
  MODULE PURE FUNCTION obj_Shape(obj) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                  Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Return size of RealMatrix_
!
!# Size
!
! This function return size of `RealMatrix_`
!
! - If `dims` is present and equal to 1 then total number of rows (m)
! - If `dims` is present and equal to 2 then total number of cols (n)
! - If `dims` is absent then ans = m * n
!
!### Usage
!
!```fortran
! trow = SIZE( obj, 1 )
! tcol = SIZE( obj, 2 )
! t = SIZE( obj )
!```

INTERFACE Size
  MODULE PURE FUNCTION obj_Size(obj, dims) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                         totalDimension@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns the total dimension of an array
!
!# TotalDimension
!
! This function returns the total dimension (or rank) of an array,

INTERFACE TotalDimension
  MODULE PURE FUNCTION obj_TotalDimension(obj) RESULT(ans)
    CLASS(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_TotalDimension
END INTERFACE TotalDimension

!----------------------------------------------------------------------------
!                                               SettotalDimension@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: This subroutine Set the total dimension (rank) of an array
!
!# SetTotalDimension
!
! This subroutine Sets the rank(total dimension) of an array

INTERFACE SetTotalDimension
  MODULE PURE SUBROUTINE obj_SetTotalDimension(obj, tDimension)
    CLASS(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tDimension
  END SUBROUTINE obj_SetTotalDimension
END INTERFACE SetTotalDimension

!----------------------------------------------------------------------------
!                                                Allocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: This subroutine allocate memory for RealMatrix_
!
!# Allocate
!
! This subroutine allocate memory for RealMatrix.
!
!### Usage
!
! ```fortran
! Call Allocate( obj, Dims )
! ```

INTERFACE ALLOCATE
  MODULE PURE SUBROUTINE obj_Allocate(obj, dims)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dims(2)
  END SUBROUTINE obj_Allocate
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Deallocate data in [[RealMatrix_]]
!
!# Deallocate
!
! This routine deallocates data stored in obj
!
!### Usage
!
!```fortran
!call Deallocate( obj )
!```

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine Initiate `obj` with Shape `Dims`
!
!# Initiate
!
! Initiate RealMatrix
!
!### Usage
!
!```fortran
!        call Initiate( obj, [2,3] )
!```
! The above call will Initiate a matrix of Shape (2,3)

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate1(obj, Dims)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Dims(2)
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: This subroutine Initiate `obj` with Shape `Dims`
!
!# Initiate
!
! Initiate real matrix using nrow and ncol.
!
!### Usage
!
!```fortran
! call Initiate( obj, [2,3] )
!```
!
! The above call will Initiate a matrix of Shape (2,3)

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2(obj, nrow, ncol)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nrow
    INTEGER(I4B), INTENT(IN) :: ncol
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Initiate vector of RealMatrix with Shape `Dims`
!
!# Initiate
!
! Initiate realmatrix using dims.
!
!### Usage
!
!```fortran
! type( realmatrix_ ) :: obj( 4 )
! call Initiate( obj, [2,3] )
!```
!
! The above call will Initiate `obj` vector of matrices of Shape (2,3)

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate3(obj, dims)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: dims(2)
  END SUBROUTINE obj_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Initiate an instance of RealMatrix_
!
!# Initiate
!
! This subroutine Initiate vector of [[realmatrix_]] with matrices of
! different Shapes given in `Dims`
!
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
  MODULE PURE SUBROUTINE obj_Initiate4(obj, dims)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: dims(:, :)
  END SUBROUTINE obj_Initiate4
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-03-19
! summary: This subroutine performs `obj%l = Val`
!
!# Initiate
!
! Initiate RealMatrix using a rank2 matrix.
!
!### Usage
!
!```fortran
! call Initiate( obj, val )
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate5(obj, val)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
  END SUBROUTINE obj_Initiate5
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Initiate5
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Matrix@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Constructor function for RealMatrix_
!
!# RealMatrix
!
! This function returns an instance of realmatrix_
!
!### Usage
!
!```fortran
!        obj = RealMatrix( [2,2] )
!```

INTERFACE RealMatrix
  MODULE PURE FUNCTION Constructor1(dims) RESULT(obj)
    TYPE(RealMatrix_) :: obj
    INTEGER(I4B), INTENT(IN) :: dims(2)
  END FUNCTION Constructor1
END INTERFACE RealMatrix

!----------------------------------------------------------------------------
!                                                    Eye@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary: Return identity matrix of type [[realmatrix_]]
!
!# Eye
!
! This function returns identity matrix of type [[realmatrix_]]
!
!### Usage
!
!```fortran
! obj = eye( 3, typeRealMatrix )
!```

INTERFACE
  MODULE PURE FUNCTION obj_Eye1(m, dataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: ans
  END FUNCTION obj_Eye1
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE obj_Eye1
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                 Convert@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Rearrange the dofs in finite element matrix
!
!# Convert
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
  MODULE PURE SUBROUTINE obj_Convert1(from, to, Conversion, &
                                      nns, tdof)
    TYPE(RealMatrix_), INTENT(IN) :: from
    !! Matrix in one format
    TYPE(RealMatrix_), INTENT(INOUT) :: to
    !! Matrix in one format
    INTEGER(I4B), INTENT(IN) :: Conversion
    !! `Conversion` can be `NodestoDOF` or `DOFToNodes`
    INTEGER(I4B), INTENT(IN) :: nns, tdof
  END SUBROUTINE obj_Convert1
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
! date: 2026-03-19
! summary: Return sym(obj) = 0.5*(obj + transpose( obj ) )
!
!# Sym
!
! Return symmetric part of obj
!
!### Usage
!
!```fortran
!realMat = Sym( obj )
!```

INTERFACE Sym
  MODULE PURE FUNCTION obj_Sym1(obj) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    !! Real matrix
    TYPE(RealMatrix_) :: ans
    !! Symmetric real matrix
  END FUNCTION obj_Sym1
END INTERFACE Sym

!----------------------------------------------------------------------------
!                                                    Sym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Return sym(obj) = 0.5*(obj + transpose( obj ) )
!
!# Sym
!
! Return symmetric part of obj
!
!### Usage
!
!```fortran
!        realMat = Sym( obj )
!```

INTERFACE Sym
  MODULE PURE FUNCTION obj_Sym2(obj) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj(:, :)
    !! Two dimensiona array
    REAL(DFP) :: ans(SIZE(obj, 1), SIZE(obj, 2))
    !! Symmetric array
  END FUNCTION obj_Sym2
END INTERFACE Sym

!----------------------------------------------------------------------------
!                                                SkewSym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Return SkewSymmetric part of obj
!
!# SkewSym
!
! Skew symmetric.
!
!### Usage
!
!```fortran
!realMat = SkewSym( obj )
!```

INTERFACE SkewSym
  MODULE PURE FUNCTION obj_SkewSym1(obj) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    !! Real matrix
    TYPE(RealMatrix_) :: ans
    !! SkewSymmetric real matrix
  END FUNCTION obj_SkewSym1
END INTERFACE SkewSym

!----------------------------------------------------------------------------
!                                                 SkewSym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Return SkewSym(obj) = 0.5*(obj + transpose( obj ) )
!
!# SkewSym
!
! Return SkewSymmetric part of obj
!
!### Usage
!
!```fortran
! realMat = SkewSym( obj )
!```

INTERFACE SkewSym
  MODULE PURE FUNCTION obj_SkewSym2(obj) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj(:, :)
    !! Two dimensiona array
    REAL(DFP) :: ans(SIZE(obj, 1), SIZE(obj, 2))
    !! SkewSymmetric array
  END FUNCTION obj_SkewSym2
END INTERFACE SkewSym

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Make diagonal copies of Matrix
!
!# MakeDiaginalCopies
!
! This subroutine makes `ncopy` diagonal copies of `Mat` The size of `Mat` on
! return is ncopy * SIZE( Mat, 1 )
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( Mat, ncopy )
!```

INTERFACE MakeDiagonalCopies
  MODULE PURE SUBROUTINE obj_MakeDiagonalCopies1(mat, ncopy)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: ncopy
  END SUBROUTINE obj_MakeDiagonalCopies1
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                     MakeDiagonalCopies_@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: make diagonal copies without allocation
!
!# MakeDiagonalCopies_
!
! Make diagonal copies without allocation.
!
INTERFACE MakeDiagonalCopies_
  MODULE PURE SUBROUTINE obj_MakeDiagonalCopies1_(mat, ncopy, nrow, ncol)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: ncopy
    INTEGER(i4b), INTENT(IN) :: nrow, ncol
    !! nrow and ncol are size of data which is used for making
    !! diagonal copies
  END SUBROUTINE obj_MakeDiagonalCopies1_
END INTERFACE MakeDiagonalCopies_

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Make diagonal copies of Matrix
!
!# MakeDiaginalCopies
!
! This subroutine makes `ncopy` diagonal copies of `Mat`
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( from = Mat, to = anotherMat, ncopy = nCopy )
!```

INTERFACE MakeDiagonalCopies
  MODULE PURE SUBROUTINE obj_MakeDiagonalCopies2(from, to, ncopy)
    REAL(DFP), INTENT(IN) :: from(:, :)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: to(:, :)
    INTEGER(I4B), INTENT(IN) :: ncopy
  END SUBROUTINE obj_MakeDiagonalCopies2
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Make diagonal copies
!
!# MakeDiaginalCopies_
!
! Make diagonal copies.
!
INTERFACE MakeDiagonalCopies_
  MODULE PURE SUBROUTINE obj_MakeDiagonalCopies2_(from, to, ncopy)
    REAL(DFP), INTENT(IN) :: from(:, :)
    REAL(DFP), INTENT(INOUT) :: to(:, :)
    INTEGER(I4B), INTENT(IN) :: ncopy
  END SUBROUTINE obj_MakeDiagonalCopies2_
END INTERFACE MakeDiagonalCopies_

!----------------------------------------------------------------------------
!                                       MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Make diagonal copies of RealMatrix
!
!# MakeDiagonalCopies
!
! This subroutine makes `ncopy` diagonal copies of `Mat`, The size of `Mat`
! on return is ncopy * SIZE( Mat, 1 )
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( Mat, ncopy )
!```

INTERFACE MakeDiagonalCopies
  MODULE PURE SUBROUTINE obj_MakeDiagonalCopies3(Mat, ncopy)
    TYPE(RealMatrix_), INTENT(INOUT) :: Mat
    INTEGER(I4B), INTENT(IN) :: ncopy
  END SUBROUTINE obj_MakeDiagonalCopies3
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                                          MakeDiagonalCopies
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!                                      MakeDiagonalCopies@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Make diagonal copies of Matrix
!
!# MakeDiaginalCopies
!
! This subroutine makes `ncopy` diagonal copies of `Mat`
!
!### Usage
!
!```fortran
! call MakeDiagonalCopies( from = Mat, to = anotherMat, ncopy = nCopy )
!```

INTERFACE MakeDiagonalCopies
  MODULE PURE SUBROUTINE obj_MakeDiagonalCopies4(from, to, ncopy)
    TYPE(RealMatrix_), INTENT(IN) :: from
    TYPE(RealMatrix_), INTENT(INOUT) :: to
    INTEGER(I4B), INTENT(IN) :: ncopy
  END SUBROUTINE obj_MakeDiagonalCopies4
END INTERFACE MakeDiagonalCopies

!----------------------------------------------------------------------------
!                                          Random_number@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set a values in RealMatrix obj to random values
!
!# Random_Number
!
! This subroutine Set values in `obj%Val` to random
! - This subroutine calls `RANDOM_NUMBER()` function from Fortran

INTERFACE Random_Number
  MODULE SUBROUTINE obj_Random_Number1(obj, m, n)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: m, n
  END SUBROUTINE obj_Random_Number1
END INTERFACE Random_Number

!----------------------------------------------------------------------------
!                                             TestMatrix@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: This function returns the example matrix

INTERFACE
  MODULE FUNCTION TestMatrix(matNo) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: matNo
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION TestMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns the values of RealMatrix_ obj in 2D array
!
!# Get
!
! This function returns the value stored in `obj%l` in a 2D fortran array
!
!### Usage
!
!```fortran
! val = Get(obj, 1.0_DFP)
!```

INTERFACE Get
  MODULE PURE FUNCTION obj_Get1(obj, dataType) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_Get1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns the values of RealMatrix_ obj in 2D array
!
!# Get_
!
! This function returns the value stored in `obj%l` in a 2D fortran array
!
!### Usage
!
!```fortran
! val = Get(obj, 1.0_DFP)
!```

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_1(obj, ans, nrow, ncol)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_Get_1
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Get
!
! This function returns the value stored in `obj%l` in a 2D fortran array
!
!### Usage
!
!```fortran
! val = Get( obj, 1.0_dfp )
!```

INTERFACE Get
  MODULE PURE FUNCTION obj_Get1b(obj) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_Get1b
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns the values of RealMatrix_ obj in 2D array
!
!# Get
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
  MODULE PURE FUNCTION obj_Get2(obj, rindx, cindx, dataType) &
    RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: rindx(:), cindx(:)
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_Get2
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns the values of RealMatrix_ obj in 2D array
!
!# Get_
!
! This function returns a section of `obj%l` in a 2D fortran array. This
! is equivalent to `Val = obj%l(RIndx, CIndx)`
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_2(obj, rindx, cindx, ans, nrow, ncol)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: rindx(:), cindx(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_Get_2
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Get
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
  MODULE PURE FUNCTION obj_Get3(obj, iStart, iEnd, Stride, &
                                dataType) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, Stride
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_Get3
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Get_
!
! This function returns a section of `obj%l` in a 2D fortran array. This
! is equivalent to `Val = obj%l(is:ie:s, is:ie:s)`
!

INTERFACE Get_
  MODULE PURE SUBROUTINE obj_Get_3(obj, iStart, iEnd, stride, ans, nrow, &
                                   ncol)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, stride
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_Get_3
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-20
! summary: Returns [[RealMatrix_]] obj from [[realmatrix_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get4(obj, dataType) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: ans
  END FUNCTION obj_Get4
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]
!
!# Get
!
! This function is essentially Copy method `Ans=obj(RIndx, CIndx)`

INTERFACE Get
  MODULE PURE FUNCTION obj_Get5(obj, rindx, cindx, dataType) &
    RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: rindx(:), cindx(:)
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: ans
  END FUNCTION obj_Get5
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns RealMatrix_ obj from a section of RealMatrix
!
!# Get
!
! This function is essentially Copy method `Ans=obj(is:ie, is:ie)`

INTERFACE Get
  MODULE PURE FUNCTION obj_Get6(obj, iStart, iEnd, &
                                stride, dataType) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iStart, iEnd, stride
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: ans
  END FUNCTION obj_Get6
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns values in 2D fortran array from [[realmatrix_]]
!
!# Get
!
! This function combines all [[realmatrix_]] value of `obj` and
! returns a 2D fortrn array

INTERFACE Get
  MODULE PURE FUNCTION obj_Get7(obj, dataType) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj(:, :)
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_Get7
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Returns RealMatrix object from a 2D array of RealMatrix
!
!# Get
!
! This function combines all [[realmatrix_]] value of `obj` and
! returns a [[realmatrix_]] object

INTERFACE Get
  MODULE PURE FUNCTION obj_Get8(obj, dataType) RESULT(ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj(:, :)
    TYPE(RealMatrix_), INTENT(IN) :: dataType
    TYPE(RealMatrix_) :: ans
  END FUNCTION obj_Get8
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                            Copy@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Copy from RealMatrix to 2D fortran array
!
!# Copy
!
! This subroutine Copy the contents of [[realmatrix_]] object into a 2D
! fortran array

INTERFACE Copy
  MODULE PURE SUBROUTINE obj_Copy1(from, to)
    TYPE(RealMatrix_), INTENT(IN) :: from
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: to(:, :)
  END SUBROUTINE obj_Copy1
END INTERFACE Copy

INTERFACE Convert
  MODULE PROCEDURE obj_Copy1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                             Copy@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Copy from RealMatrix to another RealMatrix
!
!# Copy
!
! This subroutine Copy the contents of RealMatrix object to another
! RealMatrix object

INTERFACE Copy
  MODULE PURE SUBROUTINE obj_Copy2(from, to)
    TYPE(RealMatrix_), INTENT(IN) :: from
    TYPE(RealMatrix_), INTENT(INOUT) :: to
  END SUBROUTINE obj_Copy2
END INTERFACE Copy

INTERFACE Convert
  MODULE PROCEDURE obj_Copy2
END INTERFACE Convert

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Copy2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Copy@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Copy from 2D fortran array to RealMatrix
!
!# Copy
!
! This subroutine Copy the contents of a 2D fortran array to RealMatrix
! object

INTERFACE Copy
  MODULE PURE SUBROUTINE obj_Copy3(from, to)
    REAL(DFP), INTENT(IN) :: from(:, :)
    TYPE(RealMatrix_), INTENT(INOUT) :: to
  END SUBROUTINE obj_Copy3
END INTERFACE Copy

INTERFACE Convert
  MODULE PROCEDURE obj_Copy3
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                      GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Get pointer to the values stored inside [[realmatrix_]]
!
!# GetPointer
!
! This function returns the pointer to the values stored inside the
! [[realmatrix_]]

INTERFACE GetPointer
  MODULE FUNCTION obj_GetPointer(obj, dataType) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN), TARGET :: obj
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), POINTER :: Ans(:, :)
  END FUNCTION obj_GetPointer
END INTERFACE GetPointer

!----------------------------------------------------------------------------
!                                                       GetColumn_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Get a column of real matrix
!
!# GetColumn_
!
! Get a column of real matrix.
!

INTERFACE GetColumn_
  MODULE PURE SUBROUTINE obj_GetColumn_1(obj, col, ans, tsize)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: col
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetColumn_1
END INTERFACE GetColumn_

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Display content of realmatrix_
!
!# Display
!
! Display the content of RealMatrix.
!

INTERFACE Display
  MODULE SUBROUTINE obj_Display1(obj, msg, unitno)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-03-19
! summary: Display content of realmatrix_
!
!# Display
!
! Display the content of real matrix.

INTERFACE Display
  MODULE SUBROUTINE obj_Display2(obj, msg, unitno)
    TYPE(RealMatrix_), INTENT(IN) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display2
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                 CG@IterativeSolverMethods
!----------------------------------------------------------------------------

INTERFACE LinearSolver_CG
  MODULE PURE SUBROUTINE obj_LinearSolver_CG1( &
    mat, rhs, sol, maxIter, rtol, atol, convergenceIn, &
    relativeToRHS, restartAfter)
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativetoRHS
  !! FALSE <--- relative converfence is checked with respect to ||res||
  !! TRUE Convergence is checked with respect to ||rhs||
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: restartAfter
  !! recompute residual by using b-Ax
  END SUBROUTINE obj_LinearSolver_CG1
END INTERFACE LinearSolver_CG

!----------------------------------------------------------------------------
!                                                 CG@IterativeSolverMethods
!----------------------------------------------------------------------------

INTERFACE LinearSolver_CG
  MODULE PURE SUBROUTINE obj_LinearSolver_CG2( &
    mat, rhs, sol, w, maxIter, rtol, atol, convergenceIn, &
    relativeToRHS, restartAfter)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    !! Symmetric matrix, size should be tsize x tsize
    REAL(DFP), INTENT(IN) :: rhs(:)
    !! right hand side, size should be tsize
    REAL(DFP), INTENT(INOUT) :: sol(:)
    !! solution, size should be tsize
    REAL(DFP), INTENT(INOUT) :: w(:, :)
    !! working array, number of rows tsize
    !! number of cols are 3
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: relativetoRHS
  !! FALSE <--- relative converfence is checked with respect to ||res||
  !! TRUE Convergence is checked with respect to ||rhs||
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: restartAfter
  !! recompute residual by using b-Ax
  END SUBROUTINE obj_LinearSolver_CG2
END INTERFACE LinearSolver_CG

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE Matmul
  MODULE PURE FUNCTION obj_MatMul1(obj1, obj2) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj1, obj2
    TYPE(RealMatrix_) :: Ans
  END FUNCTION obj_MatMul1
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE Matmul
  MODULE PURE FUNCTION obj_MatMul2(obj, Vec) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Vec(:)
    REAL(DFP), ALLOCATABLE :: Ans(:)
  END FUNCTION obj_MatMul2
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE Matmul
  MODULE PURE FUNCTION obj_MatMul3(obj, Vec) RESULT(Ans)
    TYPE(RealMatrix_), INTENT(IN) :: obj
    TYPE(RealVector_), INTENT(IN) :: Vec
    TYPE(RealVector_) :: Ans
  END FUNCTION obj_MatMul3
END INTERFACE Matmul

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Add values in RealMatrix
!
!# Set
!
! This subroutine Set `obj%val` to `val`

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1(obj, val)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set values in [[realmatrix_]]
!
!# Set
!
! This subroutine Set values in `obj%l` `obj%l( i, j ) = Val`

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set2(obj, Val, Row, Col)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
    INTEGER(I4B), INTENT(IN) :: Col, Row
  END SUBROUTINE obj_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set values in RealMatrix
!
!# Set
!
! This subroutine Set values in `obj%l`
!

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set3(obj, Val, Row, Col)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    INTEGER(I4B), INTENT(IN) :: Col(:), Row(:)
  END SUBROUTINE obj_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set values in RealMatrix
!
!# Set
!
! This subroutine Set values in `obj%l`
!
! - If `ExtraOption=MATRIX_DIAGONAL` then diagonal values are Set; and `Indx`
! denotes diagonal number with `0` being the main diagonal
! - If `Extraoption=MATRIX_ROW` then row values are Set; `Indx` then denotes
! row number
! - If `Extraoption=MATRIX_COLUMN` then col values are Set; `Indx` then
! denotes col number

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set4(obj, val, indx, extraOption)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B), INTENT(IN) :: extraOption
  END SUBROUTINE obj_Set4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set values in RealMatrix
!
!# Set
!
! This subroutine Set values in `obj%l`
! - If `ExtraOption=MATRIX_DIAGONAL` then diagonal values are Set; and `Indx`
! denotes the diagonal number with `0` being the main diagonal
! - If `Extraoption=ROW` then row values are Set; `Indx` then denotes row
! number
! - If `Extraoption=COLUMN` then col values are Set; `Indx` then denotes col
! number

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set5(obj, val, indx, extraOption)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(IN) :: indx(:), extraOption
  END SUBROUTINE obj_Set5
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Add values in RealMatrix
!
!# Set
!
! This subroutine Set `obj%l` to `Val`

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set6(obj, val)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val
  END SUBROUTINE obj_Set6
END INTERFACE Set

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Set6
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                             Add@AddMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Add contribution in values of [[realmatrix_]]
!
!# Add
!
! This subroutine Adds contribution in values of `obj%l`. This subroutine
! performs following task:
!
! $$obj = obj <op> scale * val $$
!
! Here `op` can be `+, -, *, /`.
!

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add1(obj, val, scale, op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    REAL(DFP), INTENT(IN) :: scale
    !! Scaling for `Val`
    CHARACTER(1), INTENT(IN) :: op
    !! operator symbol; `+, -, *, /`
  END SUBROUTINE obj_Add1
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@AddMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-20
! summary: Add contribution in values of [[Realmatrix_]]
!
!# Add
!
! This subroutine Adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add2(obj, val, row, col, scale, op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: row
    INTEGER(I4B), INTENT(IN) :: col
    CHARACTER(1), INTENT(IN) :: op
  END SUBROUTINE obj_Add2
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@AddMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-20
! summary: Add contribution in values of [[realmatrix_]]
!
!# Add
!
! This subroutine Adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add3(obj, val, row, col, scale, op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: row(:)
    INTEGER(I4B), INTENT(IN) :: col(:)
    CHARACTER(1), INTENT(IN) :: op
  END SUBROUTINE obj_Add3
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@AddMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-20
! summary: Add contribution in values of Realmatrix_
!
!# Add
!
! This subroutine Adds contribution in values of `obj%l`
!
! ```fortran
! obj%l = obj%v%*Op* scale * val
! ```

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add4(obj, val, indx, extraOption, scale, &
                                  op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:), scale
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B), INTENT(IN) :: extraOption
    CHARACTER(1), INTENT(IN) :: op
  END SUBROUTINE obj_Add4
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@AddMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-20
! summary: Add contribution in values of RealMatrix
!
!# Add
!
! This subroutine Adds contribution in values of `obj%l`
!
!```fortran
! obj%l = obj%v%*Op* scale * val
!```

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add5(obj, val, indx, extraoption, &
                                  scale, op)
    TYPE(RealMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :), scale
    INTEGER(I4B), INTENT(IN) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: extraOption
    CHARACTER(1), INTENT(IN) :: op
  END SUBROUTINE obj_Add5
END INTERFACE Add

END MODULE RealMatrix_Method

