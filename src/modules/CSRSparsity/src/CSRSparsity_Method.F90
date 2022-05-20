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
! summary: 	This module contains methods for [[CSRSparsity_]]

MODULE CSRSparsity_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine constructs the CSR sparsity object
!
!# Introduction
!
! This subroutine initiate the instance of [[CSRSparsity_]] object
!
! - ncol is the number of columns
! - nrow is the number of rows
! - dof is the degrees of freedom object, if it is present then it used
! to initiate [[DOF_:dof]].
!
!@note
! 	If dof object is not present, then this routine initiates
! [[CSRSparsity_:dof]] internally with following options.
!
! - tNodes = [nrow]
! - names= ["K"]
! - spacecompo= [1]
! - timecompo = [1]
! - storageFMT = FMT_NODES
!@endnote
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! !
! ! [[CSRSparsity_]], [[DOF_]]
! !
! TYPE( CSRSparsity_ ) :: obj
! TYPE( DOF_ ) :: dofobj
! INTEGER( I4B ) :: i
! !
! ! #DOF_/Initiate
! !
! CALL Initiate( obj=dofobj, tNodes=[12], names=['K'], &
!   & spacecompo=[1], timecompo=[1], storageFMT=NODES_FMT )
! !
! ! #CSRSparsity_/Initiate
! !
! CALL Initiate( obj, ncol=12, nrow=12, dof=dofobj )
! !
! ! #CSRSparsity_/Display
! !
! CALL Display( obj, "CSRSparsity : " )
! !
! ! #CSRSparsity_/Deallocate
! !
! CALL Deallocate( obj )
! END PROGRAM main
!```
!
! another example for multi-physics applications
!
!## Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! !
! ! [[CSRSparsity_]], [[DOF_]]
! !
! TYPE( CSRSparsity_ ) :: obj
! TYPE( DOF_ ) :: dofobj
! INTEGER( I4B ) :: i
! !
! ! #DOF_/Initiate
! !
! CALL Initiate( obj=dofobj, tNodes=[20, 10], names=['V', 'P'], &
!   & spacecompo=[3, 1], timecompo=[1, 1], storageFMT=FMT_DOF )
! !
! ! #CSRSparsity_/Initiate
! !
! CALL Initiate( obj, ncol=(.tnodes. dofobj), nrow=(.tNodes. dofobj),  &
!   & dof=dofobj )
! !
! ! #CSRSparsity_/Display
! !
! CALL Display( obj, "CSRSparsity : " )
! !
! ! #CSRSparsity_/Deallocate
! !
! CALL Deallocate( obj )
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE csr_initiate1( obj, ncol, nrow, dof )
    TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: ncol, nrow
    TYPE( DOF_ ), OPTIONAL, INTENT( IN ) ::  dof
  END SUBROUTINE csr_initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine construct `CSRSparsity_` object from copying
!
!# Introduction
!
! This subroutine copies `obj2` into `obj`, and initiates the latter one.
! This routine is used to define the assignment operator.

INTERFACE
  MODULE SUBROUTINE csr_initiate2( obj, obj2 )
    TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
    TYPE( CSRSparsity_ ), INTENT( IN ) :: obj2
  END SUBROUTINE csr_initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine constructs `CSRSparsity_` object from IA, JA
!
!# Introduction
!
! - This routine constructs [[CSRSparsity_]] instance by using the
! indices `IA` and `JA`
! - This routine is helpful in reading data from files.
! - This routine calls [[CSRSparsity_Method:csr_initiate1]] method
! without `dof` argument. So this type of initiation does not contain
! useful information about the degree of freedoms.
!
!
!## Usage
!
!```fortran
!
!```


INTERFACE
  MODULE SUBROUTINE csr_initiate3( obj, IA, JA )
    TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: IA( : ), JA( : )
  END SUBROUTINE csr_initiate3
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csr_initiate1, csr_initiate2, csr_initiate3
END INTERFACE Initiate

PUBLIC :: Initiate

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE csr_initiate2
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                            CSRSparsity@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an instance of [[CSRSparsity_]]
!
!# Introduction
!
! This function returns an instance of [[CSRSparsity_]]

INTERFACE
  MODULE FUNCTION csr_constructor1( nrow, ncol, dof ) RESULT( Ans )
    INTEGER( I4B ), INTENT( IN ) :: nrow
    INTEGER( I4B ), INTENT( IN ) :: ncol
    TYPE( DOF_ ), OPTIONAL, INTENT( IN ) :: dof
    TYPE( CSRSparsity_ ) :: ans
  END FUNCTION csr_constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                            CSRSparsity@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an instance of [[CSRSparsity_]]
!
!# Introduction
!
! This function returns an instance of [[CSRSparsity_]]

INTERFACE
  MODULE FUNCTION csr_constructor2( IA, JA ) RESULT( Ans )
    INTEGER( I4B ), INTENT( IN ) :: IA(:)
    INTEGER( I4B ), INTENT( IN ) :: JA(:)
    TYPE( CSRSparsity_ ) :: ans
  END FUNCTION csr_constructor2
END INTERFACE

INTERFACE CSRSpasity
  MODULE PROCEDURE csr_constructor1, csr_constructor2
END INTERFACE CSRSpasity

PUBLIC :: CSRSpasity

!----------------------------------------------------------------------------
!                                       CSRSparsityPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an instance of [[CSRSparsity_]]
!
!# Introduction
!
! This function returns an instance of [[CSRSparsity_]]

INTERFACE
MODULE FUNCTION csr_constructor_1( nrow, ncol, dof ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: nrow
  INTEGER( I4B ), INTENT( IN ) :: ncol
  TYPE( DOF_ ), OPTIONAL, INTENT( IN ) :: dof
  TYPE( CSRSparsity_ ), POINTER :: ans
END FUNCTION csr_constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                      CSRSparsityPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an instance of [[CSRSparsity_]]
!
!# Introduction
!
! This function returns an instance of [[CSRSparsity_]]

INTERFACE
MODULE FUNCTION csr_constructor_2( IA, JA ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: IA(:)
  INTEGER( I4B ), INTENT( IN ) :: JA(:)
  TYPE( CSRSparsity_ ), POINTER :: ans
END FUNCTION csr_constructor_2
END INTERFACE

INTERFACE CSRSpasityPointer
  MODULE PROCEDURE csr_constructor_1, csr_constructor_2
END INTERFACE CSRSpasityPointer

PUBLIC :: CSRSpasityPointer

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine deallocates the data

INTERFACE
MODULE PURE SUBROUTINE csr_Deallocate( obj )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
END SUBROUTINE csr_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE csr_Deallocate
END INTERFACE Deallocate

PUBLIC :: Deallocate

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine display the content of sparsity

INTERFACE
MODULE SUBROUTINE csr_Display( obj, Msg, UnitNo )
  TYPE( CSRSparsity_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE csr_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE csr_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                          Shape@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This function returns the shape of the sparse matrix
!
!# Introduction
!
! This function returns the shape of sparse matrix

INTERFACE
MODULE PURE FUNCTION csr_shape( obj ) RESULT( Ans )
  TYPE( CSRSparsity_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans( 2 )
END FUNCTION csr_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE csr_shape
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
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
MODULE PURE FUNCTION csr_size( obj, Dims ) RESULT( Ans )
  TYPE( CSRSparsity_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION csr_size
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE csr_size
END INTERFACE Size

PUBLIC :: Size

!----------------------------------------------------------------------------
!                                                          getNNZ@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Return the total number of non zero entry

INTERFACE
MODULE PURE FUNCTION csr_getNNZ( obj ) RESULT( Ans )
  TYPE( CSRSparsity_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION csr_getNNZ
END INTERFACE

INTERFACE getNNZ
  MODULE PROCEDURE csr_getNNZ
END INTERFACE getNNZ

PUBLIC :: getNNZ

!----------------------------------------------------------------------------
!                                                         getDiagonal@Unary
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the diagonal of sparse matrix
!
!# Introduction
!
! This subroutine returns the diagonal entries of sparse matrix. This
! Routine calls the Saad's sparse library.
!
! `offset`: containing the `offset` of the wanted diagonal. The diagonal
! extracted is the one corresponding to the entries `a(i,j)` with `j-i =
! offset`. Therefore, `offset = 0` means the main diagonal
!
! `diag` : real array of length `nrow` containing the wanted diagonal. `diag`
! contains the diagonal (`a(i,j),j-i = offset`) as defined above.
!
! `idiag` = integer array. It contains the poisitions of diagonal in the
! original arrays `A`. If `idiag(i)=0` then it means that there was no
! diagonal found in row=i.

INTERFACE
MODULE SUBROUTINE csr_getDiagonal1( obj, A, diag, idiag, offset )
  TYPE( CSRSparsity_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: A( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: diag( : )
    !! Diagonal entries
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: idiag( : )
    !! Position of diagonal entries in `A(:)`
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset
    !! offset of the wanted diagonal
END SUBROUTINE csr_getDiagonal1
END INTERFACE

INTERFACE getDiagonal
  MODULE PROCEDURE csr_getDiagonal1
END INTERFACE getDiagonal

PUBLIC :: getDiagonal

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
! This routine is similar to [[CSRSparsity_Method:csr_getDiagonal1]].
! However, this routine does not return the position of diagonal in `A`

INTERFACE
MODULE SUBROUTINE csr_getDiagonal2( obj, A, diag, offset )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: A( : )
    !! Sparse matrix values
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: diag( : )
    !! Diagonal entries
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset
    !! offset of diagonal
END SUBROUTINE csr_getDiagonal2
END INTERFACE

INTERFACE getDiagonal
  MODULE PROCEDURE csr_getDiagonal2
END INTERFACE getDiagonal

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
! - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.

INTERFACE
MODULE SUBROUTINE csr_setSparsity1( obj, Row, Col )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Row
  !! row number
  INTEGER( I4B ), INTENT( IN ) :: Col( : )
  !! column number
END SUBROUTINE csr_setSparsity1
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity1
END INTERFACE setSparsity

PUBLIC :: setSparsity

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine sets the sparsity pattern of several rows
!
!# Introduction
! This routine is similar to [[CSRSparsity_Method:csr_setSparsity1]].
! However, in this routine several rows can be given.

INTERFACE
MODULE SUBROUTINE csr_setSparsity2( obj, Row, Col )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Row( : )
    !! row number
  TYPE( IntVector_ ), INTENT( IN ) :: Col( : )
    !! column number
END SUBROUTINE csr_setSparsity2
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity2
END INTERFACE setSparsity

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct 2021
! summary: This subroutine sets sparsity pattern for block `CSRSparsity_`

INTERFACE
MODULE SUBROUTINE csr_setSparsity3( obj, row, col, ivar, jvar )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row
    !! row number
  INTEGER( I4B ), INTENT( IN ) :: col( : )
    !! sparsity of row, column numbers
  INTEGER( I4B ), INTENT( IN ) :: ivar
    !! block address (row index)
  INTEGER( I4B ), INTENT( IN ) :: jvar
    !! block address (col index)
END SUBROUTINE csr_setSparsity3
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity3
END INTERFACE setSparsity

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This subroutine sets the sparsity pattern of a given row
!
!# Introduction
! This routine is similar to the [[CSRSparsity_Method:csr_setSparsity3]],
! however, in this routine we can specify several rows and their
! column indices.

INTERFACE
MODULE SUBROUTINE csr_setSparsity4( obj, Row, Col, iVar, jVar )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Row( : )
    !! several row numbers
  TYPE( IntVector_ ), INTENT( IN ) :: Col( : )
    !! column index for each row number
  INTEGER( I4B ), INTENT( IN ) :: iVar
    !! block address (row index)
  INTEGER( I4B ), INTENT( IN ) :: jVar
    !! block address (col index)
END SUBROUTINE csr_setSparsity4
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity4
END INTERFACE setSparsity

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern by using the graph.
! graph( i, j ) is either 0 or 1, if zero then there is not connection
! between row-i and row-j

INTERFACE
MODULE SUBROUTINE csr_setSparsity5( obj, graph )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: graph( :, : )
  !! graph of sparsity
  !! If graph( i, j ) .EQ. 0, then i and j are not connected
  !! else they are connected.
END SUBROUTINE csr_setSparsity5
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity5
END INTERFACE setSparsity

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern by using the graph.
! graph( i, j ) is either FALSE or TRUE, if FALSE then there is not connection
! between row-i and row-j

INTERFACE
MODULE SUBROUTINE csr_setSparsity6( obj, graph )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: graph( :, : )
  !! graph of sparsity
  !! If graph( i, j ) .EQ. FALSE, then i and j are not connected
  !! else they are connected.
END SUBROUTINE csr_setSparsity6
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity6
END INTERFACE setSparsity

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set sparsity pattern of `CSRSparsity_`
!
!# Introduction
! This subroutine set sparsity pattern of `CSRSparsity_`
! This will finally set the data into
! - `obj%IA(:)`,
! - `obj%JA(:)`
! in CSR format. This routine also set data inside `obj%ColSize(:)` and
! `obj%RowSize(:) `, and `obj%DiagIndx(:)`

INTERFACE
MODULE SUBROUTINE csr_setSparsity_final( obj )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
END SUBROUTINE csr_setSparsity_final
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity_final
END INTERFACE setSparsity


END MODULE CSRSparsity_Method