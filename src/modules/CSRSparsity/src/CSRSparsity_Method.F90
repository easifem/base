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
!### Introduction
!
! This subroutine initiate the instance of [[CSRSparsity_]] object

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
! summary: This subroutine construct `CSRSparsity_` object from IA, JA

INTERFACE
MODULE SUBROUTINE csr_initiate2( obj, obj2 )
  TYPE( CSRSparsity_ ), INTENT( INOUT) :: obj
  TYPE( CSRSparsity_ ), INTENT( IN ) :: obj2
END SUBROUTINE csr_initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine construct `CSRSparsity_` object from IA, JA

INTERFACE
MODULE SUBROUTINE csr_initiate3( obj, IA, JA )
  TYPE( CSRSparsity_ ), INTENT( INOUT) :: obj
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
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This function returns the shape of the sparse matrix
!
!### Introduction
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
!                                                         getNNZ@Constructor
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
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine deallocates the data

INTERFACE
MODULE PURE SUBROUTINE csr_DeallocateData( obj )
  TYPE( CSRSparsity_ ), INTENT( INOUT) :: obj
END SUBROUTINE csr_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE csr_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                                Display@IO
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
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!### Introduction
!
! This subroutine sets the sparsity pattern of a given row
! - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.

INTERFACE
MODULE SUBROUTINE csr_setSparsity1( obj, Row, Col )
  TYPE( CSRSparsity_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Row
  INTEGER( I4B ), INTENT( IN ) :: Col( : )
END SUBROUTINE csr_setSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine sets the sparsity pattern of a given row

INTERFACE
MODULE SUBROUTINE csr_setSparsity2( obj, Row, Col )
  TYPE( CSRSparsity_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Row( : )
  TYPE( IntVector_ ), INTENT( IN ) :: Col( : )
END SUBROUTINE csr_setSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set sparsity pattern of `CSRSparsity_`
!
!### Introduction
! This subroutine set sparsity pattern of `CSRSparsity_`
! This will finally set the data into
! - `obj%IA(:)`,
! - `obj%JA(:)`
! in CSR format. This routine also set data inside `obj%ColSize(:)` and
! `obj%RowSize(:) `, and `obj%DiagIndx(:)`

INTERFACE
MODULE SUBROUTINE csr_setSparsity3( obj )
  TYPE( CSRSparsity_ ), INTENT( INOUT) :: obj
END SUBROUTINE csr_setSparsity3
END INTERFACE

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity1, csr_setSparsity2, csr_setSparsity3
END INTERFACE setSparsity

PUBLIC :: setSparsity

END MODULE CSRSparsity_Method