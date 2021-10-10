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
! - spaceCompo= [1]
! - timeCompo = [1]
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
!   & spaceCompo=[1], timeCompo=[1], storageFMT=NODES_FMT )
! !
! ! #CSRSparsity_/Initiate
! !
! CALL Initiate( obj, ncol=12, nrow=12, dof=dofobj )
! !
! ! #CSRSparsity_/Display
! !
! CALL Display( obj, "CSRSparsity : " )
! !
! ! #CSRSparsity_/DeallocateData
! !
! CALL DeallocateData( obj )
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
!   & spaceCompo=[3, 1], timeCompo=[1, 1], storageFMT=FMT_DOF )
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
! ! #CSRSparsity_/DeallocateData
! !
! CALL DeallocateData( obj )
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
!
!## Usage
!
!```fortran
!
!```

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
MODULE FUNCTION csr_constructor_1( nrow, ncol, dof ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: nrow
  INTEGER( I4B ), INTENT( IN ) :: ncol
  TYPE( DOF_ ), OPTIONAL, INTENT( IN ) :: dof
  TYPE( CSRSparsity_ ), POINTER :: ans
END FUNCTION csr_constructor_1
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
!                                                          Shape@Constructor
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
!                                                           Size@Constructor
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
!                                                      setSparsity@setMethod
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
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct 2021
! summary: This subroutine sets sparsity pattern for block `CSRSparsity_`

INTERFACE
MODULE SUBROUTINE csr_setSparsity3( obj, row, col, ivar, jvar )
  TYPE( CSRSparsity_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row
  INTEGER( I4B ), INTENT( IN ) :: col( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
END SUBROUTINE csr_setSparsity3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
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
  TYPE( CSRSparsity_ ), INTENT( INOUT) :: obj
END SUBROUTINE csr_setSparsity_final
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE setSparsity
  MODULE PROCEDURE csr_setSparsity1, csr_setSparsity2, csr_setSparsity3,  &
    & csr_setSparsity_final
END INTERFACE setSparsity

PUBLIC :: setSparsity

END MODULE CSRSparsity_Method