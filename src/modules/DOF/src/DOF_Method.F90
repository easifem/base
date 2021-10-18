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
! date: 	23 Feb 2021
! summary: 	This module contains methods of [[DOF_]] object
!
!# Introduction
!This module contains methods for derived type called [[DOF_]]

MODULE DOF_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: This subroutine initiate [[DOF_]] object
!
!# Introduction
!
! This subroutine initiate [[DOF_]] object
!
!- If the size of all physical variables are equal then set
! tNodes = [tNodes] otherwise we need to provide size of each dof
!- For a scalar physical variable such as pressure and temperature,
! `spaceCompo` is set to -1.
!- For a time independent physical variable `TimeCompo` is set to 1.
!- The size of `Names`, `SpaceCompo`, `TimeCompo` should be same
!
!@note
! 	$\matbf{v}$ is a physical variable, however,
!   its component $v_1, v_2, v_3$ all are degrees of freedom.
!@endnote
!
!### Usage
!
!```fortran
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! !
! ! [[DOF_]]
! !
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! CALL Display( obj, "obj : " )
! ! #DOF/Display
! CALL Display( .tNames. obj, '.tNames. obj : ' )
! CALL Display( .tNodes. obj, '.tNodes. obj : ' )
! CALL Display( .tDOF. obj, '.tDOF. obj : ' )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE SUBROUTINE dof_initiate1( obj, tNodes, Names, SpaceCompo, &
  & TimeCompo, StorageFMT )
  CLASS( DOF_ ), INTENT( INOUT ) :: obj
    !! degree of freedom object
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : )
    !! number of nodes for each physical variable
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
    !! Names of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: SpaceCompo( : )
    !! Space component of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: TimeCompo( : )
    !! Time component of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
    !! Storage format `FMT_DOF`, `FMT_Nodes`
END SUBROUTINE dof_initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: Initiate a fortran vector using [[DOF_]] object
!
!# Introduction
!
! This subroutine initiates a fortran vector (rank-1 fortran array ) of
! real using the information stored inside [[DOF_]] object. This subroutine
! gets the size of array from the [[DOF_]] object and then reallocates
! `Val` and set its all values to zero.
!
!### Usage
!
!```fortran
! [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   REAL( DFP ), ALLOCATABLE :: val( : )
!   ! #DOF/Initiate
!   CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!     & timeCompo=[1], storageFMT = FMT_DOF )
!   ! #DOF/Initiate
!   CALL Initiate( Val=val, obj=obj )
!   CALL Display( obj, "CALL Initiate( Val=val, obj=obj ) : " )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE SUBROUTINE dof_initiate2( Val, obj )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
    !! This vector will be initiated by using obj
  CLASS( DOF_ ), INTENT( IN ) :: obj
    !! DOF object
END SUBROUTINE dof_initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: Initiate [[RealVector_]] using [[dof_]] object
!
!# Introduction
!
! This subroutine initiate [[RealVector_]] using the information stored inside
! [[dof_]] object. It gets the information of total size of [[RealVector_]]
! from [[DOF_]] and call [[RealVector_Method:Initiate]] routine.
! All values of [[RealVector_]] is set to zero.
!
!## Usage
!
!```fortran
! [[DOF_]], [[RealVector_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   TYPE(RealVector_) :: val
!   ! #DOF/Initiate
!   CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!     & timeCompo=[1], storageFMT = FMT_DOF )
!   ! #DOF/Initiate
!   CALL Initiate( Val=val, obj=obj )
!   CALL Display( Val, "CALL Initiate( Val=val, obj=obj ) : " )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE SUBROUTINE dof_initiate3( Val, obj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Val
  CLASS( DOF_ ), INTENT( IN ) :: obj
END SUBROUTINE dof_initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Initiate a vector of [[realvector_]] from [[dof_]] object
!
!# Introduction
!
! This subroutine initiates a vector of [[realvector_]] object.
! The size of `Val` will be total number of degrees of freedom inside
! the [[DOF_]] object. Therefore, each `Val( idof )` denotes the
! nodal vector of correrponding to a degree of freedom number `idof`
!
!
!## Usage
!
!```fortran
! ! [[DOF_]], [[RealVector_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   TYPE(RealVector_), ALLOCATABLE :: val( : )
!   ! #DOF/Initiate
!   CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!     & timeCompo=[1], storageFMT = FMT_DOF )
!   ! #DOF/Initiate
!   CALL Initiate( Val=val, obj=obj )
!   CALL Display( Val, "CALL Initiate( Val=val, obj=obj ) : " )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE SUBROUTINE dof_initiate4( Val, obj )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
END SUBROUTINE dof_initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Initiate two fortran vectors using [[dof_]] object
!
!# Introduction
!
! This subroutine can initiate two fortran vectors (rank-1 fortran arrays)
! using  the information stored inside the [[DOF_]] object

INTERFACE
MODULE PURE SUBROUTINE dof_initiate5( Val1, Val2, obj )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val1( : ), Val2( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
END SUBROUTINE dof_initiate5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: Initiate an instance of [[DOF_]] by copying other object
!
!# Introduction
!
! This routine copy obj2 into obj1. It also define an assignment operator
!
!
!## Usage
!
!```fortran
! ! [[DOF_]], [[RealVector_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj, anotherObj
!   ! #DOF/Initiate
!   CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!     & timeCompo=[1], storageFMT = FMT_DOF )
!   ! #DOF/Initiate
!   anotherObj=obj
!   CALL Display( anotherObj, "anotherObj=obj : " )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
!   CALL DeallocateData( anotherObj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE SUBROUTINE dof_initiate6( obj1, obj2 )
  CLASS( DOF_ ), INTENT( INOUT ) :: obj1
  CLASS( DOF_ ), INTENT( IN ) :: obj2
END SUBROUTINE dof_initiate6
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE dof_initiate6
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> Generic interface to initiate Fortran vectors or [[RealVector_]] from
! [[DOF_]] object
INTERFACE Initiate
  MODULE PROCEDURE &
    & dof_initiate1, &
    & dof_initiate2, &
    & dof_initiate3, &
    & dof_initiate4, &
    & dof_initiate5, &
    & dof_initiate6
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                            DOF@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 oct 2021
! summary: 	 Constructor for [[dof_]] object
!
!# Introduction
!
! This function return instance of [[DOF_]]
! This function calls [[DOF_Method:DOF_Initiate1]] method
! for more see [[dof_]]
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! obj = DOF( tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/Display
! CALL Display( obj, "DOF() : " )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_Constructor1( tNodes, Names, SpaceCompo, TimeCompo, &
  & StorageFMT ) RESULT( obj )
  TYPE(DOF_) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : ), SpaceCompo( : ), &
    & TimeCompo( : ), StorageFMT
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
END FUNCTION dof_Constructor1
END INTERFACE

!> Generic function to construct [[dof_]] object
INTERFACE DOF
  MODULE PROCEDURE dof_Constructor1
END INTERFACE DOF

PUBLIC :: DOF

!----------------------------------------------------------------------------
!                                                     DOF_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns pointer to newly created [[dof_]] object
!
!# Introduction
!
! This function returns the pointer to instance of [[dof_]] object
! for more see [[dof_]]
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ), POINTER :: obj
! ! #DOF/Initiate
! obj => DOF_POINTER( tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/Display
! CALL Display( obj, "DOF() : " )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE FUNCTION dof_Constructor_1( tNodes, Names, SpaceCompo, TimeCompo, &
  & StorageFMT ) RESULT( obj )
  CLASS(DOF_), POINTER :: obj
    !! [[dof_]] object
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : )
    !! total number of nodes for each dof
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
    !! name of each dof
  INTEGER( I4B ), INTENT( IN ) :: SpaceCompo( : )
    !! space components for each dof
  INTEGER( I4B ), INTENT( IN ) :: TimeCompo( : )
    !! time component for each dof
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
    !! storage format for dof
END FUNCTION dof_Constructor_1
END INTERFACE

!> Generic interface to get pointer to instance of [[dof_]] object
INTERFACE DOF_Pointer
  MODULE PROCEDURE dof_Constructor_1
END INTERFACE DOF_Pointer

PUBLIC :: DOF_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: Oct 10, 2021
! summary: Deallocate data in [[dof_]]
!
!# Introduction
!
! This subroutine deallocates the data in [[DOF_]] object

INTERFACE
MODULE PURE SUBROUTINE dof_DeallocateData( obj )
  CLASS(DOF_), INTENT( INOUT ) :: obj
END SUBROUTINE dof_DeallocateData
END INTERFACE

!> Generic interface to deallocate data in [[dof_]]
INTERFACE DeallocateData
  MODULE PROCEDURE dof_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 June 2021
! summary: 	Display content of [[dof_]]

INTERFACE
MODULE SUBROUTINE dof_Display1( obj, msg, UnitNo )
  CLASS(DOF_), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE dof_Display1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 June 2021
! summary: Display content of fortran vec with [[DOF_]] object info
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! REAL( DFP ), ALLOCATABLE :: val( : )
! ! main
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/Initiate
! CALL Initiate( Val=val, obj=obj )
! val(1:10) = 1; val(11:20)=2; val(21:)=3
! CALL Display( Val, obj, "CALL Initiate( Val=val, obj=obj ) : " )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE SUBROUTINE dof_Display2( Vec, obj, msg, unitno )
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE dof_Display2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 June 2021
! summary: Display content of fortran vec with [[DOF_]] object info

INTERFACE
MODULE SUBROUTINE dof_Display3( Vec, obj, msg, unitno )
  CLASS( RealVector_ ), INTENT( IN ) :: Vec
  CLASS( DOF_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE dof_Display3
END INTERFACE

!> Generic subroutine to displacy content of [[dof_]]
INTERFACE Display
  MODULE PROCEDURE dof_Display1, dof_Display2, dof_Display3
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                  DOFStartIndex@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: returns obj%map( ivar, 5 )

INTERFACE
MODULE PURE FUNCTION dof_DOFStartIndex( obj, ivar ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ) :: ans
END FUNCTION dof_DOFStartIndex
END INTERFACE

INTERFACE OPERATOR(.DOFStartIndex.)
  MODULE PROCEDURE dof_DOFStartIndex
END INTERFACE OPERATOR(.DOFStartIndex.)

PUBLIC :: OPERATOR(.DOFStartIndex.)

!----------------------------------------------------------------------------
!                                                  DOFEndIndex@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: returns obj%map( ivar+1, 5 ) - 1

INTERFACE
MODULE PURE FUNCTION dof_DOFEndIndex( obj, ivar ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ) :: ans
END FUNCTION dof_DOFEndIndex
END INTERFACE

INTERFACE OPERATOR(.DOFEndIndex.)
  MODULE PROCEDURE dof_DOFEndIndex
END INTERFACE OPERATOR(.DOFEndIndex.)

PUBLIC :: OPERATOR(.DOFEndIndex.)

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the total length of the vector
!
!# Introduction
!
! Returns the total length of the vector.
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/.tNodes.
! CALL Display(     .tNodes. obj, '.tNodes. obj [30] : ' )
! CALL Display( obj .tNodes. 1, 'obj .tNodes. 1 [10] : ' )
! CALL Display( obj .tNodes. 2, 'obj .tNodes. 2 [10] : ' )
! CALL Display( obj .tNodes. 3, 'obj .tNodes. 3 [10] : ' )
! CALL Display( obj .tNodes. [1,2,3], 'obj .tNodes. [1,2,3] [30] : ' )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION dof_tNodes1( obj ) RESULT( Ans )
    CLASS( DOF_ ), INTENT( IN ) :: obj
    INTEGER( I4B ) :: Ans
  END FUNCTION dof_tNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of nodes
!
!# Introduction
!
! This function returns the total number of nodes for a given degree of
! freedom number
! idof should be lesser than the total degree of freedom
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/.tNodes.
! CALL Display(     .tNodes. obj, '.tNodes. obj [30] : ' )
! CALL Display( obj .tNodes. 1, 'obj .tNodes. 1 [10] : ' )
! CALL Display( obj .tNodes. 2, 'obj .tNodes. 2 [10] : ' )
! CALL Display( obj .tNodes. 3, 'obj .tNodes. 3 [10] : ' )
! CALL Display( obj .tNodes. [1,2,3], 'obj .tNodes. [1,2,3] [30] : ' )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_tNodes2( obj, idof ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of nodes
!
!# Introduction
!
! This function returns the total number of nodes for a given degree of
! freedom number
! idof should be lesser than the total degree of freedom
!
!## Usage
!
!```fortran
! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/.tNodes.
! CALL Display(     .tNodes. obj, '.tNodes. obj [30] : ' )
! CALL Display( obj .tNodes. 1, 'obj .tNodes. 1 [10] : ' )
! CALL Display( obj .tNodes. 2, 'obj .tNodes. 2 [10] : ' )
! CALL Display( obj .tNodes. 3, 'obj .tNodes. 3 [10] : ' )
! CALL Display( obj .tNodes. [1,2,3], 'obj .tNodes. [1,2,3] [30] : ' )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_tNodes3( obj, varName ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: varName
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of nodes
!
!# Introduction
!
! This function returns the total number of nodes for a given degree of
! freedom number
! idof should be lesser than the total degree of freedom
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/.tNodes.
! CALL Display(     .tNodes. obj, '.tNodes. obj [30] : ' )
! CALL Display( obj .tNodes. 1, 'obj .tNodes. 1 [10] : ' )
! CALL Display( obj .tNodes. 2, 'obj .tNodes. 2 [10] : ' )
! CALL Display( obj .tNodes. 3, 'obj .tNodes. 3 [10] : ' )
! CALL Display( obj .tNodes. [1,2,3], 'obj .tNodes. [1,2,3] [30] : ' )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_tNodes4( obj, idof ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: idof( : )
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tNodes4
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE OPERATOR( .tNodes. )
  MODULE PROCEDURE dof_tNodes1, dof_tNodes2, dof_tNodes3, dof_tNodes4
END INTERFACE

PUBLIC :: OPERATOR( .tNodes. )

INTERFACE SIZE
  MODULE PROCEDURE dof_tNodes1, dof_tNodes2, dof_tNodes3, dof_tNodes4
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                             tDOF@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of degree of freedom
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/.tNodes.
! CALL Display(     .tDOF. obj, '.tDOF. obj [3] : ' )
! CALL Display( obj .tDOF. 1,   'obj .tDOF. 1 [3] : ' )
! CALL Display( obj .tDOF. 'U', 'obj .tDOF. "U" [3] : ' )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION dof_tdof1( obj ) RESULT( Ans )
    CLASS( DOF_ ), INTENT( IN ) :: obj
    INTEGER( I4B ) :: Ans
  END FUNCTION dof_tdof1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tDOF@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This subroutine returns the total number of degrees of freedom
!
!# Introduction
! This function returns the total number of degrees of freedom in a
! physical variable.
! The physical variable is specified by using its name.
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/.tNodes.
! CALL Display(     .tDOF. obj, '.tDOF. obj [3] : ' )
! CALL Display( obj .tDOF. 1,   'obj .tDOF. 1 [3] : ' )
! CALL Display( obj .tDOF. 'U', 'obj .tDOF. "U" [3] : ' )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_tdof2( obj, Name ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Name
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tdof2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tDOF@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This subroutine returns the total number of degrees of freedom
!
!# Introduction
! This function returns the total number of degrees of freedom in a
! physical variable.
! The physical variable is specified by using its name.
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spaceCompo=[3],  &
!   & timeCompo=[1], storageFMT = FMT_DOF )
! ! #DOF/.tNodes.
! CALL Display(     .tDOF. obj, '.tDOF. obj [3] : ' )
! CALL Display( obj .tDOF. 1,   'obj .tDOF. 1 [3] : ' )
! CALL Display( obj .tDOF. 'U', 'obj .tDOF. "U" [3] : ' )
! ! #DOF/DeallocateData
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_tdof3( obj, ivar ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tdof3
END INTERFACE

INTERFACE OPERATOR( .tDOF. )
  MODULE PROCEDURE dof_tdof1, dof_tdof2, dof_tdof3
END INTERFACE

PUBLIC :: OPERATOR( .tDOF. )

!----------------------------------------------------------------------------
!                                                           tNames@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the total number of names in dof object
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   !> main
!   ! #DOF/Initiate
!   CALL Initiate( obj, tNodes=[20, 10], names=["V", "P"], spaceCompo=[3,1],  &
!     & timeCompo=[2,2], storageFMT = FMT_DOF )
!   ! #DOF/.tNames.
!   CALL Display( .tNames. obj, '.tNames. obj [2] : ' )
!   ! #DOF/.Names.
!   CALL Display( obj .Names. 1,   'obj .Names. 1 ["V"] : ' )
!   CALL Display( obj .Names. 2,   'obj .Names. 2 ["P"] : ' )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_tNames( obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tNames
END INTERFACE

INTERFACE OPERATOR( .tNames. )
  MODULE PROCEDURE dof_tNames
END INTERFACE

PUBLIC :: OPERATOR( .tNames. )

!----------------------------------------------------------------------------
!                                                           Names@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the name of all physical variables stored in obj
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   !> main
!   ! #DOF/Initiate
!   CALL Initiate( obj, tNodes=[20, 10], names=["V", "P"], spaceCompo=[3,1],  &
!     & timeCompo=[2,2], storageFMT = FMT_DOF )
!   ! #DOF/.tNames.
!   CALL Display( .tNames. obj, '.tNames. obj [2] : ' )
!   ! #DOF/.Names.
!   CALL Display( obj .Names. 1,   'obj .Names. 1 ["V"] : ' )
!   CALL Display( obj .Names. 2,   'obj .Names. 2 ["P"] : ' )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_names1( obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = 1 ), ALLOCATABLE :: Ans( : )
END FUNCTION dof_names1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Names@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the name of a physical variable
!
!# Introduction
!
! This function returns the name of a physical variable
! The physical variable is given by its number ii, i.e., the first, second,
! third, and so on, physical variable.
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   !> main
!   ! #DOF/Initiate
!   CALL Initiate( obj, tNodes=[20, 10], names=["V", "P"], spaceCompo=[3,1],  &
!     & timeCompo=[2,2], storageFMT = FMT_DOF )
!   ! #DOF/.tNames.
!   CALL Display( .tNames. obj, '.tNames. obj [2] : ' )
!   ! #DOF/.Names.
!   CALL Display( obj .Names. 1,   'obj .Names. 1 ["V"] : ' )
!   CALL Display( obj .Names. 2,   'obj .Names. 2 ["P"] : ' )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_names2( obj, ii ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ii
  CHARACTER( LEN = 1 ), ALLOCATABLE :: Ans
END FUNCTION dof_names2
END INTERFACE

INTERFACE OPERATOR( .Names. )
  MODULE PROCEDURE dof_names1, dof_names2
END INTERFACE OPERATOR( .Names. )

PUBLIC :: OPERATOR( .Names. )

!----------------------------------------------------------------------------
!                                                     NameToIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Return the index of a physical variable

INTERFACE
MODULE PURE FUNCTION NameToIndex( obj, Name ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Name
  INTEGER( I4B ) :: Ans
END FUNCTION NameToIndex
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! This routine is like [[DOF_Method:getIndex]].
! However, it does not return the index of all degrees of freedom of
! a given physical variable. Instead, it returns the index of location of
! degree of freedom number `idof` for a given node number `inode`.
!
! Note that `inode` should be lesser than the total number of nodes
! defined for dof number `idof`.
!
! Also node that idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   !> main
!   ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], names=["V", "P"], spaceCompo=[3,1],  &
!     & timeCompo=[2,2], storageFMT = FMT_DOF )
!   ! #DOF/getNodeLoc
!   CALL Display( getNodeLoc(obj, 1, 1), 'getNodeLoc(obj, 1, 1) [1] : ' )
!   CALL Display( getNodeLoc(obj, 1, 2), 'getNodeLoc(obj, 1, 2) [21] : ' )
!   CALL Display( getNodeLoc(obj, 1, 3), 'getNodeLoc(obj, 1, 3) [41] : ' )
!   CALL Display( getNodeLoc(obj, 1, 4), 'getNodeLoc(obj, 1, 4) [61] : ' )
!   CALL Display( getNodeLoc(obj, 1, 5), 'getNodeLoc(obj, 1, 5) [81] : ' )
!   CALL Display( getNodeLoc(obj, 1, 6), 'getNodeLoc(obj, 1, 6) [101] : ' )
!   CALL Display( getNodeLoc(obj, 1, 7), 'getNodeLoc(obj, 1, 7) [121] : ' )
!   CALL Display( getNodeLoc(obj, 1, 8), 'getNodeLoc(obj, 1, 8) [131] : ' )
!   CALL Display( getNodeLoc(obj, 10, 1), 'getNodeLoc(obj, 10, 1) [10] : ' )
!   CALL Display( getNodeLoc(obj, 10, 2), 'getNodeLoc(obj, 10, 2) [30] : ' )
!   CALL Display( getNodeLoc(obj, 10, 3), 'getNodeLoc(obj, 10, 3) [50] : ' )
!   CALL Display( getNodeLoc(obj, 10, 4), 'getNodeLoc(obj, 10, 4) [70] : ' )
!   CALL Display( getNodeLoc(obj, 10, 5), 'getNodeLoc(obj, 10, 5) [90] : ' )
!   CALL Display( getNodeLoc(obj, 10, 6), 'getNodeLoc(obj, 10, 6) [110] : ' )
!   CALL Display( getNodeLoc(obj, 10, 7), 'getNodeLoc(obj, 10, 7) [130] : ' )
!   CALL Display( getNodeLoc(obj, 10, 8), 'getNodeLoc(obj, 10, 8) [140] : ' )
!   CALL Display( getNodeLoc(obj, 1), 'getNodeLoc(obj, 1) [1,20,1] : ' )
!   CALL Display( getNodeLoc(obj, 2), 'getNodeLoc(obj, 2) [21,40,1] : ' )
!   CALL Display( getNodeLoc(obj, 3), 'getNodeLoc(obj, 3) [41,60,1] : ' )
!   CALL Display( getNodeLoc(obj, 7), 'getNodeLoc(obj, 7) [121,130,1] : ' )
!   CALL Display( getNodeLoc(obj, 8), 'getNodeLoc(obj, 7) [131,140,1] : ' )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   !> main
!   ! #DOF/Initiate
!  CALL Initiate( obj, tNodes=[20, 20], names=["V", "P"], spaceCompo=[3,1], &
!     & timeCompo=[2,2], storageFMT = FMT_NODES )
!   ! #DOF/getNodeLoc
!   CALL Display( getNodeLoc(obj, 1, 1), 'getNodeLoc(obj, 1, 1) [1] : ' )
!   CALL Display( getNodeLoc(obj, 1, 2), 'getNodeLoc(obj, 1, 2) [2] : ' )
!   CALL Display( getNodeLoc(obj, 1, 3), 'getNodeLoc(obj, 1, 3) [3] : ' )
!   CALL Display( getNodeLoc(obj, 1, 4), 'getNodeLoc(obj, 1, 4) [4] : ' )
!   CALL Display( getNodeLoc(obj, 1, 5), 'getNodeLoc(obj, 1, 5) [5] : ' )
!   CALL Display( getNodeLoc(obj, 1, 6), 'getNodeLoc(obj, 1, 6) [6] : ' )
!   CALL Display( getNodeLoc(obj, 1, 7), 'getNodeLoc(obj, 1, 7) [7] : ' )
!   CALL Display( getNodeLoc(obj, 1, 8), 'getNodeLoc(obj, 1, 8) [8] : ' )
!   CALL Display( getNodeLoc(obj, 1), 'getNodeLoc(obj, 1) [1,160,8] : ' )
!   CALL Display( getNodeLoc(obj, 2), 'getNodeLoc(obj, 2) [2,160,8] : ' )
!   CALL Display( getNodeLoc(obj, 3), 'getNodeLoc(obj, 3) [3,160,8] : ' )
!   CALL Display( getNodeLoc(obj, 7), 'getNodeLoc(obj, 7) [7,160,8] : ' )
!   CALL Display( getNodeLoc(obj, 8), 'getNodeLoc(obj, 7) [8,160,8] : ' )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getNodeLoc1( obj, inode, idof ) RESULT( Ans )
  TYPE( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: inode
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ) :: ans
END FUNCTION dof_getNodeLoc1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! ans(1) : istart
! ans(2) : iend
! ans(3) : stride
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   !> main
!   ! #DOF/Initiate
!   CALL Initiate( obj, tNodes=[20, 10], names=["V", "P"], spaceCompo=[3,1],  &
!     & timeCompo=[2,2], storageFMT = FMT_DOF )
!   ! #DOF/getNodeLoc
!   CALL Display( getNodeLoc(obj, 1, 1), 'getNodeLoc(obj, 1, 1) [1] : ' )
!   CALL Display( getNodeLoc(obj, 1, 2), 'getNodeLoc(obj, 1, 2) [21] : ' )
!   CALL Display( getNodeLoc(obj, 1, 3), 'getNodeLoc(obj, 1, 3) [41] : ' )
!   CALL Display( getNodeLoc(obj, 1, 4), 'getNodeLoc(obj, 1, 4) [61] : ' )
!   CALL Display( getNodeLoc(obj, 1, 5), 'getNodeLoc(obj, 1, 5) [81] : ' )
!   CALL Display( getNodeLoc(obj, 1, 6), 'getNodeLoc(obj, 1, 6) [101] : ' )
!   CALL Display( getNodeLoc(obj, 1, 7), 'getNodeLoc(obj, 1, 7) [121] : ' )
!   CALL Display( getNodeLoc(obj, 1, 8), 'getNodeLoc(obj, 1, 8) [131] : ' )
!   CALL Display( getNodeLoc(obj, 10, 1), 'getNodeLoc(obj, 10, 1) [10] : ' )
!   CALL Display( getNodeLoc(obj, 10, 2), 'getNodeLoc(obj, 10, 2) [30] : ' )
!   CALL Display( getNodeLoc(obj, 10, 3), 'getNodeLoc(obj, 10, 3) [50] : ' )
!   CALL Display( getNodeLoc(obj, 10, 4), 'getNodeLoc(obj, 10, 4) [70] : ' )
!   CALL Display( getNodeLoc(obj, 10, 5), 'getNodeLoc(obj, 10, 5) [90] : ' )
!   CALL Display( getNodeLoc(obj, 10, 6), 'getNodeLoc(obj, 10, 6) [110] : ' )
!   CALL Display( getNodeLoc(obj, 10, 7), 'getNodeLoc(obj, 10, 7) [130] : ' )
!   CALL Display( getNodeLoc(obj, 10, 8), 'getNodeLoc(obj, 10, 8) [140] : ' )
!   CALL Display( getNodeLoc(obj, 1), 'getNodeLoc(obj, 1) [1,20,1] : ' )
!   CALL Display( getNodeLoc(obj, 2), 'getNodeLoc(obj, 2) [21,40,1] : ' )
!   CALL Display( getNodeLoc(obj, 3), 'getNodeLoc(obj, 3) [41,60,1] : ' )
!   CALL Display( getNodeLoc(obj, 7), 'getNodeLoc(obj, 7) [121,130,1] : ' )
!   CALL Display( getNodeLoc(obj, 8), 'getNodeLoc(obj, 7) [131,140,1] : ' )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
!   USE easifemBase
!   IMPLICIT NONE
!   TYPE( DOF_ ) :: obj
!   !> main
!   ! #DOF/Initiate
!  CALL Initiate( obj, tNodes=[20, 20], names=["V", "P"], spaceCompo=[3,1], &
!     & timeCompo=[2,2], storageFMT = FMT_NODES )
!   ! #DOF/getNodeLoc
!   CALL Display( getNodeLoc(obj, 1, 1), 'getNodeLoc(obj, 1, 1) [1] : ' )
!   CALL Display( getNodeLoc(obj, 1, 2), 'getNodeLoc(obj, 1, 2) [2] : ' )
!   CALL Display( getNodeLoc(obj, 1, 3), 'getNodeLoc(obj, 1, 3) [3] : ' )
!   CALL Display( getNodeLoc(obj, 1, 4), 'getNodeLoc(obj, 1, 4) [4] : ' )
!   CALL Display( getNodeLoc(obj, 1, 5), 'getNodeLoc(obj, 1, 5) [5] : ' )
!   CALL Display( getNodeLoc(obj, 1, 6), 'getNodeLoc(obj, 1, 6) [6] : ' )
!   CALL Display( getNodeLoc(obj, 1, 7), 'getNodeLoc(obj, 1, 7) [7] : ' )
!   CALL Display( getNodeLoc(obj, 1, 8), 'getNodeLoc(obj, 1, 8) [8] : ' )
!   CALL Display( getNodeLoc(obj, 1), 'getNodeLoc(obj, 1) [1,160,8] : ' )
!   CALL Display( getNodeLoc(obj, 2), 'getNodeLoc(obj, 2) [2,160,8] : ' )
!   CALL Display( getNodeLoc(obj, 3), 'getNodeLoc(obj, 3) [3,160,8] : ' )
!   CALL Display( getNodeLoc(obj, 7), 'getNodeLoc(obj, 7) [7,160,8] : ' )
!   CALL Display( getNodeLoc(obj, 8), 'getNodeLoc(obj, 7) [8,160,8] : ' )
!   ! #DOF/DeallocateData
!   CALL DeallocateData( obj )
! END PROGRAM main
!```


INTERFACE
MODULE PURE FUNCTION dof_getNodeLoc2( obj, idof ) RESULT( Ans )
  TYPE( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ) :: ans(3)
END FUNCTION dof_getNodeLoc2
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc1, dof_getNodeLoc2
END INTERFACE getNodeLoc

PUBLIC :: getNodeLoc

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodeNum`
!
!# Introduction
!
! - This function returns a vector of integers (indices) for a
! a given node number.
! - The size of these indices is equal to the total number of DOF in obj
! - The returned indices represents the degrees of freedom defined on
! each node.
! - It is user's responsibility to ensure that for every physical variable
! the `nodeNumber` is lesser than the total number of
! nodes defined for that physical variable.
! - The returned indices can be used for getting the dof (all dof)
! defined on the nodeNum.
! - The returned indiced can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.
!
!@note
! 	The size of returned vector `ans` will be the total number of
! degrees of freedom in the [[DOF_]] object
!@endnote
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getIndex1( obj, nodeNum ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION dof_getIndex1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodeNum`
!
!# Introduction
!
! - This function returns a vector of integers (indices) for a
! a given node number and a given physical Variable.
! - The physical variable is defined by an `iVar`
! - The size of these indices is equal to the total number of DOF
! defined for the `iVar` physical variable.
! - The returned indices represents the degrees of freedom of
! physical variable `ivar` defined on each node.
! - It is user's responsibility to ensure that for the selected physical var
! the `nodeNumber` is lesser than the total number of
! nodes defined for that physical variable.
! - The returned indices can be used for getting the dof (all dof)
! defined on the nodeNum for the given physical variable.
! - The returned indices can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getIndex2( obj, nodeNum, iVar ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum
  INTEGER( I4B ), INTENT( IN ) :: iVar
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION dof_getIndex2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodeNum`
!
!# Introduction
!
! - This function returns a vector of integers (indices) for a
! a given node number and a given physical Variable.
! - The physical variable is defined by an `varName`
! - The size of these indices is equal to the total number of DOF
! defined for the `varName` physical variable.
! - The returned indices represents the degrees of freedom of
! physical variable `varName` defined on each node.
! - It is user's responsibility to ensure that for the selected physical var
! the `nodeNumber` is lesser than the total number of
! nodes defined for that physical variable.
! - The returned indices can be used for getting the dof (all dof)
! defined on the nodeNum for the given physical variable.
! - The returned indices can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getIndex3( obj, nodeNum, varName ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum
  CHARACTER( LEN = 1 ), INTENT( IN ) :: varName
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION dof_getIndex3
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodeNum`
!
!# Introduction
!
! - This function returns a vector of integers (indices) for a
! given list of node number.
! - The size of these indices is equal to the total number of DOF in obj
! times the size of nodeNum(:)
! - The returned indices represents the degrees of freedom defined on
! each nodes.
! - It is user's responsibility to ensure that for every physical variable
! the `nodeNumber` is lesser than the total number of
! nodes defined for that physical variable.
! - The returned indices can be used for getting the dof (all dof)
! defined on the nodeNum.
! - The returned indiced can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getIndex4( obj, nodeNum ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION dof_getIndex4
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodeNum`
!
!# Introduction
!
! - This function returns a vector of integers (indices) for a
! a given node number and a given physical Variable.
! - The physical variable is defined by an `iVar`
! - The size of these indices is equal to the total number of DOF
! defined for the `iVar` physical variable.
! - The returned indices represents the degrees of freedom of
! physical variable `ivar` defined on each node.
! - It is user's responsibility to ensure that for the selected physical var
! the `nodeNumber` is lesser than the total number of
! nodes defined for that physical variable.
! - The returned indices can be used for getting the dof (all dof)
! defined on the nodeNum for the given physical variable.
! - The returned indices can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getIndex5( obj, nodeNum, iVar ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: iVar
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION dof_getIndex5
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodeNum`
!
!# Introduction
!
! - This function returns a vector of integers (indices) for a
! a given node number and a given physical Variable.
! - The physical variable is defined by an `varName`
! - The size of these indices is equal to the total number of DOF
! defined for the `varName` physical variable.
! - The returned indices represents the degrees of freedom of
! physical variable `varName` defined on each node.
! - It is user's responsibility to ensure that for the selected physical var
! the `nodeNumber` is lesser than the total number of
! nodes defined for that physical variable.
! - The returned indices can be used for getting the dof (all dof)
! defined on the nodeNum for the given physical variable.
! - The returned indices can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getIndex6( obj, nodeNum, varName ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  CHARACTER( LEN = 1 ), INTENT( IN ) :: varName
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION dof_getIndex6
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the index
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1, idof=1 ),  "[5] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1, idof=2 ),  "[25] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1, idof=3 ),  "[45] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getIndex7( obj, nodeNum, iVar, idof ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum
  INTEGER( I4B ), INTENT( IN ) :: iVar
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ) :: ans
END FUNCTION dof_getIndex7
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the index
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1, idof=1 ),  "[5] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1, idof=2 ),  "[25] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1, idof=3 ),  "[45] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_getIndex8( obj, nodeNum, iVar, idof ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: iVar
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ) :: ans( SIZE(nodeNum) )
END FUNCTION dof_getIndex8
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE dof_getIndex1, dof_getIndex2, dof_getIndex3, &
    & dof_getIndex4, dof_getIndex5, dof_getIndex6, dof_getIndex7,  &
    & dof_getIndex8
END INTERFACE getIndex

PUBLIC :: getIndex

!----------------------------------------------------------------------------
!                                                tSpaceComponents@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the total physical variable which have space-compo
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_tSpaceComponents( obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tSpaceComponents
END INTERFACE

INTERFACE OPERATOR( .tSpaceComponents. )
  MODULE PROCEDURE dof_tSpaceComponents
END INTERFACE

PUBLIC :: OPERATOR( .tSpaceComponents. )

!----------------------------------------------------------------------------
!                                                SpaceComponents@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the space components of each physical vars
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_SpaceComponents1( obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION dof_SpaceComponents1
END INTERFACE

!----------------------------------------------------------------------------
!                                                SpaceComponents@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the space component of a given physical vars
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_SpaceComponents2( obj,ivar) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ) :: ans
END FUNCTION dof_SpaceComponents2
END INTERFACE

INTERFACE OPERATOR( .SpaceComponents. )
  MODULE PROCEDURE dof_SpaceComponents1, dof_SpaceComponents2
END INTERFACE

PUBLIC :: OPERATOR( .SpaceComponents. )

!----------------------------------------------------------------------------
!                                                tTimeComponents@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the total physical var which has time compo
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_tTimeComponents( obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tTimeComponents
END INTERFACE

INTERFACE OPERATOR( .tTimeComponents. )
  MODULE PROCEDURE dof_tTimeComponents
END INTERFACE

PUBLIC :: OPERATOR( .tTimeComponents. )

!----------------------------------------------------------------------------
!                                                TimeComponents@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the timecompo
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_TimeComponents1( obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION dof_TimeComponents1
END INTERFACE

!----------------------------------------------------------------------------
!                                                TimeComponents@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the timecompo
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[20, 10], &
!   & names=["V", "P"], spaceCompo=[3, 1], &
!   & timeCompo=[1, 1], storageFMT = FMT_DOF )
! ! #DOF/GetIndex
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=1 ),  "[5, 25, 45] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="V" ), '[5, 25, 45] : ' )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, iVar=2 ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=5, varName="P" ), "[65] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=1 ),  "[10, 30, 50] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=10, iVar=2 ), "[70] : " )
! CALL Display( &
!   & GetIndex( obj, nodeNum=[5,10], iVar=1 ),  "[5,25,45,10,30,50] : " )
! CALL DeallocateData( obj )
! END PROGRAM main
!```

INTERFACE
MODULE PURE FUNCTION dof_TimeComponents2( obj, ivar ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ) :: Ans
END FUNCTION dof_TimeComponents2
END INTERFACE

INTERFACE OPERATOR( .TimeComponents. )
  MODULE PROCEDURE dof_TimeComponents1, dof_TimeComponents2
END INTERFACE

PUBLIC :: OPERATOR( .TimeComponents. )

!----------------------------------------------------------------------------
!                                                        getValue@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
! This subroutine extracts the values of from `val` corresponding to
! degrees of freedom specified by `DOFNo(:)` and return it in `V`
!
! - `StorageFMT` can be 'Nodes_FMT' or `DOF_FMT`. It specify the storage
! format of returned vector.

INTERFACE
MODULE PURE SUBROUTINE dof_getValue1( v, val, obj, dofNO, storageFMT, nptrs )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: v( : )
  REAL( DFP ), INTENT( IN ) :: val( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT(IN ) :: dofNo( : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nptrs( : )
END SUBROUTINE dof_getValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              get@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a 2D array
!
!# Introduction
! This subroutine extracts all the values of from `val` corresponding to
! degrees of freedom specified by `DOFNo(:)` and return it in `V(:,:)`
! Values in `Val(:,:)` are stored in xiJ format.
!
! - Force3D will return a vector in 3D. if there are only two components
! then it will set the third component to 0
!

INTERFACE
MODULE PURE SUBROUTINE dof_getValue2( v, Val, obj, DOFNo, force3D )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: v( :, : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT(IN ) :: DOFNo( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: force3D
END SUBROUTINE dof_getValue2
END INTERFACE

!----------------------------------------------------------------------------
!                                                        getValue@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
! This subroutine extracts the values of from `val` corresponding to
! degrees of freedom specified by `DOFNo(:)` and return it in `V`
!
! - `StorageFMT` can be 'Nodes_FMT' or `DOF_FMT`. It specify the storage
! format of returned vector.

INTERFACE
MODULE PURE SUBROUTINE dof_getValue3( v, val, obj, dofNO, storageFMT, nptrs )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: v( : )
  TYPE( RealVector_ ), INTENT( IN ) :: val
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT(IN ) :: dofNo( : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nptrs( : )
END SUBROUTINE dof_getValue3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              get@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a 2D array
!
!# Introduction
! This subroutine extracts all the values of from `val` corresponding to
! degrees of freedom specified by `DOFNo(:)` and return it in `V(:,:)`
! Values in `Val(:,:)` are stored in xiJ format.
!
! - Force3D will return a vector in 3D. if there are only two components
! then it will set the third component to 0
!

INTERFACE
MODULE PURE SUBROUTINE dof_getValue4( v, Val, obj, DOFNo, force3D )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: v( :, : )
  TYPE( RealVector_ ), INTENT( IN ) :: Val
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT(IN ) :: DOFNo( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: force3D
END SUBROUTINE dof_getValue4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@getMethod
!----------------------------------------------------------------------------

INTERFACE getValue
  MODULE PROCEDURE dof_getValue1, dof_getValue2, dof_getValue3, dof_getValue4
END INTERFACE getValue

PUBLIC :: getValue

!----------------------------------------------------------------------------
!                                                             get@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_get1( Val, obj, DOFNo, StorageFMT, Nptrs, &
    & Force3D )  RESULT( Ans )
    REAL( DFP ), INTENT( IN ) :: Val( : )
    CLASS( DOF_ ), INTENT( IN ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: DOFNo( : )
    INTEGER( I4B ), INTENT( IN ) :: StorageFMT
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Nptrs( : )
    LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: force3D
    REAL( DFP ), ALLOCATABLE :: Ans( : )
  END FUNCTION dof_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@getMethod
!----------------------------------------------------------------------------

INTERFACE get
  MODULE PROCEDURE dof_get1
END INTERFACE get

PUBLIC :: get

!----------------------------------------------------------------------------
!                                                               EQ@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION dof_isEqual( obj1, obj2 ) RESULT( Ans )
  TYPE( DOF_ ), INTENT( IN ) :: obj1
  TYPE( DOF_ ), INTENT( IN ) :: obj2
  LOGICAL( LGT ) :: ans
END FUNCTION dof_isEqual
END INTERFACE

INTERFACE OPERATOR( .EQ. )
  MODULE PROCEDURE dof_isEqual
END INTERFACE OPERATOR( .EQ. )

PUBLIC :: OPERATOR( .EQ. )

!----------------------------------------------------------------------------
!                                                               NE@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION dof_isNE( obj1, obj2 ) RESULT( Ans )
  TYPE( DOF_ ), INTENT( IN ) :: obj1
  TYPE( DOF_ ), INTENT( IN ) :: obj2
  LOGICAL( LGT ) :: ans
END FUNCTION dof_isNE
END INTERFACE

INTERFACE OPERATOR( .NE. )
  MODULE PROCEDURE dof_isNE
END INTERFACE OPERATOR( .NE. )

PUBLIC :: OPERATOR( .NE. )

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to set the values in a vector of real number
!
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `Val` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `Conversion` can be set to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
! - This subroutine effectivily performes `Vec( Nptrs ) = Val`
! - If `SIZE(val)==1` then all values are set to `val(1)`
! - If `SIZE(val) .EQ. SIZE(Nptrs)` then for each dof its value set to
!  `val`
! - If `SIZE(val)=tDOF*Size(Nptrs)` then each dof will be set to
! corresponding val

INTERFACE
MODULE PURE SUBROUTINE dof_set1( Vec, obj, Nptrs, Val, Conversion )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: Conversion( 1 )
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE dof_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to set the values in a vector of real number.
! This subroutine handles only those entries which belongs to the
! dofno.
!
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes `Vec( Nptrs ) = Val`

INTERFACE
MODULE PURE SUBROUTINE dof_set2( Vec, obj, Nptrs, Val, dofno )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: dofno
END SUBROUTINE dof_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to set the values in a vector of real number
!
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `Val` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `Conversion` can be set to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
! - This subroutine effectivily performes `Vec( Nptrs ) = Val`
! - If `SIZE(val)==1` then all values are set to `val(1)`
! - If `SIZE(val) .EQ. SIZE(Nptrs)` then for each dof its value set to
!  `val`
! - If `SIZE(val)=tDOF*Size(Nptrs)` then each dof will be set to
! corresponding val

INTERFACE
MODULE PURE SUBROUTINE dof_set3( Vec, obj, Nptrs, Val, Conversion )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Vec
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: Conversion( 1 )
END SUBROUTINE dof_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to set the values in a vector of real number.
! This subroutine handles only those entries which belongs to the
! dofno.
!
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes `Vec( Nptrs ) = Val`

INTERFACE
MODULE PURE SUBROUTINE dof_set4( Vec, obj, Nptrs, Val, dofno )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Vec
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: dofno
END SUBROUTINE dof_set4
END INTERFACE

!> Generic subroutine to set values in fortran vectors using [[dof_]] object
INTERFACE set
  MODULE PROCEDURE dof_set1, dof_set2, dof_set3, dof_set4
END INTERFACE set

PUBLIC :: set

!----------------------------------------------------------------------------
!                                                            add@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: 	 Set values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `Val` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `Conversion`  can be set to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
!
! This subroutine effectivily performes
! `Vec( Nptrs ) = Vec(Nptrs) + scale * Val`

INTERFACE
MODULE PURE SUBROUTINE dof_add1( Vec, obj, Nptrs, Val, scale, &
  & Conversion )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Conversion( 1 )
  REAL( DFP ), INTENT( IN ) :: Val( : ), scale
END SUBROUTINE dof_add1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            add@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 June 2021
! summary: 	add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `Vec( Nptrs ) = Vec(Nptrs) + scale * Val`

INTERFACE
MODULE PURE SUBROUTINE dof_add2( Vec, obj, Nptrs, Val, scale, dofno )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( : ), scale
  INTEGER( I4B ), INTENT( IN ) :: dofno
END SUBROUTINE dof_add2
END INTERFACE

!> Generic subroutine to add values in vectors using [[dof_]] object
INTERFACE add
  MODULE PROCEDURE dof_add1, dof_add2
END INTERFACE add

PUBLIC :: add

END MODULE DOF_Method