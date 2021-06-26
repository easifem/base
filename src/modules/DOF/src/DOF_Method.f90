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
!### Introduction
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
!### Introduction
!
! This subroutine initiate [[DOF_]] object
!
!- If the size of all physical variables are equal then set tNodes = [tNodes] otherwise we need to provide size of each dof
!- For a scalar physical variable such as pressure and temperature, `spaceCompo` is set to -1.
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
!	type( dof_ ) :: obj
! call initiate( obj, tNodes = [10], Names = ['x', 'y'], &
!   & SpaceCompo = [3,3], TimeCompo = [1,1], StorageFMT = FMT_Nodes )
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_st_dof( obj, tNodes, Names, SpaceCompo, &
  & TimeCompo, StorageFMT )
  CLASS( DOF_ ), INTENT( INOUT ) :: obj
    !! degree of freedom object
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : )
    !! number of nodes for each physical variable
  INTEGER( I4B ), INTENT( IN ) :: SpaceCompo( : )
    !! Space component of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: TimeCompo( : )
    !! Time component of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
    !! Storage format `FMT_DOF`, `FMT_Nodes`
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
    !! Names of each physical variable
END SUBROUTINE initiate_st_dof
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: Initiate a fortran vector using [[DOF_]] object
!
!### Introduction
!
! This subroutine initiate a fortran vector of real using the information stored inside [[DOF_]] object.
!
!@todo
!usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE initiate_val( Val, obj )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
    !! This vector will be initiated by using obj
  CLASS( DOF_ ), INTENT( IN ) :: obj
    !! DOF object
END SUBROUTINE initiate_val
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: Initiate [[RealVector_]] using [[dof_]] object
!
!### Introduction
!
! This subroutine initiate [[RealVector_]] using the information stored inside
! [[dof_]] object
!
!@todo
!usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE initiate_realvector_scalar( Val, obj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Val
  CLASS( DOF_ ), INTENT( IN ) :: obj
END SUBROUTINE initiate_realvector_scalar
END INTERFACE

INTERFACE
!! Initiate a vector of [[realvector_]] from [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate a vector of [[realvector_]] object
! Each entry Val( idof ) denotes degree of freedom `idof`

MODULE PURE SUBROUTINE initiate_realvector_vector( Val, obj )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
END SUBROUTINE initiate_realvector_vector
END INTERFACE

INTERFACE
!! Initiate two fortran vectors using [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate two fortran vectors using  the information
! stored inside the [[dof_]] object

MODULE PURE SUBROUTINE initiate_2val( Val1, Val2, obj )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: Val1( : ), Val2( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
END SUBROUTINE initiate_2val
END INTERFACE

!> Generic interface to initiate Fortran vectors or [[RealVector_]] from
! [[DOF_]] object
INTERFACE Initiate
  MODULE PROCEDURE initiate_st_dof, initiate_val, &
    & initiate_realvector_scalar, &
    & initiate_realvector_vector, &
    & initiate_2val
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                            DOF@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Constructor for [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This function return instance of [[dof_]]
! for more see [[dof_]]

MODULE PURE FUNCTION Constructor1( tNodes, Names, SpaceCompo, TimeCompo, &
  & StorageFMT ) RESULT( obj )
  TYPE(DOF_) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : ), SpaceCompo( : ), &
    & TimeCompo( : ), StorageFMT
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
END FUNCTION Constructor1
END INTERFACE

!> Generic function to construct [[dof_]] object
INTERFACE DOF
  MODULE PROCEDURE Constructor1
END INTERFACE DOF

PUBLIC :: DOF

!----------------------------------------------------------------------------
!                                                     DOF_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Constructor for [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This function returns the pointer to instance of [[dof_]] object
! for more see [[dof_]]

MODULE FUNCTION Constructor_1( tNodes, Names, SpaceCompo, TimeCompo, &
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
END FUNCTION Constructor_1
END INTERFACE

!> Generic interface to get pointer to instance of [[dof_]] object
INTERFACE DOF_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE DOF_Pointer

PUBLIC :: DOF_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Deallocate data in [[dof_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocates the data in [[dof_]] object

MODULE PURE SUBROUTINE deallocate_data( obj )
  CLASS(DOF_), INTENT( INOUT ) :: obj
END SUBROUTINE deallocate_data
END INTERFACE

!> Generic interface to deallocate data in [[dof_]]
INTERFACE DeallocateData
  MODULE PROCEDURE deallocate_data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
!! Display content of [[dof_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine display the content of [[dof_]] object

MODULE SUBROUTINE display_obj( obj, msg, UnitNo )
  CLASS(DOF_), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE display_obj
END INTERFACE

INTERFACE
!! Display content of fortran vec with [[DOF_]] object info

MODULE SUBROUTINE dof_display_vec( Vec, obj, msg, unitno )
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE dof_display_vec
END INTERFACE

!> Generic subroutine to displacy content of [[dof_]]
INTERFACE Display
  MODULE PROCEDURE display_obj, dof_display_vec
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total length of the vector which stores the dof stored inside `obj`.

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
!### Introduction
! This function returns the total number of nodes for a given degree of
! freedom number
! idof should be lesser than the total degree of freedom

INTERFACE
MODULE PURE FUNCTION dof_tNodes2( obj, idof ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ) :: Ans
END FUNCTION dof_tNodes2
END INTERFACE

INTERFACE OPERATOR( .tNodes. )
  MODULE PROCEDURE dof_tNodes1, dof_tNodes2
END INTERFACE

PUBLIC :: OPERATOR( .tNodes. )

INTERFACE SIZE
  MODULE PROCEDURE dof_tNodes1, dof_tNodes2
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                             tDOF@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of degree of freedom

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
!### Introduction
! This function returns the total number of degrees of freedom in a physical variable.
! The physical variable is specified by using its name.

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
!### Introduction
! This function returns the total number of degrees of freedom in a physical variable.
! The physical variable is specified by using its name.

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
! summary: This subroutine returns the total number of names in dof object

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
! summary: This function returns the name of all physical variables stored in obj

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
!### Introduction
! This function returns the name of a physical variable
! The physical variable is given by its number ii, i.e., the first, second, third, and so on, physical variable.

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
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodeNum`
!
!### Introduction
! This function returns the indices of a given node number. This indices can be used for getting all the dof defined on that nodeNum. The returned indiced can be used to extract values from the [[RealVector_]] or fortran vector of real numbers.

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
!### Introduction
! This function returns the indices of a given node number. This indices can be used for getting all the dof defined on that nodeNum. The returned indiced can be used to extract values from the [[RealVector_]] or fortran vector of real numbers.

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
!### Introduction
! This function returns the indices of a given node number. This indices can be used for getting all the dof defined on that nodeNum. The returned indiced can be used to extract values from the [[RealVector_]] or fortran vector of real numbers.

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
!### Introduction
! This function returns the indices of a given node number. This indices can be used for getting all the dof defined on that nodeNum. The returned indiced can be used to extract values from the [[RealVector_]] or fortran vector of real numbers.

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
!### Introduction
! This function returns the indices of a given node number. This indices can be used for getting all the dof defined on that nodeNum. The returned indiced can be used to extract values from the [[RealVector_]] or fortran vector of real numbers.

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
!### Introduction
! This function returns the indices of a given node number. This indices can be used for getting all the dof defined on that nodeNum. The returned indiced can be used to extract values from the [[RealVector_]] or fortran vector of real numbers.

INTERFACE
MODULE PURE FUNCTION dof_getIndex6( obj, nodeNum, varName ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  CHARACTER( LEN = 1 ), INTENT( IN ) :: varName
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION dof_getIndex6
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE dof_getIndex1, dof_getIndex2, dof_getIndex3, &
      & dof_getIndex4, dof_getIndex5, dof_getIndex6
END INTERFACE getIndex

PUBLIC :: getIndex

!----------------------------------------------------------------------------
!                                                tSpaceComponents@getMethod
!----------------------------------------------------------------------------

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
!                                                tTimeComponents@getMethod
!----------------------------------------------------------------------------

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
!                                                   getArrayValues@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a single vector
!
!### Introduction
! This subroutine extracts the values of from `val` corresponding to
! degrees of freedom specified by `DOFNo(:)` and return it in `V`
!
! - `StorageFMT` can be 'Nodes_FMT' or `DOF_FMT`

INTERFACE
MODULE PURE SUBROUTINE get_arrayvalues_single_vec( v, Val, obj, DOFNo, &
  & StorageFMT, Nptrs )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: v( : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT(IN ) :: DOFNo( : ), StorageFMT
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Nptrs( : )
END SUBROUTINE get_arrayvalues_single_vec
END INTERFACE

INTERFACE getArrayValues
  MODULE PROCEDURE get_arrayvalues_single_vec
END INTERFACE

PUBLIC :: getArrayValues

!----------------------------------------------------------------------------
!                                                   getArrayValues@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a 2D array
!
!### Introduction
! This subroutine extracts the values of from `val` corresponding to
! degrees of freedom specified by `DOFNo(:)` and return it in `V(:,:)`
! Values in `Val(:,:)` are stored in xiJ format.
!
! - Force3D will return a vector in 3D. if there are only two components
! then it will set the third component to 0
!

INTERFACE
MODULE PURE SUBROUTINE get_arrayvalues_array( v, Val, obj, DOFNo, force3D )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: v( :, : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT(IN ) :: DOFNo( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: force3D
END SUBROUTINE get_arrayvalues_array
END INTERFACE

INTERFACE getArrayValues
  MODULE PROCEDURE get_arrayvalues_array
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ArrayValues@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION arrayvalues_single_vec( Val, obj, DOFNo, &
    & StorageFMT, Nptrs, Force3D )  RESULT( Ans )
    REAL( DFP ), INTENT( IN ) :: Val( : )
    CLASS( DOF_ ), INTENT( IN ) :: obj
    INTEGER( I4B ), INTENT(IN ) :: DOFNo( : ), StorageFMT
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Nptrs( : )
    LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: force3D
    REAL( DFP ), ALLOCATABLE :: Ans( : )
  END FUNCTION arrayvalues_single_vec
END INTERFACE

INTERFACE ArrayValues
  MODULE PROCEDURE arrayvalues_single_vec
END INTERFACE

PUBLIC :: ArrayValues

!----------------------------------------------------------------------------
!                                                        setValue@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers
!
!### Introduction
!
! This subroutine is designed to set the values in a vector of real number
!
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `Val` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `Conversion`  can be set to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
! - This subroutine effectivily performes `Vec( Nptrs ) = Val`
! - If `SIZE(val)==1` then all values are set to `val(1)`
! - If `SIZE(val) .EQ. SIZE(Nptrs)` then for each dof its value set to
!  `val`
! - If `SIZE(val)=tDOF*Size(Nptrs)` then each dof will be set to
! corresponding val

INTERFACE
MODULE PURE SUBROUTINE dof_setValue_1( Vec, obj, Nptrs, Val, Conversion )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Conversion( 1 )
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE dof_setValue_1
END INTERFACE

INTERFACE
!! Set values in a vector of real numbers

!> authors: Dr. Vikas Sharma
!
! This subroutine is designed to set the values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes `Vec( Nptrs ) = Val`

MODULE PURE SUBROUTINE dof_setValue_2( Vec, obj, Nptrs, Val, dofno )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: dofno
END SUBROUTINE dof_setValue_2
END INTERFACE

!> Generic subroutine to set values in fortran vectors using [[dof_]] object
INTERFACE setValue
  MODULE PROCEDURE dof_setValue_1, dof_setValue_2
END INTERFACE setValue

PUBLIC :: setValue

!----------------------------------------------------------------------------
!                                                 addContribution@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! Set values in a vector of real numbers

!> authors: Dr. Vikas Sharma
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

MODULE PURE SUBROUTINE dof_addValue_1( Vec, obj, Nptrs, Val, scale, &
  & Conversion )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Conversion( 1 )
  REAL( DFP ), INTENT( IN ) :: Val( : ), scale
END SUBROUTINE dof_addValue_1
END INTERFACE

INTERFACE
!! Set values in a vector of real numbers

!> authors: Dr. Vikas Sharma
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `Vec( Nptrs ) = Vec(Nptrs) + scale * Val`

MODULE PURE SUBROUTINE dof_addValue_2( Vec, obj, Nptrs, Val, scale, dofno )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( : ), scale
  INTEGER( I4B ), INTENT( IN ) :: dofno
END SUBROUTINE dof_addValue_2
END INTERFACE

!> Generic subroutine to add values in vectors using [[dof_]] object
INTERFACE addContribution
  MODULE PROCEDURE dof_addValue_1, dof_addValue_2
END INTERFACE addContribution

PUBLIC :: addContribution

END MODULE DOF_Method