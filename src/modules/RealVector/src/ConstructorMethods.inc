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
PUBLIC :: SIZE
PUBLIC :: getTotalDimension
PUBLIC :: setTotalDimension
PUBLIC :: Allocate
PUBLIC :: Deallocate
PUBLIC :: Initiate
PUBLIC :: RANDOM_NUMBER
PUBLIC :: RealVector
PUBLIC :: RealVector_Pointer

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This function returns the shape of [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_shape( obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans( 1 )
END FUNCTION realVec_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE realVec_shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This function returns the size of [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_size( obj, Dims ) RESULT( Ans )
  TYPE( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION realVec_size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE realVec_size
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the total dimension of an array
!
!# Introduction
!
! This function returns the total dimension (or rank) of an array,

INTERFACE
MODULE PURE FUNCTION RealVec_getTotalDimension( obj ) RESULT( Ans )
  TYPE( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION RealVec_getTotalDimension
END INTERFACE

INTERFACE getTotalDimension
  MODULE PROCEDURE RealVec_getTotalDimension
END INTERFACE getTotalDimension

!----------------------------------------------------------------------------
!                                             setTotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This subroutine set the total dimension (rank) of an array
!
!# Introduction
!
! This subroutine sets the rank(total dimension) of an array

INTERFACE
MODULE PURE SUBROUTINE RealVec_setTotalDimension( obj, tDimension )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tDimension
END SUBROUTINE RealVec_setTotalDimension
END INTERFACE

INTERFACE setTotalDimension
  MODULE PROCEDURE RealVec_setTotalDimension
END INTERFACE setTotalDimension

!----------------------------------------------------------------------------
!                                                   AllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: 	This subroutine allocates memory for [[RealVector_]]

INTERFACE
MODULE PURE SUBROUTINE realVec_Allocate( obj, Dims )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Dims
END SUBROUTINE realVec_Allocate
END INTERFACE

INTERFACE Allocate
  MODULE PROCEDURE realVec_Allocate
END INTERFACE Allocate

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This subroutine deallocates the data in [[RealVector_]]

INTERFACE
MODULE PURE SUBROUTINE realVec_Deallocate( obj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE realVec_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE realVec_Deallocate
END INTERFACE Deallocate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This subroutine allocates the memory for [[RealVector_]]
!
!# Introduction This subroutine allocates the memeory for [[RealVector_]]
!
!@note
! This subroutine is an alias for [[Allocate_Data]]
!@endnote

INTERFACE
MODULE PURE SUBROUTINE realVec_initiate1( obj, tSize )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END SUBROUTINE realVec_initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realVec_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: This subroutine allocate the memory for a vector of type [[RealVector_]]
!
!# Introduction
! This subroutine allocate the memory for a vector of type [[RealVector_]]
!@note
! The size of `obj` would be same as the size of `tSize`
!@endnote

INTERFACE
MODULE PURE SUBROUTINE realVec_Initiate2( obj, tSize )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: tSize( : )
END SUBROUTINE realVec_Initiate2
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realVec_initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: This subroutine allocate the memory for an instance of [[RealVector_]]
!
!# Introduction
! This subroutine allocate the memory for an instance of [[RealVector_]].
! User can specify the lowerbounds and upper bounds.

INTERFACE
MODULE PURE SUBROUTINE realVec_Initiate3( obj, a, b )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: a, b
END SUBROUTINE realVec_Initiate3
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realVec_initiate3
END INTERFACE Initiate

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

INTERFACE
MODULE PURE SUBROUTINE realVec_Initiate4( obj, dofobj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
END SUBROUTINE realVec_Initiate4
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realVec_Initiate4
END INTERFACE Initiate

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
! The size of `val` will be total number of degrees of freedom inside
! the [[DOF_]] object. Therefore, each `val( idof )` denotes the
! nodal vector of correrponding to a degree of freedom number `idof`

INTERFACE
MODULE PURE SUBROUTINE realVec_Initiate5( obj, dofobj )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: obj( : )
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
END SUBROUTINE realVec_Initiate5
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE realVec_Initiate5
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This routine computes radom_number
!
!# Introduction
!
! This routine calls `RANDOM_NUMBER` to generate a random instnance of [[RealVector_]]

INTERFACE
MODULE SUBROUTINE realVec_Random_Number1( obj, tsize )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tsize
END SUBROUTINE realVec_Random_Number1
END INTERFACE

INTERFACE RANDOM_NUMBER
  MODULE PROCEDURE realVec_Random_Number1
END INTERFACE RANDOM_NUMBER

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This routine computes radom_number
!
!# Introduction
!
! This routine calls `RANDOM_NUMBER` to generate a random instnance of [[RealVector_]]
!
!@note
! 	Here argument `obj` is a vector of [[RealVector_]] data-types.
!@endnote

INTERFACE
MODULE SUBROUTINE realVec_Random_Number2( obj, tsize )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: obj(:)
  INTEGER( I4B ), INTENT( IN ) :: tsize( : )
END SUBROUTINE realVec_Random_Number2
END INTERFACE

INTERFACE RANDOM_NUMBER
  MODULE PROCEDURE realVec_Random_Number2
END INTERFACE RANDOM_NUMBER

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!# Introduction
!
! This function returns an instance of [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_Constructor1( tSize ) RESULT( obj )
  TYPE( RealVector_ ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION realVec_Constructor1
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE realVec_Constructor1
END INTERFACE RealVector

!----------------------------------------------------------------------------
!                                                     RealVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!# Introduction
! This function returns an instance of [[RealVector_]] by copying the contents of a fortran integer vector.
!
!@note
! 	This routine internally calls [[RealVector_Method:COPY]] routine.
!@endnote

INTERFACE
MODULE PURE FUNCTION realVec_Constructor2( Val ) RESULT( obj )
  TYPE( RealVector_ ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Val( : )
END FUNCTION realVec_Constructor2
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE realVec_Constructor2
END INTERFACE RealVector

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!# Introduction
! This function returns an instance of [[RealVector_]] by copying the contents of a fortran real vector.
!
!@note
! 	This routine internally calls [[RealVector_Method:COPY]] routine.
!@endnote

INTERFACE
MODULE PURE FUNCTION realVec_Constructor3( Val ) RESULT( obj )
  TYPE( RealVector_ ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
END FUNCTION realVec_Constructor3
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE realVec_Constructor3
END INTERFACE RealVector

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returnt the pointer to a newly created instance of [[RealVector_]]
!
!# Introduction
! This function returnt the pointer to a newly created instance of [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_Constructor_1( tSize ) RESULT( obj )
  CLASS( RealVector_ ), POINTER :: obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION realVec_Constructor_1
END INTERFACE

INTERFACE RealVector_Pointer
  MODULE PROCEDURE realVec_Constructor_1
END INTERFACE RealVector_Pointer

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to an instance of [[RealVector_]]
!
!# Introduction
! This function returns a pointer to an newly created instance of [[RealVector_]] by copying the contents of a fortran integer vector.
!
!@note
! 	This routine internally calls [[RealVector_Method:COPY]] routine.
!@endnote

INTERFACE
MODULE PURE FUNCTION realVec_Constructor_2( Val ) RESULT( obj )
  CLASS( RealVector_ ), POINTER :: obj
  INTEGER( I4B ), INTENT( IN ) :: Val( : )
END FUNCTION realVec_Constructor_2
END INTERFACE

INTERFACE RealVector_Pointer
  MODULE PROCEDURE realVec_Constructor_2
END INTERFACE RealVector_Pointer

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to an instance of [[RealVector_]]
!
!# Introduction
! This function returns a pointer to an newly created instance of [[RealVector_]] by copying the contents of a fortran real vector.
!
!@note
! 	This routine internally calls [[RealVector_Method:COPY]] routine.
!@endnote

INTERFACE
MODULE PURE FUNCTION realVec_Constructor_3( Val ) RESULT( obj )
  CLASS( RealVector_ ), POINTER :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
END FUNCTION realVec_Constructor_3
END INTERFACE

INTERFACE RealVector_Pointer
  MODULE PROCEDURE realVec_Constructor_3
END INTERFACE RealVector_Pointer