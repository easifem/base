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

PUBLIC :: Initiate
PUBLIC :: DOF_Pointer
PUBLIC :: Deallocate
PUBLIC :: ASSIGNMENT( = )
PUBLIC :: DOF

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
! `spacecompo` is set to -1.
!- For a time independent physical variable `timecompo` is set to 1.
!- The size of `Names`, `spacecompo`, `timecompo` should be same
!
!@note
! 	$\matbf{v}$ is a physical variable, however,
!   its component $v_1, v_2, v_3$ all are degrees of freedom.
!@endnote

INTERFACE
MODULE PURE SUBROUTINE dof_initiate1( obj, tNodes, Names, spacecompo, &
  & timecompo, StorageFMT )
  CLASS( DOF_ ), INTENT( INOUT ) :: obj
    !! degree of freedom object
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : )
    !! number of nodes for each physical variable
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
    !! Names of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: spacecompo( : )
    !! Space component of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: timecompo( : )
    !! Time component of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
    !! Storage format `FMT_DOF`, `FMT_Nodes`
END SUBROUTINE dof_initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE dof_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: Initiate a fortran vector using [[DOF_]] object
!
!# Introduction
!
! This subroutine initiates a fortran vector (rank-1 fortran array ) of
! real using the information stored inside [[DOF_]] object. This subroutine
! gets the size of array from the [[DOF_]] object and then reallocates
! `val` and set its all values to zero.

INTERFACE
MODULE PURE SUBROUTINE dof_initiate2( val, obj )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: val( : )
    !! This vector will be initiated by using obj
  CLASS( DOF_ ), INTENT( IN ) :: obj
    !! DOF object
END SUBROUTINE dof_initiate2
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE dof_initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Initiate two fortran vectors using [[dof_]] object
!
!# Introduction
!
! This subroutine can initiate two fortran vectors (rank-1 fortran arrays)
! using  the information stored inside the [[DOF_]] object

INTERFACE
MODULE PURE SUBROUTINE dof_initiate3( Val1, Val2, obj )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val1( : ), Val2( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
END SUBROUTINE dof_initiate3
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE dof_initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: Initiate an instance of [[DOF_]] by copying other object
!
!# Introduction
!
! This routine copy obj2 into obj1. It also define an assignment operator

INTERFACE
MODULE PURE SUBROUTINE dof_initiate4( obj1, obj2 )
  CLASS( DOF_ ), INTENT( INOUT ) :: obj1
  CLASS( DOF_ ), INTENT( IN ) :: obj2
END SUBROUTINE dof_initiate4
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE dof_initiate4
END INTERFACE ASSIGNMENT( = )

INTERFACE Initiate
  MODULE PROCEDURE dof_initiate4
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                            DOF@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 oct 2021
! summary: 	 Constructor for [[dof_]] object
!
!# Introduction
!
! This function return instance of [[DOF_]]
! This function calls [[DOF_Method:DOF_Initiate1]] method
! for more see [[dof_]]

INTERFACE
MODULE PURE FUNCTION dof_Constructor1( tNodes, Names, spacecompo, timecompo, &
  & StorageFMT ) RESULT( obj )
  TYPE(DOF_) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : ), spacecompo( : ), &
    & timecompo( : ), StorageFMT
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
END FUNCTION dof_Constructor1
END INTERFACE

!> Generic function to construct [[dof_]] object
INTERFACE DOF
  MODULE PROCEDURE dof_Constructor1
END INTERFACE DOF

!----------------------------------------------------------------------------
!                                                     DOF_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns pointer to newly created [[dof_]] object
!
!# Introduction
!
! This function returns the pointer to instance of [[dof_]] object
! for more see [[dof_]]

INTERFACE
MODULE FUNCTION dof_Constructor_1( tNodes, Names, spacecompo, timecompo, &
  & StorageFMT ) RESULT( obj )
  CLASS(DOF_), POINTER :: obj
    !! [[dof_]] object
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : )
    !! total number of nodes for each dof
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
    !! name of each dof
  INTEGER( I4B ), INTENT( IN ) :: spacecompo( : )
    !! space components for each dof
  INTEGER( I4B ), INTENT( IN ) :: timecompo( : )
    !! time component for each dof
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
    !! storage format for dof
END FUNCTION dof_Constructor_1
END INTERFACE

INTERFACE DOF_Pointer
  MODULE PROCEDURE dof_Constructor_1
END INTERFACE DOF_Pointer

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: Oct 10, 2021
! summary: Deallocate data in [[dof_]]
!
!# Introduction
!
! This subroutine deallocates the data in [[DOF_]] object

INTERFACE
MODULE PURE SUBROUTINE dof_Deallocate( obj )
  CLASS(DOF_), INTENT( INOUT ) :: obj
END SUBROUTINE dof_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE dof_Deallocate
END INTERFACE Deallocate
