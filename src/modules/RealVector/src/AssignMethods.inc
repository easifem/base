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

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign1( lhs, rhs )
  CLASS( RealVector_ ), INTENT( INOUT ) :: lhs
  CLASS( RealVector_ ), INTENT( IN ) :: rhs
END SUBROUTINE realVec_assign1
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign1
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign2( lhs, rhs )
  CLASS( RealVector_ ), INTENT( INOUT ) :: lhs
  CLASS( RealVector_ ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign2
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign2
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign3( lhs, rhs )
  CLASS( RealVector_ ), INTENT( INOUT ) :: lhs
  REAL( DFP ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign3
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign3
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign4( lhs, rhs )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: lhs( : )
  CLASS( RealVector_ ), INTENT( IN ) :: rhs
END SUBROUTINE realVec_assign4
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign4
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign5( lhs, rhs )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: lhs( : )
  CLASS( RealVector_ ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign5
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign5
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign6( lhs, rhs )
  CLASS( RealVector_ ), INTENT( INOUT ) :: lhs
  INTEGER( I4B ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign6
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign6
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign7( lhs, rhs )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: lhs( : )
  CLASS( RealVector_ ), INTENT( IN ) :: rhs
END SUBROUTINE realVec_assign7
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign7
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign8( lhs, rhs )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: lhs( : )
  CLASS( RealVector_ ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign8
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign8
END INTERFACE ASSIGNMENT( = )
