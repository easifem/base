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

PUBLIC :: Norm2

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realvec_norm2_1( obj, dof, ivar, idof ) &
  & RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dof
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ) :: ans
END FUNCTION realvec_norm2_1
END INTERFACE

INTERFACE norm2
  MODULE PROCEDURE realvec_norm2_1
END INTERFACE norm2

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realvec_norm2_2( obj, dof, ivar, idof ) &
  & RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dof
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: idof(:)
  REAL( DFP ) :: ans
END FUNCTION realvec_norm2_2
END INTERFACE

INTERFACE norm2
  MODULE PROCEDURE realvec_norm2_2
END INTERFACE norm2

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realvec_norm2_3( obj, dof, idof ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dof
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ) :: ans
END FUNCTION realvec_norm2_3
END INTERFACE

INTERFACE norm2
  MODULE PROCEDURE realvec_norm2_3
END INTERFACE norm2

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realvec_norm2_4( obj, dof, idof) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dof
  INTEGER( I4B ), INTENT( IN ) :: idof(:)
  REAL( DFP ) :: ans
END FUNCTION realvec_norm2_4
END INTERFACE

INTERFACE norm2
  MODULE PROCEDURE realvec_norm2_4
END INTERFACE norm2

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realvec_norm2_5( obj, dof, ivar, &
  & spaceCompo, timeCompo ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dof
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
  REAL( DFP ) :: ans
END FUNCTION realvec_norm2_5
END INTERFACE

INTERFACE norm2
  MODULE PROCEDURE realvec_norm2_5
END INTERFACE norm2

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realvec_norm2_6( obj, dof, ivar, &
  & spaceCompo, timeCompo ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dof
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo(:)
  REAL( DFP ) :: ans
END FUNCTION realvec_norm2_6
END INTERFACE

INTERFACE norm2
  MODULE PROCEDURE realvec_norm2_6
END INTERFACE norm2

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realvec_norm2_7( obj, dof, ivar, &
  & spaceCompo, timeCompo ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dof
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo(:)
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
  REAL( DFP ) :: ans
END FUNCTION realvec_norm2_7
END INTERFACE

INTERFACE norm2
  MODULE PROCEDURE realvec_norm2_7
END INTERFACE norm2
