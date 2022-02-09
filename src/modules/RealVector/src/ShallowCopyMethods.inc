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

PUBLIC :: SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY1( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY1
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY1
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY2( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE realVec_SHALLOWCOPY2
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY2
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY3( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY3
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY3
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY4( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY4
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY4
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY5( Y, X )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY5
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY5
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY6( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE realVec_SHALLOWCOPY6
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY6
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY7( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY7
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY7
END INTERFACE SHALLOWCOPY