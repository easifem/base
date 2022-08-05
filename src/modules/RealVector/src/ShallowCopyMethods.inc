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
!                                            SHALLOWCOPY@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY1a( Y, X )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  REAL( Real32 ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY1a

MODULE PURE SUBROUTINE realVec_SHALLOWCOPY1b( Y, X )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  REAL( Real64 ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY1b

MODULE PURE SUBROUTINE realVec_SHALLOWCOPY1c( Y, X )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  REAL( Real32 ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY1c

MODULE PURE SUBROUTINE realVec_SHALLOWCOPY1d( Y, X )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  REAL( Real64 ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY1d
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY1a, realVec_SHALLOWCOPY1b, &
    & realVec_SHALLOWCOPY1c, realVec_SHALLOWCOPY1d
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                            SHALLOWCOPY@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
!                                            SHALLOWCOPY@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
!                                            SHALLOWCOPY@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
!                                            SHALLOWCOPY@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY5a( Y, X )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( Real32 ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY5a
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY5b( Y, X )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( Real64 ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY5b
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY5a, realVec_SHALLOWCOPY5b
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                            SHALLOWCOPY@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY6a( Y, X )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE realVec_SHALLOWCOPY6a
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY6b( Y, X )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE realVec_SHALLOWCOPY6b
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY6a, realVec_SHALLOWCOPY6b
END INTERFACE SHALLOWCOPY

!----------------------------------------------------------------------------
!                                            SHALLOWCOPY@ShallowCopyMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY7a( Y, X )
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY7a
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY7b( Y, X )
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY7b
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY7a, realVec_SHALLOWCOPY7b
END INTERFACE SHALLOWCOPY