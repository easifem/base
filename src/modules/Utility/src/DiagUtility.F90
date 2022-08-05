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

MODULE DiagUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Diag

!----------------------------------------------------------------------------
!                                                           Diag@DiagMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Diag_1( a ) RESULT( Ans )
  INTEGER( Int8 ), INTENT( IN ) :: a(:)
  REAL( DFP ) :: ans( size(a), size(a) )
END FUNCTION Diag_1
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_1
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                           Diag@DiagMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Diag_2( a ) RESULT( Ans )
  INTEGER( Int16 ), INTENT( IN ) :: a(:)
  REAL( DFP ) :: ans( size(a), size(a) )
END FUNCTION Diag_2
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_2
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                           Diag@DiagMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Diag_3( a ) RESULT( Ans )
  INTEGER( Int32 ), INTENT( IN ) :: a(:)
  REAL( DFP ) :: ans( size(a), size(a) )
END FUNCTION Diag_3
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_3
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                           Diag@DiagMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Diag_4( a ) RESULT( Ans )
  INTEGER( Int64 ), INTENT( IN ) :: a(:)
  REAL( DFP ) :: ans( size(a), size(a) )
END FUNCTION Diag_4
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_4
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                           Diag@DiagMethods
!----------------------------------------------------------------------------

#ifdef USE_Int128

INTERFACE
MODULE PURE FUNCTION Diag_5( a ) RESULT( Ans )
  INTEGER( Int64 ), INTENT( IN ) :: a(:)
  REAL( DFP ) :: ans( size(a), size(a) )
END FUNCTION Diag_5
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_5
END INTERFACE Diag

#endif

!----------------------------------------------------------------------------
!                                                           Diag@DiagMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Diag_6( a ) RESULT( Ans )
  REAL( Real32 ), INTENT( IN ) :: a(:)
  REAL( DFP ) :: ans( size(a), size(a) )
END FUNCTION Diag_6
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_6
END INTERFACE Diag

!----------------------------------------------------------------------------
!                                                           Diag@DiagMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Diag_7( a ) RESULT( Ans )
  REAL( Real64 ), INTENT( IN ) :: a(:)
  REAL( DFP ) :: ans( size(a), size(a) )
END FUNCTION Diag_7
END INTERFACE

INTERFACE Diag
  MODULE PROCEDURE Diag_7
END INTERFACE Diag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DiagUtility