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

MODULE IndexValue_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                    IndexValue@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Constructor1( Indx, Val ) RESULT( obj )
    INTEGER( I4B ), INTENT( IN ) :: Indx
    REAL( DFP ), INTENT( IN ) :: Val
    TYPE(IndexValue_) :: obj
  END FUNCTION Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    IndexValue@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Constructor2( Indx, Val ) RESULT( obj )
    INTEGER( I4B ), INTENT( IN ) :: Indx( : )
    REAL( DFP ), INTENT( IN ) :: Val( : )
    TYPE(IndexValue_), ALLOCATABLE :: obj( : )
  END FUNCTION Constructor2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    IndexValue@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Constructor3( Indx, Val ) RESULT( obj )
    INTEGER( I4B ), INTENT( IN ) :: Indx( : )
    REAL( DFP ), INTENT( IN ) :: Val
    TYPE(IndexValue_), ALLOCATABLE :: obj( : )
  END FUNCTION Constructor3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    IndexValue@Constructor
!----------------------------------------------------------------------------

INTERFACE IndexValue
  MODULE PROCEDURE Constructor1, Constructor2, Constructor3
END INTERFACE IndexValue

PUBLIC :: IndexValue

END MODULE IndexValue_Method