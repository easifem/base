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
! date: 	25 Feb 2021
! summary: 	This submodule contains set methods of [[RealVector_]]

SUBMODULE(RealVector_Method) setMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Append1
  CALL Append( obj%Val, Value )
END PROCEDURE realVec_Append1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Append2
  CALL Append( obj%Val, Value )
END PROCEDURE realVec_Append2

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Append3
  CALL Append( obj%Val, Anotherobj%Val )
END PROCEDURE realVec_Append3

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set1
  IF( ALLOCATED( obj%Val ) ) THEN
    IF( SIZE( Value) .EQ. 1 ) THEN
      obj%Val( Indx ) = Value( 1 )
    ELSE
      obj%Val( Indx ) = Value
    END IF
  END IF
END PROCEDURE realVec_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set2
  IF( ALLOCATED( obj%Val ) ) THEN
      obj%Val( : ) = Value
  END IF
END PROCEDURE realVec_set2

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set3
  IF( ALLOCATED( obj%Val ) ) THEN
      obj%Val = Value
  END IF
END PROCEDURE realVec_set3

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set4
  IF( ALLOCATED( obj%Val ) ) THEN
    obj%Val( istart:iend:stride ) = Value
  END IF
END PROCEDURE realVec_set4


!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set5
  IF( ALLOCATED( obj%Val ) ) THEN
    obj%Val( istart:iend:stride ) = Value
  END IF
END PROCEDURE realVec_set5



END SUBMODULE setMethod