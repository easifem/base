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

SUBMODULE( RealVector_Method ) setMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_1
  CALL Append( Obj%Val, Value )
END PROCEDURE Append_1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_2
  CALL Append( Obj%Val, Value )
END PROCEDURE Append_2

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_3
  CALL Append( Obj%Val, AnotherObj%Val )
END PROCEDURE Append_3

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_setValue1
  IF( ALLOCATED( Obj%Val ) ) THEN
    IF( SIZE( Value) .EQ. 1 ) THEN
      Obj%Val( Indx ) = Value( 1 )
    ELSE
      Obj%Val( Indx ) = Value
    END IF
  END IF
END PROCEDURE realVec_setValue1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_setValue2
  IF( ALLOCATED( Obj%Val ) ) THEN
      Obj%Val( : ) = Value
  END IF
END PROCEDURE realVec_setValue2

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_setValue3
  IF( ALLOCATED( Obj%Val ) ) THEN
      Obj%Val = Value
  END IF
END PROCEDURE realVec_setValue3

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_setValue4
  IF( ALLOCATED( Obj%Val ) ) THEN
    Obj%Val( istart:iend:stride ) = Value
  END IF
END PROCEDURE realVec_setValue4


!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_setValue5
  IF( ALLOCATED( Obj%Val ) ) THEN
    Obj%Val( istart:iend:stride ) = Value
  END IF
END PROCEDURE realVec_setValue5



END SUBMODULE setMethod