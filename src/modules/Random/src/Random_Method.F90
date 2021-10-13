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

MODULE Random_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE initRandom( obj )
  CLASS( Random_ ), INTENT( INOUT ) :: obj
END SUBROUTINE initRandom
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initRandom
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                                 getRandom
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION getRandom( obj, distribution ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: distribution
  REAL( DFP ) :: Ans
END FUNCTION getRandom
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE getRandom
END INTERFACE RandomValue

PUBLIC :: RandomValue

!----------------------------------------------------------------------------
!                                                                 SaveRandom
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE SaveRandom( obj )
  CLASS( Random_ ), INTENT( INOUT ) :: obj
END SUBROUTINE SaveRandom
END INTERFACE

PUBLIC :: SaveRandom

!----------------------------------------------------------------------------
!                                                             UniformRandom
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION uniformRandom( obj, From, To ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: From, To
  REAL( DFP ) :: Ans
END FUNCTION uniformRandom
END INTERFACE

PUBLIC :: uniformRandom

INTERFACE RandomValue
  MODULE PROCEDURE uniformRandom
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                             RandomInteger
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION getRandomInteger( obj, From, To ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) ::  obj
  INTEGER( I4B ), INTENT( IN ) ::  From, To
  INTEGER( I4B ) :: Ans
END FUNCTION getRandomInteger
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE getRandomInteger
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION select_random_int_from_vec( obj, Val ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ) :: Ans
END FUNCTION select_random_int_from_vec
END INTERFACE

INTERFACE
MODULE FUNCTION select_random_int_from_array( obj, Val ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Val( :, : )
  INTEGER( I4B ) :: Ans
END FUNCTION select_random_int_from_array
END INTERFACE

INTERFACE
MODULE FUNCTION select_random_real_from_vec( obj, Val ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION select_random_real_from_vec
END INTERFACE

INTERFACE
MODULE FUNCTION select_random_real_from_array( obj, Val ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  REAL( DFP ) :: Ans
END FUNCTION select_random_real_from_array
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE select_random_int_from_vec, select_random_int_from_array,&
    & select_random_real_from_vec, select_random_real_from_array
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


END MODULE Random_Method