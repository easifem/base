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
! summary: 	This submodule implements set methods of [[IntVector_]]

SUBMODULE( IntVector_Method ) setMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_SetValue_1
  IF( ALLOCATED( Obj%Val ) ) THEN
    IF( SIZE( Value ) .EQ. 1 ) THEN
      Obj%Val( Indx ) = Value( 1 )
    ELSE
      Obj%Val( Indx ) = Value
    END IF
  END IF
END PROCEDURE IntVec_SetValue_1

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_SetValue_2
  IF( ALLOCATED( Obj%Val ) ) THEN
    Obj%Val(Indx) = Value
  END IF
END PROCEDURE IntVec_SetValue_2

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Append_1
  CALL Append( Obj%Val, Value )
END PROCEDURE IntVec_Append_1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Append_2
  CALL Append( Obj%Val, Value )
END PROCEDURE IntVec_Append_2

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Append_3
  CALL Append( Obj%Val, AnotherObj%Val )
END PROCEDURE IntVec_Append_3

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_RemoveDuplicates_1
  ! Define internal variables
  INTEGER( I4B ) :: i,k, j, N
  INTEGER( I4B ), ALLOCATABLE :: Res( : )

  IF( ALLOCATED( Obj%Val )  ) THEN

    N = SIZE ( Obj%Val )
    ALLOCATE( Res( N ) )
    Res = 0
    Res( 1 ) = Obj%Val ( 1 )
    k = 1

    DO i = 2, N
      IF( .NOT. ANY( Res .EQ. Obj%Val( i ) ) ) THEN
        k = k + 1
        Res( k ) = Obj%Val( i )
      END IF
    END DO

    Obj%Val = Res( 1 : k )
    DEALLOCATE( Res )

  END IF

END PROCEDURE IntVec_RemoveDuplicates_1

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_RemoveDuplicates_2
  ! Define internal variables
  INTEGER( I4B ) :: i,k, j, N
  INTEGER( I4B ), ALLOCATABLE :: Res( : )

  IF( ALLOCATED( Obj )  ) THEN

    N = SIZE ( Obj )
    ALLOCATE( Res( N ) )
    Res = 0
    Res( 1 ) = Obj ( 1 )
    k = 1

    DO i = 2, N
      IF( .NOT. ANY( Res .EQ. Obj( i ) ) ) THEN
        k = k + 1
        Res( k ) = Obj( i )
      END IF
    END DO

    Obj = Res( 1 : k )
    DEALLOCATE( Res )

  END IF

END PROCEDURE IntVec_RemoveDuplicates_2

!----------------------------------------------------------------------------
!                                                                     Repeat
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Repeat_1
  INTEGER( I4B ) :: n, i
  n = SIZE( Val )
  Ans( 1 : n ) = Val
  DO i = 1, rtimes-1
    Ans( i * n + 1: ( i + 1 ) * n ) = Val
  END DO
END PROCEDURE IntVec_Repeat_1

!----------------------------------------------------------------------------
!                                                                     Repeat
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Repeat_2
  Ans = Repeat( Obj%Val, rtimes )
END PROCEDURE IntVec_Repeat_2

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_H_CONCAT_1
  INTEGER( I4B ) :: s1, s2
  s1 = SIZe( vec1 )
  s2 = SIZe( vec2 )
  ans( 1:s1 ) = vec1( : )
  ans( s1+1: ) = vec2( : )
END PROCEDURE IntVec_H_CONCAT_1

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_H_CONCAT_2
  INTEGER( I4B ) :: s1, s2
  s1 = SIZE( obj1 )
  s2 = SIZE( obj2 )
  CALL Initiate( ans, s1+s2 )
  ans%val(1:s1) = obj1%val(:)
  ans%val(s1+1:) = obj2%val(:)
END PROCEDURE IntVec_H_CONCAT_2

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_H_CONCAT_3
  INTEGER( I4B ) :: s1, s2
  s1 = SIZE( vec1 )
  s2 = SIZE( obj2 )
  CALL Initiate( ans, s1+s2 )
  ans%val(1:s1) = vec1(:)
  ans%val(s1+1:) = obj2%val(:)
END PROCEDURE IntVec_H_CONCAT_3

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_H_CONCAT_4
  INTEGER( I4B ) :: s1, s2
  s1 = SIZE( obj1 )
  s2 = SIZE( vec2 )
  CALL Initiate( ans, s1+s2 )
  ans%val(1:s1) = obj1%val(:)
  ans%val(s1+1:) = vec2(:)
END PROCEDURE IntVec_H_CONCAT_4

END SUBMODULE setMethod
