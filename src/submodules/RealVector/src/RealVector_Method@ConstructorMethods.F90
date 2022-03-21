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
! date: 25 Feb 2021
! summary: This module contains constructor methods of [[RealVector_]]

SUBMODULE ( RealVector_Method) ConstructorMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Shape
  IF( ALLOCATED( obj%Val ) ) THEN
    Ans(1) = SIZE( obj%Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE realVec_Shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Size
  IF( ALLOCATED( obj%Val ) ) THEN
    Ans = SIZE( obj%Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE realVec_Size

!----------------------------------------------------------------------------
!                                                         getTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE RealVec_getTotalDimension
  ans = obj%tDimension
END PROCEDURE RealVec_getTotalDimension

!----------------------------------------------------------------------------
!                                                        setTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE RealVec_setTotalDimension
  obj%tDimension = tDimension
END PROCEDURE RealVec_setTotalDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Allocate
  CALL Reallocate( obj%Val, Dims )
  CALL setTotalDimension( obj, 1_I4B )
END PROCEDURE realVec_Allocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Deallocate
  IF( ALLOCATED( obj%Val ) ) DEALLOCATE( obj%Val )
END PROCEDURE realVec_Deallocate

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Initiate1
  CALL Allocate( obj, tSize )
END PROCEDURE realVec_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Initiate2
  INTEGER( I4B ) :: n, i
  n = SIZE( tSize )
  IF( ALLOCATED( obj ) ) THEN
    IF( SIZE( obj ) .NE. n ) THEN
      DEALLOCATE( obj )
      ALLOCATE( obj( n ) )
    END IF
  ELSE
    ALLOCATE( obj( n ) )
  END IF
  DO i = 1, n
    CALL Allocate( obj( i ), tSize( i ) )
  END DO
END PROCEDURE realVec_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Initiate3
  IF( ALLOCATED( obj%Val ) ) DEALLOCATE( obj%Val )
  ALLOCATE( obj%Val( a:b ) )
  obj%Val = 0.0_DFP
  CALL setTotalDimension( obj, 1_I4B )
END PROCEDURE realVec_Initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Initiate4
  CALL Initiate( obj=obj, tSize= (.tNodes. dofobj))
END PROCEDURE realVec_Initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Initiate5
  INTEGER( I4B ) :: ii
  INTEGER( I4B ), ALLOCATABLE :: tsize( : )
  !!
  !! main
  !!
  ASSOCIATE( Map => dofobj%Map )
    !!
    ALLOCATE( tsize( .tDOF. dofobj ) )
    !!
    DO ii = 1, SIZE( Map, 1 ) - 1
      tsize( Map( ii, 5 ) : Map( ii + 1, 5 ) - 1 ) = Map( ii, 6 )
    END DO
    !!
    CALL Initiate( obj=obj, tsize=tsize )
    !!
    DEALLOCATE( tsize )
    !!
  END ASSOCIATE
  !!
END PROCEDURE realVec_Initiate5

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Random_Number1
  CALL Initiate( obj=obj, tSize=tSize )
  CALL RANDOM_NUMBER( obj%Val )
END PROCEDURE realVec_Random_Number1

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Random_Number2
  INTEGER( I4B ) :: ii, n
  n = SIZE( tSize )
  IF( ALLOCATED( obj ) ) THEN
    IF( SIZE( obj ) .NE. n ) THEN
      DEALLOCATE( obj )
      ALLOCATE( obj( n ) )
    END IF
  ELSE
    ALLOCATE( obj( n ) )
  END IF
  DO ii = 1, n
    CALL Initiate( obj=obj(ii), tSize=tSize(ii) )
    CALL RANDOM_NUMBER( obj(ii)%Val )
  END DO
END PROCEDURE realVec_Random_Number2

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor1
  CALL Allocate( obj, tSize )
END PROCEDURE realVec_Constructor1

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor2
  CALL Allocate( obj, SIZE( val ))
  CALL COPY( Y=obj%val, X=REAL(val, DFP) )
END PROCEDURE realVec_Constructor2

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor3
  CALL Allocate( obj, SIZE( val ))
  CALL COPY( Y=obj%val, X=val )
END PROCEDURE realVec_Constructor3

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor_1
  ALLOCATE( obj )
  CALL Allocate( obj, tSize )
END PROCEDURE realVec_Constructor_1

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor_2
  ALLOCATE( obj )
  CALL Allocate( obj, SIZE( val ))
  CALL COPY( Y=obj%val, X=REAL(val, DFP) )
END PROCEDURE realVec_Constructor_2

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor_3
  ALLOCATE( obj )
    CALL Allocate( obj, SIZE( val ))
  CALL COPY( Y=obj%val, X=REAL(val, DFP) )
END PROCEDURE realVec_Constructor_3

END SUBMODULE ConstructorMethods