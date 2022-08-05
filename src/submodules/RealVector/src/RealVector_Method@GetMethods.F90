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

!> author: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This submodule contains get methods of [[RealVector_]]

SUBMODULE ( RealVector_Method) getMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getPointer1
  val => obj%val
END PROCEDURE realVec_getPointer1

!----------------------------------------------------------------------------
!                                                               getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getPointer2
  INTEGER( I4B ) :: s(3)
  s = GetNodeLoc( obj=dofobj, idof=idof )
  val => obj%val(s(1):s(2):s(3))
END PROCEDURE realVec_getPointer2

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getIndex1
  Ans = MINLOC( ABS( obj%val - Value ), 1 )
END PROCEDURE realVec_getIndex1

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getIndex2
  ! Ans = MINLOC( ABS( obj%val - Value ), 1 )
  INTEGER( I4B ) :: i, j, m
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )
  REAL( DFP ) :: tol0
  !!
  !!
  !!
  tol0 = INPUT( default = REAL(1.0E-10, DFP), option = tol )
  m = SIZE( Value )
  ALLOCATE( Search( m ), Ans( m ) )
  Search = .TRUE.
  Ans = 0
  DO i = 1, SIZE( obj%val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( ABS(Value(j) - obj%val(i)) .LE. tol0  ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = i
        END IF
      END IF
    END DO
  END DO
END PROCEDURE realVec_getIndex2

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_isPresent1
  INTEGER( I4B ) :: i
  REAL( DFP ) :: tol0
  Ans = .FALSE.
  tol0 = INPUT( default = REAL(1.0E-10, DFP), option = tol )
  DO i = 1, SIZE( obj%val )
    IF( ABS( obj%val(i) - Value ) .LE. tol0 ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE realVec_isPresent1

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_isPresent2
  INTEGER( I4B ) :: i, m, j
  REAL( DFP ) :: tol0
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )

  tol0 = INPUT( default = REAL(1.0E-10, DFP), option = tol )
  m = SIZE( Value )
  ALLOCATE( Ans( m ), Search( m ) )
  Search = .TRUE.
  Ans = .FALSE.
  DO i = 1, SIZE( obj%val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( ABS( Value(j) - obj%val(i) ) .LE. tol0 ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = .TRUE.
        END IF
      END IF
    END DO
  END DO
END PROCEDURE realVec_isPresent2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get1
  IF( ALLOCATED( obj%val ) ) THEN
    ans = obj%val
  ELSE
    ALLOCATE( ans(0) )
  END IF
END PROCEDURE realVec_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get2
  IF( ALLOCATED( obj%val ) ) THEN
    ans = obj%val( nodenum )
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get3
  IF( ALLOCATED( obj%val ) ) THEN
    ans = obj%val( iStart:iEnd:Stride )
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get4a
  IF( ALLOCATED( obj%val ) ) THEN
    ans = obj
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get4a

MODULE PROCEDURE realVec_get4b
  IF( ALLOCATED( obj%val ) ) THEN
    ans = obj
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get4b

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get5a
  IF( ALLOCATED( obj%val ) ) THEN
    CALL Reallocate( ans, SIZE(nodenum ) )
    ans=obj%val(nodenum)
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get5a

MODULE PROCEDURE realVec_get5b
  IF( ALLOCATED( obj%val ) ) THEN
    CALL Reallocate( ans, SIZE(nodenum ) )
    ans=obj%val(nodenum)
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get5b

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get6
  IF( ALLOCATED( obj%val ) ) THEN
    ans = obj%val ( iStart:iEnd:Stride )
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get6

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get7
  INTEGER( I4B ) :: N, i, tNodes, r1, r2
  !!
  N = SIZE( obj )
  tNodes = 0
  !!
  DO i = 1, N
    tNodes = tNodes + SIZE( obj( i ) )
  END DO
  !!
  ALLOCATE( val( tNodes ) )
  !!
  tNodes = 0
  r1 = 0
  r2 = 0
  !!
  DO i = 1, N
    r1 = r2 + 1
    r2 = r2 + SIZE( obj( i ) )
    val( r1 : r2 ) = obj( i )%val
  END DO
  !!
END PROCEDURE realVec_get7

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get8
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = SIZE( nodenum )
  ALLOCATE( val( N * M ) )
  DO i = 1, N
    val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%val( nodenum )
  END DO
END PROCEDURE realVec_get8

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get9
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = 1 + ( iEnd - iStart ) / Stride
  ALLOCATE( val( M * N ) )
  DO i = 1, N
    val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%val( iStart:iEnd:Stride )
  END DO
END PROCEDURE realVec_get9

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get10a
  INTEGER( I4B ) :: N, i, tNodes, r1, r2
  N = SIZE( obj )
  tNodes = 0
  DO i = 1, N
    tNodes = tNodes + SIZE( obj( i )%val )
  END DO
  ALLOCATE( val( tNodes ) )
  tNodes = 0; r1 = 0; r2 = 0
  DO i = 1, N
    r1 = r2 + 1; r2 = r2 + SIZE( obj( i )%val )
    val( r1 : r2 ) = obj( i )%val
  END DO
END PROCEDURE realVec_get10a
MODULE PROCEDURE realVec_get10b
  INTEGER( I4B ) :: N, i, tNodes, r1, r2
  N = SIZE( obj )
  tNodes = 0
  DO i = 1, N
    tNodes = tNodes + SIZE( obj( i )%val )
  END DO
  ALLOCATE( val( tNodes ) )
  tNodes = 0; r1 = 0; r2 = 0
  DO i = 1, N
    r1 = r2 + 1; r2 = r2 + SIZE( obj( i )%val )
    val( r1 : r2 ) = obj( i )%val
  END DO
END PROCEDURE realVec_get10b

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get11a
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = SIZE( nodenum )
  ALLOCATE( val( M * N ) )
  DO i = 1, N
    val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%val( nodenum )
  END DO
END PROCEDURE realVec_get11a

MODULE PROCEDURE realVec_get11b
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = SIZE( nodenum )
  ALLOCATE( val( M * N ) )
  DO i = 1, N
    val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%val( nodenum )
  END DO
END PROCEDURE realVec_get11b

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get12a
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = 1 + ( iEnd - iStart ) / Stride
  ALLOCATE( val( M * N ) )
  DO i = 1, N
    val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%val( iStart:iEnd:Stride )
  END DO
END PROCEDURE realVec_get12a
MODULE PROCEDURE realVec_get12b
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = 1 + ( iEnd - iStart ) / Stride
  ALLOCATE( val( M * N ) )
  DO i = 1, N
    val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%val( iStart:iEnd:Stride )
  END DO
END PROCEDURE realVec_get12b

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get13
  val = get(obj=obj, dataType=1.0_DFP)
END PROCEDURE realVec_get13

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get14
  val = get(obj=obj, nodenum=nodenum, dataType=1.0_DFP)
END PROCEDURE realVec_get14

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get15
  val = get(obj=obj, istart=istart, iend=iend, stride=stride,  &
    & dataType=1.0_DFP)
END PROCEDURE realVec_get15

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get16
  val = get( obj=obj, nodenum=nodenum, dataType = 1.0_DFP )
END PROCEDURE realVec_get16

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get17
  val = get(obj=obj, istart=istart, iend=iend, stride=stride,  &
    & dataType=1.0_DFP)
END PROCEDURE realVec_get17

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get18a
  val = obj%val( nodenum )
END PROCEDURE realVec_get18a

MODULE PROCEDURE realVec_get18b
  val = obj%val( nodenum )
END PROCEDURE realVec_get18b

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get19
  IF( ALLOCATED( obj%val ) ) THEN
    ans = obj
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get19

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get20
  IF( ALLOCATED( obj%val ) ) THEN
    CALL Reallocate( ans, SIZE(nodenum ) )
    CALL COPY( Y=ans, X=obj%val(nodenum) )
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get20

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get21
  IF( ALLOCATED( obj%val ) ) THEN
    ans = obj%val ( iStart:iEnd:Stride )
  ELSE
    ALLOCATE( ans( 0 ) )
  END IF
END PROCEDURE realVec_get21

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get22
  INTEGER( I4B ) :: N, i, tNodes, r1, r2
  N = SIZE( obj )
  tNodes = 0
  DO i = 1, N
    tNodes = tNodes + SIZE( obj( i )%val )
  END DO
  ALLOCATE( val( tNodes ) )
  tNodes = 0; r1 = 0; r2 = 0
  DO i = 1, N
    r1 = r2 + 1; r2 = r2 + SIZE( obj( i )%val )
    val( r1 : r2 ) = obj( i )%val
  END DO
END PROCEDURE realVec_get22

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get23
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = SIZE( nodenum )
  ALLOCATE( val( M * N ) )
  DO i = 1, N
    val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%val( nodenum )
  END DO
END PROCEDURE realVec_get23

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get24
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = 1 + ( iEnd - iStart ) / Stride
  ALLOCATE( val( M * N ) )
  DO i = 1, N
    val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%val( iStart:iEnd:Stride )
  END DO
END PROCEDURE realVec_get24

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get25
  ans = obj%val(getIndex(obj=dofobj, nodenum=nodenum, &
    & ivar=ivar, idof=idof ))
END PROCEDURE realVec_get25

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get26
  ans = obj%val(getIndex(obj=dofobj, nodenum=nodenum, &
    & ivar=ivar, idof=idof ))
END PROCEDURE realVec_get26

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get27
  ans = obj%val(getIndex(obj=dofobj, nodenum=nodenum, &
    & ivar=ivar))
END PROCEDURE realVec_get27

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get28
  !!
  ans = obj%val( &
    & getIndex( &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ) )
  !!
END PROCEDURE realVec_get28

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!!
END SUBMODULE getMethods
