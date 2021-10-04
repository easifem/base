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
! summary: 	This submodule contains get methods of [[RealVector_]]

SUBMODULE ( RealVector_Method) getMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get1
  IF( ALLOCATED( obj%Val ) ) THEN
    ans = obj%Val
  END IF
END PROCEDURE realVec_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get2
  IF( ALLOCATED( obj%Val ) ) THEN
    ans = obj%Val( Indx )
  END IF
END PROCEDURE realVec_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get3
  IF( ALLOCATED( obj%Val ) ) THEN
    ans = obj%Val( iStart:iEnd:Stride )
  END IF
END PROCEDURE realVec_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get4
  IF( ALLOCATED( obj%Val ) ) THEN
    ans = obj
  END IF
END PROCEDURE realVec_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get5
  IF( ALLOCATED( obj%Val ) ) THEN
    CALL Reallocate( ans, SIZE(Indx ) )
    CALL COPY( Y=ans, X=obj%Val(Indx) )
  END IF
END PROCEDURE realVec_get5

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get6
  IF( ALLOCATED( obj%Val ) ) THEN
    ans = obj%Val ( iStart:iEnd:Stride )
  END IF
END PROCEDURE realVec_get6

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get7
  INTEGER( I4B ) :: N, i, tNodes, r1, r2
  N = SIZE( obj )
  tNodes = 0
  DO i = 1, N
    tNodes = tNodes + SIZE( obj( i )%Val )
  END DO
  ALLOCATE( Val( tNodes ) )
  tNodes = 0; r1 = 0; r2 = 0
  DO i = 1, N
    r1 = r2 + 1; r2 = r2 + SIZE( obj( i )%Val )
    Val( r1 : r2 ) = obj( i )%Val
  END DO
END PROCEDURE realVec_get7

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get8
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = SIZE( Indx )
  ALLOCATE( Val( N * M ) )
  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%Val( Indx )
  END DO
END PROCEDURE realVec_get8

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get9
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = 1 + ( iEnd - iStart ) / Stride
  ALLOCATE( Val( M * N ) )
  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%Val( iStart:iEnd:Stride )
  END DO
END PROCEDURE realVec_get9

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get10
  INTEGER( I4B ) :: N, i, tNodes, r1, r2
  N = SIZE( obj )
  tNodes = 0
  DO i = 1, N
    tNodes = tNodes + SIZE( obj( i )%Val )
  END DO
  ALLOCATE( Val( tNodes ) )
  tNodes = 0; r1 = 0; r2 = 0
  DO i = 1, N
    r1 = r2 + 1; r2 = r2 + SIZE( obj( i )%Val )
    Val( r1 : r2 ) = obj( i )%Val
  END DO
END PROCEDURE realVec_get10

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get11
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = SIZE( Indx )
  ALLOCATE( Val( M * N ) )
  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%Val( Indx )
  END DO
END PROCEDURE realVec_get11

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_get12
  INTEGER( I4B ) :: N, i, M
  N = SIZE( obj )
  M = 1 + ( iEnd - iStart ) / Stride
  ALLOCATE( Val( M * N ) )
  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%Val( iStart:iEnd:Stride )
  END DO
END PROCEDURE realVec_get12

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
  val = get(obj=obj, Indx=Indx, dataType=1.0_DFP)
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
  val = get( obj=obj, indx=indx, dataType = 1.0_DFP )
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

MODULE PROCEDURE realVec_get18
  val = obj%val( indx )
END PROCEDURE realVec_get18

!----------------------------------------------------------------------------
!                                                               getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getPointer1
  Val => obj%Val
END PROCEDURE realVec_getPointer1

!----------------------------------------------------------------------------
!                                                               getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getPointer2
  INTEGER( I4B ) :: tdof, aa, ss

  tdof = .tdof. dofobj

  SELECT CASE( dofobj%storageFMT )
  CASE( NODES_FMT )
    aa = dofno
    ! bb = SIZE( obj )
    ss = tdof
    Val => obj%Val(aa::ss)
  CASE( DOF_FMT )
    aa = dofobj%valMap( dofno )
    ss = dofobj%valMap(dofno+1) - 1
    Val => obj%Val(aa:ss)
  END SELECT
END PROCEDURE realVec_getPointer2

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getIndex1
  Ans = MINLOC( ABS( obj%Val - Value ), 1 )
END PROCEDURE realVec_getIndex1

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_getIndex2
  ! Ans = MINLOC( ABS( obj%Val - Value ), 1 )
  INTEGER( I4B ) :: i, j, m
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )
  REAL( DFP ) :: tol0

  tol0 = INPUT( default = REAL(1.0E-10, DFP), option = tol )
  m = SIZE( Value )
  ALLOCATE( Search( m ), Ans( m ) )
  Search = .TRUE.
  Ans = 0
  DO i = 1, SIZE( obj%Val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( ABS(Value(j) - obj%Val(i)) .LE. tol0  ) THEN
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
  DO i = 1, SIZE( obj%Val )
    IF( ABS( obj%Val(i) - Value ) .LE. tol0 ) THEN
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
  DO i = 1, SIZE( obj%Val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( ABS( Value(j) - obj%Val(i) ) .LE. tol0 ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = .TRUE.
        END IF
      END IF
    END DO
  END DO
END PROCEDURE realVec_isPresent2

END SUBMODULE getMethod
