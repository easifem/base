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

SUBMODULE(DOF_Method) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNodes1
  IF( ALLOCATED( obj%Map ) ) THEN
    Ans = obj%Map( SIZE( obj%Map, 1 ), 6 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE dof_tNodes1

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNodes2
  Ans = 0
  IF( ALLOCATED( obj%ValMap ) ) THEN
    Ans = obj%ValMap( idof + 1 ) - obj%ValMap( idof )
  ELSE
    Ans = 0
  END IF
END PROCEDURE dof_tNodes2

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNodes3
  ans = obj .tNodes. NameToIndex( obj, varName )
END PROCEDURE dof_tNodes3

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNodes4
  INTEGER( I4B ) :: ii
  Ans = 0
  DO ii = 1, SIZE( idof )
    Ans = Ans + (obj .tNodes. idof( ii ))
  END DO
END PROCEDURE dof_tNodes4

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tdof1
  IF( ALLOCATED( obj%Map ) ) THEN
    Ans = obj%Map( SIZE( obj%Map, 1 ), 4 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE dof_tdof1

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tdof2
  INTEGER( I4B ) :: i, k
  k = ICHAR( Name )
  IF( ALLOCATED( obj%Map ) ) THEN
    DO i = 1, SIZE( obj%Map, 1 ) - 1
      IF( obj%Map( i, 1 ) .EQ. k ) Ans = obj%Map( i, 4 )
    END DO
  ELSE
    Ans = 0
  END IF
END PROCEDURE dof_tdof2

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tdof3
  INTEGER( I4B ) :: i

  i = SIZE( obj%Map, 1 ) - 1
  IF( ALLOCATED( obj%Map ) .AND. iVar .LE. i ) THEN
    Ans = obj%Map( iVar, 4 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE dof_tdof3

!----------------------------------------------------------------------------
!                                                               getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc1
  IF( obj%storageFMT .EQ. NODES_FMT ) THEN
    ans = (inode-1)*(.tdof. obj) + idof
  ELSE
    ans = obj%valmap(idof) + inode - 1
  END IF
END PROCEDURE dof_getNodeLoc1

!----------------------------------------------------------------------------
!                                                               getNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getNodeLoc2
  IF( obj%storageFMT .EQ. NODES_FMT ) THEN
    ans = [idof, .tnodes. obj, .tdof. obj ]
  ELSE
    ans = [obj%valmap( idof ), obj%valmap(idof+1)-1, 1 ]
  END IF
END PROCEDURE dof_getNodeLoc2

!----------------------------------------------------------------------------
!                                                                     tNames
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tNames
  IF( ALLOCATED( obj%Map ) ) THEN
    Ans = SIZE( obj%Map, 1 ) - 1
  ELSE
    Ans = 0
  END IF
END PROCEDURE dof_tNames

!----------------------------------------------------------------------------
!                                                                     Names
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_names1
  INTEGER( I4B ) :: ii, n

  n = SIZE( obj%Map, 1 ) - 1
  ALLOCATE( Ans( n ) )

  DO ii = 1, n
    Ans( ii ) = ACHAR( obj%Map( ii, 1 ) )
  END DO
END PROCEDURE dof_names1

!----------------------------------------------------------------------------
!                                                                     Names
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_names2
  Ans = ACHAR( obj%Map( ii, 1 ) )
END PROCEDURE dof_names2

!----------------------------------------------------------------------------
!                                                                 IndexOF
!----------------------------------------------------------------------------

MODULE PROCEDURE NameToIndex
  INTEGER( I4B ) :: n, i, ic
  n = SIZE( obj%Map, 1 ) - 1
  ic = ICHAR( Name )
  Ans = 0
  DO i =1, n
    IF( obj%Map( i, 1 ) .EQ. ic ) THEN
      Ans = i
      EXIT
    END IF
  END DO
END PROCEDURE NameToIndex

!----------------------------------------------------------------------------
!                                                                 IndexOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex1
  INTEGER( I4B ) :: jj, ii

  jj = .tDOF. obj
  ALLOCATE( ans( jj ) ); ans = 0

  SELECT CASE( obj%storageFMT )
  CASE( DOF_FMT )
    DO ii = 1, jj
      ans( ii ) = obj%ValMap( ii ) - 1 + nodeNum
    END DO
  CASE( NODES_FMT )
    DO ii = 1, jj
      ans( ii ) = (nodeNum - 1) * jj + ii
    END DO
  END SELECT

END PROCEDURE dof_getIndex1

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex2
  INTEGER( I4B ) :: ii, jj, tdof

  ALLOCATE( ans( obj .tdof. ivar ) )
  jj = 0

  SELECT CASE( obj%storageFMT )
  CASE( DOF_FMT )
    DO ii = obj%map( iVar, 5), obj%map( iVar+1, 5) - 1
      jj = jj + 1
      ans( jj ) = obj%ValMap( ii ) - 1 + nodeNum
    END DO
  CASE( NODES_FMT )
    tdof = .tdof. obj
    DO ii = obj%map( iVar, 5), obj%map( iVar+1, 5) - 1
      jj = jj+1
      ans( jj ) = (nodeNum - 1) * tdof + ii
    END DO
  END SELECT
END PROCEDURE dof_getIndex2

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex3
  INTEGER( I4B ) :: iVar
  iVar = NameToIndex( obj, varName )
  ans = getIndex( obj=obj, iVar=iVar, nodeNum=nodeNum )
END PROCEDURE dof_getIndex3

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex4
  INTEGER( I4B ) :: jj, ii, tdof
  tdof = .tdof. obj
  ALLOCATE( ans( tdof * SIZE( nodeNum ) ) )
  ans = 0
  DO ii = 1, SIZE( nodeNum )
    ans( (ii-1) * tdof + 1 : ii*tdof ) = getIndex( obj=obj, &
      & nodeNum=nodeNum(ii) )
  END DO
END PROCEDURE dof_getIndex4

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex5
  INTEGER( I4B ) :: jj, ii, tdof

  tdof = obj .tdof. iVar
  ALLOCATE( ans( tdof * SIZE( nodeNum ) ) )
  ans = 0
  DO ii = 1, SIZE( nodeNum )
    ans( (ii-1) * tdof + 1 : ii*tdof ) = getIndex( obj=obj, &
    & nodeNum=nodeNum(ii), iVar=iVar )
  END DO
END PROCEDURE dof_getIndex5

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getIndex6
  INTEGER( I4B ) :: iVar
  iVar = NameToIndex( obj, varName )
  ans = getIndex( obj=obj, iVar=iVar, nodeNum=nodeNum )
END PROCEDURE dof_getIndex6

!----------------------------------------------------------------------------
!                                                            tSpaceComponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tSpaceComponents
  INTEGER( I4B ) :: n, i
  n = SIZE( obj%Map, 1 ) - 1
  Ans = 0
  DO i = 1, n
    IF( obj%Map( i, 2 ) .GT. 0 ) Ans = Ans + 1
  END DO
END PROCEDURE dof_tSpaceComponents

!----------------------------------------------------------------------------
!                                                            SpaceComponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_SpaceComponents1
  INTEGER( I4B ) :: n, i
  CALL Reallocate( ans, SIZE( obj%Map, 1 ) - 1 )
  DO i = 1, SIZE(ans)
    Ans( i ) = obj%Map( i, 2 )
  END DO
END PROCEDURE dof_SpaceComponents1

!----------------------------------------------------------------------------
!                                                            SpaceComponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_SpaceComponents2
  ans = obj%Map( ivar, 2 )
END PROCEDURE dof_SpaceComponents2

!----------------------------------------------------------------------------
!                                                            tTimeComponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_tTimeComponents
  INTEGER( I4B ) :: n, i
  n = SIZE( obj%Map, 1 ) - 1
  Ans = 0
  DO i = 1, n
    IF( obj%Map( i, 3 ) .GT. 1 ) Ans = Ans + 1
  END DO
END PROCEDURE dof_tTimeComponents

!----------------------------------------------------------------------------
!                                                            TimeComponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_TimeComponents1
  INTEGER( I4B ) :: n, i
  CALL Reallocate( ans, SIZE( obj%Map, 1 ) - 1 )
  DO i = 1, SIZE(ans)
    Ans( i ) = obj%Map( i, 3 )
  END DO
END PROCEDURE dof_TimeComponents1

!----------------------------------------------------------------------------
!                                                            TimeComponents
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_TimeComponents2
  ans = obj%Map( ivar, 3 )
END PROCEDURE dof_TimeComponents2

!----------------------------------------------------------------------------
!                                                             getArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getValue1
  INTEGER( I4B ) :: m, n, i, k, tdof

  ASSOCIATE( Map => obj%Map, vm => obj%ValMap )
    !
    IF( PRESENT( Nptrs ) ) THEN
      m = SIZE( dofno )
      n = SIZE( Nptrs )
      k = m * n
      CALL Reallocate( v, k )

      SELECT CASE( obj%StorageFMT )

      CASE( DOF_FMT )

        ! If the storage pattern is different make transformation
        IF( StorageFMT .EQ. nodes_FMT ) THEN
          tdof = .tdof. obj
          DO i = 1, m
            DO k = 1, n
              v( ( k-1 ) * m + i ) = Val( Nptrs( k ) + vm( dofno( i ) ) - 1 )
            END DO
          END DO

        ELSE

          DO i = 1, m
            v( ( i-1 ) * n + 1 : i * n ) = Val( Nptrs + vm( dofno( i ) ) - 1 )
          END DO

        END IF

      CASE( Nodes_FMT )

        tdof = .tdof. obj
        IF( StorageFMT .EQ. dof_FMT ) THEN

          DO i = 1, n
            DO k = 1, m
              v( ( k-1 ) * n + i ) = Val( ( Nptrs(i) - 1 ) * tdof + dofno(k))
            END DO
          END DO

        ELSE

          DO i = 1, n
            DO k = 1, m
              v( ( i - 1 ) * m + k ) &
                & = Val( ( Nptrs( i ) - 1 ) * tdof + dofno( k ) )
            END DO
          END DO

        END IF
      END SELECT

    ELSE
      ! get total size to alloc v
      k = 0
      DO i = 1, SIZE( dofno )
        k = k + vm( dofno( i ) + 1 ) - vm( dofno( i ) )
      END DO
      CALL reallocate( v, k )

      SELECT CASE( obj%StorageFMT )

      CASE( dof_FMT )
        ! convert if different storage
        IF( StorageFMT .EQ. nodes_FMT ) THEN
          tdof = .tdof. obj
          m = SIZE( dofno )
          DO i = 1, m
            n = vm( dofno( i ) + 1 ) - vm( dofno( i )  )
            DO k = 1, n
              v( ( k-1 ) * m + i ) = Val( k + vm( dofno( i ) ) - 1 )
            END DO
          END DO
        ELSE
          m = 0; n = 0
          DO i = 1, SIZE( dofno )
            m = n + 1
            n = n + vm( dofno( i ) + 1 ) - vm( dofno( i ) )
            v( m : n ) = &
              & Val( vm( dofno( i ) ) : vm( dofno( i + 1 ) - 1 ) )
          END DO
        END IF

      CASE( Nodes_FMT )
        tdof = .tdof. obj
        m = SIZE( dofno )
        IF( StorageFMT .EQ. dof_FMT ) THEN
          n = vm( 2 ) - vm( 1 )
          DO i = 1, n
            DO k = 1, m
              v( (k-1) * n + i ) = Val( (i-1)*tdof + dofno( k ) )
            END DO
          END DO
        ELSE
          DO i = 1, vm( 2 ) - vm( 1 )
            DO k = 1, m
              v( ( i - 1 ) * m + k ) &
                & = Val( ( i - 1 ) * tdof + dofno( k ) )
            END DO
          END DO
        END IF
      END SELECT
    END IF
  END ASSOCIATE

END PROCEDURE dof_getValue1

!----------------------------------------------------------------------------
!                                                             getArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getValue2
  INTEGER( I4B ) :: m, n, i, k, tdof

  ASSOCIATE( Map => obj%Map, vm => obj%ValMap )

    k = vm( dofno( 1 ) + 1 ) - vm( dofno( 1 ) )
    m = SIZE( dofno )
    DO i = 1, m
      k = MAX( k, vm( dofno( i ) + 1 ) - vm( dofno( i ) ) )
    END DO

    IF( PRESENT( force3D ) .AND. m .LT. 3 ) THEN
      CALL reallocate( v, 3, k )
    ELSE
      CALL reallocate( v, m, k )
    END IF

    SELECT CASE( obj%StorageFMT )
    CASE( dof_FMT )

      tdof = .tdof. obj
      DO i = 1, m
        n = vm( dofno( i ) + 1 ) - vm( dofno( i )  )
          !! length of dofno( i )
        DO k = 1, n
          v( i, k ) = Val( k + vm( dofno( i ) ) - 1 )
        END DO
      END DO

    CASE( Nodes_FMT )
      tdof = .tdof. obj
      n = vm( 2 ) - vm( 1 ) ! size of dof; homogenous
      DO i = 1, n
        DO k = 1, m
          v( k, i ) = Val( (i-1)*tdof + dofno( k ) )
        END DO
      END DO
    END SELECT

  END ASSOCIATE

END PROCEDURE dof_getValue2

!----------------------------------------------------------------------------
!                                                                 getVlaue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getValue3
  CALL getValue(v=v, val=val%val, obj=obj, dofNo=dofNO, &
    & storageFMT=storageFMT, nptrs=nptrs)
END PROCEDURE dof_getValue3

!----------------------------------------------------------------------------
!                                                                 getVlaue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getValue4
  CALL getValue(v=v, val=val%val, obj=obj, dofNo=dofNO, &
    & force3D=force3D )
END PROCEDURE dof_getValue4

!----------------------------------------------------------------------------
!                                                                ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_get1
  CALL getValue( v=Ans, Val=Val, obj=obj, dofno=dofno, &
    & Nptrs = Nptrs, StorageFMT = StorageFMT )
END PROCEDURE dof_get1

!----------------------------------------------------------------------------
!                                                                 EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_isEqual
  ans = .TRUE.
  IF( obj1%storageFMT .NE. obj2%storageFMT ) ans = .FALSE.
  IF( ANY( obj1%map( :, 2: ) .NE. obj2%map( :, 2: ) ) ) ans = .FALSE.
  IF( ANY( obj1%valmap .NE. obj2%valmap ) ) ans = .FALSE.
END PROCEDURE dof_isEqual

!----------------------------------------------------------------------------
!                                                                 NE
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_isNE
  ans = .NOT. ( dof_isEqual( obj1, obj2 ) )
END PROCEDURE dof_isNE

END SUBMODULE GetMethods