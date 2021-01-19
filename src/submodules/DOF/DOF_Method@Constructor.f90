SUBMODULE(DOF_Method) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_st_dof
  INTEGER( I4B ) :: n, i, k, j

  Obj % StorageFMT = StorageFMT; n = SIZE( Names )

  CALL reallocate( Obj % Map, n + 1, 6 )
  ASSOCIATE( Map => Obj % Map )
    !
    !<- Names in ascii code
    Map( 1:n, 1 ) = ICHAR( Names( 1:n ) )
    Map( 1 + n, 1 ) = 0
    !
    !<- Space components; -1 if scalar component like pressure
    Map( 1:n, 2 ) = SpaceCompo
    Map( 1 + n, 2 ) = 0
    !
    !<- Time component; 1 if time invariant
    Map( 1:n, 3 ) = TimeCompo
    Map( 1 + n, 3 ) = 0
    !
    !<- tDOF for each physical name
    DO i = 1, n
      IF( SpaceCompo( i ) .LT. 0 ) THEN
        Map( i, 4 ) = TimeCompo( i )
      ELSE
        Map( i, 4 ) = TimeCompo( i ) * SpaceCompo( i )
      END IF
    END DO
    Map( n + 1, 4 ) = SUM( Map( 1:n, 4 ) )
    !
    !<- Here we set Indx
    Map( 1, 5 ) = 1
    DO i = 2, n + 1
      Map( i, 5 ) = Map( i - 1, 5 ) + Map( i - 1, 4 )
    END DO
    !
    !<- tNodes
    Map( 1:n, 6 ) = tNodes
    Map( n+1, 6 ) = SUM( Map( 1:n, 6 ) * Map( 1:n, 4 ) )
    !
    !<- ValMap( tDOF + 1, 2 )
    CALL Reallocate( Obj % ValMap, Map( n + 1, 4 ) + 1 )
    Obj % ValMap( 1 ) = 1; k = 1
    DO i = 1, n
      DO j = 1, Map( i, 4 )
        k = k + 1
        Obj % ValMap( k ) = Obj % ValMap( k-1 ) + Map( i, 6 )
      END DO
    END DO
  END ASSOCIATE
END PROCEDURE initiate_st_dof

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_val
  INTEGER( I4B ) :: tNodes
  tNodes = .tNodes. Obj
  CALL Reallocate( Val, tNodes )
END PROCEDURE initiate_val

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_2val
  INTEGER( I4B ) :: tNodes
  tNodes = .tNodes. Obj
  CALL Reallocate( Val1, tNodes, Val2, tNodes )
END PROCEDURE initiate_2val

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_realvector_scalar
  INTEGER( I4B ) :: tNodes
  tNodes = .tNodes. Obj
  CALL Reallocate( Val % Val, tNodes )
END PROCEDURE initiate_realvector_scalar

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_realvector_vector
  INTEGER( I4B ) :: tDOF, i, n
  INTEGER( I4B ), ALLOCATABLE :: tNodes( : )
  !
  ASSOCIATE( Map => Obj % Map )
    tDOF = .tDOF. Obj
    ALLOCATE( tNodes( tDOF ) )
    n = SIZE( Map, 1 )
    DO i = 1, n-1
      tNodes( Map( i, 5 ) : Map( i + 1, 5 ) - 1 ) = Map( i, 6 )
    END DO
    CALL Initiate( Val, tNodes )
    DEALLOCATE( tNodes )
  END ASSOCIATE
END PROCEDURE initiate_realvector_vector

!----------------------------------------------------------------------------
!                                                                        DOF
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Initiate( Obj = Obj, Names = Names, tNodes = tNodes, &
    & SpaceCompo = SpaceCompo, TimeCompo = TimeCompo, StorageFMT = StorageFMT)
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  IF( ALLOCATED( Obj % Map ) ) DEALLOCATE( Obj % Map )
  IF( ALLOCATED( Obj % ValMap ) ) DEALLOCATE( Obj % ValMap )
END PROCEDURE deallocate_data

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj
  INTEGER( I4B ) :: I, n, j
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF

  IF( LEN_TRIM( msg ) .NE. 0 ) THEN
    CALL BlankLines( I, 1 )
    WRITE( I, "(A)" ) TRIM( msg )
  END IF

  IF( ALLOCATED( Obj % Map ) ) THEN
    CALL DashLine( UnitNo = I )
    ASSOCIATE( Map => Obj % Map, ValMap => Obj % ValMap )
      n = SIZE( Map, 1 ) - 1
      CALL BlankLines( I, 1 )
      WRITE( I, "(A, I4 )") "Number of Physical Quantities :: ", n
      DO j = 1, n
        CALL BlankLines( I, 1 )
        WRITE( I, "(A)") "Name :: " // CHAR( Map( j, 1 ) )
        IF( Map( j, 2 ) .LT. 0 ) THEN
          WRITE( I, "(A)") "Space Components :: " // "Scalar"
        ELSE
          WRITE( I, "(A, I4)") "Space Components :: ", Map( j, 2 )
        END IF
        WRITE( I, "(A, I4)") "Time Components :: ", Map( j, 3 )
        WRITE( I, "(A, I6)") "Total Nodes :: ", Map( j, 6 )
      END DO
      SELECT CASE( Obj % StorageFMT )
      CASE( dof_FMT )
        WRITE( I, "(A)") "Storage Format :: DOF"
      CASE( Nodes_FMT )
        WRITE( I, "(A)") "Storage Format :: Nodes"
      END SELECT
      CALL Display( Obj % ValMap, "Value Map :: " )
    END ASSOCIATE
    CALL DashLine( UnitNo = I )
  END IF
END PROCEDURE display_obj

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_display_vec
  INTEGER( I4B ) :: I, j, n, tdof, idof, k

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF

  tdof = .tdof. Obj

  CALL Display( Obj, 'Degree of freedom info=', Unitno = I )

  n = SIZE( Obj % Map, 1 ) - 1

  SELECT CASE( Obj % StorageFMT )
  CASE( FMT_Nodes  )

    DO j = 1, n

      CALL BlankLines( UnitNo = I )
      WRITE( I, "(4X, A)" ) "VAR : "//ACHAR( Obj % Map( j, 1 )  )

      DO idof = Obj % Map( j, 5 ), Obj % Map( j+1, 5 ) - 1
        WRITE( I, "( 6X, A )", ADVANCE="NO" ) "--------------"
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "

      DO idof = Obj % Map( j, 5 ), Obj % Map( j+1, 5 ) - 1
        WRITE( I, "(6X, 4X, A, 4X )", ADVANCE="NO" ) "DOF-"//TRIM( INT2STR( idof ) )
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "

      DO idof = Obj % Map( j, 5 ), Obj % Map( j+1, 5 ) - 1
        WRITE( I, "( 6X, A )", ADVANCE="NO" ) "--------------"
      END DO
      WRITE( I, "(A)", ADVANCE="YES" ) " "

      DO k = 1, Obj % Map( j,  6)
        DO idof = Obj % Map( j, 5 ), Obj % Map( j+1, 5 ) - 1
          WRITE( I, "(I6, 2X, G10.2, 2X )", ADVANCE="NO" ) k, &
            & Vec(  ( k - 1 ) * tdof + idof )
        END DO
        WRITE( I, "(A)", ADVANCE="YES" ) " "
      END DO
    END DO

  CASE( FMT_DOF )
  END SELECT
END PROCEDURE dof_display_vec

END SUBMODULE Constructor