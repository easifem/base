SUBMODULE(DOF_Method) GetMethod
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tNodes
  IF( ALLOCATED( Obj%Map ) ) THEN
    Ans = Obj%Map( SIZE( Obj%Map, 1 ), 6 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_tNodes

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tNodes_idof
  Ans = 0
  IF( ALLOCATED( Obj%ValMap ) ) THEN
    Ans = Obj%ValMap( idof + 1 ) - Obj%ValMap( idof )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_tNodes_idof

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tDOF
  IF( ALLOCATED( Obj%Map ) ) THEN
    Ans = Obj%Map( SIZE( Obj%Map, 1 ), 4 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_tDOF

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tDOF_iname
  INTEGER( I4B ) :: i, k

  k = ICHAR( Name )
  IF( ALLOCATED( Obj%Map ) ) THEN
    DO i = 1, SIZE( Obj%Map, 1 )
      IF( Obj%Map( i, 1 ) .EQ. k ) Ans = Obj%Map( i, 4 )
    END DO
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_tDOF_iname

!----------------------------------------------------------------------------
!                                                                     tNames
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tNames
  IF( ALLOCATED( Obj%Map ) ) THEN
    Ans = SIZE( Obj%Map, 1 ) - 1
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_tNames

!----------------------------------------------------------------------------
!                                                                     Names
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_all_names
  INTEGER( I4B ) :: ii, n

  n = SIZE( Obj%Map, 1 ) - 1
  ALLOCATE( Ans( n ) )

  DO ii = 1, n
    Ans( ii ) = ACHAR( Obj%Map( ii, 1 ) )
  END DO
END PROCEDURE dof_all_names

!----------------------------------------------------------------------------
!                                                                     Names
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_single_name
  Ans = ACHAR( Obj%Map( ii, 1 ) )
END PROCEDURE dof_single_name

!----------------------------------------------------------------------------
!                                                                 IndexOF
!----------------------------------------------------------------------------

MODULE PROCEDURE get_index_of_name
  INTEGER( I4B ) :: n, i, ic
  n = SIZE( Obj%Map, 1 ) - 1
  ic = ICHAR( Name )
  Ans = 0
  DO i =1, n
    IF( Obj%Map( i, 1 ) .EQ. ic ) THEN
      Ans = i
      EXIT
    END IF
  END DO
END PROCEDURE get_index_of_name

!----------------------------------------------------------------------------
!                                                            tSpaceComponents
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tspace_compo
  INTEGER( I4B ) :: n, i
  n = SIZE( Obj%Map, 1 ) - 1
  Ans = 0
  DO i = 1, n
    IF( Obj%Map( i, 2 ) .GT. 0 ) Ans = Ans + 1
  END DO
END PROCEDURE get_tspace_compo

!----------------------------------------------------------------------------
!                                                            tTimeComponents
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tTime_compo
  INTEGER( I4B ) :: n, i
  n = SIZE( Obj%Map, 1 ) - 1
  Ans = 0
  DO i = 1, n
    IF( Obj%Map( i, 3 ) .GT. 1 ) Ans = Ans + 1
  END DO
END PROCEDURE get_tTime_compo

!----------------------------------------------------------------------------
!                                                             getArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE get_arrayvalues_single_vec
  INTEGER( I4B ) :: m, n, i, k, tdof

  ASSOCIATE( Map => Obj%Map, vm => Obj%ValMap )
    !
    IF( PRESENT( Nptrs ) ) THEN
      m = SIZE( dofno )
      n = SIZE( Nptrs )
      k = m * n
      call reallocate( v, k )

      SELECT CASE( Obj%StorageFMT )

      CASE( dof_FMT )

        ! is storage pattern is different make transformation
        IF( StorageFMT .EQ. nodes_FMT ) THEN

          tdof = .tdof. Obj
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

        tdof = .tdof. Obj
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

      SELECT CASE( Obj%StorageFMT )

      CASE( dof_FMT )

        ! convert if different storage
        IF( StorageFMT .EQ. nodes_FMT ) THEN

          tdof = .tdof. Obj
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

        tdof = .tdof. Obj
        m = SIZE( dofno );

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
    !
  END ASSOCIATE

END PROCEDURE get_arrayvalues_single_vec

!----------------------------------------------------------------------------
!                                                                ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE arrayvalues_single_vec
  IF( PRESENT( Nptrs ) ) THEN
    CALL getArrayValues( v=Ans, Val=Val, Obj=Obj, dofno=dofno, &
      & Nptrs = Nptrs, StorageFMT = StorageFMT )
  ELSE
    CALL getArrayValues( v=Ans, Val=Val, Obj=Obj, dofno=dofno, &
      & StorageFMT = StorageFMT )
  END IF
END PROCEDURE arrayvalues_single_vec

!----------------------------------------------------------------------------
!                                                             getArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE get_arrayvalues_array
  INTEGER( I4B ) :: m, n, i, k, tdof

  ASSOCIATE( Map => Obj%Map, vm => Obj%ValMap )

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

    SELECT CASE( Obj%StorageFMT )
    CASE( dof_FMT )

      tdof = .tdof. Obj
      DO i = 1, m
        n = vm( dofno( i ) + 1 ) - vm( dofno( i )  )
          !! length of dofno( i )
        DO k = 1, n
          v( i, k ) = Val( k + vm( dofno( i ) ) - 1 )
        END DO
      END DO

    CASE( Nodes_FMT )
      tdof = .tdof. Obj
      n = vm( 2 ) - vm( 1 ) ! size of dof; homogenous
      DO i = 1, n
        DO k = 1, m
          v( k, i ) = Val( (i-1)*tdof + dofno( k ) )
        END DO
      END DO
    END SELECT

  END ASSOCIATE

END PROCEDURE get_arrayvalues_array

END SUBMODULE GetMethod