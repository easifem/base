SUBMODULE( Buffer_Method ) Methods
USE BaseMethod
IMPLICIT NONE

INTEGER( I4B ), PARAMETER :: buff_len = 50

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Append
!----------------------------------------------------------------------------

MODULE PROCEDURE buffer_append_str

  INTEGER( I4B ) :: ii, n
  TYPE( StringPointer_  ), ALLOCATABLE :: temp( : )

  IF( .NOT. ALLOCATED( Obj  % Line ) ) THEN

    ALLOCATE( Obj % Line( buff_len ) )
    Obj % tLine = 1
    ALLOCATE( Obj % Line( 1 ) % Ptr )
    Obj % Line( 1 ) % Ptr = Entry

  ELSE

    n = SIZE( Obj % Line )

    IF( Obj % tLine .EQ. n ) THEN

      ALLOCATE( temp( n ) )

      DO ii = 1, n
        temp( ii ) % ptr => Obj % Line( ii ) % ptr
        Obj % Line( ii ) % ptr => NULL( )
      END DO

      DEALLOCATE( Obj % Line )
      ALLOCATE( Obj % Line( 2 *  n  ) )

      DO ii = 1, n
        Obj % Line( ii ) % ptr => temp( ii ) % ptr
        temp( ii ) % ptr => NULL( )
      END DO

      DEALLOCATE( temp )

    END IF

    Obj % tLine = Obj % tLine + 1
    ALLOCATE( Obj % Line( Obj % tLine ) % Ptr )
    Obj % Line( Obj % tLine ) % Ptr = Entry

  END IF

END PROCEDURE buffer_append_str


END SUBMODULE Methods