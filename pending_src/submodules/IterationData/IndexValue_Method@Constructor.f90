SUBMODULE(IndexValue_Method) Constructor

  IMPLICIT NONE
  CONTAINS

!----------------------------------------------------------------------------
!                                                                 Constructor
!----------------------------------------------------------------------------

  MODULE PROCEDURE Constructor1
    Obj % Indx = Indx
    Obj % Val = Val
  END PROCEDURE Constructor1

  MODULE PROCEDURE Constructor2
    INTEGER( I4B ) :: n, i
    n = SIZE( Indx )
    ALLOCATE( Obj( n ) )
    DO i = 1, n
      Obj( i ) % Indx = Indx( i )
      Obj( i ) % Val = Val( i )
    END DO
  END PROCEDURE Constructor2

  MODULE PROCEDURE Constructor3
    INTEGER( I4B ) :: n, i
    n = SIZE( Indx )
    ALLOCATE( Obj( n ) )
    DO i = 1, n
      Obj( i ) % Indx = Indx( i )
      Obj( i ) % Val = Val
    END DO
  END PROCEDURE Constructor3

END SUBMODULE Constructor