SUBMODULE( File_Method ) ReadMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SkipLines
!----------------------------------------------------------------------------

MODULE PROCEDURE SkipLines
  INTEGER( I4B ) :: ii
  DO ii = 1, nLines
    READ( Obj%UnitNo, * )
  END DO
END PROCEDURE SkipLines

END SUBMODULE ReadMethods