SUBMODULE( File_Method ) Inquire
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 FileSize
!----------------------------------------------------------------------------

MODULE PROCEDURE file_size
  TYPE( string ) :: FileName

  Ans = 0
  IF (.NOT. fileExists(Obj)) RETURN

  FileName = FileName%Join( [Obj%path, Obj%FileName, Obj%Extension])
  INQUIRE(file=TRIM(FileName%chars()), size=Ans )
END PROCEDURE file_size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE fileExists
  INQUIRE( &
    & FILE=TRIM( Obj%Path ) // &
    & TRIM( Obj%FileName ) // &
    & TRIM( Obj%Extension), &
    & EXIST = Ans )
END PROCEDURE fileExists

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hasExtension
  IF( EXTENSION .EQ. Obj%Extension%Raw(2:4) ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE hasExtension

!----------------------------------------------------------------------------
!                                                                 isOpen
!----------------------------------------------------------------------------

MODULE PROCEDURE checkIsOpen
  INQUIRE( &
    & FILE=TRIM( Obj%Path ) // &
    & TRIM( Obj%FileName ) // &
    & TRIM( Obj%Extension), &
    & OPENED = Ans )
END PROCEDURE checkIsOpen
END SUBMODULE Inquire