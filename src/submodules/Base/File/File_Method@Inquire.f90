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
!                                                                 TotalLines
!----------------------------------------------------------------------------

MODULE PROCEDURE getTotalLines
  INTEGER(I4B) :: istat

  CALL CloseFile( Obj )
  CALL OpenFileToRead( Obj, Obj%Path, Obj%FileName, Obj%Extension )

  IF ( PRESENT( nHeader ) ) THEN
    IF ( nHeader > 0 ) THEN
      CALL SkipLines( Obj, nHeader )
    END IF
  ENDIF

  Ans = 0

  READ( Obj%UnitNo, '(a)', iostat=istat )
  DO WHILE( istat .EQ. 0 )
    Ans = Ans + 1
    READ( Obj%UnitNo, '(a)', iostat=istat )
  END DO
  CALL CloseFile( Obj )
END PROCEDURE getTotalLines

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