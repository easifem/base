SUBMODULE( File_Method ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_file
  Obj%Path = String( trim(Path) )
  Obj%FileName = String( trim(FileName) )
  Obj%Extension = String( trim(Extension) )
  Obj%Status = String( trim(Status) )
  Obj%Action = String( trim(Action) )

  IF( PRESENT( Access ) ) THEN
    Obj%Access = String(trim(Access))
  ELSE
    Obj%Access = String( " " )
  END IF

  CALL OpenFile( Obj )
  Obj%WriteNo = 0

  IF( PRESENT( isBinary ) ) THEN
    Obj%isBinary = isBinary
  ELSE
    Obj%isBinary = .FALSE.
  END IF

  IF( PRESENT( Comment ) ) THEN
    Obj%Comment = Comment
  ELSE
    Obj%Comment = "#"
  END IF

  IF( PRESENT( Separator ) ) THEN
    Obj%Separator = Separator
  ELSE
    Obj%Separator = ","
  END IF

END PROCEDURE init_file

!----------------------------------------------------------------------------
!                                                                 OpenFile
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file
  ! Define internal variable
  TYPE( String ) :: FileName, cmd
  INTEGER( I4B ) :: tsize, uni

  cmd = "mkdir -p "// trim( Obj % Path )
  CALL ExecuteCommand( trim( cmd )//"", "File_Method@Constructor.f90" )
  FileName = FileName % Join ([Obj % Path, Obj % FileName, Obj % Extension])

  INQUIRE( file = FileName % Raw,  number = uni )

  ! if uni = -1 then filename is not opened else opened
  IF( uni .GE. 0 ) THEN
    CALL CloseFile( Obj )
  END IF

  tsize = LEN_TRIM( Obj % Access )

  IF( tsize .EQ. 0 ) THEN
    IF( Obj%isBinary ) THEN
      OPEN( &
        & NewUnit = Obj % UnitNo,  &
        &	FILE = TRIM( FileName % Raw ), &
        &	STATUS = TRIM( Obj % STATUS % Raw ), &
        &	ACTION = TRIM( Obj % ACTION % Raw ), &
        &	IOSTAT = Obj % IOSTAT, &
        & FORM="UNFORMATTED"  )
    ELSE
      OPEN( &
        & NewUnit = Obj % UnitNo,  &
        &	FILE = TRIM( FileName % Raw ), &
        &	STATUS = TRIM( Obj % STATUS % Raw ), &
        &	ACTION = TRIM( Obj % ACTION % Raw ), &
        &	IOSTAT = Obj % IOSTAT  )
    END IF
  ELSE
    IF( Obj%isBinary ) THEN
      OPEN( &
        & NewUnit = Obj % UnitNo,  &
        &	FILE = TRIM( FileName % Raw ), &
        &	STATUS = TRIM( Obj % STATUS % Raw ), &
        &	ACTION = TRIM( Obj % ACTION % Raw ), &
        & ACCESS = TRIM( Obj % Access % Raw ), &
        &	IOSTAT = Obj % IOSTAT, &
        & FORM="UNFORMATTED"  )
    ELSE
      OPEN( &
        & NewUnit = Obj % UnitNo,  &
        &	FILE = TRIM( FileName % Raw ), &
        &	STATUS = TRIM( Obj % STATUS % Raw ), &
        &	ACTION = TRIM( Obj % ACTION % Raw ), &
        & ACCESS = TRIM( Obj % Access % Raw ), &
        &	IOSTAT = Obj % IOSTAT  )
    END IF
  END IF

  IF( Obj % IOSTAT .NE. 0 ) THEN
    Obj % isOpen = .FALSE.
    CALL Display( "ERROR: File_Method@Constructor.f90")
    CALL Display( "       OpenFile()")
    CALL Display( "        Could not open the file")
    CALL Display( Obj%IOSTAT, "        IOSTAT :: ")
    CALL Display( "        Program stoped")
    STOP
  ELSE
    Obj % isOpen = .TRUE.
  END IF

END PROCEDURE open_file

!-----------------------------------------------------------------------------
!                                                                       File
!-----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  Ans % isOpen = .FALSE.
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                           OpenFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_write_a
  CALL Initiate( Obj, Path, FileName, Extension, "REPLACE", "WRITE" )
END PROCEDURE open_file_write_a

!----------------------------------------------------------------------------
!                                                            OpenFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_write_b
  CALL Initiate( Obj, PFE( 1 ) % Raw, PFE( 2 ) % Raw, PFE( 3 ) % Raw , &
    & "REPLACE", "WRITE" )
END PROCEDURE open_file_write_b

!----------------------------------------------------------------------------
!                                                            OpenFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_write_c
  CALL Initiate( Obj, Path % Raw, FileName % Raw, Extension % Raw, &
  & "REPLACE", "WRITE" )
END PROCEDURE open_file_write_c

!----------------------------------------------------------------------------
!                                                     OpenBinaryFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_bfile_write_a
  CALL Initiate( Obj=Obj, Path=Path, FileName=FileName, &
    & Extension=Extension, Status="REPLACE", ACTION="WRITE", &
    & isBinary=.TRUE. )
END PROCEDURE open_bfile_write_a

!----------------------------------------------------------------------------
!                                                           OpenFileToRead
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Read_a
  CALL Initiate( Obj, Path, FileName, Extension, "OLD", "READ" )
END PROCEDURE open_file_Read_a

!----------------------------------------------------------------------------
!                                                            OpenFileToRead
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Read_b
  CALL Initiate( Obj, PFE( 1 ) % Raw, PFE( 2 ) % Raw, PFE( 3 ) % Raw , &
    & "OLD", "READ" )
END PROCEDURE open_file_Read_b

!----------------------------------------------------------------------------
!                                                            OpenFileToRead
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Read_c
  CALL Initiate( Obj, Path % Raw, FileName % Raw, Extension % Raw, &
  & "OLD", "READ" )
END PROCEDURE open_file_Read_c

!----------------------------------------------------------------------------
!                                                           OpenFileToAppend
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Append_a
  TYPE( String ) :: s
  LOGICAL( LGT ) :: isExists

  s = TRIM(Path) // TRIM( FileName ) // TRIM( Extension )
  INQUIRE(file=s%chars(), exist=isExists)
  IF( isExists ) THEN
  CALL Initiate( Obj=Obj, Path=Path, FileName=FileName, Extension=Extension, &
    & Status="OLD", ACTION="WRITE", ACCESS="APPEND" )
  ELSE
    CALL Initiate( Obj=Obj, Path=Path, FileName=FileName, &
      & Extension=Extension, Status="NEW", ACTION="WRITE" )
  END IF
END PROCEDURE open_file_Append_a

!----------------------------------------------------------------------------
!                                                            OpenFileToAppend
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Append_b
  CALL OpenFileToAppend( Obj=Obj, Path=PFE(1)%chars(), &
    & FileName=PFE(2)%Chars(), Extension=PFE(3)%chars() )
END PROCEDURE open_file_Append_b

!----------------------------------------------------------------------------
!                                                            OpenFileToAppend
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Append_c
  CALL OpenFileToAppend( Obj=Obj, Path=Path%chars(), &
    & FileName=FileName%Chars(), Extension=Extension%chars() )
END PROCEDURE open_file_Append_c

!----------------------------------------------------------------------------
!                                                                 CloseFile
!----------------------------------------------------------------------------

MODULE PROCEDURE close_file
  IF( Obj % isOpen ) THEN
    CLOSE( Obj % Unitno )
    Obj % isOpen = .FALSE.
  END IF
END PROCEDURE close_file

!----------------------------------------------------------------------------
!                                                                 DeleteFile
!----------------------------------------------------------------------------

MODULE PROCEDURE DeleteFile
  INTEGER( I4B ) :: u, istat
  TYPE( string ) :: FileName

  FileName = FileName%Join ([Obj % Path, Obj % FileName, Obj % Extension])

  OPEN( newunit = u, file = FileName%chars(), status='old', iostat=istat)
  if(istat==0) close(u,status='delete')
END PROCEDURE DeleteFile
!----------------------------------------------------------------------------
!                                                                 ReopenFile
!----------------------------------------------------------------------------

MODULE PROCEDURE reopen_file
  CALL CloseFile( Obj )
  CALL OpenFile( Obj )
END PROCEDURE reopen_file

END SUBMODULE Constructor




