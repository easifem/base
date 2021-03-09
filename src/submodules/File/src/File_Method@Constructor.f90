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

SUBMODULE( File_Method ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_file

  IF( Extension%is_allocated() )THEN
    IF( Extension%Raw(1:1) .NE. "." ) THEN
      Obj%Extension = "." // Extension
    END IF
  END IF
  Obj%Path = Path
  Obj%FileName = FileName
  Obj%Status = Status
  Obj%Action = Action
  IF( PRESENT( Access ) ) THEN
    Obj%Access = Access
  ELSE
    Obj%Access = String( "" )
  END IF
  CALL Open( Obj )
  Obj%WriteNo = 0
  IF( PRESENT( Binary ) ) THEN
    Obj%isBinary = .TRUE.
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
  IF( PRESENT( Delimiter ) ) THEN
    Obj%Delimiter = Delimiter
  ELSE
    Obj%Delimiter = "\n"
  END IF
END PROCEDURE init_file

!----------------------------------------------------------------------------
!                                                                 OpenFile
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_a
  ! Define internal variable
  TYPE( String ) :: FileName
  INTEGER( I4B ) :: tsize, uni, ierr

  ierr = system_mkdir( Obj%Path//"", IANY([R_USR,W_USR,X_USR]))
  FileName = Obj%Path // Obj%FileName // Obj%Extension
  INQUIRE( file = FileName//"",  number = uni )

  ! if uni = -1 then filename is not opened else opened
  IF( uni .GE. 0 ) THEN
    CALL Close( Obj )
  END IF

  tsize = LEN_TRIM( Obj % Access )

  IF( tsize .EQ. 0 ) THEN
    IF( Obj%isBinary ) THEN
      OPEN( &
        & NewUnit = Obj%UnitNo,  &
        & FILE = FileName // "", &
        & STATUS = Obj%Status // "", &
        & ACTION = Obj%Action // "", &
        & IOSTAT = Obj%IOSTAT, &
        & FORM="UNFORMATTED"  )
    ELSE
      OPEN( &
        & NewUnit = Obj%UnitNo,  &
        & FILE = FileName // "", &
        & STATUS = Obj%Status // "", &
        & ACTION = Obj%Action // "", &
        & IOSTAT = Obj%IOSTAT )
    END IF
  ELSE
    IF( Obj%isBinary ) THEN
      OPEN( &
        & NewUnit = Obj%UnitNo,  &
        & FILE = FileName // "", &
        & STATUS = Obj%Status // "", &
        & ACTION = Obj%Action // "", &
        & IOSTAT = Obj%IOSTAT, &
        & FORM="UNFORMATTED", &
        & ACCESS = Obj%ACCESS // "" )
    ELSE
      OPEN( &
        & NewUnit = Obj%UnitNo,  &
        & FILE = FileName // "", &
        & STATUS = Obj%Status // "", &
        & ACTION = Obj%Action // "", &
        & IOSTAT = Obj%IOSTAT, &
        & ACCESS = Obj%ACCESS // "" )
    END IF
  END IF

  IF( Obj%IOSTAT .NE. 0 ) THEN
    Obj % isOpen = .FALSE.
    CALL ErrorMSG( &
      & Msg = "Could not open the file", &
      & File = "File_Method@Constructor.f90", &
      & Routine = "open_file_a()", &
      & Line = __LINE__, &
      & UnitNo = stderr )
    STOP
  ELSE
    Obj % isOpen = .TRUE.
  END IF

END PROCEDURE open_file_a

!----------------------------------------------------------------------------
!                                                                      Open
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_b
  SELECT CASE ( TRIM( tag ) )
  CASE( 'w', 'W' )
    CALL OpenFileToWrite( Obj, Path, FileName, Extension )
  CASE( 'r', 'R' )
    CALL OpenFileToRead( Obj, Path, FileName, Extension )
  CASE( 'a', 'A', 'w+', 'W+' )
    CALL OpenFileToAppend( Obj, Path, FileName, Extension )
  CASE DEFAULT
    CALL ErrorMSG( &
      & Msg = "Could not open the file", &
      & File = "File_Method@Constructor.f90", &
      & Routine = "open_file_a()", &
      & Line = __LINE__, &
      & UnitNo = stderr )
    STOP
  END SELECT
END PROCEDURE open_file_b

!----------------------------------------------------------------------------
!                                                           OpenFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_write_a
  CALL Initiate( Obj, Path, FileName, Extension, STATUS=String("REPLACE"), &
    & ACTION=String("WRITE") )
END PROCEDURE open_file_write_a

!----------------------------------------------------------------------------
!                                                     OpenBinaryFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_bfile_write_a
  CALL Initiate( Obj=Obj, Path=Path, FileName=FileName, &
    & Extension=Extension, STATUS=String("REPLACE"), ACTION=String("WRITE"), &
    & Binary=.TRUE. )
END PROCEDURE open_bfile_write_a

!----------------------------------------------------------------------------
!                                                           OpenFileToRead
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_read_a
  CALL Initiate( Obj, Path, FileName, Extension, &
    & STATUS = String("OLD"), ACTION=String("READ") )
END PROCEDURE open_file_read_a

!----------------------------------------------------------------------------
!                                                           OpenFileToAppend
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_append_a
  TYPE( String ) :: s
  LOGICAL( LGT ) :: Exists

  IF( Extension%is_allocated() )THEN
    IF( Extension%Raw(1:1) .NE. "." ) THEN
      s = Path // FileName // "." // Extension
    ELSE
      s = Path // FileName // Extension
    END IF
  ELSE
    s = Path // FileName
  END IF
  INQUIRE(file=s//"", exist=Exists)
  IF( Exists ) THEN
    CALL Initiate( Obj=Obj, Path=Path, FileName=FileName, &
      & Extension=Extension, Status=String("OLD"), ACTION=String("WRITE"), &
      & ACCESS=String("APPEND") )
  ELSE
    CALL Initiate( Obj=Obj, Path=Path, FileName=FileName, &
      & Extension=Extension, Status=String("NEW"), ACTION=String("WRITE") )
  END IF
END PROCEDURE open_file_append_a

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

  FileName = Obj%Path // Obj%FileName // Obj%Extension
  OPEN( newunit = u, file = FileName//"", status='OLD', iostat=ISTAT)
  if(istat==0) close(u,status='delete')
END PROCEDURE DeleteFile

!----------------------------------------------------------------------------
!                                                                 ReopenFile
!----------------------------------------------------------------------------

MODULE PROCEDURE reopen_file
  CALL Close( Obj )
  CALL Open( Obj )
END PROCEDURE reopen_file

END SUBMODULE Constructor


