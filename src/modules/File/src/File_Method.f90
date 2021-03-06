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

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary:This module contains routine related to file handling
! Routines for File_ userdata type has been defined
!
!### Introduction
! This module contains routine related to file handling Routines for [[File_]] userdata type has been defined. Following submodules have been declared.
! - Constructor
! - WriteMethods
! - ReadLine
MODULE File_Method
USE GlobalData
USE BaseType
PRIVATE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE init_file( Obj, Path, FileName, Extension, Status, &
  & Action, Access, isBinary, Comment, Separator )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  !! File object
  CHARACTER( LEN = * ), INTENT( IN ) :: Path
  CHARACTER( LEN = * ), INTENT( IN ) :: FileName
  CHARACTER( LEN = * ), INTENT( IN ) :: Extension
  CHARACTER( LEN = * ), INTENT( IN ) :: Status
  CHARACTER( LEN = * ), INTENT( IN ) :: Action
  CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL ::  Access
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isBinary
  !! Flag for binary file
  CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: Comment
  CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: Separator
END SUBROUTINE init_file
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_file
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                        OpenFile@Constuctor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_file_a( Obj )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE open_file_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                      OpenFile@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_file_b( Obj, Path, FileName, Extension, tag )
  CLASS( File_ ), INTENT( INOUT) :: Obj
  TYPE( String ), INTENT( IN ) :: Path
  TYPE( String ), INTENT( IN ) :: FileName
  TYPE( String ), INTENT( IN ) :: Extension
  TYPE( String ), INTENT( IN ) :: tag
END SUBROUTINE open_file_b
END INTERFACE

INTERFACE Open
  MODULE PROCEDURE open_file_a, open_file_b
END INTERFACE Open

PUBLIC :: Open

!----------------------------------------------------------------------------
!                                               OpenFileToWrite@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: This routine opens a file to write

INTERFACE
MODULE SUBROUTINE open_file_write_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: Path
  TYPE( String ), INTENT( IN ) :: FileName
  TYPE( String ), INTENT( IN ) :: Extension
END SUBROUTINE open_file_write_a
END INTERFACE

INTERFACE OpenFileToWrite
  MODULE PROCEDURE open_file_write_a
END INTERFACE OpenFileToWrite

PUBLIC :: OpenFileToWrite

!----------------------------------------------------------------------------
!                                        OpenBinaryFileToWrite@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: This routine opens a file to write

INTERFACE
MODULE SUBROUTINE open_bfile_write_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: Path
  TYPE( String ), INTENT( IN ) :: FileName
  TYPE( String ), INTENT( IN ) :: Extension
END SUBROUTINE open_bfile_write_a
END INTERFACE

INTERFACE OpenBinaryFileToWrite
  MODULE PROCEDURE open_bfile_write_a
END INTERFACE OpenBinaryFileToWrite

PUBLIC :: OpenBinaryFileToWrite

!----------------------------------------------------------------------------
!                                                OpenFileToRead@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_file_Read_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: Path
  TYPE( String ), INTENT( IN ) :: FileName
  TYPE( String ), INTENT( IN ) :: Extension
END SUBROUTINE open_file_Read_a
END INTERFACE

INTERFACE OpenFileToRead
  MODULE PROCEDURE open_file_Read_a
END INTERFACE OpenFileToRead

PUBLIC :: OpenFileToRead

!----------------------------------------------------------------------------
!                                              OpenFileToAppend@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_file_Append_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: Path
  TYPE( String ), INTENT( IN ) :: FileName
  TYPE( String ), INTENT( IN ) :: Extension
END SUBROUTINE open_file_Append_a
END INTERFACE

INTERFACE OpenFileToAppend
  MODULE PROCEDURE open_file_Append_a
END INTERFACE OpenFileToAppend

PUBLIC :: OpenFileToAppend

!----------------------------------------------------------------------------
!                                                      CloseFile@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE close_file( Obj )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE close_file
END INTERFACE

INTERFACE CloseFile
  MODULE PROCEDURE close_file
END INTERFACE CloseFile

PUBLIC :: CloseFile

!----------------------------------------------------------------------------
!                                                     DeleteFile@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March
! summary: This subroutine deletes the file on the hard-disk

INTERFACE
MODULE SUBROUTINE DeleteFile( Obj )
  CLASS( File_ ), INTENT( IN ) :: Obj
END SUBROUTINE DeleteFile
END INTERFACE

PUBLIC :: DeleteFile

!----------------------------------------------------------------------------
!                                                     ReopenFile@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: This subroutine opens a file to

INTERFACE
MODULE SUBROUTINE reopen_file( Obj )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE reopen_file
END INTERFACE

INTERFACE ReopenFile
  MODULE PROCEDURE reopen_file
END INTERFACE ReopenFile

PUBLIC :: Reopen


!----------------------------------------------------------------------------
!                                                       FileSize@Inquiry
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: This function returns the file size in bytes

INTERFACE
MODULE FUNCTION file_size( Obj ) RESULT( Ans )
  CLASS( File_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION file_size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE file_size
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                         TotalLines@Inquiry
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: This function counts total number of lines in [[file_]] object

INTERFACE
MODULE FUNCTION getTotalLines( Obj, nHeader ) RESULT( Ans )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
    !! File object
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nHeader
    !! Skip this number of lines at the top of the file
  INTEGER( I4B ) :: Ans
    !! Total number of lines
END FUNCTION getTotalLines
END INTERFACE

INTERFACE TotalLines
  MODULE PROCEDURE getTotalLines
END INTERFACE TotalLines

PUBLIC :: TotalLines

!----------------------------------------------------------------------------
!                                                              Exist@Inquiry
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: This subroutine checks whether filename exists or not

INTERFACE
MODULE FUNCTION fileExists( Obj ) RESULT( Ans )
  CLASS( File_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
END FUNCTION fileExists
END INTERFACE

INTERFACE Exists
  MODULE PROCEDURE fileExists
END INTERFACE Exists

PUBLIC :: Exists

!----------------------------------------------------------------------------
!                                                       hasExtension@Inquiry
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION hasExtension( Obj, Extension ) RESULT( Ans )
  CLASS( File_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = 3 ), INTENT( IN ) :: Extension
  LOGICAL( LGT ) :: Ans
END FUNCTION
END INTERFACE

PUBLIC :: hasExtension

!----------------------------------------------------------------------------
!                                                            isOpen@Inquiry
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION checkIsOpen( Obj ) RESULT( Ans )
  CLASS( File_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
END FUNCTION checkIsOpen
END INTERFACE

INTERFACE isOpen
  MODULE PROCEDURE checkIsOpen
END INTERFACE isOpen

PUBLIC :: isOpen

END MODULE File_Method