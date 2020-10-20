MODULE File_Method
USE GlobalData
USE BaseType
PRIVATE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE init_file( Obj, Path, FileName, Extension, Status, &
  & Action, Access )
CLASS( File_ ), INTENT( INOUT ) :: Obj
CHARACTER( LEN = * ), INTENT( IN ) :: Path, FileName, Extension, Status, &
  & Action
CHARACTER( LEN = * ), INTENT( IN ), OPTIONAL ::  Access
END SUBROUTINE init_file
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_file
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          File@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor1(  ) RESULT( Ans )
  TYPE( File_ ) :: Ans
END FUNCTION Constructor1
END INTERFACE

INTERFACE File
  MODULE PROCEDURE Constructor1
END INTERFACE

PUBLIC :: File

!----------------------------------------------------------------------------
!                                                       File_Pointer@Contains
!----------------------------------------------------------------------------

INTERFACE File_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE

PUBLIC :: File_Pointer

!----------------------------------------------------------------------------
!                                                        OpenFile@Constuctor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_file( Obj )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE open_file
END INTERFACE

INTERFACE OpenFile
  MODULE PROCEDURE open_file
END INTERFACE OpenFile

PUBLIC :: OpenFile

!----------------------------------------------------------------------------
!                                                            OpenFileToWrite
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_file_write_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path, FileName, Extension
END SUBROUTINE open_file_write_a
END INTERFACE

INTERFACE
MODULE SUBROUTINE open_file_write_b( Obj, PFE )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: PFE( : )
END SUBROUTINE open_file_write_b
END INTERFACE

INTERFACE
MODULE SUBROUTINE open_file_write_c( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: Path, FileName, Extension
END SUBROUTINE open_file_write_c
END INTERFACE

INTERFACE OpenFileToWrite
  MODULE PROCEDURE open_file_write_a, open_file_write_b, open_file_write_c
END INTERFACE OpenFileToWrite

PUBLIC :: OpenFileToWrite

!----------------------------------------------------------------------------
!                                                            OpenFileToRead
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_file_Read_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path, FileName, Extension
END SUBROUTINE open_file_Read_a
END INTERFACE

INTERFACE
MODULE SUBROUTINE open_file_Read_b( Obj, PFE )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: PFE( : )
END SUBROUTINE open_file_Read_b
END INTERFACE

INTERFACE
MODULE SUBROUTINE open_file_Read_c( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: Path, FileName, Extension
END SUBROUTINE open_file_Read_c
END INTERFACE

INTERFACE OpenFileToRead
  MODULE PROCEDURE open_file_Read_a, open_file_Read_b, open_file_Read_c
END INTERFACE OpenFileToRead

PUBLIC :: OpenFileToRead

!----------------------------------------------------------------------------
!                                                            OpenFileToAppend
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_file_Append_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path, FileName, Extension
END SUBROUTINE open_file_Append_a
END INTERFACE

INTERFACE
MODULE SUBROUTINE open_file_Append_b( Obj, PFE )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: PFE( : )
END SUBROUTINE open_file_Append_b
END INTERFACE

INTERFACE
MODULE SUBROUTINE open_file_Append_c( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: Path, FileName, Extension
END SUBROUTINE open_file_Append_c
END INTERFACE

INTERFACE OpenFileToAppend
  MODULE PROCEDURE open_file_Append_a, open_file_Append_b, open_file_Append_c
END INTERFACE OpenFileToAppend

PUBLIC :: OpenFileToAppend

!----------------------------------------------------------------------------
!                                                                  CloseFile
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
!                                                                 ReopenFile
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE reopen_file( Obj )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE reopen_file
END INTERFACE

INTERFACE ReopenFile
  MODULE PROCEDURE reopen_file
END INTERFACE ReopenFile

PUBLIC :: ReopenFile

!----------------------------------------------------------------------------
!                                                               WriteMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE write_data_ascii_r1( Obj, Val, transpose )
  CLASS(File_), INTENT( INOUT) :: Obj
  CLASS(*), INTENT( IN ) :: Val(:)
  LOGICAL( LGT ), INTENT( IN ) :: transpose
END SUBROUTINE write_data_ascii_r1
END INTERFACE

INTERFACE
MODULE SUBROUTINE write_data_ascii_r2( Obj, Val, transpose )
  CLASS(File_), INTENT( INOUT) :: Obj
  CLASS(*), INTENT( IN ) :: Val(:, :)
  LOGICAL( LGT ), INTENT( IN ) :: transpose
END SUBROUTINE write_data_ascii_r2
END INTERFACE

INTERFACE
MODULE SUBROUTINE write_data_ascii_scalar( Obj, Val )
  CLASS( File_ ), INTENT( INOUT) :: Obj
  CLASS( * ), INTENT( IN ) :: Val
END SUBROUTINE write_data_ascii_scalar
END INTERFACE

INTERFACE Write
  MODULE PROCEDURE write_data_ascii_r1, write_data_ascii_r2, &
    & write_data_ascii_scalar
END INTERFACE Write

PUBLIC :: Write

!----------------------------------------------------------------------------
!                                                                    Contains
!----------------------------------------------------------------------------
CONTAINS
FUNCTION Constructor_1( ) RESULT( Obj )
! Define intent of dummy variables
CLASS( File_ ), POINTER :: Obj
ALLOCATE( Obj )
Obj % isOpen = .FALSE.
END FUNCTION Constructor_1

END MODULE File_Method