MODULE File_Method
  !! This module contains routine related to file handling
  !! Routines for File_ userdata typehas been defined
  !! Submodules
  !! - Constructor
  !! - WriteMethods
  !! - ReadLine
USE GlobalData
USE BaseType
PRIVATE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This routine intiate the [[file_]] object

!> authors: Dr. Vikas Sharma
! 	This routine initiate the [[file_]] obj
!
MODULE SUBROUTINE init_file( Obj, Path, FileName, Extension, Status, &
  & Action, Access, isBinary, Comment, Separator )
CLASS( File_ ), INTENT( INOUT ) :: Obj
  !! File object
CHARACTER( LEN = * ), INTENT( IN ) :: Path, FileName, Extension, Status, &
  & Action
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
!                                               OpenFileToWrite@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This routine open a file to write

!> authors: Dr. Vikas Sharma
!
! This routine opens a file to write

MODULE SUBROUTINE open_file_write_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path, FileName, Extension
END SUBROUTINE open_file_write_a
END INTERFACE

INTERFACE
!! This routine open a file to write

!> authors: Dr. Vikas Sharma
!
! This routine opens a file to write

MODULE SUBROUTINE open_file_write_b( Obj, PFE )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ) :: PFE( : )
END SUBROUTINE open_file_write_b
END INTERFACE

INTERFACE
!! This routine open a file to write

!> authors: Dr. Vikas Sharma
!
! This routine opens a file to write

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
!                                        OpenBinaryFileToWrite@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This routine open a binary file to write

!> authors: Dr. Vikas Sharma
!
! This routine opens a binary file to write

MODULE SUBROUTINE open_bfile_write_a( Obj, Path, FileName, Extension )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path, FileName, Extension
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
!                                              OpenFileToAppend@Constructor
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

INTERFACE
!! This subroutine deletes the file on the hard-disk

!> authors: Dr. Vikas Sharma
!
! This routine deletes the file on the hard disk
MODULE SUBROUTINE DeleteFile( Obj )
  CLASS( File_ ), INTENT( IN ) :: Obj
END SUBROUTINE DeleteFile
END INTERFACE

PUBLIC :: DeleteFile

!----------------------------------------------------------------------------
!                                                     ReopenFile@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine opens a file to

!> authors: Dr. Vikas Sharma
!
! This subroutine reopens the file

MODULE SUBROUTINE reopen_file( Obj )
  CLASS( File_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE reopen_file
END INTERFACE

INTERFACE ReopenFile
  MODULE PROCEDURE reopen_file
END INTERFACE ReopenFile

PUBLIC :: ReopenFile


!----------------------------------------------------------------------------
!                                                       FileSize@Inquiry
!----------------------------------------------------------------------------

INTERFACE
!! this function returns the file size in bytes

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the file size in bytes

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
!                                                              Exist@Inquiry
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine checks whether filename exists or not

!> authors: Dr. Vikas Sharma
!
! This function checks whether the filename exists or not

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

! !----------------------------------------------------------------------------
! !                                                       Write@WriteMethods
! !----------------------------------------------------------------------------

! INTERFACE
! !! This routine writes data into a file

! !> authors: Dr. Vikas Sharma
! ! This routine writes data into a file
! ! If transpose is true then data is printed as row
! ! If transpose is false then data is printed as column

! MODULE SUBROUTINE write_data_ascii_r1( Obj, Val, transpose )
!   CLASS(File_), INTENT( INOUT) :: Obj
!     !! File object
!   CLASS(*), INTENT( IN ) :: Val(:)
!     !! One D array
!   LOGICAL( LGT ), INTENT( IN ) :: transpose
!     !! Transpose flag
! END SUBROUTINE write_data_ascii_r1
! END INTERFACE

!----------------------------------------------------------------------------
!                                                       Write@WriteMethods
!----------------------------------------------------------------------------

INTERFACE
!! This routine writes data into a file

!> authors: Dr. Vikas Sharma
! This routine writes data into a file
! If row is present then data is printed as row
! If col is present then data is printed as column
! If both row or col are absent then data is printed as row

MODULE SUBROUTINE write_data_ascii_r1( Obj, Val, row, col )
  CLASS(File_), INTENT( INOUT) :: Obj
    !! File object
  CLASS(*), INTENT( IN ) :: Val(:)
    !! One D array
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: row
    !! If present then vector will be printed as rowwise
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: col
    !! If present then vector will be printed as column wise
END SUBROUTINE write_data_ascii_r1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Write@WriteMethods
!----------------------------------------------------------------------------

INTERFACE
!! This routine writes data into a file

!> authors: Dr. Vikas Sharma
! This routine writes data into a file
! If transpose is true then data is printed after taking transpose

MODULE SUBROUTINE write_data_ascii_r2( Obj, Val, transpose )
  CLASS(File_), INTENT( INOUT) :: Obj
  CLASS(*), INTENT( IN ) :: Val(:, :)
  LOGICAL( LGT ), INTENT( IN ) :: transpose
END SUBROUTINE write_data_ascii_r2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Write@WriteMethods
!----------------------------------------------------------------------------

INTERFACE
!! This routine writes data into a file

!> authors: Dr. Vikas Sharma
! This routine writes data into a file
! If transpose is present then data is printed after taking transpose

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
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_a( a, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_ab( a, b, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a, b
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_ab
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_abc( a, b, c, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a, b, c
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_abc
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_abcd( a, b, c, d, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a, b, c, d
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_abcd
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_abcde( a, b, c, d, e, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a, b, c, d, e
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_abcde
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_av( a, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_av
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_avbv( a, b, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a( : ), b( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_avbv
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_avbvcv( a, b, c, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a( : ), b( : ), c( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_avbvcv
END INTERFACE

INTERFACE WriteLine
  MODULE PROCEDURE writeLine_a, writeLine_ab, writeLine_abc, writeLine_abcd,&
    & writeLine_abcde, writeLine_av, writeLine_avbv, writeLine_avbvcv
END INTERFACE WriteLine

PUBLIC :: WriteLine

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_a( a, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_ab( a, b, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_ab
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abc( a, b, c, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  character(len=*), intent(in), optional :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abc
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcd( a, b, c, d, buffer, fileName, unitNo )
  real(DFP), intent(out) :: a, b, c, d
    !! Number
  character(len=*), intent(in) :: fileName
    !! File name
  integer(I4B), intent(in) :: UnitNo
    !! File id number to read from
  character(len=*), intent(in), optional :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcd
END INTERFACE
INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

MODULE SUBROUTINE readline_abcde( a, b, c, d, e, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c, d, e
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  character(len=*), intent(in), optional :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcde
END INTERFACE

INTERFACE ReadLine
  MODULE PROCEDURE readline_a, readline_ab, readline_abc, readline_abcd, &
    & readline_abcde
END INTERFACE ReadLine

PUBLIC :: ReadLine

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_av( a, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_av
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_avbv( a, b, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a( : ), b( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_avbv
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_avbvcv( a, b, c, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a( : ), b( : ), c( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_avbvcv
END INTERFACE

INTERFACE ReadLine
  MODULE PROCEDURE readline_av, readline_avbv, readline_avbvcv
END INTERFACE ReadLine

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abv( a, b, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abv
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abvcv( a, b, c, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b( : ), c( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abvcv
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcv( a, b, c, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcv
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcvdv( a, b, c, d, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c( : ), d( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcvdv
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcdv( a, b, c, d, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c, d( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcdv
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcdvev( a, b, c, d, e, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c, d( : ), e( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcdvev
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Readline@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcdev( a, b, c, d, e, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c, d, e( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcdev
END INTERFACE

INTERFACE ReadLine
  MODULE PROCEDURE readline_abv, readline_abvcv, readline_abcv, &
    & readline_abcvdv, readline_abcdv, readline_abcdvev, readline_abcdev
END INTERFACE ReadLine

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