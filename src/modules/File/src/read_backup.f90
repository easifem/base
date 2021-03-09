

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


!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_a
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a
  ELSE
    READ(unitNo,*,iostat=istat) a
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, &
      & File= __FILE__, Routine="readline_a()", Line= __LINE__ )
  ENDIF

END PROCEDURE readline_a

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_ab
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a, b
  ELSE
    READ(unitNo,*,iostat=istat) a, b
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
    & Routine="readline_ab()", Line=__LINE__ )
  ENDIF

END PROCEDURE readline_ab

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abc
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a, b, c
  ELSE
    READ(unitNo,*,iostat=istat) a, b, c
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
    & Routine="readline_abc()", Line=__LINE__ )
  ENDIF

END PROCEDURE readline_abc

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcd
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a, b, c, d
  ELSE
    READ(unitNo,*,iostat=istat) a, b, c, d
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
    & Routine="readline_abcd()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcd

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcde
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a, b, c, d, e
  ELSE
    READ(unitNo,*,iostat=istat) a, b, c, d, e
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
    & Routine="readline_abcde()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcde

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_av
  INTEGER( I4B ) :: i,Na,istat

  Na=size(a)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) ( a(i),i=1,Na )
  ELSE
    READ( unitNo,*,iostat=istat ) ( a(i),i=1,Na )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_av()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_av

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_avbv
  INTEGER( I4B ) :: i,Na,istat, Nb

  Na=size(a); Nb=size(b)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) ( a(i),i=1,Na ), ( b(i),i=1,Nb )
  ELSE
    READ( unitNo,*,iostat=istat ) ( a(i),i=1,Na ), ( b(i),i=1,Nb )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_avbv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_avbv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_avbvcv
  INTEGER( I4B ) :: i,Na,istat, Nb, Nc

  Na=size(a); Nb=size(b); Nc=size(c)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) ( a(i),i=1,Na ), ( b(i),i=1,Nb ), &
      & ( c(i),i=1,Nc )
  ELSE
    READ( unitNo,*,iostat=istat ) ( a(i),i=1,Na ), ( b(i),i=1,Nb ), &
      & ( c(i),i=1,Nc )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_avbvcv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_avbvcv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abv
  INTEGER( I4B ) :: i,Nb,istat

  Nb=size(b)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, ( b(i),i=1,Nb )
  ELSE
    READ( unitNo,*,iostat=istat ) a, ( b(i),i=1,Nb )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abvcv
  INTEGER( I4B ) :: i,Nb,istat, Nc

  Nb=size(b)
  Nc=size(c)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, ( b(i),i=1,Nb ), ( c(i),i=1,Nc )
  ELSE
    READ( unitNo,*,iostat=istat ) a, ( b(i),i=1,Nb ), ( c(i),i=1,Nc )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abvcv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abvcv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcv
  INTEGER( I4B ) :: i,istat, Nc

  Nc=size(c)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, ( c(i),i=1,Nc )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, ( c(i),i=1,Nc )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcvdv
  INTEGER( I4B ) :: i,istat, Nc, Nd

  Nc=size(c)
  Nd=size(d)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, ( c(i),i=1,Nc ), ( d(i),i=1,Nd )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, ( c(i),i=1,Nc ), ( d(i),i=1,Nd )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcvdv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcvdv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcdv
  INTEGER( I4B ) :: i,istat, Nd

  Nd=size(d)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, c, ( d(i),i=1,Nd )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, c, ( d(i),i=1,Nd )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcdv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcdv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcdvev
  INTEGER( I4B ) :: i,istat, Nd, Ne

  Nd=size(d)
  Ne=size(e)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, c, ( d(i),i=1,Nd ), ( e(i),i=1,Ne )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, c, ( d(i),i=1,Nd ), ( e(i),i=1,Ne )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcdvev()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcdvev

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcdev
  INTEGER( I4B ) :: i,istat, Ne

  Ne=size(e)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, c, d, ( e(i),i=1,Ne )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, c, d, ( e(i),i=1,Ne )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcdev()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcdev

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END INTERFACE ReadLine
