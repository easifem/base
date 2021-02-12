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
!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_a
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a
  ELSE
    READ(unitNo,*,iostat=istat) a
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_a()", Line=__LINE__ )
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

END SUBMODULE ReadMethods