SUBMODULE( SparseMatrix_Method ) IO
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

! in this routine we call dump routine from sparsekit lib
MODULE PROCEDURE Display_Obj
  INTEGER( I4B ) :: I, j
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = Stdout
  END IF
  CALL Blanklines( UnitNo = I, NOL = 1 )
  IF( LEN_TRIM( msg ) .NE. 0 ) WRITE( I, "(A)" ) TRIM( msg )
  WRITE( I, "(A, I6, A, I6, A)" ) "Shape :: ( ", Obj % nrow, ", ", Obj % ncol, " )"
  WRITE( I, "(A, I4)") "tDOF :: ", Obj % tDOF
  WRITE( I, "(A, I6)" ) "tNodes :: ", Obj % tNodes
  WRITE( I, "(A, I8)") "NNZ :: ", Obj % nnz
  IF( PRESENT( UnitNo )  ) THEN
    CALL Blanklines( unitno = UnitNo, nol = 1 )
    CALL DUMP (1, Obj % nrow, .true., Obj % A, Obj % JA, Obj % IA, UnitNo )
  ELSE
    ! call dump from sparsekit
    CALL Blanklines( unitno = stdout, nol = 1 )
    CALL DUMP (1, Obj % nrow, .true., Obj % A, Obj % JA, Obj % IA, stdout )
  END IF
END PROCEDURE Display_Obj

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_csr_2
  INTEGER( I4B ) :: I
  LOGICAL( LGT ) :: l

  IF( SIZE( options ) .LT. 4 ) THEN
    I = stdout
  ELSE
    I = options( 4 )
  END IF

  IF( options( 3 ) .eq. 1 ) THEN
    ! print value
    l = .true.
  ELSE
    l = .false.
  END IF

  CALL DUMP( options( 1 ), options( 2 ), l, Obj % A, Obj % JA, Obj % IA, I )

END PROCEDURE display_csr_2

!----------------------------------------------------------------------------
!                                                                       Spy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_spy
  TYPE( File_ ) :: aFile
  INTEGER( I4B ):: i, nrow, j, m, ncol, nnz
  TYPE( String ) :: PFE0( 3 )

  nrow = Obj % nrow
  ncol = Obj % ncol
  nnz = Obj % nnz

  CALL OpenFileToWrite( aFile, PFE )
  WRITE( aFile % UnitNo, "(A, I6)" ) '#m = ', nrow
  WRITE( aFile % UnitNo, "(A, I6)" ) '#n = ', ncol
  WRITE( aFile % UnitNo, "(A, I8)" ) '#nnz = ', nnz

  IF( Values ) THEN
    DO i = 1, nrow
      DO j = Obj % IA( i ), Obj % IA( i + 1 ) - 1
        WRITE( aFile % UnitNo,  '(I6, 2X, I6, 2X, G14.6)') i, &
          & Obj % JA( j ), &
          & Obj % A( j )
      END DO
    END DO
  ELSE
    DO i = 1, nrow
      DO j = Obj % IA( i ), Obj % IA( i + 1 ) - 1
        WRITE( aFile % UnitNo,  '(I6, 2X, I6, 2X, G14.6)') i, &
          & Obj % JA( j ), &
          & 1.0
      END DO
    END DO
  END IF

  CALL CloseFile( aFile )

  IF( TRIM( ScriptLang ) .EQ. 'gnuplot' ) THEN
    PFE0 = PFE
    PFE0( 3 ) = '.gp'
    CALL OpenFileToWrite( aFile, PFE0 )
    WRITE( aFile % UnitNo, '(A)' ) '# Gnuplot script file'
    WRITE( aFile % UnitNo, '(A)' ) '# Author :: Vikas Sharma'
    WRITE( aFile % UnitNo, '(A)' ) &
      & '# From :: SparseMatrix_Method@Constructor.f90>>Spy()'
    WRITE( aFile % UnitNo, '(A)' ) &
      & "set terminal postscript eps enhance color font 'Helvetica,10'"
    WRITE( aFile % UnitNo, '(A)' ) &
      & "set output '"//TRIM(PFE0(1)%Raw)//TRIM( PFE0(2)%Raw) // ".eps'"
    WRITE( aFile % UnitNo, '(A)' ) &
      & "set xlabel 'I'"
    WRITE( aFile % UnitNo, '(A)' ) "set ylabel 'J'"
    WRITE( aFile % UnitNo, '(A)' ) "set size ratio -1"
    WRITE( aFile % UnitNo, '(A)' ) &
      & "set title 'nnz = "//TRIM( INT2STR( nnz ) )// "'"
    WRITE( aFile % UnitNo, '(A)' ) &
      & 'set xrange[1:'//TRIM( INT2STR( nrow ) )//"]"
    WRITE( aFile % UnitNo, '(A)' ) &
      & 'set yrange['//TRIM( INT2STR( ncol ) )//":1]"
    WRITE( aFile % UnitNo, "(A)" ) &
      & "plot" // "'"// TRIM(PFE(1 ) % Raw ) // TRIM( PFE( 2 ) % Raw) &
      & // TRIM( PFE( 3 ) % Raw) // "' with points pt 5 ps 0.5"
    CALL CloseFile( aFile )
  END IF
END PROCEDURE obj_spy

END SUBMODULE IO