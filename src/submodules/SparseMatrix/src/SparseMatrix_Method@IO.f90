SUBMODULE( SparseMatrix_Method ) IO
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                      DUMP
!----------------------------------------------------------------------------

SUBROUTINE DUMP (i1,i2,values,a,ja,ia,iout)
  INTEGER( I4B ), INTENT( IN ) :: i1
  INTEGER( I4B ), INTENT( IN ) :: i2
  INTEGER( I4B ), INTENT( IN ) :: ia( : )
  INTEGER( I4B ), INTENT( IN ) :: ja( : )
  INTEGER( I4B ) :: iout
  REAL( DFP ), INTENT( IN ) :: a( : )
  LOGICAL( LGT ) :: values

  ! internal variable
  INTEGER( I4B ) :: maxr, i, k1, k2, k
  CHARACTER( LEN = 400 ) :: fmt100, fmt101, fmt102, fmt200, fmt201, fmt202, fmt203

  fmt100 = "(1h ,34(1h-),' row',i6,1x,34(1h-) )"
  fmt101 = "(' col:',8(i5,6h     : ))"
  fmt102 = "(' val:',8(D9.2,2h :) )"
  fmt200 = "(1h ,30(1h-),' row',i3,1x,30(1h-), 3('  columns :    values  * ') )"
  fmt201 = "(3(1h ,i6,6h   :  ,D9.2,3h * ) )"
  fmt202 = "(6(1h ,i5,6h  *    ) )"
  fmt203 = "format (1h ,30(1h-),' row',i3,1x,30(1h-),3('  column  :  column   *') )"

  ! select mode horizontal or vertical
  maxr = 0
  do i=i1, i2
    maxr = max0( maxr, ia(i+1)-ia(i) )
  end do

  if (maxr .le. 8) then
    !able to do one row acros line
    do i=i1, i2
      write(iout,fmt100) i
      k1=ia(i)
      k2 = ia(i+1)-1
      write (iout,fmt101) (ja(k),k=k1,k2)
      if (values) write (iout,fmt102) (a(k),k=k1,k2)
    end do
  else
  ! unable to one row acros line. do three items at a time
  ! across a line
    do i=i1, i2
      if (values) then
        write(iout,fmt200) i
      else
        write(iout,fmt203) i
      endif
      k1=ia(i)
      k2 = ia(i+1)-1
      if (values) then
        write (iout,fmt201) (ja(k),a(k),k=k1,k2)
      else
        write (iout,fmt202) (ja(k),k=k1,k2)
      endif
    end do
  endif
END SUBROUTINE DUMP

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

! in this routine we call dump routine from sparsekit lib
MODULE PROCEDURE Display_Obj
  INTEGER( I4B ) :: I
  I = INPUT( Option = UnitNo, Default = stdout )
  IF( LEN_TRIM( msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) "#" // TRIM( msg )
  END IF
  WRITE( I, "(A, I6, A, I6, A)" ) "Shape : ( ", Obj%nrow, ", ", Obj%ncol, " )"
  WRITE( I, "(A, I4)") "tDOF : ", Obj%tDOF
  WRITE( I, "(A, I8)") "NNZ : ", Obj%nnz
  CALL Display( Obj%tNodes, 'tNodes : ', UnitNo=I )
  CALL DUMP (1, Obj%nrow, .true., Obj%A, Obj%JA, Obj%IA, I )
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
  CALL DUMP( options( 1 ), options( 2 ), l, Obj%A, Obj%JA, Obj%IA, I )
END PROCEDURE display_csr_2

!----------------------------------------------------------------------------
!                                                                       Spy
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_spy
!   TYPE( File_ ) :: aFile
!   INTEGER( I4B ):: i, nrow, j, m, ncol, nnz
!   TYPE( String ) :: PFE0( 3 )

!   nrow = Obj%nrow
!   ncol = Obj%ncol
!   nnz = Obj%nnz

!   CALL OpenFileToWrite( aFile, Path, File, Extension )
!   WRITE( aFile%UnitNo, "(A, I6)" ) '#m = ', nrow
!   WRITE( aFile%UnitNo, "(A, I6)" ) '#n = ', ncol
!   WRITE( aFile%UnitNo, "(A, I8)" ) '#nnz = ', nnz

!   IF( Values ) THEN
!     DO i = 1, nrow
!       DO j = Obj%IA( i ), Obj%IA( i + 1 ) - 1
!         WRITE( aFile%UnitNo,  '(I6, 2X, I6, 2X, G14.6)') i, &
!           & Obj%JA( j ), &
!           & Obj%A( j )
!       END DO
!     END DO
!   ELSE
!     DO i = 1, nrow
!       DO j = Obj%IA( i ), Obj%IA( i + 1 ) - 1
!         WRITE( aFile%UnitNo,  '(I6, 2X, I6, 2X, G14.6)') i, &
!           & Obj%JA( j ), &
!           & 1.0
!       END DO
!     END DO
!   END IF

!   CALL CloseFile( aFile )

!   IF( TRIM( ScriptLang ) .EQ. 'gnuplot' ) THEN
!     PFE0 = PFE
!     PFE0( 3 ) = '.gp'
!     CALL OpenFileToWrite( aFile, PFE0 )
!     WRITE( aFile%UnitNo, '(A)' ) '# Gnuplot script file'
!     WRITE( aFile%UnitNo, '(A)' ) '# Author :: Vikas Sharma'
!     WRITE( aFile%UnitNo, '(A)' ) &
!       & '# From :: SparseMatrix_Method@Constructor.f90>>Spy()'
!     WRITE( aFile%UnitNo, '(A)' ) &
!       & "set terminal postscript eps enhance color font 'Helvetica,10'"
!     WRITE( aFile%UnitNo, '(A)' ) &
!       & "set output '"//TRIM(PFE0(1)%Raw)//TRIM( PFE0(2)%Raw) // ".eps'"
!     WRITE( aFile%UnitNo, '(A)' ) &
!       & "set xlabel 'I'"
!     WRITE( aFile%UnitNo, '(A)' ) "set ylabel 'J'"
!     WRITE( aFile%UnitNo, '(A)' ) "set size ratio -1"
!     WRITE( aFile%UnitNo, '(A)' ) &
!       & "set title 'nnz = "//TRIM( INT2STR( nnz ) )// "'"
!     WRITE( aFile%UnitNo, '(A)' ) &
!       & 'set xrange[1:'//TRIM( INT2STR( nrow ) )//"]"
!     WRITE( aFile%UnitNo, '(A)' ) &
!       & 'set yrange['//TRIM( INT2STR( ncol ) )//":1]"
!     WRITE( aFile%UnitNo, "(A)" ) &
!       & "plot" // "'"// TRIM(PFE(1 )%Raw ) // TRIM( PFE( 2 )%Raw) &
!       & // TRIM( PFE( 3 )%Raw) // "' with points pt 5 ps 0.5"
!     CALL CloseFile( aFile )
!   END IF
! END PROCEDURE obj_spy

END SUBMODULE IO