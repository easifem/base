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
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_Method) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Display
  INTEGER( I4B ) :: I
  I = INPUT( Option = UnitNo, Default = stdout )
  CALL Display( msg, unitNo = I )
  CALL Display( obj%csrOwnership, "CSR OWNERSHIP : " )
  CALL Display( obj%tDimension, "TOTAL DIMENSION : " )
  CALL Display( obj%MatrixProp, "MATRIX PROPERTY : " )
  IF( ASSOCIATED( obj%csr ) ) THEN
    CALL Display( obj=obj%csr, msg="CSR SPARSITY : ", unitNo=I )
  ELSE
    CALL Display( "CSR SPARSITY PATTERN obj%csr is not associated!" )
  END IF
  IF( ALLOCATED( obj%A ) ) THEN
    CALL DUMP(1, obj%csr%nrow, .true., obj%A, obj%csr%JA, obj%csr%IA, I )
  ELSE
    CALL DUMP(1, obj%csr%nrow, .false., obj%A, obj%csr%JA, obj%csr%IA, I )
  END IF
END PROCEDURE csrMat_Display

!----------------------------------------------------------------------------
!                                                                       Spy
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_SPY
  INTEGER( I4B ):: i, nrow, j, m, ncol, nnz, unitno, a, b
  CHARACTER( LEN = 256 ) :: scripFile
  LOGICAL( LGT ) :: isOpen

  OPEN( FILE=trim(filename)//".txt", NEWUNIT=unitno, STATUS="REPLACE", &
    & ACTION="WRITE" )
  nrow = obj%csr%nrow
  ncol = obj%csr%ncol
  nnz = obj%csr%nnz
  WRITE( unitNo, "(A, I6)" ) '#m = ', nrow
  WRITE( unitNo, "(A, I6)" ) '#n = ', ncol
  WRITE( unitNo, "(A, I8)" ) '#nnz = ', nnz
  DO i = 1, nrow
    DO j = obj%csr%IA( i ), obj%csr%IA( i + 1 ) - 1
      WRITE( unitNo,  '(I6, 2X, I6, 2X, G14.6)') &
        & obj%csr%JA( j ), i, obj%A( j )
    END DO
  END DO
  CLOSE( unitno )
  OPEN( FILE=trim(filename)//".gp", NEWUNIT=unitno, STATUS="REPLACE", &
    & ACTION="WRITE" )
  WRITE( unitNo, '(A)' ) '# Gnuplot script file'
  WRITE( unitNo, '(A)' ) '# Author :: Vikas Sharma'
  WRITE( unitNo, '(A)' ) &
    & '# From :: EASIFEM'
  WRITE( unitNo, '(A)' ) &
    & "set terminal postscript eps enhance color font 'Helvetica,10'"
  WRITE( unitNo, '(A)' ) &
    & "set output '"//TRIM(filename)// ".eps'"
  WRITE( unitNo, '(A)' ) &
    & "set xlabel 'Col(J)'"
  WRITE( unitNo, '(A)' ) "set ylabel 'Row(I)'"
  WRITE( unitNo, '(A)' ) "set size ratio -1"
  WRITE( unitNo, '(A)' ) &
    & "set title 'NNZ = "//TRIM( INT2STR( nnz ) )// "'"
  a = 1 - ncol * 0.1
  b = ncol + ncol * 0.1
  WRITE( unitNo, '(A)' ) &
    & 'set xrange['// TRIM( INT2STR( a ) ) // ':'  &
    & // TRIM( INT2STR( b ) ) // "]"

  a = 1 - nrow * 0.1
  b = nrow + nrow * 0.1
  WRITE( unitNo, '(A)' ) &
    & 'set yrange['// TRIM( INT2STR( b ) ) // ':'  &
    & // TRIM( INT2STR( a ) ) // "]"

  WRITE( unitNo, '(A)' ) 'set mxtics 5'
  WRITE( unitNo, '(A)' ) 'set mytics 5'
  WRITE( unitNo, '(A)' ) 'set grid xtics ytics mxtics mytics'
  WRITE( unitNo, "(A)" ) &
    & "plot" // "'"// TRIM(filename)//".txt" // "' with points pt 7 ps 1.0"
  CLOSE( unitno )
END PROCEDURE csrMat_SPY

!----------------------------------------------------------------------------
!                                                                 IMPORT
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_IMPORT
  INTEGER( I4B ) :: unitNo, nrow, ncol, nnz, ii
  INTEGER( I4B ), ALLOCATABLE :: ROW( : ), COL( : ), IA( : ), JA( : )
  REAL( DFP ), ALLOCATABLE :: A( : ), X( : )
  TYPE( String ) :: aline
  !
  OPEN( FILE=filename, NEWUNIT=unitNo, STATUS="OLD", ACTION="READ" )
  CALL aline%read_line( unit=unitNo )
  READ( unitNo, * ) nrow, ncol, nnz
  ALLOCATE( ROW( nnz ), COL( nnz ), X( nnz ) )
  DO ii = 1, nnz
    READ( unitNo, * ) ROW( ii ), COL( ii ), X( ii )
  END DO
  ALLOCATE( IA( nrow + 1 ), JA( nnz ), A( nnz ) )
  CALL COOCSR( nrow, nnz, X, ROW, COL, A, JA, IA )
  CALL Initiate( obj=obj, A=A, IA=IA, JA=JA )
  DEALLOCATE( ROW, COL, X, IA, JA, A )
  CLOSE( unitNo )
END PROCEDURE csrMat_IMPORT
END SUBMODULE IO