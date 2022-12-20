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

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_Method) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Display
INTEGER(I4B) :: I
I = INPUT(Option=UnitNo, Default=stdout)
CALL Display(msg, unitNo=I)
CALL Display(obj%csrOwnership, "CSR OWNERSHIP : ")
CALL Display(obj%tDimension, "TOTAL DIMENSION : ")
CALL Display(obj%MatrixProp, "MATRIX PROPERTY : ")
CALL Display(obj=obj%csr, msg="CSR SPARSITY : ", unitNo=I)
IF (ALLOCATED(obj%A)) THEN
  CALL DUMP(1, obj%csr%nrow, .true., obj%A, obj%csr%JA, obj%csr%IA, I)
ELSE
  CALL DUMP(1, obj%csr%nrow, .false., obj%A, obj%csr%JA, obj%csr%IA, I)
END IF
END PROCEDURE csrMat_Display

!----------------------------------------------------------------------------
!                                                                       Spy
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_SPY
SELECT CASE (TRIM(ext))
CASE ("gp", ".gp", ".GP", "GP")
  CALL csrMat_SPY_gnuplot(obj, filename)
CASE ("pdf", ".pdf")
  CALL csrMat_SPY_PLPLOT(obj, filename, ext, "pdf")
CASE ("svg", ".svg")
  CALL csrMat_SPY_PLPLOT(obj, filename, ext, "svg")
CASE ("eps", ".eps")
  CALL csrMat_SPY_PLPLOT(obj, filename, ext, "epscairo")
CASE ("png", ".png")
  CALL csrMat_SPY_PLPLOT(obj, filename, ext, "pngcairo")
CASE ("ps", ".ps")
  CALL csrMat_SPY_PLPLOT(obj, filename, ext, "ps")
CASE DEFAULT
END SELECT
END PROCEDURE csrMat_SPY

!----------------------------------------------------------------------------
!                                                    csrMat_SPY_PLPLOT
!----------------------------------------------------------------------------

SUBROUTINE csrMat_SPY_PLPLOT(obj, filename, ext, driver)
  TYPE(CSRMatrix_), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: filename
  CHARACTER(LEN=*), INTENT(IN) :: ext
  CHARACTER(LEN=*), INTENT(IN) :: driver
#ifdef USE_PLPLOT
  !> Internal
  REAL(DFP), ALLOCATABLE :: X(:), Y(:) !, A( : )!
  REAL(DFP) :: xmin, xmax, ymin, ymax
  INTEGER(I4B) :: ii, jj, kk
  !> main
  CALL Reallocate(X, obj%csr%nnz, Y, obj%csr%nnz)
  kk = 0
  DO ii = 1, obj%csr%nrow
    DO jj = obj%csr%IA(ii), obj%csr%IA(ii + 1) - 1
      kk = kk + 1
      X(kk) = obj%csr%JA(jj)
      Y(kk) = ii
      ! A(kk) = obj%A(jj)
    END DO
  END DO
  !
  xmin = 1 - obj%csr%ncol * 0.1
  xmax = obj%csr%ncol + obj%csr%ncol * 0.1
  ymin = obj%csr%nrow + obj%csr%nrow * 0.1
  ymax = 1 - obj%csr%nrow * 0.1
  !
  CALL PLSDEV(TRIM(driver))
  IF (ext(1:1) .EQ. ".") THEN
    CALL PLSFNAM(TRIM(filename)//TRIM(ext))
  ELSE
    CALL PLSFNAM(TRIM(filename)//"."//TRIM(ext))
  END IF
  !>
  CALL PLSCOLBG(255, 255, 255)
  CALL PLINIT
  CALL PLSCOL0(0, 0, 0, 0)
  CALL PLCOL0(0)
  CALL PLENV(xmin, xmax, ymin, ymax, 1, -1)
  ! CALL PLBOX('bcgnst', 0.0_DFP, 2, 'bcgnstv', 0.0_DFP, 2)
  ! I am removing grids, if you want them then please activate
  ! above line of code, and comment the following line.
  ! I am deactivating the numerical labels
  ! I am deactivating the subticks

  CALL PLBOX('bcx', 0.0_DFP, 2, 'bcx', 0.0_DFP, 2)
  CALL PLLAB("COLUMN", "ROW", "STRUCTURE OF SPARSE MATRIX")
  CALL PLSSYM(0.0_DFP, 0.2_DFP)
  CALL PLCOL0(9)
  CALL PLPOIN(X, Y, 3)
  CALL PLEND
  IF (ALLOCATED(X)) DEALLOCATE (X)
  IF (ALLOCATED(Y)) DEALLOCATE (Y)
  ! IF( ALLOCATED(A) ) DEALLOCATE(A)
#endif
END SUBROUTINE csrMat_SPY_PLPLOT

!----------------------------------------------------------------------------
!                                                        csrMat_SPY_gnuplot
!----------------------------------------------------------------------------

SUBROUTINE csrMat_SPY_gnuplot(obj, filename)
  TYPE(CSRMatrix_), INTENT(IN) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: filename
  ! internal variable
  INTEGER(I4B) :: i, nrow, j, m, ncol, nnz, unitno, a, b, IOSTAT
  CHARACTER(LEN=256) :: scripFile
  LOGICAL(LGT) :: isOpen
  !> main
  OPEN (FILE=TRIM(filename)//".txt", NEWUNIT=unitno, STATUS="REPLACE", &
    & ACTION="WRITE", IOSTAT=IOSTAT)
  !> check
  IF (IOSTAT .NE. 0) THEN
    CALL ErrorMSG(Msg="Error opening "//TRIM(filename)//".txt file",  &
      & File=__FILE__, Routine="csrMat_SPY_gnuplot()",  &
      & LINE=__LINE__)
    STOP
  END IF
  nrow = obj%csr%nrow; ncol = obj%csr%ncol; nnz = obj%csr%nnz
  CALL Display("#m = "//TOSTRING(nrow), unitNo=unitNo)
  CALL Display("#n = "//TOSTRING(ncol), unitNo=unitNo)
  CALL Display("#nnz = "//TOSTRING(nnz), unitNo=unitNo)
  !> write data in txt file
  !> columns are in x direction
  !> rows are in y direction
  DO i = 1, nrow
    DO j = obj%csr%IA(i), obj%csr%IA(i + 1) - 1
      WRITE (unitNo, '(I6, 2X, I6, 2X, G14.6)') &
        & obj%csr%JA(j), i, obj%A(j)
    END DO
  END DO
  CLOSE (unitno)
  !> open gnuplot script file
  OPEN (FILE=trim(filename)//".gp", NEWUNIT=unitno, STATUS="REPLACE", &
    & ACTION="WRITE", IOSTAT=IOSTAT)
  !> check
  IF (IOSTAT .NE. 0) THEN
    CALL ErrorMSG(Msg="Error opening "//TRIM(filename)//".gp file",  &
      & File=__FILE__, Routine="csrMat_SPY_gnuplot()",  &
      & LINE=__LINE__)
    STOP
  END IF
  CALL Display('# Gnuplot script file', unitNo=unitNo)
  CALL Display('# Generated by :: EASIFEM', unitNo=unitNo)
  CALL Display( &
    & "set terminal postscript eps enhance color font 'Helvetica,10'", &
    & unitNo=unitNo)

  CALL Display( &
    & "set output '"//TRIM(filename)//".eps'", &
    & unitNo=unitNo)

  CALL Display( &
    & "set xlabel 'Col(J)'", &
    & unitNo=unitNo)

  CALL Display( &
    & "set ylabel 'Row(I)'", &
    & unitNo=unitNo)

  CALL Display( &
    & "set size ratio -1", &
    & unitNo=unitNo)

  CALL Display( &
    & "set title 'NNZ = "//TRIM(INT2STR(nnz))//"'", &
    & unitNo=unitNo)

  a = 1 - ncol * 0.1
  b = ncol + ncol * 0.1

  CALL Display( &
    & 'set xrange['//TOSTRING(a)//':'  &
    & //TOSTRING(b)//"]", &
    & unitNo=unitNo)

  a = 1 - nrow * 0.1
  b = nrow + nrow * 0.1

  CALL Display( &
    & 'set yrange['//TOSTRING(b)//':'  &
    & //TOSTRING(a)//"]", &
    & unitNo=unitNo)

  WRITE (unitNo, '(A)') 'set mxtics 5'
  WRITE (unitNo, '(A)') 'set mytics 5'
  WRITE (unitNo, '(A)') 'set grid xtics ytics mxtics mytics'
  WRITE (unitNo, "(A)") &
    & "plot"//"'"//TRIM(filename)//".txt"//"' with points pt 7 ps 1.0"
  CLOSE (unitno)
END SUBROUTINE csrMat_SPY_gnuplot

!----------------------------------------------------------------------------
!                                                                 IMPORT
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_IMPORT
INTEGER(I4B) :: unitNo, nrow, ncol, nnz, ii
INTEGER(I4B), ALLOCATABLE :: ROW(:), COL(:), IA(:), JA(:)
REAL(DFP), ALLOCATABLE :: A(:), X(:)
TYPE(String) :: aline
!
OPEN (FILE=filename, NEWUNIT=unitNo, STATUS="OLD", ACTION="READ")
CALL aline%read_line(unit=unitNo)
READ (unitNo, *) nrow, ncol, nnz
ALLOCATE (ROW(nnz), COL(nnz), X(nnz))
DO ii = 1, nnz
  READ (unitNo, *) ROW(ii), COL(ii), X(ii)
END DO
ALLOCATE (IA(nrow + 1), JA(nnz), A(nnz))
CALL COOCSR(nrow, nnz, X, ROW, COL, A, JA, IA)
CALL Initiate(obj=obj, A=A, IA=IA, JA=JA)
DEALLOCATE (ROW, COL, X, IA, JA, A)
CLOSE (unitNo)
END PROCEDURE csrMat_IMPORT
END SUBMODULE IOMethods
