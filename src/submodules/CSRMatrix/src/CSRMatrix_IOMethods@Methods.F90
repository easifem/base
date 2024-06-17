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

SUBMODULE(CSRMatrix_IOMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
INTEGER(I4B) :: I
I = INPUT(Option=UnitNo, Default=stdout)
CALL Display(msg, unitNo=I)
CALL Display(obj%csrOwnership, "CSR OWNERSHIP : ")
CALL Display(obj%tDimension, "TOTAL DIMENSION : ")
CALL Display(obj%MatrixProp, "MATRIX PROPERTY : ")
CALL Display(obj=obj%csr, msg="CSR SPARSITY : ", unitNo=I)
IF (ALLOCATED(obj%A)) THEN
  CALL DUMP(1, obj%csr%nrow, .TRUE., obj%A, obj%csr%JA, obj%csr%IA, I)
ELSE
  CALL DUMP(1, obj%csr%nrow, .FALSE., obj%A, obj%csr%JA, obj%csr%IA, I)
END IF
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                       Spy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SPY
SELECT CASE (TRIM(ext))
CASE ("gp", ".gp", ".GP", "GP")
  CALL obj_SPY_gnuplot(obj, filename)
CASE ("pdf", ".pdf")
  CALL obj_SPY_PLPLOT(obj, filename, ext, "pdf")
CASE ("svg", ".svg")
  CALL obj_SPY_PLPLOT(obj, filename, ext, "svg")
CASE ("eps", ".eps")
  CALL obj_SPY_PLPLOT(obj, filename, ext, "epscairo")
CASE ("png", ".png")
  CALL obj_SPY_PLPLOT(obj, filename, ext, "pngcairo")
CASE ("ps", ".ps")
  CALL obj_SPY_PLPLOT(obj, filename, ext, "ps")
CASE DEFAULT
END SELECT
END PROCEDURE obj_SPY

!----------------------------------------------------------------------------
!                                                    obj_SPY_PLPLOT
!----------------------------------------------------------------------------

SUBROUTINE obj_SPY_PLPLOT(obj, filename, ext, driver)
  TYPE(CSRMatrix_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: filename
  CHARACTER(*), INTENT(IN) :: ext
  CHARACTER(*), INTENT(IN) :: driver
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
END SUBROUTINE obj_SPY_PLPLOT

!----------------------------------------------------------------------------
!                                                        obj_SPY_gnuplot
!----------------------------------------------------------------------------

SUBROUTINE obj_SPY_gnuplot(obj, filename)
  TYPE(CSRMatrix_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: filename
  ! internal variable
  INTEGER(I4B) :: i, nrow, j, m, ncol, nnz, unitno, a, b, IOSTAT
  CHARACTER(256) :: scripFile
  LOGICAL(LGT) :: isOpen
  !> main
  OPEN (FILE=TRIM(filename)//".txt", NEWUNIT=unitno, STATUS="REPLACE", &
    & ACTION="WRITE", IOSTAT=IOSTAT)
  !> check
  IF (IOSTAT .NE. 0) THEN
    CALL ErrorMSG(Msg="Error opening "//TRIM(filename)//".txt file",  &
      & File=__FILE__, Routine="obj_SPY_gnuplot()",  &
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
  OPEN (FILE=TRIM(filename)//".gp", NEWUNIT=unitno, STATUS="REPLACE", &
    & ACTION="WRITE", IOSTAT=IOSTAT)
  !> check
  IF (IOSTAT .NE. 0) THEN
    CALL ErrorMSG(Msg="Error opening "//TRIM(filename)//".gp file",  &
      & File=__FILE__, Routine="obj_SPY_gnuplot()",  &
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
END SUBROUTINE obj_SPY_gnuplot

!----------------------------------------------------------------------------
!                                                                 IMPORT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IMPORT
INTEGER(I4B) :: iostat, unitno, rows, cols, nnz, ii
INTEGER(I4B), ALLOCATABLE :: indx(:), jndx(:), IA(:), JA(:)
REAL(DFP), ALLOCATABLE :: A(:), rval(:)
TYPE(String) :: aline
CHARACTER(1024) :: iomsg
CHARACTER(50) :: rep, field, symm

! Open file
OPEN (FILE=filename, NEWUNIT=unitno, STATUS="OLD", ACTION="READ", &
  & IOSTAT=iostat, iomsg=iomsg)

IF (iostat .NE. 0) THEN
  CALL ErrorMSG(&
    & msg="Error in opening file, following msg = "//TRIM(iomsg), &
    & file=__FILE__, &
    & routine="obj_IMPORT()", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END IF

CALL MMRead(unitno=unitno, rep=rep, field=field, symm=symm, rows=rows, &
  & cols=cols, nnz=nnz, indx=indx, jndx=jndx, rval=rval)

CALL toUpperCase(symm)
IF (symm .EQ. "SYMMETRIC") THEN
  symm = "SYM"
ELSEIF (symm .EQ. "SKEW-SYMMETRIC") THEN
  symm = "SKEWSYM"
ELSE
  symm = "UNSYM"
END IF

ALLOCATE (IA(rows + 1), JA(nnz), A(nnz))

! Call COOCSR from sparsekit
CALL COOCSR(rows, nnz, rval, indx, jndx, A, JA, IA)
CALL Initiate(obj=obj, A=A, IA=IA, JA=JA, MatrixProp=symm)

CLOSE (unitNo)
DEALLOCATE (indx, jndx, rval, IA, JA, A)
END PROCEDURE obj_IMPORT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE deprecated_obj_IMPORT
INTEGER(I4B) :: iostat, unitNo, nrow, ncol, nnz, ii
INTEGER(I4B), ALLOCATABLE :: ROW(:), COL(:), IA(:), JA(:)
REAL(DFP), ALLOCATABLE :: A(:), X(:)
TYPE(String) :: aline
CHARACTER(1024) :: iomsg
!
OPEN (FILE=filename, NEWUNIT=unitNo, STATUS="OLD", ACTION="READ", &
  & IOSTAT=iostat, iomsg=iomsg)
!
IF (iostat .NE. 0) THEN
  CALL ErrorMSG(&
    & msg="Error in opening file, following msg = "//TRIM(iomsg), &
    & file=__FILE__, &
    & routine="obj_IMPORT()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF
!
CALL aline%read_line(unit=unitNo, iostat=iostat, iomsg=iomsg)
!
IF (iostat .NE. 0) THEN
  CALL ErrorMSG(&
    & msg="Error while calling read_line method from String Class, &
    & following msg is returned "//TRIM(iomsg), &
    & file=__FILE__, &
    & routine="obj_IMPORT()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF
!
iostat = 0
READ (unitNo, *, iostat=iostat, iomsg=iomsg) nrow, ncol, nnz
!
IF (iostat .NE. 0) THEN
  CALL ErrorMSG(&
    & msg="Error while reading nrow, ncol, nnz from the given file, &
    & following msg is returned "//TRIM(iomsg), &
    & file=__FILE__, &
    & routine="obj_IMPORT()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF
!
ALLOCATE (ROW(nnz), COL(nnz), X(nnz))
!
iostat = 0
DO ii = 1, nnz
  READ (unitNo, *, iostat=iostat, iomsg=iomsg) ROW(ii), COL(ii), X(ii)
  IF (iostat .NE. 0) EXIT
END DO
!
IF (iostat .NE. 0) THEN
  CALL ErrorMSG(&
    & msg="Error while reading row(ii), col(ii), x(ii) from the given file, &
    & following msg is returned "//TRIM(iomsg), &
    & file=__FILE__, &
    & routine="obj_IMPORT()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF
!
ALLOCATE (IA(nrow + 1), JA(nnz), A(nnz))
!
! Call COOCSR from sparsekit
!
CALL COOCSR(nrow, nnz, X, ROW, COL, A, JA, IA)
!
CALL Initiate(obj=obj, A=A, IA=IA, JA=JA)
!
DEALLOCATE (ROW, COL, X, IA, JA, A)
CLOSE (unitNo)
END PROCEDURE deprecated_obj_IMPORT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods
