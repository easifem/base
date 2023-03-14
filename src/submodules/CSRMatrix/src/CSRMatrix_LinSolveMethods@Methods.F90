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

#define _x1 IPAR(8)
#define _x2 IPAR(8) + n - 1
#define _y1 IPAR(9)
#define _y2 IPAR(9) + n - 1

SUBMODULE(CSRMatrix_LinSolveMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getLinSolverCodeFromName
SELECT CASE (TRIM(name))
CASE ("SUPERLU") !1
  ans = LIS_SUPERLU
CASE ("CG") !1
  ans = LIS_CG
CASE ("BICG") !2
  ans = LIS_BICG
CASE ("CGS") !3
  ans = LIS_CGS
CASE ("BICGSTAB") !4
  ans = LIS_BICGSTAB
CASE ("BICGSTABL") !5
  ans = LIS_BICGSTABL
CASE ("GPBICG") !6
  ans = LIS_GPBICG
CASE ("TFQMR") !7
  ans = LIS_TFQMR
CASE ("OMN", "FOM", "ORTHOMIN") !8
  ans = LIS_OMN
CASE ("GMRES", "GMR") !9
  ans = LIS_GMRES
CASE ("JACOBI") !10
  ans = LIS_JACOBI
CASE ("GS") !11
  ans = LIS_GS
CASE ("SOR") !12
  ans = LIS_SOR
CASE ("BICGSAFE") !13
  ans = LIS_BICGSAFE
CASE ("CR") !14
  ans = LIS_CR
CASE ("BICR") !15
  ans = LIS_BICR
CASE ("CRS") !16
  ans = LIS_CRS
CASE ("BICRSTAB") !17
  ans = LIS_BICRSTAB
CASE ("GPBICR") !18
  ans = LIS_GPBICR
CASE ("BICRSAFE") !19
  ans = LIS_BICRSAFE
CASE ("FGMRES") !20
  ans = LIS_FGMRES
CASE ("IDRS") !21
  ans = LIS_IDRS
CASE ("IDR1") !22
  ans = LIS_IDR1
CASE ("MINRES") !23
  ans = LIS_MINRES
CASE ("COCG") !24
  ans = LIS_COCG
CASE ("COCR") !25
  ans = LIS_COCR
CASE ("CGNR", "CGN") !26
  ans = LIS_CGNR
CASE ("DBICG") !27
  ans = LIS_DBICG
CASE ("DQGMRES") !28
  ans = LIS_DQGMRES
END SELECT
END PROCEDURE getLinSolverCodeFromName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getLinSolverNameFromCode
SELECT CASE (name)
CASE (LIS_SUPERLU)
  ans = "SUPERLU" !1
CASE (LIS_CG)
  ans = "CG" !1
CASE (LIS_BICG)
  ans = "BICG" !2
CASE (LIS_CGS)
  ans = "CGS" !3
CASE (LIS_BICGSTAB)
  ans = "BICGSTAB" !4
CASE (LIS_BICGSTABL)
  ans = "BICGSTABL" !5
CASE (LIS_GPBICG)
  ans = "GPBICG" !6
CASE (LIS_TFQMR)
  ans = "TFQMR" !7
CASE (LIS_OMN)
  ans = "ORTHOMIN" !8
CASE (LIS_GMRES)
  ans = "GMRES" !9
CASE (LIS_JACOBI)
  ans = "JACOBI" !10
CASE (LIS_GS)
  ans = "GS" !11
CASE (LIS_SOR)
  ans = "SOR" !12
CASE (LIS_BICGSAFE)
  ans = "BICGSAFE" !13
CASE (LIS_CR)
  ans = "CR" !14
CASE (LIS_BICR)
  ans = "BICR" !15
CASE (LIS_CRS)
  ans = "CRS" !16
CASE (LIS_BICRSTAB)
  ans = "BICRSTAB" !17
CASE (LIS_GPBICR)
  ans = "GPBICR" !18
CASE (LIS_BICRSAFE)
  ans = "BICRSAFE" !19
CASE (LIS_FGMRES)
  ans = "FGMRES" !20
CASE (LIS_IDRS)
  ans = "IDRS" !21
CASE (LIS_IDR1)
  ans = "IDR1" !22
CASE (LIS_MINRES)
  ans = "MINRES" !23
CASE (LIS_COCG)
  ans = "COCG" !24
CASE (LIS_COCR)
  ans = "COCR" !25
CASE (LIS_CGNR)
  ans = "CGNR" !26
CASE (LIS_DBICG)
  ans = "DBICG" !27
CASE (LIS_DQGMRES)
  ans = "DQGMRES" !28
END SELECT
END PROCEDURE getLinSolverNameFromCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE setPreconditionOption(IPAR, PRECOND_TYPE)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: PRECOND_TYPE

  SELECT CASE (PRECOND_TYPE)
  CASE (NO_PRECONDITION)
    IPAR(2) = 0
  CASE (LEFT_PRECONDITION)
    IPAR(2) = 1
  CASE (RIGHT_PRECONDITION)
    IPAR(2) = 2
  CASE (LEFT_RIGHT_PRECONDITION)
    IPAR(2) = 3
  END SELECT
END SUBROUTINE setPreconditionOption

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE setKrylovSubspaceSize(IPAR, m)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: m
  IPAR(5) = INPUT(default=15, option=m)
END SUBROUTINE setKrylovSubspaceSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE setMaxIter(IPAR, maxIter)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: maxIter
  IPAR(6) = maxIter
END SUBROUTINE setMaxIter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE setConvergenceType(IPAR, convergenceIn, convergenceType, &
  & relativeToRHS)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: convergenceIn
  INTEGER(I4B), INTENT(IN) :: convergenceType
  LOGICAL(LGT), INTENT(IN) :: relativeToRHS

  IPAR(3) = 1
  SELECT CASE (convergenceType)

  CASE (absoluteConvergence)
    IF (convergenceIn .EQ. convergenceInSol) THEN
      IPAR(3) = -1
    ELSE IF (convergenceIn .EQ. convergenceInRes) THEN
      IPAR(3) = 1
    END IF

  CASE (relativeConvergence)

    IF (convergenceIn .EQ. convergenceInSol) THEN
      IF (relativeToRHS) THEN
        IPAR(3) = -2
      ELSE
        IPAR(3) = -1
      END IF

    ELSE IF (convergenceIn .EQ. convergenceInRes) THEN
      IF (relativeToRHS) THEN
        IPAR(3) = 2
      ELSE
        IPAR(3) = 1
      END IF
    END IF

  END SELECT
END SUBROUTINE setConvergenceType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE PERFORM_TASK(Amat, y, x, ierr, myName)
  ! intent of dummy variables
  CLASS(CSRMatrix_), INTENT(INOUT) :: Amat
  REAL(DFP), INTENT(INOUT) :: y(:)
  REAL(DFP), INTENT(IN) :: x(:)
  INTEGER(I4B), INTENT(IN) :: ierr
  CHARACTER(*), INTENT(IN) :: myName

  SELECT CASE (ierr)
  CASE (1)
    ! MatVec, y=Ax
    CALL Matvec(obj=Amat, y=y, x=x, isTranspose=.FALSE.)
  CASE (2)
    ! Transposed MatVec
    CALL Matvec(obj=Amat, y=y, x=x, isTranspose=.TRUE.)
  CASE (3, 5)
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    ! CALL Amat%ILUSOLVE(sol=y, rhs=x, isTranspose=.FALSE.)
    CALL Display("File = "//__FILE__)
    CALL Display("Precondition is not supported yet!!!")
    STOP

  CASE (4, 6)
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    ! CALL Amat%ILUSOLVE(sol=y, rhs=x, isTranspose=.TRUE.)
    CALL Display("File = "//__FILE__)
    CALL Display("Precondition is not supported yet!!!")
    STOP
  END SELECT
END SUBROUTINE PERFORM_TASK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CHECKERROR(IPAR, FPAR, myName)
  INTEGER(I4B), INTENT(IN) :: IPAR(:)
  REAL(DFP), INTENT(IN) :: FPAR(:)
  CHARACTER(*), INTENT(IN) :: myName
  INTEGER(I4B) :: ierr, unitNo

  ierr = IPAR(1)

  SELECT CASE (ierr)
  CASE (-1)
    unitNo = stdout
    CALL EqualLine(unitNo=unitNo)
    CALL Display(IPAR(7), "# Number of Matrix-Vector Multiplication = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(3), "# Initial residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(4), "# Target residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(6), "# Current residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(5), "# Current residual norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(7), "# Convergence rate = ",&
      & unitNo=unitNo)
    CALL EqualLine(unitNo=unitNo)
    CALL Display("Termination because iteration number exceeds the limit", &
      & unitno)
  CASE (-2)
    CALL Display("Return due to insufficient work space", &
      & unitno)
  CASE (-3)
    CALL Display("Return due to anticipated break-down / divide by zero", &
      & unitno)
  CASE (-4)
    CALL Display( &
      & "The values of `fpar(1)` and `fpar(2)` are both <= 0"// &
      & "the valid ranges are 0 <= fpar(1) < 1, 0 <= fpar(2)"// &
      & "and they can not be zero at the same time", unitno)
  CASE (-9)
    CALL Display( &
      & "While trying to detect a break-down, "// &
      & "an abnormal number is detected", unitno)
  CASE (-10)
    CALL Display( &
      & "Return due to some non-numerical reasons, "// &
      & "e.g. invalid floating-point numbers etc", unitno)
  CASE DEFAULT
    CALL Display( &
      & "Unknown error encountered. Cannot read the error message", &
      & unitno)
  END SELECT
END SUBROUTINE CHECKERROR

!----------------------------------------------------------------------------
!                                                        DisplayConvergence
!----------------------------------------------------------------------------

SUBROUTINE DisplayConvergence(myName, iter, FPAR)
  CHARACTER(*), INTENT(IN) :: myName
  INTEGER(I4B), INTENT(IN) :: iter
  REAL(DFP), INTENT(IN) :: FPAR(:)
  INTEGER(I4B) :: unitno

  unitno = stdout

  CALL display('Convergence is achieved 🎖', unitNo)
  CALL Blanklines(nol=2, unitno=unitno)
  CALL Display(iter, "# Number of Matrix-Vector Multiplication = ",&
    & unitno=unitno)
  CALL Display(fpar(3), "# Initial residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(4), "# Target residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(6), "# Current residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(5), "# Current residual norm = ",&
    & unitno=unitno)
  CALL Display(fpar(7), "# Convergence rate = ",&
    & unitno=unitno)
END SUBROUTINE DisplayConvergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This subroutine allocates the workspace required for the linear solver
!
!# Introduction
!
! This routine allocates the workspace required for the linear solver

SUBROUTINE AllocateWorkSpace(W, IPAR, solverName, n)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: W(:)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: solverName
  INTEGER(I4B), INTENT(IN) :: n

  INTEGER(I4B) :: i, m

  SELECT CASE (solverName)
  CASE (LIS_CG, LIS_CGNR)
    i = 5 * n
  CASE (LIS_BICG)
    i = 7 * n
  CASE (LIS_DBICG)
    i = 11 * n
  CASE (LIS_BICGSTAB)
    i = 8 * n
  CASE (LIS_TFQMR)
    i = 11 * n
  CASE (LIS_ORTHOMIN, LIS_GMRES)
    m = INPUT(default=15, option=IPAR(5))
    i = (n + 3) * (m + 2) + (m + 1) * m / 2
  CASE (LIS_FGMRES)
    m = INPUT(default=15, option=IPAR(5))
    i = 2 * n * (m + 1) + (m + 1) * m / 2 + 3 * m + 2
  CASE (LIS_DQGMRES)
    m = INPUT(default=15, option=IPAR(5)) + 1
    i = n + m * (2 * n + 4)
  END SELECT
  IPAR(4) = i
  CALL Reallocate(W, i)
END SUBROUTINE AllocateWorkSpace

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrix_LinSolve_Initiate

IF (.NOT. ALLOCATED(ipar)) ALLOCATE (ipar(13))
IF (.NOT. ALLOCATED(fpar)) ALLOCATE (fpar(13))

CALL setPreconditionOption( &
  & ipar=IPAR, &
  & PRECOND_TYPE=input(option=preconditionOption, &
  & default=NO_PRECONDITION))

CALL setConvergenceType(ipar=IPAR, &
  & convergenceIn=input(option=convergenceIn, default=convergenceInRes), &
  & convergenceType = input(option= convergenceType, default=relativeConvergence), &
  & relativeToRHS=input(option=relativeToRHS, default=.FALSE.))

IPAR(5) = KrylovSubspaceSize

CALL setMaxIter(IPAR, input(option=maxIter, default=100_I4B))

FPAR = 0.0_DFP

IF (PRESENT(atol)) THEN
  FPAR(2) = atol
END IF

IF (PRESENT(rtol)) THEN
  FPAR(1) = rtol
END IF

IF (.NOT. ALLOCATED(W)) THEN
  CALL AllocateWorkSpace(W, ipar, solverName, n)
END IF

END PROCEDURE CSRMatrix_LinSolve_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrix_GMRES
CHARACTER(*), PARAMETER :: myName = "CSRMatrix_GMRES"
INTEGER(I4B) :: n, ii
! REAL(DFP), ALLOCATABLE :: adiag(:)
REAL(DFP) :: error0
REAL(DFP) :: error
REAL(DFP) :: tol
REAL(DFP) :: normRes
INTEGER(I4B) :: ierr
INTEGER(I4B) :: iter

! IF (preconditionOption .EQ. NO_PRECONDITION) THEN
!   CALL Display('No precondition = Diagonal precondition !')
!   CALL obj%GetDiagonal(diag=adiag)
!   CALL obj%DiagonalScaling(side='BOTH', diag=adiag)
!   DO ii = 1, SIZE(adiag)
!     rhs(ii) = rhs(ii) / SQRT(ABS(adiag(ii)))
!     sol(ii) = sol(ii) * SQRT(ABS(adiag(ii)))
!   END DO
! END IF

IPAR(1) = 0
FPAR(11) = 0.0_DFP
n = SIZE(obj, 1)
IPAR(7) = 1

DO
  CALL GMRES(n, rhs, sol, ipar, fpar, W)
  ! obj%RES(ipar(7)) = fpar(6)

  IF (ipar(1) .GT. 0) THEN
    CALL PERFORM_TASK(obj, y=W(_y1:_y2), x=W(_x1:_x2), &
      & ierr=ipar(1), myName=myName)

  ELSE IF (ipar(1) .LT. 0) THEN
    CALL CHECKERROR(IPAR=ipar, FPAR=fpar, myName=myName)
    EXIT
  ELSE IF (ipar(1) .EQ. 0) THEN
    ierr = ipar(1)
    iter = ipar(7)
    CALL DisplayConvergence(myName, iter, fpar)
    EXIT
  END IF
END DO

! Initial residual/error norm
error0 = fpar(3)
! Target residual/error norm
tol = fpar(4)
! Current residual/error norm
error = fpar(6)
! Current residual norm
normRes = fpar(5)

! ! Applying diagnoal precondition
! IF (preconditionOption .EQ. NO_PRECONDITION) THEN
!   DO ii = 1, SIZE(diag)
!     sol(ii) = sol(ii) / SQRT(ABS(diag(ii)))
!   END DO
!   DEALLOCATE (diag)
! END IF

END PROCEDURE CSRMatrix_GMRES

END SUBMODULE Methods

#undef _x1
#undef _x2
#undef _y1
#undef _y2
