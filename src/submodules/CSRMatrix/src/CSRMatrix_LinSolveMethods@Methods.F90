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
USE BaseType, ONLY: TypeSolverNameOpt
USE BaseType, ONLY: TypePrecondOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: cnvgopt => TypeConvergenceOpt
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: EqualLine
USE InputUtility, ONLY: Input
USE CSRMatrix_MatVecMethods, ONLY: MatVec
USE CSRMatrix_ConstructorMethods, ONLY: CSRMatrixInitiate => Initiate
USE CSRMatrix_ConstructorMethods, ONLY: CSRMatrixSize => Size
USE ReallocateUtility, ONLY: Reallocate
USE ErrorHandling, ONLY: Errormsg
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetLinSolverCodeFromName
SELECT CASE (TRIM(name))
CASE ("SUPERLU")
  ans = TypeSolverNameOpt%superlu
CASE ("CG")
  ans = TypeSolverNameOpt%cg
CASE ("BICG")
  ans = TypeSolverNameOpt%bicg
CASE ("CGS")
  ans = TypeSolverNameOpt%cgs
CASE ("BICGSTAB")
  ans = TypeSolverNameOpt%bicgstab
CASE ("BICGSTABL")
  ans = TypeSolverNameOpt%bicgstabl
CASE ("GPBICG")
  ans = TypeSolverNameOpt%gpbicg
CASE ("TFQMR")
  ans = TypeSolverNameOpt%tfqmr
CASE ("OMN", "FOM", "ORTHOMIN")
  ans = TypeSolverNameOpt%omn
CASE ("GMRES", "GMR")
  ans = TypeSolverNameOpt%gmres
CASE ("JACOBI")
  ans = TypeSolverNameOpt%jacobi
CASE ("GS")
  ans = TypeSolverNameOpt%gs
CASE ("SOR")
  ans = TypeSolverNameOpt%sor
CASE ("BICGSAFE")
  ans = TypeSolverNameOpt%bicgsafe
CASE ("CR")
  ans = TypeSolverNameOpt%cr
CASE ("BICR")
  ans = TypeSolverNameOpt%bicr
CASE ("CRS")
  ans = TypeSolverNameOpt%crs
CASE ("BICRSTAB")
  ans = TypeSolverNameOpt%bicrstab
CASE ("GPBICR")
  ans = TypeSolverNameOpt%gpbicr
CASE ("BICRSAFE")
  ans = TypeSolverNameOpt%bicrsafe
CASE ("FGMRES")
  ans = TypeSolverNameOpt%fgmres
CASE ("IDRS")
  ans = TypeSolverNameOpt%idrs
CASE ("IDR1")
  ans = TypeSolverNameOpt%idr1
CASE ("MINRES")
  ans = TypeSolverNameOpt%minres
CASE ("COCG")
  ans = TypeSolverNameOpt%cocg
CASE ("COCR")
  ans = TypeSolverNameOpt%cocr
CASE ("CGNR", "CGN")
  ans = TypeSolverNameOpt%cgnr
CASE ("DBICG")
  ans = TypeSolverNameOpt%dbicg
CASE ("DQGMRES")
  ans = TypeSolverNameOpt%dqgmres
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE GetLinSolverCodeFromName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetLinSolverNameFromCode
SELECT CASE (name)
CASE (TypeSolverNameOpt%superlu)
  ans = "SUPERLU"
CASE (TypeSolverNameOpt%cg)
  ans = "CG"
CASE (TypeSolverNameOpt%bicg)
  ans = "BICG"
CASE (TypeSolverNameOpt%cgs)
  ans = "CGS"
CASE (TypeSolverNameOpt%bicgstab)
  ans = "BICGSTAB"
CASE (TypeSolverNameOpt%bicgstabl)
  ans = "BICGSTABL"
CASE (TypeSolverNameOpt%gpbicg)
  ans = "GPBICG"
CASE (TypeSolverNameOpt%tfqmr)
  ans = "TFQMR"
CASE (TypeSolverNameOpt%orthomin)
  ans = "ORTHOMIN"
CASE (TypeSolverNameOpt%gmres)
  ans = "GMRES"
CASE (TypeSolverNameOpt%jacobi)
  ans = "JACOBI"
CASE (TypeSolverNameOpt%gs)
  ans = "GS"
CASE (TypeSolverNameOpt%sor)
  ans = "SOR"
CASE (TypeSolverNameOpt%bicgsafe)
  ans = "BICGSAFE"
CASE (TypeSolverNameOpt%cr)
  ans = "CR"
CASE (TypeSolverNameOpt%bicr)
  ans = "BICR"
CASE (TypeSolverNameOpt%crs)
  ans = "CRS"
CASE (TypeSolverNameOpt%bicrstab)
  ans = "BICRSTAB"
CASE (TypeSolverNameOpt%gpbicr)
  ans = "GPBICR"
CASE (TypeSolverNameOpt%bicrsafe)
  ans = "BICRSAFE"
CASE (TypeSolverNameOpt%fgmres)
  ans = "FGMRES"
CASE (TypeSolverNameOpt%idrs)
  ans = "IDRS"
CASE (TypeSolverNameOpt%idr1)
  ans = "IDR1"
CASE (TypeSolverNameOpt%minres)
  ans = "MINRES"
CASE (TypeSolverNameOpt%cocg)
  ans = "COCG"
CASE (TypeSolverNameOpt%cocr)
  ans = "COCR"
CASE (TypeSolverNameOpt%cgnr)
  ans = "CGNR"
CASE (TypeSolverNameOpt%dbicg)
  ans = "DBICG"
CASE (TypeSolverNameOpt%dqgmres)
  ans = "DQGMRES"
CASE DEFAULT
  ans = "NONE"
END SELECT
END PROCEDURE GetLinSolverNameFromCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetPreconditionOption(IPAR, PRECOND_TYPE)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: PRECOND_TYPE

  SELECT CASE (PRECOND_TYPE)
  CASE (TypePrecondOpt%NONE)
    IPAR(2) = 0
  CASE (TypePrecondOpt%left)
    IPAR(2) = 1
  CASE (TypePrecondOpt%right)
    IPAR(2) = 2
  CASE (TypePrecondOpt%both)
    IPAR(2) = 3
  CASE DEFAULT
#ifdef DEBUG_VER
    CALL Errormsg( &
      msg="No case found for PRECOND_TYPE", &
      file=__FILE__, &
      routine="SetPreconditionOption()", &
      line=__LINE__)
#endif
  END SELECT
END SUBROUTINE SetPreconditionOption

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetKrylovSubspaceSize(IPAR, m)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: m
  IPAR(5) = Input(default=15_I4B, option=m)
END SUBROUTINE SetKrylovSubspaceSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetMaxIter(IPAR, maxIter)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: maxIter
  IPAR(6) = maxIter
END SUBROUTINE SetMaxIter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetConvergenceType(IPAR, convergenceIn, convergenceType, &
                              relativeToRHS)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: convergenceIn
  INTEGER(I4B), INTENT(IN) :: convergenceType
  LOGICAL(LGT), INTENT(IN) :: relativeToRHS

  IPAR(3) = 1
  SELECT CASE (convergenceType)

  CASE (cnvgopt%absolute)
    IF (convergenceIn .EQ. cnvgopt%sol) THEN
      IPAR(3) = -1
    ELSE IF (convergenceIn .EQ. cnvgopt%res) THEN
      IPAR(3) = 1
    END IF

  CASE (cnvgopt%relative)

    IF (convergenceIn .EQ. cnvgopt%sol) THEN
      IF (relativeToRHS) THEN
        IPAR(3) = -2
      ELSE
        IPAR(3) = -1
      END IF

    ELSE IF (convergenceIn .EQ. cnvgopt%res) THEN
      IF (relativeToRHS) THEN
        IPAR(3) = 2
      ELSE
        IPAR(3) = 1
      END IF
    END IF

  CASE DEFAULT

#ifdef DEBUG_VER
    CALL Errormsg( &
      msg="No case found for convergenceType", &
      file=__FILE__, &
      routine="SetConvergenceType()", &
      line=__LINE__)
#endif

  END SELECT
END SUBROUTINE SetConvergenceType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE PERFORM_TASK(Amat, y, x, ierr)
  ! intent of dummy variables
  CLASS(CSRMatrix_), INTENT(INOUT) :: Amat
  REAL(DFP), INTENT(INOUT) :: y(:)
  REAL(DFP), INTENT(IN) :: x(:)
  INTEGER(I4B), INTENT(IN) :: ierr

  SELECT CASE (ierr)
  CASE (1)
    ! MatVec, y=Ax
#ifdef DEBUG_VER
    CALL Display("Calling Matvec (tranposed FALSE)...")
#endif
    CALL Matvec(obj=Amat, y=y, x=x, isTranspose=math%no)

  CASE (2)
    ! Transposed MatVec
#ifdef DEBUG_VER
    CALL Display("Calling Matvec (tranposed TRUE)...")
#endif
    CALL Matvec(obj=Amat, y=y, x=x, isTranspose=math%no)

  CASE (3, 5)
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    ! CALL Amat%ILUSOLVE(sol=y, rhs=x, isTranspose=.FALSE.)
#ifdef DEBUG_VER
    CALL Errormsg( &
      msg="Precondition is not supported", &
      file=__FILE__, &
      routine="PERFORM_TASK()", &
      line=__LINE__)
#endif

  CASE (4, 6)
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    ! CALL Amat%ILUSOLVE(sol=y, rhs=x, isTranspose=.TRUE.)
#ifdef DEBUG_VER
    CALL Errormsg( &
      msg="Precondition is not supported", &
      file=__FILE__, &
      routine="PERFORM_TASK()", &
      line=__LINE__)
#endif

  CASE DEFAULT

#ifdef DEBUG_VER
    CALL Errormsg( &
      msg="No case found for ierr", &
      file=__FILE__, &
      routine="PERFORM_TASK()", &
      line=__LINE__)
#endif

  END SELECT
END SUBROUTINE PERFORM_TASK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CHECKERROR(IPAR, FPAR)
  INTEGER(I4B), INTENT(IN) :: IPAR(:)
  REAL(DFP), INTENT(IN) :: FPAR(:)
  INTEGER(I4B) :: ierr

  ierr = IPAR(1)

  SELECT CASE (ierr)
  CASE (-1)
    CALL EqualLine()
    CALL Display(IPAR(7), "Number of Matrix-Vector Multiplication: ")
    CALL Display(FPAR(3), "Initial residual/error norm: ")
    CALL Display(FPAR(4), "Target residual/error norm: ")
    CALL Display(FPAR(6), "Current residual/error norm: ")
    CALL Display(FPAR(5), "Current residual norm: ")
    CALL Display(FPAR(7), "Convergence rate: ")
    CALL EqualLine()
    CALL Display("Termination because iteration number exceeds the limit")
  CASE (-2)
    CALL Display("Return due to insufficient work space")
  CASE (-3)
    CALL Display("Return due to anticipated break-down / divide by zero")
  CASE (-4)
    CALL Display( &
      "The values of `fpar(1)` and `fpar(2)` are both <= 0"// &
      "the valid ranges are 0 <= fpar(1) < 1, 0 <= fpar(2)"// &
      "and they can not be zero at the same time")
  CASE (-9)
    CALL Display( &
      "While trying to detect a break-down, "// &
      "an abnormal number is detected")
  CASE (-10)
    CALL Display( &
      "Return due to some non-numerical reasons, "// &
      "e.g. invalid floating-point numbers etc")
  CASE DEFAULT
    CALL Display( &
      "Unknown error encountered. Cannot read the error message")
  END SELECT
END SUBROUTINE CHECKERROR

!----------------------------------------------------------------------------
!                                                        DisplayConvergence
!----------------------------------------------------------------------------

SUBROUTINE DisplayConvergence(iter, FPAR)
  INTEGER(I4B), INTENT(IN) :: iter
  REAL(DFP), INTENT(IN) :: FPAR(:)

  CALL Display('Convergence is achieved')
  CALL Display(iter, "Number of Matrix-Vector Multiplication: ")
  CALL Display(fpar(3), "Initial residual/error norm: ")
  CALL Display(fpar(4), "Target residual/error norm: ")
  CALL Display(fpar(6), "Current residual/error norm: ")
  CALL Display(fpar(5), "Current residual norm: ")
  CALL Display(fpar(7), "Convergence rate: ")
END SUBROUTINE DisplayConvergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This subroutine allocates workspace required for linear solver
!
! AllocateWorkSpace
!
! This routine allocates the workspace required for the linear solver

SUBROUTINE AllocateWorkSpace(W, IPAR, solverName, n)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: W(:)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: solverName
  INTEGER(I4B), INTENT(IN) :: n

  INTEGER(I4B) :: i, m

  SELECT CASE (solverName)
  CASE (TypeSolverNameOpt%cg, TypeSolverNameOpt%cgnr)
    i = 5 * n
    ! CASE (LIS_BICG)
  CASE (TypeSolverNameOpt%bicg)
    i = 7 * n
    ! CASE (LIS_DBICG)
  CASE (TypeSolverNameOpt%dbicg)
    i = 11 * n
    ! CASE (LIS_BICGSTAB)
  CASE (TypeSolverNameOpt%bicgstab)
    i = 8 * n
    ! CASE (LIS_TFQMR)
  CASE (TypeSolverNameOpt%tfqmr)
    i = 11 * n
    ! CASE (LIS_ORTHOMIN, LIS_GMRES)
  CASE (TypeSolverNameOpt%orthomin, TypeSolverNameOpt%gmres)
    m = Input(default=15, option=IPAR(5))
    i = (n + 3) * (m + 2) + (m + 1) * m / 2
    ! CASE (LIS_FGMRES)
  CASE (TypeSolverNameOpt%fgmres)
    m = Input(default=15, option=IPAR(5))
    i = 2 * n * (m + 1) + (m + 1) * m / 2 + 3 * m + 2
    ! CASE (LIS_DQGMRES)
  CASE (TypeSolverNameOpt%dqgmres)
    m = Input(default=15, option=IPAR(5)) + 1
    i = n + m * (2 * n + 4)
  CASE DEFAULT
  END SELECT
  IPAR(4) = i
  CALL Reallocate(W, i)
END SUBROUTINE AllocateWorkSpace

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrix_LinSolve_Initiate
REAL(DFP), PARAMETER :: default_atol = 1.0D-10, &
                        default_rtol = 1.0D-10
INTEGER(I4B) :: preConditionOption0, convergenceIn0, convergenceType0, &
                maxIter0, solvername0
LOGICAL(LGT) :: relativeToRHS0

IF (.NOT. ALLOCATED(ipar)) ALLOCATE (ipar(13))
IF (.NOT. ALLOCATED(fpar)) ALLOCATE (fpar(13))

preConditionOption0 = Input(option=preconditionOption, &
                            default=TypePrecondOpt%NONE)
CALL SetPreconditionOption(ipar=ipar, PRECOND_TYPE=preConditionOption0)

convergenceIn0 = Input(option=convergenceIn, default=cnvgopt%res)
convergenceType0 = Input(option=convergenceType, default=cnvgopt%relative)
relativeToRHS0 = Input(option=relativeToRHS, default=math%yes)

CALL SetConvergenceType(ipar=ipar, convergenceIn=convergenceIn0, &
                        convergenceType=convergenceType0, &
                        relativeToRHS=relativeToRHS0)

IPAR(5) = Input(option=KrylovSubspaceSize, default=5_I4B)

maxIter0 = Input(option=maxIter, default=math%minus_one_i)
CALL SetMaxIter(ipar, maxIter0)

fpar = 0.0_DFP

fpar(1) = Input(option=rtol, default=default_rtol)
fpar(2) = Input(option=atol, default=default_atol)
solvername0 = Input(default=TypeSolverNameOpt%cg, option=solverName)

IF (.NOT. ALLOCATED(W)) THEN
  CALL AllocateWorkSpace(W, ipar, solvername0, n)
END IF
END PROCEDURE CSRMatrix_LinSolve_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrix_GMRES
INTEGER(I4B) :: n
! REAL(DFP) :: error0, error, tol, normRes
! INTEGER(I4B) :: ierr, iter

IPAR(1) = 0
FPAR(11) = 0.0_DFP
n = CSRMatrixSize(obj, 1)
IPAR(7) = 1

DO
#ifdef DEBUG_VER
  CALL Display("Calling GMRES...")
#endif

  CALL GMRES(n, rhs, sol, ipar, fpar, W)
  ! obj%RES(ipar(7)) = fpar(6)

  IF (ipar(1) .GT. 0) THEN

#ifdef DEBUG_VER
    CALL Display("Calling PERFORM_TASK...")
#endif

    CALL PERFORM_TASK(obj, y=W(_y1:_y2), x=W(_x1:_x2), &
                      ierr=ipar(1))

  ELSE IF (ipar(1) .LT. 0) THEN

#ifdef DEBUG_VER
    CALL Display("Calling CHECKERROR...")
#endif

    CALL CHECKERROR(IPAR=ipar, FPAR=fpar)
    EXIT

  ELSE IF (ipar(1) .EQ. 0) THEN
    ! ierr = ipar(1)
    ! iter = ipar(7)
    CALL DisplayConvergence(ipar(7), fpar)
    EXIT

  END IF
END DO

! Initial residual/error norm
! error0 = fpar(3)
! Target residual/error norm
! tol = fpar(4)
! Current residual/error norm
! error = fpar(6)
! Current residual norm
! normRes = fpar(5)

END PROCEDURE CSRMatrix_GMRES

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrix_CG
INTEGER(I4B) :: n
! REAL(DFP) :: error0, error, tol, normRes
! INTEGER(I4B) :: ierr, iter

IPAR(1) = 0
FPAR(11) = 0.0_DFP
n = CSRMatrixSize(obj, 1)
IPAR(7) = 1

DO
  CALL CG(n, rhs, sol, ipar, fpar, W)
  ! obj%RES(ipar(7)) = fpar(6)

  IF (ipar(1) .GT. 0) THEN
    CALL PERFORM_TASK(obj, y=W(_y1:_y2), x=W(_x1:_x2), &
      & ierr=ipar(1))

  ELSE IF (ipar(1) .LT. 0) THEN
    CALL CHECKERROR(IPAR=ipar, FPAR=fpar)
    EXIT

  ELSE IF (ipar(1) .EQ. 0) THEN
    ! ierr = ipar(1)
    ! iter = ipar(7)
    CALL DisplayConvergence(ipar(7), fpar)
    EXIT

  END IF
END DO

! Initial residual/error norm
! error0 = fpar(3)
! Target residual/error norm
! tol = fpar(4)
! Current residual/error norm
! error = fpar(6)
! Current residual norm
! normRes = fpar(5)

END PROCEDURE CSRMatrix_CG

!----------------------------------------------------------------------------
!                                                                 BiCGStab
!----------------------------------------------------------------------------

MODULE PROCEDURE CSRMatrix_BiCGStab
INTEGER(I4B) :: n
! REAL(DFP) :: error0, error, tol, normRes
! INTEGER(I4B) :: ierr, iter

IPAR(1) = 0
FPAR(11) = 0.0_DFP
n = CSRMatrixSize(obj, 1)
IPAR(7) = 1

DO
  CALL BCGSTAB(n, rhs, sol, ipar, fpar, W)
  ! obj%RES(ipar(7)) = fpar(6)

  IF (ipar(1) .GT. 0) THEN
    CALL PERFORM_TASK(obj, y=W(_y1:_y2), x=W(_x1:_x2), &
      & ierr=ipar(1))

  ELSE IF (ipar(1) .LT. 0) THEN
    CALL CHECKERROR(IPAR=ipar, FPAR=fpar)
    EXIT

  ELSE IF (ipar(1) .EQ. 0) THEN
    ! ierr = ipar(1)
    ! iter = ipar(7)
    CALL DisplayConvergence(ipar(7), fpar)
    EXIT

  END IF
END DO

! Initial residual/error norm
! error0 = fpar(3)
! Target residual/error norm
! tol = fpar(4)
! Current residual/error norm
! error = fpar(6)
! Current residual norm
! normRes = fpar(5)

END PROCEDURE CSRMatrix_BiCGStab

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods

#undef _x1
#undef _x2
#undef _y1
#undef _y2
