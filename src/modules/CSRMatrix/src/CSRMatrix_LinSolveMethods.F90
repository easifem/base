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

MODULE CSRMatrix_LinSolveMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: CSRMatrixLinSolveInitiate
PUBLIC :: CSRMatrix_GMRES
PUBLIC :: CSRMatrix_CG
PUBLIC :: CSRMatrix_BiCGStab

INTEGER(I4B), PARAMETER :: IPAR_LENGTH = 14
INTEGER(I4B), PARAMETER :: FPAR_LENGTH = 14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-14
! summary: Return integer code of linear solver from character name

INTERFACE
  MODULE PURE FUNCTION GetLinSolverCodeFromName(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION GetLinSolverCodeFromName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-14
! summary: Return character name of linear solver from integer code

INTERFACE
  MODULE PURE FUNCTION GetLinSolverNameFromCode(name) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: name
    CHARACTER(15) :: ans
  END FUNCTION GetLinSolverNameFromCode
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE CSRMatrixLinSolveInitiate
  MODULE SUBROUTINE CSRMatrix_LinSolve_Initiate(ipar, fpar, W, n, &
       & solverName, preConditionOption, convergenceIn, convergenceType, &
       & maxIter, KrylovSubspaceSize, rtol, atol, relativeToRHS)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: ipar(:)
    !! Integer PARAMETER
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: fpar(:)
    !! Read PARAMETER
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: W(:)
    !! Workspace requirement
    INTEGER(I4B), INTENT(IN) :: n
    !! size of the problem
    INTEGER(I4B), OPTIONAL, INTENT(in) :: solverName
    !! solverName
    INTEGER(I4B), OPTIONAL, INTENT(in) :: preconditionOption
    !! preconditionOption
    !! NO_PRECONDITION
    !! LEFT_PRECONDITION
    !! RIGHT_PRECONDITON
    !! LEFT_RIGHT_PRECONDITION
    INTEGER(I4B), OPTIONAL, INTENT(in) :: convergenceIn
    !! convergenceInRes
    !! convergenceInSol
    INTEGER(I4B), OPTIONAL, INTENT(in) :: convergenceType
    !! absoluteConvergence
    !! relativeConvergence
    INTEGER(I4B), OPTIONAL, INTENT(in) :: maxIter
    !! maximum number of iterations
    INTEGER(I4B), OPTIONAL, INTENT(in) :: KrylovSubspaceSize
    !! Size of KrylovSubspace
    REAL(DFP), OPTIONAL, INTENT(in) :: rtol
    !! relative tolerance
    REAL(DFP), OPTIONAL, INTENT(in) :: atol
    !! absolute tolerance
    LOGICAL(LGT), OPTIONAL, INTENT(in) :: relativeToRHS
    !! true if convergence is checked relatative to RHS
  END SUBROUTINE CSRMatrix_LinSolve_Initiate
END INTERFACE CSRMatrixLinSolveInitiate

!----------------------------------------------------------------------------
!                                                                  LinSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-14
! summary: Solver

INTERFACE
  MODULE SUBROUTINE CSRMatrix_GMRES(obj, sol, rhs, ipar, fpar, W)
    CLASS(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: sol(:)
    REAL(DFP), INTENT(INOUT) :: rhs(:)
    INTEGER(I4B), INTENT(INOUT) :: ipar(:)
    REAL(DFP), INTENT(INOUT) :: fpar(:)
    REAL(DFP), INTENT(INOUT) :: W(:)
  END SUBROUTINE CSRMatrix_GMRES
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  LinSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-14
! summary: Solver

INTERFACE
  MODULE SUBROUTINE CSRMatrix_CG(obj, sol, rhs, ipar, fpar, W)
    CLASS(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: sol(:)
    REAL(DFP), INTENT(INOUT) :: rhs(:)
    INTEGER(I4B), INTENT(INOUT) :: ipar(:)
    REAL(DFP), INTENT(INOUT) :: fpar(:)
    REAL(DFP), INTENT(INOUT) :: W(:)
  END SUBROUTINE CSRMatrix_CG
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  LinSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-14
! summary: Solver

INTERFACE
  MODULE SUBROUTINE CSRMatrix_BiCGStab(obj, sol, rhs, ipar, fpar, W)
    CLASS(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: sol(:)
    REAL(DFP), INTENT(INOUT) :: rhs(:)
    INTEGER(I4B), INTENT(INOUT) :: ipar(:)
    REAL(DFP), INTENT(INOUT) :: fpar(:)
    REAL(DFP), INTENT(INOUT) :: W(:)
  END SUBROUTINE CSRMatrix_BiCGStab
END INTERFACE

END MODULE CSRMatrix_LinSolveMethods
