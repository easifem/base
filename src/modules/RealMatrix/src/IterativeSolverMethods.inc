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

PUBLIC :: LinearSolver_CG

!----------------------------------------------------------------------------
!                                                 CG@IterativeSolverMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realmat_CG_1( mat, rhs, sol, maxIter, &
    & rtol, atol, convergenceIn, relativeToRHS, &
    & restartAfter )
  REAL( DFP ), INTENT( IN ) :: mat( :, : )
  !! Symmetric matrix
  REAL( DFP ), INTENT( IN ) :: rhs( : )
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: maxIter
  !! maximum number of iteration
  !! if maxIter < 0 then maxIter=infinite
  !! if maxIter is absent then min( size(mat,1), 10 )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: rtol
  !! relative tolerance, default is 1.0E-6
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: atol
  !! absolute tolerance, default is 0.0
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: convergenceIn
  !! convergenceInRes <-- default
  !! convergenceInSol
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: relativeToRHS
  !! FALSE <--- relative converfence is checked with respect to ||res||
  !! TRUE Convergence is checked with respect to ||rhs||
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: restartAfter
  !! recompute residual by using b-Ax
END SUBROUTINE realmat_CG_1
END INTERFACE

INTERFACE LinearSolver_CG
  MODULE PROCEDURE realmat_CG_1
END INTERFACE LinearSolver_CG
