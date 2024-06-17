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

MODULE CSRMatrix_SpectralMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: SymLargestEigenval
PUBLIC :: SymSmallestEigenval

!----------------------------------------------------------------------------
!                                                       SymLargestEigenval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-10
! summary: Calculate the largest eigenvalue of a real sym dense matrix
!
!# Introduction
!
!- This routine calculates the largest eigenvalue of a real sym dense matrix.
!- It calls ARPACK SSAUPD or DSAUPD routine

INTERFACE
  MODULE FUNCTION SymLargestEigenVal1(mat, which, NCV, maxIter, tol) &
    & RESULT(ans)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! dense matrix
    CHARACTER(*), OPTIONAL, INTENT(IN) :: which
    !! `which = "LM"` ⇨ absolute largest eigenvalue
    !! `which = "LA"` ⇨ algebraic largest eigenvalue
    !! default is "LA"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: NCV
    !! Number of Lanczos vectors generated
    !! It must be greater than 1 and smaller than `size(mat,1)`
    !! Default is `NCV = MIN(n, 20)`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! Maximum number of iteration default = `N*10`
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! tolerance, default = 0.0
    REAL(DFP) :: ans
    !! maximum eigenvalue
  END FUNCTION SymLargestEigenVal1
END INTERFACE

INTERFACE SymLargestEigenVal
  MODULE PROCEDURE SymLargestEigenVal1
END INTERFACE SymLargestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-10
! summary: Calculate the `nev` smallest eigenvalue of a real sym dense matrix
!
!# Introduction
!
!- This routine calculates the smallest eigenvalue of a real sym dense matrix.
!- It calls ARPACK SSAUPD or DSAUPD routine

INTERFACE
  MODULE FUNCTION SymLargestEigenVal2(mat, nev, which, NCV, maxIter, tol) &
    & RESULT(ans)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! dense matrix
    INTEGER(I4B), INTENT(IN) :: nev
    !! number of eigenvalues requested
    CHARACTER(*), OPTIONAL, INTENT(IN) :: which
    !! `which = "LM"` ⇨ absolute largest eigenvalue
    !! `which = "LA"` ⇨ algebraic largest eigenvalue
    !! default is "LA"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: NCV
    !! Number of Lanczos vectors generated
    !! It must be greater than 1 and smaller than `size(mat,1)`
    !! Default is `NCV = MIN(n, MAX(2*nev+1, 20))`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! Maximum number of iteration default = `N*10`
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! tolerance, default = 0.0
    REAL(DFP) :: ans(nev)
    !! first k, largest eigenvalue
  END FUNCTION SymLargestEigenVal2
END INTERFACE

INTERFACE SymLargestEigenVal
  MODULE PROCEDURE SymLargestEigenVal2
END INTERFACE SymLargestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-10
! summary: Calculate the smallest eigenvalue of a real sym dense matrix
!
!# Introduction
!
!- This routine calculates the smallest eigenvalue of a real sym dense matrix.
!- It calls ARPACK SSAUPD or DSAUPD routine with MODE=3
!
! In this routine we use shift-inverted method to compute the
! smallest eigenvalue of a regular (standard) eigenvalue problem. This is
! because `ARPACK` is good at finding the largest eigenvalue.
!
! Internally this routine solves a system of linear equations: `mat * y = x`
! by using LU decomposition.
!
! In this routine we make a call to LUSolve and getLU routine.
!
!@note
! In this routine we make a copy of mat in mat0. Then, compute the LU
! decomposition of mat0.
!@endnote

INTERFACE
  MODULE FUNCTION SymSmallestEigenVal1(mat, which, NCV, maxIter, tol) &
    & RESULT(ans)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! dense matrix
    CHARACTER(*), OPTIONAL, INTENT(IN) :: which
    !! `which = "SM"` ⇨ absolute smallest eigenvalue
    !! `which = "SA"` ⇨ algebraic smallest eigenvalue
    !! default is "SA"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: NCV
    !! Number of Lanczos vectors generated
    !! It must be greater than 1 and smaller than `size(mat,1)`
    !! Default is `NCV = MIN(n, 20)`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! Maximum number of iteration default = `N*10`
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! tolerance, default = 0.0
    REAL(DFP) :: ans
    !! maximum eigenvalue
  END FUNCTION SymSmallestEigenVal1
END INTERFACE

INTERFACE SymSmallestEigenVal
  MODULE PROCEDURE SymSmallestEigenVal1
END INTERFACE SymSmallestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-10
! summary: Calculate the smallest eigenvalue of a real sym dense matrix
!
!# Introduction
!
! This routine is similar to SysSmallestEigenVal1()
! In this routine you can pass a factorized matrix `mat` and set `isLU=true`
! Then, this routine will not perform LU decomposition on mat.
!
! However, if `isLU=false`, then we will change mat, and on return
! it will contain the LU factorization of `mat`
!
!- [ ] TODO use Cholsky factorization instead of LU as mat is
! symmetric.
!

INTERFACE
  MODULE FUNCTION SymSmallestEigenVal2(mat, nev, which, &
    & NCV, maxIter, tol) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! CSRMatrix
    INTEGER(I4B), INTENT(IN) :: nev
    !! number of eigenvalues
    CHARACTER(*), OPTIONAL, INTENT(IN) :: which
    !! `which = "SM"` ⇨ absolute smallest eigenvalue
    !! `which = "SA"` ⇨ algebraic smallest eigenvalue
    !! default is "SA"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: NCV
    !! Number of Lanczos vectors generated
    !! It must be greater than 1 and smaller than `size(mat,1)`
    !! Default is `NCV = MIN(n, 20)`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! Maximum number of iteration default = `N*10`
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! tolerance, default = 0.0
    REAL(DFP) :: ans(nev)
    !! smallest eigenvalue
  END FUNCTION SymSmallestEigenVal2
END INTERFACE

INTERFACE SymSmallestEigenVal
  MODULE PROCEDURE SymSmallestEigenVal2
END INTERFACE SymSmallestEigenVal

END MODULE CSRMatrix_SpectralMethods
