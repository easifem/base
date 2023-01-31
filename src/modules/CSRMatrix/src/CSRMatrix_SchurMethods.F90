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

MODULE CSRMatrix_SchurMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
PRIVATE

PUBLIC :: SchurMatVec
PUBLIC :: SymSchurLargestEigenval

!----------------------------------------------------------------------------
!                                                    AMatVec1@MatvecMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: This routine computes y = (Transpose(B) * Inv(A) * B)
!
!# Introduction
!
!$$
!y = S \cdot x
!$$
!
!where,
!
!$$
! {\bf S}=\left({\bf B}^{T}{\bf A}^{-1}{\bf B}\right),
!$$

INTERFACE
  MODULE SUBROUTINE csrMat_AMatVec(A, B, x, y)
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    TYPE(CSRMatrix_), INTENT(INOUT) :: B
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
  END SUBROUTINE csrMat_AMatVec
END INTERFACE

!----------------------------------------------------------------------------
!                                                     AtMatvec@MatvecMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: This routine computes y = (Transpose(B) * Inv(A) * B)
!
!# Introduction
!
!$$
!y = S^{T} \cdot x
!$$
!
!where,
!
!$$
! {\bf S}=\left({\bf B}^{T}{\bf A}^{-1}{\bf B}\right),
!$$

INTERFACE
  MODULE SUBROUTINE csrMat_AtMatVec(A, B, x, y, isASym)
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    TYPE(CSRMatrix_), INTENT(INOUT) :: B
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isASym
    !! True if A is symmetric
    !! False if A is not symmetric
    !! Default is False
  END SUBROUTINE csrMat_AtMatVec
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Matvec@MatVec
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-30
! summary: This routine computes y = (Transpose(B) * Inv(A) * B)
!

INTERFACE
  MODULE SUBROUTINE csrMat_SchurMatVec(A, B, x, y, isTranspose, isASym)
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    TYPE(CSRMatrix_), INTENT(INOUT) :: B
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isASym
  END SUBROUTINE csrMat_SchurMatVec
END INTERFACE

INTERFACE SchurMatVec
  MODULE PROCEDURE csrMat_SchurMatVec
END INTERFACE SchurMatVec

!----------------------------------------------------------------------------
!                                                   SymSchurLargestEigenval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-10
! summary:
!

INTERFACE
  MODULE FUNCTION SymSchurLargestEigenVal1(A, B, which, NCV, maxIter, tol) &
    & RESULT(ans)
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    !! Symmetric matrix
    TYPE(CSRMatrix_), INTENT(INOUT) :: B
    !! B matrix, it can be rectangle
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
  END FUNCTION SymSchurLargestEigenVal1
END INTERFACE

INTERFACE SymSchurLargestEigenVal
  MODULE PROCEDURE SymSchurLargestEigenVal1
END INTERFACE SymSchurLargestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary:

INTERFACE
  MODULE FUNCTION SymSchurLargestEigenVal2(A, B, nev, which, NCV, &
      & maxIter, tol) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    !! CSRMatrix, symmetric
    TYPE(CSRMatrix_), INTENT(INOUT) :: B
    !! B matrix, possibly rectangle
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
  END FUNCTION SymSchurLargestEigenVal2
END INTERFACE

INTERFACE SymSchurLargestEigenVal
  MODULE PROCEDURE SymSchurLargestEigenVal2
END INTERFACE SymSchurLargestEigenVal

END MODULE CSRMatrix_SchurMethods
