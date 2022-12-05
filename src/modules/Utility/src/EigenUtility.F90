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

MODULE EigenUtility
USE GlobalData
IMPLICIT NONE

!----------------------------------------------------------------------------
!                                                                 SymEigen
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION SymEigenValues2by2(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(2, 2)
    REAL(DFP) :: ans(2)
  END FUNCTION SymEigenValues2by2
END INTERFACE

PUBLIC :: SymEigenValues2by2

!----------------------------------------------------------------------------
!                                                                 SymEigen
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION SymEigenValues3by3(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    REAL(DFP) :: ans(3)
  END FUNCTION SymEigenValues3by3
END INTERFACE

PUBLIC :: SymEigenValues3by3

!----------------------------------------------------------------------------
!                                                                 SymEigen
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION SymEigenValuesUpto3(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    !! size(mat, 1) = [1,2,3]
    REAL(DFP) :: ans(SIZE(mat, 1))
  END FUNCTION SymEigenValuesUpto3
END INTERFACE

PUBLIC :: SymEigenValuesUpto3

!----------------------------------------------------------------------------
!                                                                 SymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Nov 2022
! summary:         Compute eigenvalues of matrix
!
!# Introduction
!
! Calculate eigenvalues of symetric matrix.
!
!- If `n=2` call SymEigenValues2by2
!- If `n=3` call SymEigenValues3by3
!- If `n>=4` call SYEV from Lapack, It needs Lapack95 interface

INTERFACE
  MODULE FUNCTION SymEigenValues(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    !! for n=2, we call SymEigenValues2by2
    !! for n=3, we call SymEigenValues3by3
    !! for n>=4, we call Lapack
    REAL(DFP) :: ans(SIZE(mat, 1))
  END FUNCTION SymEigenValues
END INTERFACE

PUBLIC :: SymEigenValues

!----------------------------------------------------------------------------
!                                                                GetSymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Nov 2022
! summary: Compute eigenvalues of matrix
!
!# Introduction
!
! Calculate eigenvalues of symetric matrix.
!
!- If `n=2` call SymEigenValues2by2
!- If `n=3` call SymEigenValues3by3
!- If `n>=4` call SYEV from Lapack, It needs Lapack95 interface

INTERFACE
  MODULE SUBROUTINE GetSymEigenValues(mat, eigenValues)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    REAL(DFP), INTENT(OUT) :: eigenValues(:)
  END SUBROUTINE GetSymEigenValues
END INTERFACE

PUBLIC :: GetSymEigenValues

!----------------------------------------------------------------------------
!                                                                GetSymEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Nov 2022
! summary: Compute eigenvalues of matrix
!
!# Introduction
!
! Calculate eigenvalues of symetric matrix.
!
!- If `n=2` call SymEigenValues2by2
!- If `n=3` call SymEigenValues3by3
!- If `n>=4` call SYEV from Lapack, It needs Lapack95 interface
!
!- mat will be destroyed.

INTERFACE
  MODULE SUBROUTINE GetSymEigenValues_(mat, eigenValues)
    REAL(DFP), INTENT(INOUT) :: mat(:, :)
    REAL(DFP), INTENT(OUT) :: eigenValues(:)
  END SUBROUTINE GetSymEigenValues_
END INTERFACE

PUBLIC :: GetSymEigenValues_

!----------------------------------------------------------------------------
!                                                  GetSymEigenJacobi@LAPACK
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Returns all the eigenvalues of symmetric matrix
!
!# Introduction
!
! This subroutine computes all eigenvalues and eigenvectors of a real
! symmetric N × N matrix `Mat`.
! -  On output, elements of `Mat` above the diagonal are destroyed.
! -  `eigenvalues` is a vector of length N that returns the eigenvalues of
! `Mat`.
! -  `EigenVectors` is an `N × N` matrix whose columns contain on output,
! the normalized eigenvectors (directions) of `Mat`.
! - `maxIter` returns the number of Jacobi rotations that were required.
!
! ### Reference:: Numerical Reciepe in Fortran, Page 1225
!
! TODO: Remove this subroutine, instead call Lapack.

INTERFACE
  MODULE PURE SUBROUTINE GetSymEigenJacobi(mat, eigenValues, eigenVectors, &
    & maxIter)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    REAL(DFP), INTENT(INOUT) :: eigenValues(:)
    REAL(DFP), INTENT(INOUT) :: eigenVectors(:, :)
    INTEGER(I4B), INTENT(IN) :: maxIter
  END SUBROUTINE GetSymEigenJacobi
END INTERFACE

PUBLIC :: GetSymEigenJacobi

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EigenUtility
