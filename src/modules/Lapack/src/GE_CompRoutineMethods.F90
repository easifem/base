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

MODULE GE_CompRoutineMethods
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: ConditionNo
PUBLIC :: GetInvMat

!----------------------------------------------------------------------------
!                                                                ConditionNo
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION ge_ConditionNo_1(A, NORM) RESULT(ans)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! General matrix
    CHARACTER(1), INTENT(IN) :: NORM
    !! "1", "0"
    REAL(DFP) :: ans
    !!
  END FUNCTION ge_ConditionNo_1
END INTERFACE

INTERFACE ConditionNo
  MODULE PROCEDURE ge_ConditionNo_1
END INTERFACE ConditionNo

!----------------------------------------------------------------------------
!                                                                    GetInvMat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:         Inverse of matrix
!
!# Introduction
!
! This routine calls `DGETRI` routine from Lapack.
! A copy of matrix A is made into invA, then LU decomposition is performed and
! `DGETRI` is called from lapack

INTERFACE
  MODULE SUBROUTINE ge_GetInvMat1(A, invA)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! General matrix
    REAL(DFP), INTENT(INOUT) :: invA(:, :)
    !!
  END SUBROUTINE ge_GetInvMat1
END INTERFACE

INTERFACE GetInvMat
  MODULE PROCEDURE ge_GetInvMat1
END INTERFACE GetInvMat

!----------------------------------------------------------------------------
!                                                                    GetInvMat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:         Inverse of matrix
!
!# Introduction
!
!- This routine calls `DGETRI` routine from Lapack.
!- A and IPIV are obtained from LU decomposition
!- A contains the LU decomposition of matrix A
!- A copy of matrix A is made into invA, then
! `DGETRI` is called from lapack

INTERFACE
  MODULE SUBROUTINE ge_GetInvMat2(A, IPIV, invA)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! General matrix
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! General matrix
    REAL(DFP), INTENT(INOUT) :: invA(:, :)
    !!
  END SUBROUTINE ge_GetInvMat2
END INTERFACE

INTERFACE GetInvMat
  MODULE PROCEDURE ge_GetInvMat2
END INTERFACE GetInvMat

!----------------------------------------------------------------------------
!                                                                    GetInvMat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:         Inverse of matrix
!
!# Introduction
!
!- This routine calls `DGETRI` routine from Lapack.
!- A and IPIV are obtained from LU decomposition
!- A contains the LU decomposition of matrix A at input
!- At output invese of A is stored inside A
!- No copy is made.

INTERFACE
  MODULE SUBROUTINE ge_GetInvMat3(A, IPIV)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LU Decompose at input
    !! inverse at output
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !!
  END SUBROUTINE ge_GetInvMat3
END INTERFACE

INTERFACE GetInvMat
  MODULE PROCEDURE ge_GetInvMat3
END INTERFACE GetInvMat

!----------------------------------------------------------------------------
!                                                                 GetInvMat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:         Inverse of matrix
!
!# Introduction
!
!- This routine calls `DGETRI` routine from Lapack.
!- First LU decomposition is performed
!- Then `DGETRI` is called from lapack
!- At output A contains the inverse.

INTERFACE
  MODULE SUBROUTINE ge_GetInvMat4(A)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
  END SUBROUTINE ge_GetInvMat4
END INTERFACE

INTERFACE GetInvMat
  MODULE PROCEDURE ge_GetInvMat4
END INTERFACE GetInvMat

END MODULE GE_CompRoutineMethods
