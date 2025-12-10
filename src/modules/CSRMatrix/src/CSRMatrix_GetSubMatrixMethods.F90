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

MODULE CSRMatrix_GetSubMatrixMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE

PRIVATE
PUBLIC :: GetSubMatrix
PUBLIC :: GetSubMatrix_
PUBLIC :: GetSubMatrixNNZ

!----------------------------------------------------------------------------
!                                                       GetColumn@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the submatrix

INTERFACE
  MODULE SUBROUTINE obj_GetSubMatrixNNZ(obj, cols, selectCol, ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: cols(:)
    !! column indices to be extracted
    LOGICAL(LGT), INTENT(INOUT) :: selectCol(:)
    !! size of subIndices
    INTEGER(I4B), INTENT(OUT) :: ans
  END SUBROUTINE obj_GetSubMatrixNNZ
END INTERFACE

INTERFACE GetSubMatrixNNZ
  MODULE PROCEDURE obj_GetSubMatrixNNZ
END INTERFACE GetSubMatrixNNZ

!----------------------------------------------------------------------------
!                                                       GetColumn@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the submatrix

INTERFACE
  MODULE SUBROUTINE obj_GetSubMatrix_1( &
    obj, cols, submat, subIndices, selectCol, tsize)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: cols(:)
    !! column indices to be extracted
    TYPE(CSRMatrix_), INTENT(INOUT) :: submat
    !! CSRMatrix to store the submatrix
    INTEGER(I4B), INTENT(INOUT) :: subIndices(:)
    LOGICAL(LGT), INTENT(INOUT) :: selectCol(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of subIndices
  END SUBROUTINE obj_GetSubMatrix_1
END INTERFACE

INTERFACE GetSubMatrix_
  MODULE PROCEDURE obj_GetSubMatrix_1
END INTERFACE GetSubMatrix_

!----------------------------------------------------------------------------
!                                                       GetColumn@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the submatrix

INTERFACE
  MODULE SUBROUTINE obj_GetSubMatrix1(obj, cols, submat, subIndices)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: cols(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: submat
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: subIndices(:)
  END SUBROUTINE obj_GetSubMatrix1
END INTERFACE

INTERFACE GetSubMatrix
  MODULE PROCEDURE obj_GetSubMatrix1
END INTERFACE GetSubMatrix

!----------------------------------------------------------------------------
!                                                       GetColumn@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the submatrix

INTERFACE
  MODULE SUBROUTINE obj_GetSubMatrix2(obj, subIndices, submat)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: subIndices(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: submat
  END SUBROUTINE obj_GetSubMatrix2
END INTERFACE

INTERFACE GetSubMatrix
  MODULE PROCEDURE obj_GetSubMatrix2
END INTERFACE GetSubMatrix

INTERFACE GetSubMatrix_
  MODULE PROCEDURE obj_GetSubMatrix2
END INTERFACE GetSubMatrix_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSRMatrix_GetSubMatrixMethods
