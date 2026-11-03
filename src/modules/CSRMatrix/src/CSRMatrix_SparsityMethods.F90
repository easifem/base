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

MODULE CSRMatrix_SparsityMethods
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE
PUBLIC :: SetSparsity

!----------------------------------------------------------------------------
!                                                    SetSparsity@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
!  - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity1(obj, row, col)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: row
    !! row number
    INTEGER(I4B), INTENT(IN) :: col(:)
    !! column indices (only node number is required)
  END SUBROUTINE obj_SetSparsity1
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine sets the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of many rows

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity2(obj, row, col)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: row(:)
    !! row indices
    TYPE(IntVector_), INTENT(IN) :: col(:)
    !! each intVector, col(i), contains col indices of row(i)
  END SUBROUTINE obj_SetSparsity2
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
!  - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity3(obj, row, col, ivar, jvar)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: row
    !! row index
    INTEGER(I4B), INTENT(IN) :: col(:)
    !! col indices
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable i
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable j
  END SUBROUTINE obj_SetSparsity3
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine sets the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
! This subroutine calls `obj_SetSparsity1`

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity4(obj, row, col, ivar, jvar)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: row(:)
    TYPE(IntVector_), INTENT(IN) :: col(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
  END SUBROUTINE obj_SetSparsity4
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine set sparsity pattern of `sparsematrix_`
!
!# Introduction
!
! authors: Dr. Vikas Sharma
!
! This subroutine set sparsity pattern of `sparsematrix_`
! This will finally set the data into
!
! - `obj%A(:)`
! - `obj%IA(:)`
! - `obj%JA(:)`
! in CSR format. This routine also set data inside `obj%ColSize(:)` and
! `obj%RowSize(:) `, and `obj%DiagIndx(:)`

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity5(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetSparsity5
END INTERFACE SetSparsity

END MODULE CSRMatrix_SparsityMethods
