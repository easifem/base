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

MODULE CSRMatrix_ReorderingMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_, SparseMatrixReOrdering_

PUBLIC :: NestedDissect

!----------------------------------------------------------------------------
!                                             NestedDissect@ReoderingMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Nested dissection using Metis library

INTERFACE
  MODULE SUBROUTINE csrMat_NestedDissect(reorder, csrMat)
    TYPE(SparseMatrixReOrdering_), INTENT(INOUT) :: reorder
    TYPE(CSRMatrix_), INTENT(IN) :: csrMat
  END SUBROUTINE csrMat_NestedDissect
END INTERFACE

INTERFACE NestedDissect
  MODULE PROCEDURE csrMat_NestedDissect
END INTERFACE NestedDissect

!----------------------------------------------------------------------------
!                                                   Display@ReorderingMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: Display the content of SparseMatrixReordering

INTERFACE
  MODULE SUBROUTINE csrMat_reorderDisplay(obj, msg, unitNo)
    TYPE(SparseMatrixReOrdering_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE csrMat_reorderDisplay
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE csrMat_reorderDisplay
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                   Permute@ReorderingMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION csrMat_Permute2(obj, rowPERM, colPERM) RESULT(Ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    TYPE(SparseMatrixReOrdering_), INTENT(IN) :: rowPERM
    TYPE(SparseMatrixReOrdering_), INTENT(IN) :: colPERM
    TYPE(CSRMatrix_) :: ans
  END FUNCTION csrMat_Permute2
END INTERFACE

INTERFACE Permute
  MODULE PROCEDURE csrMat_Permute2
END INTERFACE Permute
END MODULE CSRMatrix_ReorderingMethods
