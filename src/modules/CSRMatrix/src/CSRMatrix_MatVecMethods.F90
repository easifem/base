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

MODULE CSRMatrix_MatVecMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: MatVec
PUBLIC :: AMatVec
PUBLIC :: AtMatvec
PUBLIC :: CSRMatrixAMUX
PUBLIC :: CSRMatrixAMUX_Add
PUBLIC :: CSRMatrixATMUX
PUBLIC :: CSRMatrixATMUX_Add

!----------------------------------------------------------------------------
!                                                           CSRMatrixAMUX
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  Mat vec

! y = A *x
INTERFACE CSRMatrixAMUX
  MODULE SUBROUTINE CSRMatrixAMUX1(n, x, y, a, ja, ia)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    REAL(DFP), INTENT(IN) :: a(:)
    INTEGER(I4B), INTENT(IN) :: ja(:)
    INTEGER(I4B), INTENT(IN) :: ia(:)
  END SUBROUTINE CSRMatrixAMUX1
END INTERFACE CSRMatrixAMUX

!----------------------------------------------------------------------------
!                                                             CSRMatrixAMUX
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  y = s * A*x

INTERFACE CSRMatrixAMUX
  MODULE SUBROUTINE CSRMatrixAMUX2(n, x, y, a, ja, ia, s)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    REAL(DFP), INTENT(IN) :: a(:)
    INTEGER(I4B), INTENT(IN) :: ja(:)
    INTEGER(I4B), INTENT(IN) :: ia(:)
    REAL(DFP), INTENT(IN) :: s
  END SUBROUTINE CSRMatrixAMUX2
END INTERFACE CSRMatrixAMUX

!----------------------------------------------------------------------------
!                                                       CSRMatrixAMUX_Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  y = y+s * A*x

INTERFACE CSRMatrixAMUX_Add
  MODULE SUBROUTINE CSRMatrixAMUX_Add_1(n, x, y, a, ja, ia, s)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    REAL(DFP), INTENT(IN) :: a(:)
    INTEGER(I4B), INTENT(IN) :: ja(:)
    INTEGER(I4B), INTENT(IN) :: ia(:)
    REAL(DFP), INTENT(IN) :: s
  END SUBROUTINE CSRMatrixAMUX_Add_1
END INTERFACE CSRMatrixAMUX_Add

!----------------------------------------------------------------------------
!                                                           CSRMatrixATMUX
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:   y = A^T *x

INTERFACE CSRMatrixATMUX
  MODULE SUBROUTINE CSRMatrixATMUX1(n, x, y, a, ja, ia)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    REAL(DFP), INTENT(IN) :: a(:)
    INTEGER(I4B), INTENT(IN) :: ja(:)
    INTEGER(I4B), INTENT(IN) :: ia(:)
  END SUBROUTINE CSRMatrixATMUX1
END INTERFACE CSRMatrixATMUX

!----------------------------------------------------------------------------
!                                                             CSRMatrixATMUX
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  y = s * A^T*x

INTERFACE CSRMatrixATMUX
  MODULE SUBROUTINE CSRMatrixATMUX2(n, x, y, a, ja, ia, s)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    REAL(DFP), INTENT(IN) :: a(:)
    INTEGER(I4B), INTENT(IN) :: ja(:)
    INTEGER(I4B), INTENT(IN) :: ia(:)
    REAL(DFP), INTENT(IN) :: s
  END SUBROUTINE CSRMatrixATMUX2
END INTERFACE CSRMatrixATMUX

!----------------------------------------------------------------------------
!                                                       CSRMatrixATMUX_Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  y = y+s * A^T*x

INTERFACE CSRMatrixATMUX_Add
  MODULE SUBROUTINE CSRMatrixATMUX_Add_1(n, x, y, a, ja, ia, s)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    REAL(DFP), INTENT(IN) :: a(:)
    INTEGER(I4B), INTENT(IN) :: ja(:)
    INTEGER(I4B), INTENT(IN) :: ia(:)
    REAL(DFP), INTENT(IN) :: s
  END SUBROUTINE CSRMatrixATMUX_Add_1
END INTERFACE CSRMatrixATMUX_Add

!----------------------------------------------------------------------------
!                                                    AMatVec1@MatvecMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x

INTERFACE AMatVec
  MODULE SUBROUTINE csrMat_AMatVec1(obj, x, y, addContribution, scale)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE csrMat_AMatVec1
END INTERFACE AMatVec

!----------------------------------------------------------------------------
!                                                    AMatVec2@MatvecMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x, A is in MSR format

INTERFACE AMatVec
  MODULE SUBROUTINE csrMat_AMatVec2(A, JA, x, y, addContribution, scale)
    REAL(DFP), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: JA(:)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE csrMat_AMatVec2
END INTERFACE AMatvec

!----------------------------------------------------------------------------
!                                                     AtMatvec@MatvecMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 july 2021
! summary: This routine computes y = A*x

INTERFACE AtMatvec
  MODULE SUBROUTINE csrMat_AtMatvec(obj, x, y, addContribution, scale)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE csrMat_AtMatvec
END INTERFACE AtMatvec

!----------------------------------------------------------------------------
!                                                             Matvec@MatVec
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine performs matrix-vector multiplication
!
!# Introduction
! y = A*x

INTERFACE MatVec
  MODULE SUBROUTINE csrMat_MatVec1(obj, x, y, isTranspose, addContribution, &
    & scale)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE csrMat_MatVec1
END INTERFACE MatVec

!----------------------------------------------------------------------------
!                                                             Matvec@MatVec
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine performs matrix-vector multiplication
!
!# Introduction
!
! y = A*x

INTERFACE MatVec
  MODULE SUBROUTINE csrMat_MatVec2(A, JA, x, y, addContribution, &
    & scale)
    REAL(DFP), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: JA(:)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: y(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE csrMat_MatVec2
END INTERFACE MatVec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSRMatrix_MatVecMethods
