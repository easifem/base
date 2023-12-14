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

MODULE CSRMatrix_GetBlockRowMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: GetBlockRow

!----------------------------------------------------------------------------
!                                                     getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of given block matrix
! - This routine is designed to handle block matrices, which
! means it only works when StorageFMT is DOF_FMT
! - jvar is the column number for the block matrix, whose row we are
! extracting
! - the result is returned inside `value`.
! - `value` should be allocated
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow1(obj, jvar, irow, VALUE, scale, &
    & addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: irow
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow1
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow1
END INTERFACE getBlockRow

!----------------------------------------------------------------------------
!                                                     getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of given block matrix
! - This routine is designed to handle block matrices, which
! means it only works when StorageFMT is DOF_FMT
! - jvar is the column number for the block matrix, whose row we are
! extracting
! - the result is returned inside `value`.
! - `value` should be allocated
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow1b(obj, jvar, irow, VALUE, scale, &
    & addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: irow(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow1b
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow1b
END INTERFACE getBlockRow

!----------------------------------------------------------------------------
!                                                     getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of given block matrix
! - This routine is designed to handle block matrices, which
! means it only works when StorageFMT is DOF_FMT
! - jvar is the column number for the block matrix, whose row we are
! extracting
! - the result is returned inside `value`.
! - `value` should be allocated
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow2(obj, jvar, nodenum, idof, VALUE, &
    & scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow2
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow2
END INTERFACE getBlockRow

!----------------------------------------------------------------------------
!                                                    getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of a sparse matrix. The row index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number of physical variable ivar
! - `idof` should be between 1 and the total number of dof in ivar
! - `irow` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, ivar, idof, and nodenum is used to calculate the index of
! physical variable ivar and its degree of freedom
!

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow3(obj, ivar, jvar, nodenum, idof, &
    & VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow3
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow3
END INTERFACE getBlockRow

!----------------------------------------------------------------------------
!                                                    getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of a sparse matrix. The row index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number of physical variable ivar
! - `idof` should be between 1 and the total number of dof in ivar
! - `irow` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, ivar, idof, and nodenum is used to calculate the index of
! physical variable ivar and its degree of freedom
!

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow4(obj, ivar, jvar, nodenum, &
    & spacecompo, timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow4
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow4
END INTERFACE getBlockRow

!----------------------------------------------------------------------------
!                                                    getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of a sparse matrix. The row index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number of physical variable ivar
! - `idof` should be between 1 and the total number of dof in ivar
! - `irow` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, ivar, idof, and nodenum is used to calculate the index of
! physical variable ivar and its degree of freedom
!

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow5(obj, ivar, jvar, nodenum, &
    & spacecompo, timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow5
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow5
END INTERFACE getBlockRow

!----------------------------------------------------------------------------
!                                                    getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of a sparse matrix. The row index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number of physical variable ivar
! - `idof` should be between 1 and the total number of dof in ivar
! - `irow` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, ivar, idof, and nodenum is used to calculate the index of
! physical variable ivar and its degree of freedom
!

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow6(obj, ivar, jvar, nodenum, &
    & spacecompo, timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow6
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow6
END INTERFACE getBlockRow

!----------------------------------------------------------------------------
!                                                    getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of a sparse matrix. The row index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number of physical variable ivar
! - `idof` should be between 1 and the total number of dof in ivar
! - `irow` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, ivar, idof, and nodenum is used to calculate the index of
! physical variable ivar and its degree of freedom
!

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow7(obj, ivar, jvar, nodenum, &
    & spacecompo, timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow7
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow7
END INTERFACE getBlockRow

!----------------------------------------------------------------------------
!                                                    getBlockRow@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix
!
!# Introduction
!
! - This routine returns the row of a sparse matrix. The row index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number of physical variable ivar
! - `idof` should be between 1 and the total number of dof in ivar
! - `irow` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, ivar, idof, and nodenum is used to calculate the index of
! physical variable ivar and its degree of freedom

INTERFACE
  MODULE SUBROUTINE csrMat_getBlockRow8(obj, ivar, jvar, nodenum, &
    & spacecompo, timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getBlockRow8
END INTERFACE

INTERFACE getBlockRow
  MODULE PROCEDURE csrMat_getBlockRow8
END INTERFACE getBlockRow

END MODULE CSRMatrix_GetBlockRowMethods
