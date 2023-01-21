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

MODULE CSRMatrix_GetColMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_

PUBLIC :: GetColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the column of a sparse matrix
! - This rouine does not care about the storage pattern
! - Therefore, it should be used with care.
! - The result is returned inside the value
! - `value` should be allocated
! - the size of `value` should be atleast the number of rows in csrmatrix
!
! If addContribution is not present
! then the this routine performs the following action
!
!```fortran
! DO i = 1, obj%csr%nrow
!   value( i ) = 0.0_DFP
!   DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
!     IF( obj%csr%JA(j) .EQ. iColumn ) value( i ) = obj%A( j )
!   END DO
! END DO
!```

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn1(obj, iColumn, VALUE, scale, &
    & addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iColumn
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn1
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn1
END INTERFACE getColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the column of a sparse matrix
! - This rouine does not care about the storage pattern
! - Therefore, it should be used with care.
! - The result is returned inside the value
! - `value` should be allocated
! - the size of `value` should be atleast the number of rows in csrmatrix
!
! If addContribution is not present
! then the this routine performs the following action
!
!```fortran
! DO i = 1, obj%csr%nrow
!   value( i ) = 0.0_DFP
!   DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
!     IF( obj%csr%JA(j) .EQ. iColumn ) value( i ) = obj%A( j )
!   END DO
! END DO
!```

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn1b(obj, iColumn, VALUE, scale, &
    & addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iColumn(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn1b
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn1b
END INTERFACE getColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the `nodenum` and `idof`.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote
!
!@note
! This routine calls [[DOF_Method:GetNodeLoc]] method to calculate
! the column index from nodenum and idof
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn2(obj, nodenum, idof, VALUE, scale,  &
    & addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn2
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn2
END INTERFACE getColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the `nodenum` and `idof`.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote
!
!@note
! This routine calls [[DOF_Method:GetNodeLoc]] method to calculate
! the column index from nodenum and idof
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn3(obj, nodenum, ivar, idof, VALUE,  &
    & scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn3
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn3
END INTERFACE getColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the `nodenum` and `idof`.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote
!
!@note
! This routine calls [[DOF_Method:GetNodeLoc]] method to calculate
! the column index from nodenum and idof
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn4(obj, nodenum, ivar, spacecompo, &
    & timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn4
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn4
END INTERFACE getColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the `nodenum` and `idof`.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote
!
!@note
! This routine calls [[DOF_Method:GetNodeLoc]] method to calculate
! the column index from nodenum and idof
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn5(obj, nodenum, ivar, spacecompo, &
    & timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn5
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn5
END INTERFACE getColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the `nodenum` and `idof`.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote
!
!@note
! This routine calls [[DOF_Method:GetNodeLoc]] method to calculate
! the column index from nodenum and idof
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn6(obj, nodenum, ivar, spacecompo, &
    & timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn6
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn6
END INTERFACE getColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the `nodenum` and `idof`.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote
!
!@note
! This routine calls [[DOF_Method:GetNodeLoc]] method to calculate
! the column index from nodenum and idof
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn7(obj, nodenum, ivar, spacecompo, &
    & timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn7
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn7
END INTERFACE getColumn

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the `nodenum` and `idof`.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote
!
!@note
! This routine calls [[DOF_Method:GetNodeLoc]] method to calculate
! the column index from nodenum and idof
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_getColumn8(obj, nodenum, ivar, spacecompo, &
    & timecompo, VALUE, scale, addContribution)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE csrMat_getColumn8
END INTERFACE

INTERFACE getColumn
  MODULE PROCEDURE csrMat_getColumn8
END INTERFACE getColumn

END MODULE CSRMatrix_GetColMethods
