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

MODULE CSRMatrix_SetColMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE

PRIVATE
PUBLIC :: SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn1(obj, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: icolumn
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn1
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn1b(obj, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: icolumn(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn1b
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn2(obj, nodenum, idof, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn2
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn3(obj, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: icolumn
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn3
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn3b(obj, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: icolumn(:)
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn3b
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn4(obj, nodenum, idof, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn4
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn5(obj, nodenum, ivar, idof, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn5
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn6(obj, nodenum, ivar, idof, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn6
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn7(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn7
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn8(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn8
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn9(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn9
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn10(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn10
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                      setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn11(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn11
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                   setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn12(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn12
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                      setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn13(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn13
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                      setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn14(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn14
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                     setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn15(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setColumn15
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!                                                      setColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the Column of a sparse matrix
!
!# Introduction
!
! - This routine sets the Column of a sparse matrix. The Column index is
! calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - icolumn calculated from nodenum and idof depends upon the storageFMT.

INTERFACE SetColumn
  MODULE SUBROUTINE csrMat_setColumn16(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setColumn16
END INTERFACE SetColumn

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSRMatrix_SetColMethods
