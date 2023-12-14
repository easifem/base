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

MODULE CSRMatrix_SetRowMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the the row of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setRow1(obj, irow, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow1
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow1
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the the row of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setRow1b(obj, irow, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: irow(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow1b
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow1b
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `irow` calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow2(obj, nodenum, idof, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow2
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow2
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the the row of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setRow3(obj, irow, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow3
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow3
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the the row of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setRow3b(obj, irow, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: irow(:)
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow3b
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow3b
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow4(obj, nodenum, idof, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow4
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow4
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow5(obj, nodenum, ivar, idof, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow5
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow5
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow6(obj, nodenum, ivar, idof, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow6
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow6
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow7(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow7
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow7
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow8(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow8
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow8
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow9(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow9
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow9
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow10(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow10
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow10
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow11(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow11
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow11
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow12(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow12
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow12
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow13(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow13
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow13
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow14(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow14
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow14
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow15(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setRow15
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow15
END INTERFACE setRow

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
!
! - This routine sets the row of a sparse matrix. The row index is calculated
! using the nodenum and idof.
! - nodenum is the node number
! - idof is the degree of freedom number
! - irow calculated from nodenum and idof depends upon the storageFMT.

INTERFACE
  MODULE SUBROUTINE csrMat_setRow16(obj, nodenum, ivar, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setRow16
END INTERFACE

INTERFACE setRow
  MODULE PROCEDURE csrMat_setRow16
END INTERFACE setRow
END MODULE CSRMatrix_SetRowMethods
