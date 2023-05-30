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

MODULE CSRMatrix_SetBlockColMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_

PUBLIC :: SetBlockColumn

!----------------------------------------------------------------------------
!                                                  setBlockColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setBlockColumn1(obj, ivar, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: icolumn
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setBlockColumn1
END INTERFACE

INTERFACE setBlockColumn
  MODULE PROCEDURE csrMat_setBlockColumn1
END INTERFACE setBlockColumn

!----------------------------------------------------------------------------
!                                                  setBlockColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setBlockColumn2(obj, ivar, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: icolumn
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setBlockColumn2
END INTERFACE

INTERFACE setBlockColumn
  MODULE PROCEDURE csrMat_setBlockColumn2
END INTERFACE setBlockColumn

!----------------------------------------------------------------------------
!                                                  setBlockColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setBlockColumn3(obj, ivar, jvar, nodenum, idof, &
    & VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setBlockColumn3
END INTERFACE

INTERFACE setBlockColumn
  MODULE PROCEDURE csrMat_setBlockColumn3
END INTERFACE setBlockColumn

!----------------------------------------------------------------------------
!                                                   setBlockColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setBlockColumn4(obj, ivar, jvar, nodenum, idof, &
    & VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setBlockColumn4
END INTERFACE

INTERFACE setBlockColumn
  MODULE PROCEDURE csrMat_setBlockColumn4
END INTERFACE setBlockColumn

!----------------------------------------------------------------------------
!                                                  setBlockColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setBlockColumn5(obj, ivar, jvar, nodenum,  &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE csrMat_setBlockColumn5
END INTERFACE

INTERFACE setBlockColumn
  MODULE PROCEDURE csrMat_setBlockColumn5
END INTERFACE setBlockColumn

!----------------------------------------------------------------------------
!                                                 setBlockColumn@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
  MODULE SUBROUTINE csrMat_setBlockColumn6(obj, ivar, jvar, nodenum, &
    & spacecompo, timecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE csrMat_setBlockColumn6
END INTERFACE

INTERFACE setBlockColumn
  MODULE PROCEDURE csrMat_setBlockColumn6
END INTERFACE setBlockColumn

END MODULE CSRMatrix_SetBlockColMethods
