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

!> author: Vikas Sharma, Ph. D.
! date: 13 Jul 2021
! summary: Input output related methods

SUBMODULE(CSRSparsity_Method) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_Display
CALL Display(Msg, unitNo=unitNo)
CALL Display(obj%nnz, "# NNZ : ", unitNo=unitNo)
CALL Display(obj%ncol, "# NCOL : ", unitNo=unitNo)
CALL Display(obj%nrow, "# NROW : ", unitNo=unitNo)
CALL Display(obj%dof, "# DOF : ", unitNo=unitNo)
!!
IF (ALLOCATED(obj%IA)) THEN
  CALL Display(obj%IA, "# IA : ", unitNo=unitNo, advance="NO")
ELSE
  CALL Display("# IA is not allocated", UnitNo=UnitNo)
END IF
!!
IF (ALLOCATED(obj%JA)) THEN
  CALL Display(obj%JA, "# JA : ", unitNo=unitNo, advance="NO")
ELSE
  CALL Display("# JA is not allocated", UnitNo=UnitNo)
END IF
!!
CALL Display("", unitNo=UnitNo, advance=.TRUE.)
!!
IF (ALLOCATED(obj%idiag)) THEN
  CALL Display(obj%idiag, "# idiag : ", unitNo=unitNo)
ELSE
  CALL Display("# idiag is not allocated", UnitNo=UnitNo)
END IF
!!
IF (ALLOCATED(obj%row)) THEN
  CALL Display(obj%row, "# ROW : ", unitNo=unitNo, orient="ROW")
END IF
!!
END PROCEDURE csr_Display

END SUBMODULE IOMethods
