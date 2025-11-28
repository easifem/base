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
! date:         28 Feb 2021
! summary:         This submodule contains IO method for [[DOF_]]

SUBMODULE(DOF_IOMethods) Methods
USE Display_Method, ONLY: MyDisplay => Display
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: OPERATOR(.tNames.)
USE DOF_Method, ONLY: GetNodeLoc
USE GlobalData, ONLY: FMT_DOF, FMT_NODES
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_Display1
INTEGER(I4B) :: n, j
LOGICAL(LGT) :: isok

CALL MyDisplay(msg, unitNo=unitNo)

isok = ALLOCATED(obj%map)
CALL MyDisplay(isok, "obj%map allocated: ", UnitNo=UnitNo)
IF (.NOT. isok) RETURN

n = SIZE(obj%map, 1) - 1
CALL MyDisplay(n, "Total Physical Variables :", unitNo=unitNo)

DO j = 1, n
  CALL MyDisplay("Name : "//CHAR(obj%map(j, 1)), unitNo=unitNo)

  IF (obj%map(j, 2) .LT. 0) THEN
    CALL MyDisplay("Space Components : "//"Scalar", unitNo=unitNo)
  ELSE
    CALL MyDisplay(obj%map(j, 2), "Space Components : ", unitNo=unitNo)
  END IF

  CALL MyDisplay(obj%map(j, 3), "Time Components : ", unitNo=unitNo)
  CALL MyDisplay(obj%map(j, 6), "Total Nodes : ", unitNo=unitNo)
END DO

SELECT CASE (obj%StorageFMT)
CASE (FMT_DOF)
  CALL MyDisplay("Storage Format : DOF", unitNo=unitNo)
CASE (FMT_NODES)
  CALL MyDisplay("Storage Format : Nodes", unitNo=unitNo)
END SELECT

CALL MyDisplay(obj%valmap, "Value map : ", unitNo=unitNo)

END PROCEDURE dof_Display1

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_Display2
INTEGER(I4B) :: jj, tnames, idof, a(3)
!> main
CALL Display(obj, 'DOF data : ', unitNo=unitNo)

tnames = .tNames.obj

DO jj = 1, tnames
  CALL MyDisplay(ACHAR(obj%Map(jj, 1)), "VAR : ", unitNo)

  DO idof = obj%Map(jj, 5), obj%Map(jj + 1, 5) - 1
    a = GetNodeLoc(obj=obj, idof=idof)
    CALL MyDisplay(Vec(a(1):a(2):a(3)), &
                   msg="DOF-"//ToString(idof), unitNo=unitNo, advance="NO")
  END DO
  CALL MyDisplay(" ", unitNo=unitNo, advance=.TRUE.)
END DO
END PROCEDURE dof_Display2

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_Display3
IF (ALLOCATED(vec%val)) THEN
  CALL Display(vec=vec%val, obj=obj, msg=msg, unitNo=unitNo)
END IF
END PROCEDURE dof_Display3

END SUBMODULE Methods
