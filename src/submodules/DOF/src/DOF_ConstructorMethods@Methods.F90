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

SUBMODULE(DOF_ConstructorMethods) Methods
USE ReallocateUtility, ONLY: Reallocate
USE DOF_GetMethods, ONLY: OPERATOR(.tNodes.)

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate1
INTEGER(I4B) :: n, i, k, j
!> main
obj%StorageFMT = StorageFMT; n = SIZE(Names)
CALL Reallocate(obj%Map, n + 1, 6)
ASSOCIATE (Map => obj%Map)

  !<- Names in ascii code
  Map(1:n, 1) = ICHAR(Names(1:n))
  Map(1 + n, 1) = 0

  !<- Space components; -1 if scalar component like pressure
  Map(1:n, 2) = spacecompo
  Map(1 + n, 2) = 0

  ! <- Time component; 1 if time invariant
  Map(1:n, 3) = timecompo
  Map(1 + n, 3) = 0

  !<- tDOF for each physical name
  DO i = 1, n
    IF (spacecompo(i) .LT. 0) THEN
      Map(i, 4) = timecompo(i)
    ELSE
      Map(i, 4) = timecompo(i) * spacecompo(i)
    END IF
  END DO
  Map(n + 1, 4) = SUM(Map(1:n, 4))

  !<- Here we set Indx
  Map(1, 5) = 1
  DO i = 2, n + 1
    Map(i, 5) = Map(i - 1, 5) + Map(i - 1, 4)
  END DO

  !<- tNodes
  Map(1:n, 6) = tNodes
  Map(n + 1, 6) = SUM(Map(1:n, 6) * Map(1:n, 4))

  !<- ValMap( tDOF + 1, 2 )
  CALL Reallocate(obj%ValMap, Map(n + 1, 4) + 1)
  obj%ValMap(1) = 1; k = 1
  DO i = 1, n
    DO j = 1, Map(i, 4)
      k = k + 1
      obj%ValMap(k) = obj%ValMap(k - 1) + Map(i, 6)
    END DO
  END DO
END ASSOCIATE
END PROCEDURE obj_initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate2
CALL Reallocate(Val, .tNodes.obj)
END PROCEDURE obj_initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate3
CALL Reallocate(Val1, .tNodes.obj, Val2, .tNodes.obj)
END PROCEDURE obj_initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate4
obj1%StorageFMT = obj2%StorageFMT
IF (ALLOCATED(obj2%valmap)) obj1%valmap = obj2%valmap
IF (ALLOCATED(obj2%map)) obj1%map = obj2%map
END PROCEDURE obj_initiate4

!----------------------------------------------------------------------------
!                                                                       DOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL Initiate(obj=obj, Names=Names, tNodes=tNodes, &
  & spacecompo=spacecompo, timecompo=timecompo, &
  & StorageFMT=StorageFMT)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                                DOF_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (obj)
CALL Initiate(obj=obj, Names=Names, tNodes=tNodes, &
  & spacecompo=spacecompo, timecompo=timecompo, &
  & StorageFMT=StorageFMT)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
IF (ALLOCATED(obj%Map)) DEALLOCATE (obj%Map)
IF (ALLOCATED(obj%ValMap)) DEALLOCATE (obj%ValMap)
END PROCEDURE obj_Deallocate

END SUBMODULE Methods
