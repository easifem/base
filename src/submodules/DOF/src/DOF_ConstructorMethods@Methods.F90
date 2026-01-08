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

MODULE PROCEDURE obj_Initiate1
INTEGER(I4B) :: n, i, k, j

!> main&
obj%storageFMT = storageFMT
n = SIZE(names)
CALL Reallocate(obj%map, n + 1, 6)
ASSOCIATE (map => obj%map)

  !<- names in ascii code
  map(1:n, 1) = ICHAR(names(1:n))
  map(1 + n, 1) = 0

  !<- Space components; -1 if scalar component like pressure
  map(1:n, 2) = spacecompo
  map(1 + n, 2) = 0

  ! <- Time component; 1 if time invariant
  map(1:n, 3) = timecompo
  map(1 + n, 3) = 0

  !<- tDOF for each physical name
  DO i = 1, n
    IF (spacecompo(i) < 0) THEN
      map(i, 4) = timecompo(i)
    ELSE
      map(i, 4) = timecompo(i) * spacecompo(i)
    END IF
  END DO
  map(n + 1, 4) = SUM(map(1:n, 4))

  !<- Here we set Indx
  map(1, 5) = 1
  DO i = 2, n + 1
    map(i, 5) = map(i - 1, 5) + map(i - 1, 4)
  END DO

  !<- tNodes
  map(1:n, 6) = tNodes
  map(n + 1, 6) = SUM(map(1:n, 6) * map(1:n, 4))

  !<- valMap( tDOF + 1, 2 )
  CALL Reallocate(obj%valMap, map(n + 1, 4) + 1)
  obj%valMap(1) = 1
  k = 1
  DO i = 1, n
    DO j = 1, map(i, 4)
      k = k + 1
      obj%valMap(k) = obj%valMap(k - 1) + map(i, 6)
    END DO
  END DO
END ASSOCIATE
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CALL Reallocate(val, .tNodes.obj)
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CALL Reallocate(val1, .tNodes.obj, val2, .tNodes.obj)
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
obj1%storageFMT = obj2%storageFMT
IF (ALLOCATED(obj2%valMap)) obj1%valMap = obj2%valMap
IF (ALLOCATED(obj2%map)) obj1%map = obj2%map
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                       DOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor1
CALL Initiate(obj=obj, names=names, tNodes=tNodes, &
              spacecompo=spacecompo, timecompo=timecompo, &
              storageFMT=storageFMT)
END PROCEDURE obj_Constructor1

!----------------------------------------------------------------------------
!                                                                DOF_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Constructor_1
ALLOCATE (obj)
CALL Initiate(obj=obj, names=names, tNodes=tNodes, &
              spacecompo=spacecompo, timecompo=timecompo, &
              storageFMT=storageFMT)
END PROCEDURE obj_Constructor_1

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
IF (ALLOCATED(obj%map)) DEALLOCATE (obj%map)
IF (ALLOCATED(obj%valMap)) DEALLOCATE (obj%valMap)
END PROCEDURE obj_Deallocate

END SUBMODULE Methods
