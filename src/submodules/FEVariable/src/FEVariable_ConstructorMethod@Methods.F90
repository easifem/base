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

SUBMODULE(FEVariable_ConstructorMethod) Methods
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CALL Initiate(obj=obj, s=s, defineon=defineon, vartype=vartype, rank=rank, &
              len=len)
obj%val(1:obj%len) = val(1:obj%len)
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

obj%tshape = SIZE(s)
obj%isInit = .TRUE.
obj%s(1:obj%tshape) = s(1:obj%tshape)
obj%defineon = defineon
obj%vartype = vartype
obj%rank = rank
obj%len = len
obj%capacity = TypeFEVariableOpt%capacityExpandFactor * obj%len

isok = ALLOCATED(obj%val)
IF (.NOT. isok) THEN
  CALL Reallocate(obj%val, obj%capacity)
  RETURN
END IF

tsize = SIZE(obj%val)
IF (tsize .GE. obj%len) THEN
  obj%capacity = tsize
  obj%val(1:obj%capacity) = 0.0_DFP
ELSE
  CALL Reallocate(obj%val, obj%capacity)
END IF

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%isInit = .FALSE.
obj%s = 0
obj%tshape = 0
obj%defineOn = 0
obj%vartype = 0
obj%rank = 0
obj%len = 0
obj%capacity = 0
IF (ALLOCATED(obj%val)) DEALLOCATE (obj%val)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                      Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
LOGICAL(LGT) :: isok

obj1%s = obj2%s
obj1%tshape = obj2%tshape
obj1%defineOn = obj2%defineOn
obj1%rank = obj2%rank
obj1%vartype = obj2%vartype
obj1%len = obj2%len
obj1%isInit = obj2%isInit

IF (obj1%capacity .GE. obj1%len) THEN
  obj1%val(1:obj1%len) = obj2%val(1:obj1%len)
  RETURN
END IF

obj1%capacity = TypeFEVariableOpt%capacityExpandFactor * obj1%len
CALL Reallocate(obj1%val, obj1%capacity)

isok = ALLOCATED(obj2%val)
IF (isok) obj1%val(1:obj1%len) = obj2%val(1:obj1%len)
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
