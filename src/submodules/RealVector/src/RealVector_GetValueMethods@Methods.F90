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

SUBMODULE(RealVector_GetValueMethods) Methods
USE GlobalData, ONLY: DOF_FMT, NODES_FMT

USE DOF_Method, ONLY: GetIDOF, &
                      GetNodeLoc, &
                      GetIndex, &
                      OPERATOR(.tdof.), &
                      OPERATOR(.tnodes.), &
                      DOF_GetValue => GetValue, &
                      DOF_GetValue_ => GetValue_

USE ReallocateUtility, ONLY: Reallocate

USE F95_BLAS, ONLY: COPY

USE RealVector_SetMethods, ONLY: Set

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue1
CALL Set(obj=VALUE, VALUE=obj%val, istart=istart, iend=iend, stride=stride)
END PROCEDURE obj_GetValue1

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue2
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=dofobj, idof=idof)
CALL Set(obj=VALUE, VALUE=obj%val, istart=s(1), iend=s(2), stride=s(3))
END PROCEDURE obj_GetValue2

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue3
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=dofobj, &
               idof=GetIDOF(obj=dofobj, ivar=ivar, idof=idof))
CALL Set(obj=VALUE, VALUE=obj%val, istart=s(1), iend=s(2), stride=s(3))
END PROCEDURE obj_GetValue3

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue4
INTEGER(I4B) :: s(3)

s = GetNodeLoc(obj=dofobj, idof=GetIDOF(obj=dofobj, &
                                        ivar=ivar, &
                                        spaceCompo=spaceCompo, &
                                        timeCompo=timeCompo))

CALL Set(obj=VALUE, VALUE=obj%val, istart=s(1), iend=s(2), stride=s(3))
END PROCEDURE obj_GetValue4

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue5
CALL Set(obj1=VALUE, dofobj1=dofvalue, idof1=idofvalue, &
         obj2=obj, dofobj2=dofobj, idof2=idofobj)
END PROCEDURE obj_GetValue5

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue6
INTEGER(I4B) :: ii

DO ii = 1, SIZE(idofobj)
  CALL Set(obj1=VALUE, dofobj1=dofvalue, idof1=idofvalue(ii), &
           obj2=obj, dofobj2=dofobj, idof2=idofobj(ii))
END DO

END PROCEDURE obj_GetValue6

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue7
INTEGER(I4B) :: global_idofobj, global_idofvalue
global_idofobj = GetIDOF(obj=dofobj, ivar=ivarobj, idof=idofobj)
global_idofvalue = GetIDOF(obj=dofvalue, ivar=ivarvalue, idof=idofvalue)
CALL Set(obj1=VALUE, dofobj1=dofvalue, idof1=global_idofvalue, &
         obj2=obj, dofobj2=dofobj, idof2=global_idofobj)
END PROCEDURE obj_GetValue7

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue8
INTEGER(I4B) :: global_idofobj, global_idofvalue, ii

DO ii = 1, SIZE(idofobj)
  global_idofobj = GetIDOF(obj=dofobj, ivar=ivarobj, idof=idofobj(ii))
  global_idofvalue = GetIDOF(obj=dofvalue, ivar=ivarvalue, idof=idofvalue(ii))
  CALL Set(obj1=VALUE, dofobj1=dofvalue, idof1=global_idofvalue, &
           obj2=obj, dofobj2=dofobj, idof2=global_idofobj)
END DO

END PROCEDURE obj_GetValue8

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue9
INTEGER(I4B) :: global_idofobj, global_idofvalue

global_idofobj = GetIDOF(obj=dofobj, ivar=ivarobj, spaceCompo=spaceCompoObj, &
                         timeCompo=timeCompoObj)

global_idofvalue = GetIDOF(obj=dofvalue, ivar=ivarvalue, &
                         spaceCompo=spaceCompoValue, timeCompo=timeCompoValue)

CALL Set(obj1=VALUE, dofobj1=dofvalue, idof1=global_idofvalue, &
         obj2=obj, dofobj2=dofobj, idof2=global_idofobj)

END PROCEDURE obj_GetValue9

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue10
INTEGER(I4B) :: global_idofobj, global_idofvalue, ii

DO ii = 1, SIZE(timeCompoObj)
  global_idofobj = GetIDOF(obj=dofobj, ivar=ivarobj, &
                         spaceCompo=spaceCompoObj, timeCompo=timeCompoObj(ii))

  global_idofvalue = GetIDOF(obj=dofvalue, ivar=ivarvalue, &
                     spaceCompo=spaceCompoValue, timeCompo=timeCompoValue(ii))

  CALL Set(obj1=VALUE, dofobj1=dofvalue, idof1=global_idofvalue, &
           obj2=obj, dofobj2=dofobj, idof2=global_idofobj)
END DO

END PROCEDURE obj_GetValue10

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue11
INTEGER(I4B) :: global_idofobj, global_idofvalue, ii

DO ii = 1, SIZE(spaceCompoObj)

  global_idofobj = GetIDOF(obj=dofobj, ivar=ivarobj, &
                         spaceCompo=spaceCompoObj(ii), timeCompo=timeCompoObj)

  global_idofvalue = GetIDOF(obj=dofvalue, ivar=ivarvalue, &
                     spaceCompo=spaceCompoValue(ii), timeCompo=timeCompoValue)

  CALL Set(obj1=VALUE, dofobj1=dofvalue, idof1=global_idofvalue, &
           obj2=obj, dofobj2=dofobj, idof2=global_idofobj)

END DO

END PROCEDURE obj_GetValue11

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue12
CALL DOF_GetValue(v=VALUE, val=obj%val, obj=dofobj, idof=idof, &
                  storageFMT=storageFMT, nodenum=nodenum)
END PROCEDURE obj_GetValue12

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_12
CALL DOF_GetValue_(v=VALUE, tsize=tsize, val=obj%val, obj=dofobj, idof=idof, &
                   storageFMT=storageFMT, nodenum=nodenum)
END PROCEDURE obj_GetValue_12

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue13
CALL DOF_GetValue(v=VALUE, val=obj%val, obj=dofobj, idof=idof, &
                  storageFMT=storageFMT)
END PROCEDURE obj_GetValue13

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_13
CALL DOF_GetValue_(v=VALUE, tsize=tsize, val=obj%val, obj=dofobj, idof=idof, &
                   storageFMT=storageFMT)
END PROCEDURE obj_GetValue_13

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue14
CALL DOF_GetValue(v=VALUE, val=obj%val, obj=dofobj, idof=idof, &
                  force3D=force3D)
END PROCEDURE obj_GetValue14

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_14
CALL DOF_GetValue_(v=VALUE, val=obj%val, obj=dofobj, idof=idof, &
                   force3D=force3D, nrow=nrow, ncol=ncol)
END PROCEDURE obj_GetValue_14

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue15
VALUE = obj%val(GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar, &
                         idof=idof))
END PROCEDURE obj_GetValue15

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue16
VALUE = obj%val(GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar, idof=idof))
END PROCEDURE obj_GetValue16

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_16
INTEGER(I4B) :: global_idof
global_idof = GetIDOF(obj=dofobj, ivar=ivar, idof=idof)
CALL DOF_GetValue_(obj=dofobj, nodenum=nodenum, idof=global_idof, &
                   v=VALUE, tsize=tsize, val=obj%val)
END PROCEDURE obj_GetValue_16

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue17
VALUE = obj%val(GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar))
END PROCEDURE obj_GetValue17

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_17
INTEGER(I4B), ALLOCATABLE :: idof(:)
idof = GetIDOF(obj=dofobj, ivar=ivar)
CALL GetValue_(obj=obj, dofobj=dofobj, nodenum=nodenum, idof=idof, &
               VALUE=VALUE, tsize=tsize, &
               storageFMT=dofobj%storageFMT)
END PROCEDURE obj_GetValue_17

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue18
VALUE = obj%val(GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar, &
                         spaceCompo=spaceCompo, timeCompo=timeCompo))
END PROCEDURE obj_GetValue18

!----------------------------------------------------------------------------
!                                                               GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_18
INTEGER(I4B) :: idof
idof = GetIDOF(obj=dofobj, ivar=ivar, spaceCompo=spaceCompo, &
               timeCompo=timeCompo)
CALL DOF_GetValue_(v=VALUE, val=obj%val, obj=dofobj, idof=idof, &
                   nodenum=nodenum, tsize=tsize)
END PROCEDURE obj_GetValue_18

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue19
INTEGER(I4B) :: s(3), tsize

tsize = dofobj.tNodes.idof
CALL Reallocate(VALUE, tsize)
CALL obj_GetValue_19(obj=obj, dofobj=dofobj, VALUE=VALUE, tsize=tsize, &
                     idof=idof)

END PROCEDURE obj_GetValue19

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_19
CALL DOF_GetValue_(v=VALUE, val=obj%val, obj=dofobj, idof=idof, &
                   tsize=tsize, isidof=.TRUE.)
END PROCEDURE obj_GetValue_19

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue20
INTEGER(I4B) :: global_idof
global_idof = GetIDOF(obj=dofobj, ivar=ivar, idof=idof)
CALL GetValue(obj=obj, dofobj=dofobj, idof=global_idof, &
              VALUE=VALUE)
END PROCEDURE obj_GetValue20

!----------------------------------------------------------------------------
!                                                               GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_20
INTEGER(I4B) :: global_idof
global_idof = GetIDOF(obj=dofobj, ivar=ivar, idof=idof)
CALL GetValue_(obj=obj, dofobj=dofobj, idof=global_idof, &
               VALUE=VALUE, tsize=tsize)
END PROCEDURE obj_GetValue_20

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue21
INTEGER(I4B) :: global_idof
global_idof = GetIDOF(obj=dofobj, ivar=ivar, spaceCompo=spaceCompo, &
                      timeCompo=timeCompo)
CALL GetValue(obj=obj, dofobj=dofobj, idof=global_idof, &
              VALUE=VALUE)
END PROCEDURE obj_GetValue21

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_21
INTEGER(I4B) :: global_idof
global_idof = GetIDOF(obj=dofobj, ivar=ivar, spaceCompo=spaceCompo, &
                      timeCompo=timeCompo)
CALL GetValue_(obj=obj, dofobj=dofobj, idof=global_idof, &
               VALUE=VALUE, tsize=tsize)
END PROCEDURE obj_GetValue_21

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue22
INTEGER(I4B) :: tsize
tsize = SIZE(idof) * SIZE(nodenum)
CALL Reallocate(VALUE, tsize)
CALL GetValue_(obj=obj, dofobj=dofobj, idof=idof, VALUE=VALUE, tsize=tsize, &
               nodenum=nodenum)
END PROCEDURE obj_GetValue22

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_22
CALL DOF_GetValue_(v=VALUE, val=obj%val, obj=dofobj, idof=idof, &
                   tsize=tsize, nodenum=nodenum, storageFMT=dofobj%storageFMT)
END PROCEDURE obj_GetValue_22

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue23
INTEGER(I4B) :: tsize
tsize = dofobj.tNodes.idof
CALL Reallocate(VALUE, tsize)
CALL GetValue_(obj=obj, dofobj=dofobj, idof=idof, VALUE=VALUE, tsize=tsize)
END PROCEDURE obj_GetValue23

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_23
CALL DOF_GetValue_(obj=dofobj, val=obj%val, v=VALUE, idof=idof, tsize=tsize, &
                   storageFMT=dofobj%StorageFMT)
END PROCEDURE obj_GetValue_23

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue24
CALL COPY(Y=VALUE%val, X=obj%val)
END PROCEDURE obj_GetValue24

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
