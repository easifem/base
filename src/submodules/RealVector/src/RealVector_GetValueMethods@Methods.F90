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

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue1
INTEGER(I4B) :: ii, jj
DO CONCURRENT(ii=istart:iend:stride)
  jj = INT((ii - istart + stride) / stride)
  VALUE%val(jj) = obj%val(ii)
END DO
END PROCEDURE obj_GetValue1

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue2
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=dofobj, idof=idof)
CALL GetValue(obj=obj, VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3))
END PROCEDURE obj_GetValue2

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue3
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=dofobj, &
               idof=GetIDOF(obj=dofobj, ivar=ivar, idof=idof))
CALL GetValue(obj=obj, VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3))
END PROCEDURE obj_GetValue3

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue4
INTEGER(I4B) :: s(3)

s = GetNodeLoc(obj=dofobj, idof=GetIDOF(obj=dofobj, &
                                        ivar=ivar, &
                                        spacecompo=spacecompo, &
                                        timecompo=timecompo))

CALL GetValue(obj=obj, VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3))
END PROCEDURE obj_GetValue4

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue5
INTEGER(I4B) :: p(3), s(3), ii, jj

s = GetNodeLoc(obj=dofobj, idof=idofobj)
p = GetNodeLoc(obj=dofvalue, idof=idofvalue)

DO CONCURRENT(ii=s(1):s(2):s(3))
  jj = INT((ii - s(1) + s(3)) / s(3))
  VALUE%val(p(1) + (jj - 1) * p(3)) = obj%val(ii)
END DO

END PROCEDURE obj_GetValue5

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue6
INTEGER(I4B) :: p(3), s(3), ii, jj, kk, ll

ll = SIZE(idofobj)

DO CONCURRENT(kk=1:ll)

  s = GetNodeLoc(obj=dofobj, idof=idofobj(kk))
  p = GetNodeLoc(obj=dofvalue, idof=idofvalue(kk))

  DO ii = s(1), s(2), s(3)
    jj = INT((ii - s(1) + s(3)) / s(3))
    VALUE%val(p(1) + (jj - 1) * p(3)) = obj%val(ii)
  END DO

END DO

END PROCEDURE obj_GetValue6

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue7
INTEGER(I4B) :: p(3), s(3), ii, jj

s = GetNodeLoc(obj=dofobj, idof=GetIDOF(obj=dofobj, &
                                        ivar=ivarobj, idof=idofobj))

p = GetNodeLoc(obj=dofvalue, idof=GetIDOF(obj=dofvalue, &
                                          ivar=ivarvalue, &
                                          idof=idofvalue))

DO CONCURRENT(ii=s(1):s(2):s(3))
  jj = INT((ii - s(1) + s(3)) / s(3))
  VALUE%val(p(1) + (jj - 1) * p(3)) = obj%val(ii)
END DO

END PROCEDURE obj_GetValue7

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue8
INTEGER(I4B) :: p(3), s(3), ii, jj, kk

DO kk = 1, SIZE(idofobj)

  s = GetNodeLoc(obj=dofobj, idof=GetIDOF(obj=dofobj, &
                                          ivar=ivarobj, &
                                          idof=idofobj(kk)))

  p = GetNodeLoc(obj=dofvalue, idof=GetIDOF(obj=dofvalue, &
                                            ivar=ivarvalue, &
                                            idof=idofvalue(kk)))

  jj = 0

  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    VALUE%val(p(1) + (jj - 1) * p(3)) = obj%val(ii)
  END DO

END DO

END PROCEDURE obj_GetValue8

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue9
INTEGER(I4B) :: p(3), s(3), ii, jj

s = GetNodeLoc(obj=dofobj, idof=GetIDOF(obj=dofobj, &
                                        ivar=ivarobj, &
                                        spacecompo=spacecompoobj, &
                                        timecompo=timecompoobj))

p = GetNodeLoc(obj=dofvalue, idof=GetIDOF(obj=dofvalue, &
                                          ivar=ivarvalue, &
                                          spacecompo=spacecompovalue, &
                                          timecompo=timecompovalue))

jj = 0

DO ii = s(1), s(2), s(3)
  jj = jj + 1
  VALUE%val(p(1) + (jj - 1) * p(3)) = obj%val(ii)
END DO

END PROCEDURE obj_GetValue9

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue10
INTEGER(I4B) :: p(3), s(3), ii, jj, kk

DO kk = 1, SIZE(timecompoobj)

  s = GetNodeLoc(obj=dofobj, idof=GetIDOF(obj=dofobj, &
                                          ivar=ivarobj, &
                                          spacecompo=spacecompoobj, &
                                          timecompo=timecompoobj(kk)))

  p = GetNodeLoc(obj=dofvalue, idof=GetIDOF(obj=dofvalue, &
                                            ivar=ivarvalue, &
                                            spacecompo=spacecompovalue, &
                                            timecompo=timecompovalue(kk)))

  jj = 0

  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    VALUE%val(p(1) + (jj - 1) * p(3)) = obj%val(ii)
  END DO

END DO

END PROCEDURE obj_GetValue10

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue11
INTEGER(I4B) :: p(3), s(3), ii, jj, kk

DO kk = 1, SIZE(spacecompoobj)

  s = GetNodeLoc(obj=dofobj, idof=GetIDOF(obj=dofobj, &
                                          ivar=ivarobj, &
                                          spacecompo=spacecompoobj(kk), &
                                          timecompo=timecompoobj))

  p = GetNodeLoc(obj=dofvalue, idof=GetIDOF(obj=dofvalue, &
                                            ivar=ivarvalue, &
                                            spacecompo=spacecompovalue(kk), &
                                            timecompo=timecompovalue))

  jj = 0

  DO ii = s(1), s(2), s(3)
    jj = jj + 1
    VALUE%val(p(1) + (jj - 1) * p(3)) = obj%val(ii)
  END DO

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

MODULE PROCEDURE realVec_GetValue15
VALUE = obj%val(GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar, &
                         idof=idof))
END PROCEDURE realVec_GetValue15

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue16
VALUE = obj%val(GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar, idof=idof))
END PROCEDURE realVec_GetValue16

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue_16
INTEGER(I4B) :: global_idof
global_idof = GetIDOF(obj=dofobj, ivar=ivar, idof=idof)
CALL DOF_GetValue_(obj=dofobj, nodenum=nodenum, idof=global_idof, &
                   v=VALUE, tsize=tsize, val=obj%val)
END PROCEDURE realVec_GetValue_16

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue17
VALUE = obj%val(GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar))
END PROCEDURE realVec_GetValue17

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue_17
INTEGER(I4B), ALLOCATABLE :: indx(:)
indx = GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar)
tsize = SIZE(indx)
VALUE(1:tsize) = obj%val(indx)
END PROCEDURE realVec_GetValue_17

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue18
VALUE = obj%val(GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar, &
                         spacecompo=spacecompo, timecompo=timecompo))
END PROCEDURE realVec_GetValue18

!----------------------------------------------------------------------------
!                                                               GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue_18
INTEGER(I4B), ALLOCATABLE :: indx(:)
indx = GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar, &
                spacecompo=spacecompo, timecompo=timecompo)
tsize = SIZE(indx)
VALUE(1:tsize) = obj%val(indx)
END PROCEDURE realVec_GetValue_18

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue19
INTEGER(I4B) :: s(3), ii, jj

s = GetNodeLoc(obj=dofobj, idof=idof)
CALL Reallocate(VALUE, dofobj.tNodes.idof)

jj = 0

DO ii = s(1), s(2), s(3)
  jj = jj + 1
  VALUE(jj) = obj%val(ii)
END DO

END PROCEDURE realVec_GetValue19

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue_19
INTEGER(I4B) :: s(3), ii, jj

s = GetNodeLoc(obj=dofobj, idof=idof)
tsize = dofobj.tNodes.idof

jj = 0
DO ii = s(1), s(2), s(3)
  jj = jj + 1
  VALUE(jj) = obj%val(ii)
END DO

END PROCEDURE realVec_GetValue_19

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue20
INTEGER(I4B) :: s(3), ii, jj

ii = GetIDOF(obj=dofobj, ivar=ivar, idof=idof)
s = GetNodeLoc(obj=dofobj, idof=ii)
CALL Reallocate(VALUE, dofobj.tNodes.ii)

jj = 0

DO ii = s(1), s(2), s(3)
  jj = jj + 1
  VALUE(jj) = obj%val(ii)
END DO

END PROCEDURE realVec_GetValue20

!----------------------------------------------------------------------------
!                                                               GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue_20
INTEGER(I4B) :: s(3), ii, jj

ii = GetIDOF(obj=dofobj, ivar=ivar, idof=idof)
tsize = dofobj.tNodes.ii

s = GetNodeLoc(obj=dofobj, idof=ii)

jj = 0
DO ii = s(1), s(2), s(3)
  jj = jj + 1
  VALUE(jj) = obj%val(ii)
END DO

END PROCEDURE realVec_GetValue_20

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue21
INTEGER(I4B) :: s(3), ii, jj

ii = GetIDOF(obj=dofobj, ivar=ivar, spacecompo=spacecompo, &
             timecompo=timecompo)
s = GetNodeLoc(obj=dofobj, idof=ii)
CALL Reallocate(VALUE, dofobj.tNodes.ii)

jj = 0

DO ii = s(1), s(2), s(3)
  jj = jj + 1
  VALUE(jj) = obj%val(ii)
END DO

END PROCEDURE realVec_GetValue21

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_GetValue_21
INTEGER(I4B) :: s(3), ii, jj

ii = GetIDOF(obj=dofobj, ivar=ivar, spacecompo=spacecompo, &
             timecompo=timecompo)
tsize = dofobj.tNodes.ii

s = GetNodeLoc(obj=dofobj, idof=ii)

jj = 0

DO ii = s(1), s(2), s(3)
  jj = jj + 1
  VALUE(jj) = obj%val(ii)
END DO

END PROCEDURE realVec_GetValue_21

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue22
INTEGER(I4B) :: tsize
CALL Reallocate(VALUE, SIZE(idof) * SIZE(nodenum))
CALL GetValue_(obj, dofobj, idof, VALUE, tsize, nodenum)
END PROCEDURE obj_GetValue22

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_22
INTEGER(I4B) :: m, n, i, k, tdof

m = SIZE(idof)
n = SIZE(nodenum)

tsize = m * n

SELECT CASE (dofobj%StorageFMT)

CASE (DOF_FMT)

  DO i = 1, m
  VALUE((i - 1) * n + 1:i * n) = obj%val(nodenum + dofobj%valmap(idof(i)) - 1)
  END DO

CASE (NODES_FMT)

  tdof = .tdof.dofobj

  DO i = 1, n
    DO k = 1, m
      VALUE((i - 1) * m + k) = obj%val((nodenum(i) - 1) * tdof + idof(k))
    END DO
  END DO

END SELECT

END PROCEDURE obj_GetValue_22

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue23

INTEGER(I4B) :: i, tsize

! main

tsize = 0
DO i = 1, SIZE(idof)
  tsize = tsize + dofobj%valmap(idof(i) + 1) - dofobj%valmap(idof(i))
END DO

CALL Reallocate(VALUE, tsize)
CALL GetValue_(obj, dofobj, idof, VALUE, tsize)

END PROCEDURE obj_GetValue23

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_23
INTEGER(I4B) :: m, n, i, k, tdof, tsize_idof

! main

tsize_idof = SIZE(idof)
k = 0
DO i = 1, tsize_idof
  k = k + dofobj%valmap(idof(i) + 1) - dofobj%valmap(idof(i))
END DO

tsize = k

SELECT CASE (dofobj%StorageFMT)

CASE (DOF_FMT)

  m = 0; n = 0
  DO i = 1, tsize_idof
    m = n + 1
    n = n + dofobj%valmap(idof(i) + 1) - dofobj%valmap(idof(i))
   VALUE(m:n) = obj%val(dofobj%valmap(idof(i)):dofobj%valmap(idof(i + 1) - 1))
  END DO

CASE (NODES_FMT)

  tdof = .tdof.dofobj; m = tsize_idof

  DO i = 1, dofobj%valmap(2) - dofobj%valmap(1)
    DO k = 1, m
      VALUE((i - 1) * m + k) = obj%val((i - 1) * tdof + idof(k))
    END DO
  END DO

END SELECT

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
