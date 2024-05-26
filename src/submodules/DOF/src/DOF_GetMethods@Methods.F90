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

SUBMODULE(DOF_GetMethods) Methods
USE ReallocateUtility, ONLY: Reallocate
USE ArangeUtility, ONLY: Arange
USE GlobalData, ONLY: NODES_FMT, DOF_FMT, FMT_DOF, FMT_NODES

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              DOFStartIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DOFStartIndex
ans = obj%map(ivar, 5)
END PROCEDURE obj_DOFStartIndex

!----------------------------------------------------------------------------
!                                                              DOFEndIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DOFEndIndex
ans = obj%map(ivar + 1, 5) - 1
END PROCEDURE obj_DOFEndIndex

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes1
ans = 0
IF (ALLOCATED(obj%map)) ans = obj%map(SIZE(obj%map, 1), 6)
END PROCEDURE obj_tNodes1

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes2
ans = 0
IF (ALLOCATED(obj%valmap)) ans = obj%valmap(idof + 1) - obj%valmap(idof)
END PROCEDURE obj_tNodes2

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes3
ans = obj.tNodes. (NameToIndex(obj, varName))
END PROCEDURE obj_tNodes3

!----------------------------------------------------------------------------
!                                                                     tNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNodes4
INTEGER(I4B) :: ii
ans = 0
DO ii = 1, SIZE(idof)
  ans = ans + (obj.tNodes.idof(ii))
END DO
END PROCEDURE obj_tNodes4

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tdof1
ans = 0
IF (ALLOCATED(obj%map)) ans = obj%map(SIZE(obj%map, 1), 4)
END PROCEDURE obj_tdof1

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tdof2
INTEGER(I4B) :: i, k
ans = 0
IF (ALLOCATED(obj%map)) THEN
  k = ICHAR(Name)
  DO i = 1, SIZE(obj%map, 1) - 1
    IF (obj%map(i, 1) .EQ. k) ans = obj%map(i, 4)
  END DO
END IF
END PROCEDURE obj_tdof2

!----------------------------------------------------------------------------
!                                                                       tDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tdof3
INTEGER(I4B) :: i
LOGICAL(LGT) :: isok
ans = 0

i = SIZE(obj%map, 1) - 1
isok = ALLOCATED(obj%map) .AND. (ivar .LE. i)
IF (isok) ans = obj%map(ivar, 4)
END PROCEDURE obj_tdof3

!----------------------------------------------------------------------------
!                                                                     tNames
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tNames
ans = 0
IF (ALLOCATED(obj%map)) ans = SIZE(obj%map, 1) - 1
END PROCEDURE obj_tNames

!----------------------------------------------------------------------------
!                                                                     Names
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_names1
INTEGER(I4B) :: ii, n

n = SIZE(obj%map, 1) - 1
ALLOCATE (ans(n))

DO ii = 1, n
  ans(ii) = ACHAR(obj%map(ii, 1))
END DO
END PROCEDURE obj_names1

!----------------------------------------------------------------------------
!                                                                     Names
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_names2
ans = ACHAR(obj%map(ii, 1))
END PROCEDURE obj_names2

!----------------------------------------------------------------------------
!                                                                 IndexOF
!----------------------------------------------------------------------------

MODULE PROCEDURE NameToIndex
INTEGER(I4B) :: n, i, ic
n = SIZE(obj%map, 1) - 1
ic = ICHAR(Name)
ans = 0
DO i = 1, n
  IF (obj%map(i, 1) .EQ. ic) THEN
    ans = i
    EXIT
  END IF
END DO
END PROCEDURE NameToIndex

!----------------------------------------------------------------------------
!                                                            tspacecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_tspacecomponents
INTEGER(I4B) :: n, i
n = SIZE(obj%map, 1) - 1
ans = 0
DO i = 1, n
  IF (obj%map(i, 2) .GT. 0) ans = ans + 1
END DO
END PROCEDURE obj_tspacecomponents

!----------------------------------------------------------------------------
!                                                            spacecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_spacecomponents1
INTEGER(I4B) :: n, i
CALL Reallocate(ans, SIZE(obj%map, 1) - 1)
DO i = 1, SIZE(ans)
  ans(i) = obj%map(i, 2)
END DO
END PROCEDURE obj_spacecomponents1

!----------------------------------------------------------------------------
!                                                            spacecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_spacecomponents2
ans = obj%map(ivar, 2)
END PROCEDURE obj_spacecomponents2

!----------------------------------------------------------------------------
!                                                            ttimecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ttimecomponents
INTEGER(I4B) :: n, i
n = SIZE(obj%map, 1) - 1
ans = 0
DO i = 1, n
  IF (obj%map(i, 3) .GT. 1) ans = ans + 1
END DO
END PROCEDURE obj_ttimecomponents

!----------------------------------------------------------------------------
!                                                            timecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_timecomponents1
INTEGER(I4B) :: n, i
CALL Reallocate(ans, SIZE(obj%map, 1) - 1)
DO i = 1, SIZE(ans)
  ans(i) = obj%map(i, 3)
END DO
END PROCEDURE obj_timecomponents1

!----------------------------------------------------------------------------
!                                                            timecomponents
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_timecomponents2
ans = obj%map(ivar, 3)
END PROCEDURE obj_timecomponents2

!----------------------------------------------------------------------------
!                                                                 EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isEqual
ans = .TRUE.
IF (obj1%storageFMT .NE. obj2%storageFMT) ans = .FALSE.
IF (ANY(obj1%map(:, 2:) .NE. obj2%map(:, 2:))) ans = .FALSE.
IF (ANY(obj1%valmap .NE. obj2%valmap)) ans = .FALSE.
END PROCEDURE obj_isEqual

!----------------------------------------------------------------------------
!                                                                 NE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isNE
ans = .NOT. (obj_isEqual(obj1, obj2))
END PROCEDURE obj_isNE

!----------------------------------------------------------------------------
!                                                                 GetIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIDOF1
ans = spacecompo + (timecompo - 1) * tspacecompo
END PROCEDURE obj_GetIDOF1

!----------------------------------------------------------------------------
!                                                                 GetIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIDOF2
ans = (obj.DOFStartIndex.ivar) &
      + spacecompo - 1 &
      + (timecompo - 1) * (obj.spacecomponents.ivar)
END PROCEDURE obj_GetIDOF2

!----------------------------------------------------------------------------
!                                                                 GetIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIDOF3
ans = (obj.DOFStartIndex.ivar) &
      + spacecompo - 1 &
      + (timecompo - 1) * (obj.spacecomponents.ivar)
END PROCEDURE obj_GetIDOF3

!----------------------------------------------------------------------------
!                                                                 GetIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIDOF4
ans = (obj.DOFStartIndex.ivar) &
      + spacecompo - 1 &
      + (timecompo - 1) * (obj.spacecomponents.ivar)
END PROCEDURE obj_GetIDOF4

!----------------------------------------------------------------------------
!                                                                 GetIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIDOF5
ans = spacecompo + (timecompo - 1) * tspacecompo
END PROCEDURE obj_GetIDOF5

!----------------------------------------------------------------------------
!                                                                 GetIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIDOF6
ans = spacecompo + (timecompo - 1) * tspacecompo
END PROCEDURE obj_GetIDOF6

!----------------------------------------------------------------------------
!                                                                 GetIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIDOF7
ans = (obj.DOFStartIndex.ivar) + idof - 1
END PROCEDURE obj_GetIDOF7

!----------------------------------------------------------------------------
!                                                                 GetIDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIDOF8
ans = (obj.DOFStartIndex.ivar) + Arange(1, obj.tdof.ivar) - 1
END PROCEDURE obj_GetIDOF8

!----------------------------------------------------------------------------
!                                                               GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc1
IF (obj%storageFMT .EQ. NODES_FMT) THEN
  ans = (nodenum - 1) * (.tdof.obj) + idof
ELSE
  ans = obj%valmap(idof) + nodenum - 1
END IF
END PROCEDURE obj_GetNodeLoc1

!----------------------------------------------------------------------------
!                                                               GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc2
INTEGER(I4B) :: tsize
CALL obj_getnodeloc_2(obj, nodenum, idof, ans, tsize)
END PROCEDURE obj_GetNodeLoc2

!----------------------------------------------------------------------------
!                                                               GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_2
tsize = SIZE(nodenum)
IF (obj%storageFMT .EQ. NODES_FMT) THEN
  ans(1:tsize) = (nodenum - 1) * (.tdof.obj) + idof
ELSE
  ans(1:tsize) = obj%valmap(idof) - 1 + nodenum
END IF
END PROCEDURE obj_GetNodeLoc_2

!----------------------------------------------------------------------------
!                                                               GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc3
INTEGER(I4B) :: tsize
CALL obj_getnodeloc_3(obj, nodenum, idof, ans, tsize)
END PROCEDURE obj_GetNodeLoc3

!----------------------------------------------------------------------------
!                                                               GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_3
tsize = SIZE(idof)
IF (obj%storageFMT .EQ. NODES_FMT) THEN
  ans(1:tsize) = (nodenum - 1) * (.tdof.obj) + idof
ELSE
  ans(1:tsize) = obj%valmap(idof) - 1 + nodenum
END IF
END PROCEDURE obj_GetNodeLoc_3

!----------------------------------------------------------------------------
!                                                               GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc4
IF (obj%storageFMT .EQ. NODES_FMT) THEN
  ans = [idof, .tnodes.obj, .tdof.obj]
ELSE
  ans = [obj%valmap(idof), obj%valmap(idof + 1) - 1, 1]
END IF
END PROCEDURE obj_GetNodeLoc4

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc5
IF (obj%storageFMT .EQ. DOF_FMT) THEN
  ans = obj%valmap(obj%map(ivar, 5) - 1 + idof) + nodenum - 1
ELSE
  ans = (nodenum - 1) * (.tdof.obj) + (obj%map(ivar, 5) - 1 + idof)
END IF
END PROCEDURE obj_GetNodeLoc5

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc6
INTEGER(I4B) :: tsize
CALL obj_GetNodeLoc_6(obj, nodenum, ivar, idof, ans, tsize)
END PROCEDURE obj_GetNodeLoc6

!----------------------------------------------------------------------------
!                                                                GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_6
INTEGER(I4B) :: a, b

tsize = SIZE(nodenum)
a = obj%map(ivar, 5) - 1 + idof

IF (obj%storageFMT .EQ. DOF_FMT) THEN
  ans(1:tsize) = obj%valmap(a) + nodenum - 1
  RETURN
END IF

b = .tdof.obj
ans(1:tsize) = (nodenum - 1) * b + a
END PROCEDURE obj_GetNodeLoc_6

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc7
ans = GetNodeLoc(obj=obj, nodenum=nodenum, ivar=ivar, &
                 idof=GetIDOF(spacecompo=spacecompo, &
                              timecompo=timecompo, &
                              tspacecompo=obj.spacecomponents.ivar))
END PROCEDURE obj_GetNodeLoc7

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc8
INTEGER(I4B) :: tsize
CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ivar=ivar, ans=ans, tsize=tsize, &
                 idof=GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
                              tspacecompo=obj.spacecomponents.ivar))
END PROCEDURE obj_GetNodeLoc8

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_8
CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ivar=ivar, ans=ans, tsize=tsize, &
                 idof=GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
                              tspacecompo=obj.spacecomponents.ivar))
END PROCEDURE obj_GetNodeLoc_8

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc9
INTEGER(I4B) :: tsize
CALL obj_GetNodeLoc_9(obj, nodenum, ivar, idof, ans, tsize)
END PROCEDURE obj_GetNodeLoc9

!----------------------------------------------------------------------------
!                                                               GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_9
INTEGER(I4B) :: ii, a, b

tsize = SIZE(idof)
a = obj%map(ivar, 5) - 1
b = nodenum - 1

IF (obj%storageFMT .EQ. DOF_FMT) THEN

  DO ii = 1, tsize
    ans(ii) = obj%valmap(a + idof(ii)) + b
  END DO

ELSE

  b = b * (.tdof.obj)
  b = b + a

  DO ii = 1, tsize
    ans(ii) = b + idof(ii)
  END DO

END IF

END PROCEDURE obj_GetNodeLoc_9

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc10
INTEGER(I4B) :: tsize
CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ivar=ivar, ans=ans, tsize=tsize, &
                 idof=GetIDOF(spacecompo=spacecompo, &
                              timecompo=timecompo, &
                              tspacecompo=obj.spacecomponents.ivar))
END PROCEDURE obj_GetNodeLoc10

!----------------------------------------------------------------------------
!                                                                GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_10
CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ivar=ivar, ans=ans, tsize=tsize, &
                 idof=GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
                              tspacecompo=obj.spacecomponents.ivar))
END PROCEDURE obj_GetNodeLoc_10

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc11
INTEGER(I4B) :: tsize
CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ivar=ivar, ans=ans, tsize=tsize, &
                 idof=GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
                              tspacecompo=obj.spacecomponents.ivar))
END PROCEDURE obj_GetNodeLoc11

!----------------------------------------------------------------------------
!                                                                GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_11
CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ivar=ivar, ans=ans, tsize=tsize, &
                 idof=GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
                              tspacecompo=obj.spacecomponents.ivar))
END PROCEDURE obj_GetNodeLoc_11

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc12
INTEGER(I4B) :: tsize
CALL obj_GetNodeLoc_12(obj, nodenum, ivar, spacecompo, &
                       timecompo, ans, tsize)
END PROCEDURE obj_GetNodeLoc12

!----------------------------------------------------------------------------
!                                                                GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_12
INTEGER(I4B) :: idofs(SIZE(timecompo)), ii, tempsize, tnode

tempsize = SIZE(timecompo)
tnode = SIZE(nodenum)
tsize = tempsize * tnode

idofs = GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
                tspacecompo=obj.spacecomponents.ivar)

tsize = 1
DO ii = 1, tnode
  CALL GetNodeLoc_(obj=obj, nodenum=nodenum(ii), ivar=ivar, idof=idofs, &
                   ans=ans(tsize:), tsize=tempsize)
  tsize = tsize + tempsize
END DO

tsize = tsize - 1

END PROCEDURE obj_GetNodeLoc_12

!----------------------------------------------------------------------------
!                                                                GetNodeLoc
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc13
INTEGER(I4B) :: tsize
CALL obj_GetNodeLoc_13(obj, nodenum, ivar, spacecompo, &
                       timecompo, ans, tsize)
END PROCEDURE obj_GetNodeLoc13

!----------------------------------------------------------------------------
!                                                                GetNodeLoc_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeLoc_13
INTEGER(I4B) :: idofs(SIZE(spacecompo)), ii, tempsize, tnode

tempsize = SIZE(spacecompo)
tnode = SIZE(nodenum)
tsize = tempsize * tnode

idofs = GetIDOF(spacecompo=spacecompo, timecompo=timecompo, &
                tspacecompo=obj.spacecomponents.ivar)

tsize = 1
DO ii = 1, tnode
  CALL GetNodeLoc_(obj=obj, nodenum=nodenum(ii), ivar=ivar, idof=idofs, &
                   ans=ans(tsize:), tsize=tempsize)
  tsize = tsize + tempsize
END DO

tsize = tsize - 1

END PROCEDURE obj_GetNodeLoc_13

!----------------------------------------------------------------------------
!                                                               GetIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex1
ans = GetNodeLoc(obj=obj, nodenum=nodenum, idof=Arange(1, .tdof.obj))
END PROCEDURE obj_GetIndex1

!----------------------------------------------------------------------------
!                                                                 GetIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex2
ans = GetNodeLoc(obj=obj, nodenum=nodenum, &
                 idof=Arange(obj.DOFStartIndex.ivar, obj.DOFEndIndex.ivar))
END PROCEDURE obj_GetIndex2

!----------------------------------------------------------------------------
!                                                                 GetIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex3
ans = GetIndex(obj=obj, ivar=NameToIndex(obj, varName), nodenum=nodenum)
END PROCEDURE obj_GetIndex3

!----------------------------------------------------------------------------
!                                                                 GetIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex4
INTEGER(I4B) :: jj, ii, tdof, nn

tdof = .tdof.obj
nn = SIZE(nodenum)
ALLOCATE (ans(tdof * nn))
ans = 0

IF (obj%storageFMT .EQ. FMT_NODES) THEN

  DO ii = 1, nn
    ans((ii - 1) * tdof + 1:ii * tdof) = GetIndex(obj=obj, &
                                                  nodenum=nodenum(ii))
  END DO

ELSE

  DO jj = 1, tdof
    DO ii = 1, nn
      ans((jj - 1) * nn + ii) = GetNodeLoc(obj=obj, &
                                           nodenum=nodenum(ii), idof=jj)
    END DO
  END DO

END IF

END PROCEDURE obj_GetIndex4

!----------------------------------------------------------------------------
!                                                                 GetIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex5
INTEGER(I4B) :: jj, ii, tdof, nn

tdof = obj.tdof.ivar
nn = SIZE(nodenum)
ALLOCATE (ans(tdof * nn))
ans = 0

IF (obj%storageFMT .EQ. FMT_NODES) THEN

  DO ii = 1, nn
    ans((ii - 1) * tdof + 1:ii * tdof) = GetIndex(obj=obj, &
                                               nodenum=nodenum(ii), ivar=ivar)
  END DO

ELSE

  tdof = 0 ! using tdof as counter
  DO jj = (obj.DOFStartIndex.ivar), (obj.DOFEndIndex.ivar)
    tdof = tdof + 1
    DO ii = 1, nn
      ans((tdof - 1) * nn + ii) = GetNodeLoc(obj=obj, nodenum=nodenum(ii), &
                                             idof=jj)
      ! here tdof is local counter
    END DO
  END DO
END IF

END PROCEDURE obj_GetIndex5

!----------------------------------------------------------------------------
!                                                                 GetIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex6
ans = GetIndex(obj=obj, ivar=NameToIndex(obj, varName), nodenum=nodenum)
END PROCEDURE obj_GetIndex6

END SUBMODULE Methods
