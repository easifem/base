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

SUBMODULE(DOF_SetMethods) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set1
INTEGER(I4B) :: tdof, idof, i, n, m
!
tdof = .tdof.obj
n = SIZE(nodenum)
m = SIZE(VALUE)
!
ASSOCIATE (vm => obj%valmap)
  SELECT CASE (obj%StorageFMT)
    !
    !
    !
    !
  CASE (DOF_FMT)
    !
    IF (m .NE. n) THEN
      !
      IF (m .EQ. 1) THEN
        !
        DO idof = 1, tdof
          Vec(vm(idof) - 1 + nodenum) = VALUE(1)
        END DO
        !
      ELSE IF (m .EQ. tdof * n) THEN
        !
        IF (Conversion(1) .EQ. nodesToDOF) THEN
          !
          DO idof = 1, tdof
            DO i = 1, n
              Vec(vm(idof) - 1 + nodenum(i)) = &
                & VALUE((i - 1) * tdof + idof)
            END DO
          END DO
          !
        ELSE
          !
          DO idof = 1, tdof
            Vec(vm(idof) - 1 + nodenum) = &
              & VALUE((idof - 1) * n + 1:idof * n)
          END DO
          !
        END IF
        !
      END IF
      !
    ELSE
      !
      DO idof = 1, tdof
        Vec(vm(idof) - 1 + nodenum) = VALUE(:)
      END DO
      !
    END IF
    !
    !
    !
    !
  CASE (NODES_FMT)
    !
    IF (m .NE. n) THEN
      !
      IF (m .EQ. 1) THEN
        !
        DO idof = 1, tdof
          Vec((nodenum - 1) * tdof + idof) = VALUE(1)
        END DO
        !
      ELSE IF (m .EQ. tdof * n) THEN
        !
        IF (Conversion(1) .EQ. DOFToNodes) THEN
          !
          DO idof = 1, tdof
            DO i = 1, n
              Vec((nodenum(i) - 1) * tdof + idof) &
                & = VALUE((idof - 1) * n + i)
            END DO
          END DO
          !
        ELSE
          !
          DO idof = 1, tdof
            DO i = 1, n
              Vec((nodenum(i) - 1) * tdof + idof) &
                & = VALUE((i - 1) * tdof + idof)
            END DO
          END DO
          !
        END IF
        !
      END IF
      !
    ELSE
      !
      DO idof = 1, tdof
        Vec((nodenum - 1) * tdof + idof) = VALUE(:)
      END DO
      !
    END IF
    !
  END SELECT
  !
  !
  !
  !
END ASSOCIATE
!
END PROCEDURE dof_set1

!----------------------------------------------------------------------------
!                                                                    setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set2
vec(getIndex(obj=obj, nodenum=nodenum)) = VALUE
END PROCEDURE dof_set2

!----------------------------------------------------------------------------
!                                                                 setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set3
!
IF (SIZE(nodenum) .EQ. SIZE(VALUE)) THEN
  Vec( &
    & getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & idof=idof)) = VALUE(:)
ELSE
  Vec( &
    & getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & idof=idof)) = VALUE(1)
END IF
!
END PROCEDURE dof_set3

!----------------------------------------------------------------------------
!                                                                 setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set4
!
IF (SIZE(nodenum) .EQ. SIZE(VALUE)) THEN
  Vec( &
    & getIndex( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=idof)) &
    & = VALUE(:)
ELSE
  Vec( &
    & getIndex( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=idof)) &
    & = VALUE(1)
END IF
!
END PROCEDURE dof_set4

!----------------------------------------------------------------------------
!                                                                 set
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set5
!
IF (SIZE(nodenum) .EQ. SIZE(VALUE)) THEN
  !
  Vec(getNodeLoc(&
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)) &
    & = VALUE(:)
  !
ELSE
  !
  Vec( &
    & getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)) &
    & = VALUE(1)
  !
END IF
!
END PROCEDURE dof_set5

!----------------------------------------------------------------------------
!                                                                 set
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set6
!
IF (SIZE(nodenum) .EQ. SIZE(VALUE) * SIZE(timecompo)) THEN
  Vec( &
    & getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)) &
    & = VALUE(:)
ELSE
  Vec( &
    & getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)) &
    & = VALUE(1)
END IF
!
END PROCEDURE dof_set6

!----------------------------------------------------------------------------
!                                                                 set
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set7
!
IF (SIZE(nodenum) .EQ. SIZE(VALUE) * SIZE(spacecompo)) THEN
  Vec(getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)) &
    & = VALUE(:)
ELSE
  Vec( &
    & getNodeLoc( &
    & obj=obj, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)) &
    & = VALUE(1)
END IF
!
END PROCEDURE dof_set7

!----------------------------------------------------------------------------
!                                                                    setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set8
vec( &
  & getIndex( &
  & obj=obj, &
  & nodenum=nodenum)) = VALUE
END PROCEDURE dof_set8

!----------------------------------------------------------------------------
!                                                                 setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set9
Vec( &
  & getNodeLoc( &
  & obj=obj, &
  & nodenum=nodenum, &
  & idof=idof)) = VALUE
END PROCEDURE dof_set9

!----------------------------------------------------------------------------
!                                                                 setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set10
Vec( &
  & getNodeLoc( &
  & obj=obj, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & idof=idof)) = VALUE
END PROCEDURE dof_set10

!----------------------------------------------------------------------------
!                                                                 setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set11
Vec(getNodeLoc(obj=obj, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo)) = VALUE
END PROCEDURE dof_set11

!----------------------------------------------------------------------------
!                                                                 setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set12
Vec(getNodeLoc(obj=obj, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo)) = VALUE
END PROCEDURE dof_set12

!----------------------------------------------------------------------------
!                                                                 setvalue
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_set13
Vec(getNodeLoc(obj=obj, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo)) = VALUE
END PROCEDURE dof_set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
