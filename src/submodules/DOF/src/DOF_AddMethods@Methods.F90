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

SUBMODULE(DOF_AddMethods) Methods
USE DOF_GetMethods, ONLY: GetNodeLoc, &
                          OPERATOR(.tdof.), &
                          GetNodeLoc_, &
                          GetIndex_, &
                          GetIDOF

USE GlobalData, ONLY: NodesToDOF, DOFToNodes, NODES_FMT, DOF_FMT

USE SafeSizeUtility, ONLY: SafeSize

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: PARAM_EXPAND_FACTOR_TEMP_INTVEC = 2
INTEGER(I4B), PARAMETER :: PARAM_TEMP_INTVEC_SIZE = 1024
INTEGER(I4B) :: tempIntVec(PARAM_TEMP_INTVEC_SIZE)
!$OMP THREADPRIVATE(tempIntVec)

INTEGER(I4B), ALLOCATABLE :: tempAllocIntVec(:)
!$OMP THREADPRIVATE(tempAllocIntVec)

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add1
INTEGER(I4B) :: tdof, idof, i, n, m

tdof = .tdof.obj
n = SIZE(nodenum)
m = SIZE(VALUE)

SELECT CASE (obj%StorageFMT)

CASE (DOF_FMT)

  IF (m .EQ. n) THEN

    DO CONCURRENT(idof=1:tdof, i=1:n)
      vec(obj%valmap(idof) - 1 + nodenum(i)) = &
        vec(obj%valmap(idof) - 1 + nodenum(i)) + scale * VALUE(i)
    END DO

    RETURN
  END IF

  ! vec( nodenum ) += scale * value( 1 )
  IF (m .EQ. 1) THEN

    DO CONCURRENT(idof=1:tdof, i=1:n)
      vec(obj%valmap(idof) - 1 + nodenum(i)) = &
        vec(obj%valmap(idof) - 1 + nodenum(i)) + scale * VALUE(1)
    END DO

    RETURN
  END IF

  ! Vec_obj_i( nodenum ) += scale * val_obj_i( : )
  ! IF (m .EQ. tdof * n) THEN
  IF (conversion(1) .EQ. NodesToDOF) THEN

    DO CONCURRENT(idof=1:tdof, i=1:n)
      vec(obj%valmap(idof) - 1 + nodenum(i)) = &
        vec(obj%valmap(idof) - 1 + nodenum(i)) &
        + scale * VALUE((i - 1) * tdof + idof)
    END DO

    RETURN

  END IF

  ! Vec_obj_i( nodenum ) += scale * val_obj_i( : )
  ! IF (m .EQ. tdof * n) THEN
  DO CONCURRENT(idof=1:tdof, i=1:n)

    vec(obj%valmap(idof) - 1 + nodenum(i)) = &
      vec(obj%valmap(idof) - 1 + nodenum(i)) &
      + scale * VALUE((idof - 1) * n + i)

  END DO

  RETURN

CASE (NODES_FMT)

  IF (m .EQ. n) THEN

    DO CONCURRENT(idof=1:tdof, i=1:n)

      vec((nodenum(i) - 1) * tdof + idof) &
        = vec((nodenum(i) - 1) * tdof + idof) &
          + scale * VALUE(i)

    END DO

    RETURN

  END IF

  IF (m .EQ. 1) THEN

    DO idof = 1, tdof
      vec((nodenum - 1) * tdof + idof) &
        & = vec((nodenum - 1) * tdof + idof) &
        & + scale * VALUE(1)
    END DO

    RETURN
  END IF

  ! ELSE IF (m .EQ. tdof * n) THEN

  IF (conversion(1) .EQ. DOFToNodes) THEN

    DO CONCURRENT(idof=1:tdof, i=1:n)

      vec((nodenum(i) - 1) * tdof + idof) &
        = vec((nodenum(i) - 1) * tdof + idof) &
          + scale * VALUE((idof - 1) * n + i)

    END DO

    RETURN

  END IF

  DO CONCURRENT(idof=1:tdof, i=1:n)
    vec((nodenum(i) - 1) * tdof + idof) &
      = vec((nodenum(i) - 1) * tdof + idof) &
        + scale * VALUE((i - 1) * tdof + idof)
  END DO
  RETURN

  ! END IF

END SELECT

END PROCEDURE obj_Add1

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add2
INTEGER(I4B) :: tsize
tsize = (.tdof.obj) * SIZE(nodenum)

IF (tsize .GT. PARAM_TEMP_INTVEC_SIZE) THEN

  IF (tsize .GT. SafeSize(tempAllocIntVec)) THEN
    CALL Reallocate(tempAllocIntVec, tsize * PARAM_EXPAND_FACTOR_TEMP_INTVEC)
  END IF

  CALL GetIndex_(obj=obj, nodenum=nodenum, ans=tempAllocIntVec, tsize=tsize)
  CALL obj_add_help_1(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, indx=tempAllocIntVec)

  RETURN
END IF

CALL GetIndex_(obj=obj, nodenum=nodenum, ans=tempIntVec, tsize=tsize)
CALL obj_add_help_1(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                    indx=tempIntVec)

END PROCEDURE obj_Add2

!----------------------------------------------------------------------------
!                                                            obj_add_help_1
!----------------------------------------------------------------------------

PURE SUBROUTINE obj_add_help_1(vec, scale, VALUE, tsize, indx)
  REAL(DFP), INTENT(INOUT) :: vec(:)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: VALUE
  INTEGER(I4B), INTENT(IN) :: tsize
  INTEGER(I4B), INTENT(IN) :: indx(:)

  INTEGER(I4B) :: ii

  DO CONCURRENT(ii=1:tsize)
    vec(indx(ii)) = vec(indx(ii)) + scale * VALUE
  END DO

END SUBROUTINE obj_add_help_1

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add3
INTEGER(I4B) :: tsize

tsize = SIZE(nodenum)

IF (tsize .GT. PARAM_TEMP_INTVEC_SIZE) THEN

  IF (tsize .GT. SafeSize(tempAllocIntVec)) THEN
    CALL Reallocate(tempAllocIntVec, tsize * PARAM_EXPAND_FACTOR_TEMP_INTVEC)
  END IF

  CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ans=tempAllocIntVec, &
                   tsize=tsize, idof=idof)

  CALL obj_add_help_2(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                      indx=tempAllocIntVec)

  RETURN
END IF

CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ans=tempIntVec, &
                 tsize=tsize, idof=idof)
CALL obj_add_help_2(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                    indx=tempIntVec)

END PROCEDURE obj_Add3

!----------------------------------------------------------------------------
!                                                         obj_add_help_2
!----------------------------------------------------------------------------

PURE SUBROUTINE obj_add_help_2(vec, scale, VALUE, tsize, indx)
  REAL(DFP), INTENT(INOUT) :: vec(:)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: VALUE(:)
  INTEGER(I4B), INTENT(IN) :: tsize
  INTEGER(I4B), INTENT(IN) :: indx(:)

  INTEGER(I4B) :: ii, n

  n = SIZE(VALUE)

  IF (n .EQ. 1) THEN

    DO CONCURRENT(ii=1:tsize)
      vec(indx(ii)) = vec(indx(ii)) + scale * VALUE(1)
    END DO

    RETURN

  END IF

  DO CONCURRENT(ii=1:tsize)
    vec(indx(ii)) = vec(indx(ii)) + scale * VALUE(ii)
  END DO

END SUBROUTINE obj_add_help_2

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add4
INTEGER(I4B) :: global_idof
global_idof = GetIDOF(obj=obj, ivar=ivar, idof=idof)
CALL obj_Add3(vec=vec, obj=obj, nodenum=nodenum, VALUE=VALUE, scale=scale, &
              idof=global_idof)
END PROCEDURE obj_Add4

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add5
INTEGER(I4B) :: global_idof
global_idof = GetIDOF(obj=obj, ivar=ivar, spaceCompo=spaceCompo, &
                      timeCompo=timeCompo)
CALL obj_Add3(vec=vec, obj=obj, nodenum=nodenum, VALUE=VALUE, scale=scale, &
              idof=global_idof)
END PROCEDURE obj_Add5

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add6
INTEGER(I4B) :: tsize

tsize = SIZE(nodenum)

IF (tsize .GT. PARAM_TEMP_INTVEC_SIZE) THEN

  IF (tsize .GT. SafeSize(tempAllocIntVec)) THEN
    CALL Reallocate(tempAllocIntVec, tsize * PARAM_EXPAND_FACTOR_TEMP_INTVEC)
  END IF

  CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ans=tempAllocIntVec, &
                   tsize=tsize, ivar=ivar, spacecompo=spacecompo, &
                   timecompo=timecompo)

  CALL obj_add_help_2(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                      indx=tempAllocIntVec)

  RETURN
END IF

CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ans=tempIntVec, &
                 tsize=tsize, ivar=ivar, spacecompo=spacecompo, &
                 timecompo=timecompo)
CALL obj_add_help_2(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                    indx=tempIntVec)

END PROCEDURE obj_Add6

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add7
INTEGER(I4B) :: tsize

tsize = SIZE(nodenum)

IF (tsize .GT. PARAM_TEMP_INTVEC_SIZE) THEN

  IF (tsize .GT. SafeSize(tempAllocIntVec)) THEN
    CALL Reallocate(tempAllocIntVec, tsize * PARAM_EXPAND_FACTOR_TEMP_INTVEC)
  END IF

  CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ans=tempAllocIntVec, &
                   tsize=tsize, ivar=ivar, spacecompo=spacecompo, &
                   timecompo=timecompo)

  CALL obj_add_help_2(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                      indx=tempAllocIntVec)

  RETURN
END IF

CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ans=tempIntVec, &
                 tsize=tsize, ivar=ivar, spacecompo=spacecompo, &
                 timecompo=timecompo)
CALL obj_add_help_2(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                    indx=tempIntVec)

END PROCEDURE obj_Add7

!----------------------------------------------------------------------------
!                                                                      Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add8
INTEGER(I4B) :: tsize
CALL GetIndex_(obj=obj, nodenum=nodenum, ans=tempIntVec, tsize=tsize)
CALL obj_add_help_1(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                    indx=tempIntVec)
END PROCEDURE obj_Add8

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add9
INTEGER(I4B) :: indx
indx = GetNodeLoc(obj=obj, nodenum=nodenum, idof=idof)
vec(indx) = vec(indx) + scale * VALUE
END PROCEDURE obj_Add9

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add10
INTEGER(I4B) :: indx
indx = GetNodeLoc(obj=obj, nodenum=nodenum, ivar=ivar, idof=idof)
vec(indx) = vec(indx) + scale * VALUE
END PROCEDURE obj_Add10

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add11
INTEGER(I4B) :: indx
indx = GetNodeLoc( obj=obj, nodenum=nodenum, ivar=ivar, spacecompo=spacecompo, &
                  timecompo=timecompo)
vec(indx) = vec(indx) + scale * VALUE
END PROCEDURE obj_Add11

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add12
INTEGER(I4B) :: tsize

CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ivar=ivar, &
      spacecompo=spacecompo, timecompo=timecompo, ans=tempIntVec, tsize=tsize)

CALL obj_add_help_1(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                    indx=tempIntVec)

END PROCEDURE obj_Add12

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add13
INTEGER(I4B) :: tsize

CALL GetNodeLoc_(obj=obj, nodenum=nodenum, ivar=ivar, &
      spacecompo=spacecompo, timecompo=timecompo, ans=tempIntVec, tsize=tsize)

CALL obj_add_help_1(vec=vec, scale=scale, VALUE=VALUE, tsize=tsize, &
                    indx=tempIntVec)

END PROCEDURE obj_Add13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
