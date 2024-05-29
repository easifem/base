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

SUBMODULE(RealVector_AddMethods) Methods
USE GlobalData, ONLY: DOF_FMT, NODES_FMT

USE DOF_Method, ONLY: DOF_Add => Add, &
                      OPERATOR(.tdof.), &
                      GetNodeLoc

USE F77_BLAS, ONLY: F77_AXPY

USE F95_BLAS, ONLY: F95_AXPY => AXPY

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add1
! obj%val = obj%val + scale * VALUE
REAL(DFP) :: aval(1)
INTEGER(I4B) :: N
aval(1) = VALUE
N = SIZE(obj%val)
CALL F77_AXPY(N=N, A=scale, X=aval, INCX=0_I4B, Y=obj%val, INCY=1_I4B)
END PROCEDURE obj_Add1

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add2
! obj%val = obj%val + scale * VALUE
CALL F95_AXPY(A=scale, X=VALUE, Y=obj%val)
END PROCEDURE obj_Add2

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add3
obj%val(nodenum) = obj%val(nodenum) + scale * VALUE
END PROCEDURE obj_Add3

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add4
obj%val(nodenum) = obj%val(nodenum) + scale * VALUE
END PROCEDURE obj_Add4

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add5
IF (SIZE(VALUE) .EQ. 1) THEN
  obj%val(nodenum) = obj%val(nodenum) + scale * VALUE(1)
  RETURN
END IF

obj%val(nodenum) = obj%val(nodenum) + scale * VALUE
END PROCEDURE obj_Add5

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add6
! obj%val(istart:iend:stride) = obj%val(istart:iend:stride) + scale * VALUE
REAL(DFP) :: aval(1)
INTEGER(I4B) :: N
aval(1) = VALUE
N = INT((iend - istart + stride) / stride)
CALL F77_AXPY(N=N, A=scale, X=aval, INCX=0_I4B, Y=obj%val(istart:), &
              INCY=stride)
END PROCEDURE obj_Add6

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add7
! obj%val(istart:iend:stride) = obj%val(istart:iend:stride) + scale * VALUE
INTEGER(I4B) :: N

N = SIZE(VALUE)
CALL F77_AXPY(N=N, A=scale, X=VALUE, INCX=1_I4B, Y=obj%val(istart:), &
              INCY=stride)
END PROCEDURE obj_Add7

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add8
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, conversion=conversion)
END PROCEDURE obj_Add8

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add9
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale)
END PROCEDURE obj_Add9

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add10
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, idof=idof)
END PROCEDURE obj_Add10

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add11
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             scale=scale, idof=idof)
END PROCEDURE obj_Add11

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add12
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, idof=idof, ivar=ivar)
END PROCEDURE obj_Add12

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add13
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             scale=scale, idof=idof, ivar=ivar)
END PROCEDURE obj_Add13

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add14
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add14

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add15
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add15

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add16
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add16

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add17
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add17

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add18
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add18

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add19
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add19

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add20
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale)
END PROCEDURE obj_Add20

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add21
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, idof=idof)
END PROCEDURE obj_Add21

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add22
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, ivar=ivar, idof=idof)
END PROCEDURE obj_Add22

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add23
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add23

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add24
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add24

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add25
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Add25

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add26
! obj%val = obj%val + scale * VALUE%val
CALL F95_AXPY(A=scale, X=VALUE%val, Y=obj%val)
END PROCEDURE obj_Add26

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add27
INTEGER(I4B) :: tdof, s(3), idof

tdof = .tdof.dofobj

DO idof = 1, tdof
  s = GetNodeLoc(obj=dofobj, idof=idof)
  CALL obj_Add7(obj=obj, istart=s(1), iend=s(2), stride=s(3), scale=scale, &
                VALUE=VALUE(:, idof))
END DO

END PROCEDURE obj_Add27

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
