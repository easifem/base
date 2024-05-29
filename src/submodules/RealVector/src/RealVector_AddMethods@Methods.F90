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
USE DOF_Method, ONLY: DOF_Add => Add
USE F77_BLAS, ONLY: F77_AXPY
USE F95_BLAS, ONLY: F95_AXPY => AXPY
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add1
! obj%val = obj%val + scale * VALUE
REAL(DFP) :: aval(1)
INTEGER(I4B) :: N
aval(1) = VALUE
N = SIZE(obj%val)
CALL F77_AXPY(N=N, A=scale, X=aval, INCX=0_I4B, Y=obj%val, INCY=1_I4B)
END PROCEDURE obj_add1

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add2
! obj%val = obj%val + scale * VALUE
CALL F95_AXPY(A=scale, X=VALUE, Y=obj%val)
END PROCEDURE obj_add2

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add3
obj%val(nodenum) = obj%val(nodenum) + scale * VALUE
END PROCEDURE obj_add3

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add4
obj%val(nodenum) = obj%val(nodenum) + scale * VALUE
END PROCEDURE obj_add4

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add5
IF (SIZE(VALUE) .EQ. 1) THEN
  obj%val(nodenum) = obj%val(nodenum) + scale * VALUE(1)
  RETURN
END IF

obj%val(nodenum) = obj%val(nodenum) + scale * VALUE
END PROCEDURE obj_add5

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add6
! obj%val(istart:iend:stride) = obj%val(istart:iend:stride) + scale * VALUE
REAL(DFP) :: aval(1)
INTEGER(I4B) :: N
aval(1) = VALUE
N = INT((iend - istart + stride) / stride)
CALL F77_AXPY(N=N, A=scale, X=aval, INCX=0_I4B, Y=obj%val(istart:), &
              INCY=stride)
END PROCEDURE obj_add6

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add7
! obj%val(istart:iend:stride) = obj%val(istart:iend:stride) + scale * VALUE
INTEGER(I4B) :: N

N = SIZE(VALUE)
CALL F77_AXPY(N=N, A=scale, X=VALUE, INCX=1_I4B, Y=obj%val(istart:), &
              INCY=stride)
END PROCEDURE obj_add7

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add8
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, conversion=conversion)
END PROCEDURE obj_add8

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add9
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale)
END PROCEDURE obj_add9

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add10
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, idof=idof)
END PROCEDURE obj_add10

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add11
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             scale=scale, idof=idof)
END PROCEDURE obj_add11

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add12
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, idof=idof, ivar=ivar)
END PROCEDURE obj_add12

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add13
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             scale=scale, idof=idof, ivar=ivar)
END PROCEDURE obj_add13

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add14
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add14

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add15
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add15

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add16
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add16

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add17
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add17

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add18
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add18

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add19
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add19

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add20
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale)
END PROCEDURE obj_add20

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add21
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, idof=idof)
END PROCEDURE obj_add21

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add22
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             scale=scale, ivar=ivar, idof=idof)
END PROCEDURE obj_add22

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add23
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add23

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add24
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add24

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add25
CALL DOF_Add(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
           scale=scale, ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_add25

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add26
! obj%val = obj%val + scale * VALUE%val
CALL F95_AXPY(A=scale, X=VALUE%val, Y=obj%val)
END PROCEDURE obj_add26

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_add27
CALL DOF_Add(vec=obj%val, obj=dofobj, scale=scale, VALUE=VALUE)
END PROCEDURE obj_add27

END SUBMODULE Methods
