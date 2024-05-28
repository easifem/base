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

SUBMODULE(RealVector_SetMethods) Methods
USE DOF_Method, ONLY: DOF_Set => Set
USE F77_Blas, ONLY: F77_Copy
USE F95_Blas, ONLY: F95_Copy => Copy
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set1
REAL(DFP) :: aval(1)
INTEGER(I4B) :: N
aval(1) = VALUE
N = SIZE(obj%val)
CALL F77_Copy(N=N, X=aval, INCX=0_I4B, Y=obj%val, INCY=1_I4B)
END PROCEDURE obj_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set2
CALL F95_Copy(X=VALUE, Y=obj%val)
END PROCEDURE obj_set2

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set3
obj%val(nodenum) = VALUE
END PROCEDURE obj_set3

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set4
obj%val(nodenum) = VALUE
END PROCEDURE obj_set4

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set5
IF (SIZE(VALUE) .EQ. 1) THEN
  obj%val(nodenum) = VALUE(1)
  RETURN
END IF

obj%val(nodenum) = VALUE
END PROCEDURE obj_set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set6
REAL(DFP) :: aval(1)
INTEGER(I4B) :: N
aval(1) = VALUE
N = INT((iend - istart + stride) / stride)
CALL F77_Copy(N=N, X=aval, INCX=0_I4B, Y=obj%val(istart:), &
              INCY=stride)
END PROCEDURE obj_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set7
INTEGER(I4B) :: N

N = SIZE(VALUE)
CALL F77_Copy(N=N, X=VALUE, INCX=1_I4B, Y=obj%val(istart:), &
              INCY=stride)
END PROCEDURE obj_set7

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set8
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             conversion=conversion)
END PROCEDURE obj_set8

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set9
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE)
END PROCEDURE obj_set9

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set10
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             idof=idof)
END PROCEDURE obj_set10

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set11
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             idof=idof)
END PROCEDURE obj_set11

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set12
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             idof=idof, ivar=ivar)
END PROCEDURE obj_set12

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set13
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             idof=idof, ivar=ivar)
END PROCEDURE obj_set13

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set14
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set14

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set15
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set15

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set16
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set16

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set17
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set17

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set18
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set18

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set19
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set19

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set20
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE)
END PROCEDURE obj_set20

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set21
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             idof=idof)
END PROCEDURE obj_set21

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set22
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, idof=idof)
END PROCEDURE obj_set22

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set23
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set23

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set24
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set24

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set25
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_set25

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set26
obj%val = VALUE%val
END PROCEDURE obj_set26

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

END SUBMODULE Methods
