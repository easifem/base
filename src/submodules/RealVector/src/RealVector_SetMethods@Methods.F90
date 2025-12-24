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
USE DOF_Method, ONLY: DOF_Set => Set, &
                      OPERATOR(.tdof.), &
                      GetNodeLoc
USE F77_Blas, ONLY: F77_Copy
USE F95_Blas, ONLY: F95_Copy => Copy
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
REAL(DFP) :: aval(1)
INTEGER(I4B) :: N
aval(1) = VALUE
N = SIZE(obj%val)
CALL F77_Copy(N=N, X=aval, INCX=0_I4B, Y=obj%val, INCY=1_I4B)
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CALL F95_Copy(X=VALUE, Y=obj%val)
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
obj%val(nodenum) = VALUE
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
obj%val(nodenum) = VALUE
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
IF (SIZE(VALUE) .EQ. 1) THEN
  obj%val(nodenum) = VALUE(1)
  RETURN
END IF

obj%val(nodenum) = VALUE
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
REAL(DFP) :: aval(1)
INTEGER(I4B) :: N
aval(1) = VALUE
N = INT((iend - istart + stride) / stride)
CALL F77_Copy(N=N, X=aval, INCX=0_I4B, Y=obj%val(istart:), &
              INCY=stride)
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
INTEGER(I4B) :: N

N = SIZE(VALUE)
CALL F77_Copy(N=N, X=VALUE, INCX=1_I4B, Y=obj%val(istart:), &
              INCY=stride)
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             conversion=conversion)
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE)
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             idof=idof)
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             idof=idof)
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             idof=idof, ivar=ivar)
END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             idof=idof, ivar=ivar)
END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set15
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set15

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set16
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set16

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set17
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set17

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set18
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set18

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set19
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=[VALUE], &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set19

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set20
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE)
END PROCEDURE obj_Set20

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set21
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             idof=idof)
END PROCEDURE obj_Set21

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set22
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, idof=idof)
END PROCEDURE obj_Set22

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set23
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set23

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set24
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set24

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set25
CALL DOF_Set(vec=obj%val, obj=dofobj, nodenum=nodenum, VALUE=VALUE, &
             ivar=ivar, spacecompo=spacecompo, timecompo=timecompo)
END PROCEDURE obj_Set25

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set26
! obj%val = VALUE%val
CALL F95_Copy(X=VALUE%val, Y=obj%val)
END PROCEDURE obj_Set26

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set27
INTEGER(I4B) :: tdof, s(3), idof

tdof = .tdof.dofobj

DO idof = 1, tdof
  s = GetNodeLoc(obj=dofobj, idof=idof)
  CALL obj_Set7(obj=obj, istart=s(1), iend=s(2), stride=s(3), &
                VALUE=VALUE(:, idof))
END DO

END PROCEDURE obj_Set27

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set28
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=dofobj, idof=idof)
CALL obj_Set7(obj=obj, istart=s(1), iend=s(2), stride=s(3), VALUE=VALUE)
END PROCEDURE obj_Set28

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set29
INTEGER(I4B) :: s1(3), s2(3)
INTEGER(I4B) :: N

s1 = GetNodeLoc(obj=dofobj1, idof=idof1)
s2 = GetNodeLoc(obj=dofobj2, idof=idof2)

N = (s1(2) - s1(1) + s1(3)) / s1(3)

CALL F77_Copy(N=N, X=obj2%val(s2(1):), INCX=s2(3), Y=obj1%val(s1(1):), &
              INCY=s1(3))
END PROCEDURE obj_Set29

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set30
INTEGER(I4B) :: ii, jj
!$OMP PARALLEL DO PRIVATE(ii, jj)
DO ii = istart, iend, stride
  jj = GetNodeLoc(obj=dofobj, idof=idof, nodenum=ii)
  obj%val(jj) = VALUE
END DO
!$OMP END PARALLEL DO
END PROCEDURE obj_Set30

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set31
INTEGER(I4B) :: ii, jj
!$OMP PARALLEL DO PRIVATE(ii, jj)
DO ii = istart, iend, stride
  jj = GetNodeLoc(obj=dofobj, idof=idof, nodenum=ii)
  obj%val(jj) = VALUE((ii - istart + stride) / stride)
END DO
!$OMP END PARALLEL DO
END PROCEDURE obj_Set31

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set32
INTEGER(I4B) :: tsize
tsize = (iend - istart + stride) / stride
CALL F77_Copy(N=tsize, X=VALUE(istart_value:), INCX=stride_value, &
              Y=obj%val(istart:), INCY=stride)
! !$OMP PARALLEL DO PRIVATE(ii)
! DO ii = 1, tsize
!   obj%val(istart+(stride-1)*ii) = value(istart_value+(stride_value-1)*ii)
! END DO
! !$OMP END PARALLEL DO
END PROCEDURE obj_Set32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
