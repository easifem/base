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

SUBMODULE(RealVector_Method) AddMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_add1
  obj%val = obj%val + scale * value
END PROCEDURE realVec_add1

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_add2
  obj%val = obj%val + scale * value
END PROCEDURE realVec_add2

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add3
  obj%val(nodenum) = obj%val(nodenum) + scale * value
END PROCEDURE realvec_add3

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add4
  obj%val(nodenum) = obj%val(nodenum) + scale * value
END PROCEDURE realvec_add4

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_add5
  !!
  IF( SIZE( value) .EQ. 1 ) THEN
    obj%val( nodenum ) = obj%val( nodenum ) + scale * value( 1 )
  ELSE
    obj%val( nodenum ) = obj%val( nodenum ) + scale * value
  END IF
  !!
END PROCEDURE realVec_add5

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_add6
  obj%val( istart:iend:stride ) = obj%val( istart:iend:stride ) &
    & + scale * value
END PROCEDURE realVec_add6

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_add7
  obj%val( istart:iend:stride ) = obj%val( istart:iend:stride ) &
    & + scale * value
END PROCEDURE realVec_add7

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add8
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & conversion = conversion )
END PROCEDURE realvec_add8

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add9
  !!
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale )
  !!
END PROCEDURE realvec_add9

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add10
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & idof=idof )
END PROCEDURE realvec_add10

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add11
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & scale=scale, &
    & idof=idof )
END PROCEDURE realvec_add11

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add12
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & idof=idof, &
    & ivar=ivar )
END PROCEDURE realvec_add12

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add13
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & scale=scale, &
    & idof=idof, &
    & ivar=ivar )
END PROCEDURE realvec_add13

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add14
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_add14

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add15
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_add15

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add16
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_add16

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add17
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_add17

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add18
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_add18

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add19
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_add19

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add20
  !!
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale )
  !!
END PROCEDURE realvec_add20

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add21
  !!
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & idof=idof )
  !!
END PROCEDURE realvec_add21

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add22
  !!
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & ivar=ivar, &
    & idof=idof )
  !!
END PROCEDURE realvec_add22

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add23
  !!
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
  !!
END PROCEDURE realvec_add23

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add24
  !!
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
  !!
END PROCEDURE realvec_add24

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add25
  !!
  CALL add( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & scale=scale, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
  !!
END PROCEDURE realvec_add25

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_add26
  obj%val=obj%val+scale*value%val
END PROCEDURE realvec_add26

END SUBMODULE AddMethods