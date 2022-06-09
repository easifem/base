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

SUBMODULE(RealVector_Method) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set1
  obj%val( : ) = value
END PROCEDURE realVec_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set2
  obj%val = value
END PROCEDURE realVec_set2

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set3
  obj%val(nodenum) = value
END PROCEDURE realvec_set3

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set4
  obj%val(nodenum) = value
END PROCEDURE realvec_set4

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set5
  !!
  IF( SIZE( value) .EQ. 1 ) THEN
    obj%val( nodenum ) = value( 1 )
  ELSE
    obj%val( nodenum ) = value
  END IF
  !!
END PROCEDURE realVec_set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set6
  obj%val( istart:iend:stride ) = value
END PROCEDURE realVec_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_set7
  obj%val( istart:iend:stride ) = value
END PROCEDURE realVec_set7

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set8
  !!
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & conversion = conversion )
  !!
END PROCEDURE realvec_set8

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set9
  !!
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value )
  !!
END PROCEDURE realvec_set9

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set10
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & idof=idof )
END PROCEDURE realvec_set10

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set11
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & idof=idof )
END PROCEDURE realvec_set11

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set12
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & idof=idof, &
    & ivar=ivar )
END PROCEDURE realvec_set12

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set13
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & idof=idof, &
    & ivar=ivar )
END PROCEDURE realvec_set13

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set14
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_set14

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set15
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_set15

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set16
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_set16

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set17
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_set17

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set18
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_set18

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set19
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=[value], &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo)
END PROCEDURE realvec_set19

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set20
  !!
  CALL Set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value )
  !!
END PROCEDURE realvec_set20

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set21
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & idof=idof )
END PROCEDURE realvec_set21

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set22
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & ivar=ivar, &
    & idof=idof )
END PROCEDURE realvec_set22

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set23
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
END PROCEDURE realvec_set23

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set24
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
END PROCEDURE realvec_set24

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set25
  CALL set( &
    & vec=obj%val, &
    & obj=dofobj, &
    & nodenum=nodenum, &
    & value=value, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo )
END PROCEDURE realvec_set25

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_set26
  obj%val=value%val
END PROCEDURE realvec_set26

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

END SUBMODULE SetMethods