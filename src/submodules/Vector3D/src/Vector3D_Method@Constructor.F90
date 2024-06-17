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

!> author: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This submodule contains the implementation of construction relted methods defined inside [[Vector3D_Method]] module.
!

SUBMODULE(Vector3D_Method) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS


!----------------------------------------------------------------------------
!                                                                     SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE get_shape
  Ans = 3_I4B
END PROCEDURE get_shape

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE get_size
  Ans = 3_I4B
END PROCEDURE get_size

!----------------------------------------------------------------------------
!                                                         getTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE Vec3D_getTotalDimension
  ans = obj%tDimension
END PROCEDURE Vec3D_getTotalDimension

!----------------------------------------------------------------------------
!                                                        setTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE Vec3D_setTotalDimension
  obj%tDimension = tDimension
END PROCEDURE Vec3D_setTotalDimension

!----------------------------------------------------------------------------
!                                                                   ALLOCATE
!----------------------------------------------------------------------------

MODULE PROCEDURE allocate_data
  obj%Val=0.0_DFP
  CALL setTotalDimension( obj, 1_I4B )
END PROCEDURE allocate_data

!----------------------------------------------------------------------------
!                                                                 DEALLOCATE
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  obj%Val=0.0_DFP
  CALL setTotalDimension( obj, 1_I4B )
END PROCEDURE deallocate_data


!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_from_val
  SELECT CASE( SIZE( Val ) )
  CASE( 1 )
    obj%Val(1) = Val(1)
    obj%Val(2) = 0.0_DFP
    obj%Val(3) = 0.0_DFP
  CASE( 2 )
    obj%Val(1) = Val(1)
    obj%Val(2) = Val(2)
    obj%Val(3) = 0.0_DFP
  CASE DEFAULT
    obj%Val = Val( 1:3 )
  END SELECT
END PROCEDURE initiate_obj_from_val

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_from_obj
  obj%Val = Anotherobj%Val
END PROCEDURE initiate_obj_from_obj

!----------------------------------------------------------------------------
!                                                                  Vector3D
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  Ans = Val
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                         Vector3D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( Ans )
  CALL Initiate(obj=Ans, Val=Val)
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                           Vector3D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_2
  ALLOCATE( Ans )
  CALL Initiate( obj=Ans, Anotherobj=obj )
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Display_obj
  INTEGER( I4B ) :: i
  i = Input(default=stdout, option=unitNo)
  CALL Display( Val=obj%Val, msg=msg, UnitNo = i)
END PROCEDURE Display_obj

END SUBMODULE Constructor
