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

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This submodule contains the implementation of construction relted methods defined inside [[Vector3D_Method]] module.
!

SUBMODULE( Vector3D_Method ) Constructor
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
!                                                                   ALLOCATE
!----------------------------------------------------------------------------

MODULE PROCEDURE allocate_data
  Obj%Val=0.0_DFP
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE allocate_data

!----------------------------------------------------------------------------
!                                                                 DEALLOCATE
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  Obj%Val=0.0_DFP
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE deallocate_data


!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_from_val
  SELECT CASE( SIZE( Val ) )
  CASE( 1 )
    Obj%Val(1) = Val(1)
    Obj%Val(2) = 0.0_DFP
    Obj%Val(3) = 0.0_DFP
  CASE( 2 )
    Obj%Val(1) = Val(1)
    Obj%Val(2) = Val(2)
    Obj%Val(3) = 0.0_DFP
  CASE DEFAULT
    Obj%Val = Val( 1:3 )
  END SELECT
END PROCEDURE initiate_obj_from_val

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_from_obj
  Obj%Val = AnotherObj%Val
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
  CALL Initiate(Obj=Ans, Val=Val)
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                           Vector3D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_2
  ALLOCATE( Ans )
  CALL Initiate( Obj=Ans, AnotherObj=Obj )
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Display_obj
  INTEGER( I4B ) :: i
  i = Input(default=stdout, option=unitNo)
  CALL Display( Val=Obj%Val, msg=msg, UnitNo = i)
END PROCEDURE Display_obj

END SUBMODULE Constructor
