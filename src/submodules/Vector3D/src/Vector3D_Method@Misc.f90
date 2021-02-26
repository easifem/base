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
! summary: 	This submodule implements the methods of [[Vector3D_]] which are defined in [[Vector3D_Method]] module.

SUBMODULE( Vector3D_Method ) Misc
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE dot_product_1
  Ans = DOT_PRODUCT( Obj1%Val, Obj2%Val )
END PROCEDURE dot_product_1

!----------------------------------------------------------------------------
!                                                               DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE dot_product_2
  IF( SIZE( Val ) .LT. 3 ) THEN
    Ans = DOT_PRODUCT( Obj, Vector3D( Val ) )
  ELSE
    Ans = DOT_PRODUCT( Obj%Val, Val(1:3) )
  END IF
END PROCEDURE dot_product_2

!----------------------------------------------------------------------------
!                                                                DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE dot_product_3
  Ans = DOT_PRODUCT(Obj=Obj, Val=Val)
END PROCEDURE dot_product_3

!----------------------------------------------------------------------------
!                                                               DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE dot_product_4
  Ans = DOT_PRODUCT(VECTOR_PRODUCT(v, w), u)
END PROCEDURE dot_product_4

!----------------------------------------------------------------------------
!                                                             Vector_Product
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_product_1
  Ans = Vector_Product(Obj1%val, Obj2%val)
END PROCEDURE vector_product_1

!----------------------------------------------------------------------------
!                                                            Vector_Product
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_product_2
  IF( SIZE( Val ) .LT. 3 ) THEN
    Ans = VECTOR_PRODUCT( Obj, Vector3D( Val ) )
  ELSE
    Ans = VECTOR_PRODUCT( Obj%Val, Val(1:3) )
  END IF
END PROCEDURE vector_product_2

!----------------------------------------------------------------------------
!                                                            Vector_Product
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_product_3
  Ans = VECTOR_PRODUCT(Obj=Obj, Val=Val)
END PROCEDURE vector_product_3

!----------------------------------------------------------------------------
!                                                            Vector_Product
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_product_4
  Ans = DOT_PRODUCT( u%val, w%val ) * v%val- DOT_PRODUCT( u%val, v%val ) * w%val
END PROCEDURE vector_product_4

!----------------------------------------------------------------------------
!                                                                 Norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE Norm2_obj
  Ans = SQRT( DOT_PRODUCT( Obj%Val, Obj%Val ) )
END PROCEDURE Norm2_obj

!----------------------------------------------------------------------------
!                                                                 UnitVector
!----------------------------------------------------------------------------

MODULE PROCEDURE get_UnitVector
  Ans = Obj % Val / NORM2( Obj % Val )
END PROCEDURE get_UnitVector

!----------------------------------------------------------------------------
!                                                                 Angle
!----------------------------------------------------------------------------

MODULE PROCEDURE get_angle
  Ans = ACOS( DOT_PRODUCT( u, v ) / NORM2(u) / NORM2(v) )
END PROCEDURE get_angle

!----------------------------------------------------------------------------
!                                                           ProjectionVector
!----------------------------------------------------------------------------

MODULE PROCEDURE get_projection_vector_obj
  Ans = (DOT_PRODUCT( u, v ) / DOT_PRODUCT( v, v )) * v % val
END PROCEDURE get_projection_vector_obj

!----------------------------------------------------------------------------
!                                                                 Normal
!----------------------------------------------------------------------------

MODULE PROCEDURE getNormal_Vector
  Ans = u % val - (DOT_PRODUCT( u, v ) / DOT_PRODUCT( v, v )) * v % val
END PROCEDURE getNormal_Vector

!----------------------------------------------------------------------------
!                                                                 Projection
!----------------------------------------------------------------------------

MODULE PROCEDURE get_projection_obj
  Ans = DOT_PRODUCT( u, v ) / NORM2( v )
END PROCEDURE get_projection_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Misc