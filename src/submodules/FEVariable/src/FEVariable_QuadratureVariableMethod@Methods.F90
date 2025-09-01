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

SUBMODULE(FEVariable_QuadratureVariableMethod) Methods
USE GlobalData, ONLY: Scalar, Vector, Matrix, Constant, Space, &
                      Time, SpaceTime, Nodal, Quadrature

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Constant
#define _DEFINEON_ Quadrature
#include "./include/scalar_constant.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Scalar_Constant

!----------------------------------------------------------------------------
!                                                     QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Space
#define _DEFINEON_ Quadrature
#include "./include/scalar_space.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Scalar_Space

!----------------------------------------------------------------------------
!                                                     QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Time
#define _DEFINEON_ Quadrature
#include "./include/scalar_time.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Scalar_Time

!----------------------------------------------------------------------------
!                                                     QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_SpaceTime
#define _DEFINEON_ Quadrature
#include "./include/scalar_space_time.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Scalar_SpaceTime

!----------------------------------------------------------------------------
!                                                     QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_SpaceTime2
#define _DEFINEON_ Quadrature
#include "./include/scalar_space_time2.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Scalar_SpaceTime2

!----------------------------------------------------------------------------
!                                                     QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Constant
#define _DEFINEON_ Quadrature
#include "./include/vector_constant.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Vector_Constant

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space
#define _DEFINEON_ Quadrature
#include "./include/vector_space.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Vector_Space

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space2
#define _DEFINEON_ Quadrature
#include "./include/vector_space2.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Vector_Space2

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Time
#define _DEFINEON_ Quadrature
#include "./include/vector_time.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Vector_Time

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Time2
#define _DEFINEON_ Quadrature
#include "./include/vector_time2.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Vector_Time2

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_SpaceTime
#define _DEFINEON_ Quadrature
#include "./include/vector_space_time.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Vector_SpaceTime

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_SpaceTime2
#define _DEFINEON_ Quadrature
#include "./include/vector_space_time2.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Vector_SpaceTime2

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Constant
#define _DEFINEON_ Quadrature
#include "./include/matrix_constant.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Matrix_Constant

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Constant2
#define _DEFINEON_ Quadrature
#include "./include/matrix_constant2.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Matrix_Constant2

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Space
#define _DEFINEON_ Quadrature
#include "./include/matrix_space.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Matrix_Space

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Space2
#define _DEFINEON_ Quadrature
#include "./include/matrix_space2.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Matrix_Space2

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Time
#define _DEFINEON_ Quadrature
#include "./include/matrix_time.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Matrix_Time

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Time2
#define _DEFINEON_ Quadrature
#include "./include/matrix_time2.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Matrix_Time2

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_SpaceTime
#define _DEFINEON_ Quadrature
#include "./include/matrix_space_time.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Matrix_SpaceTime

!----------------------------------------------------------------------------
!                                                       QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_SpaceTime2
#define _DEFINEON_ Quadrature
#include "./include/matrix_space_time2.F90"
#undef _DEFINEON_
END PROCEDURE Quadrature_Matrix_SpaceTime2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
