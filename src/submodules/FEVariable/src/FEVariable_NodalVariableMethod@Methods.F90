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

SUBMODULE(FEVariable_NodalVariableMethod) Methods
USE GlobalData, ONLY: Scalar, Vector, Matrix, Constant, Space, &
                      Time, SpaceTime, Nodal, Quadrature

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Constant
#define _DEFINEON_ Nodal
#include "./include/scalar_constant.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Scalar_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Space
#define _DEFINEON_ Nodal
#include "./include/scalar_space.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Scalar_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Time
#define _DEFINEON_ Nodal
#include "./include/scalar_time.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Scalar_Time

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_SpaceTime
#define _DEFINEON_ Nodal
#include "./include/scalar_space_time.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Scalar_SpaceTime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_SpaceTime2
#define _DEFINEON_ Nodal
#include "./include/scalar_space_time2.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Scalar_SpaceTime2

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Constant
#define _DEFINEON_ Nodal
#include "./include/vector_constant.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Vector_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Space
#define _DEFINEON_ Nodal
#include "./include/vector_space.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Vector_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Space2
#define _DEFINEON_ Nodal
#include "./include/vector_space2.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Vector_Space2

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Time
#define _DEFINEON_ Nodal
#include "./include/vector_time.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Vector_Time

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Time2
#define _DEFINEON_ Nodal
#include "./include/vector_time2.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Vector_Time2

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_SpaceTime
#define _DEFINEON_ Nodal
#include "./include/vector_space_time.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Vector_SpaceTime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_SpaceTime2
#define _DEFINEON_ Nodal
#include "./include/vector_space_time2.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Vector_SpaceTime2

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Constant
#define _DEFINEON_ Nodal
#include "./include/matrix_constant.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Matrix_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Constant2
#define _DEFINEON_ Nodal
#include "./include/matrix_constant2.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Matrix_Constant2

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Space
#define _DEFINEON_ Nodal
#include "./include/matrix_space.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Matrix_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Space2
#define _DEFINEON_ Nodal
#include "./include/matrix_space2.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Matrix_Space2

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Time
#define _DEFINEON_ Nodal
#include "./include/matrix_time.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Matrix_Time

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Time2
#define _DEFINEON_ Nodal
#include "./include/matrix_time2.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Matrix_Time2

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_SpaceTime
#define _DEFINEON_ Nodal
#include "./include/matrix_space_time.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Matrix_SpaceTime

!----------------------------------------------------------------------------
!                                                              NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_SpaceTime2
#define _DEFINEON_ Nodal
#include "./include/matrix_space_time2.F90"
#undef _DEFINEON_
END PROCEDURE Nodal_Matrix_SpaceTime2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
