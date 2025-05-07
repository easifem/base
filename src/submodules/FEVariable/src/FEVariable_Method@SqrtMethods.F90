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

#define _ELEM_METHOD_ SQRT

SUBMODULE(FEVariable_Method) SqrtMethods
USE GlobalData, ONLY: Constant, Space, Time, SpaceTime, &
                      Scalar, Vector, Matrix, &
                      Nodal, Quadrature
USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableVector, &
                    TypeFEVariableMatrix, &
                    TypeFEVariableConstant, &
                    TypeFEVariableSpace, &
                    TypeFEVariableTime, &
                    TypeFEVariableSpaceTime

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      SQRT
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_sqrt
SELECT CASE (obj%rank)
CASE (scalar)
#include "./include/ScalarElemMethod.F90"
CASE (vector)
#include "./include/VectorElemMethod.F90"
CASE (matrix)
#include "./include/MatrixElemMethod.F90"
END SELECT
END PROCEDURE fevar_sqrt

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SqrtMethods

#undef _ELEM_METHOD_
