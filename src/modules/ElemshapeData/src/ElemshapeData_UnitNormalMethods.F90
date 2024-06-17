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

module ElemshapeData_UnitNormalMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: GetUnitNormal

!----------------------------------------------------------------------------
!                                                    GetUnitNormal@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine can be used in SUPG formulation
!
!# Introduction
!
! This routine can be used in the SUPG formulation:
!
! $$
! \frac{\nabla \vert \phi \vert}{\Vert \nabla \vert \phi \vert \Vert}
! $$

INTERFACE
  MODULE PURE SUBROUTINE GetUnitNormal_1(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    !! unit vector defined over quadrature points, in xiJ format
    REAL(DFP), INTENT(IN) :: val(:)
    !! spatial nodal values of scalar
  END SUBROUTINE GetUnitNormal_1
END INTERFACE

INTERFACE GetUnitNormal
  MODULE PROCEDURE GetUnitNormal_1
END INTERFACE GetUnitNormal

!----------------------------------------------------------------------------
!                                                    GetUnitNormal@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine can be used in SUPG formulation
!
!# Introduction
!
! This routine can be used in the SUPG formulation, here
! val is spatial nodal values of a vector.
!
! $$
! {\bf r}=\frac{\nabla\Vert{\bf v}\Vert}{\left|\nabla\Vert{\bf v}\Vert\right|}
! $$

INTERFACE
  MODULE PURE SUBROUTINE GetUnitNormal_2(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    !! unit vector defined over quadrature points, in xiJ format
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! spatial nodal values of velocity (vector field)
  END SUBROUTINE GetUnitNormal_2
END INTERFACE

INTERFACE GetUnitNormal
  MODULE PROCEDURE GetUnitNormal_2
END INTERFACE GetUnitNormal

!----------------------------------------------------------------------------
!                                                    GetUnitNormal@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine can be used in SUPG formulation
!
!# Introduction
!
! - This routine can be used in the SUPG formulation.
! - `val` is the [[FEVariable_]], it can be vector or scalar
!
! $$
! \frac{\nabla \vert \phi \vert}{\Vert \nabla \vert \phi \vert \Vert}
! $$
!
! $$
! {\bf r}=\frac{\nabla\Vert{\bf v}\Vert}{\left|\nabla\Vert{\bf v}\Vert\right|}
! $$
!
! TODO: Make implementation simple:
!  extract scalar or vector values from fevariable val,
! and call above routines

INTERFACE
  MODULE PURE SUBROUTINE GetUnitNormal_3(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    !! unit vector defined over quadrature points, in xiJ format
    TYPE(FEVariable_), INTENT(IN) :: val
    !! it can be scalar or vector fe variable
  END SUBROUTINE GetUnitNormal_3
END INTERFACE

INTERFACE GetUnitNormal
  MODULE PROCEDURE GetUnitNormal_3
END INTERFACE GetUnitNormal

end module ElemshapeData_UnitNormalMethods
