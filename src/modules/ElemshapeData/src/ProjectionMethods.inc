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

PUBLIC :: getProjectionOfdNdXt
PUBLIC :: getProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                     getProjectionOfdNdXt@ProjectionMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: Computes $\frac{dN}{dx_k}c_k$
!
!# Introduction
!
! This subroutine computes the projcetion cdNdXt on the vector `val`
! Here the vector `val` is constant in space and time
!
! $$P^{I}=c_{i}\frac{\partial N^{I}}{\partial x_{i}} $$

INTERFACE
  MODULE PURE SUBROUTINE getProjectionOfdNdXt_1(obj, cdNdXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNdXt(:, :)
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
    REAL(DFP), INTENT(IN) :: val(:)
    !! constant value of vector
  END SUBROUTINE getProjectionOfdNdXt_1
END INTERFACE

INTERFACE getProjectionOfdNdXt
  MODULE PROCEDURE getProjectionOfdNdXt_1
END INTERFACE getProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNdXt@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: computes the projection of dNdXt on a vector
!
!# Introduction
!
! This subroutine computes the projcetion cdNdXt on the vector `val`
! Here the vector `val` is a finite element variable
!
! $$P^{I}=c_{i}\frac{\partial N^{I}}{\partial x_{i}} $$

INTERFACE
  MODULE PURE SUBROUTINE getProjectionOfdNdXt_2(obj, cdNdXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! ElemshapeData object
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNdXt(:, :)
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
    CLASS(FEVariable_), INTENT(IN) :: val
    !! FEVariable vector
  END SUBROUTINE getProjectionOfdNdXt_2
END INTERFACE

INTERFACE getProjectionOfdNdXt
  MODULE PROCEDURE getProjectionOfdNdXt_2
END INTERFACE getProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNdXt@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: computes the projection of dNdXt on a vector
!
!# Introduction
!
! This subroutine computes the projcetion cdNdXt on the vector `val`
! Here the vector `val` is constant in space and time
!
! $$P^{I}=c_{i}\frac{\partial N^{I}}{\partial x_{i}} $$

INTERFACE
  MODULE PURE SUBROUTINE getProjectionOfdNdXt_3(obj, cdNdXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! ElemshapeData object
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNdXt(:, :)
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! a vector, defined over quadrature points
  END SUBROUTINE getProjectionOfdNdXt_3
END INTERFACE

INTERFACE getProjectionOfdNdXt
  MODULE PROCEDURE getProjectionOfdNdXt_3
END INTERFACE getProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-23
! update: 2021-11-23
! summary: Computes the projection of dNTdXt on a vector
!
! This subroutine computes the projcetion cdNTdXt on the vector `val`
! Here the vector `val` is constant in space and time
!
! $$P^{I,a}=c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$$

INTERFACE
  MODULE PURE SUBROUTINE getProjectionOfdNTdXt_1(obj, cdNTdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNTdXt(:, :, :)
    !! returned $c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$
    REAL(DFP), INTENT(IN) :: val(:)
    !! constant value of vector
  END SUBROUTINE getProjectionOfdNTdXt_1
END INTERFACE

INTERFACE getProjectionOfdNTdXt
  MODULE PROCEDURE getProjectionOfdNTdXt_1
END INTERFACE getProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-07
! update: 2021-12-07
! summary:  returns the projection of dNTdXt on a vector
!
!# Introduction
!
! This subroutine computes the projcetion cdNTdXt on the vector `val`
! Here the vector `val` is a vector variable
!
! - It can be constant in space and time
! - It can be vary in space but contant in time
! - It can vary in space and time domain
!
! $$P^{I,a}=c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$$
!
INTERFACE
  MODULE PURE SUBROUTINE getProjectionOfdNTdXt_2(obj, cdNTdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNTdXt(:, :, :)
    !! returned $c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$
    TYPE(FEVariable_), INTENT(IN) :: val
    !! constant value of vector
  END SUBROUTINE getProjectionOfdNTdXt_2
END INTERFACE

INTERFACE getProjectionOfdNTdXt
  MODULE PROCEDURE getProjectionOfdNTdXt_2
END INTERFACE getProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-07
! update: 2021-12-07
! summary:  returns the projection of dNTdXt on a vector
!
!# Introduction
!
!
! This subroutine computes the projcetion cdNTdXt on the vector `val`
! Here the vector `val` is a vector variable
!
! - It can be constant in space and time
! - It can be vary in space but contant in time
! - It can vary in space and time domain
!
! $$P^{I,a}=c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$$
!
INTERFACE
  MODULE PURE SUBROUTINE getProjectionOfdNTdXt_3(obj, cdNTdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNTdXt(:, :, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE getProjectionOfdNTdXt_3
END INTERFACE

INTERFACE getProjectionOfdNTdXt
  MODULE PROCEDURE getProjectionOfdNTdXt_3
END INTERFACE getProjectionOfdNTdXt