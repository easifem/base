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

MODULE ElemshapeData_ProjectionMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: GetProjectionOfdNdXt
PUBLIC :: GetProjectionOfdNdXt_
PUBLIC :: GetProjectionOfdNTdXt

! TODO: implement
! PUBLIC :: getProjectionOfdNTdXt_

!----------------------------------------------------------------------------
!                                                        GetProjectionOfdNdXt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt_1(obj, c, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: c(:)
    !! constant value of vector
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
  END SUBROUTINE GetProjectionOfdNdXt_1
END INTERFACE

INTERFACE GetProjectionOfdNdXt
  MODULE PROCEDURE GetProjectionOfdNdXt_1
END INTERFACE GetProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNdXt_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-05
! summary:  get interpolation of vector without allocation

INTERFACE
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt1_(obj, c, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE GetProjectionOfdNdXt1_
END INTERFACE

INTERFACE GetProjectionOfdNdXt_
  MODULE PROCEDURE GetProjectionOfdNdXt1_
END INTERFACE GetProjectionOfdNdXt_

!----------------------------------------------------------------------------
!                                                        GetProjectionOfdNdXt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt_2(obj, c, crank, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! ElemshapeData object
    CLASS(FEVariable_), INTENT(IN) :: c
    !! FEVariable vector
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! rank of c should be vector
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
  END SUBROUTINE GetProjectionOfdNdXt_2
END INTERFACE

INTERFACE GetProjectionOfdNdXt
  MODULE PROCEDURE GetProjectionOfdNdXt_2
END INTERFACE GetProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                                       GetProjectionofdNdXt_
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt2_(obj, c, crank, ans, nrow, &
                                                ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    CLASS(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE GetProjectionOfdNdXt2_
END INTERFACE

INTERFACE GetProjectionOfdNdXt_
  MODULE PROCEDURE GetProjectionOfdNdXt2_
END INTERFACE GetProjectionOfdNdXt_

!----------------------------------------------------------------------------
!                                            getProjectionOfdNdXt@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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

INTERFACE GetProjectionOfdNdXt
  MODULE PURE SUBROUTINE getProjectionOfdNdXt_3(obj, cdNdXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! ElemshapeData object
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNdXt(:, :)
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! a vector, defined over quadrature points
  END SUBROUTINE getProjectionOfdNdXt_3
END INTERFACE GetProjectionOfdNdXt

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-05
! summary:  get interpolation of vector without allocation

INTERFACE GetProjectionOfdNdXt_
  MODULE PURE SUBROUTINE getProjectionOfdNdXt3_(obj, cdNdXt, val, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: cdNdXt(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE getProjectionOfdNdXt3_
END INTERFACE GetProjectionOfdNdXt_

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-23
! update: 2021-11-23
! summary: Computes the projection of dNTdXt on a vector
!
! This subroutine computes the projcetion cdNTdXt on the vector `val`
! Here the vector `val` is constant in space and time
!
! $$P^{I,a}=c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$$

INTERFACE GetProjectionOfdNTdXt
  MODULE PURE SUBROUTINE getProjectionOfdNTdXt_1(obj, cdNTdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNTdXt(:, :, :)
    !! returned $c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$
    REAL(DFP), INTENT(IN) :: val(:)
    !! constant value of vector
  END SUBROUTINE getProjectionOfdNTdXt_1
END INTERFACE GetProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
INTERFACE GetProjectionOfdNTdXt
  MODULE PURE SUBROUTINE getProjectionOfdNTdXt_2(obj, cdNTdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNTdXt(:, :, :)
    !! returned $c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$
    TYPE(FEVariable_), INTENT(IN) :: val
    !! constant value of vector
  END SUBROUTINE getProjectionOfdNTdXt_2
END INTERFACE GetProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
INTERFACE GetProjectionOfdNTdXt
  MODULE PURE SUBROUTINE getProjectionOfdNTdXt_3(obj, cdNTdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNTdXt(:, :, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE getProjectionOfdNTdXt_3
END INTERFACE GetProjectionOfdNTdXt

END MODULE ElemshapeData_ProjectionMethods
