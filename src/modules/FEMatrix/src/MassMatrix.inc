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

PUBLIC :: MassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain
!
!# Introduction
!
! This subroutine makes space matrix in space domain, Here Rho $\rho$ is a
! finite element variable
!
! $$\int_{\Omega } N^{I}\rho N^{J}d\Omega$$
!

INTERFACE
  MODULE PURE FUNCTION MassMatrix_1(test, trial, rho) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! Shapedata for test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! Shapedata for trial function
    CLASS(FEVariable_), INTENT(IN), OPTIONAL :: rho
    !! Finite element variable (density)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MassMatrix_1
END INTERFACE

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space time domain
!
!# Introduction
!
! This subroutine makes space matrix in space domain, Here Rho $\rho$ is a
!  finite element variable. Following expression can be evaluated
!
! $$\int_{\Omega } N^{I}T_{a}\rho N^{J}T_{b}d\Omega$$
! $$\int \frac{\partial N^{I}T_{a}}{\partial t} \rho N^{J}T_{b}d\Omega dt$$
! $$\int \frac{\partial N^{I}T_{a}}{\partial t} \rho
! \frac{\partial N^{J}T_{b}}{\partial t} d\Omega dt$$
! $$\int N^{I}T_{a}\rho \frac{\partial N^{J}T_{b}}{\partial t} d\Omega dt$$
!

INTERFACE
  MODULE PURE FUNCTION MassMatrix_2(test, trial, rho, term1, term2)&
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: rho
    INTEGER(I4B), INTENT(IN) :: term1
    !! If 0 then time derivative in first term is absent
    !! If 1 then first order time derivative is present in first term
    INTEGER(I4B), INTENT(IN) :: term2
    !! If 0 then time derivative in second term absent
    !! If 1 then first order time derivative is present in the second term
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned finite element matrix.
  END FUNCTION MassMatrix_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 MassMatrix
!----------------------------------------------------------------------------

INTERFACE MassMatrix
  MODULE PROCEDURE MassMatrix_1, MassMatrix_2
END INTERFACE MassMatrix
