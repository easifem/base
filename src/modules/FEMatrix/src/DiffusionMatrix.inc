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

PUBLIC :: DiffusionMatrix

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine returns the diffusion matrix in space domain
!
!# Introduction
!
! This function returns the diffusion matrix in space domain
!
! $$
! \int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}}
! \frac{\partial N^{J}}{\partial x_{i}} d\Omega
! $$

INTERFACE
  MODULE PURE FUNCTION DiffusionMatrix_1(test, trial) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_1
END INTERFACE

INTERFACE DiffusionMatrix
  MODULE PROCEDURE DiffusionMatrix_1
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine returns the diffusion matrix in space domain
!
!# Introduction
!
! This function returns the diffusion matrix in space domain
!
! $$
! \int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}} k_{ij}
! \frac{\partial N^{J}}{\partial x_{j}} d\Omega
! $$

INTERFACE
  MODULE PURE FUNCTION DiffusionMatrix_2(test, trial, k) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: k
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_2
END INTERFACE

INTERFACE DiffusionMatrix
  MODULE PROCEDURE DiffusionMatrix_2
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine returns the diffusion matrix in space domain
!
!# Introduction
!
! This function returns the diffusion matrix in space domain
!
! $$
! \int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}} c_i
!\frac{\partial N^{J}}{\partial x_{j}} c_j d\Omega
! $$
!

INTERFACE
  MODULE PURE FUNCTION DiffusionMatrix_3(test, trial, c1, c2) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: c1, c2
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_3
END INTERFACE

INTERFACE DiffusionMatrix
  MODULE PROCEDURE DiffusionMatrix_3
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat2_STDiffusionMatrix_1(test, trial) &
       & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION mat2_STDiffusionMatrix_1
END INTERFACE

INTERFACE DiffusionMatrix
  MODULE PROCEDURE mat2_STDiffusionMatrix_1
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat2_STDiffusionMatrix_2(test, trial, k) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: k
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION mat2_STDiffusionMatrix_2
END INTERFACE

INTERFACE DiffusionMatrix
  MODULE PROCEDURE mat2_STDiffusionMatrix_2
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mat2_STDiffusionMatrix_3(test, trial, c1, c2) &
    & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION mat2_STDiffusionMatrix_3
END INTERFACE

INTERFACE DiffusionMatrix
  MODULE PROCEDURE mat2_STDiffusionMatrix_3
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

