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

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This module contains method to construct finite element matrices

MODULE NitscheMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_1(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: lambda
    CLASS(FEVariable_), INTENT(IN) :: mu
    CLASS(FEVariable_), INTENT(IN) :: evec
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION space_nitsche_mat_1
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_1
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_3(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: evec
    REAL(DFP), INTENT(IN) :: lambda, mu
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION space_nitsche_mat_3
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_3
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_5(test, trial, lambda, mu, isNoSlip)&
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda, mu
    LOGICAL(LGT), INTENT(IN) :: isNoSlip
    !! this is a dummy variable, It is used only to create distinct interface
    !! It is not used in the routine
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION space_nitsche_mat_5
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_5
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_7(test, trial, lambda, mu, isNoSlip)&
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: lambda, mu
    LOGICAL(LGT), INTENT(IN) :: isNoSlip
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION space_nitsche_mat_7
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_7
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_2(test, trial, alpha, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: alpha, evec
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION space_nitsche_mat_2
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_2
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_4(test, trial, alpha, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: evec
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION space_nitsche_mat_4
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_4
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE NitscheMatrix_Method
