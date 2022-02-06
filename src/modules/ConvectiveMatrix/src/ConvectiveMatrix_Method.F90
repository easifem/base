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
! date: 6 March 2021
! summary: This module contains method to construct finite element matrices

MODULE ConvectiveMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: ConvectiveMatrix

!----------------------------------------------------------------------------
!                                   ConvectiveMatrix@ConvectiveMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: returns the convective matrix

INTERFACE
  MODULE PURE FUNCTION ConvectiveMatrix_1(test, trial, term1, &
      & term2, opt) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_x, del_y, del_z, del_x_all, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_x, del_y, del_z, del_x_all, del_none
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ConvectiveMatrix_1
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE ConvectiveMatrix_1
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                   ConvectiveMatrix@ConvectiveMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: returns the convective matrix

INTERFACE
  MODULE PURE FUNCTION ConvectiveMatrix_2(test, trial, c, crank, term1, &
      & term2, opt) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    TYPE(FEVariable_), INTENT(IN) :: c
    !! scalar variable
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    !! scalar variable
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_x, del_y, del_z, del_x_all, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_x, del_y, del_z, del_x_all, del_none
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    !! number of copies
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ConvectiveMatrix_2
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE ConvectiveMatrix_2
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                   ConvectiveMatrix@ConvectiveMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: returns the convective matrix

INTERFACE
  MODULE PURE FUNCTION ConvectiveMatrix_3(test, trial, c, crank, term1, &
      & term2, opt) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    TYPE(FEVariable_), INTENT(IN) :: c
    !! It can be a scalar or vector variable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! It can be a scalar or vector variable
    INTEGER(I4B), INTENT(IN) :: term1
    !! del_x, del_y, del_z, del_x_all, del_none
    INTEGER(I4B), INTENT(IN) :: term2
    !! del_x, del_y, del_z, del_x_all, del_none
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    !! number of copies
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ConvectiveMatrix_3
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE ConvectiveMatrix_3
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConvectiveMatrix_Method
