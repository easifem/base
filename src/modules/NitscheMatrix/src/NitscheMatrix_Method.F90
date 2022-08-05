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
  MODULE PURE FUNCTION space_nitsche_mat_1(Test, Trial, Lambda, Mu, Evec) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: Lambda, Mu, Evec
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION space_nitsche_mat_1
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_1
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_2(Test, Trial, Alpha, Evec) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: Alpha, Evec
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION space_nitsche_mat_2
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_2
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_3(Test, Trial, Lambda, Mu, Evec) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: Evec
    REAL(DFP), INTENT(IN) :: Lambda, Mu
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION space_nitsche_mat_3
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_3
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_4(Test, Trial, Alpha, Evec) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: Evec
    REAL(DFP), INTENT(IN) :: Alpha
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION space_nitsche_mat_4
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_4
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_5(Test, Trial, Lambda, Mu, isNoSlip)&
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    REAL(DFP), INTENT(IN) :: Lambda, Mu
    LOGICAL(LGT), INTENT(IN) :: isNoSlip
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION space_nitsche_mat_5
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_5
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION space_nitsche_mat_7(Test, Trial, Lambda, Mu, isNoSlip)&
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: Lambda, Mu
    LOGICAL(LGT), INTENT(IN) :: isNoSlip
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION space_nitsche_mat_7
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_7
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE NitscheMatrix_Method
