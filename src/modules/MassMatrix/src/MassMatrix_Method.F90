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

MODULE MassMatrix_Method
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE GlobalData, ONLY: DFP, I4B, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: MassMatrix
PUBLIC :: MassMatrix_
PUBLIC :: ViscousBoundaryMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain
!
!# Introduction
!
! This subroutine makes space matrix in space domain, Here Rho $\rho$ is a
! finite element variable
!
! $$\int_{\Omega } N^{I} N^{J}d\Omega$$
!

INTERFACE
  MODULE PURE FUNCTION MassMatrix_1(test, trial, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! Shapedata for test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! Shapedata for trial function
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MassMatrix_1
END INTERFACE

INTERFACE MassMatrix
  MODULE PROCEDURE MassMatrix_1
END INTERFACE MassMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE MassMatrix1_(test, trial, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  END SUBROUTINE MassMatrix1_
END INTERFACE

INTERFACE MassMatrix_
  MODULE PROCEDURE MassMatrix1_
END INTERFACE MassMatrix_

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE
  MODULE PURE FUNCTION MassMatrix_2(test, trial, rho, rhorank, opt) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! Shapedata for test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! Shapedata for trial function
    CLASS(FEVariable_), INTENT(IN) :: rho
    TYPE(FEVariableScalar_), INTENT(IN) :: rhorank
    !! Scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MassMatrix_2
END INTERFACE

INTERFACE MassMatrix
  MODULE PROCEDURE MassMatrix_2
END INTERFACE MassMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE MassMatrix2_(test, trial, rho, rhorank, &
                                      ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: rho
    TYPE(FEVariableScalar_), INTENT(IN) :: rhorank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  END SUBROUTINE MassMatrix2_
END INTERFACE

INTERFACE MassMatrix_
  MODULE PROCEDURE MassMatrix2_
END INTERFACE MassMatrix_

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE
  MODULE PURE FUNCTION MassMatrix_3(test, trial, rho, rhorank, opt) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! Shapedata for test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! Shapedata for trial function
    CLASS(FEVariable_), INTENT(IN) :: rho
    !! rho
    TYPE(FEVariableVector_), INTENT(IN) :: rhorank
    !! Vector
    INTEGER(I4B), INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MassMatrix_3
END INTERFACE

INTERFACE MassMatrix
  MODULE PROCEDURE MassMatrix_3
END INTERFACE MassMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-02
! summary:  mass matrix in space
! notice: not implemented yet

INTERFACE
  MODULE PURE SUBROUTINE MassMatrix3_(test, trial, rho, opt, nrow, ncol, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: rho
    INTEGER(I4B), INTENT(IN) :: opt
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
  END SUBROUTINE MassMatrix3_
END INTERFACE

INTERFACE MassMatrix_
  MODULE PROCEDURE MassMatrix3_
END INTERFACE MassMatrix_

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain

INTERFACE
  MODULE PURE FUNCTION MassMatrix_4(test, trial, rho, rhorank) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! Shapedata for test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! Shapedata for trial function
    CLASS(FEVariable_), INTENT(IN) :: rho
    !! coefficient
    TYPE(FEVariableMatrix_), INTENT(IN) :: rhorank
    !! coefficient is a matrix
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MassMatrix_4
END INTERFACE

INTERFACE MassMatrix
  MODULE PROCEDURE MassMatrix_4
END INTERFACE MassMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-02
! summary:  mass matrix in space
! notice: not implemented yet

INTERFACE
  MODULE PURE SUBROUTINE MassMatrix4_( &
    test, trial, rho, rhorank, m4, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: rho
    !! FEVariable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rhorank
    !! Matrix FEVariable
    REAL(DFP), INTENT(INOUT) ::  m4(:, :, :, :)
    !! These matrix is needed internally,
    !! size of m4: nns, nns, size(rho,1), size(rho,2)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! result
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Data written in ans
  END SUBROUTINE MassMatrix4_
END INTERFACE

INTERFACE MassMatrix_
  MODULE PROCEDURE MassMatrix4_
END INTERFACE MassMatrix_

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-15
! summary: This subroutine makes mass matrix used for viscous boundary

INTERFACE
  MODULE PURE FUNCTION MassMatrix_5(test, trial, lambda, mu, rho) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! Shapedata for test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! Shapedata for trial function
    CLASS(FEVariable_), INTENT(IN) :: lambda
      !! Lame parameter
    CLASS(FEVariable_), INTENT(IN) :: mu
      !! Lame parameter
    CLASS(FEVariable_), INTENT(IN) :: rho
      !! Mass Density
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MassMatrix_5
END INTERFACE

INTERFACE ViscousBoundaryMassMatrix
  MODULE PROCEDURE MassMatrix_5
END INTERFACE ViscousBoundaryMassMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MassMatrix_Method
