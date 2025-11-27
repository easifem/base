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
! summary: This subroutine makes mass matrix in space domain (see below)

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

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-27
! summary: This subroutine makes mass matrix in space domain
!
!# Introduction
!
! This subroutine makes space matrix in space domain, Here mass density
! is constant and one.
!
! $$\int_{\Omega } N^{I} N^{J}d\Omega$$

INTERFACE
  MODULE PURE SUBROUTINE MassMatrix1_(test, trial, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! Shape function data for test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function data
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! mass matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! size of mass matrix
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! option for ncopy
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
! summary: This subroutine makes mass matrix in space domain (see below)

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

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-27
! summary: This subroutine makes mass matrix in space domain (see below)
!
!# Introduction
!
! This subroutine makes space matrix in space domain, Here mass density
! is a FEVariable of scalar type.
!
! ans(I,J)=\int N^{I}\rho N^{J}d\Omega

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
! summary: This subroutine makes mass matrix in space domain (see below)

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

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-27
! summary: This subroutine makes mass matrix in space domain
!
!# Introduction
!
! This subroutine makes space matrix in space domain, Here mass density
! is a FEVariable of vector type.
! Based on opt value following tasks can be perfoemd:
!
! opt=1: M_{i1}(I,J)=\int N^{I}v_{i}N^{J}d\Omega
! opt=2: M_{1i}(I,J)=\int N^{I}v_{i}N^{J}d\Omega
! opt=3: M_{ii}(I,J)=\int N^{I}v_{i}N^{J}d\Omega
! opt=4: M_{ij}(I,J)=\int N^{I}v_{i}v_{j}N^{J}d\Omega

INTERFACE
  MODULE PURE SUBROUTINE MassMatrix3_(test, trial, rho, rhorank, opt, &
                                      nrow, ncol, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: rho
    TYPE(FEVariableVector_), INTENT(IN) :: rhorank
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
    REAL(DFP), INTENT(INOUT) :: m4(:, :, :, :)
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
  MODULE PURE FUNCTION MassMatrix_5(test, trial, lambda, mu, rho, &
                                    lambdaRank, muRank, rhoRank) &
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
    TYPE(FEVariableScalar_), INTENT(IN) :: lambdaRank, muRank, rhoRank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MassMatrix_5
END INTERFACE

INTERFACE MassMatrix
  MODULE PROCEDURE MassMatrix_5
END INTERFACE MassMatrix

INTERFACE ViscousBoundaryMassMatrix
  MODULE PROCEDURE MassMatrix_5
END INTERFACE ViscousBoundaryMassMatrix

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-15
! summary: This subroutine makes mass matrix used for viscous boundary

INTERFACE
  MODULE PURE SUBROUTINE MassMatrix5_( &
    test, trial, lambda, mu, rho, lambdaRank, muRank, rhoRank, ans, &
    nrow, ncol)
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
    TYPE(FEVariableScalar_), INTENT(IN) :: lambdaRank, muRank, rhoRank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE MassMatrix5_
END INTERFACE

INTERFACE MassMatrix_
  MODULE PROCEDURE MassMatrix5_
END INTERFACE MassMatrix_

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-15
! summary: This subroutine makes mass matrix mass routine

INTERFACE
  MODULE PURE SUBROUTINE MassMatrix6_( &
    N, M, js, ws, thickness, nips, nns1, nns2, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! test function data
    REAL(DFP), INTENT(IN) :: M(:, :)
    !! trial function data
    REAL(DFP), INTENT(IN) :: js(:)
    !! Jacobian determinant at integration points
    REAL(DFP), INTENT(IN) :: ws(:)
    !! Weights at integration points
    REAL(DFP), INTENT(IN) :: thickness(:)
    !! thickness at integration points
    INTEGER(I4B), INTENT(IN) :: nips, nns1, nns2
    !! number of integration points
    !! number of shape functions for test function
    !! number of shape functions for trial function
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE MassMatrix6_
END INTERFACE

INTERFACE MassMatrix_
  MODULE PROCEDURE MassMatrix6_
END INTERFACE MassMatrix_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MassMatrix_Method
