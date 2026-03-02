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

MODULE DiffusionMatrix_Method
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE

PRIVATE

PUBLIC :: DiffusionMatrix
PUBLIC :: DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If opt is not present.
!
!$$
! M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}\frac{\partial N^{J}}
! {\partial x_{i}}d\Omega
!$$
!
! If opt is present, then it represents number of diagonal copies.
!
!$$
! \left[M\right]_{IJ}^{ij}=\delta_{ij}\int\frac{\partial N^{I}}
! {\partial x_{k}}\frac{\partial N^{J}}{\partial x_{k}}d\Omega
!$$

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_1(test, trial, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_1
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-28
! summary:  DiffusionMatrix without allocation
!
!# DiffusionMatrix_
!
! If opt is not present.
!
!$$
! M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}\frac{\partial N^{J}}
! {\partial x_{i}}d\Omega
!$$
!
! If opt is present, then it represents number of diagonal copies.
!
!$$
! \left[M\right]_{IJ}^{ij}=\delta_{ij}\int\frac{\partial N^{I}}
! {\partial x_{k}}\frac{\partial N^{J}}{\partial x_{k}}d\Omega
!$$

INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix1_(test, trial, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  END SUBROUTINE DiffusionMatrix1_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then following matrix is computed.
!
!$$
! M(I,J)=\int\rho\frac{\partial N^{I}}{\partial x_{i}}
! \frac{\partial N^{J}}{\partial x_{i}}d\Omega
!$$
!
! If `opt` is present then it represents number of diagonal copies.
!
!$$
! \left[M\right]_{IJ}^{ij}=\delta_{ij}\int\rho
! \frac{\partial N^{I}}{\partial x_{k}}
! \frac{\partial N^{J}}{\partial x_{k}}d\Omega
!$$
!
INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_2(test, trial, k, krank, opt) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: k
    !! scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: krank
    !! scalar fe variable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_2
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: Diffusion matrix without allocation
!
!# DiffusionMatrix_
!
! Diffusion matrix without allocation.
!
! If `opt` is not present then following matrix is computed.
!
!$$
! M(I,J)=\int\rho\frac{\partial N^{I}}{\partial x_{i}}
! \frac{\partial N^{J}}{\partial x_{i}}d\Omega
!$$
!
! If `opt` is present then it represents number of diagonal copies.
!
!$$
! \left[M\right]_{IJ}^{ij}=\delta_{ij}\int\rho
! \frac{\partial N^{I}}{\partial x_{k}}
! \frac{\partial N^{J}}{\partial x_{k}}d\Omega
!$$
!

INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix2_(test, trial, k, krank, opt, &
                                           ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test shape function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial shape function
    CLASS(FEVariable_), INTENT(IN) :: k
    !! scalar diffusion coeffcient
    TYPE(FEVariableScalar_), INTENT(IN) :: krank
    !! type fevariable scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE DiffusionMatrix2_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
!If `opt` is not present then following diffusion matrix is computed
!
!$$
!M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}v_{i}v_{j}
!\frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then it represents the number of diagonal matrix.
!
!$$
!\left[M\right]_{IJ}^{ij}=\delta_{ij}\int\frac{\partial N^{I}}{\partial
! x_{p}}v_{p}v_{q}\frac{\partial N^{J}}{\partial x_{q}}d\Omega
!$$

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_3(test, trial, k, krank, opt) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: k
    !! vector
    TYPE(FEVariableVector_), INTENT(IN) :: krank
    !! vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_3
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: DiffusionMatrix without allocation
!
!# DiffusionMatrix_
!
! Diffusion matrix without allocation.
!
!If `opt` is not present then following diffusion matrix is computed
!
!$$
!M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}v_{i}v_{j}
!\frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then it represents the number of diagonal matrix.
!
!$$
!\left[M\right]_{IJ}^{ij}=\delta_{ij}\int\frac{\partial N^{I}}{\partial
! x_{p}}v_{p}v_{q}\frac{\partial N^{J}}{\partial x_{q}}d\Omega
!$$
!
INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix3_( &
    test, trial, k, krank, c1bar, c2bar, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function data
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function data
    CLASS(FEVariable_), INTENT(IN) :: k
    !! vector
    TYPE(FEVariableVector_), INTENT(IN) :: krank
    !! vector FEVariable
    REAL(DFP), INTENT(INOUT) :: c1bar(:)
    !! place holder for keeping projection of test%dNdXt on vector
    !! the size should be atleast test%nns
    REAL(DFP), INTENT(INOUT) :: c2bar(:)
    !! place holder for keeping projection of trial%dNdXt on vector
    !! the size should be atleast trial%nns
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! diffusion matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! option
  END SUBROUTINE DiffusionMatrix3_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then the following Diffusion matrix is
! called.
!
!$$
! M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}k_{ij}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then it represents the number of diagonal copies.
!
!$$
! \left[M\right]_{IJ}^{ij}=\delta_{ij}\int\frac{\partial N^{I}}
! {\partial x_{p}}k_{pq}\frac{\partial N^{J}}{\partial x_{q}}d\Omega
!$$

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_4(test, trial, k, krank, opt) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: k
    !! matrix
    TYPE(FEVariableMatrix_), INTENT(IN) :: krank
    !! matrix
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_4
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: Diffusion matrix without allocation.
!
!# DiffusionMatrix_
!
! Diffusion matrix without allocation.
!
! If `opt` is not present then the following Diffusion matrix is
! called.
!
!$$
! M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}k_{ij}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then it represents the number of diagonal copies.
!
!$$
! \left[M\right]_{IJ}^{ij}=\delta_{ij}\int\frac{\partial N^{I}}
! {\partial x_{p}}k_{pq}\frac{\partial N^{J}}{\partial x_{q}}d\Omega
!$$
INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix4_(test, trial, k, krank, opt, &
                                           ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: k
    TYPE(FEVariableMatrix_), INTENT(IN) :: krank
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE DiffusionMatrix4_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then the following diffussion matrix is
! computed.
!
!$$
! M(I,J)=\int\rho_{1}\rho_{2}\frac{\partial N^{I}}{\partial x_{i}}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then the following diffussion matrix is
! computed. In this case `opt` denotes the number of
! diagonal copies.

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_5(test, trial, c1, c2, c1rank, &
                                         c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Scalar
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    !! Scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_5
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: DiffusionMatrix without allocation
!
!# DiffusionMatrix_
!
! Diffusion matrix without allocation.
!
! If `opt` is not present then the following diffussion matrix is
! computed.
!
!$$
! M(I,J)=\int\rho_{1}\rho_{2}\frac{\partial N^{I}}{\partial x_{i}}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then the following diffussion matrix is
! computed. In this case `opt` denotes the number of
! diagonal copies.
!
INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix5_(test, trial, c1, c2, c1rank, &
                                           c2rank, opt, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE DiffusionMatrix5_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then the following diffusion matrix is
! computed.
!
!$$
!M(I,J)=\int\rho\frac{\partial N^{I}}{\partial x_{i}}v_{i}v_{j}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then the following diffusion matrix is computed.
! In this case `opt` represents the number of diagnonal copies.
!
!$$
!M(I,J)=\delta_{ij}\int\rho\frac{\partial N^{I}}{\partial x_{p}}v_{p}v_{q}
! \frac{\partial N^{J}}{\partial x_{q}}d\Omega
!$$

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_6(test, trial, c1, c2, c1rank, &
                                         c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Scalar
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Vector
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! Vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_6
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: Diffusion matrix without allocation.
!
!# DiffusionMatrix_
!
! Diffusion matrix without allocation.
!
! If `opt` is not present then the following diffusion matrix is
! computed.
!
!$$
!M(I,J)=\int\rho\frac{\partial N^{I}}{\partial x_{i}}v_{i}v_{j}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then the following diffusion matrix is computed.
! In this case `opt` represents the number of diagnonal copies.
!
!$$
!M(I,J)=\delta_{ij}\int\rho\frac{\partial N^{I}}{\partial x_{p}}v_{p}v_{q}
! \frac{\partial N^{J}}{\partial x_{q}}d\Omega
!$$
!
INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix6_( &
    test, trial, c1, c2, c1rank, c2rank, c1bar, c2bar, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: c1
    CLASS(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: c1bar(:)
    !! for internal use only, its size should be test%nns
    REAL(DFP), INTENT(INOUT) :: c2bar(:)
    !! for internal use only, its size should be trial%nns
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  END SUBROUTINE DiffusionMatrix6_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then we compute the following diffusion matrix.
!
!$$
! M(I,J)=\int\rho_{1}\frac{\partial N^{I}}{\partial x_{i}}k_{ij}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then we compute the following diffusion matrix. In
! this case `opt` represents the number of diagonal copies.
!
!$$
! M(I,J)=\delta_{ij}\int\rho_{1}\frac{\partial N^{I}}{\partial x_{p}}k_{pq}
! \frac{\partial N^{J}}{\partial x_{q}}d\Omega
!$$

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_7(test, trial, c1, c2, c1rank, &
                                         c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Scalar
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Matrix
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    !! Matrix
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_7
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix_
!
! If `opt` is not present then we compute the following diffusion matrix.
!
!$$
! M(I,J)=\int\rho_{1}\frac{\partial N^{I}}{\partial x_{i}}k_{ij}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then we compute the following diffusion matrix. In
! this case `opt` represents the number of diagonal copies.
!
!$$
! M(I,J)=\delta_{ij}\int\rho_{1}\frac{\partial N^{I}}{\partial x_{p}}k_{pq}
! \frac{\partial N^{J}}{\partial x_{q}}d\Omega
!$$

INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix7_(test, trial, c1, c2, c1rank, &
                                           c2rank, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function data
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function data
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Scalar fevariable
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Matrix fevariable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    !! Matrix
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! diffusion matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
  END SUBROUTINE DiffusionMatrix7_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then the following DiffusionMatrix is computed.
!
! This method is similar to the DiffusionMatrix_6, but in this case
! c1 is a vector and c2 is a scalar.
!
!$$
!M(I,J)=\int\rho\frac{\partial N^{I}}{\partial x_{i}}v_{i}v_{j}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_8(test, trial, c1, c2, c1rank, &
                                         c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Vector
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Scalar
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    !! Vector
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    !! Scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_8
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix_
!
! If `opt` is not present then the following DiffusionMatrix is computed.
!
! This method is similar to the DiffusionMatrix_6, but in this case
! c1 is a vector and c2 is a scalar.
!
!$$
!M(I,J)=\int\rho\frac{\partial N^{I}}{\partial x_{i}}v_{i}v_{j}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix8_( &
    test, trial, c1, c2, c1rank, c2rank, c1bar, c2bar, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Vector
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Scalar
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    !! Vector
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    !! Scalar
    REAL(DFP), INTENT(INOUT) :: c1bar(:)
    !! for internal use, size should be atleast test%nns
    REAL(DFP), INTENT(INOUT) :: c2bar(:)
    !! for internal use, size should be atleast trial%nns
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! diffusion matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
  END SUBROUTINE DiffusionMatrix8_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then the following matrix is computed.
!
!$$
!M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}v_{i}w_{j}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega!
!$$
!
! If `opt` is present then `opt` denotes the number of diagonal copies.
!
INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_9( &
    test, trial, c1, c2, c1rank, c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Vector
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Vector
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    !! Vector
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! Vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_9
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then the following matrix is computed.
!
!$$
!M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}v_{i}w_{j}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega!
!$$
!
! If `opt` is present then `opt` denotes the number of diagonal copies.
!
INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix9_( &
    test, trial, c1, c2, c1rank, c2rank, c1bar, c2bar, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Vector
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Vector
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    !! Vector
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! Vector
    REAL(DFP), INTENT(INOUT) :: c1bar(:)
    !! internal variable, size should be atleast test%nns
    REAL(DFP), INTENT(INOUT) :: c2bar(:)
    !! internal variable, size should be atleast trial%nns
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! diffusion matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
  END SUBROUTINE DiffusionMatrix9_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
!If `opt` is not present then the following matrix is computed.
!
!$$
! M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}v_{p}b_{pi}v_{q}b_{qj}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
!If opt is present then it represents the number of diagonal copies.
!

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_10(test, trial, c1, c2, c1rank, &
                                          c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Vector
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Matrix
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    !! Vector
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    !! Matrix
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_10
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
!If `opt` is not present then the following matrix is computed.
!
!$$
! M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}v_{p}b_{pi}v_{q}b_{qj}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
!If opt is present then it represents the number of diagonal copies.
!

INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix10_( &
    test, trial, c1, c2, c1rank, c2rank, c1bar, c2bar, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Vector fevariable
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Matrix fevariable
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    !! Vector fevariable
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    !! Matrix fevariable
    REAL(DFP), INTENT(INOUT) :: c1bar(:)
    !! internal variables, size should be atleast test%nns
    REAL(DFP), INTENT(INOUT) :: c2bar(:)
    !! internal variables, size should be atleast trial%nns
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! diffusion matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! number of diagonal copies
  END SUBROUTINE DiffusionMatrix10_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
!If `opt` is not present then the following matrix is computed.
!
!$$
!M(I,J)=\int\rho_{1}\frac{\partial N^{I}}{\partial x_{i}}k_{ij}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
!If opt is present then it represents the number of diagonal copies.
!

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_11(test, trial, c1, c2, c1rank, &
                                          c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Matrix
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Scalar
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! Matrix
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    !! Scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_11
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix_
!
!If `opt` is not present then the following matrix is computed.
!
!$$
!M(I,J)=\int\rho_{1}\frac{\partial N^{I}}{\partial x_{i}}k_{ij}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
!If opt is present then it represents the number of diagonal copies.
!

INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix11_( &
    test, trial, c1, c2, c1rank, c2rank, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Matrix
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Scalar
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! Matrix
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    !! Scalar
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! diffusion matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! number of diagonal copies
  END SUBROUTINE DiffusionMatrix11_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then the following matrix is computed.
!
!$$
!M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}b_{ip}v_{p}b_{jq}v_{q}
!\frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then it represents the number of diagonal
! copies.

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_12(test, trial, c1, c2, c1rank, &
                                          c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Matrix
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Vector
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! Matrix
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! Vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_12
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix_
!
! If `opt` is not present then the following matrix is computed.
!
!$$
!M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}b_{ip}v_{p}b_{jq}v_{q}
!\frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then it represents the number of diagonal
! copies.

INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix12_( &
    test, trial, c1, c2, c1rank, c2rank, c1bar, c2bar, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Matrix
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Vector
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! Matrix
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! Vector
    REAL(DFP), INTENT(INOUT) :: c1bar(:)
    !! internal variable, size should test%nns
    REAL(DFP), INTENT(INOUT) :: c2bar(:)
    !! internal variable, size should trial%nns
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Diffusion matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written to ans
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! number of diagonal copies
  END SUBROUTINE DiffusionMatrix12_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If `opt` is not present then the following diffusion matrix is computed.
!
!$$
!M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}b_{ip}c_{pj}
!\frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then it represents the number of diagonal copies
!
INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_13(test, trial, c1, c2, c1rank, &
                                          c2rank, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Matrix
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Matrix
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! Matrix
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    !! Matrix
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! ncopy
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_13
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix_
!
! If `opt` is not present then the following diffusion matrix is computed.
!
!$$
!M(I,J)=\int\frac{\partial N^{I}}{\partial x_{i}}b_{ip}c_{pj}
!\frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If `opt` is present then it represents the number of diagonal copies
!
INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix13_( &
    test, trial, c1, c2, c1rank, c2rank, ans, nrow, ncol, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: c1
    !! Matrix finite element variable
    CLASS(FEVariable_), INTENT(IN) :: c2
    !! Matrix finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! Matrix finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    !! Matrix finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! diffusion matrix
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! number of diagonal copies
  END SUBROUTINE DiffusionMatrix13_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If opt is [1] then:
!
!$$
! \left[M\right]_{IJ}^{ij}=\int\frac{\partial N^{I}}{\partial x_{i}}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If opt is [2] then:
!
!$$
! \left[M\right]_{IJ}^{ij}=\int\frac{\partial N^{I}}{\partial x_{j}}
! \frac{\partial N^{J}}{\partial x_{i}}d\Omega
!$$

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_14(test, trial, opt) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    INTEGER(I4B), INTENT(IN) :: opt(1)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_14
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
! If opt is [1] then:
!
!$$
! \left[M\right]_{IJ}^{ij}=\int\frac{\partial N^{I}}{\partial x_{i}}
! \frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If opt is [2] then:
!
!$$
! \left[M\right]_{IJ}^{ij}=\int\frac{\partial N^{I}}{\partial x_{j}}
! \frac{\partial N^{J}}{\partial x_{i}}d\Omega
!$$

INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix14_( &
    test, trial, mat4, opt, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), INTENT(INOUT) :: mat4(:, :, :, :)
    !! rank 4 matrix for internal use, it upperbound should be
    !! test%nns, trial%nns, nsd, nsd
    INTEGER(I4B), INTENT(IN) :: opt(1)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE DiffusionMatrix14_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix
!
!This method computes the following DiffusionMatrix.
!
! If opt is equal to 1 then the following diffusion matrix is computed.
!
!$$
!\left[M\right]_{IJ}^{ij}=\int\rho\frac{\partial N^{I}}
!{\partial x_{i}}\frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If opt is equal to 2 then the following diffusion matrix is computed.
!
!$$
! \left[M\right]_{IJ}^{ij}=\int\rho\frac{\partial N^{I}}{\partial x_{j}}
! \frac{\partial N^{J}}{\partial x_{i}}d\Omega
!$$
!

INTERFACE DiffusionMatrix
  MODULE PURE FUNCTION DiffusionMatrix_15(test, trial, k, krank, opt) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: k
    !! scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: krank
    !! scalar fe variable
    INTEGER(I4B), INTENT(IN) :: opt(1)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION DiffusionMatrix_15
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-02
! summary: This subroutine returns the diffusion matrix in space domain
!
!# DiffusionMatrix_
!
!This method computes the following DiffusionMatrix.
!
! If opt is equal to 1 then the following diffusion matrix is computed.
!
!$$
!\left[M\right]_{IJ}^{ij}=\int\rho\frac{\partial N^{I}}
!{\partial x_{i}}\frac{\partial N^{J}}{\partial x_{j}}d\Omega
!$$
!
! If opt is equal to 2 then the following diffusion matrix is computed.
!
!$$
! \left[M\right]_{IJ}^{ij}=\int\rho\frac{\partial N^{I}}{\partial x_{j}}
! \frac{\partial N^{J}}{\partial x_{i}}d\Omega
!$$
!

INTERFACE DiffusionMatrix_
  MODULE PURE SUBROUTINE DiffusionMatrix15_( &
    test, trial, k, krank, mat4, opt, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    CLASS(FEVariable_), INTENT(IN) :: k
    !! scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: krank
    !! scalar fe variable
    REAL(DFP), INTENT(INOUT) :: mat4(:, :, :, :)
    !! for internal use only, upperbounds are test%nns, trial%nns, nsd, nsd
    INTEGER(I4B), INTENT(IN) :: opt(1)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE DiffusionMatrix15_
END INTERFACE DiffusionMatrix_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DiffusionMatrix_Method
