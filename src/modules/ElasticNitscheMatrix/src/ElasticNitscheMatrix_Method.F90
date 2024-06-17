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

MODULE ElasticNitscheMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: ElasticNitscheMatrix
PUBLIC :: ElasticNitscheMatrixNormal
PUBLIC :: ElasticNitscheMatrixTangent

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1a(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: lambda
    CLASS(FEVariable_), INTENT(IN) :: mu
    CLASS(FEVariable_), INTENT(IN) :: evec
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1a
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1a
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1b(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: evec
    REAL(DFP), INTENT(IN) :: lambda, mu
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1b
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1b
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                               ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1c(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: evec
    REAL(DFP), INTENT(IN) :: lambda(:)
    !! quadrature values
    REAL(DFP), INTENT(IN) :: mu(:)
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1c
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1c
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                               ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1d(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: evec(:, :)
    !! vector at quadrature value
    REAL(DFP), INTENT(IN) :: lambda(:)
    !! quadrature values
    REAL(DFP), INTENT(IN) :: mu(:)
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1d
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1d
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1e(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: evec(:, :)
    !! vector at quadrature value
    REAL(DFP), INTENT(IN) :: lambda, mu
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1e
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1e
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1f(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: evec(:)
    !! constant vector
    REAL(DFP), INTENT(IN) :: lambda, mu
    !! constant lambda and mu
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1f
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1f
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                               ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1g(test, trial, lambda, mu, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: evec(:)
    !! vector at quadrature value
    REAL(DFP), INTENT(IN) :: lambda(:)
    !! quadrature values
    REAL(DFP), INTENT(IN) :: mu(:)
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1g
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1g
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                               ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1h(test, trial, lambda, mu, dim) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    INTEGER(I4B), INTENT(IN) :: dim
    !! evec represent e1 , e2, e3 (1,2,3)
    REAL(DFP), INTENT(IN) :: lambda(:)
    !! quadrature values
    REAL(DFP), INTENT(IN) :: mu(:)
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1h
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1h
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                               ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1i(test, trial, lambda, mu, dim) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    INTEGER(I4B), INTENT(IN) :: dim
    !! evec represent e1 , e2, e3 (1,2,3)
    REAL(DFP), INTENT(IN) :: lambda
    !! quadrature values
    REAL(DFP), INTENT(IN) :: mu
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1i
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1i
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                               ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix1j(test, trial, lambda, mu, dim) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    INTEGER(I4B), INTENT(IN) :: dim
    !! evec represent e1 , e2, e3 (1,2,3)
    !! dim=4 normal direction
    !! dim=5 tangent direction
    TYPE(FEVariable_), INTENT(IN) :: lambda
    !! quadrature values
    TYPE(FEVariable_), INTENT(IN) :: mu
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix1j
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix1j
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ElasticNitscheMatrix2a(test, trial, lambda, mu, isNoSlip)&
                                                                 & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda, mu
    LOGICAL(LGT), INTENT(IN) :: isNoSlip
    !! this is a dummy variable, It is used only to create distinct interface
    !! It is not used in the routine
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix2a
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix2a
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix2b(test, trial, lambda, mu, isNoSlip)&
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: lambda, mu
    LOGICAL(LGT), INTENT(IN) :: isNoSlip
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix2b
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix2b
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                              ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix3a(test, trial, alpha, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: alpha
    CLASS(FEVariable_), INTENT(IN) :: evec
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix3a
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix3a
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                               ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix3b(test, trial, alpha, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: evec
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix3b
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix3b
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                              ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix3c(test, trial, alpha, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: alpha(:)
    REAL(DFP), INTENT(IN) :: evec(:, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix3c
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix3c
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                              ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix3d(test, trial, alpha, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: evec(:, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix3d
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix3d
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                              ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix3e(test, trial, alpha, evec) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: evec(:)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix3e
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix3e
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                              ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix3f(test, trial, alpha, dim) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    CLASS(FEVariable_), INTENT(IN) :: alpha
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix3f
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix3f
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                               ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix3g(test, trial, alpha, dim) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: alpha
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix3g
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix3g
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                              ElasticNitscheMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrix3h(test, trial, alpha, dim) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: alpha(:)
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrix3h
END INTERFACE

INTERFACE ElasticNitscheMatrix
  MODULE PROCEDURE ElasticNitscheMatrix3h
END INTERFACE ElasticNitscheMatrix

!----------------------------------------------------------------------------
!                                        ElasticNitscheMatrixNormal@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrixNormal1a(test, trial, lambda, mu) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), INTENT(IN) :: lambda(:)
    !! quadrature values
    REAL(DFP), INTENT(IN) :: mu(:)
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrixNormal1a
END INTERFACE

INTERFACE ElasticNitscheMatrixNormal
  MODULE PROCEDURE ElasticNitscheMatrixNormal1a
END INTERFACE ElasticNitscheMatrixNormal

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrixNormal@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrixNormal1b(test, trial, lambda, mu) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), INTENT(IN) :: lambda
    !! quadrature values
    REAL(DFP), INTENT(IN) :: mu
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrixNormal1b
END INTERFACE

INTERFACE ElasticNitscheMatrixNormal
  MODULE PROCEDURE ElasticNitscheMatrixNormal1b
END INTERFACE ElasticNitscheMatrixNormal

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrixNormal@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrixNormal1c(test, trial, lambda, mu) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    TYPE(FEVariable_), INTENT(IN) :: lambda
    !! quadrature values
    TYPE(FEVariable_), INTENT(IN) :: mu
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrixNormal1c
END INTERFACE

INTERFACE ElasticNitscheMatrixNormal
  MODULE PROCEDURE ElasticNitscheMatrixNormal1c
END INTERFACE ElasticNitscheMatrixNormal

!----------------------------------------------------------------------------
!                                        ElasticNitscheMatrixTangent@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrixTangent1a(test, trial, mu, &
    & jacobian) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), INTENT(IN) :: mu(:)
    !! quadrature values
    REAL(DFP), INTENT(IN) :: jacobian(:, :, :)
    !! jacobian
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrixTangent1a
END INTERFACE

INTERFACE ElasticNitscheMatrixTangent
  MODULE PROCEDURE ElasticNitscheMatrixTangent1a
END INTERFACE ElasticNitscheMatrixTangent

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrixTangent@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrixTangent1b(test, trial, mu) &
         & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), INTENT(IN) :: mu
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrixTangent1b
END INTERFACE

INTERFACE ElasticNitscheMatrixTangent
  MODULE PROCEDURE ElasticNitscheMatrixTangent1b
END INTERFACE ElasticNitscheMatrixTangent

!----------------------------------------------------------------------------
!                                         ElasticNitscheMatrixTangent@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ElasticNitscheMatrixTangent1c(test, trial, mu) &
          & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    TYPE(FEVariable_), INTENT(IN) :: mu
    !! quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ElasticNitscheMatrixTangent1c
END INTERFACE

INTERFACE ElasticNitscheMatrixTangent
  MODULE PROCEDURE ElasticNitscheMatrixTangent1c
END INTERFACE ElasticNitscheMatrixTangent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ElasticNitscheMatrix_Method
