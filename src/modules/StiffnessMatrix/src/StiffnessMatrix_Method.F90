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

MODULE StiffnessMatrix_Method
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP, LGT, I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: StiffnessMatrix
PUBLIC :: StiffnessMatrix_

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix1(test, trial, Cijkl) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: Cijkl
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix1
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-02-28
! summary: subroutine to calculate stiffness matrix

INTERFACE StiffnessMatrix_
  MODULE PURE SUBROUTINE obj_StiffnessMatrix1_(test, trial, Cijkl, &
                                               nrow, ncol, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: Cijkl
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
  END SUBROUTINE obj_StiffnessMatrix1_
END INTERFACE StiffnessMatrix_

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix2( &
    test, trial, lambda, mu, isLambdaYoungsModulus) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    !! Shape function data
    CLASS(FEVariable_), INTENT(IN) :: lambda, mu
    !! Two elastic parameters
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLambdaYoungsModulus
    !! if it is true then lambda is YoungsModulus
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix2
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix_
  MODULE PURE SUBROUTINE obj_StiffnessMatrix2_( &
    test, trial, lambda, mu, isLambdaYoungsModulus, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    CLASS(FEVariable_), INTENT(IN) :: lambda, mu
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLambdaYoungsModulus
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_StiffnessMatrix2_
END INTERFACE StiffnessMatrix_

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix3(test, trial, lambda, &
                                            mu) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda, mu
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix3
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix_
  MODULE PURE SUBROUTINE obj_StiffnessMatrix3_(test, trial, lambda, &
                                               mu, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda, mu
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_StiffnessMatrix3_
END INTERFACE StiffnessMatrix_

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix4(test, trial, Cijkl) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: Cijkl(:, :)
    ! Constant Cijkl
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix4
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix_
  MODULE PURE SUBROUTINE obj_StiffnessMatrix4_(test, trial, Cijkl, ans, &
                                               nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: Cijkl(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_StiffnessMatrix4_
END INTERFACE StiffnessMatrix_

!----------------------------------------------------------------------------
!                                                    StiffnessMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PURE FUNCTION obj_StiffnessMatrix5(test, trial, lambda, mu) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda(:)
    ! These are quadrature values
    REAL(DFP), INTENT(IN) :: mu(:)
    ! These are quadrature values
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_StiffnessMatrix5
END INTERFACE StiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix_
  MODULE PURE SUBROUTINE obj_StiffnessMatrix5_(test, trial, lambda, mu, &
                                               ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda(:)
    ! These are quadrature values
    REAL(DFP), INTENT(IN) :: mu(:)
    ! These are quadrature values
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_StiffnessMatrix5_
END INTERFACE StiffnessMatrix_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix_
  MODULE PURE SUBROUTINE obj_StiffnessMatrix6_(test, trial, lambda, mu, &
                                               tempMat, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test, trial
    REAL(DFP), INTENT(IN) :: lambda(:)
    ! These are quadrature values
    REAL(DFP), INTENT(IN) :: mu(:)
    ! These are quadrature values
    REAL(DFP), INTENT(INOUT) :: tempMat(:, :)
    !! temperory matrix, size should be atleast nns x nns
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_StiffnessMatrix6_
END INTERFACE StiffnessMatrix_

!----------------------------------------------------------------------------
!                                                           StiffnessMatrix_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-02
! summary: Stiffness matrix with a Voigt tangent at each quadrature point
!
! This routine computes the element stiffness matrix when the material
! tangent is given as a raw array at each quadrature point, for example
! the consistent tangent of an elastoplastic model.
!
! Cijkl(:, :, ips) is the Voigt tangent at quadrature point ips. The
! Voigt order is [11] in 1D, [11, 22, 12] in 2D and
! [11, 22, 33, 12, 23, 13] in 3D. Cijkl maps the engineering strain
! vector (shear $\gamma = 2\varepsilon$) to the stress vector, so
! Cijkl(I, J) equals $C_{ijkl}$. Cijkl need not be symmetric (for
! example non-associative plasticity).
!
! In 2D the tangent must be in the reduced order [11, 22, 12]. A 3D
! tangent such as the C of MaterialJacobian_ is reduced first with
! ReduceVoigtTangent_ and VoigtFormPlaneStrain or VoigtFormPlaneStress.
! The axisymmetric form (4 components) is not supported.
!
! The row and column ordering of ans is the same as in the other
! methods: component major, row (i-1)*nns + I.
!
! Formula:
!
! $$
! K_{iI,jJ} = \int_{\Omega} \frac{\partial N^I}{\partial x_k}
! C_{ikjl} \frac{\partial N^J}{\partial x_l} d\Omega
! $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: ElemShapeData_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent_
! USE StiffnessMatrix_Method, ONLY: StiffnessMatrix_
! IMPLICIT NONE
!
! TYPE(ElemShapeData_) :: elemsd
! REAL(DFP) :: Cv(6, 6), Cq(3, 3, 4), K(8, 8)
! INTEGER(I4B) :: nrow, ncol, ips
!
! ! elemsd: Q4 element with 4 quadrature points (not shown)
! CALL ReduceVoigtTangent_(C=IsotropicTangent(lambda=1.3_DFP, &
!                          mu=0.7_DFP), voigtForm=VoigtFormPlaneStrain, &
!                          ans=Cv, nrow=nrow, ncol=ncol)
! DO ips = 1, 4
!   Cq(:, :, ips) = Cv(1:3, 1:3)
! END DO
! CALL StiffnessMatrix_(test=elemsd, trial=elemsd, Cijkl=Cq, ans=K, &
!                       nrow=nrow, ncol=ncol)
! END PROGRAM main
!```
!
! Full example:
! src/modules/StiffnessMatrix/examples/StiffnessMatrix_test_1.F90
!
! See also: [[InternalForceVector_]], [[ReduceVoigtTangent_]]

INTERFACE StiffnessMatrix_
  MODULE PURE SUBROUTINE obj_StiffnessMatrix7_(test, trial, Cijkl, ans, &
                                               nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! shape data of the test function, nns1 nodes
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! shape data of the trial function, nns2 nodes, same nsd and nips
    REAL(DFP), INTENT(IN) :: Cijkl(:, :, :)
    !! Voigt tangent at quadrature points, SHAPE (nv, nv, nips) with
    !! nv = nsd*(nsd+1)/2 (at least)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! stiffness matrix, SIZE at least (nns1*nsd, nns2*nsd)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans, nns1*nsd
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans, nns2*nsd
  END SUBROUTINE obj_StiffnessMatrix7_
END INTERFACE StiffnessMatrix_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE StiffnessMatrix_Method
