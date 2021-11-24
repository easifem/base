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

MODULE FEMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: MassMatrix
PUBLIC :: DiffusionMatrix
PUBLIC :: StiffnessMatrix
PUBLIC :: NitscheMatrix
PUBLIC :: ConvectiveMatrix
PUBLIC :: STConvectiveMatrix

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
! $$\int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}} \frac{\partial N^
! {J}}{\partial x_{i}} d\Omega$$

INTERFACE
  MODULE PURE FUNCTION Space_DiffusionMatrix(Test, Trial, nCopy) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nCopy
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION Space_DiffusionMatrix
END INTERFACE

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
! $$\int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}} k_{ij}
!\frac{\partial N^{J}}{\partial x_{j}} d\Omega$$

INTERFACE
  MODULE PURE FUNCTION Space_DiffusionMatrix_K(Test, Trial, K, nCopy) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: K
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nCopy
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION Space_DiffusionMatrix_K
END INTERFACE

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
! $$\int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}} c_i
!\frac{\partial N^{J}}{\partial x_{j}} c_j d\Omega$$
!

INTERFACE
  MODULE PURE FUNCTION Space_DiffusionMatrix_C(Test, Trial, C1, C2, nCopy) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: C1, C2
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nCopy
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION Space_DiffusionMatrix_C
END INTERFACE

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION st_diffusionMatrix(Test, Trial, nCopy) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: Test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: Trial(:)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nCopy
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION st_diffusionMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION st_diffusionMatrix_K(Test, Trial, K, nCopy) &
    & RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: Test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: Trial(:)
    CLASS(FEVariable_), INTENT(IN) :: K
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nCopy
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION st_diffusionMatrix_K
END INTERFACE

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION st_diffusionMatrix_C(Test, Trial, C1, C2, nCopy) &
    & RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: Test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: Trial(:)
    CLASS(FEVariable_), INTENT(IN) :: C1
    CLASS(FEVariable_), INTENT(IN) :: C2
    INTEGER(I4B), INTENT(IN), OPTIONAL :: nCopy
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION st_diffusionMatrix_C
END INTERFACE

!----------------------------------------------------------------------------
!                                     DiffusionMatrix@DiffusionMatrixMethods
!----------------------------------------------------------------------------

INTERFACE DiffusionMatrix
  MODULE PROCEDURE &
    & Space_DiffusionMatrix, &
    & Space_DiffusionMatrix_K,&
    & Space_DiffusionMatrix_C, &
    & st_DiffusionMatrix, &
    & st_DiffusionMatrix_K,&
    & st_DiffusionMatrix_C
END INTERFACE DiffusionMatrix

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION femat_StiffnessMatrix1(Test, Trial, Cijkl) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: Cijkl
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION femat_StiffnessMatrix1
END INTERFACE

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION femat_StiffnessMatrix2(Test, Trial, Lambda, Mu) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    CLASS(FEVariable_), INTENT(IN) :: Lambda, Mu
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION femat_StiffnessMatrix2
END INTERFACE

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION femat_StiffnessMatrix3(Test, Trial, Lambda,  &
    & Mu) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    REAL(DFP), INTENT(IN) :: Lambda, Mu
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION femat_StiffnessMatrix3
END INTERFACE

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION femat_StiffnessMatrix4(Test, Trial, Cijkl) &
    & RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: Test, Trial
    REAL(DFP), INTENT(IN) :: Cijkl(:, :)
    REAL(DFP), ALLOCATABLE :: Ans(:, :)
  END FUNCTION femat_StiffnessMatrix4
END INTERFACE

!----------------------------------------------------------------------------
!                                     StiffnessMatrix@StiffnessMatrixMethods
!----------------------------------------------------------------------------

INTERFACE StiffnessMatrix
  MODULE PROCEDURE femat_StiffnessMatrix1, femat_StiffnessMatrix2, &
    & femat_StiffnessMatrix3, femat_StiffnessMatrix4
END INTERFACE StiffnessMatrix

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

!----------------------------------------------------------------------------
!                                         NitscheMatrix@NitscheMatrixMethods
!----------------------------------------------------------------------------

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_1, space_nitsche_mat_2, &
    & space_nitsche_mat_3, space_nitsche_mat_4, space_nitsche_mat_5, &
    & space_nitsche_mat_7
END INTERFACE NitscheMatrix

!----------------------------------------------------------------------------
!                                   ConvectiveMatrix@ConvectiveMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: returns the convective matrix
!
!
!# Introduction
!
!$$
!{}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} N^I c_k
!\frac{\partial N^J}{\partial x_k} d{\Omega} \quad u_{iJ}
!$$
!
!$$
!{}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} c_k
!\frac{\partial N^I}{\partial x_k} N^J d{\Omega} \quad u_{iJ}
!$$

INTERFACE
  MODULE PURE FUNCTION ConvectiveMatrix_1(test, trial, c, term1, &
       & term2) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    TYPE(FEVariable_), INTENT(IN) :: c
    INTEGER(I4B), INTENT(IN) :: term1
    INTEGER(I4B), INTENT(IN) :: term2
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
!
!
!# Introduction
!
! This routine similar to [[ConvectiveMatrix_1]], but the only difference
! is that convective velocity is constant.
! This subroutine reduces the overhead.

INTERFACE
  MODULE PURE FUNCTION ConvectiveMatrix_1b(test, trial, c, term1, &
       & term2) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    REAL(DFP), INTENT(IN) :: c(:)
    INTEGER(I4B), INTENT(IN) :: term1
    INTEGER(I4B), INTENT(IN) :: term2
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ConvectiveMatrix_1b
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE ConvectiveMatrix_1b
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                   ConvectiveMatrix@ConvectiveMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: returns the space-time convective matrix in rank2 array
!
!
!# Introduction
!
! $$
! M(I,J) = \int_{\Omega} N^I \frac{\partial N^J}{\partial x} d{\Omega}
! $$
!
! $$
! M(!,J) = \int_{\Omega} \frac{\partial N^I}{\partial x} N^J d{\Omega}
! $$
!
! $$
! M(I,J) = \int_{\Omega} c N^I \frac{\partial N^J}{\partial y} d{\Omega}
! $$
!
! $$
! M(I,J) =  \int_{\Omega} c \frac{\partial N^I}{\partial y} N^J d{\Omega}
! $$
!
! $$
! M(I,J) =  \int_{\Omega} c N^I \frac{\partial N^J}{\partial z} d{\Omega}
! $$
!
! $$
! M(I,J) =  \int_{\Omega} c \frac{\partial N^I}{\partial z} N^J d{\Omega}
! $$

INTERFACE
  MODULE PURE FUNCTION ConvectiveMatrix_2(test, trial, term1, &
       & term2, dim, c) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    !! test function
    CLASS(ElemshapeData_), INTENT(IN) :: trial
    !! trial function
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1, if 1, then first term contains the space derivative
    !! if 0, then the first term does not contain the space derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2, if 1, then second term contains the space derivative
    !! if 0, then the second term does not contains the space derivative
    INTEGER(I4B), INTENT(IN) :: dim
    !! it has one of the following values
    !! 1 => dx
    !! 2 => dy
    !! 3 => dz
    !! dim should be less than or equal to the nsd of elemsd
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    !! a scalar finite element variable
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned finte element matrix
  END FUNCTION ConvectiveMatrix_2
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE ConvectiveMatrix_2
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array
!
!# Introduction
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}} \cdot
! {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left(I,J,a,b\right)=\int_{I_{n}}\int_{\Omega}N^{I}T_{a}c_{j}\frac{\partial
! N^{J}T_{b}}{\partial x_{j}}d\Omega dt
! $$
!
! $$
! {M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}}
! \cdot {N^J}{T_b}d\Omega dt} } } \right]{\delta _{pq}}
! $$
!
! $$
! M\left( {I,J,a,b} \right) = \int_{{I_n}}^{} {\int_\Omega ^{}
! {\frac{{\partial {N^I}{T_a}}}{{\partial t}}
! \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} }
! {\delta _{pq}}
! $$

INTERFACE
  MODULE FUNCTION Mat4_STConvectiveMatrix_1(test, trial, c, term1, term2) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity, it can be
    !! - nodal/quadrature  variable
    !!    - constant
    !!    - space
    !!    - time
    !!    - spacetime
    INTEGER(I4B), INTENT(IN) :: term1
    !! If 0 then time derivative in first term is absent
    !! If 1 then first order time derivative is present in first term
    INTEGER(I4B), INTENT(IN) :: term2
    !! If 0 then time derivative in second term absent
    !! If 1 then first order time derivative is present in the second term
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_1
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_1
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                    STConvectiveMatrix@STConvectiveMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-23
! update: 2021-11-23
! summary: Returns the space-time convective matrix in rank4 array
!
!# Introduction
!
! This is same as [[Mat4_STConvectiveMatrix_1]] , but here velocity is
! constant. This is just to remove some overhead in simple cases.

INTERFACE
  MODULE FUNCTION Mat4_STConvectiveMatrix_1b(test, trial, c, term1, term2) &
       & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    REAL(DFP), INTENT(IN) :: c(:)
    !! constant convective velocity
    INTEGER(I4B), INTENT(IN) :: term1
    !! If 0 then time derivative in first term is absent
    !! If 1 then first order time derivative is present in first term
    INTEGER(I4B), INTENT(IN) :: term2
    !! If 0 then time derivative in second term absent
    !! If 1 then first order time derivative is present in the second term
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_1b
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_1b
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                         ConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix
!
!# Introduction
!
! $$
!   M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
!   {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}} \cdot
!   {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left(I,J,a,b\right)=\int_{I_{n}}\int_{\Omega}N^{I}T_{a}c_{j}\frac{\partial
! N^{J}T_{b}}{\partial x_{j}}d\Omega dt
! $$
!
! $$
! {M^{pq}}\left( {I,J,a,b} \right) = \left[ {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{c_j}\frac{{\partial {N^I}{T_a}}}{{\partial {x_j}}}
! \cdot {N^J}{T_b}d\Omega dt} } } \right]{\delta _{pq}}
! $$
!
! $$
! M\left( {I,J,a,b} \right) = \int_{{I_n}}^{} {\int_\Omega ^{}
! {\frac{{\partial {N^I}{T_a}}}{{\partial t}}
! \cdot {c_j}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} }
! {\delta _{pq}}
! $$

INTERFACE
  MODULE FUNCTION Mat2_STConvectiveMatrix_1(test, trial, c, term1, term2) &
       & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity, it can be
    !! - nodal/quadrature  variable
    !!    - constant
    !!    - space
    !!    - time
    !!    - spacetime
    INTEGER(I4B), INTENT(IN) :: term1
    !! If 0 then time derivative in first term is absent
    !! If 1 then first order time derivative is present in first term
    INTEGER(I4B), INTENT(IN) :: term2
    !! If 0 then time derivative in second term absent
    !! If 1 then first order time derivative is present in the second term
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned finite element matrix.
  END FUNCTION Mat2_STConvectiveMatrix_1
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_1
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                      STConvectiveMatrix@STConvectiveMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-23
! update: 2021-11-23
! summary: Returns space-time convective matrix in Rank2 array
!
!# Introduction
!
! This rotuine is same as [[Mat2_STConvectiveMatrix_1]], but here
! convective velocity is constant.
! It removes some overhead.

INTERFACE
  MODULE FUNCTION Mat2_STConvectiveMatrix_1b(test, trial, c, term1, term2) &
       & RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    REAL(DFP), INTENT(IN) :: c(:)
    !! convective velocity, it can be
    !! - nodal/quadrature  variable
    !!    - constant
    !!    - space
    !!    - time
    !!    - spacetime
    INTEGER(I4B), INTENT(IN) :: term1
    !! If 0 then time derivative in first term is absent
    !! If 1 then first order time derivative is present in first term
    INTEGER(I4B), INTENT(IN) :: term2
    !! If 0 then time derivative in second term absent
    !! If 1 then first order time derivative is present in the second term
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned finite element matrix.
  END FUNCTION Mat2_STConvectiveMatrix_1b
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_1b
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!                                      STConvectiveMatrix@STConvectiveMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-23
! update: 2021-11-23
! summary: Returns space-time convective matrix in rank4 array
!
!
!# Introduction
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {\frac{{\partial {N^I}{T_a}}}{{\partial x}} c \cdot {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {\frac{{\partial {N^I}{T_a}}}{{\partial y}} c \cdot {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {\frac{{\partial {N^I}{T_a}}}{{\partial z}} c \cdot {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{N^J}{T_b} c \cdot  \frac{{\partial {N^J}{T_b}}}{{\partial x}}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{N^J}{T_b} c \cdot \frac{{\partial {N^J}{T_b}}}{{\partial y}}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{N^J}{T_b} c \cdot \frac{{\partial {N^J}{T_b}}}{{\partial z}}d\Omega dt} } }
! $$
!
! NOTE If `dim` is -1 then this routine performs the following task
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {\frac{{\partial {N^I}{T_a}}}{{\partial x_{i}}} c \cdot
! {N^J}{T_b}d\Omega dt} } }
! $$
!
! $$
! M\left( {I,J,a,b} \right) =  {\int_{{I_n}}^{} {\int_\Omega ^{}
! {{N^J}{T_b} c \cdot
! \frac{{\partial {N^J}{T_b}}}{{\partial x_{i}}}d\Omega dt} } }
! $$

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_2(test, trial, term1, &
       & term2, dim, c) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! test function
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    !! trial function
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1, if 1, then first term contains the space derivative
    !! if 0, then the first term does not contain the space derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2, if 1, then second term contains the space derivative
    !! if 0, then the second term does not contains the space derivative
    INTEGER(I4B), INTENT(IN) :: dim
    !! it has one of the following values
    !! 1 => dx
    !! 2 => dy
    !! 3 => dz
    !! dim should be less than or equal to the nsd of elemsd
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    !! a scalar finite element variable
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finte element matrix
  END FUNCTION Mat4_STConvectiveMatrix_2
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_2
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                      STConvectiveMatrix@STConvectiveMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-24
! update: 2021-11-24
! summary: Returns the space-time convective matrix in a rank-2 array.
!
!# Introduction
!
! This routine works similar to [[Mat4_STConvectiveMatrix_2]] but it
! returns a rank2 array.
!

INTERFACE
  MODULE PURE FUNCTION Mat2_STConvectiveMatrix_2(test, trial, term1, &
       & term2, dim, c) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! test function
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    !! trial function
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1, if 1, then first term contains the space derivative
    !! if 0, then the first term does not contain the space derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2, if 1, then second term contains the space derivative
    !! if 0, then the second term does not contains the space derivative
    INTEGER(I4B), INTENT(IN) :: dim
    !! it has one of the following values
    !! 1 => dx
    !! 2 => dy
    !! 3 => dz
    !! dim should be less than or equal to the nsd of elemsd
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: c
    !! a scalar finite element variable
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned finte element matrix
  END FUNCTION Mat2_STConvectiveMatrix_2
END INTERFACE

INTERFACE ConvectiveMatrix
  MODULE PROCEDURE Mat2_STConvectiveMatrix_2
END INTERFACE ConvectiveMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEMatrix_Method
