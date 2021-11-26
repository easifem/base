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

  PUBLIC :: ConvectiveMatrix
  PUBLIC :: STConvectiveMatrix

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
  MODULE FUNCTION Mat4_STConvectiveMatrix_1(test, trial, c, term1, term2) &
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
