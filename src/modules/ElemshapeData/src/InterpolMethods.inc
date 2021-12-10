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
!
! This file contains the interpolation methods interfaces\

PUBLIC :: getInterpolation
PUBLIC :: Interpolation
PUBLIC :: STInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolations of scalar
!
!# Introduction
!
! This subroutine performs interpolation of a scalar from its spatial nodal
! values.
!
! $$u=u_{I}N^{I}$$
!
! - TODO Make it work when the size of val is not the same as NNS

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_1(obj, Interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:)
    !! Interpolation value of `val` at integration points
    REAL(DFP), INTENT(IN) :: val(:)
    !! spatial nodal values of scalar
  END SUBROUTINE elemsd_getInterpolation_1
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_1
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of scalar FEVariable
!
!# Introduction
!
! Returns the interpolation of scalar variable
! The scalar variable can be+
!
! - constant
! - spatial nodal values
! - spatial quadrature values
! - space-time nodal values
!
!@note
!This routine calls [[Interpolation]] function from the same module.
!@endnote

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_2(obj, Interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: Interpol(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Scalar FE variable
  END SUBROUTINE elemsd_getInterpolation_2
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_2
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of a vector
!
!# Introduction
!
! This subroutine performs interpolation of a vector from its spatial
! nodal values
!
! $$u_{i}=u_{iI}N^{I}$$

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_3(obj, Interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:, :)
    !! Interpol(:,ips) => interpolation value at integration points
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE elemsd_getInterpolation_3
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_3
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_4(obj, Interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal value of matrix
  END SUBROUTINE elemsd_getInterpolation_4
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_4
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix FEVariable
!
!# Introduction
!
! This routine performs interpolation of matrix [[FEVariable_]]
!
! TODO  Test this routine [[elemsd_getInterpolation_5]],
!       make Interpol allocatable

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_5(obj, Interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: Interpol(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
    !! varType
    !! Constant
    !! Space
    !! SpaceTime
  END SUBROUTINE elemsd_getInterpolation_5
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_5
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolations of scalar nodal values
!
!# Introduction
!
! This subroutine performs interpolation of a scalar from its space-time nodal
! values.
!
! $$u=u^{a}_{I}N^{I}T_{a}$$
!
! The resultant represents the interpolation value of `val` at
! spatial-quadrature points

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_6(obj, Interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:)
    !! Interpolation value of `val` at spatial-quadrature points
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
  END SUBROUTINE elemsd_getInterpolation_6
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_6
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolations of scalar FEVariable
!
!# Introduction
!
! This subroutine performs interpolation of a scalar [[FEVariable_]]
! The FE Variable can be a
!
! - constant
! - spatial nodal values
! - spatial quadrature values
! - space-time nodal values
!
! $$u=u^{a}_{I}N^{I}T_{a}$$
!
! The resultant represents the interpolation value of `val` at
! spatial-quadrature points

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_7(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :)
    !! scalar value of scalar on space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FE variable
  END SUBROUTINE elemsd_getInterpolation_7
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_7
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of a vector
!
!# Introduction
!
! This subroutine performs interpolation of a vector from its space-time
! nodal values
!
! $$u_{i}=u^{a}_{iI}N^{I}T_{a}$$

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_8(obj, Interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:, :)
    !! Interpol(:,ips) => interpolation value at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE elemsd_getInterpolation_8
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_8
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_9(obj, Interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE elemsd_getInterpolation_9
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_9
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_10(obj, Interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: Interpol(:, :, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE elemsd_getInterpolation_10
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_10
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of vector FEVariable
!
!# Introduction
!
! Returns the interpolation of vector variable
! The vector variable can be+
!
! - constant
! - spatial nodal values
! - spatial quadrature values
! - space-time nodal values
!
! NOTE This routine calls [[Interpolation]] function from the same module.
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getInterpolation_11(obj, Interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: Interpol(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE elemsd_getInterpolation_11
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE elemsd_getInterpolation_11
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of a scalar

INTERFACE
  MODULE PURE FUNCTION interpol_scalar(obj, val) RESULT(Interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    REAL(DFP), ALLOCATABLE :: Interpol(:)
  END FUNCTION interpol_scalar
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE interpol_scalar
END INTERFACE Interpolation


!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of vector

INTERFACE
  MODULE PURE FUNCTION interpol_vector(obj, val) RESULT(Interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    REAL(DFP), ALLOCATABLE :: Interpol(:, :)
  END FUNCTION interpol_vector
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE interpol_vector
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of matrix

INTERFACE
  MODULE PURE FUNCTION interpol_matrix(obj, val) RESULT(Interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    REAL(DFP), ALLOCATABLE :: Interpol(:, :, :)
  END FUNCTION interpol_matrix
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE interpol_matrix
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                            STInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-23
! update: 2021-11-23
! summary: This function performs interpolations of scalar
!
!# Introduction
!
! This function performs interpolation of a scalar from its space-time nodal
! values.
!
! $$u=u^{a}_{I}N^{I}T_{a}$$

INTERFACE
  MODULE PURE FUNCTION stsd_interpol_scalar(obj, val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
    REAL(DFP), ALLOCATABLE :: Interpol(:)
    !! Interpolation value of `val` at integration points
  END FUNCTION stsd_interpol_scalar
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE stsd_interpol_scalar
END INTERFACE STInterpolation


!----------------------------------------------------------------------------
!                                            STInterpolation@InterpolMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of vector

!> authors: Dr. Vikas Sharma
!
! This function performs interpolation of a vector from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

  MODULE PURE FUNCTION stsd_interpol_vector(obj, val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! spatial nodal values of vector
    REAL(DFP), ALLOCATABLE :: Interpol(:, :)
    !! Interpolation value of `val` at integration points
  END FUNCTION stsd_interpol_vector
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE stsd_interpol_vector
END INTERFACE STInterpolation

!----------------------------------------------------------------------------
!                                            STInterpolation@InterpolMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of matrix

!> authors: Dr. Vikas Sharma
!
! This function performs interpolation of a matrix from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

  MODULE PURE FUNCTION stsd_interpol_matrix(obj, val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! spatial nodal values of matrix
    REAL(DFP), ALLOCATABLE :: Interpol(:, :, :)
    !! Interpolation value of `val` at integration points
  END FUNCTION stsd_interpol_matrix
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE stsd_interpol_matrix
END INTERFACE STInterpolation
