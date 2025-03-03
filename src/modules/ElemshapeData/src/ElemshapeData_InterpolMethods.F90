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

MODULE ElemshapeData_InterpolMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: GetInterpolation
PUBLIC :: GetInterpolation_
PUBLIC :: Interpolation
PUBLIC :: STInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE scalar_getInterpolation_1(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:)
    !! Interpolation value of of scalar
    REAL(DFP), INTENT(IN) :: val(:)
    !! spatial nodal values of scalar
  END SUBROUTINE scalar_getInterpolation_1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE scalar_getInterpolation1_(obj, interpol, val, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    REAL(DFP), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE scalar_getInterpolation1_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE scalar_getInterpolation_2(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: interpol(:)
    !! Interpolation of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
  END SUBROUTINE scalar_getInterpolation_2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE scalar_getInterpolation2_(obj, interpol, val, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE scalar_getInterpolation2_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
! spatial-temporal quadrature points

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE scalar_getInterpolation_3(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: interpol(:, :)
    !! space-time Interpolation of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
  END SUBROUTINE scalar_getInterpolation_3
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE scalar_getInterpolation3_(obj, interpol, val, &
                                                   nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE scalar_getInterpolation3_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE scalar_getInterpolation_4(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:)
    !! interpolation of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Scalar FE variable
  END SUBROUTINE scalar_getInterpolation_4
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE scalar_getInterpolation4_(obj, interpol, val, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE scalar_getInterpolation4_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE scalar_getInterpolation_5(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :)
    !! space-time interpolation of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FE variable
  END SUBROUTINE scalar_getInterpolation_5
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE scalar_getInterpolation5_(obj, interpol, val, &
                                                   nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE scalar_getInterpolation5_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of a vector
!
!# Introduction
!
! This subroutine performs interpolation of a vector from its spatial
! nodal values
!
! $$u_{i}=u_{iI}N^{I}$$

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE vector_getInterpolation_1(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :)
    !! interpolation of vector
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE vector_getInterpolation_1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE vector_getInterpolation1_(obj, interpol, val, &
                                                   nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE vector_getInterpolation1_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of a vector
!
!# Introduction
!
! This subroutine performs interpolation of a vector from its space-time
! nodal values
!
! $$u_{i}=u^{a}_{iI}N^{I}T_{a}$$

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE vector_getInterpolation_2(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: interpol(:, :)
    !!
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE vector_getInterpolation_2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE vector_getInterpolation2_(obj, interpol, val, &
                                                   nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE vector_getInterpolation2_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of a vector
!
!# Introduction
!
! This subroutine performs interpolation of a vector from its space-time
! nodal values
!
! $$u_{i}=u^{a}_{iI}N^{I}T_{a}$$

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE vector_getInterpolation_3(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: interpol(:, :, :)
    !!
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE vector_getInterpolation_3
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE vector_getInterpolation3_(obj, interpol, val, &
                                                   dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: interpol(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE vector_getInterpolation3_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE vector_getInterpolation_4(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :)
    !! interpolation of vector
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector FEvariable
  END SUBROUTINE vector_getInterpolation_4
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE vector_getInterpolation4_(obj, interpol, val, &
                                                   nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE vector_getInterpolation4_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE vector_getInterpolation_5(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :, :)
    !! space-time interpolation of vector
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector FEvariable
  END SUBROUTINE vector_getInterpolation_5
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE vector_getInterpolation5_(obj, interpol, val, &
                                                   dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: interpol(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE vector_getInterpolation5_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix

INTERFACE
  MODULE PURE SUBROUTINE matrix_getInterpolation_1(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :, :)
    !! interpolation of matrix
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal value of matrix
  END SUBROUTINE matrix_getInterpolation_1
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE matrix_getInterpolation_1
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values

INTERFACE
  MODULE PURE SUBROUTINE matrix_getInterpolation_2(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: interpol(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE matrix_getInterpolation_2
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE matrix_getInterpolation_2
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values

INTERFACE
  MODULE PURE SUBROUTINE matrix_getInterpolation_3(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: interpol(:, :, :, :)
    !! space-time interpolation
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE matrix_getInterpolation_3
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE matrix_getInterpolation_3
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix FEVariable
!
INTERFACE
  MODULE PURE SUBROUTINE matrix_getInterpolation_4(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :, :)
    !! interpolation of matrix
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
  END SUBROUTINE matrix_getInterpolation_4
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE matrix_getInterpolation_4
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE matrix_getInterpolation_5(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :, :, :)
    !! space-time interpolation of matrix
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
  END SUBROUTINE matrix_getInterpolation_5
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE matrix_getInterpolation_5
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of a FEVariable
!
!# Introduction
!
! - Returns the interpolation of a [[fevariable_]]
! - The result is returned in interpol
! - interpol is a FEVariable
! - The rank of interpol is same as the rank of val
! - interpol is defined on Quadrature, that is, interpol is QuadratureVariable
!
! - The val can have following ranks; scalar, vector, matrix
! - the val can be defined on quadrature (do nothing) or nodal (interpol)
! - The `vartype` of val can be constant, space, time, spacetime
!
INTERFACE
  MODULE PURE SUBROUTINE master_getInterpolation_1(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: interpol
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE master_getInterpolation_1
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE master_getInterpolation_1
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of a FEVariable
!
!# Introduction
!
! - Returns the interpolation of a [[fevariable_]]
! - The result is returned in interpol
! - interpol is a FEVariable
! - The rank of interpol is same as the rank of val
! - interpol is defined on Quadrature, that is, interpol is QuadratureVariable
!
! - The val can have following ranks; scalar, vector, matrix
! - the val can be defined on quadrature (do nothing) or nodal (interpol)
! - The `vartype` of val can be constant, space, time, spacetime
!
INTERFACE
  MODULE PURE SUBROUTINE master_getInterpolation_2(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: interpol
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE master_getInterpolation_2
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE master_getInterpolation_2
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of a scalar

INTERFACE Interpolation
  MODULE PURE FUNCTION scalar_interpolation_1(obj, val) RESULT(interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    REAL(DFP), ALLOCATABLE :: interpol(:)
  END FUNCTION scalar_interpolation_1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of vector

INTERFACE
  MODULE PURE FUNCTION vector_interpolation_1(obj, val) RESULT(interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    REAL(DFP), ALLOCATABLE :: interpol(:, :)
  END FUNCTION vector_interpolation_1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE vector_interpolation_1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of matrix

INTERFACE
  MODULE PURE FUNCTION matrix_interpolation_1(obj, val) RESULT(interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    REAL(DFP), ALLOCATABLE :: interpol(:, :, :)
  END FUNCTION matrix_interpolation_1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE matrix_interpolation_1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                               Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-13
! update: 2021-12-13
! summary: Interpolation of FEVariable

INTERFACE
  MODULE PURE FUNCTION master_interpolation_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION master_interpolation_1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE master_interpolation_1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                            STInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
  MODULE PURE FUNCTION scalar_stinterpolation_1(obj, val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
    REAL(DFP), ALLOCATABLE :: interpol(:)
    !! Interpolation value of `val` at integration points
  END FUNCTION scalar_stinterpolation_1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE scalar_stinterpolation_1
END INTERFACE STInterpolation

!----------------------------------------------------------------------------
!                                            STInterpolation@InterpolMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of vector

!> author: Dr. Vikas Sharma
!
! This function performs interpolation of a vector from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

  MODULE PURE FUNCTION vector_stinterpolation_1(obj, val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! spatial nodal values of vector
    REAL(DFP), ALLOCATABLE :: interpol(:, :)
    !! Interpolation value of vector
  END FUNCTION vector_stinterpolation_1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE vector_stinterpolation_1
END INTERFACE STInterpolation

!----------------------------------------------------------------------------
!                                            STInterpolation@InterpolMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of matrix

!> author: Dr. Vikas Sharma
!
! This function performs interpolation of a matrix from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

  MODULE PURE FUNCTION matrix_stinterpolation_1(obj, val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! spatial nodal values of matrix
    REAL(DFP), ALLOCATABLE :: interpol(:, :, :)
    !! Interpolation value of matrix
  END FUNCTION matrix_stinterpolation_1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE matrix_stinterpolation_1
END INTERFACE STInterpolation

END MODULE ElemshapeData_InterpolMethods
