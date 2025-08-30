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

MODULE ElemshapeData_VectorInterpolMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: ElemShapeData_, STElemShapeData_, FEVariable_
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
! summary: This subroutine performs interpolation of a vector
!
!# Introduction
!
! This subroutine performs interpolation of a vector from its spatial
! nodal values
!
! $$u_{i}=u_{iI}N^{I}$$

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE vector_getInterpolation_1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation1_(obj,ans, val, &
                                                   nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation_2(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation2_(obj, ans, val, &
                                                   nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation_3(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT), ALLOCATABLE ::ans(:, :, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation3_(obj,ans, val, &
                                                   dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) ::ans(:, :, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation_4(obj,ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation4_(obj, ans, val, &
                                                   nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation_5(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
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
  MODULE PURE SUBROUTINE vector_getInterpolation5_(obj, ans, val, &
                                                   dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE vector_getInterpolation5_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of vector

INTERFACE
  MODULE PURE FUNCTION vector_interpolation_1(obj, val) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION vector_interpolation_1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE vector_interpolation_1
END INTERFACE Interpolation

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

  MODULE PURE FUNCTION vector_stinterpolation_1(obj, val) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! spatial nodal values of vector
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Interpolation value of vector
  END FUNCTION vector_stinterpolation_1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE vector_stinterpolation_1
END INTERFACE STInterpolation

END MODULE ElemshapeData_VectorInterpolMethods
