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
!                                                    GetInterpolation@Methods
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
  MODULE PURE SUBROUTINE GetInterpolation1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! interpolation of vector
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE GetInterpolation1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary: get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1(obj, ans, val, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE GetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1a(obj, ans, val, nrow, ncol, &
                                             scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_1a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
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
  MODULE PURE SUBROUTINE GetInterpolation2(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :)
    !!
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE GetInterpolation2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2(obj, ans, val, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE GetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2a(obj, ans, val, nrow, ncol, &
                                             scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_2a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
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
  MODULE PURE SUBROUTINE GetInterpolation3(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :, :)
    !!
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE GetInterpolation3
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3(obj, ans, val, dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE GetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3a(obj, ans, val, dim1, dim2, &
                                             dim3, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_3a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
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
  MODULE PURE SUBROUTINE GetInterpolation4(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! interpolation of vector
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector FEvariable
  END SUBROUTINE GetInterpolation4
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4(obj, ans, val, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE GetInterpolation_4
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4a(obj, ans, val, nrow, ncol, &
                                             scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_4a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
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
  MODULE PURE SUBROUTINE GetInterpolation5(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! space-time interpolation of vector
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector FEvariable
  END SUBROUTINE GetInterpolation5
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5(obj, ans, val, dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE GetInterpolation_5
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-30
! summary:  get interpolation of vector without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5a(obj, ans, val, dim1, dim2, &
                                             dim3, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_5a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of vector

INTERFACE
  MODULE PURE FUNCTION Interpolation1(obj, val) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Interpolation1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE Interpolation1
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

  MODULE PURE FUNCTION STInterpolation1(obj, val) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! spatial nodal values of vector
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Interpolation value of vector
  END FUNCTION STInterpolation1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE STInterpolation1
END INTERFACE STInterpolation

END MODULE ElemshapeData_VectorInterpolMethods
