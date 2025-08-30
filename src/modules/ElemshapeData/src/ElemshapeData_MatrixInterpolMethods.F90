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

MODULE ElemshapeData_MatrixInterpolMethods
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
! summary: This subroutine performs interpolation of matrix

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE matrix_getInterpolation_1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! interpolation of matrix
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal value of matrix
  END SUBROUTINE matrix_getInterpolation_1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE matrix_getInterpolation1_(obj, ans, val, &
                                                   dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matrix_getInterpolation1_
END INTERFACE GetInterpolation_

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

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE matrix_getInterpolation_2(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE matrix_getInterpolation_2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE matrix_getInterpolation2_(obj, ans, val, &
                                                   dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matrix_getInterpolation2_
END INTERFACE GetInterpolation_

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

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE matrix_getInterpolation_3(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :, :, :)
    !! space-time interpolation
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE matrix_getInterpolation_3
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix FEVariable
!
INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE matrix_getInterpolation_4(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! interpolation of matrix
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
  END SUBROUTINE matrix_getInterpolation_4
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE matrix_getInterpolation4_(obj, ans, val, &
                                                   dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE matrix_getInterpolation4_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE matrix_getInterpolation_5(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! space-time interpolation of matrix
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
  END SUBROUTINE matrix_getInterpolation_5
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE matrix_getInterpolation5_(obj, ans, val, &
                                                   dim1, dim2, dim3, dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE matrix_getInterpolation5_
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of matrix

INTERFACE
  MODULE PURE FUNCTION matrix_interpolation_1(obj, val) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION matrix_interpolation_1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE matrix_interpolation_1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                            STInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: This function performs interpolations of matrix
!
!!# Introduction
!
! This function performs interpolation of a matrix from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

INTERFACE

  MODULE PURE FUNCTION matrix_stinterpolation_1(obj, val) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! spatial nodal values of matrix
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! Interpolation value of matrix
  END FUNCTION matrix_stinterpolation_1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE matrix_stinterpolation_1
END INTERFACE STInterpolation

END MODULE ElemshapeData_MatrixInterpolMethods
