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

MODULE ElemshapeData_HRGNParamMethods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: STElemshapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: GetHRGNParam

!----------------------------------------------------------------------------
!                                                        GetHRGNParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the HRGN parameter `h` at the quadrature points
!
! This routine computes the HRGN parameter `h` at each quadrature point
! of one element. It returns `h` as a real vector.
!
! At each quadrature point, the routine:
!
! 1. computes a unit normal `r` from `val`, see [[GetUnitNormal_3]];
! 2. projects the shape function gradients on `r`;
! 3. sets `h = 2 / s`, where `s` is the sum of the absolute values
!    of the projections.
!
! Formula:
!   $$ h = 2 \left( \sum_I | \mathbf{r} \cdot \nabla N_I | \right)^{-1} $$
!
! If `s` is zero, `h` is the minimum element size `hmin` instead,
! see [[elemsd_GetHminHmax1]].
!
! @note
! `val` must hold nodal values. For other values, `r` is not valid.
! @endnote
!
! A `val` that is neither a scalar nor a vector, such as a matrix,
! gives no direction. Then `r` is zero and `h` is `hmin`.
!
! `obj` can be an [[ElemshapeData_]] or an [[STElemshapeData_]]. For
! an [[STElemshapeData_]], the sum also runs over the time nodes.
!
! The routine allocates `h` with one entry per quadrature point.
!
! @note
! `opt` is not used at present.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetHRGNParam_test_1.F90)
!   with an [[ElemshapeData_]]
! - [test 2](|base|ElemshapeData/examples/GetHRGNParam_test_2.F90)
!   with an [[STElemshapeData_]]
! - [test 3](|base|ElemshapeData/examples/GetHRGNParam_test_3.F90)
!   with a matrix `val`
!
! See also: [[elemsd_GetHRGNParam2]], [[elemsd_GetHRGNParam3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam1(obj, h, val, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! shape function data of the element; can be an
    !! [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:)
    !! HRGN parameter at the quadrature points,
    !! `SIZE(h) = SIZE(obj%N, 2)`
    TYPE(FEVariable_), INTENT(IN) :: val
    !! nodal scalar or vector variable; gives the unit normal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetHRGNParam1
END INTERFACE

INTERFACE GetHRGNParam
  MODULE PROCEDURE elemsd_GetHRGNParam1
END INTERFACE GetHRGNParam

!----------------------------------------------------------------------------
!                                                        GetHRGNParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the HRGN parameter `h` as a quadrature variable
!
! This routine does the same as [[elemsd_GetHRGNParam1]]. It returns
! `h` as a scalar [[FEVariable_]] defined on the quadrature points
! of space.
!
! @note
! `opt` is not used at present.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetHRGNParam_test_1.F90)
!   fourth block
!
! See also: [[elemsd_GetHRGNParam1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam2(obj, h, val, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! shape function data of the element; can be an
    !! [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: h
    !! HRGN parameter; a scalar quadrature variable in space
    TYPE(FEVariable_), INTENT(IN) :: val
    !! nodal scalar or vector variable; gives the unit normal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetHRGNParam2
END INTERFACE

INTERFACE GetHRGNParam
  MODULE PROCEDURE elemsd_GetHRGNParam2
END INTERFACE GetHRGNParam

!----------------------------------------------------------------------------
!                                                        GetHRGNParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the HRGN parameter `h` at space-time quadrature points
!
! This routine computes the HRGN parameter for a space-time element.
! It returns `h` as a real matrix.
!
! For each time quadrature point `ii`, it calls
! [[elemsd_GetHRGNParam1]] with `obj(ii)`. It stores the result in
! column `ii` of `h`.
!
! The routine allocates `h` with the shape
! `[SIZE(obj(1)%N, 2), SIZE(obj)]`.
! All `obj(ii)` must have the same number of space quadrature points.
! If `obj` is empty, `h` has the shape `[0, 0]`.
!
! @note
! `opt` is not used at present.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetHRGNParam_test_2.F90)
!   second block
! - [test 4](|base|ElemshapeData/examples/GetHRGNParam_test_4.F90)
!   empty `obj`
!
! See also: [[elemsd_GetHRGNParam1]], [[elemsd_GetHRGNParam4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam3(obj, h, val, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape function data; `obj(ii)` holds the data at the
    !! `ii`-th quadrature point in time
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:, :)
    !! HRGN parameter; `h(ips, ii)` is the value at the `ips`-th
    !! quadrature point in space and the `ii`-th in time
    TYPE(FEVariable_), INTENT(IN) :: val
    !! nodal scalar or vector variable; gives the unit normal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetHRGNParam3
END INTERFACE

INTERFACE GetHRGNParam
  MODULE PROCEDURE elemsd_GetHRGNParam3
END INTERFACE GetHRGNParam

!----------------------------------------------------------------------------
!                                                        GetHRGNParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the HRGN parameter `h` as a space-time quadrature variable
!
! This routine does the same as [[elemsd_GetHRGNParam3]]. It returns
! `h` as a scalar [[FEVariable_]] defined on the space-time quadrature
! points.
!
! @note
! `opt` is not used at present.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetHRGNParam_test_2.F90)
!   third block
!
! See also: [[elemsd_GetHRGNParam3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRGNParam4(obj, h, val, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape function data; `obj(ii)` holds the data at the
    !! `ii`-th quadrature point in time
    TYPE(FEVariable_), INTENT(INOUT) :: h
    !! HRGN parameter; a scalar quadrature variable in space-time
    TYPE(FEVariable_), INTENT(IN) :: val
    !! nodal scalar or vector variable; gives the unit normal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetHRGNParam4
END INTERFACE

INTERFACE GetHRGNParam
  MODULE PROCEDURE elemsd_GetHRGNParam4
END INTERFACE GetHRGNParam

END MODULE ElemshapeData_HRGNParamMethods
