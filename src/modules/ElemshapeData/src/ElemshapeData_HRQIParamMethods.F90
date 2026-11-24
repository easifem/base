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

MODULE ElemshapeData_HRQIParamMethods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: STElemshapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetHRQIParam

!----------------------------------------------------------------------------
!                                                        GetHRQIParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the HRQI element length at each quadrature point
!
! Computes the element length `h` at each space quadrature point.
! The length follows the direction `r`, a unit vector that comes
! from `val`.
!
! The routine can also return the largest and smallest directional
! lengths, `hmax` and `hmin`, and the unit vector `r`.
! It returns only the optional outputs that you pass.
! It reallocates every output array to `nips` points, the number of
! space quadrature points of `obj`.
!
! `val` holds nodal values of a scalar or a vector.
! For a scalar `p`, `r` is `sign(p) grad(p) / |grad(p)|`.
! For a vector, `r` comes from the gradient of the vector.
! A matrix `val` gives no direction. Then `r` is zero and `h` is zero.
! Where the direction is zero, `r` is zero and `h` is zero.
!
! Here `J` is the Jacobian of the element at the quadrature point.
! `J` has shape `(nsd, xidim)` and `C = J^T J` has shape
! `(xidim, xidim)`. `G` is the pseudo-inverse of `J J^T`. For
! `xidim = nsd`, `G` equals the inverse of `J J^T`. For `xidim < nsd`,
! such as a line in 2D, `G` measures only the part of `r` along the
! element.
! The value `hmax` uses the largest eigenvalue of `C` in place of
! `1 / (r^T G r)`. The value `hmin` uses the smallest eigenvalue.
!
! Formula:
!   $$ h = \frac{2}{\sqrt{r^T G r}}, \quad G = J C^{-2} J^T,
!   \quad C = J^T J $$
!
! @note
! The factor 2 is the edge length of the biunit reference element
! `[-1, 1]^d`. The routine assumes that `obj` uses a biunit
! reference domain. For shape data built with `domainName = "UNIT"`,
! `h`, `hmax` and `hmin` are twice the physical lengths; multiply
! them by 0.5.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetHRQIParam_test_1.F90)
! - [test 5](|base|ElemshapeData/examples/GetHRQIParam_test_5.F90),
!   shape data with no quadrature points
! - [test 6](|base|ElemshapeData/examples/GetHRQIParam_test_6.F90),
!   line element in 2D
! - [test 7](|base|ElemshapeData/examples/GetHRQIParam_test_7.F90),
!   matrix `val`
!
! See also: [[elemsd_GetHRQIParam2]], [[elemsd_GetHRQIParam3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam1(obj, h, val, hmax, hmin, r, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! Shape function data. It can be an instance of [[STElemshapeData_]].
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:)
    !! Element length, size(h) = nips
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of a scalar or a vector. They define the direction.
    !! A matrix gives no direction: `r` is zero and `h` is zero.
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmax(:)
    !! Maximum directional length, size(hmax) = nips
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmin(:)
    !! Minimum directional length, size(hmin) = nips
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: r(:, :)
    !! Unit vector, shape(r) = (nsd, nips)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! Not used now. Kept for future use.
  END SUBROUTINE elemsd_GetHRQIParam1
END INTERFACE

INTERFACE GetHRQIParam
  MODULE PROCEDURE elemsd_GetHRQIParam1
END INTERFACE GetHRQIParam

!----------------------------------------------------------------------------
!                                                        GetHRQIParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the HRQI element length as `FEVariable_` objects
!
! Computes the same values as [[elemsd_GetHRQIParam1]].
! It returns them as `FEVariable_` objects instead of plain arrays.
!
! `h`, `hmax` and `hmin` are scalar variables defined on space
! quadrature points. `r` is a vector variable on the same points.
! It returns only the optional outputs that you pass.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetHRQIParam_test_2.F90)
!
! See also: [[elemsd_GetHRQIParam1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam2(obj, h, val, hmax, hmin, r, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! Shape function data. It can be an instance of [[STElemshapeData_]].
    TYPE(FEVariable_), INTENT(INOUT) :: h
    !! Element length, a scalar variable on quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of a scalar or a vector. They define the direction.
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax, hmin, r
    !! `hmax` and `hmin` are the maximum and minimum directional
    !! lengths, scalar variables on quadrature points.
    !! `r` is the unit vector, a vector variable on quadrature points.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! Not used now. Kept for future use.
  END SUBROUTINE elemsd_GetHRQIParam2
END INTERFACE

INTERFACE GetHRQIParam
  MODULE PROCEDURE elemsd_GetHRQIParam2
END INTERFACE GetHRQIParam

!----------------------------------------------------------------------------
!                                                        GetHRQIParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the HRQI element length at space-time quadrature points
!
! Computes the element length `h` at each space-time quadrature point.
! It applies [[elemsd_GetHRQIParam1]] to `obj(ipt)` for each time
! quadrature point `ipt`. The first index of each output is the space
! quadrature point. The last index is the time quadrature point.
!
! The routine can also return `hmax`, `hmin` and the unit vector `r`.
! It returns only the optional outputs that you pass.
! The routine reallocates all output arrays.
!
! Examples
!
! - [test 3](|base|ElemshapeData/examples/GetHRQIParam_test_3.F90)
!
! See also: [[elemsd_GetHRQIParam1]], [[elemsd_GetHRQIParam4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam3(obj, h, val, hmax, hmin, r, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! Space-time shape function data. `obj(ipt)` is the data at
    !! the `ipt`-th quadrature point in the time domain.
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: h(:, :)
    !! Element length, shape(h) = (nips, nipt)
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of a scalar or a vector. They define the direction.
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmax(:, :)
    !! Maximum directional length, shape(hmax) = (nips, nipt)
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: hmin(:, :)
    !! Minimum directional length, shape(hmin) = (nips, nipt)
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: r(:, :, :)
    !! Unit vector, shape(r) = (nsd, nips, nipt)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! Not used now. Kept for future use.
  END SUBROUTINE elemsd_GetHRQIParam3
END INTERFACE

INTERFACE GetHRQIParam
  MODULE PROCEDURE elemsd_GetHRQIParam3
END INTERFACE GetHRQIParam

!----------------------------------------------------------------------------
!                                                        GetHRQIParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the HRQI element length at space-time quadrature
! points as `FEVariable_` objects
!
! Computes the same values as [[elemsd_GetHRQIParam3]].
! It returns them as `FEVariable_` objects instead of plain arrays.
!
! `h`, `hmax` and `hmin` are scalar variables defined on space-time
! quadrature points. `r` is a vector variable on the same points.
! It returns only the optional outputs that you pass.
!
! Examples
!
! - [test 4](|base|ElemshapeData/examples/GetHRQIParam_test_4.F90)
!
! See also: [[elemsd_GetHRQIParam3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHRQIParam4(obj, h, val, hmax, hmin, r, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! Space-time shape function data. `obj(ipt)` is the data at
    !! the `ipt`-th quadrature point in the time domain.
    TYPE(FEVariable_), INTENT(INOUT) :: h
    !! Element length, a scalar variable on space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of a scalar or a vector. They define the direction.
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax, hmin, r
    !! `hmax` and `hmin` are the maximum and minimum directional
    !! lengths, scalar variables on space-time quadrature points.
    !! `r` is the unit vector, a vector variable on the same points.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! Not used now. Kept for future use.
  END SUBROUTINE elemsd_GetHRQIParam4
END INTERFACE

INTERFACE GetHRQIParam
  MODULE PROCEDURE elemsd_GetHRQIParam4
END INTERFACE GetHRQIParam

END MODULE ElemshapeData_HRQIParamMethods
