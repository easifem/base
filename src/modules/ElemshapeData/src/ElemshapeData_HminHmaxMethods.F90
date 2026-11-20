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

module ElemshapeData_HminHmaxMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the minimum and maximum element length at each point
!
! Computes `hmin` and `hmax` at every space quadrature point of `obj`.
! They are the shortest and longest directional lengths of the element.
!
! For each quadrature point, the routine does the following.
!
! 1. Builds `G = inverse(J * TRANSPOSE(J))` from the Jacobian `J` of `obj`.
! 2. Finds the eigenvalues of `G`.
! 3. Sets `hmax = 2 / SQRT(MIN(eigenvalues))`.
! 4. Sets `hmin = 2 / SQRT(MAX(eigenvalues))`.
!
! The routine reallocates `hmax` and `hmin` to size `nips`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetHminHmax_test_1.F90)
!
! See also: [[elemsd_GetHminHmax2]], [[elemsd_GetHminHmax3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHminHmax1(obj, hmax, hmin)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! shape function data; it can be an instance of [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmax(:)
    !! maximum directional length, `SIZE(hmax) = nips`
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmin(:)
    !! minimum directional length, `SIZE(hmin) = nips`
  END SUBROUTINE elemsd_GetHminHmax1
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax1
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the minimum and maximum element length from given `G`
!
! Computes `hmin` and `hmax` at every space quadrature point of `obj`.
! The caller supplies the metric tensor `G` instead of the routine
! building it from the Jacobian.
!
! For each quadrature point, the routine does the following.
!
! 1. Finds the eigenvalues of `G(:, :, ips)`.
! 2. Sets `hmax = 2 / SQRT(MIN(eigenvalues))`.
! 3. Sets `hmin = 2 / SQRT(MAX(eigenvalues))`.
!
! The routine reallocates `hmax` and `hmin` to size `nips`.
!
! @note
! The routine reads `nips` from `obj`, so `G` must hold at least `nips`
! points.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetHminHmax_test_2.F90)
!
! See also: [[elemsd_GetHminHmax1]], [[elemsd_GetHminHmax6]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHminHmax2(obj, hmax, hmin, G)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! shape function data; it can be an instance of [[STElemshapeData_]]
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmax(:)
    !! maximum directional length, `SIZE(hmax) = nips`
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmin(:)
    !! minimum directional length, `SIZE(hmin) = nips`
    REAL(DFP), INTENT(IN) :: G(:, :, :)
    !! metric tensor at each point, `SHAPE(G) = [nsd, nsd, nips]`
  END SUBROUTINE elemsd_GetHminHmax2
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax2
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the minimum and maximum element length as `FEVariable_`
!
! Computes `hmin` and `hmax` at every space quadrature point of `obj`.
! The results are scalar `FEVariable_` objects defined on space
! quadrature points.
!
! The values are the same as those from [[elemsd_GetHminHmax1]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetHminHmax_test_1.F90)
!
! See also: [[elemsd_GetHminHmax1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHminHmax3(obj, hmax, hmin)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! shape function data; it can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: hmax
    !! maximum directional length, scalar, space, quadrature variable
    TYPE(FEVariable_), INTENT(INOUT) :: hmin
    !! minimum directional length, scalar, space, quadrature variable
  END SUBROUTINE elemsd_GetHminHmax3
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax3
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the minimum and maximum element length as `FEVariable_`
!  from given `G`
!
! Computes `hmin` and `hmax` at every space quadrature point of `obj`
! from the metric tensor `G`.
! The results are scalar `FEVariable_` objects defined on space
! quadrature points.
!
! The values are the same as those from [[elemsd_GetHminHmax2]].
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetHminHmax_test_2.F90)
!
! See also: [[elemsd_GetHminHmax2]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetHminHmax6(obj, hmax, hmin, G)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! shape function data; it can be an instance of [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: hmax
    !! maximum directional length, scalar, space, quadrature variable
    TYPE(FEVariable_), INTENT(INOUT) :: hmin
    !! minimum directional length, scalar, space, quadrature variable
    REAL(DFP), INTENT(IN) :: G(:, :, :)
    !! metric tensor at each point, `SHAPE(G) = [nsd, nsd, nips]`
  END SUBROUTINE elemsd_GetHminHmax6
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax6
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the minimum and maximum element length in space-time
!
! Computes `hmin` and `hmax` at every space-time quadrature point.
! The routine calls [[elemsd_GetHminHmax1]] for each time quadrature
! point `obj(ipt)` and stores the result in column `ipt`.
!
! The routine reallocates `hmax` and `hmin` to shape `[nips, nipt]`.
!
! Examples
!
! - [test 3](|base|ElemshapeData/examples/GetHminHmax_test_3.F90)
!
! See also: [[elemsd_GetHminHmax1]], [[elemsd_GetHminHmax5]]

INTERFACE
  MODULE SUBROUTINE elemsd_GetHminHmax4(obj, hmax, hmin)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape function data, `SIZE(obj) = nipt`;
    !! `obj(ipt)` is the data at time quadrature point `ipt`
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmax(:, :)
    !! maximum directional length, `SHAPE(hmax) = [nips, nipt]`
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmin(:, :)
    !! minimum directional length, `SHAPE(hmin) = [nips, nipt]`
  END SUBROUTINE elemsd_GetHminHmax4
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax4
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the minimum and maximum element length in space-time
!  from given `G`
!
! Computes `hmin` and `hmax` at every space-time quadrature point from
! the metric tensor `G`.
! The routine calls [[elemsd_GetHminHmax2]] for each time quadrature
! point `obj(ipt)`, using `G(:, :, :, ipt)`, and stores the result in
! column `ipt`.
!
! The routine reallocates `hmax` and `hmin` to shape `[nips, nipt]`.
!
! Examples
!
! - [test 4](|base|ElemshapeData/examples/GetHminHmax_test_4.F90)
!
! See also: [[elemsd_GetHminHmax2]], [[elemsd_GetHminHmax8]]

INTERFACE
  MODULE SUBROUTINE elemsd_GetHminHmax7(obj, hmax, hmin, G)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape function data, `SIZE(obj) = nipt`;
    !! `obj(ipt)` is the data at time quadrature point `ipt`
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmax(:, :)
    !! maximum directional length, `SHAPE(hmax) = [nips, nipt]`
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: hmin(:, :)
    !! minimum directional length, `SHAPE(hmin) = [nips, nipt]`
    REAL(DFP), INTENT(IN) :: G(:, :, :, :)
    !! metric tensor, `SHAPE(G) = [nsd, nsd, nips, nipt]`
  END SUBROUTINE elemsd_GetHminHmax7
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax7
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the minimum and maximum element length in space-time
!  as `FEVariable_`
!
! Computes `hmin` and `hmax` at every space-time quadrature point.
! The results are scalar `FEVariable_` objects defined on space-time
! quadrature points.
!
! The values are the same as those from [[elemsd_GetHminHmax4]].
!
! Examples
!
! - [test 3](|base|ElemshapeData/examples/GetHminHmax_test_3.F90)
!
! See also: [[elemsd_GetHminHmax4]]

INTERFACE
  MODULE SUBROUTINE elemsd_GetHminHmax5(obj, hmax, hmin)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape function data, `SIZE(obj) = nipt`;
    !! `obj(ipt)` is the data at time quadrature point `ipt`
    TYPE(FEVariable_), INTENT(INOUT) :: hmax, hmin
    !! maximum and minimum directional length, scalar, space-time,
    !! quadrature variables
  END SUBROUTINE elemsd_GetHminHmax5
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax5
END INTERFACE GetHminHmax

!----------------------------------------------------------------------------
!                                                   GetHminHmax@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the minimum and maximum element length in space-time
!  as `FEVariable_` from given `G`
!
! Computes `hmin` and `hmax` at every space-time quadrature point from
! the metric tensor `G`.
! The results are scalar `FEVariable_` objects defined on space-time
! quadrature points.
!
! The values are the same as those from [[elemsd_GetHminHmax7]].
!
! Examples
!
! - [test 4](|base|ElemshapeData/examples/GetHminHmax_test_4.F90)
!
! See also: [[elemsd_GetHminHmax7]]

INTERFACE
  MODULE SUBROUTINE elemsd_GetHminHmax8(obj, hmax, hmin, G)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape function data, `SIZE(obj) = nipt`;
    !! `obj(ipt)` is the data at time quadrature point `ipt`
    TYPE(FEVariable_), INTENT(INOUT) :: hmax, hmin
    !! maximum and minimum directional length, scalar, space-time,
    !! quadrature variables
    REAL(DFP), INTENT(IN) :: G(:, :, :, :)
    !! metric tensor, `SHAPE(G) = [nsd, nsd, nips, nipt]`
  END SUBROUTINE elemsd_GetHminHmax8
END INTERFACE

INTERFACE GetHminHmax
  MODULE PROCEDURE elemsd_GetHminHmax8
END INTERFACE GetHminHmax

end module ElemshapeData_HminHmaxMethods
