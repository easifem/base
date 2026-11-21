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
! This file contains the interpolation methods interfaces

MODULE ElemshapeData_ScalarInterpolMethods
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
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
! date: 2021-03-04
! summary: Interpolation of scalar nodal values, with allocation
!
! Interpolates the scalar nodal values `val(nns)` at the quadrature
! points of `obj`. It reallocates `ans(nips)`.
!
! Formula:
!   $$ ans(ips) = \sum_I N(I, ips) \, val(I) $$
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! first block
!
! See also: [[GetInterpolation_1]], [[Interpolation1]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation1(obj, ans, val)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:)
    !! Interpolation value of of scalar
    REAL(DFP), INTENT(IN) :: val(:)
    !! spatial nodal values of scalar
  END SUBROUTINE GetInterpolation1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-03
! summary: Interpolation of scalar nodal values, without allocation
!
! Interpolates `val` like [[GetInterpolation1]].
! It writes into the existing `ans` and returns the used size in
! `tsize`.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! first block
!
! See also: [[GetInterpolation1]], [[GetInterpolation_1a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1(obj, ans, val, tsize)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values, val(nns)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE GetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-03
! summary: Interpolation of scalar nodal values with scale
!
! Interpolates `val` like [[GetInterpolation_1]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! It uses only the first `MIN(SIZE(val), obj%nns)` nodes.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! first block, with `scale = 2`
!
! See also: [[GetInterpolation_1]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1a(obj, ans, val, tsize, scale, &
                                             addContribution)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values, val(nns)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
  END SUBROUTINE GetInterpolation_1a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-01
! summary: Interpolation of scalar space-time nodal values at one time point
!
! Interpolates the space-time nodal values `val(nns, nnt)` at one time
! point. `obj` is the shape data at that time quadrature point.
! It reallocates `ans(nips)`.
!
! Formula:
!   $$ ans(ips) = \sum_a T(a) \sum_I N(I, ips) \, val(I, a) $$
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! second block
!
! See also: [[GetInterpolation_2]], [[STInterpolation1]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation2(obj, ans, val)
    CLASS(STElemShapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:)
    !! Interpolation of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
    !! val(I,a) where I is the node number and a is the time level
  END SUBROUTINE GetInterpolation2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar space-time nodal values, without allocation
!
! Interpolates `val` like [[GetInterpolation2]].
! It writes into the existing `ans` and returns the used size in
! `tsize`.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetInterpolation_test_6.F90)
! first block
!
! See also: [[GetInterpolation2]], [[GetInterpolation_2a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2(obj, ans, val, tsize)
    CLASS(STElemShapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values, val(nns, nnt)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE GetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar space-time nodal values with scale
!
! Interpolates `val` like [[GetInterpolation_2]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! It uses only the first `MIN(SIZE(val, 2), obj%nnt)` time nodes.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetInterpolation_test_6.F90)
! first block
!
! See also: [[GetInterpolation_2]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2a(obj, ans, val, tsize, scale, &
                                             addContribution)
    CLASS(STElemShapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values, val(nns, nnt)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
  END SUBROUTINE GetInterpolation_2a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-01
! summary: Interpolation of scalar space-time nodal values at all time points
!
! Interpolates the space-time nodal values `val(nns, nnt)` at all time
! points. Here `obj(ipt)` is the shape data at time quadrature point
! `ipt`. It reallocates `ans(nips, nipt)`.
!
! It fills column `ipt` like [[GetInterpolation2]] with `obj(ipt)`.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90) second block
!
! See also: [[GetInterpolation_3]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation3(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :)
    !! space-time Interpolation of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
  END SUBROUTINE GetInterpolation3
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-03
! summary: Interpolation of scalar space-time nodal values at all time
!   points, without allocation
!
! Interpolates `val` like [[GetInterpolation3]].
! It writes into the existing `ans` and returns the used size in
! `nrow` and `ncol`.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetInterpolation_test_6.F90)
! second block
!
! See also: [[GetInterpolation3]], [[GetInterpolation_3a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3(obj, ans, val, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values, val(nns, nnt)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE GetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar space-time nodal values at all time points
!   with scale
!
! Interpolates `val` like [[GetInterpolation_3]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetInterpolation_test_6.F90)
! second block
!
! See also: [[GetInterpolation_3]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3a(obj, ans, val, nrow, ncol, &
                                             scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values, val(nns, nnt)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
  END SUBROUTINE GetInterpolation_3a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Interpolation of scalar FEVariable, with allocation
!
! Interpolates the scalar `FEVariable_` `val` at the quadrature points
! of `obj`. It reallocates `ans(nips)`.
!
! - For a nodal `val`, it interpolates with `N`.
! - For a quadrature `val`, it copies the values.
! - For a constant `val`, it repeats the value at every point.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! third block
! - [test 12](|base|ElemshapeData/examples/GetInterpolation_test_12.F90)
!   first block, quadrature `val`
!
! See also: [[GetInterpolation_4]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation4(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:)
    !! interpolation of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Scalar FE variable
  END SUBROUTINE GetInterpolation4
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-03
! summary: Interpolation of scalar FEVariable, without allocation
!
! Interpolates `val` like [[GetInterpolation4]].
! It writes into the existing `ans` and returns the used size in
! `tsize`.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetInterpolation_test_6.F90)
! third block
!
! See also: [[GetInterpolation4]], [[GetInterpolation_4a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4(obj, ans, val, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FEVariable
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE GetInterpolation_4
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-03
! summary: Interpolation of scalar FEVariable with scale
!
! Interpolates `val` like [[GetInterpolation_4]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! A space-time `val` needs `obj` to be a `STElemShapeData_`.
! For a quadrature `val`, `timeIndx` selects the time point.
! The default `timeIndx` is 1.
!
! @note
! A time `val` is not supported. A space-time `val` with a plain
! `ElemShapeData_` is not supported either. In both cases, it writes
! nothing.
! @endnote
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetInterpolation_test_6.F90)
! third block
! - [test 12](|base|ElemshapeData/examples/GetInterpolation_test_12.F90)
!   first and second blocks, quadrature `val`
!
! See also: [[GetInterpolation_4]], [[GetInterpolation_4b]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4a(obj, ans, val, tsize, scale, &
                                             addContribution, timeIndx)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FEVariable
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeIndx
    !! time quadrature point index
  END SUBROUTINE GetInterpolation_4a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-03
! summary: Interpolation of scalar FEVariable at one quadrature point
!
! Interpolates `val` at the quadrature point `spaceIndx` and writes it
! in the scalar `ans`. For a space-time `val`, `timeIndx` selects the
! time point.
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! A space-time `val` needs `obj` to be a `STElemShapeData_`.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! third block, at `spaceIndx = 2`
! - [test 12](|base|ElemshapeData/examples/GetInterpolation_test_12.F90)
!   third block, constant and space-time `val`
!
! See also: [[GetInterpolation_4a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4b(obj, ans, val, scale, &
                                             addContribution, timeIndx, &
                                             spaceIndx)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(INOUT) :: ans
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FEVariable
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index
  END SUBROUTINE GetInterpolation_4b
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-01
! summary: Interpolation of scalar FEVariable at all time points
!
! Interpolates the scalar `FEVariable_` `val` at all time points.
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
! It reallocates `ans(nips, nipt)`.
!
! It fills column `ipt` like [[GetInterpolation_4a]] with
! `timeIndx = ipt`.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! third block
!
! See also: [[GetInterpolation_5]]

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation5(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! space-time interpolation of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FE variable
  END SUBROUTINE GetInterpolation5
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-03
! summary: Interpolation of scalar FEVariable at all time points, without
!   allocation
!
! Interpolates `val` like [[GetInterpolation5]].
! It writes into the existing `ans` and returns the used size in
! `nrow` and `ncol`.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetInterpolation_test_6.F90)
! fourth block
! - [test 12](|base|ElemshapeData/examples/GetInterpolation_test_12.F90)
!   second block, quadrature `val`
!
! See also: [[GetInterpolation5]], [[GetInterpolation_5a]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5(obj, ans, val, nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FEVariable
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE GetInterpolation_5
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of scalar FEVariable at all time points with scale
!
! Interpolates `val` like [[GetInterpolation_5]].
! It adds `scale` times the interpolated value to `ans`.
! If `addContribution` is false, it clears `ans` first.
!
! Examples
!
! - [test 6](|base|ElemshapeData/examples/GetInterpolation_test_6.F90)
! fourth block
!
! See also: [[GetInterpolation_5]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5a(obj, ans, val, nrow, ncol, &
                                             scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated value
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FEVariable
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
  END SUBROUTINE GetInterpolation_5a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                       Interpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Function form of the interpolation of scalar nodal values
!
! Returns the interpolation of the scalar nodal values `val` at the
! quadrature points of `obj`. The result is `ans(nips)`.
!
! Formula:
!   $$ ans(ips) = \sum_I N(I, ips) \, val(I) $$
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! first block
!
! See also: [[GetInterpolation1]]

INTERFACE Interpolation
  MODULE PURE FUNCTION Interpolation1(obj, val) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values, val(nns)
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! interpolated value
  END FUNCTION Interpolation1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                                     STInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-23
! summary: Function form of the interpolation of scalar space-time nodal
!   values
!
! Returns the interpolation of the space-time nodal values `val` at one
! time point. `obj` is the shape data at that time quadrature point.
! The result is `ans(nips)`.
!
! Formula:
!   $$ ans(ips) = \sum_a T(a) \sum_I N(I, ips) \, val(I, a) $$
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetInterpolation_test_2.F90)
! second block
!
! See also: [[GetInterpolation2]]

INTERFACE
  MODULE PURE FUNCTION STInterpolation1(obj, val) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! Interpolation value of `val` at integration points
  END FUNCTION STInterpolation1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE STInterpolation1
END INTERFACE STInterpolation

END MODULE ElemshapeData_ScalarInterpolMethods
