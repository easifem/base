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

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Unit normal from the gradient of a field
!
! Compute a unit vector at the quadrature points.
! It comes from the gradient of a scalar or vector field.
! An example is the normal of a level set.

MODULE ElemshapeData_UnitNormalMethods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetUnitNormal

!----------------------------------------------------------------------------
!                                                       GetUnitNormal@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Unit normal from the gradient of a scalar field
!
! Returns the unit vector along the gradient of a scalar field `p`.
! It interpolates `p` from the nodal values `val`.
! It reallocates `R(nsd, nips)`. Where `grad(p)` is zero, `R` is zero.
!
! Formula:
!   $$ R = \operatorname{sign}(p) \frac{\nabla p}{|\nabla p|} $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetUnitNormal_test_1.F90)
!   first block, level set `p = y - 1/2`
! - [test 2](|base|ElemshapeData/examples/GetUnitNormal_test_2.F90)
!   first and second blocks, constant `p` gives `R = 0`
!
! See also: [[GetUnitNormal_2]], [[GetUnitNormal_3]]

INTERFACE
  MODULE PURE SUBROUTINE GetUnitNormal_1(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    !! unit vector defined over quadrature points, in `xiJ` format
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal values of a scalar, val(nns)
  END SUBROUTINE GetUnitNormal_1
END INTERFACE

INTERFACE GetUnitNormal
  MODULE PROCEDURE GetUnitNormal_1
END INTERFACE GetUnitNormal

!----------------------------------------------------------------------------
!                                                       GetUnitNormal@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Unit normal from the gradient of a vector field
!
! Returns a unit vector from the gradient of a vector field `p`.
! It interpolates `p` from the nodal values `val`.
! It reallocates `R(nsd, nips)`.
!
! Where `|p|` is zero, it takes every component of `p / |p|` as one.
! Where `m` is zero, `R` is zero.
!
! Formula:
!   $$ m_j = \sum_k \frac{p_k}{|p|} \frac{\partial p_k}{\partial x_j},
!   \quad R = \frac{m}{|m|} $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetUnitNormal_test_1.F90)
!   second block, field `(x, 0)`
! - [test 2](|base|ElemshapeData/examples/GetUnitNormal_test_2.F90)
!   third to fifth blocks, fields `(x, y)`, `(x, y, 0)` and zero
!
! See also: [[GetUnitNormal_1]]

INTERFACE
  MODULE PURE SUBROUTINE GetUnitNormal_2(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    !! unit vector defined over quadrature points, in `xiJ` format
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of a vector, val(nsd, nns)
  END SUBROUTINE GetUnitNormal_2
END INTERFACE

INTERFACE GetUnitNormal
  MODULE PROCEDURE GetUnitNormal_2
END INTERFACE GetUnitNormal

!----------------------------------------------------------------------------
!                                                       GetUnitNormal@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Unit normal from a scalar or vector FEVariable
!
! Returns the unit vector from the nodal values in `val`.
! For a scalar `val` it works like [[GetUnitNormal_1]].
! For a vector `val` it works like [[GetUnitNormal_2]].
!
! @note
! `val` must hold nodal values. A matrix `val` is not supported, and
! then `R` does not change.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetUnitNormal_test_1.F90)
!   both blocks, with `NodalVariable`
! - [test 2](|base|ElemshapeData/examples/GetUnitNormal_test_2.F90)
!   third block (vector) and sixth block (matrix, `R` does not change)
!
! See also: [[GetUnitNormal_1]], [[GetUnitNormal_2]]

INTERFACE
  MODULE PURE SUBROUTINE GetUnitNormal_3(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    !! unit vector defined over quadrature points, in `xiJ` format
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar or vector nodal FEVariable
  END SUBROUTINE GetUnitNormal_3
END INTERFACE

INTERFACE GetUnitNormal
  MODULE PROCEDURE GetUnitNormal_3
END INTERFACE GetUnitNormal

END MODULE ElemshapeData_UnitNormalMethods
