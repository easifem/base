! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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
! date: 2022-01-28
! summary: Get methods of ElemShapeData_
!
! Get the unit normal stored in element shape data.
!
MODULE ElemshapeData_GetMethods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: STElemshapeData_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetNormal

!----------------------------------------------------------------------------
!                                                          GetNormal@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-28
! summary: Get the unit normal as an array
!
! Copies the unit normal of `obj` into `normal`.
! It reallocates `normal(nsd, nips)` and copies `obj%normal(1:nsd, 1:nips)`.
! If `nsd` is absent, it returns all 3 components.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetNormal_test_1.F90) first block
!
! See also: [[elemsd_getNormal_2]], [[elemsd_getNormal_3]]

INTERFACE GetNormal
  MODULE PURE SUBROUTINE elemsd_getNormal_1(obj, normal, nsd)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: normal(:, :)
    !! unit normal, normal(nsd, nips)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! number of components to return; default 3
  END SUBROUTINE elemsd_getNormal_1
END INTERFACE GetNormal

!----------------------------------------------------------------------------
!                                                          GetNormal@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-28
! summary: Get the unit normal as a FEVariable
!
! Returns the unit normal as a `FEVariable_`.
! The variable is a quadrature, vector, space variable.
! Its shape is `[nsd, nips]`. If `nsd` is absent, the shape is
! `[3, nips]`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetNormal_test_1.F90) second block
!
! See also: [[elemsd_getNormal_1]]

INTERFACE GetNormal
  MODULE PURE SUBROUTINE elemsd_getNormal_2(obj, normal, nsd)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: normal
    !! unit normal, quadrature vector space variable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! number of components to return; default 3
  END SUBROUTINE elemsd_getNormal_2
END INTERFACE GetNormal

!----------------------------------------------------------------------------
!                                                         GetNormal@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-28
! summary: Get the unit normal of space-time shape data
!
! Returns the unit normals of space-time shape data as a `FEVariable_`.
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
! The variable is a quadrature, vector, space-time variable.
! Its shape is `[nsd, nips, nipt]`, where `nips` is the largest
! `obj(ipt)%nips`.
!
! @note
! All `obj(ipt)` should have the same `nips`.
! If `obj(ipt)%nips` is smaller than the largest `nips`, the missing
! entries `(:, obj(ipt)%nips+1:nips, ipt)` are zero.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetNormal_test_1.F90) third block
!
! See also: [[elemsd_getNormal_1]]

INTERFACE GetNormal
  MODULE PURE SUBROUTINE elemsd_getNormal_3(obj, normal, nsd)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(INOUT) :: normal
    !! unit normal, quadrature vector space-time variable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    !! number of components to return; default 3
  END SUBROUTINE elemsd_getNormal_3
END INTERFACE GetNormal

END MODULE ElemshapeData_GetMethods
