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
! date: 2021-03-04
! summary: Constructor methods of ElemShapeData_ and STElemShapeData_
!
! Allocate, copy and deallocate element shape data.
! Initiate space-time shape data from the shape data of a time element.

MODULE ElemshapeData_ConstructorMethods
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: STElemShapeData_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate
PUBLIC :: DEALLOCATE
PUBLIC :: ALLOCATE
PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                           Allocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Allocates the memory for various matrices in elemsd
!
! Reallocates the arrays of `obj` and sets its sizes.
! It sets `nsd`, `xidim`, `nns` and `nips`, and allocates:
!
! - `N(nns, nips)` and `NLaplacian(nns, nips)`
! - `dNdXi(nns, xidim, nips)` and `dNdXt(nns, nsd, nips)`
! - `jacobian(nsd, xidim, nips)`, `normal(3, nips)`, `coord(nsd, nips)`
! - `js(nips)`, `ws(nips)` and `thickness(nips)`
!
! All values are zero, except `thickness`, which is one.
!
! If `obj` is a `STElemShapeData_` and `nnt` is present, it also sets
! `nnt`. It then allocates `T(nnt)`, `dTdTheta(nnt)`,
! `dNTdt(nns, nnt, nips)` and `dNTdXt(nns, nnt, nsd, nips)`.
!
! You can also call it as `Initiate(obj, nsd, xidim, nns, nips, nnt)`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/ALLOCATE_test_1.F90)
! for `ElemShapeData_`, `STElemShapeData_` and `DEALLOCATE`
!
! See also: [[obj_Deallocate]]

INTERFACE ALLOCATE
  MODULE PURE SUBROUTINE obj_Allocate(obj, nsd, xidim, nns, nips, nnt)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! shape data to allocate
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension
    INTEGER(I4B), INTENT(IN) :: xidim
    !! dimension of the reference element
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in element
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of time nodes; used only for `STElemShapeData_`
  END SUBROUTINE obj_Allocate
END INTERFACE ALLOCATE

INTERFACE Initiate
  MODULE PROCEDURE obj_Allocate
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Initiate the element shape function data (not implemented)
!
! This routine should initiate `obj` from quadrature points and a
! reference element. The names of the continuity and interpolation
! types select the basis functions.
!
! @warning
! It is not implemented. It prints `"[WORK IN PROGRESS]"` and stops
! the program.
! @endwarning
!
! Use [[LagrangeElemShapeData]] instead.
! You can also use the `Initiate` that takes `TypeH1` and
! `TypeLagrangeInterpolation` arguments.
!
! See also: [[LagrangeElemShapeData]]

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate1(obj, quad, refelem, continuityType, &
                                  interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! shape data to initiate
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! quadrature points
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    CHARACTER(*), INTENT(IN) :: continuityType
    !! continuity of the basis functions
    CHARACTER(*), INTENT(IN) :: interpolType
    !! polynomial family of the basis functions
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-20
! summary: Copy data from an instance of elemshapedata to another instance
!
! Copies the shape data of `obj2` into `obj1`.
!
! 1. Allocates `obj1` with the sizes of `obj2`, see [[obj_Allocate]].
! 2. Copies `N`, `dNdXi`, `dNdXt`, `jacobian`, `js`, `ws`, `thickness`,
!    `coord` and `normal`.
! 3. If both are `STElemShapeData_`, it also copies `wt`, `jt`, `nnt`,
!    `T`, `dTdTheta`, `dNTdt` and `dNTdXt`.
!
! @note
! It allocates `NLaplacian` but does not copy it.
! @endnote
!
! You can also write it as `obj1 = obj2`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/Initiate_test_1.F90) copies with
! `Initiate` and with `=`
! - [test 2](|base|ElemshapeData/examples/Initiate_test_2.F90) copies a
! `STElemShapeData_`
!
! See also: [[obj_Allocate]]

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate2(obj1, obj2)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj1
    !! destination
    CLASS(ElemshapeData_), INTENT(IN) :: obj2
    !! source
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Initiate time shape function data in [[stelemshapedata_]]
!
! Sets the time shape data of `obj` from the time element `elemsd`.
! It reallocates `obj` with one `STElemShapeData_` per time quadrature
! point.
!
! For each time point `ip`, it sets:
!
! - `nnt` to `elemsd%nns`
! - `T` to `elemsd%N(:, ip)`
! - `dTdTheta` to `elemsd%dNdXi(:, 1, ip)`
! - `wt` to `elemsd%ws(ip)` and `jt` to `elemsd%js(ip)`
!
! @note
! It does not set the space part of `obj(ip)`.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/Initiate_test_1.F90) second block,
! from a `Line2` time element

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate3(obj, elemsd)
    TYPE(STElemshapeData_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! space-time shape data, one per time quadrature point
    TYPE(ElemshapeData_), INTENT(IN) :: elemsd
    !! shape data of the time element; `nips` gives the number of time
    !! points and `nns` the number of time nodes
  END SUBROUTINE obj_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Deallocates the data stored inside [[elemshapedata_]]
!
! Deallocates all arrays of `obj`.
! It sets `nsd`, `xidim`, `nips` and `nns` to zero.
!
! If `obj` is a `STElemShapeData_`, it also deallocates the time arrays.
! It then sets `nnt`, `wt` and `jt` to zero.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/ALLOCATE_test_1.F90) third block
!
! See also: [[obj_Allocate]]

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

END MODULE ElemshapeData_ConstructorMethods
