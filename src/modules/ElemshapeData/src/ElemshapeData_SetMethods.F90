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

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the physical part of ElemShapeData_ and STElemShapeData_
!
! These routines fill the data that depend on the physical element:
! the Jacobian, its determinant `js`, the derivatives `dNdXt`, the
! coordinates of quadrature points, the thickness, the unit normal of a
! facet, and the space-time derivatives `dNTdXt` and `dNTdt`. The
! reference data (`N`, `dNdXi`, `ws`) must be set first, for example by
! [[LagrangeElemShapeData]].

MODULE ElemshapeData_SetMethods
USE BaseType, ONLY: ElemshapeData_, STElemshapeData_, ElemshapeDataPointer_
USE GlobalData, ONLY: DFP, I4B, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: Set
PUBLIC :: SetBarycentricCoord
PUBLIC :: SetJacobian
PUBLIC :: SetJs
PUBLIC :: SetNormal
PUBLIC :: SetThickness
PUBLIC :: SetdNTdXt
PUBLIC :: SetdNTdt
PUBLIC :: SetdNdXt

!----------------------------------------------------------------------------
!                                                       SetNormal@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the unit normal of a facet at quadrature points
!
! Sets the unit normal `obj%normal(1:3, ips)` of a facet.
! It uses the Jacobian `J = obj%jacobian(:, :, ips)` and `js`.
!
! For a line in 2D, `xidim = 1` and the second vector is the unit vector
! `e_z`. Walk along the edge from its first node to its second node.
! Then the normal points to the right.
!
! @note
! Set `jacobian` and `js` first, see [[elemsd_SetJacobian]] and
! [[elemsd_SetJs]].
! @endnote
!
! @warning
! Use it only for facets, where `xidim < nsd`. For cells, the result has
! no meaning.
! @endwarning
!
! Formula:
!   $$ normal = \frac{J(:, 1) \times J(:, 2)}{js} $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetNormal_test_1.F90) bottom and right edges of a rectangle
!

INTERFACE SetNormal
  MODULE PURE SUBROUTINE elemsd_SetNormal(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
  END SUBROUTINE elemsd_SetNormal
END INTERFACE SetNormal

!----------------------------------------------------------------------------
!                                                     SetThickness@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the thickness at quadrature points
!
! Sets the thickness `obj%thickness(1:nips)` at the quadrature points.
! It interpolates the nodal thickness `val` with `N`.
!
! Formula:
!   $$ thickness = val \cdot N $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetThickness_test_1.F90) first block
!
! See also: [[stsd_SetThickness]]

INTERFACE SetThickness
  MODULE PURE SUBROUTINE elemsd_SetThickness(obj, val, N)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:)
    !! nodal thickness, val(nns)
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function values at quadrature points
    !! number of rows in n should be same as size of val
    !! number of columns in N should be equal to nips in obj
  END SUBROUTINE elemsd_SetThickness
END INTERFACE SetThickness

!----------------------------------------------------------------------------
!                                                     SetThickness@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the thickness at quadrature points from space-time nodal values
!
! Sets the thickness `obj%thickness` from space-time nodal values `val`.
! It interpolates `val` in time with `T`, then in space with `N`.
!
! Formula:
!   $$ thickness = (val \cdot T) \cdot N $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetThickness_test_1.F90) second block
!
! See also: [[elemsd_SetThickness]]

INTERFACE SetThickness
  MODULE PURE SUBROUTINE stsd_SetThickness(obj, val, N, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal thickness, val(nns, nnt)
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function at spatial quadrature
    REAL(DFP), INTENT(IN) :: T(:)
    !! Shape function at temporal quadrature
  END SUBROUTINE stsd_SetThickness
END INTERFACE SetThickness

!----------------------------------------------------------------------------
!                                              SetBarycentricCoord@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the physical coordinates of quadrature points
!
! Sets the coordinates `obj%coord(1:nsd, 1:nips)` of the quadrature
! points. It interpolates the nodal coordinates `val` with `N`.
!
! Formula:
!   $$ coord = val \cdot N $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetBarycentricCoord_test_1.F90) first block
!
! See also: [[stsd_SetBarycentricCoord]]

INTERFACE SetBarycentricCoord
  MODULE PURE SUBROUTINE elemsd_SetBarycentricCoord(obj, val, N)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal coordinates, val(nsd, nns)
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! When element is not an isoparametric we can supply N.
    !! row size should be nns
    !! col size should be nips
  END SUBROUTINE elemsd_SetBarycentricCoord
END INTERFACE SetBarycentricCoord

!----------------------------------------------------------------------------
!                                              SetBarycentricCoord@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the coordinates of quadrature points from space-time nodes
!
! Sets the coordinates `obj%coord` from space-time nodal coordinates
! `val`. It interpolates `val` in time with `T`, then in space with `N`.
!
! Formula:
!   $$ coord = (val \cdot T) \cdot N $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetBarycentricCoord_test_1.F90) second block
!
! See also: [[elemsd_SetBarycentricCoord]]

INTERFACE SetBarycentricCoord
  MODULE PURE SUBROUTINE stsd_SetBarycentricCoord(obj, val, N, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal coordinates, val(nsd, nns, nnt)
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions at quadrature points, N(nns, nips)
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time point, T(nnt)
  END SUBROUTINE stsd_SetBarycentricCoord
END INTERFACE SetBarycentricCoord

!----------------------------------------------------------------------------
!                                                            SetJs@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the Jacobian determinant js at quadrature points
!
! Sets `obj%js` from `obj%jacobian`, like [[elemsd_SetJs2]].
! Set the Jacobian first, see [[elemsd_SetJacobian]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetJs_test_1.F90) first and second blocks, cell and facet
!
! See also: [[elemsd_SetJs2]]

INTERFACE SetJs
  MODULE PURE SUBROUTINE elemsd_SetJs(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
  END SUBROUTINE elemsd_SetJs
END INTERFACE SetJs

!----------------------------------------------------------------------------
!                                                            SetJs@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Compute the Jacobian determinant js from the Jacobian
!
! Sets `js(1:nips)` from `jacobian(1:nsd, 1:xidim, 1:nips)`.
! The rule depends on the element:
!
! - For a cell, `nsd = xidim` and `js = det(J)`.
! - For a curve, `xidim = 1` and `js = |J(:, 1)|`.
! - For a surface in 3D, `xidim = 2` and `js = |J(:, 1) x J(:, 2)|`.
!
! For a point, `xidim = 0` and it does not set `js`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetJs_test_1.F90) third block, surface in 3D
!
! See also: [[elemsd_SetJs]]

INTERFACE SetJs
  MODULE PURE SUBROUTINE elemsd_SetJs2(jacobian, js, nsd, xidim, nips)
    REAL(DFP), INTENT(IN) :: jacobian(:, :, :)
    !! Jacobian, jacobian(nsd, xidim, nips)
    REAL(DFP), INTENT(INOUT) :: js(:)
    !! Jacobian determinant, js(nips)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension
    INTEGER(I4B), INTENT(IN) :: xidim
    !! dimension of the reference element
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points
  END SUBROUTINE elemsd_SetJs2
END INTERFACE SetJs

!----------------------------------------------------------------------------
!                                                          SetdNdXt@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the derivatives of shape functions with respect to x
!
! Sets `obj%dNdXt` from `obj%dNdXi` and `obj%jacobian`, like
! [[elemsd_SetdNdXt2]]. Set the Jacobian first.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetdNdXt_test_1.F90) first and second blocks, cell and facet
!
! See also: [[elemsd_SetdNdXt2]], [[elemsd_SetJacobian]]

INTERFACE SetdNdXt
  MODULE PURE SUBROUTINE elemsd_SetdNdXt(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
  END SUBROUTINE elemsd_SetdNdXt
END INTERFACE SetdNdXt

!----------------------------------------------------------------------------
!                                                          SetdNdXt@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Compute the derivatives of shape functions with respect to x
!
! Computes `dNdXt` from `dNdXi` and the Jacobian `J = jacobian(:, :, ips)`.
!
! If `nsd /= xidim`, the element is a facet. The inverse of the Jacobian
! does not exist, so it sets `dNdXt` to zero.
!
! Formula:
!   $$ dNdXt(:, :, ips) = dNdXi(:, :, ips) \, J^{-1}, \quad nsd = xidim $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetdNdXt_test_1.F90) third block
!
! See also: [[elemsd_SetdNdXt]]

INTERFACE SetdNdXt
  MODULE PURE SUBROUTINE elemsd_SetdNdXt2(dNdXt, dNdXi, jacobian, &
                                          xidim, nsd, nips, nns)
    REAL(DFP), INTENT(INOUT) :: dNdXt(:, :, :)
    !! derivatives of shape functions with respect to x, dNdXt(nns, nsd, nips)
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! derivatives of shape functions, dNdXi(nns, xidim, nips)
    REAL(DFP), INTENT(IN) :: jacobian(:, :, :)
    !! Jacobian, jacobian(nsd, xidim, nips)
    INTEGER(I4B), INTENT(IN) :: xidim
    !! dimension of the reference element
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes
  END SUBROUTINE elemsd_SetdNdXt2
END INTERFACE SetdNdXt

!----------------------------------------------------------------------------
!                                                      SetJacobian@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the Jacobian of the map from reference to physical element
!
! Sets `obj%jacobian` at each quadrature point.
! The Jacobian is `dx_i / dxi_k`, computed from the nodal coordinates
! `val`. It uses only the first `MIN(SIZE(val, 2), obj%nns)` nodes.
!
! Formula:
!   $$ jacobian(i, k, ips) = \sum_I val(i, I) \, dNdXi(I, k, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetJacobian_test_1.F90) first and second blocks, cell and facet
!
! See also: [[stsd_SetJacobian]]

INTERFACE SetJacobian
  MODULE PURE SUBROUTINE elemsd_SetJacobian(obj, val, dNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal coordinates, val(nsd, nns)
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! dim1 is equal to nns
    !! dim2 is equal to xidim
    !! dim3 is equal to nips
  END SUBROUTINE elemsd_SetJacobian
END INTERFACE SetJacobian

!----------------------------------------------------------------------------
!                                                      SetJacobian@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the Jacobian from space-time nodal coordinates
!
! Sets `obj%jacobian` from space-time nodal coordinates `val`.
! It interpolates `val` in time with `T` first.
! Then it works like [[elemsd_SetJacobian]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetJacobian_test_1.F90) third block
!
! See also: [[elemsd_SetJacobian]]

INTERFACE SetJacobian
  MODULE PURE SUBROUTINE stsd_SetJacobian(obj, val, dNdXi, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal coordinates, val(nsd, nns, nnt)
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! Local derivative of shape function for geometry
    REAL(DFP), INTENT(IN) :: T(:)
    !! Shape function for time element
  END SUBROUTINE stsd_SetJacobian
END INTERFACE SetJacobian

!----------------------------------------------------------------------------
!                                                         SetdNTdt@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the time derivative of space-time shape functions
!
! Sets `obj%dNTdt(I, a, ips)`, the time derivative of `N_I T_a` at a
! fixed physical point. `val` holds the space-time nodal coordinates.
!
! The mesh velocity `v` at the quadrature point comes from `val`.
! For a mesh that does not move, `v` is zero.
!
! @note
! Set `jt`, `dTdTheta` and `dNTdXt` first, see [[stsd_SetdNTdXt]].
! @endnote
!
! Formula:
!   $$ v = \sum_I \sum_a val(:, I, a) \frac{1}{jt}
!   \frac{dT_a}{d\theta} N_I $$
!   $$ dNTdt(I, a, ips) = \frac{N_I}{jt} \frac{dT_a}{d\theta}
!   - \sum_k dNTdXt(I, a, k, ips) \, v_k $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetdNTdt_test_1.F90) fixed mesh
!
! See also: [[stsd_SetdNTdXt]]

INTERFACE SetdNTdt
  MODULE PURE SUBROUTINE stsd_SetdNTdt(obj, val)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal coordinates, val(nsd, nns, nnt)
  END SUBROUTINE stsd_SetdNTdt
END INTERFACE SetdNTdt

!----------------------------------------------------------------------------
!                                                        SetdNTdXt@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the space derivatives of space-time shape functions
!
! Sets `obj%dNTdXt`, the space derivatives of `N_I T_a`.
! It reallocates `obj%dNTdXt(nns, nnt, nsd, nips)`.
! Set the Jacobian first.
!
! If `nsd /= xidim`, it only reallocates `dNTdXt` and leaves the values
! zero.
!
! Formula:
!   $$ dNTdXt(I, a, k, ips) = \frac{\partial N_I}{\partial x_k} T_a,
!   \quad \frac{\partial N}{\partial x} = dNdXi \, J^{-1} $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/SetdNTdXt_test_1.F90) line element in space and time
!
! See also: [[stsd_SetdNTdt]]

INTERFACE SetdNTdXt
  MODULE PURE SUBROUTINE stsd_SetdNTdXt(obj)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    !! space-time shape data at one time point
  END SUBROUTINE stsd_SetdNTdXt
END INTERFACE SetdNTdXt

!----------------------------------------------------------------------------
!                                                             Set@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the physical shape data of a cell
!
! Sets the physical shape data of a cell from its nodal coordinates
! `val`. It does these steps in order:
!
! 1. Sets the Jacobian, see [[elemsd_SetJacobian]].
! 2. Sets `js`, see [[elemsd_SetJs]].
! 3. Sets `dNdXt`, see [[elemsd_SetdNdXt]].
! 4. Sets the coordinates of quadrature points, see
!    [[elemsd_SetBarycentricCoord]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/Set_test_1.F90) first block
!
! See also: [[elemsd_Set2]], [[stelemsd_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE elemsd_Set1(obj, val, N, dNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal coordinates, val(nsd, nns)
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function for geometry
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! Local derivative of shape functions for geometry
  END SUBROUTINE elemsd_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the physical shape data of a cell and one of its facets
!
! Sets the physical shape data of a cell and of one of its facets.
!
! 1. Sets the cell data like [[elemsd_Set1]].
! 2. Sets the Jacobian, `js` and coordinates of the facet.
! 3. Sets the unit normal of the facet, see [[elemsd_SetNormal]].
! 4. Copies `normal`, `js` and `ws` of the facet to the cell.
!
! The last step lets integrals on the facet use the cell shape
! functions.
!
! @note
! `cellobj` must hold the cell shape data at the facet quadrature points.
! Then `cellobj%nips` equals `facetobj%nips`.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/Set_test_1.F90) second block
!
! See also: [[elemsd_Set1]], [[elemsd_Set3]]

INTERFACE Set
  MODULE PURE SUBROUTINE elemsd_Set2( &
    facetobj, cellobj, cellval, facetval, cellN, celldNdXi, facetN, &
    facetdNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: facetobj
    !! facet element shape data
    CLASS(ElemshapeData_), INTENT(INOUT) :: cellobj
    !! cell element shape data
    REAL(DFP), INTENT(IN) :: cellval(:, :)
    !! Spatial nodal coordinates of cell
    REAL(DFP), INTENT(IN) :: facetval(:, :)
    !! Spatial nodal coordinates of facet element
    REAL(DFP), INTENT(IN) :: cellN(:, :)
    !! shape function for cell
    REAL(DFP), INTENT(IN) :: facetN(:, :)
    !! Shape function for geometry
    REAL(DFP), INTENT(IN) :: celldNdXi(:, :, :)
    !! derivatives of cell shape functions at the facet points
    REAL(DFP), INTENT(IN) :: facetdNdXi(:, :, :)
    !! Local derivative of shape functions for geometry
  END SUBROUTINE elemsd_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the physical shape data of two cells that share a facet
!
! Sets the physical shape data of two cells that share a facet.
! It works like [[elemsd_Set2]] for the master cell and its facet.
! Then it does the same for the slave cell and its facet.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/Set_test_2.F90) master and slave cells
!
! See also: [[elemsd_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE elemsd_Set3( &
    masterFacetobj, masterCellobj, masterCellval, masterCellN, &
    masterCelldNdXi, masterFacetN, masterFacetdNdXi, masterFacetVal, &
    slaveFacetobj, slaveCellobj, slaveCellval, slaveCellN, slaveCelldNdXi, &
    slaveFacetN, slaveFacetdNdXi, slaveFacetVal)
    CLASS(ElemshapeData_), INTENT(INOUT) :: masterFacetobj
    !! master: nodal coordinates of the facet, facetval(nsd, nns of facet)
    CLASS(ElemshapeData_), INTENT(INOUT) :: masterCellobj
    !! master: nodal coordinates of the facet, facetval(nsd, nns of facet)
    REAL(DFP), INTENT(IN) :: masterCellval(:, :)
    !! Spatial nodal coordinates of master cell
    REAL(DFP), INTENT(IN) :: masterCellN(:, :)
    !! local shape function for geometry of master cell
    REAL(DFP), INTENT(IN) :: masterFacetN(:, :)
    !! Shape function for geometry of master facet element
    REAL(DFP), INTENT(IN) :: masterCelldNdXi(:, :, :)
    !! Local gradient of shape functions for geometry of master cell
    REAL(DFP), INTENT(IN) :: masterFacetdNdXi(:, :, :)
    !! Local gradient of shape functions for geometry of
    !! facet element of master cell
    REAL(DFP), INTENT(IN) :: masterFacetVal(:, :)
    !! master facet xij
    CLASS(ElemshapeData_), INTENT(INOUT) :: slaveFacetobj
    !! Shape function data for facet element of slave cell
    CLASS(ElemshapeData_), INTENT(INOUT) :: slaveCellobj
    !! Shape function data for cell element of slave cell
    REAL(DFP), INTENT(IN) :: slaveCellval(:, :)
    !! Spatial nodal coordinates of cell element of slave cell
    REAL(DFP), INTENT(IN) :: slaveCellN(:, :)
    !! Local shape function for geometry of cell element of slave
    REAL(DFP), INTENT(IN) :: slaveFacetN(:, :)
    !! Local shape function for geometry of facet element of slave
    REAL(DFP), INTENT(IN) :: slaveCelldNdXi(:, :, :)
    !! Local derivative of shape function for geometry of cell element
    !! of slave
    REAL(DFP), INTENT(IN) :: slaveFacetdNdXi(:, :, :)
    !! Local derivative of shape function for geometry of facet element
    !! of slave
    REAL(DFP), INTENT(IN) :: slaveFacetVal(:, :)
    !! slave facet xij
  END SUBROUTINE elemsd_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Set the physical shape data of a space-time element
!
! Sets the physical shape data of a space-time element from its
! space-time nodal coordinates `val`. It does these steps in order:
!
! 1. Sets the Jacobian, see [[stsd_SetJacobian]].
! 2. Sets `js`, see [[elemsd_SetJs]].
! 3. Sets `dNdXt`, see [[elemsd_SetdNdXt]].
! 4. Sets the coordinates, see [[stsd_SetBarycentricCoord]].
! 5. Sets `dNTdXt`, see [[stsd_SetdNTdXt]].
! 6. Sets `dNTdt`, see [[stsd_SetdNTdt]].
!
! @note
! Set `T`, `dTdTheta` and `jt` of `obj` first.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/Set_test_2.F90) second block, space-time
!
! See also: [[elemsd_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE stelemsd_Set1(obj, val, N, T, dNdXi)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal coordinates, val(nsd, nns, nnt)
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! shape functions at quadrature points, N(nns, nips)
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time point, T(nnt)
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! derivatives of shape functions, dNdXi(nns, xidim, nips)
  END SUBROUTINE stelemsd_Set1
END INTERFACE Set

END MODULE ElemshapeData_SetMethods
