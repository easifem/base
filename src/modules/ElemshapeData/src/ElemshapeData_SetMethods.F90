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
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Sets the normal vector

INTERFACE SetNormal
  MODULE PURE SUBROUTINE elemsd_SetNormal(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE elemsd_SetNormal
END INTERFACE SetNormal

!----------------------------------------------------------------------------
!                                                     SetThickness@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         4 March
! summary: This subroutine Set the thickness field
!
!# Introduction
!
! This subroutine Set the `thickness` field
! Here `val` denotes the nodal value of thickeness
!
! $$d = d_{I} N^{I}$$

INTERFACE SetThickness
  MODULE PURE SUBROUTINE elemsd_SetThickness(obj, val, N)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    !! Nodal values of thickness
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function values at quadrature points
    !! number of rows in n should be same as size of val
    !! number of columns in N should be equal to nips in obj
  END SUBROUTINE elemsd_SetThickness
END INTERFACE SetThickness

!----------------------------------------------------------------------------
!                                                     SetThickness@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: This subroutine Set the thickness field
!
!# Introduction
!
! This subroutine Set the `thickness` field
! Here `val` denotes the space-time nodal value of thickeness
!
! $$d = d_{I}^{a} N^{I} T_{a}$$

INTERFACE SetThickness
  MODULE PURE SUBROUTINE stsd_SetThickness(obj, val, N, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! Space-time nodal values of thickness
    !! rows represent space
    !! columns represets time value
    !! colsize should be same as size of T
    !! row size should be same as the number of rows in N
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function at spatial quadrature
    REAL(DFP), INTENT(IN) :: T(:)
    !! Shape function at temporal quadrature
  END SUBROUTINE stsd_SetThickness
END INTERFACE SetThickness

!----------------------------------------------------------------------------
!                                              SetBarycentricCoord@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: This subroutine Set the Barycentric coordinates
!
!# Introduction
!
! This subroutine Set the barycentric coordinates
!
! $$x_i = x_{iI} N^{I}$$
!

INTERFACE SetBarycentricCoord
  MODULE PURE SUBROUTINE elemsd_SetBarycentricCoord(obj, val, N)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! Nodal coordinates in `xiJ` format
    !! colsize of N should be nns
    !! row size should be same as nsd
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! When element is not an isoparametric we can supply N.
    !! row size should be nns
    !! col size should be nips
  END SUBROUTINE elemsd_SetBarycentricCoord
END INTERFACE SetBarycentricCoord

!----------------------------------------------------------------------------
!                                              SetBarycentricCoord@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: This subroutine Set the Barycentric coordinates
!
!# Introduction
!
! This subroutine Set the barycentric coordinates by using
! space-time nodal coordinates
!
! $$x=x_{I}^{a} N^I T_a$$

INTERFACE SetBarycentricCoord
  MODULE PURE SUBROUTINE stsd_SetBarycentricCoord(obj, val, N, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time Nodal coordinates in `xiJ` format
    !!
    REAL(DFP), INTENT(IN) :: N(:, :), T(:)
    !! N and T are required to handle non isoparametric elements
  END SUBROUTINE stsd_SetBarycentricCoord
END INTERFACE SetBarycentricCoord

!----------------------------------------------------------------------------
!                                                            SetJs@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Set the determinent of jacobian

INTERFACE SetJs
  MODULE PURE SUBROUTINE elemsd_SetJs(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE elemsd_SetJs
END INTERFACE SetJs

!----------------------------------------------------------------------------
!                                                          SetdNdXt@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Set $\frac{d N}{d X_t}$ internally
!
!# Introduction
!
! This subroutine will internally Set `dNdXt`.
! It use the inverse of jacobian stored internally, so make sure jacobian is
! Set before calling this  subroutine.

INTERFACE SetdNdXt
  MODULE PURE SUBROUTINE elemsd_SetdNdXt(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE elemsd_SetdNdXt
END INTERFACE SetdNdXt

!----------------------------------------------------------------------------
!                                                      SetJacobian@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Set the jacobian
!
!# Introduction
!
! This subroutine Set the jacobian by using the nodal coordinates
!
! $$\frac{d x_i}{d \xi_j} = x_{iI}\frac{d N^I}{d \xi_j}$$

INTERFACE SetJacobian
  MODULE PURE SUBROUTINE elemsd_SetJacobian(obj, val, dNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal coordinates in `xiJ` format
    !! rowsize is equal to nsd
    !! colsize equal to nns
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! dim1 is equal to nns
    !! dim2 is equal to xidim
    !! dim3 is equal to nips
  END SUBROUTINE elemsd_SetJacobian
END INTERFACE SetJacobian

!----------------------------------------------------------------------------
!                                                      SetJacobian@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Set the jacobian using space-time nodal coords
!
!# Introduction
!
! This subroutine Set the jacobian by using space-time nodal coords, `dNdXi`
! `T` are used to handle non-isoparameteric elements.
!
! $$\frac{d x_i}{d \xi_j} = x_{iI}^{a}T_a\frac{d N^I}{d \xi_j}$$
!

INTERFACE SetJacobian
  MODULE PURE SUBROUTINE stsd_SetJacobian(obj, val, dNdXi, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! Space time nodal values of coordinates
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! Local derivative of shape function for geometry
    REAL(DFP), INTENT(IN) :: T(:)
    !! Shape function for time element
  END SUBROUTINE stsd_SetJacobian
END INTERFACE SetJacobian

!----------------------------------------------------------------------------
!                                                         SetdNTdt@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Set `dNTdt` by using the space-time nodal values
!
!# Introduction
!
! - This subroutine Set `dNTdt` by using space-time nodal values
! - It is important to note that `dNTdXt` should be allocated before calling
! - This subroutine uses following formula
!
! $$
! \frac{\partial N^{I\  }T_{a}}{\partial t} =N^{I}\frac{\partial T_{a}}
! {\partial \theta } J^{-1}_{t}-\frac{\partial N^{I}T_{a}}{\partial x_{k}}
! \hat{v}_{k}
! $$

INTERFACE SetdNTdt
  MODULE PURE SUBROUTINE stsd_SetdNTdt(obj, val)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! Space-time nodal values
    !! dim1 = nsd
    !! dim2 = nns
    !! dim3 = nnt
  END SUBROUTINE stsd_SetdNTdt
END INTERFACE SetdNTdt

!----------------------------------------------------------------------------
!                                                        SetdNTdXt@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Set `dNTdXt` by using internal data
!
!# Introduction
!
! * This subroutine Set `dNTdXt` by using internal data
! * This subroutine uses inverse of Jacobian, therefore, before calling
! * this subroutine make sure to Set jacobian
!
! $$\frac{\partial N^{I\  }T_{a}}{\partial x_{i\  }}
! =\frac{\partial N^{I}T_{a}}{\partial \xi_{j} } \frac{\partial \xi_{j} }
! {\partial x_{i}} $$

INTERFACE SetdNTdXt
  MODULE PURE SUBROUTINE stsd_SetdNTdXt(obj)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
  !! Space-time nodal values
  END SUBROUTINE stsd_SetdNTdXt
END INTERFACE SetdNTdXt

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Sets parameters defined on physical element
!
!# Introduction
!
!This subroutine sets parameters defined on physical element
!
!- `val` denotes nodal coordinates of element in `xiJ` format
!- This subroutine will call
!     - `SetJacobian`
!     - `SetJs`
!     - `SetdNdXt`
!     - `SetBarycentricCoord`
!- By using `N` and `dNdXi` we can handle non-isoparametric
! elements
!
!@note
! In case `obj` is instance of [[stelemshapedata_]] then `val` will denotes
! coordinates of spatial nodes at some time in [tn, tn+1]
!@endnote
!
! The number of cols in val should be same as the number of rows
! in N and size of first index of dNdXi.

INTERFACE Set
  MODULE PURE SUBROUTINE elemsd_Set1(obj, val, N, dNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! Spatial nodal coordinates
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function for geometry
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! Local derivative of shape functions for geometry
  END SUBROUTINE elemsd_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Set parameters defined on physical element
!
!# Introduction
!
! This routine performs following tasks
!
!- Set Jacobian for cellobj
!- Set Js for cellobj
!- Set dNdXt for cellobj
!- Set SetBarycentricCoord for cellobj
!
! Then it get connectivity of facet element by using refelem stored
! inside facetobj. This conectivity is necessary for getting
! the coordinates of facet element. Then, it performs following tasks
! for facetobj
!
!- SetJacobian
!- SetJs
!- SetBarycentricCoord
!- SetNormal
!
! It is important to note that `dNdXt` in facetobj cannot be computed
! as facet elements are n-1 dimensional manifold in n dimensional space.
! Therefore, we extend (copy from) dNdXt from cellobj to facetobj.
!
! We also make normal, Js, Ws by in **cellObj** by copying from **facetObj**
!
!@note
! Both facetObj and cellObj should be defined at same quadrature
! points. These quadrature points corresponds points in facetObj.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE elemsd_Set2(facetobj, cellobj, cellval, cellN, &
                                    celldNdXi, facetN, facetdNdXi, facetNptrs)
    CLASS(ElemshapeData_), INTENT(INOUT) :: facetobj
    CLASS(ElemshapeData_), INTENT(INOUT) :: cellobj
    REAL(DFP), INTENT(IN) :: cellval(:, :)
    !! Spatial nodal coordinates of cell
    REAL(DFP), INTENT(IN) :: cellN(:, :)
    !! shape function for cell
    REAL(DFP), INTENT(IN) :: facetN(:, :)
    !! Shape function for geometry
    REAL(DFP), INTENT(IN) :: celldNdXi(:, :, :)
    REAL(DFP), INTENT(IN) :: facetdNdXi(:, :, :)
    !! Local derivative of shape functions for geometry
    INTEGER(I4B), INTENT(IN) :: facetNptrs(:)
  END SUBROUTINE elemsd_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine Set parameters defined on physical element
!
!# Introduction
!
!TODO: Add documentation of elemsd_Set3

INTERFACE Set
  MODULE PURE SUBROUTINE elemsd_Set3( &
    & masterFacetobj, &
    & masterCellobj, &
    & masterCellval, &
    & masterCellN, &
    & masterCelldNdXi, &
    & masterFacetN, &
    & masterFacetdNdXi, &
    & masterFacetNptrs, &
    & slaveFacetobj, &
    & slaveCellobj, &
    & slaveCellval, &
    & slaveCellN, &
    & slaveCelldNdXi, &
    & slaveFacetN, &
    & slaveFacetdNdXi, &
    & slaveFacetNptrs)
    CLASS(ElemshapeData_), INTENT(INOUT) :: masterFacetobj
    CLASS(ElemshapeData_), INTENT(INOUT) :: masterCellobj
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
    INTEGER(I4B), INTENT(IN) :: masterFacetNptrs(:)
    !!
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
    INTEGER(I4B), INTENT(IN) :: slaveFacetNptrs(:)
  END SUBROUTINE elemsd_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine sets the parameters defined on physical element
!
!# Introduction
!
! This subroutine Set parameters defined on physical element
!
! * `val` denotes coordinates of the space-time element in `xiJa` format
! * The facility of supplying `N`, `T`, and `dNdXi` allows us to handle
! non-isoparametric element
! * This subroutine will call
!     - `SetJacobian` uses `dNdXi`
!     - `SetJs`
!     - `SetdNdXt`
!     - `SetBarycentricCoord` uses `N` and `T`
!     - `SetdNTdXt`
!     - `SetdNTdt`
!
!@note
! In case of [[stelemshapedata_]] `val` denotes nodal coordinate at
! some intermediate space-time slab
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE stelemsd_Set1(obj, val, N, T, dNdXi)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! Spatial nodal coordinates
    REAL(DFP), INTENT(IN) :: N(:, :)
    REAL(DFP), INTENT(IN) :: T(:)
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
  END SUBROUTINE stelemsd_Set1
END INTERFACE Set

END MODULE ElemshapeData_SetMethods
