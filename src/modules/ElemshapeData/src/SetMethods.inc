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

PUBLIC :: setThickness
PUBLIC :: setBarycentricCoord
PUBLIC :: setJs
PUBLIC :: setdNdXt
PUBLIC :: setJacobian
PUBLIC :: setdNTdt
PUBLIC :: setdNTdXt
PUBLIC :: set
PUBLIC :: setNormal

!----------------------------------------------------------------------------
!                                                     setThickness@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         4 March
! summary: This subroutine set the thickness field
!
!# Introduction
!
! This subroutine set the `thickness` field
! Here `val` denotes the nodal value of thickeness
!
! $$d = d_{I} N^{I}$$

INTERFACE
  MODULE PURE SUBROUTINE elemsd_setThickness(obj, val, N)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    !! Nodal values of thickness
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function values at quadrature points
  END SUBROUTINE elemsd_setThickness
END INTERFACE

INTERFACE setThickness
  MODULE PROCEDURE elemsd_setThickness
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setThickness@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: This subroutine set the thickness field
!
!# Introduction
!
! This subroutine set the `thickness` field
! Here `val` denotes the space-time nodal value of thickeness
!
! $$d = d_{I}^{a} N^{I} T_{a}$$

INTERFACE
  MODULE PURE SUBROUTINE stsd_setThickness(obj, val, N, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! Space-time nodal values of thickness
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function at spatial quadrature
    REAL(DFP), INTENT(IN) :: T(:)
    !! Shape function at temporal quadrature
  END SUBROUTINE stsd_setThickness
END INTERFACE

INTERFACE setThickness
  MODULE PROCEDURE stsd_setThickness
END INTERFACE

!----------------------------------------------------------------------------
!                                              setBarycentricCoord@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: This subroutine set the Barycentric coordinates
!
!# Introduction
!
! This subroutine set the barycentric coordinates
!
! $$x_i = x_{iI} N^{I}$$
!

INTERFACE
  MODULE PURE SUBROUTINE elemsd_setBarycentricCoord(obj, val, N)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! Nodal coordinates in `xiJ` format
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! When element is not an isoparametric we can supply N.
  END SUBROUTINE elemsd_setBarycentricCoord
END INTERFACE

INTERFACE setBarycentricCoord
  MODULE PROCEDURE elemsd_setBarycentricCoord
END INTERFACE setBarycentricCoord

!----------------------------------------------------------------------------
!                                              setBarycentricCoord@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: This subroutine set the Barycentric coordinates
!
!# Introduction
!
! This subroutine set the barycentric coordinates by using
! space-time nodal coordinates
!
! $$x=x_{I}^{a} N^I T_a$$

INTERFACE
  MODULE PURE SUBROUTINE stsd_setBarycentricCoord(obj, val, N, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time Nodal coordinates in `xiJ` format
    REAL(DFP), INTENT(IN) :: N(:, :), T(:)
    !! N and T are required to handle non isoparametric elements
  END SUBROUTINE stsd_setBarycentricCoord
END INTERFACE

INTERFACE setBarycentricCoord
  MODULE PROCEDURE stsd_setBarycentricCoord
END INTERFACE setBarycentricCoord

!----------------------------------------------------------------------------
!                                                            setJs@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set the determinent of jacobian

INTERFACE
  MODULE PURE SUBROUTINE elemsd_setJs(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE elemsd_setJs
END INTERFACE

INTERFACE setJs
  MODULE PROCEDURE elemsd_setJs
END INTERFACE setJs

!----------------------------------------------------------------------------
!                                                          setdNdXt@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set $\frac{d N}{d X_t}$ internally
!
!# Introduction
!
! This subroutine will internally set `dNdXt`.
! It use the inverse of jacobian stored internally, so make sure jacobian is
! set before calling this  subroutine.

INTERFACE
  MODULE PURE SUBROUTINE elemsd_setdNdXt(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE elemsd_setdNdXt
END INTERFACE

INTERFACE setdNdXt
  MODULE PROCEDURE elemsd_setdNdXt
END INTERFACE setdNdXt

!----------------------------------------------------------------------------
!                                                      setJacobian@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set the jacobian
!
!# Introduction
!
! This subroutine set the jacobian by using the nodal coordinates
!
! $$\frac{d x_i}{d \xi_j} = x_{iI}\frac{d N^I}{d \xi_j}$$

INTERFACE
  MODULE PURE SUBROUTINE elemsd_setJacobian(obj, val, dNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal coordinates in `xiJ` format
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
  END SUBROUTINE elemsd_setJacobian
END INTERFACE

INTERFACE setJacobian
  MODULE PROCEDURE elemsd_setJacobian
END INTERFACE setJacobian

!----------------------------------------------------------------------------
!                                                      setJacobian@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set the jacobian using space-time nodal coords
!
!# Introduction
!
! This subroutine set the jacobian by using space-time nodal coords, `dNdXi`
! `T` are used to handle non-isoparameteric elements.
!
! $$\frac{d x_i}{d \xi_j} = x_{iI}^{a}T_a\frac{d N^I}{d \xi_j}$$
!

INTERFACE
  MODULE PURE SUBROUTINE stsd_setJacobian(obj, val, dNdXi, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! Space time nodal values of coordinates
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! Local derivative of shape function for geometry
    REAL(DFP), INTENT(IN) :: T(:)
    !! Shape function for time element
  END SUBROUTINE stsd_setJacobian
END INTERFACE

INTERFACE setJacobian
  MODULE PROCEDURE stsd_setJacobian
END INTERFACE setJacobian

!----------------------------------------------------------------------------
!                                                         setdNTdt@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set `dNTdt` by using the space-time nodal values
!
!# Introduction
!
! - This subroutine set `dNTdt` by using space-time nodal values
! - It is important to note that `dNTdXt` should be allocated before calling
! - This subroutine uses following formula
!
! $$
! \frac{\partial N^{I\  }T_{a}}{\partial t} =N^{I}\frac{\partial T_{a}}
! {\partial \theta } J^{-1}_{t}-\frac{\partial N^{I}T_{a}}{\partial x_{k}}
! \hat{v}_{k}
! $$

INTERFACE
  MODULE PURE SUBROUTINE stsd_setdNTdt(obj, val)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! Space-time nodal values
  END SUBROUTINE stsd_setdNTdt
END INTERFACE

INTERFACE setdNTdt
  MODULE PROCEDURE stsd_setdNTdt
END INTERFACE setdNTdt

!----------------------------------------------------------------------------
!                                                        setdNTdXt@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set `dNTdXt` by using internal data
!
!# Introduction
!
! * This subroutine set `dNTdXt` by using internal data
! * This subroutine uses inverse of Jacobian, therefore, before calling
! * this subroutine make sure to set jacobian
!
! $$\frac{\partial N^{I\  }T_{a}}{\partial x_{i\  }}
! =\frac{\partial N^{I}T_{a}}{\partial \xi_{j} } \frac{\partial \xi_{j} }
! {\partial x_{i}} $$

INTERFACE
  MODULE PURE SUBROUTINE stsd_setdNTdXt(obj)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
  !! Space-time nodal values
  END SUBROUTINE stsd_setdNTdXt
END INTERFACE

INTERFACE setdNTdXt
  MODULE PROCEDURE stsd_setdNTdXt
END INTERFACE setdNTdXt

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set parameters defined on physical element
!
!# Introduction
!
! This subroutine set parameters defined on physical element
!
! * `val` denotes nodal coordinates of element in `xiJ` format
! * This subroutine will call
!     - `setJacobian`
!     - `setJs`
!     - `setdNdXt`
!     - `setBarycentricCoord`
! * The facility of `N` and `dNdXi` allow us to handle non-isoparametric
! elements
!
!@note
! In case `obj` is instance of [[stelemshapedata_]] then `val` will denotes
! coordinates of spatial nodes at some time in [tn, tn+1]
!@endnote

INTERFACE
  MODULE PURE SUBROUTINE elemsd_set1(obj, val, N, dNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! Spatial nodal coordinates
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function for geometry
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! Local derivative of shape functions for geometry
  END SUBROUTINE elemsd_set1
END INTERFACE

INTERFACE set
  MODULE PROCEDURE elemsd_set1
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set parameters defined on physical element
!
!# Introduction
!
! This subroutine set parameters defined on physical element
!
! * `val` denotes coordinates of the space-time element in `xiJa` format
! * The facility of supplying `N`, `T`, and `dNdXi` allows us to handle
! non-isoparametric element
! * This subroutine will call
!     - `setJacobian` uses `dNdXi`
!     - `setJs`
!     - `setdNdXt`
!     - `setBarycentricCoord` uses `N` and `T`
!     - `setdNTdXt`
!     - `setdNTdt`
!
!@note
! In case of [[stelemshapedata_]] `val` denotes nodal coordinate at
! some intermediate space-time slab
!@endnote

INTERFACE
  MODULE PURE SUBROUTINE stelemsd_set1(obj, val, N, T, dNdXi)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! Spatial nodal coordinates
    REAL(DFP), INTENT(IN) :: N(:, :)
    REAL(DFP), INTENT(IN) :: T(:)
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
  END SUBROUTINE stelemsd_set1
END INTERFACE

INTERFACE set
  MODULE PROCEDURE stelemsd_set1
END INTERFACE set

!----------------------------------------------------------------------------
!                                                       setNormal@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine sets the normal vector
!

INTERFACE
  MODULE PURE SUBROUTINE elemsd_setNormal(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE elemsd_setNormal
END INTERFACE

INTERFACE setNormal
  MODULE PROCEDURE elemsd_setNormal
END INTERFACE setNormal
