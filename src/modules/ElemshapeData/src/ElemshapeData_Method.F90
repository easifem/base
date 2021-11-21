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

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Methods related to [[elemShapeData_]] datatype

MODULE ElemshapeData_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                  AllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: Allocate the memory for various matrices in the object
!
!# Introduction
!
! This subroutine allocates the memory for various matrices in the object.
! This subroutine belongs to the generic interface called `AllocateData()`.
!
!@note
! This routine also belongs to generic routien called `initiate`
!@endnote
!
!### Usage
!
! See [[ElementshapeData_:elemsd_initiate]] for usage.

INTERFACE
  MODULE PURE SUBROUTINE elemsd_AllocateData(obj, nsd, xidim, nns, nips)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! object to be returned
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension
    INTEGER(I4B), INTENT(IN) :: xidim
    !! xidimension
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in element
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of integration points
  END SUBROUTINE elemsd_AllocateData
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE elemsd_AllocateData
END INTERFACE AllocateData

PUBLIC :: AllocateData

INTERFACE Allocate
  MODULE PROCEDURE elemsd_AllocateData
END INTERFACE Allocate

PUBLIC :: Allocate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data

INTERFACE
  MODULE SUBROUTINE elemsd_initiate(obj, quad, refElem, continuityType, &
    & interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CHARACTER(LEN=*), INTENT(IN) :: continuityType
    CHARACTER(LEN=*), INTENT(IN) :: interpolType
  END SUBROUTINE elemsd_initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE elemsd_initiate
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Initiate time shape function data in [[stelemshapedata_]]
!
!# Introduction

! - This subroutine initiates the shape-function data related to time
! domain in the instance of [[stelemshapedata_]].
! - User should provide an instance of [[Elemshapedata_]] elemsd,
! - The `elemsd`, actually contains the information of
! the shape-function in the time domain
! - The shape-function data in the time domain is
!   - $T$
!   - $\frac{dT}{d\theta}$
!   - ...
!@note
! This routine uses `elemsd` to  set `obj%T`, `obj%dTdTheta`, `obj%Jt`,
! `obj%Wt`, `obj%Theta`.
!@endnote
!
!
!### Usage
!
! - [ ] TODO Add usage for [[ElemshapeData_:stsd_initiate]]
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE stsd_initiate(obj, elemsd)
    TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    TYPE(ElemShapeData_), INTENT(IN) :: elemsd
    !! It has information about location shape function for time element
  END SUBROUTINE stsd_initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE stsd_initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: Deallocates the data stored inside [[elemshapedata_]]
!
!# Introduction
!
! This routine deallocates the data stored inside [[elemshapedata_]]. This
! routine belongs to `AllocateData()`
!

INTERFACE
  MODULE PURE SUBROUTINE elemsd_Deallocate(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE elemsd_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE elemsd_Deallocate
END INTERFACE Deallocate

PUBLIC :: Deallocate

!----------------------------------------------------------------------------
!                                             BaseInterpolation@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         30 Aug 2021
! summary: This routine returns a pointer to a child of [[BaseInterpolation_]]

INTERFACE
  MODULE FUNCTION elemsd_BaseInterpolation(childName) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: childName
    CLASS(BaseInterpolation_), POINTER :: ans
  END FUNCTION elemsd_BaseInterpolation
END INTERFACE

INTERFACE BaseInterpolation
  MODULE PROCEDURE elemsd_BaseInterpolation
END INTERFACE BaseInterpolation

PUBLIC :: BaseInterpolation

!----------------------------------------------------------------------------
!                                             BaseContinuity@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         30 Aug 2021
! summary: This routine returns a pointer to a child of [[BaseContinuity_]]

INTERFACE
  MODULE FUNCTION elemsd_BaseContinuity(childName) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: childName
    CLASS(BaseContinuity_), POINTER :: ans
  END FUNCTION elemsd_BaseContinuity
END INTERFACE

INTERFACE BaseContinuity
  MODULE PROCEDURE elemsd_BaseContinuity
END INTERFACE BaseContinuity

PUBLIC :: BaseContinuity

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         4 March 2021
! summary: Display the content of [[elemshapedata_]] and [[stelemshapedata_]]
!
!# Introduction
!         This subroutine displays the content of [[elemshapedata_]] and
! [[stelemshapedata_]] on screen. this routine belongs to `Display()`.

INTERFACE
  MODULE SUBROUTINE display_obj(obj, Msg, UnitNo)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: Msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
  END SUBROUTINE display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                        Initiate@H1Lagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1_Lagrange(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1_), INTENT(IN) :: continuityType
    CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1_Lagrange
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1_Lagrange
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                          Initiate@H1Hermit
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1_Hermit(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1_), INTENT(IN) :: continuityType
    CLASS(HermitInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1_Hermit
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1_Hermit
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@H1Serendipity
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1_Serendipity(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1_), INTENT(IN) :: continuityType
    CLASS(SerendipityInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1_Serendipity
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1_Serendipity
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@H1Hierarchy
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1_Hierarchy(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1_), INTENT(IN) :: continuityType
    CLASS(HierarchyInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1_Hierarchy
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1_Hierarchy
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@H1DivLagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Div_Lagrange(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Div_), INTENT(IN) :: continuityType
    CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Div_Lagrange
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Div_Lagrange
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@H1DivHermit
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Div_Hermit(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Div_), INTENT(IN) :: continuityType
    CLASS(HermitInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Div_Hermit
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Div_Hermit
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                  Initiate@H1DivSerendipity
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Div_Serendipity(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Div_), INTENT(IN) :: continuityType
    CLASS(SerendipityInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Div_Serendipity
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Div_Serendipity
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@H1DivHierarchy
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Div_Hierarchy(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Div_), INTENT(IN) :: continuityType
    CLASS(HierarchyInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Div_Hierarchy
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Div_Hierarchy
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                  Initiate@H1CurlLagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Curl_Lagrange(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Curl_), INTENT(IN) :: continuityType
    CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Curl_Lagrange
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Curl_Lagrange
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                     Initiate@H1CurlHermit
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Curl_Hermit(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Curl_), INTENT(IN) :: continuityType
    CLASS(HermitInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Curl_Hermit
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Curl_Hermit
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@H1CurlSerendipity
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Curl_Serendipity(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Curl_), INTENT(IN) :: continuityType
    CLASS(SerendipityInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Curl_Serendipity
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Curl_Serendipity
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@H1CurlHierarchy
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE H1Curl_Hierarchy(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(H1Curl_), INTENT(IN) :: continuityType
    CLASS(HierarchyInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE H1Curl_Hierarchy
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1Curl_Hierarchy
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                  Initiate@DGLagrange
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE DG_Lagrange(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(DG_), INTENT(IN) :: continuityType
    CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE DG_Lagrange
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE DG_Lagrange
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                     Initiate@DGHermit
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE DG_Hermit(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(DG_), INTENT(IN) :: continuityType
    CLASS(HermitInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE DG_Hermit
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE DG_Hermit
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@DGSerendipity
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE DG_Serendipity(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(DG_), INTENT(IN) :: continuityType
    CLASS(SerendipityInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE DG_Serendipity
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE DG_Serendipity
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                    Initiate@DGHierarchy
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the shape data
!
!# Introduction
!
! This routine initiates the shape function related data inside the element.
!

INTERFACE
  MODULE PURE SUBROUTINE DG_Hierarchy(obj, quad, refElem, &
    & continuityType, interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    CLASS(DG_), INTENT(IN) :: continuityType
    CLASS(HierarchyInterpolation_), INTENT(IN) :: interpolType
  END SUBROUTINE DG_Hierarchy
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE DG_Hierarchy
END INTERFACE Initiate

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
! Here `Val` denotes the nodal value of thickeness
!
! $$d = d_{I} N^{I}$$
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE set_thickness(obj, Val, N)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
    !! Nodal values of thickness
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function values at quadrature points
  END SUBROUTINE set_thickness
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
! Here `Val` denotes the space-time nodal value of thickeness
!
! $$d = d_{I}^{a} N^{I} T_{a}$$

INTERFACE
  MODULE PURE SUBROUTINE stsd_set_thickness(obj, Val, N, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! Space-time nodal values of thickness
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function at spatial quadrature
    REAL(DFP), INTENT(IN) :: T(:)
    !! Shape function at temporal quadrature
  END SUBROUTINE stsd_set_thickness
END INTERFACE

INTERFACE setThickness
  MODULE PROCEDURE set_thickness, stsd_set_thickness
END INTERFACE

PUBLIC :: setThickness

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
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE set_coord(obj, Val, N)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! Nodal coordinates in `xiJ` format
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! When element is not an isoparametric we can supply N.
  END SUBROUTINE set_coord
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
! This subroutine set the barycentric coordinates by using
! space-time nodal coordinates
!
! $$x=x_{I}^{a} N^I T_a$$

INTERFACE
  MODULE PURE SUBROUTINE stsd_set_coord(obj, Val, N, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! space-time Nodal coordinates in `xiJ` format
    REAL(DFP), INTENT(IN) :: N(:, :), T(:)
    !! N and T are required to handle non isoparametric elements
  END SUBROUTINE stsd_set_coord
END INTERFACE

INTERFACE setBarycentricCoord
  MODULE PROCEDURE set_coord, stsd_set_coord
END INTERFACE setBarycentricCoord

PUBLIC :: setBarycentricCoord

!----------------------------------------------------------------------------
!                                                            setJs@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine set the determinent of jacobian

INTERFACE
  MODULE PURE SUBROUTINE set_Js(obj)
    CLASS(ElemShapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE set_Js
END INTERFACE

INTERFACE setJs
  MODULE PROCEDURE set_Js
END INTERFACE setJs

PUBLIC setJs

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
  MODULE PURE SUBROUTINE set_dNdXt_internally(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE set_dNdXt_internally
END INTERFACE

INTERFACE setdNdXt
  MODULE PROCEDURE set_dNdXt_internally
END INTERFACE setdNdXt

PUBLIC :: setdNdXt

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
  MODULE PURE SUBROUTINE set_jacobian(obj, Val, dNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! nodal coordinates in `xiJ` format
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
  END SUBROUTINE set_jacobian
END INTERFACE

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
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE stsd_set_jacobian(obj, Val, dNdXi, T)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! Space time nodal values of coordinates
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! Local derivative of shape function for geometry
    REAL(DFP), INTENT(IN) :: T(:)
    !! Shape function for time element
  END SUBROUTINE stsd_set_jacobian
END INTERFACE

INTERFACE setJacobian
  MODULE PROCEDURE set_jacobian, stsd_set_jacobian
END INTERFACE setJacobian

PUBLIC :: setJacobian

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
! $$\frac{\partial N^{I\  }T_{a}}{\partial t} =N^{I}\frac{\partial T_{a}}
! {\partial \theta } J^{-1}_{t}-\frac{\partial N^{I}T_{a}}{\partial x_{k}}
! \hat{v}_{k} $$

INTERFACE
  MODULE PURE SUBROUTINE stsd_set_dNTdt(obj, Val)
    CLASS(STElemShapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! Space-time nodal values
  END SUBROUTINE stsd_set_dNTdt
END INTERFACE

INTERFACE setdNTdt
  MODULE PROCEDURE stsd_set_dNTdt
END INTERFACE setdNTdt

PUBLIC :: setdNTdt

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
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE stsd_set_dNTdXt_internally(obj)
    CLASS(STElemShapeData_), INTENT(INOUT) :: obj
  !! Space-time nodal values
  END SUBROUTINE stsd_set_dNTdXt_internally
END INTERFACE

INTERFACE setdNTdXt
  MODULE PROCEDURE stsd_set_dNTdXt_internally
END INTERFACE setdNTdXt

PUBLIC :: setdNTdXt

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
! * `Val` denotes nodal coordinates of element in `xiJ` format
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
  MODULE PURE SUBROUTINE elemsd_set1(obj, Val, N, dNdXi)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! Spatial nodal coordinates
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! Shape function for geometry
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
    !! Local derivative of shape functions for geometry
  END SUBROUTINE elemsd_set1
END INTERFACE

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
! * `Val` denotes coordinates of the space-time element in `xiJa` format
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
  MODULE PURE SUBROUTINE stelemsd_set1(obj, Val, N, T, dNdXi)
    CLASS(STElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! Spatial nodal coordinates
    REAL(DFP), INTENT(IN) :: N(:, :)
    REAL(DFP), INTENT(IN) :: T(:)
    REAL(DFP), INTENT(IN) :: dNdXi(:, :, :)
  END SUBROUTINE stelemsd_set1
END INTERFACE

INTERFACE set
  MODULE PROCEDURE elemsd_set1, stelemsd_set1
END INTERFACE set

PUBLIC :: set

!----------------------------------------------------------------------------
!                                                       setNormal@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine sets the normal vector
!
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE set_normal(obj)
    CLASS(ElemShapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE set_normal
END INTERFACE

INTERFACE setNormal
  MODULE PROCEDURE set_normal
END INTERFACE setNormal

PUBLIC :: setNormal

!----------------------------------------------------------------------------
!                                                 getInterpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolations
!
!# Introduction
!
! This subroutine performs interpolation of a scalar from its spatial nodal
! values.
!
! $$u=u_{I}N^{I}$$
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE get_interpol_scalar(obj, Interpol, Val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:)
    !! Interpolation value of `val` at integration points
    REAL(DFP), INTENT(IN) :: Val(:)
    !! spatial nodal values of scalar
  END SUBROUTINE get_interpol_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                getInterpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: T
INTERFACE
  MODULE PURE SUBROUTINE get_interpol_fevar_scalar(obj, Interpol, Val)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: Val
    REAL(DFP), INTENT(INOUT) :: Interpol(:)
  END SUBROUTINE get_interpol_fevar_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getInterpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of a vector
!
!# Introduction
!
! This subroutine performs interpolation of a vector from its spatial
! nodal values
!
! $$u_{i}=u_{iI}N^{I}$$

INTERFACE
  MODULE PURE SUBROUTINE get_interpol_vector(obj, Interpol, Val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:, :)
    !! Interpol(:,ips) => interpolation value at integration points
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE get_interpol_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getInterpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
! This subroutine performs interpolation of matrix
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE SUBROUTINE get_interpol_matrix(obj, Interpol, Val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:, :, :)
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! nodal value of matrix
  END SUBROUTINE get_interpol_matrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getInterpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix
!
! This subroutine performs interpolation of matrix

INTERFACE
  MODULE PURE SUBROUTINE get_interpol_fevar_matrix(obj, Interpol, Val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: Interpol(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: Val
  END SUBROUTINE get_interpol_fevar_matrix
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE get_interpol_scalar, get_interpol_vector, &
    & get_interpol_matrix, get_interpol_fevar_scalar, &
    & get_interpol_fevar_matrix
END INTERFACE getInterpolation

PUBLIC :: getInterpolation

!----------------------------------------------------------------------------
!                                                    Interpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of a scalar
!
!# Introduction
!
! This function returns interpolation of scalar

INTERFACE
  MODULE PURE FUNCTION interpol_scalar(obj, Val) RESULT(Interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
    REAL(DFP), ALLOCATABLE :: Interpol(:)
  END FUNCTION interpol_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Interpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of vector
!
!# Introduction
!
! This function returns the interpolation of vectors
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE FUNCTION interpol_vector(obj, Val) RESULT(Interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    REAL(DFP), ALLOCATABLE :: Interpol(:, :)
  END FUNCTION interpol_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Interpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of matrix
!
!# Introduction
!
! This function returns the interpolation of matrix
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE FUNCTION interpol_matrix(obj, Val) RESULT(Interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    REAL(DFP), ALLOCATABLE :: Interpol(:, :, :)
  END FUNCTION interpol_matrix
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE interpol_scalar, interpol_vector, interpol_matrix
END INTERFACE Interpolation

PUBLIC :: Interpolation

!----------------------------------------------------------------------------
!                                               getSTInterpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolations of scalar
!
!# Introduction
!
! This subroutine performs interpolation of a scalar from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

INTERFACE
  MODULE PURE SUBROUTINE stsd_get_interpol_scalar(obj, Interpol, Val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:)
    !! Interpolation value of `val` at integration points
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! spatial nodal values of scalar
  END SUBROUTINE stsd_get_interpol_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE stsd_get_interpol_fevar_scalar(obj, Interpol, Val)
    CLASS(STElemShapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(IN) :: Val
    REAL(DFP), INTENT(INOUT) :: Interpol(:, :)
  END SUBROUTINE stsd_get_interpol_fevar_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSTInterpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of a vector
!
!# Introduction
!
! This subroutine performs interpolation of a vector from its space-time
! nodal values
!
! $$u_{i}=u^{a}_{iI}N^{I}T_{a}$$

INTERFACE
  MODULE PURE SUBROUTINE stsd_get_interpol_vector(obj, Interpol, Val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:, :)
    !! Interpol(:,ips) => interpolation value at integration points
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE stsd_get_interpol_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSTInterpolation@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values

INTERFACE
  MODULE PURE SUBROUTINE stsd_get_interpol_matrix(obj, Interpol, Val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: Interpol(:, :, :)
    REAL(DFP), INTENT(IN) :: Val(:, :, :, :)
    !! nodal value of matrix
  END SUBROUTINE stsd_get_interpol_matrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE stsd_get_interpol_fevar_matrix(obj, Interpol, Val)
    CLASS(STElemShapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(IN) :: Val
    REAL(DFP), INTENT(INOUT) :: Interpol(:, :, :, :)
  END SUBROUTINE stsd_get_interpol_fevar_matrix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE getInterpolation
  MODULE PROCEDURE stsd_get_interpol_scalar, stsd_get_interpol_vector, &
    & stsd_get_interpol_matrix, stsd_get_interpol_fevar_scalar, &
    & stsd_get_interpol_fevar_matrix
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                                 STInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of scalar

!> authors: Dr. Vikas Sharma
!
! This function performs interpolation of a scalar from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$
  MODULE PURE FUNCTION stsd_interpol_scalar(obj, Val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! space-time nodal values of scalar
    REAL(DFP), ALLOCATABLE :: Interpol(:)
    !! Interpolation value of `val` at integration points
  END FUNCTION stsd_interpol_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                 STInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of vector

!> authors: Dr. Vikas Sharma
!
! This function performs interpolation of a vector from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

  MODULE PURE FUNCTION stsd_interpol_vector(obj, Val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! spatial nodal values of vector
    REAL(DFP), ALLOCATABLE :: Interpol(:, :)
    !! Interpolation value of `val` at integration points
  END FUNCTION stsd_interpol_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                 STInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of matrix

!> authors: Dr. Vikas Sharma
!
! This function performs interpolation of a matrix from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

  MODULE PURE FUNCTION stsd_interpol_matrix(obj, Val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:, :, :, :)
    !! spatial nodal values of matrix
    REAL(DFP), ALLOCATABLE :: Interpol(:, :, :)
    !! Interpolation value of `val` at integration points
  END FUNCTION stsd_interpol_matrix
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE stsd_interpol_scalar, stsd_interpol_vector, &
    & stsd_interpol_matrix
END INTERFACE STInterpolation

!----------------------------------------------------------------------------
!                                                 getLocalGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the local gradient of a scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the local gradient of a scalar from space
! nodal values
!
! $$\frac{\partial \phi }{\partial \xi_{i} } =\phi_{I} \frac{\partial N^{I}}
! {\partial \xi_{i} }$$

  MODULE PURE SUBROUTINE getLocalGradient_scalar(obj, dPhidXi, Val)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXi(:, :)
    REAL(DFP), INTENT(IN) :: Val(:)
    !! Space nodal values of scalar
  END SUBROUTINE getLocalGradient_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getLocalGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the local gradient of a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the local gradient of a vector
  MODULE PURE SUBROUTINE getLocalGradient_vector(obj, dVdXi, Val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXi(:, :, :)
    !! local gradient at integration points
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE getLocalGradient_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getLocalGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the local gradient of a scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the local gradient of a scalar from space
! time nodal values
!
! $$\frac{\partial \phi }{\partial \xi_{i} } =\phi^{a}_{I} T_{a}\frac
!{\partial N^{I}}{\partial \xi_{i} }$$

  MODULE PURE SUBROUTINE stsd_getLocalGradient_scalar(obj, dPhidXi, Val)
    CLASS(STElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXi(:, :)
    !! local gradient of scalar
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! space-time nodal values
  END SUBROUTINE stsd_getLocalGradient_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getLocalGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the local gradient of a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the local gradient of a vector using space-time
! nodal coordinates
  MODULE PURE SUBROUTINE stsd_getLocalGradient_vector(obj, dVdXi, Val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXi(:, :, :)
    !! local gradient at integration points
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE stsd_getLocalGradient_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------
INTERFACE getLocalGradient
  MODULE PROCEDURE getLocalGradient_scalar, getLocalGradient_vector, &
    & stsd_getLocalGradient_scalar, stsd_getLocalGradient_vector
END INTERFACE getLocalGradient

! PUBLIC :: getLocalGradient/

!----------------------------------------------------------------------------
!                                               getSpatialGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the spatial gradient of vector

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the spatial gradient of a vector
  MODULE PURE SUBROUTINE getSpatialGradient_vector(obj, dVdXt, Val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXt(:, :, :)
    !! spatial gradient of `val` at integration points
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE getSpatialGradient_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSpatialGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the spatial gradient of scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the spatial gradient of scalar
  MODULE PURE SUBROUTINE getSpatialGradient_scalar(obj, dPhidXt, Val)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXt(:, :)
    !! Spatial gradient of scalar
    REAL(DFP), INTENT(IN) :: Val(:)
    !! Nodal values of scalar
  END SUBROUTINE getSpatialGradient_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSpatialGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the spatial gradient of scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the spatial gradient of scalar
  MODULE PURE SUBROUTINE stsd_getSpatialGradient_scalar(obj, dPhidXt, Val)
    CLASS(STElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dPhidXt(:, :)
    !! Spatial gradient of scalar
    REAL(DFP), INTENT(IN) :: Val(:, :)
    !! space-time Nodal values of scalar
  END SUBROUTINE stsd_getSpatialGradient_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSpatialGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the spatial gradient of vector

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the spatial gradient of a vector from its
! space-time nodal values
  MODULE PURE SUBROUTINE stsd_getSpatialGradient_vector(obj, dVdXt, Val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: dVdXt(:, :, :)
    !! spatial gradient of `val` at integration points
    REAL(DFP), INTENT(IN) :: Val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
  END SUBROUTINE stsd_getSpatialGradient_vector
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE getSpatialGradient_scalar, getSpatialGradient_vector, &
    & stsd_getSpatialGradient_scalar, &
      & stsd_getSpatialGradient_vector
END INTERFACE getSpatialGradient

PUBLIC :: getSpatialGradient

!----------------------------------------------------------------------------
!                                            getProjectionOfdNdXt@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: Computes $\frac{dN}{dx_k}c_k$
!
!# Introduction
!
! This subroutine computes the projcetion cdNdXt on the vector `Val`
! Here the vector `Val` is constant in space and time
!
! $$P^{I}=c_{i}\frac{\partial N^{I}}{\partial x_{i}} $$

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetProjectionOfdNdXt_1(obj, cdNdXt, Val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNdXt(:, :)
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
    REAL(DFP), INTENT(IN) :: Val(:)
    !! constant value of vector
  END SUBROUTINE elemsd_GetProjectionOfdNdXt_1
END INTERFACE

!----------------------------------------------------------------------------
!                                            getProjectionOfdNdXt@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-21
! update: 2021-11-21
! summary: computes the projection of dNdXt on a vector
!
!# Introduction
!
! This subroutine computes the projcetion cdNdXt on the vector `Val`
! Here the vector `Val` is constant in space and time
!
! $$P^{I}=c_{i}\frac{\partial N^{I}}{\partial x_{i}} $$

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetProjectionOfdNdXt_2(obj, cdNdXt, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! ElemshapeData object
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNdXt(:, :)
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
    CLASS(FEVariable_), INTENT(IN) :: val
    !! constant value of vector
  END SUBROUTINE elemsd_GetProjectionOfdNdXt_2
END INTERFACE

INTERFACE getProjectionOfdNdXt
  MODULE PROCEDURE &
    & elemsd_GetProjectionOfdNdXt_1,&
    & elemsd_GetProjectionOfdNdXt_2
END INTERFACE getProjectionOfdNdXt

PUBLIC :: getProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the projection of dNTdXt on a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine computes the projcetion cdNTdXt on the vector `Val`
! Here the vector `Val` is constant in space and time
!
! $$P^{I,a}=c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$$
!
  MODULE PURE SUBROUTINE getProjectionOfdNTdXt_constvector(obj, cdNTdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNTdXt(:, :, :)
    !! returned $c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$
    REAL(DFP), INTENT(IN) :: val(:)
    !! constant value of vector
  END SUBROUTINE getProjectionOfdNTdXt_constvector
END INTERFACE

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the projection of dNTdXt on a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine computes the projcetion cdNTdXt on the vector `Val`
! Here the vector `Val` is constant in space and time
!
! $$P^{I,a}=c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$$
!
  MODULE PURE SUBROUTINE getProjectionOfdNTdXt_fevar(obj, cdNTdXt, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: cdNTdXt(:, :, :)
    !! returned $c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$
    TYPE(FEVariable_), INTENT(IN) :: val
    !! constant value of vector
  END SUBROUTINE getProjectionOfdNTdXt_fevar
END INTERFACE

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

INTERFACE getProjectionOfdNTdXt
  MODULE PROCEDURE &
    & getProjectionOfdNTdXt_constvector, &
    & getProjectionOfdNTdXt_fevar
END INTERFACE getProjectionOfdNTdXt

PUBLIC :: getProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                                    getUnitNormal@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine can be used in SUPG formulation

!> authors: Dr. Vikas Sharma
!
! This routine can be used in the SUPG formulation
!
!  $$\nabla \vert phi \vert / \vert (\nabla \vert phi \vert) \vert$$
!

  MODULE PURE SUBROUTINE getUnitNormal_scalar(obj, R, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    REAL(DFP), INTENT(IN) :: val(:)
  END SUBROUTINE getUnitNormal_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getUnitNormal@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE getUnitNormal_vector(obj, R, val)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: R(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :)
  END SUBROUTINE getUnitNormal_vector
END INTERFACE

INTERFACE getUnitNormal
  MODULE PROCEDURE getUnitNormal_vector, getUnitNormal_scalar
END INTERFACE getUnitNormal

PUBLIC :: getUnitNormal

!----------------------------------------------------------------------------
!                                                                   Contains
!----------------------------------------------------------------------------

END MODULE ElemshapeData_Method
