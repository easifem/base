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

PUBLIC :: Initiate
PUBLIC :: DEALLOCATE
PUBLIC :: ALLOCATE
PUBLIC :: BaseInterpolation
PUBLIC :: BaseContinuity


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

INTERFACE ALLOCATE
  MODULE PROCEDURE elemsd_AllocateData
END INTERFACE ALLOCATE

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

INTERFACE
  MODULE PURE SUBROUTINE stsd_initiate(obj, elemsd)
    TYPE(STElemshapeData_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    TYPE(ElemshapeData_), INTENT(IN) :: elemsd
    !! It has information about location shape function for time element
  END SUBROUTINE stsd_initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE stsd_initiate
END INTERFACE Initiate

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

INTERFACE DEALLOCATE
  MODULE PROCEDURE elemsd_Deallocate
END INTERFACE DEALLOCATE

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
