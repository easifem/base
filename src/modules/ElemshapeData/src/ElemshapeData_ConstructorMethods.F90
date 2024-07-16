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

MODULE ElemshapeData_ConstructorMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate
PUBLIC :: DEALLOCATE
PUBLIC :: ALLOCATE
PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                  Allocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Allocates the memory for various matrices in elemsd
!
!# Introduction
!
!- This subroutine allocates the memory for various matrices in the obj.
!- This subroutine belongs to the generic interface called `Allocate()`.

INTERFACE ALLOCATE
  MODULE PURE SUBROUTINE elemsd_Allocate(obj, nsd, xidim, nns, nips, nnt)
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! it is used when elemshape data is STElemShapeData
  END SUBROUTINE elemsd_Allocate
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiate the element shapefunction data

INTERFACE Initiate
  MODULE SUBROUTINE elemsd_Initiate1(obj, quad, refelem, continuityType, &
                                     interpolType)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
    !! ElemshapeData to be formed
    CLASS(QuadraturePoint_), INTENT(IN) :: quad
    !! Quadrature points
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element
    CHARACTER(*), INTENT(IN) :: continuityType
    !! - continuity/ conformity of shape function (basis functions)
    CHARACTER(*), INTENT(IN) :: interpolType
    !! interpolation/polynomial family for basis functions
  END SUBROUTINE elemsd_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Copy data from an instance of elemshapedata to another instance

INTERFACE Initiate
  MODULE SUBROUTINE elemsd_Initiate2(obj1, obj2)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj1
    CLASS(ElemshapeData_), INTENT(IN) :: obj2
  END SUBROUTINE elemsd_Initiate2
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE elemsd_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Initiate time shape function data in [[stelemshapedata_]]
!
!# Introduction
!
! - This subroutine Initiates the shape-function data related to time
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

INTERFACE Initiate
  MODULE PURE SUBROUTINE stsd_Initiate(obj, elemsd)
    TYPE(STElemshapeData_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    TYPE(ElemshapeData_), INTENT(IN) :: elemsd
    !! It has information about location shape function for time element
  END SUBROUTINE stsd_Initiate
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Deallocates the data stored inside [[elemshapedata_]]
!
!# Introduction
!
! This routine deallocates the data stored inside [[elemshapedata_]]. This
! routine belongs to `Allocate()`
!

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE elemsd_Deallocate(obj)
    CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  END SUBROUTINE elemsd_Deallocate
END INTERFACE DEALLOCATE

END MODULE ElemshapeData_ConstructorMethods
