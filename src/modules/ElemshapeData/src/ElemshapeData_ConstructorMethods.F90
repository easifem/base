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
  MODULE PURE SUBROUTINE elemsd_Allocate(obj, nsd, xidim, nns, nips)
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
  END SUBROUTINE elemsd_Allocate
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate the element shapefunction data

INTERFACE Initiate
  MODULE SUBROUTINE elemsd_initiate1(obj, quad, refelem, continuityType, &
    & interpolType)
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
  END SUBROUTINE elemsd_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Copy data from an instance of elemshapedata to another instance

INTERFACE Initiate
  MODULE SUBROUTINE elemsd_initiate2(obj1, obj2)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj1
    TYPE(ElemshapeData_), INTENT(IN) :: obj2
  END SUBROUTINE elemsd_initiate2
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE elemsd_initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Initiate an instance of ElemshapeData from STElemshapeData
!
!# Introduction
!
! This subroutine initiates an instance of ElemshapeData by copying data
! from an instance of STElemshapeData.

INTERFACE Initiate
  MODULE SUBROUTINE elemsd_initiate3(obj1, obj2)
    TYPE(ElemshapeData_), INTENT(INOUT) :: obj1
    TYPE(STElemshapeData_), INTENT(IN) :: obj2
  END SUBROUTINE elemsd_initiate3
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE elemsd_initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: This routine initiates an instance of STElemshapeData
!
!# Introduction
!
! This routine initiate an instance of STElemshapeData by copying data
! from the instance of ElemshapeData

INTERFACE Initiate
  MODULE SUBROUTINE elemsd_initiate4(obj1, obj2)
    TYPE(STElemshapeData_), INTENT(INOUT) :: obj1
    TYPE(ElemshapeData_), INTENT(IN) :: obj2
  END SUBROUTINE elemsd_initiate4
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE elemsd_initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Initiate an instance of STElemshapeData from instance of same class
!
!# Introduction
! This routine initiates an instance of STElemshapeData by copying data
! from the instance of STElemshapeData.

INTERFACE Initiate
  MODULE SUBROUTINE elemsd_initiate5(obj1, obj2)
    TYPE(STElemshapeData_), INTENT(INOUT) :: obj1
    TYPE(STElemshapeData_), INTENT(IN) :: obj2
  END SUBROUTINE elemsd_initiate5
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE elemsd_initiate5
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

INTERFACE Initiate
  MODULE PURE SUBROUTINE stsd_initiate(obj, elemsd)
    TYPE(STElemshapeData_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    TYPE(ElemshapeData_), INTENT(IN) :: elemsd
    !! It has information about location shape function for time element
  END SUBROUTINE stsd_initiate
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
