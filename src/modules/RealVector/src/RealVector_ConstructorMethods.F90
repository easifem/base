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

MODULE RealVector_ConstructorMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: RealVector_, DOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: Shape
PUBLIC :: SIZE
PUBLIC :: GetTotalDimension
PUBLIC :: SetTotalDimension
PUBLIC :: ALLOCATE
PUBLIC :: DEALLOCATE
PUBLIC :: Initiate
PUBLIC :: RANDOM_NUMBER
PUBLIC :: RealVector
PUBLIC :: RealVector_Pointer
PUBLIC :: Reallocate
PUBLIC :: isAllocated
PUBLIC :: isInitiated

!----------------------------------------------------------------------------
!                                                 isAllocated@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! summary: Returns true if the instance is allocated

INTERFACE isAllocated
  MODULE PURE FUNCTION obj_isAllocated(obj) RESULT(Ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isAllocated
END INTERFACE isAllocated

INTERFACE isInitiated
  MODULE PROCEDURE obj_isAllocated
END INTERFACE isInitiated

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: This function returns the shape of RealVector_

INTERFACE Shape
  MODULE PURE FUNCTION obj_shape(obj) RESULT(Ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans(1)
  END FUNCTION obj_shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: This function returns the size of RealVector_

INTERFACE Size
  MODULE PURE FUNCTION obj_size(obj, Dims) RESULT(Ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: Dims
    INTEGER(I4B) :: Ans
  END FUNCTION obj_size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: Returns the total dimension of an array
!
!# Introduction
!
! This function returns the total dimension (or rank) of an array,

INTERFACE GetTotalDimension
  MODULE PURE FUNCTION RealVec_GetTotalDimension(obj) RESULT(Ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION RealVec_GetTotalDimension
END INTERFACE GetTotalDimension

!----------------------------------------------------------------------------
!                                             SetTotalDimension@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This subroutine Set the total dimension (rank) of an array
!
!# Introduction
!
! This subroutine Sets the rank(total dimension) of an array

INTERFACE SetTotalDimension
  MODULE PURE SUBROUTINE RealVec_SetTotalDimension(obj, tDimension)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tDimension
  END SUBROUTINE RealVec_SetTotalDimension
END INTERFACE SetTotalDimension

!----------------------------------------------------------------------------
!                                                   Allocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: This subroutine allocates memory for RealVector

INTERFACE ALLOCATE
  MODULE PURE SUBROUTINE obj_Allocate(obj, Dims)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Dims
  END SUBROUTINE obj_Allocate
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                                   Reallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: Allocate memory for the vector

INTERFACE Reallocate
  MODULE PURE SUBROUTINE obj_Reallocate(obj, row)
    TYPE(RealVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE obj_Reallocate
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: This subroutine deallocates the data in RealVector_

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    CLASS(RealVector_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: This subroutine allocates the memory for RealVector_
!
!# Introduction This subroutine allocates the memeory for RealVector_
!
!@note
! This subroutine is an alias for Allocate_Data
!@endnote

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_initiate1(obj, tSize)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END SUBROUTINE obj_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: This subroutine allocate the memory for a vector of type
! RealVector_
!
!# Introduction
! This subroutine allocate the memory for a vector of type RealVector_
!@note
! The size of `obj` would be same as the size of `tSize`
!@endnote

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2(obj, tSize)
    TYPE(RealVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tSize(:)
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: This subroutine allocate the memory for an instance of
! RealVector_
!
!# Introduction
! This subroutine allocate the memory for an instance of RealVector_.
! User can specify the lowerbounds and upper bounds.

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate3(obj, a, b)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: a, b
  END SUBROUTINE obj_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: Initiate RealVector_ using dof_ object
!
!# Introduction
!
! This subroutine initiate RealVector_ using the information stored inside
! dof_ object. It Gets the information of total size of RealVector_
! from DOF_ and call RealVector_Method:Initiate routine.
! All values of RealVector_ is Set to zero.

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate4(obj, dofobj)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
  END SUBROUTINE obj_Initiate4
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Initiate a vector of realvector_ from dof_ object
!
!# Introduction
!
! This subroutine initiates a vector of realvector_ object.
! The size of `val` will be total number of degrees of freedom inside
! the DOF_ object. Therefore, each `val( idof )` denotes the
! nodal vector of correrponding to a degree of freedom number `idof`

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate5(obj, dofobj)
    TYPE(RealVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    CLASS(DOF_), INTENT(IN) :: dofobj
  END SUBROUTINE obj_Initiate5
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This routine computes radom_number
!
!# Introduction
!
! This routine calls `RANDOM_NUMBER` to generate a random instnance of
! RealVector_

INTERFACE RANDOM_NUMBER
  MODULE SUBROUTINE obj_Random_Number1(obj, tsize)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tsize
  END SUBROUTINE obj_Random_Number1
END INTERFACE RANDOM_NUMBER

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This routine computes radom_number
!
!# Introduction
!
! This routine calls `RANDOM_NUMBER` to generate a random instnance of
! RealVector_
!
!@note
!         Here argument `obj` is a vector of RealVector_ data-types.
!@endnote

INTERFACE RANDOM_NUMBER
  MODULE SUBROUTINE obj_Random_Number2(obj, tsize)
    TYPE(RealVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tsize(:)
  END SUBROUTINE obj_Random_Number2
END INTERFACE RANDOM_NUMBER

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns an instance of RealVector_
!
!# Introduction
!
! This function returns an instance of RealVector_

INTERFACE RealVector
  MODULE PURE FUNCTION obj_Constructor1(tSize) RESULT(obj)
    TYPE(RealVector_) :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END FUNCTION obj_Constructor1
END INTERFACE RealVector

!----------------------------------------------------------------------------
!                                                     RealVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns an instance of RealVector_
!
!# Introduction
! This function returns an instance of RealVector_ by copying the
! contents of a fortran integer vector.
!
!@note
!         This routine internally calls RealVector_Method:COPY routine.
!@endnote

INTERFACE RealVector
  MODULE PURE FUNCTION obj_Constructor2(Val) RESULT(obj)
    TYPE(RealVector_) :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:)
  END FUNCTION obj_Constructor2
END INTERFACE RealVector

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns an instance of RealVector_
!
!# Introduction
! This function returns an instance of RealVector_ by copying the
! contents of a fortran real vector.
!
!@note
!         This routine internally calls RealVector_Method:COPY routine.
!@endnote

INTERFACE RealVector
  MODULE PURE FUNCTION obj_Constructor3(Val) RESULT(obj)
    TYPE(RealVector_) :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
  END FUNCTION obj_Constructor3
END INTERFACE RealVector

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returnt the pointer to a newly created instance of
! RealVector_
!
!# Introduction
! This function returnt the pointer to a newly created instance of
! RealVector_

INTERFACE RealVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_1(tSize) RESULT(obj)
    CLASS(RealVector_), POINTER :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END FUNCTION obj_Constructor_1
END INTERFACE RealVector_Pointer

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns the pointer to an instance ofRealVector_
!
!# Introduction
! This function returns a pointer to an newly created instance of
! RealVector_ by copying the contents of a fortran integer vector.
!
!@note
!         This routine internally calls RealVector_Method:COPY routine.
!@endnote

INTERFACE RealVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_2(Val) RESULT(obj)
    CLASS(RealVector_), POINTER :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:)
  END FUNCTION obj_Constructor_2
END INTERFACE RealVector_Pointer

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns the pointer to an instance ofRealVector_
!
!# Introduction
! This function returns a pointer to an newly created instance of
! RealVector_ by copying the contents of a fortran real vector.
!
!@note
!         This routine internally calls RealVector_Method:COPY routine.
!@endnote

INTERFACE RealVector_Pointer
  MODULE PURE FUNCTION obj_Constructor_3(Val) RESULT(obj)
    CLASS(RealVector_), POINTER :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
  END FUNCTION obj_Constructor_3
END INTERFACE RealVector_Pointer

END MODULE RealVector_ConstructorMethods
