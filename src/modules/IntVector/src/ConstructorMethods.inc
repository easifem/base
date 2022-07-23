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

PUBLIC :: Shape
PUBLIC :: SIZE
PUBLIC :: getTotalDimension
PUBLIC :: ALLOCATE
PUBLIC :: Deallocate
PUBLIC :: Reallocate
PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: IntVector
PUBLIC :: IntVector_Pointer
PUBLIC :: Convert

!----------------------------------------------------------------------------
!                                                         Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: Returns shape of the vector

INTERFACE
  MODULE PURE FUNCTION intVec_shape(obj) RESULT(Ans)
    CLASS(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans(1)
  END FUNCTION intVec_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE intVec_shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         25 Feb 2021
! summary:         Returns size of the vector

INTERFACE
  MODULE PURE FUNCTION intVec_Size(obj, Dims) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN), OPTIONAL :: Dims
    INTEGER(I4B) :: Ans
  END FUNCTION intVec_Size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE intVec_Size
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         Returns the total dimension of an array
!
!# Introduction
!
! This function returns the total dimension (or rank) of an array,

INTERFACE
  MODULE PURE FUNCTION IntVec_getTotalDimension(obj) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION IntVec_getTotalDimension
END INTERFACE

INTERFACE getTotalDimension
  MODULE PROCEDURE IntVec_getTotalDimension
END INTERFACE getTotalDimension

!----------------------------------------------------------------------------
!                                                   Allocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  25 Feb 2021
! summary:         Allocate memory for the vector

INTERFACE
  MODULE PURE SUBROUTINE intVec_AllocateData(obj, Dims)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Dims
  END SUBROUTINE intVec_AllocateData
END INTERFACE

!! TODO
!! Replace Allocate --> Allocate

! INTERFACE Allocate
!   MODULE PROCEDURE intVec_AllocateData
! END INTERFACE Allocate

INTERFACE ALLOCATE
  MODULE PROCEDURE intVec_AllocateData
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                                   Reallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  25 Feb 2021
! summary: Allocate memory for the vector

INTERFACE
  MODULE PURE SUBROUTINE intVec_Reallocate(obj, row)
    TYPE(IntVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE intVec_Reallocate
END INTERFACE

INTERFACE Reallocate
  MODULE PROCEDURE intVec_Reallocate
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: Deallocate memory occupied by IntVector

INTERFACE
  MODULE PURE SUBROUTINE intVec_Deallocate(obj)
    CLASS(IntVector_), INTENT(INOUT) :: obj
  END SUBROUTINE intVec_Deallocate
END INTERFACE

!! TODO Rename Deallocate to Deallocate

INTERFACE Deallocate
  MODULE PROCEDURE intVec_Deallocate
END INTERFACE Deallocate

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine initiates the [[IntVector_]]
!
!# Introduction
!
! This routine initiates an instance of IntVector
! Only the size of intvector is set.

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate1(obj, tSize)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END SUBROUTINE intVec_initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE intVec_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine initiates the vector of [[IntVector_]]

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate2(obj, tSize)
    TYPE(IntVector_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tSize(:)
  END SUBROUTINE intVec_initiate2
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE intVec_initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Initiates an instance on [[IntVector_]] with lower & upper bounds

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate3(obj, a, b)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: a, b
  END SUBROUTINE intVec_initiate3
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE intVec_initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Initiates an instance by using a vector of integers
!
!# Introduction
! This routine initiates an instance of intvector by coping data
! from integer vector.
!
! This routine also define an assignment operator, obj=val

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate4a(obj, val)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(Int8), INTENT(IN) :: val(:)
  END SUBROUTINE intVec_initiate4a
  !!
  MODULE PURE SUBROUTINE intVec_initiate4b(obj, val)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(Int16), INTENT(IN) :: val(:)
  END SUBROUTINE intVec_initiate4b
  !!
  MODULE PURE SUBROUTINE intVec_initiate4c(obj, val)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(Int32), INTENT(IN) :: val(:)
  END SUBROUTINE intVec_initiate4c
  !!
  MODULE PURE SUBROUTINE intVec_initiate4d(obj, val)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(Int64), INTENT(IN) :: val(:)
  END SUBROUTINE intVec_initiate4d
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE intVec_initiate4a, intVec_initiate4b, &
    & intVec_initiate4c, intVec_initiate4d
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE intVec_initiate4a, intVec_initiate4b, &
    & intVec_initiate4c, intVec_initiate4d
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Initiates an instance by using a vector of real numbers
!
!# Introduction
!
! This routine initiates an instance of IntVector by copying data
! from a vector of reals. This routien also defines assignment operator,
! obj=val

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate5a(obj, val)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    REAL(Real32), INTENT(IN) :: val(:)
  END SUBROUTINE intVec_initiate5a
  !!
  MODULE PURE SUBROUTINE intVec_initiate5b(obj, val)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    REAL(Real64), INTENT(IN) :: val(:)
  END SUBROUTINE intVec_initiate5b
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE intVec_initiate5a, intVec_initiate5b
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE intVec_initiate5a, intVec_initiate5b
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: IntVector returns an instance of [[IntVector_]] of given size

INTERFACE
  MODULE PURE FUNCTION intVec_Constructor1(tSize) RESULT(obj)
    TYPE(IntVector_) :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END FUNCTION intVec_Constructor1
END INTERFACE

INTERFACE IntVector
  MODULE PROCEDURE intVec_Constructor1
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Convert a integer vector into [[IntVector_]]

INTERFACE
  MODULE PURE FUNCTION intVec_Constructor2(Val) RESULT(obj)
    TYPE(IntVector_) :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:)
  END FUNCTION intVec_Constructor2
END INTERFACE

INTERFACE IntVector
  MODULE PROCEDURE intVec_Constructor2
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Converts a real vector into [[IntVector_]]
!
! TODO Implement IntVector method for Int4, Int8, Int16, Int32
! Real32, Real64
!
INTERFACE
  MODULE PURE FUNCTION intVec_Constructor3(Val) RESULT(obj)
    TYPE(IntVector_) :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
  END FUNCTION intVec_Constructor3
END INTERFACE

INTERFACE IntVector
  MODULE PROCEDURE intVec_Constructor3
END INTERFACE IntVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns the pointer to an instance of [[IntVector_]] of tsize

INTERFACE
  MODULE PURE FUNCTION intVec_Constructor_1(tSize) RESULT(obj)
    CLASS(IntVector_), POINTER :: obj
    INTEGER(I4B), INTENT(IN) :: tSize
  END FUNCTION intVec_Constructor_1
END INTERFACE

INTERFACE IntVector_Pointer
  MODULE PROCEDURE intVec_Constructor_1
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Converts integer vector into [[intvector_]] and returns the pointer

INTERFACE
  MODULE PURE FUNCTION intVec_Constructor_2(Val) RESULT(obj)
    CLASS(IntVector_), POINTER :: obj
    INTEGER(I4B), INTENT(IN) :: Val(:)
  END FUNCTION intVec_Constructor_2
END INTERFACE

INTERFACE IntVector_Pointer
  MODULE PROCEDURE intVec_Constructor_2
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Converts real vector into [[intvector_]] and returns the pointer

INTERFACE
  MODULE PURE FUNCTION intVec_Constructor_3(Val) RESULT(obj)
    CLASS(IntVector_), POINTER :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
  END FUNCTION intVec_Constructor_3
END INTERFACE


INTERFACE IntVector_Pointer
  MODULE PROCEDURE intVec_Constructor_3
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                 assign@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE IntVec_assign_a(Val, obj)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: Val(:)
    CLASS(IntVector_), INTENT(IN) :: obj
  END SUBROUTINE IntVec_assign_a
END INTERFACE

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE IntVec_assign_a
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                Convert@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE obj_convert_int(From, To)
    CLASS(IntVector_), INTENT(IN) :: From
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: To(:)
  END SUBROUTINE obj_convert_int
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE obj_convert_int
END INTERFACE Convert

