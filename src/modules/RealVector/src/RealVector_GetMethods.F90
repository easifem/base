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

MODULE RealVector_GetMethods
USE GlobalData, ONLY: DFP, I4B, LGT, REAL32, REAL64

USE BaseType, ONLY: RealVector_, DOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: GetIndex
PUBLIC :: Get
PUBLIC :: IsPresent
PUBLIC :: GetPointer

!----------------------------------------------------------------------------
!                                                    GetPointer@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: Returns the pointer to vector of real numbers in [[RealVector_]]
!
!# Introduction
!
! This function returns the pointer to vector of real numbers stored
! inside [[RealVector_]]

INTERFACE GetPointer
  MODULE FUNCTION obj_GetPointer1(obj) RESULT(val)
    TYPE(RealVector_), INTENT(IN), TARGET :: obj
    REAL(DFP), POINTER :: val(:)
  END FUNCTION obj_GetPointer1
END INTERFACE GetPointer

!----------------------------------------------------------------------------
!                                                    GetPointer@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: Returns the pointer to vector of real numbers
!
!# Introduction
! This function returns the pointer to vector of real numbers stored
! inside [[RealVector_]] for a given degree of freedom

INTERFACE GetPointer
  MODULE FUNCTION obj_GetPointer2(obj, dofobj, idof) RESULT(val)
    TYPE(RealVector_), INTENT(IN), TARGET :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), POINTER :: val(:)
  END FUNCTION obj_GetPointer2
END INTERFACE GetPointer

!----------------------------------------------------------------------------
!                                                         GetIndex@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function finds location of value inside the [[RealVector_]]
!
!# Introduction
!
! This function finds the location of `value` inside the instance of
! [[RealVector_]]

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex1(obj, VALUE, tol) RESULT(Ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    INTEGER(I4B) :: Ans
  END FUNCTION obj_GetIndex1
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                           IndexOf@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: Returns location of values inside the [[RealVector_]]
!
!# Introduction
!
! This function returns the nearest location of values inside the
! [[RealVector_]]

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex2(obj, VALUE, tol) RESULT(Ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION obj_GetIndex2
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                       IsPresent@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: Returns true if value is present inside [[RealVector_]]

INTERFACE IsPresent
  MODULE PURE FUNCTION obj_IsPresent1(obj, VALUE, tol) RESULT(Ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    LOGICAL(LGT) :: Ans
  END FUNCTION obj_IsPresent1
END INTERFACE IsPresent

!----------------------------------------------------------------------------
!                                                         IsPresentGetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: Returns true if value is present inside [[RealVector_]]

INTERFACE IsPresent
  MODULE PURE FUNCTION obj_IsPresent2(obj, VALUE, tol) RESULT(Ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    LOGICAL(LGT), ALLOCATABLE :: Ans(:)
  END FUNCTION obj_IsPresent2
END INTERFACE IsPresent

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of Integer from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get1(obj, dataType) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dataType
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of integer from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get2(obj, nodenum, dataType) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: dataType
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get2
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of integer from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get3(obj, istart, iend, stride, dataType) &
    RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    INTEGER(I4B), INTENT(IN) :: dataType
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get3
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get4a(obj, dataType) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    REAL(REAL32), INTENT(IN) :: dataType
    REAL(REAL32), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get4a

  MODULE PURE FUNCTION obj_Get4b(obj, dataType) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    REAL(REAL64), INTENT(IN) :: dataType
    REAL(REAL64), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get4b
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get5a(obj, nodenum, dataType) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(REAL32), INTENT(IN) :: dataType
    REAL(REAL32), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get5a
  MODULE PURE FUNCTION obj_Get5b(obj, nodenum, dataType) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(REAL64), INTENT(IN) :: dataType
    REAL(REAL64), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get5b
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get6(obj, istart, iend, stride, dataType) &
    RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(DFP), INTENT(IN) :: dataType
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get6
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                     Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns the vector of integer from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get7(obj, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: dataType
    INTEGER(I4B), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get7
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of integer from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get8(obj, nodenum, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: dataType
    INTEGER(I4B), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get8
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns an integer vector from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get9(obj, istart, iend, stride, dataType) &
    RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: dataType
    INTEGER(I4B), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get9
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get10a(obj, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    REAL(REAL32), INTENT(IN) :: dataType
    REAL(REAL32), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get10a

  MODULE PURE FUNCTION obj_Get10b(obj, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    REAL(REAL64), INTENT(IN) :: dataType
    REAL(REAL64), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get10b
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                     Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get11a(obj, nodenum, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    REAL(REAL32), INTENT(IN) :: dataType
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(REAL32), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get11a

  MODULE PURE FUNCTION obj_Get11b(obj, nodenum, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    REAL(REAL64), INTENT(IN) :: dataType
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(REAL64), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get11b
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get12a(obj, istart, iend, stride, dataType) &
    RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(REAL32), INTENT(IN) :: dataType
    REAL(REAL32), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get12a

  MODULE PURE FUNCTION obj_Get12b(obj, istart, iend, stride, &
    & dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(REAL64), INTENT(IN) :: dataType
    REAL(REAL64), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get12b
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns an scalar instance of [[RealVector_]] by
! combining different entries of a vector of [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get13(obj, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    TYPE(RealVector_), INTENT(IN) :: dataType
    TYPE(RealVector_) :: val
  END FUNCTION obj_Get13
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns the instance of [[RealVector_]] from the vector of
! [[RealVector_]].

INTERFACE Get
  MODULE PURE FUNCTION obj_Get14(obj, nodenum, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    TYPE(RealVector_), INTENT(IN) :: dataType
    TYPE(RealVector_) :: val
  END FUNCTION obj_Get14
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns the instance of [[RealVector_]] from the vector of
! [[RealVector_]].

INTERFACE Get
  MODULE PURE FUNCTION obj_Get15(obj, istart, iend, stride, &
                                 dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    TYPE(RealVector_), INTENT(IN) :: dataType
    TYPE(RealVector_) :: val
  END FUNCTION obj_Get15
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns an instance of [[RealVector_]]
!
!# Introduction
!
! This function returns an instance of [[RealVector_]] by using selective
! from `obj`

INTERFACE Get
  MODULE PURE FUNCTION obj_Get16(obj, nodenum, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    TYPE(RealVector_), INTENT(IN) :: dataType
    TYPE(RealVector_) :: val
  END FUNCTION obj_Get16
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns the instance of [[RealVector_]] using istart, iend,
! stride values

INTERFACE Get
  MODULE PURE FUNCTION obj_Get17(obj, istart, iend, stride, &
                                 dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    TYPE(RealVector_), INTENT(IN) :: dataType
    TYPE(RealVector_) :: val
  END FUNCTION obj_Get17
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION obj_Get18a(obj, nodenum, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(REAL32), INTENT(IN) :: dataType
    REAL(REAL32) :: val
  END FUNCTION obj_Get18a

  MODULE PURE FUNCTION obj_Get18b(obj, nodenum, dataType) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(REAL64), INTENT(IN) :: dataType
    REAL(REAL64) :: val
  END FUNCTION obj_Get18b
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get19(obj) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get19
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get20(obj, nodenum) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get20
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get21(obj, istart, iend, stride) &
    RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get21
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get22(obj) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get22
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                     Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get23(obj, nodenum) RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get23
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get24(obj, istart, iend, stride) &
    RESULT(val)
    TYPE(RealVector_), INTENT(IN) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(DFP), ALLOCATABLE :: val(:)
  END FUNCTION obj_Get24
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get25(obj, dofobj, nodenum, ivar, idof) &
    RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP) :: ans
  END FUNCTION obj_Get25
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get26(obj, dofobj, nodenum, ivar, idof) &
    RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP) :: ans(SIZE(nodenum))
  END FUNCTION obj_Get26
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get27(obj, dofobj, nodenum, ivar) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get27
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get28(obj, dofobj, nodenum, &
                                 ivar, spacecompo, timecompo) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get28
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Feb 2021
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE Get
  MODULE PURE FUNCTION obj_Get29(obj, dofobj, idof) RESULT(ans)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_Get29
END INTERFACE Get

END MODULE RealVector_GetMethods
