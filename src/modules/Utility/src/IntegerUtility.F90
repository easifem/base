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

MODULE IntegerUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: OPERATOR(.in.)
PUBLIC :: OPERATOR(.isin.)
PUBLIC :: RemoveDuplicates
PUBLIC :: Repeat
PUBLIC :: SIZE
PUBLIC :: GetMultiIndices

!----------------------------------------------------------------------------
!                                                           Size@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Get the number of touples

INTERFACE
  MODULE PURE FUNCTION obj_Size1(n, d) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n, d
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size1
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE obj_Size1
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                           Size@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Get the number of touples

INTERFACE
  MODULE PURE FUNCTION obj_Size2(n, d, upto) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n, d
    LOGICAL(LGT), INTENT(IN) :: upto
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size2
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE obj_Size2
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                         GetIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Get Indices

INTERFACE
  MODULE PURE FUNCTION obj_GetMultiIndices1(n, d) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n, d
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_GetMultiIndices1
END INTERFACE

INTERFACE GetMultiIndices
  MODULE PROCEDURE obj_GetMultiIndices1
END INTERFACE GetMultiIndices

!----------------------------------------------------------------------------
!                                                         GetIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Get Indices upto order n

INTERFACE
  MODULE PURE FUNCTION obj_GetMultiIndices2(n, d, upto) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n, d
    LOGICAL(LGT), INTENT(IN) :: upto
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_GetMultiIndices2
END INTERFACE

INTERFACE GetMultiIndices
  MODULE PROCEDURE obj_GetMultiIndices2
END INTERFACE GetMultiIndices

!----------------------------------------------------------------------------
!                                             Operator(.in.)@IntegerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns true if a integer set is inside another

INTERFACE

  MODULE PURE FUNCTION in_1a(a, b) RESULT(Ans)
    INTEGER(Int8), INTENT(IN) :: a(:)
    INTEGER(Int8), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_1a

  MODULE PURE FUNCTION in_1b(a, b) RESULT(Ans)
    INTEGER(Int16), INTENT(IN) :: a(:)
    INTEGER(Int16), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_1b

  MODULE PURE FUNCTION in_1c(a, b) RESULT(Ans)
    INTEGER(Int32), INTENT(IN) :: a(:)
    INTEGER(Int32), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_1c

  MODULE PURE FUNCTION in_1d(a, b) RESULT(Ans)
    INTEGER(Int64), INTENT(IN) :: a(:)
    INTEGER(Int64), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_1d

END INTERFACE

INTERFACE OPERATOR(.in.)
  MODULE PROCEDURE in_1a, in_1b, in_1c, in_1d
END INTERFACE OPERATOR(.in.)

!----------------------------------------------------------------------------
!                                             Operator(.in.)@IntegerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns true if a integer set is inside another
!
!# Introduction
!
! This function returns a vector of booleans
! if a(i) is inside the b, then ans(i) is true, otherwise false.

INTERFACE

  MODULE PURE FUNCTION isin_1a(a, b) RESULT(Ans)
    INTEGER(Int8), INTENT(IN) :: a(:)
    INTEGER(Int8), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans(SIZE(a))
  END FUNCTION isin_1a

  MODULE PURE FUNCTION isin_1b(a, b) RESULT(Ans)
    INTEGER(Int16), INTENT(IN) :: a(:)
    INTEGER(Int16), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans(SIZE(a))
  END FUNCTION isin_1b

  MODULE PURE FUNCTION isin_1c(a, b) RESULT(Ans)
    INTEGER(Int32), INTENT(IN) :: a(:)
    INTEGER(Int32), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans(SIZE(a))
  END FUNCTION isin_1c

  MODULE PURE FUNCTION isin_1d(a, b) RESULT(Ans)
    INTEGER(Int64), INTENT(IN) :: a(:)
    INTEGER(Int64), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans(SIZE(a))
  END FUNCTION isin_1d

END INTERFACE

INTERFACE OPERATOR(.isin.)
  MODULE PROCEDURE isin_1a, isin_1b, isin_1c, isin_1d
END INTERFACE OPERATOR(.isin.)

!----------------------------------------------------------------------------
!                                             Operator(.in.)@IntegerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-11
! update: 2021-11-11
! summary: Returns true if a integer set is inside another

INTERFACE

  MODULE PURE FUNCTION in_2a(a, b) RESULT(Ans)
    INTEGER(Int8), INTENT(IN) :: a
    INTEGER(Int8), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_2a

  MODULE PURE FUNCTION in_2b(a, b) RESULT(Ans)
    INTEGER(Int16), INTENT(IN) :: a
    INTEGER(Int16), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_2b

  MODULE PURE FUNCTION in_2c(a, b) RESULT(Ans)
    INTEGER(Int32), INTENT(IN) :: a
    INTEGER(Int32), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_2c

  MODULE PURE FUNCTION in_2d(a, b) RESULT(Ans)
    INTEGER(Int64), INTENT(IN) :: a
    INTEGER(Int64), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans
  END FUNCTION in_2d

END INTERFACE

INTERFACE OPERATOR(.in.)
  MODULE PROCEDURE in_2a, in_2b, in_2c, in_2d
END INTERFACE OPERATOR(.in.)

INTERFACE OPERATOR(.isin.)
  MODULE PROCEDURE in_2a, in_2b, in_2c, in_2d
END INTERFACE OPERATOR(.isin.)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE RemoveDuplicates_1a(obj)
    INTEGER(Int8), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RemoveDuplicates_1a
  MODULE PURE SUBROUTINE RemoveDuplicates_1b(obj)
    INTEGER(Int16), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RemoveDuplicates_1b
  MODULE PURE SUBROUTINE RemoveDuplicates_1c(obj)
    INTEGER(Int32), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RemoveDuplicates_1c
  MODULE PURE SUBROUTINE RemoveDuplicates_1d(obj)
    INTEGER(Int64), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE RemoveDuplicates_1d
END INTERFACE

INTERFACE RemoveDuplicates
  MODULE PROCEDURE RemoveDuplicates_1a, RemoveDuplicates_1b, &
    & RemoveDuplicates_1c, RemoveDuplicates_1d
END INTERFACE RemoveDuplicates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Repeat_1a(Val, rtimes) RESULT(Ans)
    INTEGER(Int8), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(Int8) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1a
  MODULE PURE FUNCTION Repeat_1b(Val, rtimes) RESULT(Ans)
    INTEGER(Int16), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(Int16) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1b
  MODULE PURE FUNCTION Repeat_1c(Val, rtimes) RESULT(Ans)
    INTEGER(Int32), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(Int32) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1c
  MODULE PURE FUNCTION Repeat_1d(Val, rtimes) RESULT(Ans)
    INTEGER(Int64), INTENT(IN) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(Int64) :: Ans(SIZE(Val) * rtimes)
  END FUNCTION Repeat_1d
END INTERFACE

INTERFACE Repeat
  MODULE PROCEDURE Repeat_1a, Repeat_1b, Repeat_1c, Repeat_1d
END INTERFACE Repeat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE IntegerUtility
