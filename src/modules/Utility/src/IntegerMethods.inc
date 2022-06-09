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

PUBLIC :: OPERATOR(.in.)
PUBLIC :: OPERATOR(.isin.)

!----------------------------------------------------------------------------
!                                             Operator(.in.)@IntegerMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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
    LOGICAL(LGT) :: ans( SIZE( a ) )
  END FUNCTION isin_1a

  MODULE PURE FUNCTION isin_1b(a, b) RESULT(Ans)
    INTEGER(Int16), INTENT(IN) :: a(:)
    INTEGER(Int16), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans( SIZE( a ) )
  END FUNCTION isin_1b

  MODULE PURE FUNCTION isin_1c(a, b) RESULT(Ans)
    INTEGER(Int32), INTENT(IN) :: a(:)
    INTEGER(Int32), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans( SIZE( a ) )
  END FUNCTION isin_1c

  MODULE PURE FUNCTION isin_1d(a, b) RESULT(Ans)
    INTEGER(Int64), INTENT(IN) :: a(:)
    INTEGER(Int64), INTENT(IN) :: b(:)
    LOGICAL(LGT) :: ans( SIZE( a ) )
  END FUNCTION isin_1d

END INTERFACE

INTERFACE OPERATOR(.isin.)
  MODULE PROCEDURE isin_1a, isin_1b, isin_1c, isin_1d
END INTERFACE OPERATOR(.isin.)

!----------------------------------------------------------------------------
!                                             Operator(.in.)@IntegerMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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


