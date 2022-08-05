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

MODULE BinomUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: Binom

!----------------------------------------------------------------------------
!                                                                Binom
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	2 Aug 2022
! summary: 	Compute the Binomial coefficient
!
!# Introduction
!
! This routine calculates the bionomial coefficient $C_{k}^{n}$
!
! Usages is given below
!
!
!## Usage
!
!```fortran
! ans = Binom( n=10, k=2 )
!```

INTERFACE
MODULE RECURSIVE FUNCTION Real32_Binom_Int8( n, k, kind ) RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: n
    !! n is integer, should be a positive number and greater or equal to k
  INTEGER( Int8 ), INTENT( IN ) :: k
  REAL( Real32 ) :: kind
  REAL( Real32 ) :: ans
END FUNCTION Real32_Binom_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Binom
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE FUNCTION Real32_Binom_Int16( n, k, kind ) RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: n
  INTEGER( Int16 ), INTENT( IN ) :: k
  REAL( Real32 ) :: kind
  REAL( Real32 ) :: ans
END FUNCTION Real32_Binom_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Binom
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE FUNCTION Real32_Binom_Int32( n, k, kind ) RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: n
  INTEGER( Int32 ), INTENT( IN ) :: k
  REAL( Real32 ) :: kind
  REAL( Real32 ) :: ans
END FUNCTION Real32_Binom_Int32
!!
MODULE RECURSIVE FUNCTION Real32_Binom_Int64( n, k, kind ) RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: n
  INTEGER( Int64 ), INTENT( IN ) :: k
  REAL( Real32 ) :: kind
  REAL( Real32 ) :: ans
END FUNCTION Real32_Binom_Int64
END INTERFACE

INTERFACE Binom
  MODULE PROCEDURE Real32_Binom_Int8, Real32_Binom_Int16, &
    & Real32_Binom_Int32, Real32_Binom_Int64
END INTERFACE Binom

!----------------------------------------------------------------------------
!                                                                Binom
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE FUNCTION Real64_Binom_Int8( n, k, kind ) RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: n
  INTEGER( Int8 ), INTENT( IN ) :: k
  REAL( Real64 ) :: kind
  REAL( Real64 ) :: ans
END FUNCTION Real64_Binom_Int8
!!
MODULE RECURSIVE FUNCTION Real64_Binom_Int16( n, k, kind ) RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: n
  INTEGER( Int16 ), INTENT( IN ) :: k
  REAL( Real64 ) :: kind
  REAL( Real64 ) :: ans
END FUNCTION Real64_Binom_Int16
!!
MODULE RECURSIVE FUNCTION Real64_Binom_Int32( n, k, kind ) RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: n
  INTEGER( Int32 ), INTENT( IN ) :: k
  REAL( Real64 ) :: kind
  REAL( Real64 ) :: ans
END FUNCTION Real64_Binom_Int32
!!
MODULE RECURSIVE FUNCTION Real64_Binom_Int64( n, k, kind ) RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: n
  INTEGER( Int64 ), INTENT( IN ) :: k
  REAL( Real64 ) :: kind
  REAL( Real64 ) :: ans
END FUNCTION Real64_Binom_Int64
END INTERFACE

INTERFACE Binom
  MODULE PROCEDURE Real64_Binom_Int8, Real64_Binom_Int16, &
    & Real64_Binom_Int32, Real64_Binom_Int64
END INTERFACE Binom

END MODULE BinomUtility