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

!> author: Vikas Sharma, Ph. D.
! date:  22 March 2021
! summary: Methods for reallocating arrays

SUBMODULE(ReallocateUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_logical
#define ZEROVALUE .FALSE.
#include "./Reallocate/reallocate1.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_logical

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R1
#define ZEROVALUE 0.0_Real64
#include "./Reallocate/reallocate1.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real64_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R1b
CALL Reallocate_Real64_R1(mat, s(1), isExpand, expandFactor)
END PROCEDURE Reallocate_Real64_R1b

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R1
#define ZEROVALUE 0.0_Real32
#include "./Reallocate/reallocate1.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real32_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R1b
CALL Reallocate_Real32_R1(mat, s(1), isExpand, expandFactor)
END PROCEDURE Reallocate_Real32_R1b

!----------------------------------------------------------------------------
!                                                                 Reallocate1
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R2
#define ZEROVALUE 0.0_Real64
#include "./Reallocate/reallocate2.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real64_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R2b
CALL Reallocate_Real64_R2(mat, s(1), s(2), isExpand, expandFactor)
END PROCEDURE Reallocate_Real64_R2b

!----------------------------------------------------------------------------
!                                                                 Reallocate1
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R2
#define ZEROVALUE 0.0_Real32
#include "./Reallocate/reallocate2.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real32_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R2b
CALL Reallocate_Real32_R2(mat, s(1), s(2), isExpand, expandFactor)
END PROCEDURE Reallocate_Real32_R2b

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R3
#define ZEROVALUE 0.0_Real64
#include "./Reallocate/reallocate3.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real64_R3

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R3b
CALL Reallocate_Real64_R3(mat, s(1), s(2), s(3), isExpand, expandFactor)
END PROCEDURE Reallocate_Real64_R3b

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R3
#define ZEROVALUE 0.0_Real32
#include "./Reallocate/reallocate3.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real32_R3

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R3b
CALL Reallocate_Real32_R3(mat, s(1), s(2), s(3), isExpand, expandFactor)
END PROCEDURE Reallocate_Real32_R3b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R4
#define ZEROVALUE 0.0_Real64
#include "./Reallocate/reallocate4.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real64_R4

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R4b
CALL Reallocate_Real64_R4(mat, s(1), s(2), s(3), s(4), isExpand, expandFactor)
END PROCEDURE Reallocate_Real64_R4b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R4
#define ZEROVALUE 0.0_Real32
#include "./Reallocate/reallocate4.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real32_R4

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R4b
CALL Reallocate_Real32_R4(mat, s(1), s(2), s(3), s(4), isExpand, expandFactor)
END PROCEDURE Reallocate_Real32_R4b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R5
#define ZEROVALUE 0.0_Real64
#include "./Reallocate/reallocate5.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real64_R5

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R5b
CALL Reallocate_Real64_R5(mat, s(1), s(2), s(3), s(4), s(5), &
                          isExpand, expandFactor)
END PROCEDURE Reallocate_Real64_R5b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R5
#define ZEROVALUE 0.0_Real32
#include "./Reallocate/reallocate5.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real32_R5

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R5b
CALL Reallocate_Real32_R5(mat, s(1), s(2), s(3), s(4), s(5), &
                          isExpand, expandFactor)
END PROCEDURE Reallocate_Real32_R5b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R6
#define ZEROVALUE 0.0_Real64
#include "./Reallocate/reallocate6.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real64_R6

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R6b
CALL Reallocate_Real64_R6(mat, s(1), s(2), s(3), s(4), s(5), s(6), &
                          isExpand, expandFactor)
END PROCEDURE Reallocate_Real64_R6b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R6
#define ZEROVALUE 0.0_Real32
#include "./Reallocate/reallocate6.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real32_R6

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R6b
CALL Reallocate_Real32_R6(mat, s(1), s(2), s(3), s(4), s(5), s(6), &
                          isExpand, expandFactor)
END PROCEDURE Reallocate_Real32_R6b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R7
#define ZEROVALUE 0.0_Real64
#include "./Reallocate/reallocate7.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real64_R7

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R7b
CALL Reallocate_Real64_R7(mat, s(1), s(2), s(3), s(4), s(5), s(6), s(7), &
                          isExpand, expandFactor)
END PROCEDURE Reallocate_Real64_R7b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R7
#define ZEROVALUE 0.0_Real32
#include "./Reallocate/reallocate7.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Real32_R7

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R7b
CALL Reallocate_Real32_R7(mat, s(1), s(2), s(3), s(4), s(5), s(6), s(7), &
                          isExpand, expandFactor)
END PROCEDURE Reallocate_Real32_R7b

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R1
#define ZEROVALUE 0_Int64
#include "./Reallocate/reallocate1.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int64_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R1b
CALL Reallocate_Int64_R1(mat, s(1), isExpand, expandFactor)
END PROCEDURE Reallocate_Int64_R1b

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R1
#define ZEROVALUE 0_Int32
#include "./Reallocate/reallocate1.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int32_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R1b
CALL Reallocate_Int32_R1(mat, s(1), isExpand, expandFactor)
END PROCEDURE Reallocate_Int32_R1b

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int16_R1
#define ZEROVALUE 0_Int16
#include "./Reallocate/reallocate1.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int16_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int16_R1b
CALL Reallocate_Int16_R1(mat, s(1), isExpand, expandFactor)
END PROCEDURE Reallocate_Int16_R1b

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int8_R1
#define ZEROVALUE 0_Int8
#include "./Reallocate/reallocate1.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int8_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int8_R1b
CALL Reallocate_Int8_R1(mat, s(1), isExpand, expandFactor)
END PROCEDURE Reallocate_Int8_R1b

!----------------------------------------------------------------------------
!                                                                 Reallocate1
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R2
#define ZEROVALUE 0_Int64
#include "./Reallocate/reallocate2.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int64_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R2b
CALL Reallocate_Int64_R2(mat, s(1), s(2), isExpand, expandFactor)
END PROCEDURE Reallocate_Int64_R2b

!----------------------------------------------------------------------------
!                                                                 Reallocate1
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R2
#define ZEROVALUE 0_Int32
#include "./Reallocate/reallocate2.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int32_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R2b
CALL Reallocate_Int32_R2(mat, s(1), s(2), isExpand, expandFactor)
END PROCEDURE Reallocate_Int32_R2b

!----------------------------------------------------------------------------
!                                                                 Reallocate1
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int16_R2
#define ZEROVALUE 0_Int16
#include "./Reallocate/reallocate2.F90"
#undef  ZEROVALUE
END PROCEDURE Reallocate_Int16_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int16_R2b
CALL Reallocate_Int16_R2(mat, s(1), s(2), isExpand, expandFactor)
END PROCEDURE Reallocate_Int16_R2b

!----------------------------------------------------------------------------
!                                                                 Reallocate1
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int8_R2
#define ZEROVALUE 0_Int8
#include "./Reallocate/reallocate2.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int8_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int8_R2b
CALL Reallocate_Int8_R2(mat, s(1), s(2), isExpand, expandFactor)
END PROCEDURE Reallocate_Int8_R2b

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R3
#define ZEROVALUE 0_Int64
#include "./Reallocate/reallocate3.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int64_R3

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R3b
CALL Reallocate_Int64_R3(mat, s(1), s(2), s(3), isExpand, expandFactor)
END PROCEDURE Reallocate_Int64_R3b

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R3
#define ZEROVALUE 0_Int32
#include "./Reallocate/reallocate3.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int32_R3

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R3b
CALL Reallocate_Int32_R3(mat, s(1), s(2), s(3), isExpand, expandFactor)
END PROCEDURE Reallocate_Int32_R3b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R4
#define ZEROVALUE 0_Int64
#include "./Reallocate/reallocate4.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int64_R4

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R4b
CALL Reallocate_Int64_R4(mat, s(1), s(2), s(3), s(4), isExpand, expandFactor)
END PROCEDURE Reallocate_Int64_R4b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R4
#define ZEROVALUE 0_Int32
#include "./Reallocate/reallocate4.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int32_R4

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R4b
CALL Reallocate_Int32_R4(mat, s(1), s(2), s(3), s(4), isExpand, expandFactor)
END PROCEDURE Reallocate_Int32_R4b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R5
#define ZEROVALUE 0_Int64
#include "./Reallocate/reallocate5.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int64_R5

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R5b
CALL Reallocate_Int64_R5(mat, s(1), s(2), s(3), s(4), s(5), isExpand, &
                         expandFactor)
END PROCEDURE Reallocate_Int64_R5b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R5
#define ZEROVALUE 0_Int32
#include "./Reallocate/reallocate5.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int32_R5

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R5b
CALL Reallocate_Int32_R5(mat, s(1), s(2), s(3), s(4), s(5), &
                         isExpand, expandFactor)
END PROCEDURE Reallocate_Int32_R5b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R6
#define ZEROVALUE 0.0_Int64
#include "./Reallocate/reallocate6.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int64_R6

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R6b
CALL Reallocate_Int64_R6(mat, s(1), s(2), s(3), s(4), s(5), s(6), &
                         isExpand, expandFactor)
END PROCEDURE Reallocate_Int64_R6b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R6
#define ZEROVALUE 0.0_Int32
#include "./Reallocate/reallocate6.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int32_R6

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R6b
CALL Reallocate_Int32_R6(mat, s(1), s(2), s(3), s(4), s(5), s(6), &
                         isExpand, expandFactor)
END PROCEDURE Reallocate_Int32_R6b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R7
#define ZEROVALUE 0.0_Int64
#include "./Reallocate/reallocate7.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int64_R7

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R7b
CALL Reallocate_Int64_R7(mat, s(1), s(2), s(3), s(4), s(5), s(6), s(7), &
                         isExpand, expandFactor)
END PROCEDURE Reallocate_Int64_R7b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R7
#define ZEROVALUE 0.0_Int32
#include "./Reallocate/reallocate7.F90"
#undef ZEROVALUE
END PROCEDURE Reallocate_Int32_R7

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R7b
CALL Reallocate_Int32_R7(mat, s(1), s(2), s(3), s(4), s(5), s(6), s(7), &
                         isExpand, expandFactor)
END PROCEDURE Reallocate_Int32_R7b

!----------------------------------------------------------------------------
!                                                                 Reallocate6
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R1_6
#define ZERO1 0_I4B
#define ZERO2 0_I4B
#define ZERO3 0_I4B
#define ZERO4 0_I4B
#define ZERO5 0_I4B
#define ZERO6 0_I4B
#include "./Reallocate/reallocate8.F90"
#undef ZERO1
#undef ZERO2
#undef ZERO3
#undef ZERO4
#undef ZERO5
#undef ZERO6
END PROCEDURE Reallocate_Int32_R1_6

!----------------------------------------------------------------------------
!                                                                 Reallocate7
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R1_6
#define ZERO1 0.0_Real64
#define ZERO2 0.0_Real64
#define ZERO3 0.0_Real64
#define ZERO4 0.0_Real64
#define ZERO5 0.0_Real64
#define ZERO6 0.0_Real64
#include "./Reallocate/reallocate8.F90"
#undef ZERO1
#undef ZERO2
#undef ZERO3
#undef ZERO4
#undef ZERO5
#undef ZERO6
END PROCEDURE Reallocate_Real64_R1_6

!----------------------------------------------------------------------------
!                                                                 Reallocate7
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R1_6
#define ZERO1 0.0_Real32
#define ZERO2 0.0_Real32
#define ZERO3 0.0_Real32
#define ZERO4 0.0_Real32
#define ZERO5 0.0_Real32
#define ZERO6 0.0_Real32
#include "./Reallocate/reallocate8.F90"
#undef ZERO1
#undef ZERO2
#undef ZERO3
#undef ZERO4
#undef ZERO5
#undef ZERO6
END PROCEDURE Reallocate_Real32_R1_6

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_AIJ
#include "./Reallocate/reallocate9.F90"
END PROCEDURE Reallocate_Real64_AIJ

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_AIJ
#include "./Reallocate/reallocate9.F90"
END PROCEDURE Reallocate_Real32_AIJ

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_AI
#include "./Reallocate/reallocate10.F90"
END PROCEDURE Reallocate_Real64_AI

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_AI
#include "./Reallocate/reallocate10.F90"
END PROCEDURE Reallocate_Real32_AI

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
