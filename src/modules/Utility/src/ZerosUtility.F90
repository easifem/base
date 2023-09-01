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

MODULE ZerosUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Zeros

!----------------------------------------------------------------------------
!                                                     Zeros@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Zeros_1(dim1, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(dim1)
  END FUNCTION Zeros_1

  MODULE PURE FUNCTION Zeros_2(dim1, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(dim1)
  END FUNCTION Zeros_2

  MODULE PURE FUNCTION Zeros_3(dim1, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(dim1)
  END FUNCTION Zeros_3

  MODULE PURE FUNCTION Zeros_4(dim1, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(dim1)
  END FUNCTION Zeros_4

#ifdef USE_Int128
  MODULE PURE FUNCTION Zeros_5(dim1, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1)
  END FUNCTION Zeros_5
#endif
END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_1, Zeros_2, Zeros_3, Zeros_4
END INTERFACE Zeros

#ifdef USE_Int128
INTERFACE Zeros
  MODULE PROCEDURE Zeros_5
END INTERFACE Zeros
#endif

!----------------------------------------------------------------------------
!                                                     Zeros@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Zeros_6(dim1, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(dim1)
  END FUNCTION Zeros_6
!!
  MODULE PURE FUNCTION Zeros_7(dim1, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(dim1)
  END FUNCTION Zeros_7
END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_6, Zeros_7
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                     Zeros@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Zeros_8(dim1, dim2, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(dim1, dim2)
  END FUNCTION Zeros_8
!!
  MODULE PURE FUNCTION Zeros_9(dim1, dim2, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(dim1, dim2)
  END FUNCTION Zeros_9
!!
  MODULE PURE FUNCTION Zeros_10(dim1, dim2, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(dim1, dim2)
  END FUNCTION Zeros_10
!!
  MODULE PURE FUNCTION Zeros_11(dim1, dim2, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(dim1, dim2)
  END FUNCTION Zeros_11
!!
#ifdef USE_Int128
!!
  MODULE PURE FUNCTION Zeros_12(dim1, dim2, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2)
  END FUNCTION Zeros_12
#endif
!!
END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_8, Zeros_9, Zeros_10, Zeros_11
END INTERFACE Zeros

#ifdef USE_Int128
INTERFACE Zeros
  MODULE PROCEDURE Zeros_12
END INTERFACE Zeros
#endif

!----------------------------------------------------------------------------
!                                                     Zeros@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Zeros_13(dim1, dim2, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(dim1, dim2)
  END FUNCTION Zeros_13
!!
  MODULE PURE FUNCTION Zeros_14(dim1, dim2, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(dim1, dim2)
  END FUNCTION Zeros_14
END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_13, Zeros_14
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                     Zeros@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Zeros_15(dim1, dim2, dim3, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_15
!!
  MODULE PURE FUNCTION Zeros_16(dim1, dim2, dim3, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_16
!!
  MODULE PURE FUNCTION Zeros_17(dim1, dim2, dim3, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_17
!!
  MODULE PURE FUNCTION Zeros_18(dim1, dim2, dim3, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_18

#ifdef USE_Int128
  !!
  MODULE PURE FUNCTION Zeros_19(dim1, dim2, dim3, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_19
#endif
END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_15, Zeros_16, Zeros_17, Zeros_18
END INTERFACE Zeros

#ifdef USE_Int128
INTERFACE Zeros
  MODULE PROCEDURE Zeros_19
END INTERFACE Zeros
#endif

!----------------------------------------------------------------------------
!                                                     Zeros@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Zeros_20(dim1, dim2, dim3, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_20
!!
  MODULE PURE FUNCTION Zeros_21(dim1, dim2, dim3, datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(dim1, dim2, dim3)
  END FUNCTION Zeros_21
END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_20, Zeros_21
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                     Zeros@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Zeros_22(dim1, dim2, dim3, dim4,&
      & datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_22
!!
  MODULE PURE FUNCTION Zeros_23(dim1, dim2, dim3, dim4,&
      & datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_23
!!
  MODULE PURE FUNCTION Zeros_24(dim1, dim2, dim3, dim4,&
      & datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_24
!!
  MODULE PURE FUNCTION Zeros_25(dim1, dim2, dim3, dim4,&
      & datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_25

#ifdef USE_Int128
!!
  MODULE PURE FUNCTION Zeros_26(dim1, dim2, dim3, dim4, &
      & datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    INTEGER(Int128), INTENT(IN) :: datatype
    INTEGER(Int128) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_26
#endif
END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_22, Zeros_23, Zeros_24, Zeros_25
END INTERFACE Zeros

#ifdef USE_Int128
INTERFACE Zeros
  MODULE PROCEDURE Zeros_26
END INTERFACE Zeros
#endif

!----------------------------------------------------------------------------
!                                                     Zeros@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_27(dim1, dim2, dim3, dim4, &
      & datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_27
!!
  MODULE PURE FUNCTION Zeros_28(dim1, dim2, dim3, dim4, &
      & datatype) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: dim3
    INTEGER(I4B), INTENT(IN) :: dim4
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(dim1, dim2, dim3, dim4)
  END FUNCTION Zeros_28
END INTERFACE Zeros

!----------------------------------------------------------------------------
!                                                         Zeros
!----------------------------------------------------------------------------

INTERFACE Zeros
  MODULE PURE FUNCTION Zeros_29_Int8(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    INTEGER(INT8), INTENT(IN) :: datatype
    INTEGER(INT8) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Int8

  MODULE PURE FUNCTION Zeros_29_Int16(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    INTEGER(INT16), INTENT(IN) :: datatype
    INTEGER(INT16) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Int16

  MODULE PURE FUNCTION Zeros_29_Int32(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    INTEGER(INT32), INTENT(IN) :: datatype
    INTEGER(INT32) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Int32

  MODULE PURE FUNCTION Zeros_29_Int64(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    INTEGER(INT64), INTENT(IN) :: datatype
    INTEGER(INT64) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Int64

  MODULE PURE FUNCTION Zeros_29_Real32(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    REAL(REAL32), INTENT(IN) :: datatype
    REAL(REAL32) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Real32

  MODULE PURE FUNCTION Zeros_29_Real64(s, datatype) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: s(:)
    REAL(REAL64), INTENT(IN) :: datatype
    REAL(REAL64) :: ans(s(1), s(2))
  END FUNCTION Zeros_29_Real64
END INTERFACE Zeros

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ZerosUtility
