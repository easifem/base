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
PUBLIC :: ones
!
!----------------------------------------------------------------------------
!                                                     Ones@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ones_1( dim1, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(Int8), INTENT(IN) :: datatype
  INTEGER(Int8) :: ans(dim1)
END FUNCTION ones_1
!!
MODULE PURE FUNCTION ones_2( dim1, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(Int16), INTENT(IN) :: datatype
  INTEGER(Int16) :: ans(dim1)
END FUNCTION ones_2
!!
MODULE PURE FUNCTION ones_3( dim1, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(Int32), INTENT(IN) :: datatype
  INTEGER(Int32) :: ans(dim1)
END FUNCTION ones_3
!!
MODULE PURE FUNCTION ones_4( dim1, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(Int64), INTENT(IN) :: datatype
  INTEGER(Int64) :: ans(dim1)
END FUNCTION ones_4

#ifdef USE_Int128
!!
MODULE PURE FUNCTION ones_5( dim1, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(Int128), INTENT(IN) :: datatype
  INTEGER(Int128) :: ans(dim1)
END FUNCTION ones_5
#endif
END INTERFACE

INTERFACE ones
  MODULE PROCEDURE ones_1, ones_2, ones_3, ones_4
END INTERFACE ones

#ifdef USE_Int128
INTERFACE ones
  MODULE PROCEDURE ones_5
END INTERFACE ones
#endif

!----------------------------------------------------------------------------
!                                                     Ones@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ones_6( dim1, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  REAL(Real32), INTENT(IN) :: datatype
  REAL(Real32) :: ans(dim1)
END FUNCTION ones_6
!!
MODULE PURE FUNCTION ones_7( dim1, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  REAL(Real64), INTENT(IN) :: datatype
  REAL(Real64) :: ans(dim1)
END FUNCTION ones_7
END INTERFACE

INTERFACE ones
  MODULE PROCEDURE ones_6, ones_7
END INTERFACE ones

!----------------------------------------------------------------------------
!                                                     Ones@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ones_8( dim1, dim2, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(Int8), INTENT(IN) :: datatype
  INTEGER(Int8) :: ans(dim1, dim2)
END FUNCTION ones_8
!!
MODULE PURE FUNCTION ones_9( dim1, dim2, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(Int16), INTENT(IN) :: datatype
  INTEGER(Int16) :: ans(dim1, dim2)
END FUNCTION ones_9
!!
MODULE PURE FUNCTION ones_10( dim1, dim2, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(Int32), INTENT(IN) :: datatype
  INTEGER(Int32) :: ans(dim1, dim2)
END FUNCTION ones_10
!!
MODULE PURE FUNCTION ones_11( dim1, dim2, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(Int64), INTENT(IN) :: datatype
  INTEGER(Int64) :: ans(dim1, dim2)
END FUNCTION ones_11
!!
#ifdef USE_Int128
!!
MODULE PURE FUNCTION ones_12( dim1, dim2, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(Int128), INTENT(IN) :: datatype
  INTEGER(Int128) :: ans(dim1, dim2)
END FUNCTION ones_12
#endif
!!
END INTERFACE

INTERFACE ones
  MODULE PROCEDURE ones_8, ones_9, ones_10, ones_11
END INTERFACE ones

#ifdef USE_Int128
INTERFACE ones
  MODULE PROCEDURE ones_12
END INTERFACE ones
#endif

!----------------------------------------------------------------------------
!                                                     Ones@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ones_13( dim1, dim2, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  REAL(Real32), INTENT(IN) :: datatype
  REAL(Real32) :: ans(dim1, dim2)
END FUNCTION ones_13
!!
MODULE PURE FUNCTION ones_14( dim1, dim2, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  REAL(Real64), INTENT(IN) :: datatype
  REAL(Real64) :: ans(dim1, dim2)
END FUNCTION ones_14
END INTERFACE

INTERFACE ones
  MODULE PROCEDURE ones_13, ones_14
END INTERFACE ones

!----------------------------------------------------------------------------
!                                                     Ones@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ones_15( dim1, dim2, dim3, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(Int8), INTENT(IN) :: datatype
  INTEGER(Int8) :: ans(dim1, dim2, dim3)
END FUNCTION ones_15
!!
MODULE PURE FUNCTION ones_16( dim1, dim2, dim3, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(Int16), INTENT(IN) :: datatype
  INTEGER(Int16) :: ans(dim1, dim2, dim3)
END FUNCTION ones_16
!!
MODULE PURE FUNCTION ones_17( dim1, dim2, dim3, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(Int32), INTENT(IN) :: datatype
  INTEGER(Int32) :: ans(dim1, dim2, dim3)
END FUNCTION ones_17
!!
MODULE PURE FUNCTION ones_18( dim1, dim2, dim3, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(Int64), INTENT(IN) :: datatype
  INTEGER(Int64) :: ans(dim1, dim2, dim3)
END FUNCTION ones_18

#ifdef USE_Int128
  !!
MODULE PURE FUNCTION ones_19( dim1, dim2, dim3, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(Int128), INTENT(IN) :: datatype
  INTEGER(Int128) :: ans(dim1, dim2, dim3)
END FUNCTION ones_19
#endif
END INTERFACE

INTERFACE ones
  MODULE PROCEDURE ones_15, ones_16, ones_17, ones_18
END INTERFACE ones

#ifdef USE_Int128
INTERFACE ones
  MODULE PROCEDURE ones_19
END INTERFACE ones
#endif

!----------------------------------------------------------------------------
!                                                     Ones@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ones_20( dim1, dim2, dim3, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  REAL(Real32), INTENT(IN) :: datatype
  REAL(Real32) :: ans(dim1, dim2, dim3)
END FUNCTION ones_20
!!
MODULE PURE FUNCTION ones_21( dim1, dim2, dim3, datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  REAL(Real64), INTENT(IN) :: datatype
  REAL(Real64) :: ans(dim1, dim2, dim3)
END FUNCTION ones_21
END INTERFACE

INTERFACE ones
  MODULE PROCEDURE ones_20, ones_21
END INTERFACE ones

!----------------------------------------------------------------------------
!                                                     Ones@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ones_22( dim1, dim2, dim3, dim4,&
    & datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(I4B), INTENT(IN) :: dim4
  INTEGER(Int8), INTENT(IN) :: datatype
  INTEGER(Int8) :: ans(dim1, dim2, dim3, dim4)
END FUNCTION ones_22
!!
MODULE PURE FUNCTION ones_23( dim1, dim2, dim3, dim4,&
    & datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(I4B), INTENT(IN) :: dim4
  INTEGER(Int16), INTENT(IN) :: datatype
  INTEGER(Int16) :: ans(dim1, dim2, dim3, dim4)
END FUNCTION ones_23
!!
MODULE PURE FUNCTION ones_24( dim1, dim2, dim3, dim4,&
    & datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(I4B), INTENT(IN) :: dim4
  INTEGER(Int32), INTENT(IN) :: datatype
  INTEGER(Int32) :: ans(dim1, dim2, dim3, dim4)
END FUNCTION ones_24
!!
MODULE PURE FUNCTION ones_25( dim1, dim2, dim3, dim4,&
    & datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(I4B), INTENT(IN) :: dim4
  INTEGER(Int64), INTENT(IN) :: datatype
  INTEGER(Int64) :: ans(dim1, dim2, dim3, dim4)
END FUNCTION ones_25

#ifdef USE_Int128
!!
MODULE PURE FUNCTION ones_26( dim1, dim2, dim3, dim4, &
    & datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(I4B), INTENT(IN) :: dim4
  INTEGER(Int128), INTENT(IN) :: datatype
  INTEGER(Int128) :: ans(dim1, dim2, dim3, dim4)
END FUNCTION ones_26
#endif
END INTERFACE

INTERFACE ones
  MODULE PROCEDURE ones_22, ones_23, ones_24, ones_25
END INTERFACE ones

#ifdef USE_Int128
INTERFACE ones
  MODULE PROCEDURE ones_26
END INTERFACE ones
#endif

!----------------------------------------------------------------------------
!                                                     Ones@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ones_27( dim1, dim2, dim3, dim4, &
    & datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(I4B), INTENT(IN) :: dim4
  REAL(Real32), INTENT(IN) :: datatype
  REAL(Real32) :: ans(dim1, dim2, dim3, dim4)
END FUNCTION ones_27
!!
MODULE PURE FUNCTION ones_28( dim1, dim2, dim3, dim4, &
    & datatype ) RESULT( Ans )
  INTEGER(I4B), INTENT(IN) :: dim1
  INTEGER(I4B), INTENT(IN) :: dim2
  INTEGER(I4B), INTENT(IN) :: dim3
  INTEGER(I4B), INTENT(IN) :: dim4
  REAL(Real64), INTENT(IN) :: datatype
  REAL(Real64) :: ans(dim1, dim2, dim3, dim4)
END FUNCTION ones_28
END INTERFACE

INTERFACE ones
  MODULE PROCEDURE ones_27, ones_28
END INTERFACE ones
