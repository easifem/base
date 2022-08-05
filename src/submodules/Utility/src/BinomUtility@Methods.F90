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

SUBMODULE(BinomUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Real32_Binom_Int8
  !!
  IF( k .EQ. 0_Int8 ) THEN
    ans = 1.0_Real32
  ELSE
    ans = REAL(n - k + 1, KIND=Real32) / REAL(k, KIND=Real32) * &
      & Real32_Binom_Int8( n=n, k=k-1_Int8, kind=kind )
  END IF
  !!
END PROCEDURE Real32_Binom_Int8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Real32_Binom_Int16
  !!
  IF( k .EQ. 0_Int16 ) THEN
    ans = 1.0_Real32
  ELSE
    ans = REAL(n - k + 1, KIND=Real32) / REAL(k, KIND=Real32) * &
      & Real32_Binom_Int16( n=n, k=k-1_Int16, kind=kind )
  END IF
  !!
END PROCEDURE Real32_Binom_Int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Real32_Binom_Int32
  !!
  IF( k .EQ. 0_Int32 ) THEN
    ans = 1.0_Real32
  ELSE
    ans = REAL(n - k + 1, KIND=Real32) / REAL(k, KIND=Real32) * &
      & Real32_Binom_Int32( n=n, k=k-1_Int32, kind=kind )
  END IF
  !!
END PROCEDURE Real32_Binom_Int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Real32_Binom_Int64
  !!
  IF( k .EQ. 0_Int64 ) THEN
    ans = 1.0_Real32
  ELSE
    ans = REAL(n - k + 1, KIND=Real32) / REAL(k, KIND=Real32) * &
      & Real32_Binom_Int64( n=n, k=k-1_Int64, kind=kind )
  END IF
  !!
END PROCEDURE Real32_Binom_Int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Real64_Binom_Int8
  !!
  IF( k .EQ. 0_Int8 ) THEN
    ans = 1.0_Real64
  ELSE
    ans = REAL(n - k + 1, KIND=Real64) / REAL(k, KIND=Real64) * &
      & Real64_Binom_Int8( n=n, k=k-1_Int8, kind=kind )
  END IF
  !!
END PROCEDURE Real64_Binom_Int8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Real64_Binom_Int16
  !!
  IF( k .EQ. 0_Int16 ) THEN
    ans = 1.0_Real64
  ELSE
    ans = REAL(n - k + 1, KIND=Real64) / REAL(k, KIND=Real64) * &
      & Real64_Binom_Int16( n=n, k=k-1_Int16, kind=kind )
  END IF
  !!
END PROCEDURE Real64_Binom_Int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Real64_Binom_Int32
  !!
  IF( k .EQ. 0_Int32 ) THEN
    ans = 1.0_Real64
  ELSE
    ans = REAL(n - k + 1, KIND=Real64) / REAL(k, KIND=Real64) * &
      & Real64_Binom_Int32( n=n, k=k-1_Int32, kind=kind )
  END IF
  !!
END PROCEDURE Real64_Binom_Int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Real64_Binom_Int64
  !!
  IF( k .EQ. 0_Int64 ) THEN
    ans = 1.0_Real64
  ELSE
    ans = REAL(n - k + 1, KIND=Real64) / REAL(k, KIND=Real64) * &
      & Real64_Binom_Int64( n=n, k=k-1_Int64, kind=kind )
  END IF
  !!
END PROCEDURE Real64_Binom_Int64

END SUBMODULE Methods