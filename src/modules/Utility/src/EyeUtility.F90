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

MODULE EyeUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Return an identity matrix of an integers

INTERFACE
MODULE PURE FUNCTION int_eye_1( m, DataType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m, DataType
  INTEGER( I4B ) :: Ans( m, m )
END FUNCTION int_eye_1
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE int_eye_1
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return identity matrix of real numbers

MODULE PURE FUNCTION real_eye_1( m, DataType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m
  REAL( Real64 ) :: Ans( m, m )
  REAL( Real64 ), INTENT( IN ) :: DataType
END FUNCTION real_eye_1
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE real_eye_1
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return identity matrix of real number

MODULE PURE FUNCTION real_eye_2( m ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m
  REAL( DFP ) :: Ans( m, m )
END FUNCTION real_eye_2
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE real_eye_2
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Return identity matrix of real numbers

MODULE PURE FUNCTION real_eye_3( m, DataType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: m
  REAL( Real32 ) :: Ans( m, m )
  REAL( Real32 ), INTENT( IN ) :: DataType
END FUNCTION real_eye_3
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE real_eye_3
END INTERFACE Eye

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EyeUtility