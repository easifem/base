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

SUBMODULE(EyeUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE int_eye_1
INTEGER(I4B) :: i
Ans = 0_INT8
DO i = 1, m
  Ans(i, i) = 1
END DO
END PROCEDURE int_eye_1

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE int_eye_2
INTEGER(I4B) :: i
Ans = 0_INT16
DO i = 1, m
  Ans(i, i) = 1
END DO
END PROCEDURE int_eye_2

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE int_eye_3
INTEGER(I4B) :: i
Ans = 0_INT32
DO i = 1, m
  Ans(i, i) = 1
END DO
END PROCEDURE int_eye_3

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE int_eye_4
INTEGER(I4B) :: i
Ans = 0_INT64
DO i = 1, m
  Ans(i, i) = 1
END DO
END PROCEDURE int_eye_4

!----------------------------------------------------------------------------
!                                                                       Eye
!----------------------------------------------------------------------------

#ifdef USE_Int128
MODULE PROCEDURE int_eye_5
INTEGER(I4B) :: i
Ans = 0_INT128
DO i = 1, m
  Ans(i, i) = 1
END DO
END PROCEDURE int_eye_5
#endif

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE real_eye_1
INTEGER(I4B) :: i
Ans = 0.0
DO i = 1, m
  Ans(i, i) = 1.0
END DO
END PROCEDURE real_eye_1

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE real_eye_2
INTEGER(I4B) :: i
Ans = 0.0
DO i = 1, m
  Ans(i, i) = 1.0
END DO
END PROCEDURE real_eye_2

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE real_eye_3
INTEGER(I4B) :: i
Ans = 0.0
DO i = 1, m
  Ans(i, i) = 1.0
END DO
END PROCEDURE real_eye_3

END SUBMODULE Methods
