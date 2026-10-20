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

SUBMODULE(SplitUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_Int8
IF (section == 1) THEN
  ans = x(1:SIZE(x) / 2)
ELSE IF (section == 2) THEN
  ans = x(SIZE(x) / 2 + 1:)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE split_Int8

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_Int16
IF (section == 1) THEN
  ans = x(1:SIZE(x) / 2)
ELSE IF (section == 2) THEN
  ans = x(SIZE(x) / 2 + 1:)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE split_Int16

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_Int32
IF (section == 1) THEN
  ans = x(1:SIZE(x) / 2)
ELSE IF (section == 2) THEN
  ans = x(SIZE(x) / 2 + 1:)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE split_Int32

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_Int64
IF (section == 1) THEN
  ans = x(1:SIZE(x) / 2)
ELSE IF (section == 2) THEN
  ans = x(SIZE(x) / 2 + 1:)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE split_Int64

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_Real32
IF (section == 1) THEN
  ans = x(1:SIZE(x) / 2)
ELSE IF (section == 2) THEN
  ans = x(SIZE(x) / 2 + 1:)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE split_Real32

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_Real64
IF (section == 1) THEN
  ans = x(1:SIZE(x) / 2)
ELSE IF (section == 2) THEN
  ans = x(SIZE(x) / 2 + 1:)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE split_Real64

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_char
IF (section == 1) THEN
  ans = x(1:LEN(x) / 2)
ELSE IF (section == 2) THEN
  ans = x(LEN(x) / 2 + 1:)
ELSE
  ans = ''
END IF
END PROCEDURE split_char

END SUBMODULE Methods
