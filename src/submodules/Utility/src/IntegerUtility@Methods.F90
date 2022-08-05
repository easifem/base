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

SUBMODULE(IntegerUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        IN
!----------------------------------------------------------------------------

MODULE PROCEDURE in_1a
#include "./In/In_1.inc"
END PROCEDURE in_1a

MODULE PROCEDURE in_1b
#include "./In/In_1.inc"
END PROCEDURE in_1b

MODULE PROCEDURE in_1c
#include "./In/In_1.inc"
END PROCEDURE in_1c

MODULE PROCEDURE in_1d
#include "./In/In_1.inc"
END PROCEDURE in_1d

!----------------------------------------------------------------------------
!                                                                        isIN
!----------------------------------------------------------------------------

MODULE PROCEDURE IsIn_1a
#include "./In/IsIn_1.inc"
END PROCEDURE IsIn_1a

MODULE PROCEDURE IsIn_1b
#include "./In/IsIn_1.inc"
END PROCEDURE IsIn_1b

MODULE PROCEDURE IsIn_1c
#include "./In/IsIn_1.inc"
END PROCEDURE IsIn_1c

MODULE PROCEDURE IsIn_1d
#include "./In/IsIn_1.inc"
END PROCEDURE IsIn_1d

!----------------------------------------------------------------------------
!                                                                        IN
!----------------------------------------------------------------------------

MODULE PROCEDURE in_2a
  ans = ANY(a .EQ. b)
END PROCEDURE in_2a

MODULE PROCEDURE in_2b
  ans = ANY(a .EQ. b)
END PROCEDURE in_2b

MODULE PROCEDURE in_2c
  ans = ANY(a .EQ. b)
END PROCEDURE in_2c

MODULE PROCEDURE in_2d
  ans = ANY(a .EQ. b)
END PROCEDURE in_2d

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE RemoveDuplicates_1a
  INTEGER(Int8), ALLOCATABLE :: temp(:)
#include "./RemoveDuplicates/RemoveDuplicates_1.inc"
END PROCEDURE RemoveDuplicates_1a

MODULE PROCEDURE RemoveDuplicates_1b
  INTEGER(Int16), ALLOCATABLE :: temp(:)
#include "./RemoveDuplicates/RemoveDuplicates_1.inc"
END PROCEDURE RemoveDuplicates_1b

MODULE PROCEDURE RemoveDuplicates_1c
  INTEGER(Int32), ALLOCATABLE :: temp(:)
#include "./RemoveDuplicates/RemoveDuplicates_1.inc"
END PROCEDURE RemoveDuplicates_1c

MODULE PROCEDURE RemoveDuplicates_1d
  INTEGER(Int64), ALLOCATABLE :: temp(:)
#include "./RemoveDuplicates/RemoveDuplicates_1.inc"
END PROCEDURE RemoveDuplicates_1d

!----------------------------------------------------------------------------
!                                                                 Repeat
!----------------------------------------------------------------------------

MODULE PROCEDURE Repeat_1a
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1a

MODULE PROCEDURE Repeat_1b
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1b

MODULE PROCEDURE Repeat_1c
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1c

MODULE PROCEDURE Repeat_1d
#include "./Repeat/Repeat_1.inc"
END PROCEDURE Repeat_1d

END SUBMODULE Methods