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

SUBMODULE(PushPopUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE push_int8
#include "./PushPop/Push_Scalar.inc"
END PROCEDURE push_int8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE push_int16
#include "./PushPop/Push_Scalar.inc"
END PROCEDURE push_int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE push_int32
#include "./PushPop/Push_Scalar.inc"
END PROCEDURE push_int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE push_int64
#include "./PushPop/Push_Scalar.inc"
END PROCEDURE push_int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE push_real32
#include "./PushPop/Push_Scalar.inc"
END PROCEDURE push_real32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE push_real64
#include "./PushPop/Push_Scalar.inc"
END PROCEDURE push_real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pop_int8
#include "./PushPop/Pop_Scalar.inc"
END PROCEDURE pop_int8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pop_int16
#include "./PushPop/Pop_Scalar.inc"
END PROCEDURE pop_int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pop_int32
#include "./PushPop/Pop_Scalar.inc"
END PROCEDURE pop_int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pop_int64
#include "./PushPop/Pop_Scalar.inc"
END PROCEDURE pop_int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pop_real32
#include "./PushPop/Pop_Scalar.inc"
END PROCEDURE pop_real32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pop_real64
#include "./PushPop/Pop_Scalar.inc"
END PROCEDURE pop_real64

END SUBMODULE Methods
