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

SUBMODULE(MedianUtility) Methods
USE BaseMethod, ONLY: SWAP
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Median
!----------------------------------------------------------------------------

MODULE PROCEDURE Median_Real32
#include "./Median/Median.inc"
END PROCEDURE Median_Real32

!----------------------------------------------------------------------------
!                                                                    Median
!----------------------------------------------------------------------------

MODULE PROCEDURE Median_Real64
#include "./Median/Median.inc"
END PROCEDURE Median_Real64

!----------------------------------------------------------------------------
!                                                                    Median
!----------------------------------------------------------------------------

MODULE PROCEDURE Median_Int8
#include "./Median/Median.inc"
END PROCEDURE Median_Int8

!----------------------------------------------------------------------------
!                                                                    Median
!----------------------------------------------------------------------------

MODULE PROCEDURE Median_Int16
#include "./Median/Median.inc"
END PROCEDURE Median_Int16

!----------------------------------------------------------------------------
!                                                                    Median
!----------------------------------------------------------------------------

MODULE PROCEDURE Median_Int32
#include "./Median/Median.inc"
END PROCEDURE Median_Int32

!----------------------------------------------------------------------------
!                                                                    Median
!----------------------------------------------------------------------------

MODULE PROCEDURE Median_Int64
#include "./Median/Median.inc"
END PROCEDURE Median_Int64

!----------------------------------------------------------------------------
!                                                                    ArgMedian
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgMedian_Real32
#include "./Median/ArgMedian.inc"
END PROCEDURE ArgMedian_Real32

!----------------------------------------------------------------------------
!                                                                    ArgMedian
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgMedian_Real64
#include "./Median/ArgMedian.inc"
END PROCEDURE ArgMedian_Real64

!----------------------------------------------------------------------------
!                                                                    ArgMedian
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgMedian_Int8
#include "./Median/ArgMedian.inc"
END PROCEDURE ArgMedian_Int8

!----------------------------------------------------------------------------
!                                                                    ArgMedian
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgMedian_Int16
#include "./Median/ArgMedian.inc"
END PROCEDURE ArgMedian_Int16

!----------------------------------------------------------------------------
!                                                                    ArgMedian
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgMedian_Int32
#include "./Median/ArgMedian.inc"
END PROCEDURE ArgMedian_Int32

!----------------------------------------------------------------------------
!                                                                    ArgMedian
!----------------------------------------------------------------------------

MODULE PROCEDURE ArgMedian_Int64
#include "./Median/ArgMedian.inc"
END PROCEDURE ArgMedian_Int64

END SUBMODULE
