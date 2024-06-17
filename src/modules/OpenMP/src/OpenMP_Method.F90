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
! date:         9 March 2021
! summary:         This module contains openmp methods

MODULE OpenMP_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: OMP_Initiate
PUBLIC :: OMP_Finalize
PUBLIC :: OMP_Partition

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE OMP_Initiate
  MODULE SUBROUTINE obj_initiate
  END SUBROUTINE obj_initiate
END INTERFACE OMP_Initiate

!----------------------------------------------------------------------------
!                                                       Finalize@Constructor
!----------------------------------------------------------------------------

INTERFACE OMP_Finalize
  MODULE SUBROUTINE obj_finalize
  END SUBROUTINE obj_finalize
END INTERFACE OMP_Finalize

!----------------------------------------------------------------------------
!                                                      Partition@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         9 March 2021
! summary:         This function partition a vector for omp
!
!# Introduction
!
! This function partition a vector for [[OpenMP_]],
! and returns a vector of length 4 (i.e., Ans)
!
! * Ans( 1 ) = istart
! * Ans( 2 ) = iend
! * Ans( 3 ) = stride
! * Ans( 4 ) = Length
!
!
!### Usage
!
!```fortran
! to do
!```

INTERFACE OMP_Partition
  MODULE FUNCTION obj_partition_vec(N, OMP_NUM_THREADS) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: N
    INTEGER(I4B), INTENT(IN) :: OMP_NUM_THREADS
    INTEGER(I4B) :: Ans(4)
  END FUNCTION obj_partition_vec
END INTERFACE OMP_Partition

END MODULE OpenMP_Method
