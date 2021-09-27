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

SUBMODULE( OpenMP_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_initiate
  OMP%State = OMP_THREADS_FORKED
  OMP%IS_INIT = .TRUE.
  OMP%DID_I_INIT = .TRUE.
  !$ OMP%MAX_THREADS = omp_get_max_threads()
  !$ OMP%NUM_THREADS = omp_get_num_threads()
  !$ OMP%Rank = omp_get_thread_num()
END PROCEDURE obj_initiate

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_finalize
  OMP%State = OMP_THREADS_JOINED
  OMP%IS_INIT = .FALSE.
  OMP%DID_I_INIT = .FALSE.
  OMP%MAX_THREADS = 1
  OMP%NUM_THREADS = 1
  OMP%Rank = 0
END PROCEDURE obj_finalize

!----------------------------------------------------------------------------
!                                                                 Partition
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_partition_vec
  INTEGER( I4B ) :: chunck
  chunck = INT( N/OMP_NUM_THREADS, KIND=I4B )

  IF( chunck .NE. 0 ) THEN
    IF( OMP%RANK .EQ. OMP_NUM_THREADS-1 ) THEN
      Ans = [(chunck*OMP%RANK) + 1, N, 1, N-chunck*OMP%RANK]
    ELSE
      Ans = [(chunck*OMP%RANK) + 1, chunck*(OMP%RANK + 1), 1, chunck]
    END IF
  ELSE
    IF( OMP%RANK .EQ. 0 ) THEN
      Ans = [1, N, 1, N]
    ELSE
      Ans = [0,0,1,0]
    END IF
  END IF
END PROCEDURE obj_partition_vec

END SUBMODULE Constructor