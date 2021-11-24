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


!----------------------------------------------------------------------------
!                                                             Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank3  and rank1 array
!
!# Introduction
!
! This fuction performs following task
! `Ans(:,:) = a1(:,:,a)*a2(a)`

INTERFACE
  MODULE PURE FUNCTION matmul_r3_r1(a1, a2) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :), a2(:)
    REAL(DFP) :: Ans(size(a1, 1), size(a1, 2))
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: matmul for rank1 and rank3 array
!
!# Introduction
!
! This fuction performs following task
! `Ans(i,j) = a1(a)*a2(a,i,j)`

INTERFACE
  MODULE PURE FUNCTION matmul_r1_r3(a1, a2) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a1(:), a2(:, :, :)
    REAL(DFP) :: Ans(size(a2, 2), size(a2, 3))
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         matmul for rank2 and rank3 array
!
!# Introduction
!
! This fuction performs following task
! `Ans(i,j,ip) = a1(i,I)*a2(I,j,ip)`

INTERFACE
  MODULE PURE FUNCTION matmul_r2_r3(a1, a2) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a1(:, :), a2(:, :, :)
    REAL(DFP) :: Ans(size(a1, 1), size(a2, 2), size(a2, 3))
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         matmul for rank4 and rank1 array
!
!# Introduction
!
! `Ans(:,:,:) = a1(:,:,:,a)*a2(a)`

INTERFACE
  MODULE PURE FUNCTION matmul_r4_r1(a1, a2) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :), a2(:)
    REAL(DFP) :: Ans(size(a1, 1), size(a1, 2), size(a1, 3))
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Matmul@Matmul
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         matmul for rank3 and rank2
!
!# Introduction
! This fuction performs following task
! `Ans(i,j,ip) = a1(i,j,I)*a2(I,ip)`

INTERFACE
  MODULE PURE FUNCTION matmul_r3_r2(a1, a2) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :), a2(:, :)
    REAL(DFP) :: Ans(size(a1, 1), size(a1, 2), size(a2, 2))
  END FUNCTION
END INTERFACE

INTERFACE MATMUL
  MODULE PROCEDURE matmul_r3_r1, matmul_r4_r1, matmul_r3_r2, &
    & matmul_r1_r3, matmul_r2_r3
END INTERFACE MATMUL

PUBLIC :: MATMUL