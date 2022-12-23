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

SUBMODULE(TriagUtility) Methods
USE BaseMethod, ONLY: Input, DiagIndx, Append, Reallocate, DiagSize
CONTAINS

!----------------------------------------------------------------------------
!                                                                 TriuIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE TriuIndx_1
INTEGER(I4B) :: m0, n0, diagNo0, tsize, tdiag, idiag, i1, i2
!
m0 = m
n0 = INPUT(default=m, option=n)
diagNo0 = INPUT(default=0_I4B, option=diagNo)
!
tsize = 0
tdiag = 0
idiag = diagNo0
!
DO
  IF (idiag .GT. n0) EXIT
  tsize = tsize + DiagSize(m0, n0, idiag)
  idiag = idiag + 1
END DO
!
ALLOCATE (ans(tsize, 2))
!
idiag = diagNo0
!
i1 = 0
i2 = 0
!
DO
  IF (idiag .GT. n0) EXIT
  i1 = i2 + 1
  i2 = i2 + DiagSize(m0, n0, idiag)
  ans(i1:i2, 1:2) = DiagIndx(m0, n0, idiag)
  idiag = idiag + 1
END DO
!
END PROCEDURE TriuIndx_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TriuIndx_2
ans = TriuIndx(SIZE(A, 1), SIZE(A, 2), diagNo)
END PROCEDURE TriuIndx_2

!----------------------------------------------------------------------------
!                                                                 TrilIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE TrilIndx_1
INTEGER(I4B) :: m0, n0, diagNo0, tsize, idiag, i1, i2
!
m0 = m
n0 = INPUT(default=m, option=n)
diagNo0 = INPUT(default=0_I4B, option=diagNo)
!
tsize = 0
idiag = diagNo0
!
DO
  IF (-idiag .GT. m0) EXIT
  tsize = tsize + DiagSize(m0, n0, idiag)
  idiag = idiag - 1
END DO
!
ALLOCATE (ans(tsize, 2))
!
i1 = 0
i2 = 0
idiag = diagNo0
!
DO
  IF (-idiag .GT. m0) EXIT
  i1 = i2 + 1
  i2 = i2 + DiagSize(m0, n0, idiag)
  ans(i1:i2, 1:2) = DiagIndx(m0, n0, idiag)
  idiag = idiag - 1
END DO
!
END PROCEDURE TrilIndx_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TrilIndx_2
ans = TrilIndx(SIZE(A, 1), SIZE(A, 2), diagNo)
END PROCEDURE TrilIndx_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Tril_1
#include "./Triag/Tril1.inc"
END PROCEDURE Tril_1
MODULE PROCEDURE Tril_2
#include "./Triag/Tril1.inc"
END PROCEDURE Tril_2
MODULE PROCEDURE Tril_3
#include "./Triag/Tril1.inc"
END PROCEDURE Tril_3
MODULE PROCEDURE Tril_4
#include "./Triag/Tril1.inc"
END PROCEDURE Tril_4
MODULE PROCEDURE Tril_5
#include "./Triag/Tril1.inc"
END PROCEDURE Tril_5
MODULE PROCEDURE Tril_6
#include "./Triag/Tril1.inc"
END PROCEDURE Tril_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Tril_7
#include "./Triag/Tril2.inc"
END PROCEDURE Tril_7
MODULE PROCEDURE Tril_8
#include "./Triag/Tril2.inc"
END PROCEDURE Tril_8
MODULE PROCEDURE Tril_9
#include "./Triag/Tril2.inc"
END PROCEDURE Tril_9
MODULE PROCEDURE Tril_10
#include "./Triag/Tril2.inc"
END PROCEDURE Tril_10
MODULE PROCEDURE Tril_11
#include "./Triag/Tril2.inc"
END PROCEDURE Tril_11
MODULE PROCEDURE Tril_12
#include "./Triag/Tril2.inc"
END PROCEDURE Tril_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Triu_1
#include "./Triag/Triu1.inc"
END PROCEDURE Triu_1
MODULE PROCEDURE Triu_2
#include "./Triag/Triu1.inc"
END PROCEDURE Triu_2
MODULE PROCEDURE Triu_3
#include "./Triag/Triu1.inc"
END PROCEDURE Triu_3
MODULE PROCEDURE Triu_4
#include "./Triag/Triu1.inc"
END PROCEDURE Triu_4
MODULE PROCEDURE Triu_5
#include "./Triag/Triu1.inc"
END PROCEDURE Triu_5
MODULE PROCEDURE Triu_6
#include "./Triag/Triu1.inc"
END PROCEDURE Triu_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Triu_7
#include "./Triag/Triu2.inc"
END PROCEDURE Triu_7
MODULE PROCEDURE Triu_8
#include "./Triag/Triu2.inc"
END PROCEDURE Triu_8
MODULE PROCEDURE Triu_9
#include "./Triag/Triu2.inc"
END PROCEDURE Triu_9
MODULE PROCEDURE Triu_10
#include "./Triag/Triu2.inc"
END PROCEDURE Triu_10
MODULE PROCEDURE Triu_11
#include "./Triag/Triu2.inc"
END PROCEDURE Triu_11
MODULE PROCEDURE Triu_12
#include "./Triag/Triu2.inc"
END PROCEDURE Triu_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTril_1
#include "./Triag/GetTril1.inc"
END PROCEDURE GetTril_1
MODULE PROCEDURE GetTril_2
#include "./Triag/GetTril1.inc"
END PROCEDURE GetTril_2
MODULE PROCEDURE GetTril_3
#include "./Triag/GetTril1.inc"
END PROCEDURE GetTril_3
MODULE PROCEDURE GetTril_4
#include "./Triag/GetTril1.inc"
END PROCEDURE GetTril_4
MODULE PROCEDURE GetTril_5
#include "./Triag/GetTril1.inc"
END PROCEDURE GetTril_5
MODULE PROCEDURE GetTril_6
#include "./Triag/GetTril1.inc"
END PROCEDURE GetTril_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTril_7
#include "./Triag/GetTril2.inc"
END PROCEDURE GetTril_7
MODULE PROCEDURE GetTril_8
#include "./Triag/GetTril2.inc"
END PROCEDURE GetTril_8
MODULE PROCEDURE GetTril_9
#include "./Triag/GetTril2.inc"
END PROCEDURE GetTril_9
MODULE PROCEDURE GetTril_10
#include "./Triag/GetTril2.inc"
END PROCEDURE GetTril_10
MODULE PROCEDURE GetTril_11
#include "./Triag/GetTril2.inc"
END PROCEDURE GetTril_11
MODULE PROCEDURE GetTril_12
#include "./Triag/GetTril2.inc"
END PROCEDURE GetTril_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTriu_1
#include "./Triag/GetTriu1.inc"
END PROCEDURE GetTriu_1
MODULE PROCEDURE GetTriu_2
#include "./Triag/GetTriu1.inc"
END PROCEDURE GetTriu_2
MODULE PROCEDURE GetTriu_3
#include "./Triag/GetTriu1.inc"
END PROCEDURE GetTriu_3
MODULE PROCEDURE GetTriu_4
#include "./Triag/GetTriu1.inc"
END PROCEDURE GetTriu_4
MODULE PROCEDURE GetTriu_5
#include "./Triag/GetTriu1.inc"
END PROCEDURE GetTriu_5
MODULE PROCEDURE GetTriu_6
#include "./Triag/GetTriu1.inc"
END PROCEDURE GetTriu_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTriu_7
#include "./Triag/GetTriu2.inc"
END PROCEDURE GetTriu_7
MODULE PROCEDURE GetTriu_8
#include "./Triag/GetTriu2.inc"
END PROCEDURE GetTriu_8
MODULE PROCEDURE GetTriu_9
#include "./Triag/GetTriu2.inc"
END PROCEDURE GetTriu_9
MODULE PROCEDURE GetTriu_10
#include "./Triag/GetTriu2.inc"
END PROCEDURE GetTriu_10
MODULE PROCEDURE GetTriu_11
#include "./Triag/GetTriu2.inc"
END PROCEDURE GetTriu_11
MODULE PROCEDURE GetTriu_12
#include "./Triag/GetTriu2.inc"
END PROCEDURE GetTriu_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTriu_1
#include "./Triag/SetTriu1.inc"
END PROCEDURE SetTriu_1
MODULE PROCEDURE SetTriu_2
#include "./Triag/SetTriu1.inc"
END PROCEDURE SetTriu_2
MODULE PROCEDURE SetTriu_3
#include "./Triag/SetTriu1.inc"
END PROCEDURE SetTriu_3
MODULE PROCEDURE SetTriu_4
#include "./Triag/SetTriu1.inc"
END PROCEDURE SetTriu_4
MODULE PROCEDURE SetTriu_5
#include "./Triag/SetTriu1.inc"
END PROCEDURE SetTriu_5
MODULE PROCEDURE SetTriu_6
#include "./Triag/SetTriu1.inc"
END PROCEDURE SetTriu_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTriu_7
#include "./Triag/SetTriu2.inc"
END PROCEDURE SetTriu_7
MODULE PROCEDURE SetTriu_8
#include "./Triag/SetTriu2.inc"
END PROCEDURE SetTriu_8
MODULE PROCEDURE SetTriu_9
#include "./Triag/SetTriu2.inc"
END PROCEDURE SetTriu_9
MODULE PROCEDURE SetTriu_10
#include "./Triag/SetTriu2.inc"
END PROCEDURE SetTriu_10
MODULE PROCEDURE SetTriu_11
#include "./Triag/SetTriu2.inc"
END PROCEDURE SetTriu_11
MODULE PROCEDURE SetTriu_12
#include "./Triag/SetTriu2.inc"
END PROCEDURE SetTriu_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTriu_13
#include "./Triag/SetTriu3.inc"
END PROCEDURE SetTriu_13
MODULE PROCEDURE SetTriu_14
#include "./Triag/SetTriu3.inc"
END PROCEDURE SetTriu_14
MODULE PROCEDURE SetTriu_15
#include "./Triag/SetTriu3.inc"
END PROCEDURE SetTriu_15
MODULE PROCEDURE SetTriu_16
#include "./Triag/SetTriu3.inc"
END PROCEDURE SetTriu_16
MODULE PROCEDURE SetTriu_17
#include "./Triag/SetTriu3.inc"
END PROCEDURE SetTriu_17
MODULE PROCEDURE SetTriu_18
#include "./Triag/SetTriu3.inc"
END PROCEDURE SetTriu_18

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTril_1
#include "./Triag/SetTril1.inc"
END PROCEDURE SetTril_1
MODULE PROCEDURE SetTril_2
#include "./Triag/SetTril1.inc"
END PROCEDURE SetTril_2
MODULE PROCEDURE SetTril_3
#include "./Triag/SetTril1.inc"
END PROCEDURE SetTril_3
MODULE PROCEDURE SetTril_4
#include "./Triag/SetTril1.inc"
END PROCEDURE SetTril_4
MODULE PROCEDURE SetTril_5
#include "./Triag/SetTril1.inc"
END PROCEDURE SetTril_5
MODULE PROCEDURE SetTril_6
#include "./Triag/SetTril1.inc"
END PROCEDURE SetTril_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTril_7
#include "./Triag/SetTril2.inc"
END PROCEDURE SetTril_7
MODULE PROCEDURE SetTril_8
#include "./Triag/SetTril2.inc"
END PROCEDURE SetTril_8
MODULE PROCEDURE SetTril_9
#include "./Triag/SetTril2.inc"
END PROCEDURE SetTril_9
MODULE PROCEDURE SetTril_10
#include "./Triag/SetTril2.inc"
END PROCEDURE SetTril_10
MODULE PROCEDURE SetTril_11
#include "./Triag/SetTril2.inc"
END PROCEDURE SetTril_11
MODULE PROCEDURE SetTril_12
#include "./Triag/SetTril2.inc"
END PROCEDURE SetTril_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTril_13
#include "./Triag/SetTril3.inc"
END PROCEDURE SetTril_13
MODULE PROCEDURE SetTril_14
#include "./Triag/SetTril3.inc"
END PROCEDURE SetTril_14
MODULE PROCEDURE SetTril_15
#include "./Triag/SetTril3.inc"
END PROCEDURE SetTril_15
MODULE PROCEDURE SetTril_16
#include "./Triag/SetTril3.inc"
END PROCEDURE SetTril_16
MODULE PROCEDURE SetTril_17
#include "./Triag/SetTril3.inc"
END PROCEDURE SetTril_17
MODULE PROCEDURE SetTril_18
#include "./Triag/SetTril3.inc"
END PROCEDURE SetTril_18

END SUBMODULE Methods
