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

MODULE ContractionUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j,k) = a1(i,j,k,l)*a2(l)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r4_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2), SIZE(a1, 3))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r4_r1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans(i,j) = a1(i,j,k,l)*a2(k,l)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r4_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r4_r2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans(i) = a1(i,j,k,l)*a2(j,k,l)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r4_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a1, 1))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r4_r3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans = a1(i,j,k,l)*a2(i,j,k,l)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r4_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r4_r4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                   Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank3  and rank1 array
!
!# Introduction
!
! This fuction performs following task
! `ans(i,j) = a1(i,j,k)*a2(k)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r3_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1), SIZE(a1, 2))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r3_r1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         Contraction for rank3 and rank2
!
!# Introduction
! This fuction performs following task
! `ans(i) = a1(i,j,k)*a2(j,k)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r3_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a1, 1))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r3_r2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans = a1(i,j,k) * a2(i,j,k)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r3_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r3_r3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans(l) = a1(i,j,k) * a2(i,j,k,l)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r3_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a2, 4))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r3_r4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                   Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank3  and rank1 array
!
!# Introduction
!
! This fuction performs following task
! `ans(i) = a1(i,j)*a2(j)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r2_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans(SIZE(a1, 1))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r2_r1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         Contraction for rank3 and rank2
!
!# Introduction
! This fuction performs following task
! `ans = a1(i,j)*a2(i,j)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r2_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r2_r2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans(k) = a1(i,j) * a2(i,j,k)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r2_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a2, 3))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r2_r3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans(k,l) = a1(i,j) * a2(i,j,k,l)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r2_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:, :)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r2_r4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                   Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank3  and rank1 array
!
!# Introduction
!
! This fuction performs following task
! `ans = a1(i)*a2(i)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r1_r1(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:)
    REAL(DFP) :: ans
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r1_r1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         Contraction for rank3 and rank2
!
!# Introduction
! This fuction performs following task
! `ans(j) = a1(i)*a2(i,j)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r1_r2(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :)
    REAL(DFP) :: ans(SIZE(a2, 2))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r1_r2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans(j,k) = a1(i) * a2(i,j,k)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r1_r3(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :)
    REAL(DFP) :: ans(SIZE(a2, 2), SIZE(a2, 3))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r1_r3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                    Contraction@Contraction
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 April 2021
! summary: Contraction for rank4 and rank1 array
!
!# Introduction
!
! `ans(j, k,l) = a1(i) * a2(i,j,k,l)`

INTERFACE
  MODULE PURE FUNCTION Contraction_r1_r4(a1, a2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a1(:)
    REAL(DFP), INTENT(IN) :: a2(:, :, :, :)
    REAL(DFP) :: ans(SIZE(a2, 2), SIZE(a2, 3), SIZE(a2, 4))
  END FUNCTION
END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE Contraction_r1_r4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ContractionUtility
