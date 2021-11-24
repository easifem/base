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

!> authors: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This submodule contains outerprod

SUBMODULE(Utility) ProductMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 VectorProd
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorProduct_1
c(1) = a(2) * b(3) - a(3) * b(2)
c(2) = a(3) * b(1) - a(1) * b(3)
c(3) = a(1) * b(2) - a(2) * b(1)
END PROCEDURE vectorProduct_1

!----------------------------------------------------------------------------
!                                                                 VectorProd
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorProduct_2
c(1) = a(2) * b(3) - a(3) * b(2)
c(2) = a(3) * b(1) - a(1) * b(3)
c(3) = a(1) * b(2) - a(2) * b(1)
END PROCEDURE vectorProduct_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_1
Ans = 0.0_DFP
Ans = SPREAD(a, dim=2, ncopies=SIZE(b)) * &
      & SPREAD(b, dim=1, ncopies=SIZE(a))
END PROCEDURE outerprod_1

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

MODULE PROCEDURE outerprod_2
Ans = 0.0_DFP
IF (Sym) THEN
  Ans = SPREAD(0.5_DFP * a, dim=2, ncopies=SIZE(b)) &
      & * SPREAD(b, dim=1, ncopies=SIZE(a)) &
      & + SPREAD(0.5_DFP * b, dim=2, ncopies=SIZE(a)) &
      & * SPREAD(a, dim=1, ncopies=SIZE(b))
ELSE
  Ans = SPREAD(a, dim=2, ncopies=SIZE(b)) * &
          & SPREAD(b, dim=1, ncopies=SIZE(a))
END IF
END PROCEDURE outerprod_2

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

MODULE PROCEDURE outerprod_3
! Definen internal variables
INTEGER(I4B) :: I
Ans = 0.0_DFP
DO CONCURRENT(I=1:SIZE(b))
  Ans(:, :, I) = a(:, :) * b(I)
END DO
END PROCEDURE outerprod_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_4
INTEGER(I4B) :: I
Ans = 0.0_DFP
DO CONCURRENT(I=1:SIZE(b))
  Ans(:, :, :, I) = a(:, :, :) * b(I)
END DO
END PROCEDURE outerprod_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_5
ans = outerprod(outerprod(a, b), c)
END PROCEDURE outerprod_5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE outerprod_6
INTEGER(I4B) :: l
DO CONCURRENT (l=1:SIZE(b,2))
   ans(:,:,:,l)=outerprod(a, b(:,l))
END DO
END PROCEDURE outerprod_6

END SUBMODULE ProductMethods
