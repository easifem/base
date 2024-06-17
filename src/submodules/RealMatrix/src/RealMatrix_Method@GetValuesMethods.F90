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
! You should have received a Copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(RealMatrix_Method) GetvaluesMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get1
IF (ALLOCATED(obj%val)) THEN
  CALL reallocate(ans, SIZE(obj, 1), SIZE(obj, 2))
  ans = obj%val
ELSE
  CALL reallocate(ans, 0, 0)
END IF
END PROCEDURE realmat_Get1

!----------------------------------------------------------------------------
!                                                                Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get1b
ans = realmat_get1(obj=obj, datatype=1.0_DFP)
END PROCEDURE realmat_Get1b

!----------------------------------------------------------------------------
!                                                                Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get2
ans = obj%val(RIndx, CIndx)
END PROCEDURE realmat_Get2

!----------------------------------------------------------------------------
!                                                                Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get3
#define Indx iStart:iEnd:Stride
ans = obj%val(Indx, Indx)
#undef Indx
END PROCEDURE realmat_Get3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get4
ans%val = obj%val
CALL SetTotalDimension(ans, 2_I4B)
END PROCEDURE realmat_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get5
ans%val = obj%val(RIndx, CIndx)
CALL SetTotalDimension(ans, 2_I4B)
END PROCEDURE realmat_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get6
#define Indx iStart:iEnd:Stride
ans%val = obj%val(Indx, Indx)
#undef Indx
CALL SetTotalDimension(ans, 2_I4B)
END PROCEDURE realmat_Get6

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get7
INTEGER(I4B) :: s(2), i, j, r1, r2, c1, c2
INTEGER(I4B), ALLOCATABLE :: rc(:, :)
  !!
  !! main
  !!
s = SHAPE(obj)
ALLOCATE (rc(0:2, 0:(s(1) * s(2))))
rc = 0
  !!
DO j = 1, s(2)
  DO i = 1, s(1)
    rc(1:2, i + (j - 1) * s(1)) = SHAPE(obj(i, j))
  END DO
END DO
  !!
i = MAXVAL(SUM(RESHAPE(rc(1, 1:), SHAPE(obj)), 1))
j = MAXVAL(SUM(RESHAPE(rc(2, 1:), SHAPE(obj)), 2))
  !!
ALLOCATE (ans(i, j)); ans = 0.0_DFP
  !!
c1 = 0; c2 = 0
  !!
DO j = 1, s(2)
  c1 = 1 + c2
  c2 = c1 + rc(2, j) - 1
  r1 = 0; r2 = 0
  DO i = 1, s(1)
    r1 = 1 + r2
    r2 = r1 + rc(1, i) - 1
    ans(r1:r2, c1:c2) = obj(i, j)%val
  END DO
END DO
  !!
END PROCEDURE realmat_Get7

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Get8
ans%val = Get(obj, TypeDFP)
CALL SetTotalDimension(ans, 2_I4B)
END PROCEDURE realmat_Get8

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Copy1
To = from%val
END PROCEDURE realmat_Copy1

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Copy2
to%val = from%val
CALL SetTotalDimension(To, 2_I4B)
END PROCEDURE realmat_Copy2

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_Copy3
to%val = from
CALL SetTotalDimension(To, 2_I4B)
END PROCEDURE realmat_Copy3

!----------------------------------------------------------------------------
!                                                               ArrayPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_GetPointer
ans => obj%val
END PROCEDURE realmat_GetPointer

END SUBMODULE GetvaluesMethods
