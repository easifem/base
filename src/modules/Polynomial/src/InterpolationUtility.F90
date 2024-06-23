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

MODULE InterpolationUtility
USE GlobalData, ONLY: I4B, DFP, REAL32, REAL64
USE String_Class, ONLY: String

IMPLICIT NONE
PRIVATE
PUBLIC :: VandermondeMatrix
PUBLIC :: GetTotalInDOF
PUBLIC :: GetTotalDOF
PUBLIC :: RefElemDomain

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 July 2022
! summary: Returns vandermonde matrix

INTERFACE VandermondeMatrix
  MODULE PURE FUNCTION VandermondeMatrix_Real32(order, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: ans(SIZE(x), order + 1)
  END FUNCTION VandermondeMatrix_Real32

  MODULE PURE FUNCTION VandermondeMatrix_Real64(order, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: ans(SIZE(x), order + 1)
  END FUNCTION VandermondeMatrix_Real64
END INTERFACE VandermondeMatrix

!----------------------------------------------------------------------------
!                                                             GetTotalDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get total number of degrees of freedom

INTERFACE GetTotalDOF
  MODULE PURE FUNCTION GetTotalDOF1(elemType, order, baseContinuity, &
                                    baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, Point, Line, Triangle
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    CHARACTER(LEN=*), INTENT(IN) :: baseContinuity
    !! continuity of basis, H1, HDiv, HCurl
    CHARACTER(LEN=*), INTENT(IN) :: baseInterpolation
    !! interpolation of basis, Lagrange, Heirarchical
    INTEGER(I4B) :: ans
    !! total number of degrees of freedom
  END FUNCTION GetTotalDOF1
END INTERFACE GetTotalDOF

!----------------------------------------------------------------------------
!                                                             GetTotalInDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get total number of degrees of freedom internal to the element

INTERFACE GetTotalInDOF
  MODULE PURE FUNCTION GetTotalInDOF1(elemType, order, baseContinuity, &
                                      baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, Point, Line, Triangle
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    CHARACTER(LEN=*), INTENT(IN) :: baseContinuity
    !! continuity of basis, H1, HDiv, HCurl
    CHARACTER(LEN=*), INTENT(IN) :: baseInterpolation
    !! interpolation of basis, Lagrange, Heirarchical
    INTEGER(I4B) :: ans
    !! total number of degrees of freedom
  END FUNCTION GetTotalInDOF1
END INTERFACE GetTotalInDOF

!----------------------------------------------------------------------------
!                                                           RefElemDomain
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference element

INTERFACE
  MODULE FUNCTION RefElemDomain(elemType, baseContinuity, baseInterpol) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
  END FUNCTION RefElemDomain
END INTERFACE

END MODULE InterpolationUtility
