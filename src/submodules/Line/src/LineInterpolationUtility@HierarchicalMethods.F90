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

SUBMODULE(LineInterpolationUtility) HierarchicalMethods
USE BaseType, ONLY: polyopt => TypePolynomialOpt
USE StringUtility, ONLY: UpperCase
USE MappingUtility, ONLY: FromUnitLine2BiUnitLine_
USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol_, &
                                       EvalAllOrthopol_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "LineInterpolationUtility@HierarchicalMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Line1
INTEGER(I4B) :: nrow, ncol
CALL HeirarchicalBasis_Line1_(order=order, xij=xij, refLine=refLine, &
                              ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Line1

!----------------------------------------------------------------------------
!                                                        BasisEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Line1_
INTEGER(I4B), PARAMETER :: orient = 1
CALL HeirarchicalBasis_Line2_(order=order, xij=xij, refLine=refLine, &
                              orient=orient, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Line2_
CHARACTER(1) :: astr
REAL(DFP) :: temp(SIZE(xij, 2)), o1
INTEGER(I4B) :: ii, k

o1 = REAL(orient, kind=DFP)
astr = UpperCase(refLine(1:1))

SELECT CASE (astr)
CASE ("U")
  CALL FromUnitLine2BiUnitLine_(xin=xij(1, :), ans=temp, tsize=nrow)
  CALL EvalAllOrthopol_(n=order, x=temp, orthopol=polyopt%Lobatto, ans=ans, &
                        nrow=nrow, ncol=ncol)

CASE ("B")
  CALL EvalAllOrthopol_(n=order, x=xij(1, :), orthopol=polyopt%Lobatto, &
                        ans=ans, nrow=nrow, ncol=ncol)

CASE DEFAULT
  nrow = 0
  ncol = 0
END SELECT

DO CONCURRENT(k=2:order, ii=1:nrow)
  ans(ii, k + 1) = (o1**k) * ans(ii, k + 1)
END DO
END PROCEDURE HeirarchicalBasis_Line2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line1
INTEGER(I4B) :: dim1, dim2, dim3
CALL HeirarchicalGradientBasis_Line1_( &
  order=order, xij=xij, refLine=refLine, ans=ans, dim1=dim1, dim2=dim2, &
  dim3=dim3)
END PROCEDURE HeirarchicalGradientBasis_Line1

!----------------------------------------------------------------------------
!                                            HeirarchicalGradientBasis_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line1_
INTEGER(I4B), PARAMETER :: orient = 1
CALL HeirarchicalGradientBasis_Line2_( &
  order=order, xij=xij, refLine=refLine, orient=orient, ans=ans, &
  dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE HeirarchicalGradientBasis_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line2
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(xij, 2)
dim2 = order + 1
dim3 = 1
ALLOCATE (ans(dim1, dim2, dim3))
CALL HeirarchicalGradientBasis_Line2_( &
  order=order, xij=xij, refLine=refLine, orient=orient, ans=ans, &
  dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE HeirarchicalGradientBasis_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line2_
CHARACTER(1) :: astr
REAL(DFP) :: temp(SIZE(xij, 2)), o1
INTEGER(I4B) :: ii, jj, k

o1 = REAL(orient, kind=DFP)
astr = UpperCase(refLine(1:1))

dim3 = 1

SELECT CASE (astr)

CASE ("U")
  CALL FromUnitLine2BiUnitLine_(xin=xij(1, :), ans=temp, tsize=dim1)
  CALL GradientEvalAllOrthopol_(n=order, x=temp, orthopol=polyopt%Lobatto, &
                                ans=ans(:, :, 1), nrow=dim1, ncol=dim2)

  DO CONCURRENT(ii=1:dim1, jj=1:dim2)
    ans(ii, jj, 1) = ans(ii, jj, 1) * 2.0_DFP
  END DO

CASE ("B")
  CALL GradientEvalAllOrthopol_(n=order, x=xij(1, :), &
                                orthopol=polyopt%Lobatto, ans=ans(:, :, 1), &
                                nrow=dim1, ncol=dim2)

CASE DEFAULT
  dim1 = 0; dim2 = 0; dim3 = 0
  RETURN
END SELECT

DO CONCURRENT(k=2:order, ii=1:dim1)
  ans(ii, k + 1, 1) = (o1**(k - 1)) * ans(ii, k + 1, 1)
END DO

END PROCEDURE HeirarchicalGradientBasis_Line2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HierarchicalMethods
