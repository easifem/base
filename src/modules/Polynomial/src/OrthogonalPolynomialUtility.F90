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

MODULE OrthogonalPolynomialUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: Clenshaw
PUBLIC :: ChebClenshaw
PUBLIC :: JacobiMatrix
PUBLIC :: EvalAllOrthopol
PUBLIC :: GradientEvalAllOrthopol

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Clenshaw_1(x, alpha, beta, y0, ym1, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: alpha(0:)
    REAL(DFP), INTENT(IN) :: beta(0:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: y0
    !! if y0 is absent then y0 = 1.0
    REAL(DFP), OPTIONAL, INTENT(IN) :: ym1
    !! if ym1 is absent then ym1 = 0.0
    REAL(DFP), INTENT(IN) :: c(0:)
    REAL(DFP) :: ans
  END FUNCTION Clenshaw_1
END INTERFACE

INTERFACE Clenshaw
  MODULE PROCEDURE Clenshaw_1
END INTERFACE Clenshaw

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Clenshaw_2(x, alpha, beta, y0, ym1, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: alpha(0:)
    REAL(DFP), INTENT(IN) :: beta(0:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: y0
    !! if y0 is absent then y0 = 1.0
    REAL(DFP), OPTIONAL, INTENT(IN) :: ym1
    !! if ym1 is absent then ym1 = 0.0
    REAL(DFP), INTENT(IN) :: c(0:)
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION Clenshaw_2
END INTERFACE

INTERFACE Clenshaw
  MODULE PROCEDURE Clenshaw_2
END INTERFACE Clenshaw

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2021
! summary: CleanShaw for Chebyshev
!
!# Introduction
!
! ClenShaw for Chebyshev polynomial expansion. It returns :
!
!$$
! s(t) = 0.5 c_{0} + \sum_{i=1}^{n} c_{i} T_{j}(x)
!$$

INTERFACE
  MODULE PURE FUNCTION ChebClenshaw_1(x, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: c(0:)
    REAL(DFP) :: ans
  END FUNCTION ChebClenshaw_1
END INTERFACE

INTERFACE Clenshaw
  MODULE PROCEDURE ChebClenshaw_1
END INTERFACE Clenshaw

INTERFACE ChebClenshaw
  MODULE PROCEDURE ChebClenshaw_1
END INTERFACE ChebClenshaw

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2021
! summary: CleanShaw for Chebyshev
!
!# Introduction
!
! ClenShaw for Chebyshev polynomial expansion. It returns :
!
!$$
! s(t) = 0.5 c_{0} + \sum_{i=1}^{n} c_{i} T_{j}(x)
!$$

INTERFACE
  MODULE PURE FUNCTION ChebClenshaw_2(x, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: c(0:)
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION ChebClenshaw_2
END INTERFACE

INTERFACE Clenshaw
  MODULE PROCEDURE ChebClenshaw_2
END INTERFACE Clenshaw

INTERFACE ChebClenshaw
  MODULE PROCEDURE ChebClenshaw_2
END INTERFACE ChebClenshaw

!----------------------------------------------------------------------------
!                                                             JacobiMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE JacobiMatrix_1(alphaCoeff, betaCoeff, D, E)
    REAL(DFP), INTENT(IN) :: alphaCoeff(0:)
  !! size n, from 0 to n-1
    REAL(DFP), INTENT(IN) :: betaCoeff(0:)
  !! size n, from 0 to n-1
    REAL(DFP), INTENT(OUT) :: D(:)
  !! entry from 1 to n are filled
    REAL(DFP), INTENT(OUT) :: E(:)
  !! entry from 1 to n-1 are filled
  END SUBROUTINE JacobiMatrix_1
END INTERFACE

INTERFACE JacobiMatrix
  MODULE PROCEDURE JacobiMatrix_1
END INTERFACE JacobiMatrix

!----------------------------------------------------------------------------
!                                                           EvalAllOrthopol
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION EvalAllOrthopol(n, x, orthopol, alpha, beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    INTEGER(I4B), INTENT(IN) :: orthopol
    !! orthogonal polynomial family
    !! Legendre
    !! Jacobi
    !! Lobatto
    !! Chebyshev
    !! Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! The number of rows in ans is equal to the number of points.
    !! The number of columns are equal to the orthogonal
    !! polynomials from order  = 0 to n
    !! Therefore, jth column is denotes the value of jth polynomial
    !! at all the points.
  END FUNCTION EvalAllOrthopol
END INTERFACE

!----------------------------------------------------------------------------
!                                                           EvalAllOrthopol
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION GradientEvalAllOrthopol( &
    & n,  &
    & x,  &
    & orthopol,  &
    & alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    INTEGER(I4B), INTENT(IN) :: orthopol
    !! orthogonal polynomial family
    !! Legendre
    !! Jacobi
    !! Lobatto
    !! Chebyshev
    !! Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! The number of rows in ans is equal to the number of points.
    !! The number of columns are equal to the orthogonal
    !! polynomials from order  = 0 to n
    !! Therefore, jth column is denotes the value of jth polynomial
    !! at all the points.
  END FUNCTION GradientEvalAllOrthopol
END INTERFACE

END MODULE OrthogonalPolynomialUtility
