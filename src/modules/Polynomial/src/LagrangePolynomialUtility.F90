! This program is a part of EASIFEM library
! Copyright (C) 2020-2021 Vikas Sharma, Ph.D
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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Nodal polynomial construction and evaluation on reference elements.
!
! Coordinate arrays place points in columns. Degree tables place monomials in
! rows and coordinate exponents in columns. Values use point rows and basis
! columns; gradients add a final coordinate axis.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$V C=I,\quad\ell_j(x_i)=\delta_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: poly
! USE LagrangePolynomialUtility, ONLY: LagrangeEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! REAL(DFP) :: coeff(3, 3)
! LOGICAL(LGT) :: firstCall
! REAL(DFP) :: points(1, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! firstCall = .TRUE.
! coeff = 0.0_DFP
! points = nodes
! y = LagrangeEvalAll(order=2_I4B, elemType=Line, x=points, xij=nodes, &
!   domainName="BIUNIT", coeff=coeff, firstCall=firstCall, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y-RESHAPE( &
!   [1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Nodal interpolation identity")
! END PROGRAM main
!```

MODULE LagrangePolynomialUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDOF
PUBLIC :: LagrangeInDOF
PUBLIC :: LagrangeDegree

PUBLIC :: EquidistancePoint
PUBLIC :: EquidistancePoint_

PUBLIC :: LagrangeVandermonde
PUBLIC :: LagrangeVandermonde_

PUBLIC :: InterpolationPoint
PUBLIC :: InterpolationPoint_

PUBLIC :: LagrangeCoeff
PUBLIC :: LagrangeCoeff_

PUBLIC :: LagrangeEvalAll
PUBLIC :: LagrangeEvalAll_

PUBLIC :: LagrangeGradientEvalAll
PUBLIC :: LagrangeGradientEvalAll_

!----------------------------------------------------------------------------
!                                                   LagrangeDOF@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count total Lagrange interpolation DOFs.
!
! The p,q,r overload of LagrangeDOF supplies directional orders for tensor
! product elements.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{line,int}}=\max(p-1,0)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = LagrangeDOF(order=2_I4B, elemType=Line)
! CALL OK(ndof == 3, "Quadratic line degree count")
! END PROGRAM main
!```

INTERFACE LagrangeDOF
  MODULE PURE FUNCTION LagrangeDOF1(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION LagrangeDOF1
END INTERFACE LagrangeDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count total Lagrange interpolation DOFs.
!
! The p,q,r overload of LagrangeDOF supplies directional orders for tensor
! product elements.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional interpolation order.
!   q : INTEGER(I4B), INTENT(IN). Second directional interpolation order.
!   r : INTEGER(I4B), INTENT(IN). Third directional interpolation order.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{line,int}}=\max(p-1,0)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = LagrangeDOF(order=2_I4B, elemType=Line)
! CALL OK(ndof == 3, "Quadratic line degree count")
! END PROGRAM main
!```

INTERFACE LagrangeDOF
  MODULE PURE FUNCTION LagrangeDOF2(p, q, r, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    !! order in x, y, and z direction
    INTEGER(I4B), INTENT(IN) :: elemType
    !! for line, triangle, tetrahedron, prism , and pyramid only p is used
    !! for quadrangle and hexahedron, pq are used and pqr are used
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION LagrangeDOF2
END INTERFACE LagrangeDOF

!----------------------------------------------------------------------------
!                                                 LagrangeInDOF@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count interior Lagrange interpolation DOFs.
!
! The p,q,r overload of LagrangeDOF supplies directional orders for tensor
! product elements.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{line,int}}=\max(p-1,0)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeInDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = LagrangeInDOF(order=2_I4B, elemType=Line)
! CALL OK(ndof == 1, "Quadratic line degree count")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION LagrangeInDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                           LagrangeDegree
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the monomial exponent table for an element.
!
! Rows enumerate monomials and columns enumerate coordinate directions.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE(:, :).
!
! Formula:
! $$m_j(x)=\prod_{k=1}^{d}x_k^{a_{jk}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeDegree
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
!
! degree = LagrangeDegree(order=2_I4B, elemType=Line)
! CALL OK(ALL(SHAPE(degree) == [3, 1]), "One exponent per monomial")
! CALL OK(ALL(degree(:, 1) == [0_I4B, 1_I4B, 2_I4B]), &
!   "Increasing line monomial powers")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type: Line, Triangle, Quadrangle, Tetrahedron, ...
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build the monomial Vandermonde matrix for an element.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns.
!
! Arguments:
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$V_{ij}=\prod_{k=1}^{d}x_{ki}^{a_{jk}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LagrangeVandermonde(order=2_I4B, elemType=Line, xij=nodes)
! CALL OK(MAXVAL(ABS(y(:, 3)-nodes(1, :)**2)) < 1.0E-11_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeVandermonde(xij, order, elemType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !!  points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! vandermonde matrix
    !! nrows := number of points
    !! ncols := number of dof
  END FUNCTION LagrangeVandermonde
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build the monomial Vandermonde matrix for an element.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns.
!
! Arguments:
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$V_{ij}=\prod_{k=1}^{d}x_{ki}^{a_{jk}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL LagrangeVandermonde_(order=2_I4B, elemType=Line, xij=nodes, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 3)-nodes(1, :)**2)) < 1.0E-11_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeVandermonde_
  MODULE PURE SUBROUTINE LagrangeVandermonde1_(xij, order, elemType, ans, &
                                               nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !!  points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! vandermonde matrix
    !! nrows := number of points
    !! ncols := number of dof
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeVandermonde1_
END INTERFACE LagrangeVandermonde_

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build the monomial Vandermonde matrix for an element.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns.
!
! Arguments:
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   degree : INTEGER(I4B), INTENT(IN)(:, :). Monomial exponent table, one
! monomial per row.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$V_{ij}=\prod_{k=1}^{d}x_{ki}^{a_{jk}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL LagrangeVandermonde_(order=2_I4B, elemType=Line, xij=nodes, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 3)-nodes(1, :)**2)) < 1.0E-11_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeVandermonde_
  MODULE PURE SUBROUTINE LagrangeVandermonde2_(xij, degree, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !!  points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
    !! degree of monomials
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! vandermonde matrix
    !! nrows := number of points
    !! ncols := number of dof
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(xij, 2)
    !! ncol = SIZE(degree, 1)
  END SUBROUTINE LagrangeVandermonde2_
END INTERFACE LagrangeVandermonde_

!----------------------------------------------------------------------------
!                                                          EquidistancePoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct equally spaced interpolation points.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Coordinate array with points
! in columns; its role is specified by the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_j=a+(b-a)j/p,\quad 0\le j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: EquidistancePoint
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(1, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = EquidistancePoint(order=2_I4B, elemType=Line, xij=nodes(:, [1, 3]))
! CALL OK(MAXVAL(ABS(y(1, :))) <= 1.0_DFP .AND. &
!   ABS(SUM(y)) < 1.0E-11_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION EquidistancePoint(order, elemType, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of element
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    !! Point, Line, Triangle, Quadrangle, Tetrahedron
    !! Hexahedron, Prism, Pyramid
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of linear elements
    !! Default values:
    !! Biunit line ! Unit triangle ! Biunit Quadrangle ! Unit Tetrahedron
    !! Biunit Hexahedron
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Equidistance points in xij format
    !! Number of rows = nsd
    !! Number of columns = Number of points
    !! The number of points depend upon the order and elemType
  END FUNCTION EquidistancePoint
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct equally spaced interpolation points.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Coordinate array with points
! in columns; its role is specified by the interface.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$x_j=a+(b-a)j/p,\quad 0\le j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: EquidistancePoint_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(1, 3)
! INTEGER(I4B) :: nr, nc
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL EquidistancePoint_(order=2_I4B, elemType=Line, xij=nodes(:, [1, 3]), &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(1, :))) <= 1.0_DFP .AND. &
!   ABS(SUM(y)) < 1.0E-11_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE EquidistancePoint_(order, elemType, ans, nrow, ncol, xij)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of element
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    !! Point, Line, Triangle, Quadrangle, Tetrahedron
    !! Hexahedron, Prism, Pyramid
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Equidistance points in xij format
    !! Number of rows = nsd
    !! Number of columns = Number of points
    !! The number of points depend upon the order and elemType
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of linear elements
    !! Default values:
    !! Biunit line ! Unit triangle ! Biunit Quadrangle ! Unit Tetrahedron
    !! Biunit Hexahedron
  END SUBROUTINE EquidistancePoint_
END INTERFACE

!----------------------------------------------------------------------------
!                                                         InterpolationPoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct interpolation nodes for a selected node family.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. layout is required; use an element supported layout such as
! INCREASING for Line or VEFC for entity ordering.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node selector from
! GlobalData.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Coordinate array with points
! in columns; its role is specified by the interface.
!   layout : CHARACTER(*), INTENT(IN). Required node ordering supported by the
! selected element.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_j=-1+2j/p,\quad 0\le j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE GlobalData, ONLY: Equidistance
! USE LagrangePolynomialUtility, ONLY: InterpolationPoint
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(1, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = InterpolationPoint(order=2_I4B, elemType=Line, ipType=Equidistance, &
!   xij=nodes(:, [1, 3]), layout="INCREASING")
! CALL OK(MAXVAL(ABS(y-nodes)) < 1.0E-11_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION InterpolationPoint(order, elemType, ipType, xij, layout, &
                                     alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, following values are allowed.
    !! Point, Line, Triangle, Quadrangle, Tetrahedron
    !! Hexahedron, Prism, Pyramid
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto,
    !! GaussUltraspherical, GaussUltrasphericalLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of linear elements.
    !! Domain of interpolation, default values are given by:
    !! Biunit line
    !! Unit triangle
    !! Biunit Quadrangle
    !! Unit Tetrahedron
    !! Biunit Hexahedron
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC" Vertex, Edge, Face, Cell
    !! "INCREASING" incresing order
    !! "DECREASING" decreasing order
    !! "XYZ" First X, then Y, then Z
    !! "YXZ" First Y, then X, then Z
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! Jacobi and Ultraspherical parameters
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
  END FUNCTION InterpolationPoint
END INTERFACE

!----------------------------------------------------------------------------
!                                                         InterpolationPoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct interpolation nodes for a selected node family.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. layout is required; use an element supported layout such as
! INCREASING for Line or VEFC for entity ordering.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node selector from
! GlobalData.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   layout : CHARACTER(*), INTENT(IN). Required node ordering supported by the
! selected element.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Coordinate array with points
! in columns; its role is specified by the interface.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$x_j=-1+2j/p,\quad 0\le j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE GlobalData, ONLY: Equidistance
! USE LagrangePolynomialUtility, ONLY: InterpolationPoint_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(1, 3)
! INTEGER(I4B) :: nr, nc
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL InterpolationPoint_(order=2_I4B, elemType=Line, ipType=Equidistance, &
!   xij=nodes(:, [1, 3]), layout="INCREASING", &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y-nodes)) < 1.0E-11_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE
 MODULE SUBROUTINE InterpolationPoint_(order, elemType, ipType, xij, layout, &
                                        alpha, beta, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, following values are allowed.
    !! Point, Line, Triangle, Quadrangle, Tetrahedron
    !! Hexahedron, Prism, Pyramid
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto,
    !! GaussUltraspherical, GaussUltrasphericalLobatto
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolation points in xij format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! the number of rows and cols written in ans
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC" Vertex, Edge, Face, Cell
    !! "INCREASING" incresing order
    !! "DECREASING" decreasing order
    !! "XYZ" First X, then Y, then Z
    !! "YXZ" First Y, then X, then Z
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of linear elements.
    !! Domain of interpolation, default values are given by:
    !! Biunit line
    !! Unit triangle
    !! Biunit Quadrangle
    !! Unit Tetrahedron
    !! Biunit Hexahedron
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! Jacobi and Ultraspherical parameters
  END SUBROUTINE InterpolationPoint_
END INTERFACE

!----------------------------------------------------------------------------
!                                                           LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. The i overload computes one basis column; isVandermonde
! accepts an existing Vandermonde matrix; ipiv overloads require its LU
! factorization and pivots.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   i : INTEGER(I4B), INTENT(IN). One based index of the nodal basis function
! to construct.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2)).
!
! Formula:
! $$VC=I,\quad \ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeCoeff
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LagrangeCoeff(order=2_I4B, elemType=Line, xij=nodes)
! CALL OK(MAXVAL(ABS(MATMUL( &
!   LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
!   y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff
  MODULE FUNCTION LagrangeCoeff1(order, elemType, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff1
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                           LagrangeCoeff_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. The i overload computes one basis column; isVandermonde
! accepts an existing Vandermonde matrix; ipiv overloads require its LU
! factorization and pivots.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   i : INTEGER(I4B), INTENT(IN). One based index of the nodal basis function
! to construct.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes ans(:), tsize.
!
! Formula:
! $$VC=I,\quad \ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeCoeff_
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL LagrangeCoeff_(order=2_I4B, elemType=Line, xij=nodes, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(MATMUL( &
!   LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
!   y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_
  MODULE SUBROUTINE LagrangeCoeff1_(order, elemType, i, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff1_
END INTERFACE LagrangeCoeff_

!----------------------------------------------------------------------------
!                                                           LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. The i overload computes one basis column; isVandermonde
! accepts an existing Vandermonde matrix; ipiv overloads require its LU
! factorization and pivots.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), SIZE(xij, 2)).
!
! Formula:
! $$VC=I,\quad \ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeCoeff
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LagrangeCoeff(order=2_I4B, elemType=Line, xij=nodes)
! CALL OK(MAXVAL(ABS(MATMUL( &
!   LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
!   y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff
  MODULE FUNCTION LagrangeCoeff2(order, elemType, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff2
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                           LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. The i overload computes one basis column; isVandermonde
! accepts an existing Vandermonde matrix; ipiv overloads require its LU
! factorization and pivots.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$VC=I,\quad \ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeCoeff_
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL LagrangeCoeff_(order=2_I4B, elemType=Line, xij=nodes, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(MATMUL( &
!   LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
!   y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_
  MODULE SUBROUTINE LagrangeCoeff2_(order, elemType, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff2_
END INTERFACE LagrangeCoeff_

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. The i overload computes one basis column; isVandermonde
! accepts an existing Vandermonde matrix; ipiv overloads require its LU
! factorization and pivots.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   i : INTEGER(I4B), INTENT(IN). One based index of the nodal basis function
! to construct.
!   v : REAL(DFP), INTENT(IN)(:, :). Vandermonde matrix, or its LU factors
! when ipiv is supplied.
!   isVandermonde : LOGICAL(LGT), INTENT(IN). Indicates that v is a
! Vandermonde matrix.
!
! Returns:
!   ans : REAL(DFP)(SIZE(v, 1)).
!
! Formula:
! $$VC=I,\quad \ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeCoeff
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LagrangeCoeff(order=2_I4B, elemType=Line, xij=nodes)
! CALL OK(MAXVAL(ABS(MATMUL( &
!   LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
!   y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff
  MODULE FUNCTION LagrangeCoeff3(order, elemType, i, v, &
                                 isVandermonde) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff3
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. The i overload computes one basis column; isVandermonde
! accepts an existing Vandermonde matrix; ipiv overloads require its LU
! factorization and pivots.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   i : INTEGER(I4B), INTENT(IN). One based index of the nodal basis function
! to construct.
!   v : REAL(DFP), INTENT(IN)(:, :). Vandermonde matrix, or its LU factors
! when ipiv is supplied.
!   isVandermonde : LOGICAL(LGT), INTENT(IN). Indicates that v is a
! Vandermonde matrix.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes ans(:), tsize.
!
! Formula:
! $$VC=I,\quad \ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeCoeff_
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL LagrangeCoeff_(order=2_I4B, elemType=Line, xij=nodes, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(MATMUL( &
!   LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
!   y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_
  MODULE SUBROUTINE LagrangeCoeff3_(order, elemType, i, v, &
                                    isVandermonde, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(v, 1))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff3_
END INTERFACE LagrangeCoeff_

!----------------------------------------------------------------------------
!                                                              LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. The i overload computes one basis column; isVandermonde
! accepts an existing Vandermonde matrix; ipiv overloads require its LU
! factorization and pivots.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   i : INTEGER(I4B), INTENT(IN). One based index of the nodal basis function
! to construct.
!   v : REAL(DFP), INTENT(INOUT)(:, :). Vandermonde matrix, or its LU factors
! when ipiv is supplied.
!   ipiv : INTEGER(I4B), INTENT(IN)(:). Pivot indices from the LU
! factorization of v.
!
! Returns:
!   ans : REAL(DFP)(SIZE(v, 1)).
!
! Formula:
! $$VC=I,\quad \ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeCoeff
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LagrangeCoeff(order=2_I4B, elemType=Line, xij=nodes)
! CALL OK(MAXVAL(ABS(MATMUL( &
!   LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
!   y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff
  MODULE FUNCTION LagrangeCoeff4(order, elemType, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff4
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                              LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. The i overload computes one basis column; isVandermonde
! accepts an existing Vandermonde matrix; ipiv overloads require its LU
! factorization and pivots.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   i : INTEGER(I4B), INTENT(IN). One based index of the nodal basis function
! to construct.
!   v : REAL(DFP), INTENT(INOUT)(:, :). Vandermonde matrix, or its LU factors
! when ipiv is supplied.
!   ipiv : INTEGER(I4B), INTENT(IN)(:). Pivot indices from the LU
! factorization of v.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes v(:, :), ans(:), tsize.
!
! Formula:
! $$VC=I,\quad \ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE LagrangePolynomialUtility, ONLY: LagrangeCoeff_
! USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL LagrangeCoeff_(order=2_I4B, elemType=Line, xij=nodes, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(MATMUL( &
!   LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
!   y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Interpolation construction")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_
  MODULE SUBROUTINE LagrangeCoeff4_(order, elemType, i, v, ipiv, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(v, 1))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff4_
END INTERFACE LagrangeCoeff_

!----------------------------------------------------------------------------
!                                                           LagrangeEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the nodal Lagrange basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. xij is INOUT and can be transformed internally. To reuse
! coeff, initialize firstCall=.TRUE. for the first call, then reuse the
! resulting cache only with the same nodes, order and basis.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation coordinate(s).
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional cached basis coefficient matrix; initialize with firstCall.
!   firstCall : LOGICAL(LGT), OPTIONAL. Initialize to .TRUE. when constructing
! a new coefficient cache.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family selector
! from GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x, 2), SIZE(xij, 2)).
!
! Formula:
! $$\ell_j(x_i)=\delta_{ij},\quad \sum_j\ell_j(x)=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: poly
! USE LagrangePolynomialUtility, ONLY: LagrangeEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! REAL(DFP) :: coeff(3, 3)
! LOGICAL(LGT) :: firstCall
! REAL(DFP) :: points(1, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! firstCall = .TRUE.
! coeff = 0.0_DFP
! points = nodes
! y = LagrangeEvalAll(order=2_I4B, elemType=Line, x=points, xij=nodes, &
!   domainName="BIUNIT", coeff=coeff, firstCall=firstCall, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y-RESHAPE( &
!   [1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Nodal interpolation identity")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll
  MODULE FUNCTION LagrangeEvalAll1(order, elemType, x, xij, domainName, &
                 coeff, firstCall, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll1
END INTERFACE LagrangeEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the nodal Lagrange basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. xij is INOUT and can be transformed internally. To reuse
! coeff, initialize firstCall=.TRUE. for the first call, then reuse the
! resulting cache only with the same nodes, order and basis.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation coordinate(s).
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(:, :). Optional cached basis
! coefficient matrix; initialize with firstCall.
!   firstCall : LOGICAL(LGT), OPTIONAL. Initialize to .TRUE. when constructing
! a new coefficient cache.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family selector
! from GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   Writes xij(:, :), ans(:, :), nrow, ncol, coeff(:, :).
!
! Formula:
! $$\ell_j(x_i)=\delta_{ij},\quad \sum_j\ell_j(x)=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: poly
! USE LagrangePolynomialUtility, ONLY: LagrangeEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
! REAL(DFP) :: coeff(3, 3)
! LOGICAL(LGT) :: firstCall
! REAL(DFP) :: points(1, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! firstCall = .TRUE.
! coeff = 0.0_DFP
! points = nodes
! CALL LagrangeEvalAll_(order=2_I4B, elemType=Line, x=points, xij=nodes, &
!   domainName="BIUNIT", coeff=coeff, firstCall=firstCall, &
!   basisType=poly%Monomial, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y-RESHAPE( &
!   [1.0_DFP, 0.0_DFP, 0.0_DFP, &
!   0.0_DFP, 1.0_DFP, 0.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
!   "Nodal interpolation identity")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_
  MODULE SUBROUTINE LagrangeEvalAll1_(order, elemType, x, xij, ans, &
     nrow, ncol, domainName, coeff, firstCall, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x, 2)
    !! ncol = SIZE(xij, 2)
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE LagrangeEvalAll1_
END INTERFACE LagrangeEvalAll_

!----------------------------------------------------------------------------
!                                                           LagrangeEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the nodal Lagrange basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. xij is INOUT and can be transformed internally. To reuse
! coeff, initialize firstCall=.TRUE. for the first call, then reuse the
! resulting cache only with the same nodes, order and basis.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation coordinate(s).
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional cached basis coefficient matrix; initialize with firstCall.
!   firstCall : LOGICAL(LGT), OPTIONAL. Initialize to .TRUE. when constructing
! a new coefficient cache.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family selector
! from GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x, 2), SIZE(xij, 2), SIZE(x, 1)).
!
! Formula:
! $$\nabla\ell_j(x)=\sum_k C_{kj}\nabla m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: poly
! USE LagrangePolynomialUtility, ONLY: LagrangeGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3, 1)
! REAL(DFP) :: coeff(3, 3)
! LOGICAL(LGT) :: firstCall
! REAL(DFP) :: points(1, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! firstCall = .TRUE.
! coeff = 0.0_DFP
! points = nodes
! y = LagrangeGradientEvalAll(order=2_I4B, elemType=Line, x=points, &
!   xij=nodes, &
!   domainName="BIUNIT", coeff=coeff, firstCall=firstCall, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(SUM(y(:, :, 1), DIM=2))) < 1.0E-10_DFP, &
!   "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll
  MODULE FUNCTION LagrangeGradientEvalAll1(order, elemType, x, xij, &
     domainName, coeff, firstCall, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), SIZE(x, 1))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeGradientEvalAll1
END INTERFACE LagrangeGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the nodal Lagrange basis.
!
! Points are columns of coordinate arrays; basis values use point rows and
! basis columns. xij is INOUT and can be transformed internally. To reuse
! coeff, initialize firstCall=.TRUE. for the first call, then reuse the
! resulting cache only with the same nodes, order and basis.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation coordinate(s).
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result storage; allocate enough
! space for all active dimensions.
!   dim1 : INTEGER(I4B), INTENT(OUT). Active first dimension (evaluation
! points).
!   dim2 : INTEGER(I4B), INTENT(OUT). Active second dimension (basis
! functions).
!   dim3 : INTEGER(I4B), INTENT(OUT). Active third dimension (coordinate
! directions).
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional cached basis coefficient matrix; initialize with firstCall.
!   firstCall : LOGICAL(LGT), OPTIONAL. Initialize to .TRUE. when constructing
! a new coefficient cache.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family selector
! from GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   Writes xij(:, :), ans(:, :, :), dim1, dim2, dim3, coeff(SIZE(xij, 2),
! SIZE(xij, 2)).
!
! Formula:
! $$\nabla\ell_j(x)=\sum_k C_{kj}\nabla m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: poly
! USE LagrangePolynomialUtility, ONLY: LagrangeGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1, 3)
! REAL(DFP) :: y(3, 3, 1)
! INTEGER(I4B) :: nr, nc, nd
! REAL(DFP) :: coeff(3, 3)
! LOGICAL(LGT) :: firstCall
! REAL(DFP) :: points(1, 3)
!
! nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! firstCall = .TRUE.
! coeff = 0.0_DFP
! points = nodes
! CALL LagrangeGradientEvalAll_(order=2_I4B, elemType=Line, x=points, &
!   xij=nodes, &
!   domainName="BIUNIT", coeff=coeff, firstCall=firstCall, &
!   basisType=poly%Monomial, &
!   ans=y, dim1=nr, dim2=nc, dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(y(:, :, 1), DIM=2))) < 1.0E-10_DFP, &
!   "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll_
  MODULE SUBROUTINE LagrangeGradientEvalAll1_(order, elemType, x, xij, ans, &
           dim1, dim2, dim3, domainName, coeff, firstCall, basisType, alpha, &
                                              beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Value of n+1 Lagrange polynomials at point x
    !! dim1 =  SIZE(x, 2)
    !! dim2 = SIZE(xij, 2)
    !! dim3 = SIZE(x, 1)
    !! ans(:, :, 1) denotes x gradient
    !! ans(:,:, 2) denotes y gradient
    !! ans(:,:, 3) denotes z gradient
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! data written in ans
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE LagrangeGradientEvalAll1_
END INTERFACE LagrangeGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LagrangePolynomialUtility
