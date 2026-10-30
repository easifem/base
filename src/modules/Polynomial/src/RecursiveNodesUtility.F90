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
! summary: Recursive interpolation nodes on lines, triangles and tetrahedra.
!
! Nodes are columns. Default barycentric output has d+1 rows; Cartesian
! domains have d rows. Coordinate mapping helpers declared in this module are
! private. Use positive orders up to 99 with the fixed internal work arrays.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$N=\binom{p+d}{d},\quad\sum_{i=1}^{d+1}\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE RecursiveNodesUtility, ONLY: RecursiveNode2D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:, :)
!
! nodes = RecursiveNode2D(order=2_I4B, ipType=Equidistance, &
!   domain="BARYCENTRIC")
! CALL OK(ALL(SHAPE(nodes) == [3, 6]), "Node dimensions")
! CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Barycentric coordinates sum to one")
! CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
! END PROGRAM main
!```

MODULE RecursiveNodesUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: RecursiveNode1D
PUBLIC :: RecursiveNode2D
PUBLIC :: RecursiveNode3D

PUBLIC :: RecursiveNode1D_
PUBLIC :: RecursiveNode2D_
PUBLIC :: RecursiveNode3D_

!----------------------------------------------------------------------------
!                                                           RecursiveNode1D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct recursive nodes on a 1D simplex.
!
! The default domain is BARYCENTRIC with d+1 coordinate rows; UNIT, BIUNIT and
! EQUILATERAL use d rows. Use positive order no larger than 99 because
! internal work arrays have fixed capacity. ipType selects the underlying line
! node family.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node selector from
! GlobalData.
!   domain : CHARACTER(*), OPTIONAL, INTENT(IN). Output coordinate domain;
! BARYCENTRIC is the default.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$N=\binom{p+d}{d},\quad \sum_{i=1}^{d+1}\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE RecursiveNodesUtility, ONLY: RecursiveNode1D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:, :)
!
! nodes = RecursiveNode1D(order=2_I4B, ipType=Equidistance, &
!   domain="BARYCENTRIC")
! CALL OK(ALL(SHAPE(nodes) == [2, 3]), "Node dimensions")
! CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Barycentric coordinates sum to one")
! CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION RecursiveNode1D(order, ipType, domain, alpha, beta, &
                                  lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order >= 0
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance
    !! LobattoGaussJacobi
    !! LobattoGaussChebyshev
    !! LobattoGaussGegenbauer
    !! GaussJacobi
    !! GaussChebyshev
    !! GaussGegenbauer
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! barycentric coordinates, in xiJ format
    !! size(ans,1) = 2 corresponding to b0 and b1
    !! size(ans,2) total number of points
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domain
    !! unit (0,1)
    !! biunit (-1, 1)
    !! equilateral
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END FUNCTION RecursiveNode1D
END INTERFACE

!----------------------------------------------------------------------------
!                                                           RecursiveNode1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct recursive nodes on a 1D simplex.
!
! The default domain is BARYCENTRIC with d+1 coordinate rows; UNIT, BIUNIT and
! EQUILATERAL use d rows. Use positive order no larger than 99 because
! internal work arrays have fixed capacity. ipType selects the underlying line
! node family.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node selector from
! GlobalData.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   domain : CHARACTER(*), OPTIONAL, INTENT(IN). Output coordinate domain;
! BARYCENTRIC is the default.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$N=\binom{p+d}{d},\quad \sum_{i=1}^{d+1}\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE RecursiveNodesUtility, ONLY: RecursiveNode1D_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2, 3)
! INTEGER(I4B) :: nr, nc
!
! CALL RecursiveNode1D_(order=2_I4B, ipType=Equidistance, &
!   domain="BARYCENTRIC", ans=nodes, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 3, "Active node dimensions")
! CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Barycentric coordinates sum to one")
! CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE RecursiveNode1D_(order, ipType, domain, alpha, beta, &
                                     lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order >= 0
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance
    !! LobattoGaussJacobi
    !! LobattoGaussChebyshev
    !! LobattoGaussGegenbauer
    !! GaussJacobi
    !! GaussChebyshev
    !! GaussGegenbauer
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! barycentric coordinates, in xiJ format
    !! size(ans,1) = 2 corresponding to b0 and b1
    !! size(ans,2) total number of points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of ans
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domain
    !! unit (0,1)
    !! biunit (-1, 1)
    !! equilateral
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE RecursiveNode1D_
END INTERFACE

!----------------------------------------------------------------------------
!                                                            RecursiveNode2D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct recursive nodes on a 2D simplex.
!
! The default domain is BARYCENTRIC with d+1 coordinate rows; UNIT, BIUNIT and
! EQUILATERAL use d rows. Use positive order no larger than 99 because
! internal work arrays have fixed capacity. ipType selects the underlying line
! node family.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node selector from
! GlobalData.
!   domain : CHARACTER(*), OPTIONAL, INTENT(IN). Output coordinate domain;
! BARYCENTRIC is the default.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$N=\binom{p+d}{d},\quad \sum_{i=1}^{d+1}\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE RecursiveNodesUtility, ONLY: RecursiveNode2D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:, :)
!
! nodes = RecursiveNode2D(order=2_I4B, ipType=Equidistance, &
!   domain="BARYCENTRIC")
! CALL OK(ALL(SHAPE(nodes) == [3, 6]), "Node dimensions")
! CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Barycentric coordinates sum to one")
! CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION RecursiveNode2D(order, ipType, domain, alpha, beta, &
                                  lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order >= 0
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance
    !! LobattoGaussJacobi
    !! LobattoGaussChebyshev
    !! LobattoGaussGegenbauer
    !! GaussJacobi
    !! GaussChebyshev
    !! GaussGegenbauer
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! barycentric coordinates, in xiJ format
    !! size(ans,1) = 3 corresponding to b0, b1, b2
    !! size(ans,2) total number of points
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domain
    !! unit
    !! Biunit
    !! Equilateral
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END FUNCTION RecursiveNode2D
END INTERFACE

!----------------------------------------------------------------------------
!                                                          RecursiveNode2D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct recursive nodes on a 2D simplex.
!
! The default domain is BARYCENTRIC with d+1 coordinate rows; UNIT, BIUNIT and
! EQUILATERAL use d rows. Use positive order no larger than 99 because
! internal work arrays have fixed capacity. ipType selects the underlying line
! node family.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node selector from
! GlobalData.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   domain : CHARACTER(*), OPTIONAL, INTENT(IN). Output coordinate domain;
! BARYCENTRIC is the default.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$N=\binom{p+d}{d},\quad \sum_{i=1}^{d+1}\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE RecursiveNodesUtility, ONLY: RecursiveNode2D_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3, 6)
! INTEGER(I4B) :: nr, nc
!
! CALL RecursiveNode2D_(order=2_I4B, ipType=Equidistance, &
!   domain="BARYCENTRIC", ans=nodes, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 6, "Active node dimensions")
! CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Barycentric coordinates sum to one")
! CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE RecursiveNode2D_(order, ipType, ans, nrow, ncol, &
                                     domain, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order >= 0
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance
    !! LobattoGaussJacobi
    !! LobattoGaussChebyshev
    !! LobattoGaussGegenbauer
    !! GaussJacobi
    !! GaussChebyshev
    !! GaussGegenbauer
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! barycentric coordinates, in xiJ format
    !! size(ans,1) = 3 corresponding to b0, b1, b2
    !! size(ans,2) total number of points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of ans
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domain
    !! unit
    !! Biunit
    !! Equilateral
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE RecursiveNode2D_
END INTERFACE

!----------------------------------------------------------------------------
!                                                            RecursiveNode3D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct recursive nodes on a 3D simplex.
!
! The default domain is BARYCENTRIC with d+1 coordinate rows; UNIT, BIUNIT and
! EQUILATERAL use d rows. Use positive order no larger than 99 because
! internal work arrays have fixed capacity. ipType selects the underlying line
! node family.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node selector from
! GlobalData.
!   domain : CHARACTER(*), OPTIONAL, INTENT(IN). Output coordinate domain;
! BARYCENTRIC is the default.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$N=\binom{p+d}{d},\quad \sum_{i=1}^{d+1}\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE RecursiveNodesUtility, ONLY: RecursiveNode3D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:, :)
!
! nodes = RecursiveNode3D(order=2_I4B, ipType=Equidistance, &
!   domain="BARYCENTRIC")
! CALL OK(ALL(SHAPE(nodes) == [4, 10]), "Node dimensions")
! CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Barycentric coordinates sum to one")
! CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION RecursiveNode3D(order, ipType, domain, alpha, beta, &
                                  lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order >= 0
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance
    !! LobattoGaussJacobi
    !! LobattoGaussChebyshev
    !! LobattoGaussGegenbauer
    !! GaussJacobi
    !! GaussChebyshev
    !! GaussGegenbauer
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! barycentric coordinates, in xiJ format
    !! size(ans,1) = 4 corresponding to b0, b1, b2, b3
    !! size(ans,2) total number of points
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domain
    !! unit
    !! Biunit
    !! Equilateral
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END FUNCTION RecursiveNode3D
END INTERFACE

!----------------------------------------------------------------------------
!                                                          RecursiveNode3D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct recursive nodes on a 3D simplex.
!
! The default domain is BARYCENTRIC with d+1 coordinate rows; UNIT, BIUNIT and
! EQUILATERAL use d rows. Use positive order no larger than 99 because
! internal work arrays have fixed capacity. ipType selects the underlying line
! node family.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node selector from
! GlobalData.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   domain : CHARACTER(*), OPTIONAL, INTENT(IN). Output coordinate domain;
! BARYCENTRIC is the default.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$N=\binom{p+d}{d},\quad \sum_{i=1}^{d+1}\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE RecursiveNodesUtility, ONLY: RecursiveNode3D_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(4, 10)
! INTEGER(I4B) :: nr, nc
!
! CALL RecursiveNode3D_(order=2_I4B, ipType=Equidistance, &
!   domain="BARYCENTRIC", ans=nodes, nrow=nr, ncol=nc)
! CALL OK(nr == 4 .AND. nc == 10, "Active node dimensions")
! CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Barycentric coordinates sum to one")
! CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE RecursiveNode3D_(order, ipType, ans, nrow, ncol, &
                                     domain, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order >= 0
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance
    !! LobattoGaussJacobi
    !! LobattoGaussChebyshev
    !! LobattoGaussGegenbauer
    !! GaussJacobi
    !! GaussChebyshev
    !! GaussGegenbauer
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! barycentric coordinates, in xiJ format
    !! size(ans,1) = 4 corresponding to b0, b1, b2, b3
    !! size(ans,2) total number of points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of ans
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domain
    !! unit
    !! Biunit
    !! Equilateral
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE RecursiveNode3D_
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ToUnit(x, domain) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:, :)
    CHARACTER(*), INTENT(IN) :: domain
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ToUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE ToUnit_(x, domain, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: x(:, :)
    CHARACTER(*), INTENT(IN) :: domain
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE ToUnit_
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION FromUnit(x, domain) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:, :)
    CHARACTER(*), INTENT(IN) :: domain
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION FromUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE FromUnit_(x, domain, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: x(:, :)
    CHARACTER(*), INTENT(IN) :: domain
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE FromUnit_
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE Unit2Equilateral(d, x)
    INTEGER(I4B), INTENT(IN) :: d
    REAL(DFP), INTENT(INOUT) :: x(:, :)
  END SUBROUTINE Unit2Equilateral
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE Equilateral2Unit(d, x)
    INTEGER(I4B), INTENT(IN) :: d
    REAL(DFP), INTENT(INOUT) :: x(:, :)
  END SUBROUTINE Equilateral2Unit
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Coord_Map
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Coord_Map(x, from, to) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:, :)
    CHARACTER(*), INTENT(IN) :: from
    CHARACTER(*), INTENT(IN) :: to
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION Coord_Map
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Coord_Map
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE Coord_Map_(x, from, to, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: x(:, :)
    CHARACTER(*), INTENT(IN) :: from
    CHARACTER(*), INTENT(IN) :: to
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Coord_Map_
END INTERFACE

END MODULE RecursiveNodesUtility
