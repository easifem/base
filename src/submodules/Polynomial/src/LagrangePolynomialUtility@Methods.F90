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

SUBMODULE(LagrangePolynomialUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         LagrangeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF
SELECT CASE (elemType)
CASE (Point)
  ans = 1
CASE (Line)
  ans = LagrangeDOF_Line(order=order)
CASE (Triangle)
  ans = LagrangeDOF_Triangle(order=order)
CASE (Quadrangle)
  ans = LagrangeDOF_Quadrangle(order=order)
CASE (Tetrahedron)
  ans = LagrangeDOF_Tetrahedron(order=order)
CASE (Hexahedron)
  ans = LagrangeDOF_Hexahedron(order=order)
CASE (Prism)
  ans = LagrangeDOF_Prism(order=order)
CASE (Pyramid)
  ans = LagrangeDOF_Pyramid(order=order)
END SELECT
END PROCEDURE LagrangeDOF

!----------------------------------------------------------------------------
!                                                             LagrangeInDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF
SELECT CASE (elemType)
CASE (Point)
  ans = 0
CASE (Line)
  ans = LagrangeInDOF_Line(order=order)
CASE (Triangle)
  ans = LagrangeInDOF_Triangle(order=order)
CASE (Quadrangle)
  ans = LagrangeInDOF_Quadrangle(order=order)
CASE (Tetrahedron)
  ans = LagrangeInDOF_Tetrahedron(order=order)
CASE (Hexahedron)
  ans = LagrangeInDOF_Hexahedron(order=order)
CASE (Prism)
  ans = LagrangeInDOF_Prism(order=order)
CASE (Pyramid)
  ans = LagrangeInDOF_Pyramid(order=order)
END SELECT
END PROCEDURE LagrangeInDOF

!----------------------------------------------------------------------------
!                                                             LagrangeDegree
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree
SELECT CASE (elemType)
CASE (Point)
  ALLOCATE (ans(0, 0))
CASE (Line)
  ans = LagrangeDegree_Line(order=order)
CASE (Triangle)
  ans = LagrangeDegree_Triangle(order=order)
CASE (Quadrangle)
  ans = LagrangeDegree_Quadrangle(order=order)
CASE (Tetrahedron)
  ans = LagrangeDegree_Tetrahedron(order=order)
CASE (Hexahedron)
  ans = LagrangeDegree_Hexahedron(order=order)
CASE (Prism)
  ans = LagrangeDegree_Prism(order=order)
CASE (Pyramid)
  ans = LagrangeDegree_Pyramid(order=order)
END SELECT
END PROCEDURE LagrangeDegree

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeVandermonde
INTEGER(I4B), ALLOCATABLE :: degree(:, :)
REAL(DFP), ALLOCATABLE :: x0(:), y0(:), z0(:)
INTEGER(I4B) :: m, n, jj, nsd
  !!
degree = TRANSPOSE(LagrangeDegree(order=order, elemType=elemType))
  !!
m = SIZE(xij, 2)
nsd = SIZE(degree, 1)
n = SIZE(degree, 2)
ALLOCATE (ans(m, n))
  !!
SELECT CASE (nsd)
CASE (1)
  x0 = xij(1, :)
  DO jj = 1, n
    ans(:, jj) = x0**degree(1, jj)
  END DO
CASE (2)
  x0 = xij(1, :)
  y0 = xij(2, :)
  DO jj = 1, n
    ans(:, jj) = x0**degree(1, jj) * y0**degree(2, jj)
  END DO
CASE (3)
  x0 = xij(1, :)
  y0 = xij(2, :)
  z0 = xij(3, :)
  DO jj = 1, n
    ans(:, jj) = x0**degree(1, jj) * y0**degree(2, jj) * z0**degree(3, jj)
  END DO
END SELECT
  !!
IF (ALLOCATED(degree)) DEALLOCATE (degree)
IF (ALLOCATED(x0)) DEALLOCATE (x0)
IF (ALLOCATED(y0)) DEALLOCATE (y0)
IF (ALLOCATED(z0)) DEALLOCATE (z0)
  !!
END PROCEDURE LagrangeVandermonde

!----------------------------------------------------------------------------
!                                                          EquidistancePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint
SELECT CASE (elemType)
CASE (Point)
  IF (PRESENT(xij)) THEN
    ans = xij
  ELSE
    ALLOCATE (ans(0, 0))
  END IF
CASE (Line)
  ans = EquidistancePoint_Line(order=order, xij=xij)
CASE (Triangle)
  ans = EquidistancePoint_Triangle(order=order, xij=xij)
CASE (Quadrangle)
  ans = EquidistancePoint_Quadrangle(order=order, xij=xij)
CASE (Tetrahedron)
  ans = EquidistancePoint_Tetrahedron(order=order, xij=xij)
CASE (Hexahedron)
  ans = EquidistancePoint_Hexahedron(order=order, xij=xij)
CASE (Prism)
  ans = EquidistancePoint_Prism(order=order, xij=xij)
CASE (Pyramid)
  ans = EquidistancePoint_Pyramid(order=order, xij=xij)
END SELECT
END PROCEDURE EquidistancePoint

!----------------------------------------------------------------------------
!                                                         InterpolationPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint
SELECT CASE (elemType)
CASE (Point)
  IF (PRESENT(xij)) THEN
    ans = xij
  ELSE
    ALLOCATE (ans(0, 0))
  END IF
CASE (Line)
  ans = InterpolationPoint_Line(order=order, ipType=ipType, xij=xij, &
    & layout=layout)
CASE (Triangle)
  ans = InterpolationPoint_Triangle(order=order, ipType=ipType, xij=xij, &
    & layout=layout)
CASE (Quadrangle)
  ans = InterpolationPoint_Quadrangle(order=order, ipType=ipType, xij=xij, &
    & layout=layout)
CASE (Tetrahedron)
  ans = InterpolationPoint_Tetrahedron(order=order, ipType=ipType, xij=xij, &
    & layout=layout)
CASE (Hexahedron)
  ans = InterpolationPoint_Hexahedron(order=order, ipType=ipType, xij=xij, &
    &layout=layout)
CASE (Prism)
  ans = InterpolationPoint_Prism(order=order, ipType=ipType, xij=xij, &
    & layout=layout)
CASE (Pyramid)
  ans = InterpolationPoint_Pyramid(order=order, ipType=ipType, xij=xij, &
  & layout=layout)
END SELECT
END PROCEDURE InterpolationPoint

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff1
SELECT CASE (elemType)
CASE (Point)
  !!
CASE (Line)
  ans = LagrangeCoeff_Line(order=order, xij=xij, i=i)
CASE (Triangle)
  ans = LagrangeCoeff_Triangle(order=order, xij=xij, i=i)
CASE (Quadrangle)
  ans = LagrangeCoeff_Quadrangle(order=order, xij=xij, i=i)
CASE (Tetrahedron)
  ans = LagrangeCoeff_Tetrahedron(order=order, xij=xij, i=i)
CASE (Hexahedron)
  ans = LagrangeCoeff_Hexahedron(order=order, xij=xij, i=i)
CASE (Prism)
  ans = LagrangeCoeff_Prism(order=order, xij=xij, i=i)
CASE (Pyramid)
  ans = LagrangeCoeff_Pyramid(order=order, xij=xij, i=i)
END SELECT
END PROCEDURE LagrangeCoeff1

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff2
SELECT CASE (elemType)
CASE (Point)
  !!
CASE (Line)
  ans = LagrangeCoeff_Line(order=order, xij=xij)
CASE (Triangle)
  ans = LagrangeCoeff_Triangle(order=order, xij=xij)
CASE (Quadrangle)
  ans = LagrangeCoeff_Quadrangle(order=order, xij=xij)
CASE (Tetrahedron)
  ans = LagrangeCoeff_Tetrahedron(order=order, xij=xij)
CASE (Hexahedron)
  ans = LagrangeCoeff_Hexahedron(order=order, xij=xij)
CASE (Prism)
  ans = LagrangeCoeff_Prism(order=order, xij=xij)
CASE (Pyramid)
  ans = LagrangeCoeff_Pyramid(order=order, xij=xij)
END SELECT
  !!
END PROCEDURE LagrangeCoeff2

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff3
SELECT CASE (elemType)
CASE (Point)
  !!
CASE (Line)
  ans = LagrangeCoeff_Line(order=order, i=i, v=v, isVandermonde=.TRUE.)
CASE (Triangle)
  ans = LagrangeCoeff_Triangle(order=order, i=i, v=v, isVandermonde=.TRUE.)
CASE (Quadrangle)
  ans = LagrangeCoeff_Quadrangle(order=order, i=i, v=v, isVandermonde=.TRUE.)
CASE (Tetrahedron)
  ans = LagrangeCoeff_Tetrahedron(order=order, i=i, v=v, isVandermonde=.TRUE.)
CASE (Hexahedron)
  ans = LagrangeCoeff_Hexahedron(order=order, i=i, v=v, isVandermonde=.TRUE.)
CASE (Prism)
  ans = LagrangeCoeff_Prism(order=order, i=i, v=v, isVandermonde=.TRUE.)
CASE (Pyramid)
  ans = LagrangeCoeff_Pyramid(order=order, i=i, v=v, isVandermonde=.TRUE.)
END SELECT
  !!
END PROCEDURE LagrangeCoeff3

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff4
SELECT CASE (elemType)
CASE (Point)
  !!
CASE (Line)
  ans = LagrangeCoeff_Line(order=order, i=i, v=v, ipiv=ipiv)
CASE (Triangle)
  ans = LagrangeCoeff_Triangle(order=order, i=i, v=v, ipiv=ipiv)
CASE (Quadrangle)
  ans = LagrangeCoeff_Quadrangle(order=order, i=i, v=v, ipiv=ipiv)
CASE (Tetrahedron)
  ans = LagrangeCoeff_Tetrahedron(order=order, i=i, v=v, ipiv=ipiv)
CASE (Hexahedron)
  ans = LagrangeCoeff_Hexahedron(order=order, i=i, v=v, ipiv=ipiv)
CASE (Prism)
  ans = LagrangeCoeff_Prism(order=order, i=i, v=v, ipiv=ipiv)
CASE (Pyramid)
  ans = LagrangeCoeff_Pyramid(order=order, i=i, v=v, ipiv=ipiv)
END SELECT
  !!
END PROCEDURE LagrangeCoeff4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods