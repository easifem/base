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

MODULE HexahedronInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDegree_Hexahedron
PUBLIC :: LagrangeDOF_Hexahedron
PUBLIC :: LagrangeInDOF_Hexahedron
PUBLIC :: EquidistancePoint_Hexahedron
PUBLIC :: EquidistanceInPoint_Hexahedron
PUBLIC :: InterpolationPoint_Hexahedron
PUBLIC :: LagrangeCoeff_Hexahedron
PUBLIC :: RefHexahedronCoord
PUBLIC :: EdgeConnectivity_Hexahedron
PUBLIC :: FacetConnectivity_Hexahedron
PUBLIC :: QuadratureNumber_Hexahedron
! PUBLIC :: TensorProdBasis_Hexahedron
! PUBLIC :: VertexBasis_Hexahedron
! PUBLIC :: xEdgeBasis_Hexahedron
! PUBLIC :: yEdgeBasis_Hexahedron
! PUBLIC :: zEdgeBasis_Hexahedron
! PUBLIC :: EdgeBasis_Hexahedron
! PUBLIC :: xFacetBasis_Hexahedron
! PUBLIC :: yFacetBasis_Hexahedron
! PUBLIC :: zFacetBasis_Hexahedron
! PUBLIC :: FacetBasis_Hexahedron
! PUBLIC :: CellBasis_Hexahedron
! PUBLIC :: HeirarchicalBasis_Hexahedron
! PUBLIC :: LagrangeEvalAll_Hexahedron
! PUBLIC :: QuadraturePoint_Hexahedron

!----------------------------------------------------------------------------
!                                                 QuadratureNumber_Quadrangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION QuadratureNumber_Hexahedron( &
    & p,  &
    & q,  &
    & r,  &
    & quadType1,  &
    & quadType2,  &
    & quadType3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2, quadType3
    INTEGER(I4B) :: ans(3)
  END FUNCTION QuadratureNumber_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-07
! summary:  This function returns the edge connectivity of Hexahedron

INTERFACE
  MODULE PURE FUNCTION FacetConnectivity_Hexahedron() RESULT(ans)
    INTEGER(I4B) :: ans(4, 6)
  END FUNCTION FacetConnectivity_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               EdgeConnectivity_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-07
! summary:  This function returns the edge connectivity of Hexahedron

INTERFACE
  MODULE PURE FUNCTION EdgeConnectivity_Hexahedron() RESULT(ans)
    INTEGER(I4B) :: ans(2, 12)
  END FUNCTION EdgeConnectivity_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       RefHexahedronCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-07
! summary:  Returns coordinates of reference Hexahedron

INTERFACE
  MODULE PURE FUNCTION RefHexahedronCoord(refHexahedron) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refHexahedron
    !! UNIT
    !! BIUNIT
    REAL(DFP) :: ans(3, 8)
  END FUNCTION RefHexahedronCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE LagrangeDegree_Hexahedron
  MODULE PURE FUNCTION LagrangeDegree_Hexahedron1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Hexahedron1
END INTERFACE LagrangeDegree_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE LagrangeDegree_Hexahedron
  MODULE PURE FUNCTION LagrangeDegree_Hexahedron2(p, q, r) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: r
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Hexahedron2
END INTERFACE LagrangeDegree_Hexahedron

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Hexahedron

INTERFACE LagrangeDOF_Hexahedron
  MODULE PURE FUNCTION LagrangeDOF_Hexahedron1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Hexahedron1
END INTERFACE LagrangeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Hexahedron

INTERFACE LagrangeDOF_Hexahedron
  MODULE PURE FUNCTION LagrangeDOF_Hexahedron2(p, q, r) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Hexahedron2
END INTERFACE LagrangeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Hexahedron
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Hexahedron
!- These dof are strictly inside the Hexahedron

INTERFACE LagrangeInDOF_Hexahedron
  MODULE PURE FUNCTION LagrangeInDOF_Hexahedron1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Hexahedron1
END INTERFACE LagrangeInDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Hexahedron
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Hexahedron
!- These dof are strictly inside the Hexahedron

INTERFACE LagrangeInDOF_Hexahedron
  MODULE PURE FUNCTION LagrangeInDOF_Hexahedron2(p, q, r) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Hexahedron2
END INTERFACE LagrangeInDOF_Hexahedron

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns equidistance points in Hexahedron
!
!# Introduction
!
!- This function returns the equidistance points in Hexahedron
!- All points are inside the Hexahedron

INTERFACE EquidistanceInPoint_Hexahedron
  MODULE PURE FUNCTION EquidistanceInPoint_Hexahedron1(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Hexahedron1
END INTERFACE EquidistanceInPoint_Hexahedron

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns equidistance points in Hexahedron
!
!# Introduction
!
!- This function returns the equidistance points in Hexahedron
!- All points are inside the Hexahedron

INTERFACE EquidistanceInPoint_Hexahedron
  MODULE PURE FUNCTION EquidistanceInPoint_Hexahedron2(p, q, r, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    !! order in x, y, and z direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Hexahedron2
END INTERFACE EquidistanceInPoint_Hexahedron

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order Hexahedron element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Hexahedron element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention, VEFC.

INTERFACE EquidistancePoint_Hexahedron
  MODULE PURE FUNCTION EquidistancePoint_Hexahedron1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Hexahedron1
END INTERFACE EquidistancePoint_Hexahedron

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order Hexahedron element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Hexahedron element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention, VEFC.

INTERFACE EquidistancePoint_Hexahedron
  MODULE PURE FUNCTION EquidistancePoint_Hexahedron2(p, q, r, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order in y direction
    INTEGER(I4B), INTENT(IN) :: r
    !! order in z direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Hexahedron2
END INTERFACE EquidistancePoint_Hexahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Interpolation point

INTERFACE InterpolationPoint_Hexahedron
  MODULE FUNCTION InterpolationPoint_Hexahedron1(order, ipType, &
    & layout, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in x, y and z direction
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation type in x, y, and z direction
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! layout can be VEFC or INCREASING
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordiantes of reference hexahedron
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
    !! rows of ans denotes x, y, z components
    !! cols of ans denotes x, y, z components
  END FUNCTION InterpolationPoint_Hexahedron1
END INTERFACE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-10
! summary:  Interpolation points

INTERFACE InterpolationPoint_Hexahedron
  MODULE FUNCTION InterpolationPoint_Hexahedron2(  &
    & p, &
    & q, &
    & r, &
    & ipType1,  &
    & ipType2, &
    & ipType3,  &
    & layout,  &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order in y direction
    INTEGER(I4B), INTENT(IN) :: r
    !! order in z direction
    INTEGER(I4B), INTENT(IN) :: ipType1
    !! interpolation type in x direction
    INTEGER(I4B), INTENT(IN) :: ipType2
    !! interpolation type in y direction
    INTEGER(I4B), INTENT(IN) :: ipType3
    !! interpolation type in z direction
    CHARACTER(*), INTENT(IN) :: layout
    !! layout can be VEFC or INCREASING
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinate of reference Hexahedron
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
    !! rows of ans denotes x, y, z components
    !! cols of ans denotes x, y, z components
  END FUNCTION InterpolationPoint_Hexahedron2
END INTERFACE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-10
! summary:  Convert IJK to VEFC format

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE IJK2VEFC_Hexahedron( &
    & xi, &
    & eta, &
    & zeta, &
    & temp, &
    & p, q, r)
    REAL(DFP), INTENT(IN) :: xi(:, :, :)
    REAL(DFP), INTENT(IN) :: eta(:, :, :)
    REAL(DFP), INTENT(IN) :: zeta(:, :, :)
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: r
  END SUBROUTINE IJK2VEFC_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns coefficients of monomials for ith lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
  MODULE FUNCTION LagrangeCoeff_Hexahedron1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points in xij format
    !! number of rows in xij is 3
    !! number of columns should be equal to the number degree of freedom
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron1
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns coefficients of monomials for ith lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
  MODULE FUNCTION LagrangeCoeff_Hexahedron2(order, i, v, isVandermonde) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron2
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns coefficients of monomials for ith lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
  MODULE FUNCTION LagrangeCoeff_Hexahedron3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron3
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns the coefficients of monomials for all lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
  MODULE FUNCTION LagrangeCoeff_Hexahedron4(order, xij, basisType, &
    & refHexahedron, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refHexahedron
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! This parameter is needed when basisType is Ultraspherical
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron4
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns the coefficients of monomials for all lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
  MODULE FUNCTION LagrangeCoeff_Hexahedron5(&
    & p, &
    & q, &
    & r, &
    & xij, &
    & basisType1, &
    & basisType2, &
    & basisType3, &
    & alpha1, &
    & beta1, &
    & lambda1, &
    & alpha2, &
    & beta2, &
    & lambda2, &
    & alpha3, &
    & beta3, &
    & lambda3, &
    & refHexahedron &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of polynomial in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of polynomial in y direction
    INTEGER(I4B), INTENT(IN) :: r
    !! order of polynomial in z direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! These are interpolation points in xij format, size(xij,2)
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basis type in x direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! basis type in y direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType3
    !! basis type in z direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! This parameter is needed when basisType1 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! This parameter is needed when basisType1 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! This parameter is needed when basisType1 is Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! This parameter is needed when basisType2 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! This parameter is needed when basisType2 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! This parameter is needed when basisType2 is Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3
    !! This parameter is needed when basisType3 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta3
    !! This parameter is needed when basisType3 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda3
    !! This parameter is needed when basisType3 is Ultraspherical
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refHexahedron
    !! UNIT
    !! BIUNIT
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron5
END INTERFACE LagrangeCoeff_Hexahedron

END MODULE HexahedronInterpolationUtility
