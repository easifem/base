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

MODULE RecursiveNodesUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                           RecursiveNode1D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary: Convert nodal coordinates to barycentric coordinates

INTERFACE
  MODULE FUNCTION RecursiveNode1D(order, ipType, &
    & domain) RESULT(ans)
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
  END FUNCTION RecursiveNode1D
END INTERFACE

PUBLIC :: RecursiveNode1D

!----------------------------------------------------------------------------
!                                                            RecursiveNode2D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary: Convert nodal coordinates to barycentric coordinates

INTERFACE
  MODULE FUNCTION RecursiveNode2D(order, ipType, domain) &
    & RESULT(ans)
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
  END FUNCTION RecursiveNode2D
END INTERFACE

PUBLIC :: RecursiveNode2D

!----------------------------------------------------------------------------
!                                                            RecursiveNode3D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Convert nodal coordinates to barycentric coordinates

INTERFACE
  MODULE FUNCTION RecursiveNode3D(order, ipType, &
    & domain) RESULT(ans)
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
  END FUNCTION RecursiveNode3D
END INTERFACE

PUBLIC :: RecursiveNode3D

!----------------------------------------------------------------------------
!                                                               ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION ToUnit(x, domain) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:, :)
    CHARACTER(*), INTENT(IN) :: domain
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ToUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION FromUnit(x, domain) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:, :)
    CHARACTER(*), INTENT(IN) :: domain
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION FromUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ToUnit
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE Unit2Equilateral(d, x)
    INTEGER(I4B), INTENT(IN) :: d
    REAL(DFP), INTENT(INOUT) :: x(:, :)
  END SUBROUTINE Unit2Equilateral
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ToUnit
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

END MODULE RecursiveNodesUtility
