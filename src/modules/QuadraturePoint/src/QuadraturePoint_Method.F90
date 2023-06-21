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

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This module contains the methods for data type [[QuadraturePoint_]]

MODULE QuadraturePoint_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates the quadrature points

INTERFACE
  MODULE PURE SUBROUTINE quad_initiate1(obj, points)
    CLASS(QuadraturePoint_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: points(:, :)
    !! points contains the quadrature points and weights
    !! points( :, ipoint ) contains quadrature points and weights of ipoint
    !! quadrature point. The last row contains the weight. The rest of the
    !! rows contains the coordinates of quadrature.
  END SUBROUTINE quad_initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE quad_initiate1
END INTERFACE

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates the quadrature points

INTERFACE
  MODULE PURE SUBROUTINE quad_initiate2(obj, tXi, tpoints)
    CLASS(QuadraturePoint_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tXi
    !! Total number of xidimension
    !! For line tXi=1
    !! For 2D element tXi=2
    !! For 3D element tXi=3
    INTEGER(I4B), INTENT(IN) :: tpoints
    !! Total number quadrature points
  END SUBROUTINE quad_initiate2
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE quad_initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates the quadrature points

INTERFACE
  MODULE SUBROUTINE quad_initiate3(obj, refElem, order, quadratureType)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! Reference element
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    CHARACTER(*), INTENT(IN) :: quadratureType
    !! Total number quadrature points
  END SUBROUTINE quad_initiate3
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE quad_initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiates the quadrature points

INTERFACE
  MODULE SUBROUTINE quad_initiate4(obj, refElem, nips, quadratureType)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! Reference element
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! order of integrand
    CHARACTER(*), INTENT(IN) :: quadratureType
    !! Total number quadrature points
  END SUBROUTINE quad_initiate4
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE quad_initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                       QuadraturePoint@ConstructureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine initiate an instance of quadrature points

INTERFACE
  MODULE PURE FUNCTION quad_Constructor1(points) RESULT(obj)
    TYPE(QuadraturePoint_) :: obj
    REAL(DFP), INTENT(IN) :: points(:, :)
  END FUNCTION quad_Constructor1
END INTERFACE

INTERFACE QuadraturePoint
  MODULE PROCEDURE quad_Constructor1
END INTERFACE QuadraturePoint

PUBLIC :: QuadraturePoint

!----------------------------------------------------------------------------
!                                 QuadraturePoint_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns a pointer to a newly created instance of quadrature points

INTERFACE
  MODULE PURE FUNCTION quad_Constructor_1(points) RESULT(obj)
    CLASS(QuadraturePoint_), POINTER :: obj
    REAL(DFP), INTENT(IN) :: points(:, :)
  END FUNCTION quad_Constructor_1
END INTERFACE

INTERFACE QuadraturePoint_Pointer
  MODULE PROCEDURE quad_Constructor_1
END INTERFACE QuadraturePoint_Pointer

PUBLIC :: QuadraturePoint_Pointer

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Deallocates the data stored inside the quadrature point

INTERFACE
  MODULE PURE SUBROUTINE quad_Deallocate(obj)
    CLASS(QuadraturePoint_), INTENT(INOUT) :: obj
  END SUBROUTINE quad_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE quad_Deallocate
END INTERFACE DEALLOCATE

PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss Legendre Quadrature points based on given order

INTERFACE
  MODULE FUNCTION getGaussLegendreQP1(refelem, order) RESULT(obj)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    !! order of accuracy in each direction
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQP1
END INTERFACE

INTERFACE GaussLegendreQuadrature
  MODULE PROCEDURE getGaussLegendreQP1
END INTERFACE GaussLegendreQuadrature

PUBLIC :: GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss-Legendre Quadrature points

INTERFACE
  MODULE FUNCTION getGaussLegendreQP2(refelem, nips) RESULT(obj)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: nips(:)
    !! number of integration points
    !! IF size(nips) = 1, then sqrt(nips(1)) points are used in both dirn
    !! IF size(nips) = 2, then in x1 direction nips(1) points and in
    !! x2 direction nips(2) points are used.
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQP2
END INTERFACE

INTERFACE GaussLegendreQuadrature
  MODULE PROCEDURE getGaussLegendreQP2
END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss Legendre Quadrature points based on given order

INTERFACE
  MODULE FUNCTION getGaussLegendreQP3(refelem, p, q, r) RESULT(obj)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: p
    !! order of accuracy in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of accuracy in x2 direction
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: r
    !! order of accuracy in x3 direction
    TYPE(QuadraturePoint_) :: obj
  END FUNCTION getGaussLegendreQP3
END INTERFACE

INTERFACE GaussLegendreQuadrature
  MODULE PROCEDURE getGaussLegendreQP3
END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                           SIZE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the size of obj%points,

INTERFACE
  MODULE PURE FUNCTION quad_Size(obj, dims) RESULT(ans)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dims
    INTEGER(I4B) :: ans
  END FUNCTION quad_Size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE quad_Size
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                       getTotalQuadraturepoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns total number of quadrature points

INTERFACE
  MODULE PURE FUNCTION quad_getTotalQuadraturepoints(obj, dims) RESULT(ans)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dims
    INTEGER(I4B) :: ans
  END FUNCTION quad_getTotalQuadraturepoints
END INTERFACE

INTERFACE getTotalQuadraturepoints
  MODULE PROCEDURE quad_getTotalQuadraturepoints
END INTERFACE getTotalQuadraturepoints

PUBLIC :: getTotalQuadraturepoints

!----------------------------------------------------------------------------
!                                              GetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns quadrature points

INTERFACE
  MODULE PURE SUBROUTINE quad_GetQuadraturepoints1(obj, point, weight, num)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: point(3)
    !! [xi, eta, zeta]
    REAL(DFP), INTENT(INOUT) :: weight
    !! weights
    INTEGER(I4B), INTENT(IN) :: num
    !! quadrature number
  END SUBROUTINE quad_GetQuadraturepoints1
END INTERFACE

INTERFACE GetQuadraturepoints
  MODULE PROCEDURE quad_GetQuadraturepoints1
END INTERFACE

PUBLIC :: GetQuadraturepoints

!----------------------------------------------------------------------------
!                                              GetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns total number of quadrature points

INTERFACE
  MODULE PURE SUBROUTINE quad_GetQuadraturepoints2(obj, point, weight)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: point(:, :)
    !! Point( :, j ) = [xi, eta, zeta] of jth quadrature point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: weight(:)
    !! Weight(j) weight of jth quadrature point
  END SUBROUTINE quad_GetQuadraturepoints2
END INTERFACE

INTERFACE GetQuadraturepoints
  MODULE PROCEDURE quad_GetQuadraturepoints2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       OuterProd@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2022
! summary:         Performs outerproduct of quadrature points

INTERFACE
  MODULE PURE FUNCTION quad_Outerprod(obj1, obj2) RESULT(ans)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj1
    !! quadrature points in 1D
    CLASS(QuadraturePoint_), INTENT(IN) :: obj2
    !! quadrature points in 1D
    TYPE(QuadraturePoint_) :: ans
    !! quadrature points in 2D
  END FUNCTION quad_Outerprod
END INTERFACE

INTERFACE Outerprod
  MODULE PROCEDURE quad_Outerprod
END INTERFACE Outerprod

PUBLIC :: Outerprod

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary:  Display the content of quadrature point

INTERFACE
  MODULE SUBROUTINE quad_Display(obj, msg, unitno)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE quad_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE quad_Display
END INTERFACE Display

PUBLIC :: Display

END MODULE QuadraturePoint_Method
