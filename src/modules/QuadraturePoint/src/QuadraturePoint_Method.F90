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
USE BaseType, ONLY: QuadraturePoint_, ReferenceElement_
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String

IMPLICIT NONE

PRIVATE

PUBLIC :: Initiate
PUBLIC :: InitiateFacetQuadrature
PUBLIC :: Copy
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: QuadraturePoint
PUBLIC :: QuadraturePoint_Pointer
PUBLIC :: DEALLOCATE
PUBLIC :: SIZE
PUBLIC :: GetTotalQuadraturePoints

PUBLIC :: GetQuadraturePoints
PUBLIC :: GetQuadraturePoints_
PUBLIC :: GetQuadratureWeights_

PUBLIC :: Outerprod
PUBLIC :: Display
! PUBLIC :: QuadraturePoint_MdEncode
PUBLIC :: QuadraturePointIdToName
PUBLIC :: QuadraturePoint_ToChar
PUBLIC :: QuadraturePoint_ToInteger
PUBLIC :: QuadraturePointNameToId
PUBLIC :: MdEncode

!----------------------------------------------------------------------------
!                                 QuadratuePointNameToId@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-06
! summary:  Quadrature point name to quadrature point id

INTERFACE QuadraturePoint_ToInteger
  MODULE FUNCTION QuadraturePointNameToId(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION QuadraturePointNameToId
END INTERFACE QuadraturePoint_ToInteger

!----------------------------------------------------------------------------
!                                 QuadratuePointIdToName@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-06
! summary: Convert Quadrature point from int id to string name

INTERFACE
  MODULE FUNCTION QuadraturePointIdToName(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    TYPE(String) :: ans
    LOGICAL, INTENT(IN), OPTIONAL :: isUpper
  END FUNCTION QuadraturePointIdToName
END INTERFACE

!----------------------------------------------------------------------------
!                                  QuadraturePoint_ToChar@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-18
! summary:  Convert Quadrature poitn from int id to char name

INTERFACE
  MODULE FUNCTION QuadraturePoint_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION QuadraturePoint_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                         QuadratureNumber@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE QuadratureNumber
  MODULE FUNCTION obj_QuadratureNumber1(topo, order, quadratureType) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: topo
    !! Reference-element
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    INTEGER(I4B) :: ans
    !! quadrature number
    !! for quadrangle element ans is number of  quadrature points in x and y
    !! so total number of quadrature points are ans*ans
  END FUNCTION obj_QuadratureNumber1
END INTERFACE QuadratureNumber

!----------------------------------------------------------------------------
!                                                    Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiates the quadrature points

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Copy(obj, obj2)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    TYPE(QuadraturePoint_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE Initiate

INTERFACE Copy
  MODULE PROCEDURE obj_Copy
END INTERFACE Copy

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Copy
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiates the quadrature points

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate1(obj, points)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: points(:, :)
    !! points contains the quadrature points and weights
    !! points( :, ipoint ) contains quadrature points and weights of ipoint
    !! quadrature point. The last row contains the weight. The rest of the
    !! rows contains the coordinates of quadrature.
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiates the quadrature points

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2(obj, tXi, tpoints)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tXi
    !! Total number of xidimension
    !! For line tXi=1
    !! For 2D element tXi=2
    !! For 3D element tXi=3
    INTEGER(I4B), INTENT(IN) :: tpoints
    !! Total number quadrature points
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiates the quadrature points
!
!# Introduction
!
! We call obj_Initiate5 in this routine

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate3(obj, refElem, order, quadratureType, &
                                  alpha, beta, lambda)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! Reference element
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    CHARACTER(*), INTENT(IN) :: quadratureType
    !! Type of quadrature points
    !! "GaussLegendre" ! "GaussLegendreLobatto"
    !! "GaussLegendreRadau", "GaussLegendreRadauLeft"
    !! "GaussLegendreRadauRight" ! "GaussChebyshev"
    !! "GaussChebyshevLobatto" ! "GaussChebyshevRadau",
    !! "GaussChebyshevRadauLeft" ! "GaussChebyshevRadauRight"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE obj_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiates the quadrature points from number of IP
!
!# Introduction
!
! This routine is used to initiate the quadrature points from number of
! integration points.
! We call obj_Initiate6 in this routine

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate4(obj, refElem, nips, quadratureType, &
                                  alpha, beta, lambda)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! Reference element
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of quadrature points
    CHARACTER(*), INTENT(IN) :: quadratureType
    !! Total number quadrature points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE obj_Initiate4
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine constructs the quadrature points

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate5(obj, refElem, order, quadratureType, &
                                  alpha, beta, lambda)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! Reference-element
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE obj_Initiate5
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiates the quadrature points

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate6(obj, refElem, nips, quadratureType, &
                                  alpha, beta, lambda)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! Reference element
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of integration points
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Type of quadrature points
    !! GaussLegendre ! GaussLegendreLobatto ! GaussLegendreRadau
    !! GaussLegendreRadauLeft ! GaussLegendreRadauRight ! GaussChebyshev
    !! GaussChebyshevLobatto ! GaussChebyshevRadau ! GaussChebyshevRadauLeft
    !! GaussChebyshevRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE obj_Initiate6
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiates the quadrature points

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate7(obj, refElem, p, q, r, quadratureType1, &
                                  quadratureType2, quadratureType3, &
                                  alpha1, beta1, lambda1, &
                                  alpha2, beta2, lambda2, &
                                  alpha3, beta3, lambda3)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! Reference-element
    INTEGER(I4B), INTENT(IN) :: p
    !! order of integrand in x
    INTEGER(I4B), INTENT(IN) :: q
    !! order of integrand in y
    INTEGER(I4B), INTENT(IN) :: r
    !! order of integrand in z direction
    INTEGER(I4B), INTENT(IN) :: quadratureType1
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: quadratureType2
    !! Type of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType3
    !! Type of quadrature points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi parameter and Ultraspherical parameters
  END SUBROUTINE obj_Initiate7
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiates the quadrature points

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate8(obj, refElem, nipsx, nipsy, nipsz, &
                                  quadratureType1, quadratureType2, &
                                  quadratureType3, alpha1, beta1, lambda1, &
                                  alpha2, beta2, lambda2, &
                                  alpha3, beta3, lambda3)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    CLASS(ReferenceElement_), INTENT(IN) :: refElem
    !! Reference element
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in y direction
    INTEGER(I4B), INTENT(IN) :: nipsz(1)
    !! number of integration points in z direction
    INTEGER(I4B), INTENT(IN) :: quadratureType1
    !! Type of quadrature points
    !! GaussLegendre ! GaussLegendreLobatto ! GaussLegendreRadau
    !! GaussLegendreRadauLeft ! GaussLegendreRadauRight ! GaussChebyshev
    !! GaussChebyshevLobatto ! GaussChebyshevRadau ! GaussChebyshevRadauLeft
    !! GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: quadratureType2
    !! Type of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType3
    !! Type of quadrature points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi parameter and Ultraspherical parameter
  END SUBROUTINE obj_Initiate8
END INTERFACE Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-21
! summary:  This routine Initiates the quadrature points
!
!# Introduction
!
! This routine is used to initiate the quadrature points from order of
! of integrand.
! This subroutine does not require formation of reference element.
! This routine calls obj_Initiate11 method.

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate9(obj, elemType, domainName, order, &
                                  quadratureType, alpha, beta, lambda, xij)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element name
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain name for reference element
    !! unit or biunit
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  END SUBROUTINE obj_Initiate9
END INTERFACE Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-21
! summary:  This routine Initiates the quadrature points
!
!# Introduction
!
! This routine is used to initiate the quadrature points from number of
! integration points.
! This subroutine does not require formation of reference element.
! This routine calls obj_Initiate12 method.

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate10(obj, elemType, domainName, nips, &
                                   quadratureType, alpha, beta, lambda, xij)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element name
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain name, reference element
    !! unit or biunit
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! Number of integration points
    !! in the case of  quadrangle element nips(1) denotes the
    !! number of quadrature points in the x and y direction
    !! so the total number of quadrature points are nips(1)*nips(1)
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  END SUBROUTINE obj_Initiate10
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate11(obj, elemType, domainName, p, q, r, &
                                   quadratureType1, quadratureType2, &
                                   quadratureType3, alpha1, beta1, lambda1, &
                                   alpha2, beta2, lambda2, &
                                   alpha3, beta3, lambda3, xij)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    INTEGER(I4B), INTENT(IN) :: elemtype
    !! Reference-element
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain name
    INTEGER(I4B), INTENT(IN) :: p
    !! order of integrand in x
    INTEGER(I4B), INTENT(IN) :: q
    !! order of integrand in y
    INTEGER(I4B), INTENT(IN) :: r
    !! order of integrand in z direction
    INTEGER(I4B), INTENT(IN) :: quadratureType1
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: quadratureType2
    !! Type of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType3
    !! Type of quadrature points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  END SUBROUTINE obj_Initiate11
END INTERFACE Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate12(obj, elemType, domainName, nipsx, nipsy, &
                                   nipsz, quadratureType1, quadratureType2, &
                                   quadratureType3, alpha1, beta1, lambda1, &
                                   alpha2, beta2, lambda2, &
                                   alpha3, beta3, lambda3, xij)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain name
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in y direction
    INTEGER(I4B), INTENT(IN) :: nipsz(1)
    !! number of integration points in z direction
    INTEGER(I4B), INTENT(IN) :: quadratureType1
    !! Type of quadrature points
    !! GaussLegendre ! GaussLegendreLobatto ! GaussLegendreRadau
    !! GaussLegendreRadauLeft ! GaussLegendreRadauRight ! GaussChebyshev
    !! GaussChebyshevLobatto ! GaussChebyshevRadau ! GaussChebyshevRadauLeft
    !! GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: quadratureType2
    !! Type of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType3
    !! Type of quadrature points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of reference element
  END SUBROUTINE obj_Initiate12
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                       QuadraturePoint@ConstructureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine Initiate an instance of quadrature points

INTERFACE QuadraturePoint
  MODULE PURE FUNCTION quad_Constructor1(points) RESULT(obj)
    TYPE(QuadraturePoint_) :: obj
    REAL(DFP), INTENT(IN) :: points(:, :)
  END FUNCTION quad_Constructor1
END INTERFACE QuadraturePoint

!----------------------------------------------------------------------------
!                                 QuadraturePoint_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns a pointer to a newly created instance of quadrature points

INTERFACE QuadraturePoint_Pointer
  MODULE PURE FUNCTION quad_Constructor_1(points) RESULT(obj)
    TYPE(QuadraturePoint_), POINTER :: obj
    REAL(DFP), INTENT(IN) :: points(:, :)
  END FUNCTION quad_Constructor_1
END INTERFACE QuadraturePoint_Pointer

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Deallocates the data stored inside the quadrature point

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE quad_Deallocate(obj)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
  END SUBROUTINE quad_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                           SIZE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the size of obj%points,

INTERFACE SIZE
  MODULE PURE FUNCTION obj_Size(obj, dims) RESULT(ans)
    TYPE(QuadraturePoint_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                       getTotalQuadraturepoints@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns total number of quadrature points

INTERFACE GetTotalQuadraturepoints
  MODULE PURE FUNCTION obj_GetTotalQuadraturePoints1(obj) RESULT(ans)
    TYPE(QuadraturePoint_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalQuadraturePoints1
END INTERFACE GetTotalQuadraturepoints

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE GetTotalQuadraturePoints
  MODULE FUNCTION obj_GetTotalQuadraturePoints2(elemType, p, q, r, &
                                                quadratureType1, &
                                                quadratureType2, &
                                                quadratureType3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemtype
    !! Reference-element
    INTEGER(I4B), INTENT(IN) :: p
    !! order of integrand in x
    INTEGER(I4B), INTENT(IN) :: q
    !! order of integrand in y
    INTEGER(I4B), INTENT(IN) :: r
    !! order of integrand in z direction
    INTEGER(I4B), INTENT(IN) :: quadratureType1
    !! Type of quadrature points: GaussLegendre, GaussLegendreLobatto
    !! GaussLegendreRadau, GaussLegendreRadauLeft, GaussLegendreRadauRight
    !! GaussChebyshev, GaussChebyshevLobatto, GaussChebyshevRadau
    !! GaussChebyshevRadauLeft, GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: quadratureType2
    !! Type of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType3
    !! Type of quadrature points
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalQuadraturePoints2
END INTERFACE GetTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                              GetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns quadrature points

INTERFACE GetQuadraturePoints
  MODULE PURE SUBROUTINE obj_GetQuadraturePoints1(obj, points, weights, num)
    TYPE(QuadraturePoint_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: points(3)
    !! [xi, eta, zeta]
    REAL(DFP), INTENT(INOUT) :: weights
    !! weights
    INTEGER(I4B), INTENT(IN) :: num
    !! quadrature number
  END SUBROUTINE obj_GetQuadraturePoints1
END INTERFACE GetQuadraturePoints

!----------------------------------------------------------------------------
!                                              GetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns total number of quadrature points

INTERFACE GetQuadraturePoints
  MODULE PURE SUBROUTINE obj_GetQuadraturePoints2(obj, points, weights)
    TYPE(QuadraturePoint_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: points(:, :)
    !! Point( :, j ) = [xi, eta, zeta] of jth quadrature point
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: weights(:)
    !! Weight(j) weight of jth quadrature point
  END SUBROUTINE obj_GetQuadraturePoints2
END INTERFACE GetQuadraturePoints

!----------------------------------------------------------------------------
!                                              GetQuadraturePoint@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-07
! summary: This routine returns total number of quadrature points

INTERFACE GetQuadraturePoints_
  MODULE PURE SUBROUTINE obj_GetQuadraturePoints1_(obj, points, weights, &
                                                   nrow, ncol)
    TYPE(QuadraturePoint_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: points(:, :)
    !! Point( :, j ) = [xi, eta, zeta] of jth quadrature point
    REAL(DFP), INTENT(INOUT) :: weights(:)
    !! Weight(j) weight of jth quadrature point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns
    !! ncol is number of columns in points and weights
  END SUBROUTINE obj_GetQuadraturePoints1_
END INTERFACE GetQuadraturePoints_

!----------------------------------------------------------------------------
!                                              GetQuadratureWeight@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-07
! summary: This routine returns the quadrature weights

INTERFACE GetQuadratureWeights_
  MODULE PURE SUBROUTINE obj_GetQuadratureWeights1_(obj, weights, tsize)
    TYPE(QuadraturePoint_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: weights(:)
    !! Weight(j) weight of jth quadrature point
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! The number of data written in weights
  END SUBROUTINE obj_GetQuadratureWeights1_
END INTERFACE GetQuadratureWeights_

!----------------------------------------------------------------------------
!                                                       OuterProd@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2022
! summary:         Performs outerproduct of quadrature points

INTERFACE Outerprod
  MODULE PURE FUNCTION obj_Outerprod(obj1, obj2) RESULT(ans)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj1
    !! quadrature points in 1D
    CLASS(QuadraturePoint_), INTENT(IN) :: obj2
    !! quadrature points in 1D
    TYPE(QuadraturePoint_) :: ans
    !! quadrature points in 2D
  END FUNCTION obj_Outerprod
END INTERFACE Outerprod

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary:  Display the content of quadrature point

INTERFACE Display
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE obj_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                        MdEncode@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary:  Display the content of quadrature point

INTERFACE MdEncode
  MODULE FUNCTION obj_MdEncode(obj) RESULT(ans)
    CLASS(QuadraturePoint_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_MdEncode
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss Legendre Quadrature points based on given order

! INTERFACE GaussLegendreQuadrature
!   MODULE FUNCTION getGaussLegendreQP1(refelem, order) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: order
!     !! order of accuracy in each direction
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreQP1
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss-Legendre Quadrature points

! INTERFACE GaussLegendreQuadrature
!   MODULE FUNCTION getGaussLegendreQP2(refelem, nips) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: nips(:)
!     !! number of integration points
!     !! IF size(nips) = 1, then sqrt(nips(1)) points are used in both dirn
!     !! IF size(nips) = 2, then in x1 direction nips(1) points and in
!     !! x2 direction nips(2) points are used.
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreQP2
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss Legendre Quadrature points based on given order

! INTERFACE GaussLegendreQuadrature
!   MODULE FUNCTION getGaussLegendreQP3(refelem, p, q, r) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: p
!     !! order of accuracy in x1 direction
!     INTEGER(I4B), INTENT(IN) :: q
!     !! order of accuracy in x2 direction
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: r
!     !! order of accuracy in x3 direction
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreQP3
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                         GaussLegendreLobattoQuadrature@GaussLegendreLobatto
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss LegendreLobatto Quadrature points

! INTERFACE GaussLegendreLobattoQuadrature
!   MODULE FUNCTION getGaussLegendreLobattoQP1(refelem, order) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: order
!     !! order of accuracy in each direction
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreLobattoQP1
! END INTERFACE GaussLegendreLobattoQuadrature

!----------------------------------------------------------------------------
!                         GaussLegendreLobattoQuadrature@GaussLegendreLobatto
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss-LegendreLobatto Quadrature points

! INTERFACE GaussLegendreLobattoQuadrature
!   MODULE FUNCTION getGaussLegendreLobattoQP2(refelem, nips) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: nips(:)
!     !! number of integration points
!     !! IF size(nips) = 1, then sqrt(nips(1)) points are used in both dirn
!     !! IF size(nips) = 2, then in x1 direction nips(1) points and in
!     !! x2 direction nips(2) points are used.
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreLobattoQP2
! END INTERFACE GaussLegendreLobattoQuadrature

!----------------------------------------------------------------------------
!                       GaussLegendreLobattoQuadrature@GaussLegendreLobatto
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss LegendreLobatto Quadrature points

! INTERFACE GaussLegendreLobattoQuadrature
!   MODULE FUNCTION getGaussLegendreLobattoQP3(refelem, p, q, r) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: p
!     !! order of accuracy in x1 direction
!     INTEGER(I4B), INTENT(IN) :: q
!     !! order of accuracy in x2 direction
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: r
!     !! order of accuracy in x3 direction
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreLobattoQP3
! END INTERFACE GaussLegendreLobattoQuadrature

!----------------------------------------------------------------------------
!                   GaussLegendreRadauLeftQuadrature@GaussLegendreRadauLeft
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the LegendreRadauLeft Quadrature points

! INTERFACE GaussLegendreRadauLeftQuadrature
!   MODULE FUNCTION getGaussLegendreRadauLeftQP1(refelem, order) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: order
!     !! order of accuracy in each direction
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreRadauLeftQP1
! END INTERFACE GaussLegendreRadauLeftQuadrature

!----------------------------------------------------------------------------
!                   GaussLegendreRadauLeftQuadrature@GaussLegendreRadauLeft
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss-LegendreRadauLeft Quadrature points

! INTERFACE GaussLegendreRadauLeftQuadrature
!   MODULE FUNCTION getGaussLegendreRadauLeftQP2(refelem, nips) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: nips(:)
!     !! number of integration points
!     !! IF size(nips) = 1, then sqrt(nips(1)) points are used in both dirn
!     !! IF size(nips) = 2, then in x1 direction nips(1) points and in
!     !! x2 direction nips(2) points are used.
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreRadauLeftQP2
! END INTERFACE GaussLegendreRadauLeftQuadrature

!----------------------------------------------------------------------------
!                   GaussLegendreRadauLeftQuadrature@GaussLegendreRadauLeft
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss LegendreRadauLeft Quadrature points

! INTERFACE GaussLegendreRadauLeftQuadrature
!   MODULE FUNCTION getGaussLegendreRadauLeftQP3(refelem, p, q, r) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: p
!     !! order of accuracy in x1 direction
!     INTEGER(I4B), INTENT(IN) :: q
!     !! order of accuracy in x2 direction
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: r
!     !! order of accuracy in x3 direction
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreRadauLeftQP3
! END INTERFACE GaussLegendreRadauLeftQuadrature

!----------------------------------------------------------------------------
!                  GaussLegendreRadauRightQuadrature@GaussLegendreRadauRight
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the LegendreRadauRight Quadrature points

! INTERFACE GaussLegendreRadauRightQuadrature
!   MODULE FUNCTION getGaussLegendreRadauRightQP1(refelem, order) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: order
!     !! order of accuracy in each direction
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreRadauRightQP1
! END INTERFACE GaussLegendreRadauRightQuadrature

!----------------------------------------------------------------------------
!                  GaussLegendreRadauRightQuadrature@GaussLegendreRadauRight
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss-LegendreRadauRight Quadrature points

! INTERFACE GaussLegendreRadauRightQuadrature
!   MODULE FUNCTION getGaussLegendreRadauRightQP2(refelem, nips) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: nips(:)
!     !! number of integration points
!     !! IF size(nips) = 1, then sqrt(nips(1)) points are used in both dirn
!     !! IF size(nips) = 2, then in x1 direction nips(1) points and in
!     !! x2 direction nips(2) points are used.
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreRadauRightQP2
! END INTERFACE GaussLegendreRadauRightQuadrature

!----------------------------------------------------------------------------
!                  GaussLegendreRadauRightQuadrature@GaussLegendreRadauRight
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the Gauss LegendreRadauRight Quadrature points

! INTERFACE GaussLegendreRadauRightQuadrature
!   MODULE FUNCTION getGaussLegendreRadauRightQP3(refelem, p, q, r) RESULT(obj)
!     CLASS(ReferenceElement_), INTENT(IN) :: refelem
!     INTEGER(I4B), INTENT(IN) :: p
!     !! order of accuracy in x1 direction
!     INTEGER(I4B), INTENT(IN) :: q
!     !! order of accuracy in x2 direction
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: r
!     !! order of accuracy in x3 direction
!     TYPE(QuadraturePoint_) :: obj
!   END FUNCTION getGaussLegendreRadauRightQP3
! END INTERFACE GaussLegendreRadauRightQuadrature

!----------------------------------------------------------------------------
!                              InitiateFacetQuadrature@FacetQuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-21
! summary:  This routine Initiates the quadrature points
!
!# Introduction
!
! This routine is used to initiate the quadrature points from order of
! of integrand.
! This subroutine does not require formation of reference element.
! This routine calls obj_Initiate11 method.

INTERFACE InitiateFacetQuadrature
  MODULE SUBROUTINE obj_InitiateFacetQuadrature1(obj, facetQuad, &
                                                 localFaceNumber, elemType, &
                                                 domainName, order, &
                                                 quadratureType, &
                                                 alpha, beta, lambda, xij)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Quadrature point in the cell
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Quadrature point on the local face
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element name
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain name for reference element
    !! unit or biunit
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), INTENT(IN) :: xij(:, :)
  END SUBROUTINE obj_InitiateFacetQuadrature1
END INTERFACE InitiateFacetQuadrature

!----------------------------------------------------------------------------
!                             InitiateFacetQuadrature@FacetQuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-21
! summary: This routine Initiates the quadrature points

INTERFACE InitiateFacetQuadrature
  MODULE SUBROUTINE obj_InitiateFacetQuadrature2(obj, facetQuad, &
                                                 localFaceNumber, elemType, &
                                                 domainName, nips, &
                                                 quadratureType, alpha, &
                                                 beta, lambda, xij)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Quadrature point in the cell
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Quadrature point on the local facet
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element name
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain name, reference element
    !! unit or biunit
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! Number of integration points
    !! in the case of  quadrangle element nips(1) denotes the
    !! number of quadrature points in the x and y direction
    !! so the total number of quadrature points are nips(1)*nips(1)
    INTEGER(I4B), INTENT(IN) :: quadratureType
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), INTENT(IN) :: xij(:, :)
  END SUBROUTINE obj_InitiateFacetQuadrature2
END INTERFACE InitiateFacetQuadrature

!----------------------------------------------------------------------------
!                              InitiateFacetQuadrature@FacetQuadratureMethods
!----------------------------------------------------------------------------

INTERFACE InitiateFacetQuadrature
  MODULE SUBROUTINE obj_InitiateFacetQuadrature3(obj, facetQuad, &
                                                 localFaceNumber, elemType, &
                                                 domainName, p, q, r, &
                                                 quadratureType1, &
                                                 quadratureType2, &
                                                 quadratureType3, &
                                                 alpha1, beta1, lambda1, &
                                                 alpha2, beta2, lambda2, &
                                                 alpha3, beta3, lambda3, &
                                                 xij)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Quadrature point in the cell
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Quadrature point on the local face element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local facet number
    INTEGER(I4B), INTENT(IN) :: elemtype
    !! Reference-element
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain name
    INTEGER(I4B), INTENT(IN) :: p
    !! order of integrand in x
    INTEGER(I4B), INTENT(IN) :: q
    !! order of integrand in y
    INTEGER(I4B), INTENT(IN) :: r
    !! order of integrand in z direction
    INTEGER(I4B), INTENT(IN) :: quadratureType1
    !! Type of quadrature points ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadau ! GaussLegendreRadauLeft ! GaussLegendreRadauRight
    !! GaussChebyshev ! GaussChebyshevLobatto ! GaussChebyshevRadau
    !! GaussChebyshevRadauLeft ! GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: quadratureType2
    !! Type of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType3
    !! Type of quadrature points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi parameter and Ultraspherical parameters
    REAL(DFP), INTENT(IN) :: xij(:, :)
  END SUBROUTINE obj_InitiateFacetQuadrature3
END INTERFACE InitiateFacetQuadrature

!----------------------------------------------------------------------------
!                              InitiateFacetQuadrature@FacetQuadratureMethods
!----------------------------------------------------------------------------

INTERFACE InitiateFacetQuadrature
  MODULE SUBROUTINE obj_InitiateFacetQuadrature4(obj, facetQuad, &
                                                 localFaceNumber, &
                                                 elemType, domainName, &
                                                 nipsx, nipsy, nipsz, &
                                                 quadratureType1, &
                                                 quadratureType2, &
                                                 quadratureType3, &
                                                 alpha1, beta1, &
                                                 lambda1, &
                                                 alpha2, beta2, lambda2, &
                                                 alpha3, beta3, lambda3, xij)
    TYPE(QuadraturePoint_), INTENT(INOUT) :: obj
    !! Total number of xidimension
    TYPE(QuadraturePoint_), INTENT(INOUT) :: facetQuad
    !! Quadrature point on the local face element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local facet number
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain name
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in y direction
    INTEGER(I4B), INTENT(IN) :: nipsz(1)
    !! number of integration points in z direction
    INTEGER(I4B), INTENT(IN) :: quadratureType1
    !! Type of quadrature points
    !! GaussLegendre ! GaussLegendreLobatto ! GaussLegendreRadau
    !! GaussLegendreRadauLeft ! GaussLegendreRadauRight ! GaussChebyshev
    !! GaussChebyshevLobatto ! GaussChebyshevRadau ! GaussChebyshevRadauLeft
    !! GaussChebyshevRadauRight
    INTEGER(I4B), INTENT(IN) :: quadratureType2
    !! Type of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadratureType3
    !! Type of quadrature points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! coordinates of reference element
  END SUBROUTINE obj_InitiateFacetQuadrature4
END INTERFACE InitiateFacetQuadrature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE QuadraturePoint_Method
