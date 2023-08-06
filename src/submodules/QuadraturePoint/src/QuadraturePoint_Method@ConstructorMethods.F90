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

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary:  Constructor methods for [[QuadraturePoint_]]

SUBMODULE(QuadraturePoint_Method) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                  QuadraturePointIDToName
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePointIDToName
SELECT CASE (name)
CASE (Equidistance)
  ans = "EQUIDISTANCE"

CASE (GaussLegendre)
  ans = "GAUSSLEGENDRE"

CASE (GaussLegendreLobatto)
  ans = "GAUSSLEGENDRELOBATTO"

CASE (GaussLegendreRadau)
  ans = "GAUSSLEGENDRERADAU"

CASE (GaussLegendreRadauLeft)
  ans = "GAUSSLEGENDRERADAULEFT"

CASE (GaussLegendreRadauRight)
  ans = "GAUSSLEGENDRERADAURIGHT"

CASE (GaussChebyshev)
  ans = "GAUSSCHEBYSHEV"

CASE (GaussChebyshevLobatto)
  ans = "GAUSSCHEBYSHEVLOBATTO"

CASE (GaussChebyshevRadau)
  ans = "GAUSSCHEBYSHEVRADAU"

CASE (GaussChebyshevRadauLeft)
  ans = "GAUSSCHEBYSHEVRADAULEFT"

CASE (GaussChebyshevRadauRight)
  ans = "GAUSSCHEBYSHEVRADAURIGHT"

CASE (GaussJacobi)
  ans = "GAUSSJACOBI"

CASE (GaussJacobiLobatto)
  ans = "GAUSSJACOBILOBATTO"

CASE (GaussJacobiRadau)
  ans = "GAUSSJACOBIRADAU"

CASE (GaussJacobiRadauLeft)
  ans = "GAUSSJACOBIRADAULEFT"

CASE (GaussJacobiRadauRight)
  ans = "GAUSSJACOBIRADAURIGHT"

CASE (GaussUltraspherical)
  ans = "GAUSSULTRASPHERICAL"

CASE (GaussUltrasphericalLobatto)
  ans = "GAUSSULTRASPHERICALLOBATTO"

CASE (GaussUltrasphericalRadau)
  ans = "GAUSSULTRASPHERICALRADAU"

CASE (GaussUltrasphericalRadauLeft)
  ans = "GAUSSULTRASPHERICALRADAULEFT"

CASE (GaussUltrasphericalRadauRight)
  ans = "GAUSSULTRASPHERICALRADAURIGHT"

CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for given quadratureType name", &
    & file=__FILE__, &
    & line=__LINE__,&
    & routine="QuadraturePointIDToName()", &
    & unitno=stderr)
END SELECT
END PROCEDURE QuadraturePointIDToName

!----------------------------------------------------------------------------
!                                                  QuadraturePointNameToID
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePointNameToID

SELECT CASE (TRIM(UpperCase(name)))
CASE ("EQUIDISTANCE")
  ans = Equidistance

CASE ("GAUSSLEGENDRE")
  ans = GaussLegendre

CASE ("GAUSSLEGENDRELOBATTO")
  ans = GaussLegendreLobatto

CASE ("GAUSSLEGENDRERADAU")
  ans = GaussLegendreRadau

CASE ("GAUSSLEGENDRERADAULEFT")
  ans = GaussLegendreRadauLeft

CASE ("GAUSSLEGENDRERADAURIGHT")
  ans = GaussLegendreRadauRight

CASE ("GAUSSCHEBYSHEV")
  ans = GaussChebyshev

CASE ("GAUSSCHEBYSHEVLOBATTO")
  ans = GaussChebyshevLobatto

CASE ("GAUSSCHEBYSHEVRADAU")
  ans = GaussChebyshevRadau

CASE ("GAUSSCHEBYSHEVRADAULEFT")
  ans = GaussChebyshevRadauLeft

CASE ("GAUSSCHEBYSHEVRADAURIGHT")
  ans = GaussChebyshevRadauRight

CASE ("GAUSSJACOBI")
  ans = GaussJacobi

CASE ("GAUSSJACOBILOBATTO")
  ans = GaussJacobiLobatto

CASE ("GAUSSJACOBIRADAU")
  ans = GaussJacobiRadau

CASE ("GAUSSJACOBIRADAULEFT")
  ans = GaussJacobiRadauLeft

CASE ("GAUSSJACOBIRADAURIGHT")
  ans = GaussJacobiRadauRight

CASE ("GAUSSULTRASPHERICAL")
  ans = GaussUltraspherical

CASE ("GAUSSULTRASPHERICALLOBATTO")
  ans = GaussUltrasphericalLobatto

CASE ("GAUSSULTRASPHERICALRADAU")
  ans = GaussUltrasphericalRadau

CASE ("GAUSSULTRASPHERICALRADAULEFT")
  ans = GaussUltrasphericalRadauLeft

CASE ("GAUSSULTRASPHERICALRADAURIGHT")
  ans = GaussUltrasphericalRadauRight

CASE DEFAULT
  ans = -1_I4B
  CALL Errormsg(&
    & msg="No case found for given quadratureType name", &
    & file=__FILE__, &
    & line=__LINE__,&
    & routine="QuadraturePointNameToID()", &
    & unitno=stderr)
END SELECT
END PROCEDURE QuadraturePointNameToID

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate1
obj%points = points
obj%tXi = SIZE(points, 1) - 1
! No of row minus one
END PROCEDURE quad_initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate2
obj%tXi = tXi
CALL Reallocate(obj%points, tXi + 1, tpoints)
END PROCEDURE quad_initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate3
INTEGER(I4B) :: quadType
quadType = QuadraturePointNameToId(quadratureType)
CALL Initiate( &
  & obj=obj, &
  & refElem=refElem, &
  & order=order, &
  & quadratureType=quadType,  &
  & alpha=alpha,  &
  & beta=beta,  &
  & lambda=lambda)
END PROCEDURE quad_initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate4
INTEGER(I4B) :: quadType
quadType = QuadraturePointNameToId(quadratureType)
CALL Initiate( &
  & obj=obj, &
  & refElem=refElem, &
  & nips=nips, &
  & quadratureType=quadType,  &
  & alpha=alpha,  &
  & beta=beta,  &
  & lambda=lambda)
END PROCEDURE quad_initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate5

SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Line( &
      & order=order, &
      & quadType=quadratureType, &
      & layout="INCREASING", &
      & xij=LocalNodeCoord(refElem), &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda) &
    & )

TYPE IS (ReferenceTriangle_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Triangle( &
      & order=order, &
      & quadType=quadratureType, &
      & refTriangle="UNIT", &
      & xij=LocalNodeCoord(refElem)) &
    & )

TYPE IS (ReferenceQuadrangle_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Quadrangle( &
      & order=order, &
      & quadType=quadratureType, &
      & refQuadrangle="BIUNIT", &
      & xij=LocalNodeCoord(refElem), &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda) &
    & )

TYPE IS (ReferenceTetrahedron_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Tetrahedron( &
      & order=order, &
      & quadType=quadratureType, &
      & refTetrahedron="UNIT", &
      & xij=LocalNodeCoord(refElem)) &
    & )

TYPE IS (ReferenceHexahedron_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Hexahedron( &
      & order=order, &
      & quadType=quadratureType, &
      & refHexahedron="BIUNIT", &
      & xij=LocalNodeCoord(refElem), &
      & alpha=alpha,  &
      & beta=beta,  &
      & lambda=lambda) &
    & )

TYPE IS (ReferencePrism_)

  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Prism( &
      & order=order, &
      & quadType=quadratureType, &
      & refPrism="BIUNIT", &
      & xij=LocalNodeCoord(refElem)) &
    & )

TYPE IS (ReferencePyramid_)

  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Pyramid( &
      & order=order, &
      & quadType=quadratureType, &
      & refPyramid="BIUNIT", &
      & xij=LocalNodeCoord(refElem)) &
    & )

TYPE IS (ReferenceElement_)

  IF (isLine(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Line( &
        & order=order, &
        & quadType=quadratureType, &
        & layout="INCREASING", &
        & xij=LocalNodeCoord(refElem), &
        & alpha=alpha, &
        & beta=beta, &
        & lambda=lambda) &
      & )
    RETURN
  END IF

  IF (isTriangle(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Triangle( &
        & order=order, &
        & quadType=quadratureType, &
        & refTriangle="UNIT", &
        & xij=LocalNodeCoord(refElem)) &
      & )
    RETURN
  END IF

  IF (isQuadrangle(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Quadrangle( &
        & order=order, &
        & quadType=quadratureType, &
        & refQuadrangle="BIUNIT", &
        & xij=LocalNodeCoord(refElem), &
        & alpha=alpha, &
        & beta=beta, &
        & lambda=lambda) &
      & )
    RETURN
  END IF

  IF (isTetrahedron(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Tetrahedron( &
        & order=order, &
        & quadType=quadratureType, &
        & refTetrahedron="UNIT", &
        & xij=LocalNodeCoord(refElem)) &
      & )
    RETURN
  END IF

  IF (isHexahedron(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Hexahedron( &
        & order=order, &
        & quadType=quadratureType, &
        & refHexahedron="BIUNIT", &
        & xij=LocalNodeCoord(refElem), &
        & alpha=alpha,  &
        & beta=beta,  &
        & lambda=lambda) &
      & )
    RETURN
  END IF

  IF (isPrism(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Prism( &
        & order=order, &
        & quadType=quadratureType, &
        & refPrism="BIUNIT", &
        & xij=LocalNodeCoord(refElem)) &
      & )
    RETURN
  END IF

  IF (isPyramid(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Pyramid( &
        & order=order, &
        & quadType=quadratureType, &
        & refPyramid="BIUNIT", &
        & xij=LocalNodeCoord(refElem)) &
      & )
    RETURN
  END IF

CLASS DEFAULT
  CALL ErrorMsg(&
    & msg="No case found",  &
    & file=__FILE__,  &
    & routine="quad_initiate5()", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END SELECT

END PROCEDURE quad_initiate5

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_initiate6

SELECT TYPE (refelem)
TYPE IS (ReferenceLine_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Line( &
      & nips=nips, &
      & quadType=quadratureType, &
      & layout="INCREASING", &
      & xij=LocalNodeCoord(refElem), &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda) &
    & )

TYPE IS (ReferenceTriangle_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Triangle( &
      & nips=nips, &
      & quadType=quadratureType, &
      & refTriangle="UNIT", &
      & xij=LocalNodeCoord(refElem)) &
    & )

TYPE IS (ReferenceQuadrangle_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Quadrangle( &
      & nips=nips, &
      & quadType=quadratureType, &
      & refQuadrangle="BIUNIT", &
      & xij=LocalNodeCoord(refElem), &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda) &
    & )

TYPE IS (ReferenceTetrahedron_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Tetrahedron( &
      & nips=nips, &
      & quadType=quadratureType, &
      & refTetrahedron="UNIT", &
      & xij=LocalNodeCoord(refElem)) &
    & )

TYPE IS (ReferenceHexahedron_)
  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Hexahedron( &
      & nips=nips, &
      & quadType=quadratureType, &
      & refHexahedron="BIUNIT", &
      & xij=LocalNodeCoord(refElem), &
      & alpha=alpha,  &
      & beta=beta,  &
      & lambda=lambda) &
    & )

TYPE IS (ReferencePrism_)

  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Prism( &
      & nips=nips, &
      & quadType=quadratureType, &
      & refPrism="BIUNIT", &
      & xij=LocalNodeCoord(refElem)) &
    & )

TYPE IS (ReferencePyramid_)

  CALL Initiate( &
    & obj=obj, &
    & points=QuadraturePoint_Pyramid( &
      & nips=nips, &
      & quadType=quadratureType, &
      & refPyramid="BIUNIT", &
      & xij=LocalNodeCoord(refElem)) &
    & )

TYPE IS (ReferenceElement_)

  IF (isLine(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Line( &
        & nips=nips, &
        & quadType=quadratureType, &
        & layout="INCREASING", &
        & xij=LocalNodeCoord(refElem), &
        & alpha=alpha, &
        & beta=beta, &
        & lambda=lambda) &
      & )
    RETURN
  END IF

  IF (isTriangle(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Triangle( &
        & nips=nips, &
        & quadType=quadratureType, &
        & refTriangle="UNIT", &
        & xij=LocalNodeCoord(refElem)) &
      & )
    RETURN
  END IF

  IF (isQuadrangle(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Quadrangle( &
        & nips=nips, &
        & quadType=quadratureType, &
        & refQuadrangle="BIUNIT", &
        & xij=LocalNodeCoord(refElem), &
        & alpha=alpha, &
        & beta=beta, &
        & lambda=lambda) &
      & )
    RETURN
  END IF

  IF (isTetrahedron(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Tetrahedron( &
        & nips=nips, &
        & quadType=quadratureType, &
        & refTetrahedron="UNIT", &
        & xij=LocalNodeCoord(refElem)) &
      & )
    RETURN
  END IF

  IF (isHexahedron(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Hexahedron( &
        & nips=nips, &
        & quadType=quadratureType, &
        & refHexahedron="BIUNIT", &
        & xij=LocalNodeCoord(refElem), &
        & alpha=alpha,  &
        & beta=beta,  &
        & lambda=lambda) &
      & )
    RETURN
  END IF

  IF (isPrism(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Prism( &
        & nips=nips, &
        & quadType=quadratureType, &
        & refPrism="BIUNIT", &
        & xij=LocalNodeCoord(refElem)) &
      & )
    RETURN
  END IF

  IF (isPyramid(refelem%name)) THEN
    CALL Initiate( &
      & obj=obj, &
      & points=QuadraturePoint_Pyramid( &
        & nips=nips, &
        & quadType=quadratureType, &
        & refPyramid="BIUNIT", &
        & xij=LocalNodeCoord(refElem)) &
      & )
    RETURN
  END IF

CLASS DEFAULT
  CALL ErrorMsg(&
    & msg="No case found",  &
    & file=__FILE__,  &
    & routine="quad_initiate6()", &
    & line=__LINE__, &
    & unitno=stderr)
  RETURN
END SELECT

END PROCEDURE quad_initiate6

!----------------------------------------------------------------------------
!                                                            QuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Constructor1
obj%points = points
obj%tXi = SIZE(points, 1) - 1
END PROCEDURE quad_Constructor1

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Constructor_1
ALLOCATE (obj)
obj%points = points
obj%tXi = SIZE(points, 1) - 1
END PROCEDURE quad_Constructor_1

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Deallocate
IF (ALLOCATED(obj%points)) DEALLOCATE (obj%points)
obj%tXi = -1
END PROCEDURE quad_Deallocate

END SUBMODULE ConstructorMethods
