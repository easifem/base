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
USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToString, &
                                    BaseInterpolation_ToInteger
USE ReallocateUtility, ONLY: Reallocate

USE ReferenceElement_Method, ONLY: ElementTopology

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                  QuadraturePointIDToName
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePointIDToName
ans = BaseInterpolation_ToString(name)
END PROCEDURE QuadraturePointIDToName

!----------------------------------------------------------------------------
!                                                  QuadraturePointNameToID
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePointNameToID
ans = BaseInterpolation_ToInteger(name)
END PROCEDURE QuadraturePointNameToID

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

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(points, 1)
ncol = SIZE(points, 2)

CALL Reallocate(obj%points, nrow, ncol)

obj%points(1:nrow, 1:ncol) = points
obj%tXi = nrow - 1
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
obj%tXi = tXi
CALL Reallocate(obj%points, tXi + 1, tpoints)
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
INTEGER(I4B) :: quadType

quadType = QuadraturePointNameToId(quadratureType)
CALL Initiate(obj=obj, refElem=refElem, order=order, &
              quadratureType=quadType, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
INTEGER(I4B) :: quadType
quadType = QuadraturePointNameToId(quadratureType)
CALL Initiate(obj=obj, refElem=refElem, nips=nips, &
              quadratureType=quadType, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Initiate5
!
! SELECT TYPE (refelem)
!
! TYPE IS (ReferenceLine_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Line( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & layout="INCREASING", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha, &
!       & beta=beta, &
!       & lambda=lambda) &
!     & )
!
! TYPE IS (ReferenceTriangle_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Triangle( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refTriangle=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceQuadrangle_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Quadrangle( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refQuadrangle=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha, &
!       & beta=beta, &
!       & lambda=lambda) &
!     & )
!
! TYPE IS (ReferenceTetrahedron_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Tetrahedron( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refTetrahedron=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceHexahedron_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Hexahedron( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refHexahedron=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha,  &
!       & beta=beta,  &
!       & lambda=lambda) &
!     & )
!
! TYPE IS (ReferencePrism_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Prism( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refPrism=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferencePyramid_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Pyramid( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refPyramid=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceElement_)
!
!   IF (isLine(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Line( &
!         & order=order, &
!         & quadType=quadratureType, &
!         & layout="INCREASING", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha=alpha, &
!         & beta=beta, &
!         & lambda=lambda) &
!       & )
!     RETURN
!   END IF
!
!   IF (isTriangle(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Triangle( &
!         & order=order, &
!         & quadType=quadratureType, &
!         & refTriangle=refelem%domainName, &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isQuadrangle(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Quadrangle( &
!         & order=order, &
!         & quadType=quadratureType, &
!         & refQuadrangle=refelem%domainName, &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha=alpha, &
!         & beta=beta, &
!         & lambda=lambda) &
!       & )
!     RETURN
!   END IF
!
!   IF (isTetrahedron(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Tetrahedron( &
!         & order=order, &
!         & quadType=quadratureType, &
!         & refTetrahedron=refelem%domainName, &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isHexahedron(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Hexahedron( &
!         & order=order, &
!         & quadType=quadratureType, &
!         & refHexahedron=refelem%domainName, &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha=alpha,  &
!         & beta=beta,  &
!         & lambda=lambda) &
!       & )
!     RETURN
!   END IF
!
!   IF (isPrism(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Prism( &
!         & order=order, &
!         & quadType=quadratureType, &
!         & refPrism=refelem%domainName, &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isPyramid(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Pyramid( &
!         & order=order, &
!         & quadType=quadratureType, &
!         & refPyramid=refelem%domainName, &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
! CLASS DEFAULT
!   CALL ErrorMsg(msg="[NO CASE FOUND] for the type of refelem", &
!                 routine="obj_Initiate5()", &
!                 file=__FILE__, line=__LINE__, unitno=stderr)
!   RETURN
! END SELECT
!
! END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Initiate9
! INTEGER(I4B) :: topo
!
! topo = ElementTopology(elemType)
!
! SELECT CASE (topo)
! CASE (Line)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Line( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & layout="INCREASING", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha, &
!       & beta=beta, &
!       & lambda=lambda) &
!     & )
! CASE (Triangle)
! CASE (Quadrangle)
! CASE (Tetrahedron)
! CASE (Hexahedron)
! CASE (Prism)
! CASE (Pyramid)
! END SELECT
!
! IF (isLine(refelem%name)) THEN
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Line( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & layout="INCREASING", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha, &
!       & beta=beta, &
!       & lambda=lambda) &
!     & )
!   RETURN
! END IF
!
! IF (isTriangle(refelem%name)) THEN
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Triangle( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refTriangle=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!   RETURN
! END IF
!
! IF (isQuadrangle(refelem%name)) THEN
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Quadrangle( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refQuadrangle=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha, &
!       & beta=beta, &
!       & lambda=lambda) &
!     & )
!   RETURN
! END IF
!
! IF (isTetrahedron(refelem%name)) THEN
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Tetrahedron( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refTetrahedron=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!   RETURN
! END IF
!
! IF (isHexahedron(refelem%name)) THEN
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Hexahedron( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refHexahedron=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha,  &
!       & beta=beta,  &
!       & lambda=lambda) &
!     & )
!   RETURN
! END IF
!
! IF (isPrism(refelem%name)) THEN
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Prism( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refPrism=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!   RETURN
! END IF
!
! IF (isPyramid(refelem%name)) THEN
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Pyramid( &
!       & order=order, &
!       & quadType=quadratureType, &
!       & refPyramid=refelem%domainName, &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!   RETURN
! END IF
!
! END PROCEDURE obj_Initiate9

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Initiate6
!
! SELECT TYPE (refelem)
! TYPE IS (ReferenceLine_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Line( &
!       & nips=nips, &
!       & quadType=quadratureType, &
!       & layout="INCREASING", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha, &
!       & beta=beta, &
!       & lambda=lambda) &
!     & )
!
! TYPE IS (ReferenceTriangle_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Triangle( &
!       & nips=nips, &
!       & quadType=quadratureType, &
!       & refTriangle="UNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceQuadrangle_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Quadrangle( &
!       & nips=nips, &
!       & quadType=quadratureType, &
!       & refQuadrangle="BIUNIT", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha, &
!       & beta=beta, &
!       & lambda=lambda) &
!     & )
!
! TYPE IS (ReferenceTetrahedron_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Tetrahedron( &
!       & nips=nips, &
!       & quadType=quadratureType, &
!       & refTetrahedron="UNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceHexahedron_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Hexahedron( &
!       & nips=nips, &
!       & quadType=quadratureType, &
!       & refHexahedron="BIUNIT", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha,  &
!       & beta=beta,  &
!       & lambda=lambda) &
!     & )
!
! TYPE IS (ReferencePrism_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Prism( &
!       & nips=nips, &
!       & quadType=quadratureType, &
!       & refPrism="BIUNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferencePyramid_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Pyramid( &
!       & nips=nips, &
!       & quadType=quadratureType, &
!       & refPyramid="BIUNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceElement_)
!
!   IF (isLine(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Line( &
!         & nips=nips, &
!         & quadType=quadratureType, &
!         & layout="INCREASING", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha=alpha, &
!         & beta=beta, &
!         & lambda=lambda) &
!       & )
!     RETURN
!   END IF
!
!   IF (isTriangle(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Triangle( &
!         & nips=nips, &
!         & quadType=quadratureType, &
!         & refTriangle="UNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isQuadrangle(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Quadrangle( &
!         & nips=nips, &
!         & quadType=quadratureType, &
!         & refQuadrangle="BIUNIT", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha=alpha, &
!         & beta=beta, &
!         & lambda=lambda) &
!       & )
!     RETURN
!   END IF
!
!   IF (isTetrahedron(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Tetrahedron( &
!         & nips=nips, &
!         & quadType=quadratureType, &
!         & refTetrahedron="UNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isHexahedron(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Hexahedron( &
!         & nips=nips, &
!         & quadType=quadratureType, &
!         & refHexahedron="BIUNIT", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha=alpha,  &
!         & beta=beta,  &
!         & lambda=lambda) &
!       & )
!     RETURN
!   END IF
!
!   IF (isPrism(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Prism( &
!         & nips=nips, &
!         & quadType=quadratureType, &
!         & refPrism="BIUNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isPyramid(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Pyramid( &
!         & nips=nips, &
!         & quadType=quadratureType, &
!         & refPyramid="BIUNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
! CLASS DEFAULT
!   CALL ErrorMsg(&
!     & msg="No case found",  &
!     & file=__FILE__,  &
!     & routine="obj_Initiate6()", &
!     & line=__LINE__, &
!     & unitno=stderr)
!   RETURN
! END SELECT
!
! END PROCEDURE obj_Initiate6

!----------------------------------------------------------------------------
!                                                     QuadraturePoint
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Initiate7
!
! SELECT TYPE (refelem)
! TYPE IS (ReferenceLine_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Line( &
!       & order=p, &
!       & quadType=quadratureType1, &
!       & layout="INCREASING", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha1, &
!       & beta=beta1, &
!       & lambda=lambda1) &
!     & )
!
! TYPE IS (ReferenceTriangle_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Triangle( &
!       & order=p, &
!       & quadType=quadratureType1, &
!       & refTriangle="UNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceQuadrangle_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Quadrangle( &
!       & p=p, &
!       & q=q, &
!       & quadType1=quadratureType1, &
!       & quadType2=quadratureType2, &
!       & refQuadrangle="BIUNIT", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha1=alpha1, &
!       & beta1=beta1, &
!       & lambda1=lambda1, &
!       & alpha2=alpha2, &
!       & beta2=beta2, &
!       & lambda2=lambda2 &
!     & ))
!
! TYPE IS (ReferenceTetrahedron_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Tetrahedron( &
!       & order=p, &
!       & quadType=quadratureType1, &
!       & refTetrahedron="UNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceHexahedron_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Hexahedron( &
!       & p=p, &
!       & q=q, &
!       & r=r, &
!       & quadType1=quadratureType1, &
!       & quadType2=quadratureType2, &
!       & quadType3=quadratureType3, &
!       & refHexahedron="BIUNIT", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha1=alpha1, &
!       & beta1=beta1, &
!       & lambda1=lambda1, &
!       & alpha2=alpha2, &
!       & beta2=beta2, &
!       & lambda2=lambda2, &
!       & alpha3=alpha3,  &
!       & beta3=beta3,  &
!       & lambda3=lambda3 &
!     & ))
!
! TYPE IS (ReferencePrism_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Prism( &
!       & order=p, &
!       & quadType=quadratureType1, &
!       & refPrism="BIUNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferencePyramid_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Pyramid( &
!       & order=p, &
!       & quadType=quadratureType1, &
!       & refPyramid="BIUNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceElement_)
!
!   IF (isLine(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Line( &
!         & order=p, &
!         & quadType=quadratureType1, &
!         & layout="INCREASING", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha=alpha1, &
!         & beta=beta1, &
!         & lambda=lambda1) &
!       & )
!     RETURN
!   END IF
!
!   IF (isTriangle(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Triangle( &
!         & order=p, &
!         & quadType=quadratureType1, &
!         & refTriangle="UNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isQuadrangle(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Quadrangle( &
!         & p=p, &
!         & q=q, &
!         & quadType1=quadratureType1, &
!         & quadType2=quadratureType2, &
!         & refQuadrangle="BIUNIT", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha1=alpha1, &
!         & beta1=beta1, &
!         & lambda1=lambda1, &
!         & alpha2=alpha2, &
!         & beta2=beta2, &
!         & lambda2=lambda2 &
!       & ))
!     RETURN
!   END IF
!
!   IF (isTetrahedron(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Tetrahedron( &
!         & order=p, &
!         & quadType=quadratureType1, &
!         & refTetrahedron="UNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isHexahedron(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Hexahedron( &
!         & p=p, &
!         & q=q, &
!         & r=r, &
!         & quadType1=quadratureType1, &
!         & quadType2=quadratureType2, &
!         & quadType3=quadratureType3, &
!         & refHexahedron="BIUNIT", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha1=alpha1, &
!         & beta1=beta1, &
!         & lambda1=lambda1, &
!         & alpha2=alpha2, &
!         & beta2=beta2, &
!         & lambda2=lambda2, &
!         & alpha3=alpha3,  &
!         & beta3=beta3,  &
!         & lambda3=lambda3 &
!       & ))
!     RETURN
!   END IF
!
!   IF (isPrism(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Prism( &
!         & order=p, &
!         & quadType=quadratureType1, &
!         & refPrism="BIUNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isPyramid(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Pyramid( &
!         & order=p, &
!         & quadType=quadratureType1, &
!         & refPyramid="BIUNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
! CLASS DEFAULT
!   CALL ErrorMsg(&
!     & msg="No case found",  &
!     & file=__FILE__,  &
!     & routine="obj_Initiate7()", &
!     & line=__LINE__, &
!     & unitno=stderr)
!   RETURN
! END SELECT
!
! END PROCEDURE obj_Initiate7

!----------------------------------------------------------------------------
!                                                       QuadraturePoint
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Initiate8
!
! SELECT TYPE (refelem)
! TYPE IS (ReferenceLine_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Line( &
!       & nips=nipsx, &
!       & quadType=quadratureType1, &
!       & layout="INCREASING", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha=alpha1, &
!       & beta=beta1, &
!       & lambda=lambda1) &
!     & )
!
! TYPE IS (ReferenceTriangle_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Triangle( &
!       & nips=nipsx, &
!       & quadType=quadratureType1, &
!       & refTriangle="UNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceQuadrangle_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Quadrangle( &
!       & nipsx=nipsx, &
!       & nipsy=nipsy, &
!       & quadType1=quadratureType1, &
!       & quadType2=quadratureType2, &
!       & refQuadrangle="BIUNIT", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha1=alpha1, &
!       & beta1=beta1, &
!       & lambda1=lambda1, &
!       & alpha2=alpha2, &
!       & beta2=beta2, &
!       & lambda2=lambda2 &
!     & ))
!
! TYPE IS (ReferenceTetrahedron_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Tetrahedron( &
!       & nips=nipsx, &
!       & quadType=quadratureType1, &
!       & refTetrahedron="UNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceHexahedron_)
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Hexahedron( &
!       & nipsx=nipsx, &
!       & nipsy=nipsy, &
!       & nipsz=nipsz, &
!       & quadType1=quadratureType1, &
!       & quadType2=quadratureType2, &
!       & quadType3=quadratureType3, &
!       & refHexahedron="BIUNIT", &
!       & xij=LocalNodeCoord(refElem), &
!       & alpha1=alpha1, &
!       & beta1=beta1, &
!       & lambda1=lambda1, &
!       & alpha2=alpha2, &
!       & beta2=beta2, &
!       & lambda2=lambda2, &
!       & alpha3=alpha3,  &
!       & beta3=beta3,  &
!       & lambda3=lambda3 &
!     & ))
!
! TYPE IS (ReferencePrism_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Prism( &
!       & nips=nipsx, &
!       & quadType=quadratureType1, &
!       & refPrism="BIUNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferencePyramid_)
!
!   CALL Initiate( &
!     & obj=obj, &
!     & points=QuadraturePoint_Pyramid( &
!       & nips=nipsx, &
!       & quadType=quadratureType1, &
!       & refPyramid="BIUNIT", &
!       & xij=LocalNodeCoord(refElem)) &
!     & )
!
! TYPE IS (ReferenceElement_)
!
!   IF (isLine(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Line( &
!         & nips=nipsx, &
!         & quadType=quadratureType1, &
!         & layout="INCREASING", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha=alpha1, &
!         & beta=beta1, &
!         & lambda=lambda1) &
!       & )
!     RETURN
!   END IF
!
!   IF (isTriangle(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Triangle( &
!         & nips=nipsx, &
!         & quadType=quadratureType1, &
!         & refTriangle="UNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isQuadrangle(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Quadrangle( &
!         & nipsx=nipsx, &
!         & nipsy=nipsy, &
!         & quadType1=quadratureType1, &
!         & quadType2=quadratureType2, &
!         & refQuadrangle="BIUNIT", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha1=alpha1, &
!         & beta1=beta1, &
!         & lambda1=lambda1, &
!         & alpha2=alpha2, &
!         & beta2=beta2, &
!         & lambda2=lambda2 &
!       & ))
!     RETURN
!   END IF
!
!   IF (isTetrahedron(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Tetrahedron( &
!         & nips=nipsx, &
!         & quadType=quadratureType1, &
!         & refTetrahedron="UNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isHexahedron(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Hexahedron( &
!         & nipsx=nipsx, &
!         & nipsy=nipsy, &
!         & nipsz=nipsz, &
!         & quadType1=quadratureType1, &
!         & quadType2=quadratureType2, &
!         & quadType3=quadratureType3, &
!         & refHexahedron="BIUNIT", &
!         & xij=LocalNodeCoord(refElem), &
!         & alpha1=alpha1, &
!         & beta1=beta1, &
!         & lambda1=lambda1, &
!         & alpha2=alpha2, &
!         & beta2=beta2, &
!         & lambda2=lambda2, &
!         & alpha3=alpha3,  &
!         & beta3=beta3,  &
!         & lambda3=lambda3 &
!       & ))
!     RETURN
!   END IF
!
!   IF (isPrism(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Prism( &
!         & nips=nipsx, &
!         & quadType=quadratureType1, &
!         & refPrism="BIUNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
!   IF (isPyramid(refelem%name)) THEN
!     CALL Initiate( &
!       & obj=obj, &
!       & points=QuadraturePoint_Pyramid( &
!         & nips=nipsx, &
!         & quadType=quadratureType1, &
!         & refPyramid="BIUNIT", &
!         & xij=LocalNodeCoord(refElem)) &
!       & )
!     RETURN
!   END IF
!
! CLASS DEFAULT
!   CALL ErrorMsg(&
!     & msg="No case found",  &
!     & file=__FILE__,  &
!     & routine="obj_Initiate7()", &
!     & line=__LINE__, &
!     & unitno=stderr)
!   RETURN
! END SELECT
!
! END PROCEDURE obj_Initiate8

END SUBMODULE ConstructorMethods
