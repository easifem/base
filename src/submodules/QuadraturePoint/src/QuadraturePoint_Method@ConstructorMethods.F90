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
USE GlobalData, ONLY: stderr

USE ErrorHandling, ONLY: ErrorMsg

USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToString, &
                                    BaseInterpolation_ToInteger, &
                                    BaseInterpolation_ToChar
USE ReallocateUtility, ONLY: Reallocate

USE ReferenceElement_Method, ONLY: ElementTopology, &
                                   XiDimension

USE LineInterpolationUtility, ONLY: QuadratureNumber_Line, &
                                    QuadraturePoint_Line_
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_, &
                                        QuadratureNumber_Triangle

USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_, &
                                          QuadratureNumber_Quadrangle

USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_, &
                                           QuadratureNumber_Tetrahedron

USE HexahedronInterpolationUtility, ONLY: QuadraturePoint_Hexahedron_, &
                                          QuadratureNumber_Hexahedron

USE BaseType, ONLY: elem => TypeElemNameOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                  QuadraturePointIDToName
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePointIDToName
ans = BaseInterpolation_ToString(name)
END PROCEDURE QuadraturePointIDToName

!----------------------------------------------------------------------------
!                                                  QuadraturePointIDToName
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_ToChar
ans = BaseInterpolation_ToChar(name)
END PROCEDURE QuadraturePoint_ToChar

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
!                                                       QuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_QuadratureNumber1
INTEGER(I4B) :: ncol

SELECT CASE (topo)

CASE (elem%line)

  ans = QuadratureNumber_Line(order=order, quadtype=quadratureType)

CASE (elem%triangle)

  ans = QuadratureNumber_Triangle(order=order, quadtype=quadratureType)

CASE (elem%quadrangle)

  ans = QuadratureNumber_Line(order=order, quadtype=quadratureType)

CASE (elem%tetrahedron)

  ans = QuadratureNumber_Tetrahedron(order=order, quadtype=quadratureType)

! CASE (elem%hexahedron)
!
! CASE (elem%prism)
!
! CASE (elem%pyramid)

CASE DEFAULT
  CALL Errormsg(msg="No case found for give topo", &
            file=__FILE__, routine="obj_QuadratureNumber1()", line=__LINE__, &
                unitno=stderr)
  STOP

END SELECT

END PROCEDURE obj_QuadratureNumber1

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

MODULE PROCEDURE obj_Initiate5
CALL obj_Initiate9(obj=obj, elemType=refelem%name, &
  domainName=refelem%domainName, order=order, quadratureType=quadratureType, &
                   alpha=alpha, beta=beta, lambda=lambda, xij=refelem%xij)
END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate6
CALL obj_Initiate10(obj=obj, elemType=refelem%name, &
    domainName=refelem%domainName, nips=nips, quadratureType=quadratureType, &
                    alpha=alpha, beta=beta, lambda=lambda, xij=refelem%xij)
END PROCEDURE obj_Initiate6

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate8
CALL obj_Initiate12(obj=obj, elemType=refelem%name, &
       domainName=refelem%domainName, nipsx=nipsx, nipsy=nipsy, nipsz=nipsz, &
           quadratureType1=quadratureType1, quadratureType2=quadratureType2, &
                quadratureType3=quadratureType3, alpha1=alpha1, beta1=beta1, &
               lambda1=lambda1, alpha2=alpha2, beta2=beta2, lambda2=lambda2, &
                 alpha3=alpha3, beta3=beta3, lambda3=lambda3, xij=refelem%xij)
END PROCEDURE obj_Initiate8

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate9
CALL obj_Initiate11(obj=obj, elemType=elemtype, domainName=domainname, &
                  p=order, q=order, r=order, quadratureType1=quadratureType, &
             quadratureType2=quadratureType, quadratureType3=quadratureType, &
         alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
            lambda2=lambda, alpha3=alpha, beta3=beta, lambda3=lambda, xij=xij)
END PROCEDURE obj_Initiate9

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate10
CALL obj_Initiate12(obj=obj, elemType=elemtype, domainName=domainName, &
         nipsx=nips, nipsy=nips, nipsz=nips, quadratureType1=quadratureType, &
             quadratureType2=quadratureType, quadratureType3=quadratureType, &
         alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
            lambda2=lambda, alpha3=alpha, beta3=beta, lambda3=lambda, xij=xij)
END PROCEDURE obj_Initiate10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate11
INTEGER(I4B) :: topo, nrow, ncol, ii, nipsx(1), nipsy(1), nipsz(1)

topo = ElementTopology(elemType)

ii = XiDimension(elemType)

IF (PRESENT(xij)) THEN
  nrow = MAX(SIZE(xij, 1), ii)
ELSE
  nrow = ii
END IF

nrow = nrow + 1

SELECT CASE (topo)

CASE (elem%line)

  nipsx(1) = QuadratureNumber_Line(order=p, quadtype=quadratureType1)

  ncol = nipsx(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Line_(nips=nipsx, quadType=quadratureType1, &
                     layout="INCREASING", xij=xij, alpha=alpha1, beta=beta1, &
                         lambda=lambda1, ans=obj%points, nrow=nrow, ncol=ncol)

CASE (elem%triangle)

  nipsx(1) = QuadratureNumber_Triangle(order=p, quadtype=quadratureType1)
  ncol = nipsx(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Triangle_(nips=nipsx, quadType=quadratureType1, &
        refTriangle=domainName, xij=xij, ans=obj%points, nrow=nrow, ncol=ncol)

CASE (elem%quadrangle)

  nipsx(1) = QuadratureNumber_Line(order=p, quadtype=quadratureType1)
  nipsy(1) = QuadratureNumber_Line(order=q, quadtype=quadratureType2)

  ncol = nipsx(1) * nipsy(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Quadrangle_(nipsx=nipsx, nipsy=nipsy, &
                       quadType1=quadratureType1, quadType2=quadratureType2, &
              refQuadrangle=domainName, xij=xij, alpha1=alpha1, beta1=beta1, &
               lambda1=lambda1, alpha2=alpha2, beta2=beta2, lambda2=lambda2, &
                                   ans=obj%points, nrow=nrow, ncol=ncol)

CASE (elem%tetrahedron)

  nipsx(1) = QuadratureNumber_Tetrahedron(order=p, quadtype=quadratureType1)
  ncol = nipsx(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Tetrahedron_(nips=nipsx, quadType=quadratureType1, &
     refTetrahedron=domainName, xij=xij, ans=obj%points, nrow=nrow, ncol=ncol)

CASE (elem%hexahedron)

  nipsx(1) = QuadratureNumber_Line(order=p, quadtype=quadratureType1)
  nipsy(1) = QuadratureNumber_Line(order=q, quadtype=quadratureType2)
  nipsz(1) = QuadratureNumber_Line(order=r, quadtype=quadratureType3)

  ncol = nipsx(1) * nipsy(1) * nipsz(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Hexahedron_(nipsx=nipsx, nipsy=nipsy, nipsz=nipsz, &
                                   quadType1=quadratureType1, &
                                   quadType2=quadratureType2, &
                                   quadType3=quadratureType3, &
                                   refHexahedron=domainName, xij=xij, &
                                alpha1=alpha1, beta1=beta1, lambda1=lambda1, &
                                alpha2=alpha2, beta2=beta2, lambda2=lambda2, &
                                alpha3=alpha3, beta3=beta3, lambda3=lambda3, &
                                   ans=obj%points, nrow=nrow, ncol=ncol)

! CASE (Prism)

! CASE (Pyramid)

CASE DEFAULT
  CALL Errormsg(msg="No case found for give topo", &
                file=__FILE__, routine="obj_Initiate11()", line=__LINE__, &
                unitno=stderr)
  STOP

END SELECT

obj%txi = SIZE(obj%points, 1) - 1

END PROCEDURE obj_Initiate11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate12
INTEGER(I4B) :: topo, nrow, ncol, ii

topo = ElementTopology(elemType)

ii = XiDimension(elemType)

IF (PRESENT(xij)) THEN
  nrow = MAX(SIZE(xij, 1), ii)
ELSE
  nrow = ii
END IF

nrow = nrow + 1

SELECT CASE (topo)

CASE (elem%line)
  ncol = nipsx(1)
  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Line_(nips=nipsx, quadType=quadratureType1, &
                     layout="INCREASING", xij=xij, alpha=alpha1, beta=beta1, &
                         lambda=lambda1, ans=obj%points, nrow=nrow, ncol=ncol)

CASE (elem%triangle)

  ncol = nipsx(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Triangle_(nips=nipsx, quadType=quadratureType1, &
        refTriangle=domainName, xij=xij, ans=obj%points, nrow=nrow, ncol=ncol)

CASE (elem%quadrangle)

  ncol = nipsx(1) * nipsy(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Quadrangle_(nipsx=nipsx, nipsy=nipsy, &
                       quadType1=quadratureType1, quadType2=quadratureType2, &
              refQuadrangle=domainName, xij=xij, alpha1=alpha1, beta1=beta1, &
               lambda1=lambda1, alpha2=alpha2, beta2=beta2, lambda2=lambda2, &
                                   ans=obj%points, nrow=nrow, ncol=ncol)

CASE (elem%tetrahedron)

  ncol = nipsx(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Tetrahedron_(nips=nipsx, quadType=quadratureType1, &
     refTetrahedron=domainName, xij=xij, ans=obj%points, nrow=nrow, ncol=ncol)

CASE (elem%hexahedron)

  ncol = nipsx(1) * nipsy(1) * nipsz(1)

  CALL Reallocate(obj%points, nrow, ncol)

  CALL QuadraturePoint_Hexahedron_(nipsx=nipsx, nipsy=nipsy, nipsz=nipsz, &
                                   quadType1=quadratureType1, &
                                   quadType2=quadratureType2, &
                                   quadType3=quadratureType3, &
                                   refHexahedron=domainName, &
                                   xij=xij, &
                                alpha1=alpha1, beta1=beta1, lambda1=lambda1, &
                                alpha2=alpha2, beta2=beta2, lambda2=lambda2, &
                                alpha3=alpha3, beta3=beta3, lambda3=lambda3, &
                                   ans=obj%points, nrow=nrow, ncol=ncol)

! CASE (Prism)
!
! CASE (Pyramid)

CASE DEFAULT
  CALL Errormsg(msg="No case found for give topo", &
                file=__FILE__, routine="obj_Initiate12()", line=__LINE__, &
                unitno=stderr)
  STOP

END SELECT

obj%txi = SIZE(obj%points, 1) - 1

END PROCEDURE obj_Initiate12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
