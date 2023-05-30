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
! date: 4 March 2021
! summary: Constructor method for [[ElemshapeData_]] and [[STElemShapeData_]]

SUBMODULE(ElemshapeData_ConstructorMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Allocate
CALL reallocate(obj%N, nns, nips)
CALL reallocate(obj%dNdXi, nns, xidim, nips)
CALL reallocate(obj%Normal, 3, nips)
CALL reallocate(obj%dNdXt, nns, nsd, nips)
CALL reallocate(obj%Jacobian, nsd, xidim, nips)
CALL reallocate(obj%Js, nips)
CALL reallocate(obj%Thickness, nips)
obj%Thickness = 1.0_DFP
CALL reallocate(obj%Coord, nsd, nips)
END PROCEDURE elemsd_Allocate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Initiate1
  !!
SELECT CASE (TRIM(interpolType)//TRIM(continuityType))
  !!
  !!
  !!
CASE ("LagrangeInterpolation"//"H1")
  CALL Initiate( &
    & obj=obj, &
    & quad=quad, &
    & refElem=refElem, &
    & continuityType=TypeH1, &
    & interpolType=TypeLagrangeInterpolation)
  !!
  !!
  !!
CASE ("LagrangeInterpolation"//"H1Div")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: LagrangeInterpolation &
    & BaseContinuityType: H1Div", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("LagrangeInterpolation"//"H1Curl")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: LagrangeInterpolation &
    & BaseContinuityType: H1Curl", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("LagrangeInterpolation"//"DG")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: LagrangeInterpolation &
    & BaseContinuityType: DG", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("HermitInterpolation"//"H1")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: HermitInterpolation &
    & BaseContinuityType: H1", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("HermitInterpolation"//"H1Div")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: HermitInterpolation &
    & BaseContinuityType: H1Div", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("HermitInterpolation"//"H1Curl")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: HermitInterpolation &
    & BaseContinuityType: H1Curl", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("HermitInterpolation"//"DG")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: HermitInterpolation &
    & BaseContinuityType: DG", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("SerendipityInterpolation"//"H1")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: SerendipityInterpolation &
    & BaseContinuityType: H1", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("SerendipityInterpolation"//"H1Div")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: SerendipityInterpolation &
    & BaseContinuityType: H1Div", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("SerendipityInterpolation"//"H1Curl")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: SerendipityInterpolation &
    & BaseContinuityType: H1Curl", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("SerendipityInterpolation"//"DG")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: SerendipityInterpolation &
    & BaseContinuityType: DG", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("HierarchyInterpolation"//"H1")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: HierarchyInterpolation &
    & BaseContinuityType: H1", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("HierarchyInterpolation"//"H1Div")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: HierarchyInterpolation &
    & BaseContinuityType: H1Div", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("HierarchyInterpolation"//"H1Curl")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: HierarchyInterpolation &
    & BaseContinuityType: H1Curl", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE ("HierarchyInterpolation"//"DG")
  CALL ErrorMSG( &
    & Msg="BaseInterpolation: HierarchyInterpolation &
    & BaseContinuityType: DG", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
  STOP
  !!
  !!
  !!
CASE DEFAULT
  CALL ErrorMSG( &
    & Msg="Unknown child name of BaseInterpolation &
    & and BaseContinuityType", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_Initiate1()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
END SELECT
  !!
  !!
  !!
END PROCEDURE elemsd_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_initiate2
IF (ALLOCATED(obj2%N)) obj1%N = obj2%N
IF (ALLOCATED(obj2%dNdXi)) obj1%dNdXi = obj2%dNdXi
IF (ALLOCATED(obj2%jacobian)) obj1%jacobian = obj2%jacobian
IF (ALLOCATED(obj2%js)) obj1%js = obj2%js
IF (ALLOCATED(obj2%ws)) obj1%ws = obj2%ws
IF (ALLOCATED(obj2%dNdXt)) obj1%dNdXt = obj2%dNdXt
IF (ALLOCATED(obj2%thickness)) obj1%thickness = obj2%thickness
IF (ALLOCATED(obj2%coord)) obj1%coord = obj2%coord
IF (ALLOCATED(obj2%normal)) obj1%normal = obj2%normal
obj1%refElem = obj2%refElem
END PROCEDURE elemsd_initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_initiate3
IF (ALLOCATED(obj2%N)) obj1%N = obj2%N
IF (ALLOCATED(obj2%dNdXi)) obj1%dNdXi = obj2%dNdXi
IF (ALLOCATED(obj2%jacobian)) obj1%jacobian = obj2%jacobian
IF (ALLOCATED(obj2%js)) obj1%js = obj2%js
IF (ALLOCATED(obj2%ws)) obj1%ws = obj2%ws
IF (ALLOCATED(obj2%dNdXt)) obj1%dNdXt = obj2%dNdXt
IF (ALLOCATED(obj2%thickness)) obj1%thickness = obj2%thickness
IF (ALLOCATED(obj2%coord)) obj1%coord = obj2%coord
IF (ALLOCATED(obj2%normal)) obj1%normal = obj2%normal
obj1%refElem = obj2%refElem
END PROCEDURE elemsd_initiate3

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_initiate4
IF (ALLOCATED(obj2%N)) obj1%N = obj2%N
IF (ALLOCATED(obj2%dNdXi)) obj1%dNdXi = obj2%dNdXi
IF (ALLOCATED(obj2%jacobian)) obj1%jacobian = obj2%jacobian
IF (ALLOCATED(obj2%js)) obj1%js = obj2%js
IF (ALLOCATED(obj2%ws)) obj1%ws = obj2%ws
IF (ALLOCATED(obj2%dNdXt)) obj1%dNdXt = obj2%dNdXt
IF (ALLOCATED(obj2%thickness)) obj1%thickness = obj2%thickness
IF (ALLOCATED(obj2%coord)) obj1%coord = obj2%coord
IF (ALLOCATED(obj2%normal)) obj1%normal = obj2%normal
obj1%refElem = obj2%refElem
END PROCEDURE elemsd_initiate4

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_initiate5
  !!
IF (ALLOCATED(obj2%N)) obj1%N = obj2%N
IF (ALLOCATED(obj2%dNdXi)) obj1%dNdXi = obj2%dNdXi
IF (ALLOCATED(obj2%jacobian)) obj1%jacobian = obj2%jacobian
IF (ALLOCATED(obj2%js)) obj1%js = obj2%js
IF (ALLOCATED(obj2%ws)) obj1%ws = obj2%ws
IF (ALLOCATED(obj2%dNdXt)) obj1%dNdXt = obj2%dNdXt
IF (ALLOCATED(obj2%thickness)) obj1%thickness = obj2%thickness
IF (ALLOCATED(obj2%coord)) obj1%coord = obj2%coord
IF (ALLOCATED(obj2%normal)) obj1%normal = obj2%normal
obj1%refElem = obj2%refElem
obj1%wt = obj2%wt
obj1%theta = obj2%theta
obj1%jt = obj2%jt
IF (ALLOCATED(obj2%T)) obj1%T = obj2%T
IF (ALLOCATED(obj2%dTdTheta)) obj1%dTdTheta = obj2%dTdTheta
IF (ALLOCATED(obj2%dNTdt)) obj1%dNTdt = obj2%dNTdt
IF (ALLOCATED(obj2%dNTdXt)) obj1%dNTdXt = obj2%dNTdXt
  !!
END PROCEDURE elemsd_initiate5

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_initiate
INTEGER(I4B) :: tip, ip
REAL(DFP) :: x(3)
tip = SIZE(elemsd%N, 2)
IF (ALLOCATED(obj)) DEALLOCATE (obj)
ALLOCATE (obj(tip))
DO ip = 1, tip
  obj(ip)%T = elemsd%N(:, ip)
  obj(ip)%dTdTheta = elemsd%dNdXi(:, 1, ip)
  obj(ip)%Jt = elemsd%Js(ip)
  CALL getQuadraturePoints( &
    & obj=elemsd%quad, &
    & weight=obj(ip)%wt,&
    & num=ip, &
    & point=x)
  obj(ip)%theta = x(1)
END DO
END PROCEDURE stsd_initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Deallocate
IF (ALLOCATED(obj%Normal)) DEALLOCATE (obj%Normal)
IF (ALLOCATED(obj%N)) DEALLOCATE (obj%N)
IF (ALLOCATED(obj%dNdXi)) DEALLOCATE (obj%dNdXi)
IF (ALLOCATED(obj%dNdXt)) DEALLOCATE (obj%dNdXt)
IF (ALLOCATED(obj%Jacobian)) DEALLOCATE (obj%Jacobian)
IF (ALLOCATED(obj%Js)) DEALLOCATE (obj%Js)
IF (ALLOCATED(obj%Ws)) DEALLOCATE (obj%Ws)
IF (ALLOCATED(obj%Thickness)) DEALLOCATE (obj%Thickness)
IF (ALLOCATED(obj%Coord)) DEALLOCATE (obj%Coord)
CALL DEALLOCATE (obj%Quad)
CALL DEALLOCATE (obj%refelem)
  !!
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  IF (ALLOCATED(obj%T)) DEALLOCATE (obj%T)
  IF (ALLOCATED(obj%dTdTheta)) DEALLOCATE (obj%dTdTheta)
  IF (ALLOCATED(obj%dNTdt)) DEALLOCATE (obj%dNTdt)
  IF (ALLOCATED(obj%dNTdXt)) DEALLOCATE (obj%dNTdXt)
END SELECT
END PROCEDURE elemsd_Deallocate

!----------------------------------------------------------------------------
!                                                         BaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_BaseInterpolation
SELECT CASE (TRIM(childName))
CASE ("LagrangeInterpolation")
  ALLOCATE (LagrangeInterpolation_ :: ans)
CASE ("HermitInterpolation")
  ALLOCATE (HermitInterpolation_ :: ans)
CASE ("SerendipityInterpolation")
  ALLOCATE (SerendipityInterpolation_ :: ans)
CASE ("HierarchyInterpolation")
  ALLOCATE (HierarchyInterpolation_ :: ans)
CASE DEFAULT
  CALL ErrorMSG( &
    & Msg="Unknown child name of BaseInterpolation.", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_BaseInterpolation()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
END SELECT
END PROCEDURE elemsd_BaseInterpolation

!----------------------------------------------------------------------------
!                                                            BaseContinuity
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_BaseContinuity
SELECT CASE (TRIM(childName))
CASE ("H1")
  ALLOCATE (H1_ :: ans)
CASE ("H1Div")
  ALLOCATE (H1Div_ :: ans)
CASE ("H1Curl")
  ALLOCATE (H1Curl_ :: ans)
CASE ("DG")
  ALLOCATE (DG_ :: ans)
CASE DEFAULT
  CALL ErrorMSG( &
    & Msg="Unknown child name of BaseContinuity.", &
    & File="ElemshapeData_Method@Constructor.F90", &
    & Routine="elemsd_BaseContinuity()", &
    & Line=__LINE__, &
    & UnitNo=stdout)
END SELECT
END PROCEDURE elemsd_BaseContinuity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
