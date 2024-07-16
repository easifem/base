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
! summary: Constructor method for ElemshapeData_ and STElemshapeData_

SUBMODULE(ElemshapeData_ConstructorMethods) Methods
USE ReallocateUtility, ONLY: Reallocate

USE QuadraturePoint_Method, ONLY: GetQuadraturePoints

USE ErrorHandling, ONLY: Errormsg

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Allocate
LOGICAL(LGT) :: isok

CALL Reallocate(obj%N, nns, nips)
CALL Reallocate(obj%dNdXi, nns, xidim, nips)
CALL Reallocate(obj%Normal, 3, nips)
CALL Reallocate(obj%dNdXt, nns, nsd, nips)
CALL Reallocate(obj%jacobian, nsd, xidim, nips)
CALL Reallocate(obj%js, nips)
CALL Reallocate(obj%thickness, nips)
obj%thickness = 1.0_DFP
CALL Reallocate(obj%coord, nsd, nips)
obj%nsd = nsd
obj%xidim = xidim
obj%nips = nips
obj%nns = nns

isok = PRESENT(nnt)

IF (isok) THEN
  SELECT TYPE (obj); TYPE is (STElemShapeData_)
    obj%nnt = nnt

    CALL Reallocate(obj%T, nnt)
    CALL Reallocate(obj%dTdTheta, nnt)
    CALL Reallocate(obj%dNTdt, nns, nnt, nips)
    CALL Reallocate(obj%dNTdXt, nns, nnt, nsd, nips)

  END SELECT
END IF

END PROCEDURE elemsd_Allocate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Initiate1

CALL ErrorMSG( &
  & Msg="[WORK IN PROGRESS]", &
  & File=__FILE__, &
  & Routine="elemsd_Initiate1()", &
  & Line=__LINE__, &
  & UnitNo=stdout)
STOP
END PROCEDURE elemsd_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Initiate2
INTEGER(I4B) :: ii, jj, kk, nns, nsd, xidim, nips, nnt, ll, nnt

nns = obj2%nns
nsd = obj2%nsd
xidim = obj2%xidim
nips = obj2%nips

SELECT TYPE (obj2); TYPE is (STElemShapeData_)
  nnt = obj2%nnt
END SELECT

CALL elemsd_Allocate(obj=obj1, nsd=nsd, xidim=xidim, nns=nns, &
                     nips=nips, nnt=nnt)

DO CONCURRENT(jj=1:nips, ii=1:nns)
  obj1%N(ii, jj) = obj2%N(ii, jj)
END DO

DO CONCURRENT(kk=1:nips, jj=1:xidim, ii=1:nns)
  obj1%dNdXi(ii, jj, kk) = obj2%dNdXi(ii, jj, kk)
END DO

DO CONCURRENT(kk=1:nips, jj=1:nsd, ii=1:nns)
  obj1%dNdXt(ii, jj, kk) = obj2%dNdXt(ii, jj, kk)
END DO

DO CONCURRENT(ii=1:nsd, jj=1:xidim, kk=1:nips)
  obj1%jacobian(ii, jj, kk) = obj2%jacobian(ii, jj, kk)
END DO

DO CONCURRENT(ii=1:nips)
  obj1%js(ii) = obj2%js(ii)
  obj1%ws(ii) = obj2%ws(ii)
  obj1%thickness(ii) = obj2%thickness(ii)
  obj1%coord(1:nsd, ii) = obj2%coord(1:nsd, ii)
  obj1%normal(1:3, ii) = obj2%normal(1:3, ii)
END DO

SELECT TYPE (obj1); TYPE is (STElemShapeData_)
  SELECT TYPE (obj2); TYPE is (STElemShapeData_)
    obj1%wt = obj2%wt
! obj1%theta = obj2%theta
    obj1%jt = obj2%jt
    obj1%nnt = obj2%nnt
    nnt = obj1%nnt

    DO CONCURRENT(ii=1:nnt)
      obj1%T(ii) = obj2%T(ii)
      obj1%dTdTheta(ii) = obj2%dTdTheta(ii)
    END DO

    DO CONCURRENT(ii=1:nns, jj=1:nnt, kk=1:nips)
      obj1%dNTdt(ii, jj, kk) = obj2%dNTdt(ii, jj, kk)
    END DO

    DO CONCURRENT(ii=1:nns, jj=1:nnt, kk=1:nsd, ll=1:nips)
      obj1%dNTdXt(ii, jj, kk, ll) = obj2%dNTdXt(ii, jj, kk, ll)
    END DO

  END SELECT
END SELECT

END PROCEDURE elemsd_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_Initiate
INTEGER(I4B) :: tip, ip, nnt

tip = elemsd%nips

IF (ALLOCATED(obj)) THEN
  DO ip = 1, SIZE(obj)
    CALL DEALLOCATE (obj(ip))
  END DO
  DEALLOCATE (obj)
END IF

ALLOCATE (obj(tip))

nnt = elemsd%nns

DO ip = 1, tip
  obj(ip)%jt = elemsd%js(ip)
  obj(ip)%wt = elemsd%ws(ip)
  obj(ip)%nnt = nnt

  CALL Reallocate(obj(ip)%T, nnt)
  obj(ip)%T(1:nnt) = elemsd%N(1:nnt, ip)

  CALL Reallocate(obj(ip)%dTdTheta, nnt)
  obj(ip)%dTdTheta(1:nnt) = elemsd%dNdXi(1:nnt, 1, ip)
END DO

END PROCEDURE stsd_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Deallocate
IF (ALLOCATED(obj%normal)) DEALLOCATE (obj%normal)
IF (ALLOCATED(obj%N)) DEALLOCATE (obj%N)
IF (ALLOCATED(obj%dNdXi)) DEALLOCATE (obj%dNdXi)
IF (ALLOCATED(obj%dNdXt)) DEALLOCATE (obj%dNdXt)
IF (ALLOCATED(obj%jacobian)) DEALLOCATE (obj%jacobian)
IF (ALLOCATED(obj%js)) DEALLOCATE (obj%js)
IF (ALLOCATED(obj%ws)) DEALLOCATE (obj%ws)
IF (ALLOCATED(obj%thickness)) DEALLOCATE (obj%thickness)
IF (ALLOCATED(obj%coord)) DEALLOCATE (obj%coord)

obj%nsd = 0
obj%xidim = 0
obj%nips = 0
obj%nns = 0
! CALL DEALLOCATE (obj%Quad)
! CALL DEALLOCATE (obj%refelem)
SELECT TYPE (obj)
TYPE IS (STElemShapeData_)
  obj%nnt = 0
  obj%wt = 0
  obj%jt = 0
  IF (ALLOCATED(obj%T)) DEALLOCATE (obj%T)
  IF (ALLOCATED(obj%dTdTheta)) DEALLOCATE (obj%dTdTheta)
  IF (ALLOCATED(obj%dNTdt)) DEALLOCATE (obj%dNTdt)
  IF (ALLOCATED(obj%dNTdXt)) DEALLOCATE (obj%dNTdXt)
END SELECT
END PROCEDURE elemsd_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
