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

SUBMODULE(ElemshapeData_SetMethods) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               SetThickness
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetThickness
obj%Thickness = MATMUL(val, N)
END PROCEDURE elemsd_SetThickness

!----------------------------------------------------------------------------
!                                                               SetThickness
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetThickness
CALL SetThickness(obj=obj, val=MATMUL(val, T), N=N)
END PROCEDURE stsd_SetThickness

!----------------------------------------------------------------------------
!                                                        SetBarycentricCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetBarycentricCoord
obj%Coord = MATMUL(val, N)
END PROCEDURE elemsd_SetBarycentricCoord

!----------------------------------------------------------------------------
!                                                        SetBarycentricCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetBarycentricCoord
CALL SetBarycentricCoord(obj=obj, val=MATMUL(val, T), N=N)
END PROCEDURE stsd_SetBarycentricCoord

!----------------------------------------------------------------------------
!                                                                      SetJs
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetJs
! Define internal variable
INTEGER(I4B) :: xidim, nsd, nips, ips
REAL(DFP) :: aa, bb, ab
!
xidim = obj%RefElem%XiDimension
nsd = obj%RefElem%nsd
nips = SIZE(obj%N, 2)
!
DO ips = 1, nips
  IF (nsd .EQ. xidim) THEN
    obj%Js(ips) = det(obj%Jacobian(:, :, ips))
  ELSE IF (xidim .EQ. 1 .AND. xidim .NE. nsd) THEN
    obj%Js(ips) = &
      & SQRT(DOT_PRODUCT(obj%Jacobian(:, 1, ips), &
      & obj%Jacobian(:, 1, ips)))
  ELSE IF (xidim .EQ. 2 .AND. xidim .NE. nsd) THEN
    aa = DOT_PRODUCT(obj%Jacobian(:, 1, ips), obj%Jacobian(:, 1, ips))
    bb = DOT_PRODUCT(obj%Jacobian(:, 2, ips), obj%Jacobian(:, 2, ips))
    ab = DOT_PRODUCT(obj%Jacobian(:, 1, ips), obj%Jacobian(:, 2, ips))
    obj%Js(ips) = SQRT(aa * bb - ab * ab)
  END IF
END DO
END PROCEDURE elemsd_SetJs

!----------------------------------------------------------------------------
!                                                                  SetdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetdNdXt
! Define internal variables
INTEGER(I4B) :: NSD, XiDim, ips, nips
REAL(DFP), ALLOCATABLE :: InvJacobian(:, :, :)

NSD = obj%RefElem%NSD

XiDim = obj%RefElem%XiDimension

IF (NSD .NE. XiDim) THEN
  obj%dNdXt = 0.0_DFP
ELSE
  ! Compute inverse of Jacobian
  nips = SIZE(obj%N, 2)
  ALLOCATE (InvJacobian(NSD, NSD, nips))
  CALL Inv(InvA=InvJacobian, A=obj%Jacobian)
  DO ips = 1, nips
    obj%dNdXt(:, :, ips) = &
      & MATMUL(obj%dNdXi(:, :, ips), InvJacobian(:, :, ips))
  END DO
  DEALLOCATE (InvJacobian)
END IF
END PROCEDURE elemsd_SetdNdXt

!----------------------------------------------------------------------------
!                                                               SetJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetJacobian
obj%jacobian = MATMUL(val, dNdXi)
END PROCEDURE elemsd_SetJacobian

!----------------------------------------------------------------------------
!                                                                 SetJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetJacobian
obj%jacobian = MATMUL(MATMUL(val, T), dNdXi)
END PROCEDURE stsd_SetJacobian

!----------------------------------------------------------------------------
!                                                                 SetdNTdt
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetdNTdt
REAL(DFP), ALLOCATABLE :: v(:, :)
INTEGER(I4B) :: ip

! get mesh velocity at space integration points
v = MATMUL(MATMUL(val, obj%dTdTheta / obj%Jt), obj%N)
CALL Reallocate(obj%dNTdt, SIZE(obj%N, 1), SIZE(obj%T), &
  & SIZE(obj%N, 2))
DO ip = 1, SIZE(obj%N, 2)
  obj%dNTdt(:, :, ip) = OUTERPROD(obj%N(:, ip), obj%dTdTheta / obj%Jt) &
    & - MATMUL(obj%dNTdXt(:, :, :, ip), v(:, ip))
END DO
END PROCEDURE stsd_SetdNTdt

!----------------------------------------------------------------------------
!                                                                 SetdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetdNTdXt
!
INTEGER(I4B) :: ip, j
REAL(DFP), ALLOCATABLE :: Q(:, :), Temp(:, :)
!
CALL Reallocate(obj%dNTdXt, SIZE(obj%N, 1), SIZE(obj%T), &
  & SIZE(obj%Jacobian, 1), SIZE(obj%N, 2))
!
IF (obj%RefElem%XiDimension .NE. obj%RefElem%NSD) THEN
  RETURN
END IF
!
Q = obj%Jacobian(:, :, 1)
!
DO ip = 1, SIZE(obj%N, 2)
  CALL INV(A=obj%Jacobian(:, :, ip), INVA=Q)
  Temp = MATMUL(obj%dNdXi(:, :, ip), Q)
  DO j = 1, SIZE(Q, 1)
    obj%dNTdXt(:, :, j, ip) = OUTERPROD(Temp(:, j), obj%T)
  END DO
END DO
!
DEALLOCATE (Q, Temp)
!
END PROCEDURE stsd_SetdNTdXt

!----------------------------------------------------------------------------
!                                                                   SetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Set1
CALL SetJacobian(obj=obj, val=val, dNdXi=dNdXi)
CALL SetJs(obj=obj)
CALL SetdNdXt(obj=obj)
CALL SetBarycentricCoord(obj=obj, val=val, N=N)
END PROCEDURE elemsd_Set1

!----------------------------------------------------------------------------
!                                                                   SetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Set2
INTEGER(I4B), ALLOCATABLE :: facetNptrs(:)

CALL SetJacobian(obj=cellobj, val=cellVal, dNdXi=celldNdXi)
CALL SetJs(obj=cellobj)
CALL SetdNdXt(obj=cellobj)
CALL SetBarycentricCoord(obj=cellobj, val=cellval, N=cellN)

facetNptrs = GetConnectivity(facetobj%refelem)

CALL SetJacobian(obj=facetobj, val=cellVal(:, facetNptrs), &
  & dNdXi=facetdNdXi)
CALL SetJs(obj=facetobj)
CALL SetBarycentricCoord(obj=facetobj, val=cellval(:, facetNptrs), &
  & N=facetN)

CALL SetNormal(obj=facetobj)

! gradient depends upon all nodes of the element
! therefore the SIZE( dNdXt, 1 ) = NNS of cell

! CALL Reallocate( facetobj%dNdXt, SHAPE( cellobj%dNdXt) )
facetobj%dNdXt = cellobj%dNdXt

! I am copying normal Js from facet to cell
! In this way, we can use cellobj to construct the element matrix

cellobj%normal = facetobj%normal
cellobj%Js = facetobj%Js
cellobj%Ws = facetobj%Ws

IF (ALLOCATED(facetNptrs)) DEALLOCATE (facetNptrs)
END PROCEDURE elemsd_Set2

!----------------------------------------------------------------------------
!                                                                   SetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Set3
!
CALL Set( &
  & facetobj=masterFacetObj, &
  & cellobj=masterCellObj, &
  & cellVal=masterCellVal, &
  & cellN=masterCellN, &
  & celldNdXi=masterCelldNdXi, &
  & facetN=masterFacetN, &
  & facetdNdXi=masterFacetdNdXi)
!
CALL Set( &
  & facetobj=slaveFacetObj, &
  & cellobj=slaveCellObj, &
  & cellVal=slaveCellVal, &
  & cellN=slaveCellN, &
  & celldNdXi=slaveCelldNdXi, &
  & facetN=slaveFacetN, &
  & facetdNdXi=slaveFacetdNdXi)
!
END PROCEDURE elemsd_Set3

!----------------------------------------------------------------------------
!                                                                   SetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE stelemsd_Set1
CALL SetJacobian(obj=obj, val=val, dNdXi=dNdXi, T=T)
CALL SetJs(obj=obj)
CALL SetdNdXt(obj=obj)
CALL SetBarycentricCoord(obj=obj, val=val, N=N, T=T)
CALL SetdNTdXt(obj=obj)
CALL SetdNTdt(obj=obj, val=val)
END PROCEDURE stelemsd_Set1

!----------------------------------------------------------------------------
!                                                                 SetNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetNormal
REAL(DFP) :: vec(3, 3)
INTEGER(I4B) :: i, xidim, nsd
vec = 0.0_DFP
vec(3, 2) = 1.0_DFP
xidim = obj%RefElem%XiDimension
nsd = obj%refElem%nsd
DO i = 1, SIZE(obj%N, 2)
  Vec(1:nsd, 1:xidim) = obj%Jacobian(1:nsd, 1:xidim, i)
  obj%Normal(:, i) = &
    & VectorProduct(Vec(:, 1), Vec(:, 2)) / obj%Js(i)
END DO
END PROCEDURE elemsd_SetNormal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
