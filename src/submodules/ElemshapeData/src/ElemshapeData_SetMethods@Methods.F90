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
USE ProductUtility, ONLY: VectorProduct, OuterProd
USE InvUtility, ONLY: Det, Inv
USE ReallocateUtility, ONLY: Reallocate
USE MatmulUtility

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               SetThickness
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetThickness
obj%thickness(1:obj%nips) = MATMUL(val, N)
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
INTEGER(I4B) :: valNNS

valNNS = SIZE(val, 2)
obj%coord(1:obj%nsd, 1:obj%nips) = MATMUL(val(1:obj%nsd, 1:valNNS), &
                                          N(1:valNNS, 1:obj%nips))
END PROCEDURE elemsd_SetBarycentricCoord

!----------------------------------------------------------------------------
!                                                        SetBarycentricCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetBarycentricCoord
! TODO: Improve this function by removing the temporary variable
! It is better to store a temporary variable in obj itself
CALL SetBarycentricCoord(obj=obj, val=MATMUL(val, T), N=N)
END PROCEDURE stsd_SetBarycentricCoord

!----------------------------------------------------------------------------
!                                                                      SetJs
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetJs
! Define internal variable
INTEGER(I4B) :: ips, caseid

REAL(DFP) :: aa, bb, ab

caseid = obj%xidim

IF (obj%nsd .EQ. obj%xidim) THEN
  caseid = 3
END IF

SELECT CASE (caseid)

CASE (1)
  DO ips = 1, obj%nips
    obj%js(ips) = NORM2(obj%jacobian(1:obj%nsd, 1, ips))
  END DO

CASE (2)

  DO ips = 1, obj%nips
    aa = DOT_PRODUCT(obj%jacobian(1:obj%nsd, 1, ips), &
                     obj%jacobian(1:obj%nsd, 1, ips))
    bb = DOT_PRODUCT(obj%jacobian(1:obj%nsd, 2, ips), &
                     obj%jacobian(1:obj%nsd, 2, ips))
    ab = DOT_PRODUCT(obj%jacobian(1:obj%nsd, 1, ips), &
                     obj%jacobian(1:obj%nsd, 2, ips))
    obj%js(ips) = SQRT(aa * bb - ab * ab)
  END DO

CASE (3)

  DO ips = 1, obj%nips
    obj%js(ips) = Det(obj%jacobian(1:obj%nsd, 1:obj%xidim, ips))
  END DO

END SELECT

END PROCEDURE elemsd_SetJs

!----------------------------------------------------------------------------
!                                                                  SetdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetdNdXt
! Define internal variables
INTEGER(I4B) :: ips
REAL(DFP) :: invJacobian(3, 3)
LOGICAL(LGT) :: abool

abool = obj%nsd .NE. obj%xidim

IF (abool) THEN
  obj%dNdXt(1:obj%nns, 1:obj%nsd, 1:obj%nips) = 0.0_DFP
  RETURN
END IF

DO ips = 1, obj%nips
  CALL Inv(InvA=invJacobian, A=obj%jacobian(1:obj%nsd, 1:obj%nsd, ips))

  obj%dNdXt(1:obj%nns, 1:obj%nsd, ips) = &
    MATMUL(obj%dNdXi(1:obj%nns, 1:obj%nsd, ips), &
           invJacobian(1:obj%nsd, 1:obj%nsd))
END DO
END PROCEDURE elemsd_SetdNdXt

!----------------------------------------------------------------------------
!                                                               SetJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_SetJacobian
INTEGER(I4B) :: valNNS, minNNS, ips

valNNS = SIZE(val, 2)
minNNS = MIN(valNNS, obj%nns)

DO ips = 1, obj%nips
  obj%jacobian(1:obj%nsd, 1:obj%xidim, ips) = MATMUL( &
                                              val(1:obj%nsd, 1:minNNS), &
                                            dNdXi(1:minNNS, 1:obj%xidim, ips))
END DO
END PROCEDURE elemsd_SetJacobian

!----------------------------------------------------------------------------
!                                                                 SetJacobian
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetJacobian
obj%jacobian(1:obj%nsd, 1:obj%xidim, 1:obj%nips) = &
  MATMUL(MATMUL(val(1:obj%nsd, :, :), T), &
         dNdXi(:, 1:obj%xidim, 1:obj%nips))
END PROCEDURE stsd_SetJacobian

!----------------------------------------------------------------------------
!                                                                 SetdNTdt
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetdNTdt
REAL(DFP), ALLOCATABLE :: v(:, :), mat2(:, :)
REAL(DFP) :: areal

INTEGER(I4B) :: ip, tsize

! get mesh velocity at space integration points

! CALL Reallocate(obj%dNTdt, obj%nns, obj%nnt, obj%nips)
areal = 1.0_DFP / obj%jt

tsize = MAX(obj%nns, obj%nips)
ALLOCATE (v(3, tsize), mat2(obj%nns, obj%nnt))

v(1:obj%nsd, 1:obj%nns) = MATMUL(val, obj%dTdTheta)
v(1:obj%nsd, 1:obj%nns) = v(1:obj%nsd, 1:obj%nns) * areal
v(1:obj%nsd, 1:obj%nips) = MATMUL(v(1:obj%nsd, 1:obj%nns), &
                                  obj%N(1:obj%nns, 1:obj%nips))

DO ip = 1, obj%nips
  mat2(1:obj%nns, 1:obj%nnt) = OUTERPROD(obj%N(1:obj%nns, ip), obj%dTdTheta(1:obj%nnt))
  mat2 = mat2 * areal

  obj%dNTdt(1:obj%nns, 1:obj%nnt, ip) = mat2 - &
     MATMUL(obj%dNTdXt(1:obj%nns, 1:obj%nnt, 1:obj%nsd, ip), v(1:obj%nsd, ip))

END DO

DEALLOCATE (v, mat2)

END PROCEDURE stsd_SetdNTdt

!----------------------------------------------------------------------------
!                                                                 SetdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_SetdNTdXt
REAL(DFP) :: Q(3, 3), temp(obj%nns, obj%nsd)
INTEGER(I4B) :: ip, j

CALL Reallocate(obj%dNTdXt, obj%nns, obj%nnt, obj%nsd, obj%nips)

IF (obj%xidim .NE. obj%nsd) THEN
  RETURN
END IF

DO ip = 1, obj%nips

  CALL INV(A=obj%jacobian(1:obj%nsd, 1:obj%xidim, ip), &
           INVA=Q(1:obj%nsd, 1:obj%nsd))

  temp = MATMUL(obj%dNdXi(1:obj%nns, 1:obj%xidim, ip), &
                Q(1:obj%nsd, 1:obj%nsd))

  DO j = 1, obj%nsd
    obj%dNTdXt(1:obj%nns, 1:obj%nnt, j, ip) = OUTERPROD(temp(1:obj%nns, j), &
                                                        obj%T(1:obj%nnt))
  END DO

END DO

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
call elemsd_Set1(obj=cellobj, val=cellval, N=cellN, dNdXi=celldNdXi)

CALL SetJacobian(obj=facetobj, val=facetval, dNdXi=facetdNdXi)
CALL SetJs(obj=facetobj)
CALL SetBarycentricCoord(obj=facetobj, val=facetval, N=facetN)
CALL SetNormal(obj=facetobj)

! gradient depends upon all nodes of the element
! therefore the SIZE( dNdXt, 1 ) = NNS of cell
! CALL Reallocate( facetobj%dNdXt, SHAPE( cellobj%dNdXt) )
! facetobj%dNdXt(1:facetobj%nns, 1:facetobj%nsd, 1:facetobj%nips) = &
!   cellobj%dNdXt(1:cellobj%nns, 1:cellobj%nsd, 1:cellobj%nips)

! I am copying normal Js from facet to cell
! In this way, we can use cellobj to construct the element matrix
cellobj%normal(1:cellobj%nsd, 1:cellobj%nips) = &
  facetobj%normal(1:facetobj%nsd, 1:facetobj%nips)

cellobj%Js(1:cellobj%nips) = facetobj%Js(1:facetobj%nips)
cellobj%Ws(1:cellobj%nips) = facetobj%Ws(1:facetobj%nips)
END PROCEDURE elemsd_Set2

!----------------------------------------------------------------------------
!                                                                   SetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_Set3
CALL Set( &
  facetobj=masterFacetObj, cellobj=masterCellObj, cellVal=masterCellVal, &
  cellN=masterCellN, celldNdXi=masterCelldNdXi, facetN=masterFacetN, &
  facetdNdXi=masterFacetdNdXi, facetval=masterFacetVal)

CALL Set( &
  facetobj=slaveFacetObj, cellobj=slaveCellObj, cellVal=slaveCellVal, &
  cellN=slaveCellN, celldNdXi=slaveCelldNdXi, facetN=slaveFacetN, &
  facetdNdXi=slaveFacetdNdXi, facetVal=slaveFacetVal)
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

xidim = obj%xidim

nsd = obj%nsd

DO i = 1, obj%nips

  vec(1:nsd, 1:xidim) = obj%jacobian(1:nsd, 1:xidim, i)
  obj%normal(1:3, i) = &
    VectorProduct(vec(:, 1), vec(:, 2)) / obj%js(i)

END DO
END PROCEDURE elemsd_SetNormal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
