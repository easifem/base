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

SUBMODULE(ElemShapeData_Hierarchical) Methods
USE ErrorHandling, ONLY: Errormsg
USE GlobalData, ONLY: stderr

USE InputUtility, ONLY: Input

USE ReferenceElement_Method, ONLY: Refelem_Initiate => Initiate, &
                                 Refelem_GetFaceElemType => GetFaceElemType, &
                                   Refelem_RefCoord_ => RefCoord_

USE ElemShapeData_Method, ONLY: Elemsd_Allocate => ALLOCATE

USE HierarchicalPolynomialUtility, ONLY: HierarchicalDOF, &
                                         HierarchicalEvalAll_, &
                                         HierarchicalGradientEvalAll_

USE QuadraturePoint_Method, ONLY: GetQuadraturePoints, &
                                  QuadraturePoint_Size => Size, &
                                  GetTotalQuadraturePoints, &
                                  GetQuadratureWeights_

USE BaseType, ONLY: TypeQuadratureOpt, &
                    TypePolynomialOpt

USE SwapUtility, ONLY: SWAP_

USE Display_Method, ONLY: Display

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                         ElemshapeData_InitiateHierarchical
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalElemShapeData1
REAL(DFP), ALLOCATABLE :: temp(:, :, :)
INTEGER(I4B) :: nips, nns, ii, jj, kk

! CALL DEALLOCATE (obj)

nips = GetTotalQuadraturePoints(obj=quad)

nns = HierarchicalDOF(elemType=elemType, cellOrder=cellOrder, &
                      faceOrder=faceOrder, edgeOrder=edgeOrder)

CALL Elemsd_Allocate(obj=obj, nsd=nsd, xidim=xidim, nns=nns, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=obj%ws, tsize=nips)

ALLOCATE (temp(nips, nns, 3))

CALL HierarchicalEvalAll_(elemType=elemType, &
                          xij=quad%points(1:xidim, 1:nips), &
                          ans=temp(:, :, 1), nrow=ii, ncol=jj, &
                          domainName=domainName, &
                          cellOrder=cellOrder, &
                          faceOrder=faceOrder, &
                          edgeOrder=edgeOrder, &
                          cellOrient=cellOrient, &
                          faceOrient=faceOrient, &
                          edgeOrient=edgeOrient)

DO CONCURRENT(ii=1:nns, jj=1:nips)
  obj%N(ii, jj) = temp(jj, ii, 1)
END DO

CALL HierarchicalGradientEvalAll_(elemType=elemType, &
                                 xij=quad%points(1:xidim, 1:nips), ans=temp, &
                                  dim1=ii, dim2=jj, dim3=kk, &
                                  domainName=domainName, &
                                  cellOrder=cellOrder, &
                                  faceOrder=faceOrder, &
                                  edgeOrder=edgeOrder, &
                                  cellOrient=cellOrient, &
                                  faceOrient=faceOrient, &
                                  edgeOrient=edgeOrient)

CALL SWAP_(a=obj%dNdXi, b=temp(1:ii, 1:jj, 1:kk), i1=2, i2=3, i3=1)
! CALL SWAP_(a=obj%dNdXi, b=temp(1:ii, 1:jj, 1:jj), i1=3, i2=1, i3=2)

IF (ALLOCATED(temp)) DEALLOCATE (temp)

END PROCEDURE HierarchicalElemShapeData1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalElemShapeData2
CALL HierarchicalElemShapeData( &
  obj=obj, quad=quad, nsd=refelem%nsd, xidim=refelem%xidimension, &
  elemType=refelem%name, refelemCoord=refelem%xij, &
  domainName=refelem%domainName, cellOrder=cellOrder, &
  faceOrder=faceOrder, edgeOrder=edgeOrder, &
  cellOrient=cellOrient, faceOrient=faceOrient, &
  edgeOrient=edgeOrient)
END PROCEDURE HierarchicalElemShapeData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalElemShapeData3
CALL HierarchicalElemShapeData( &
  obj=obj, quad=quad, refelem=refelem, cellOrder=cellOrder, &
  faceOrder=faceOrder, edgeOrder=edgeOrder, cellOrient=cellOrient, &
  faceOrient=faceOrient, edgeOrient=edgeOrient)
END PROCEDURE HierarchicalElemShapeData3

!----------------------------------------------------------------------------
!                                              HierarchicalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalFacetElemShapeData1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "HierarchicalFacetElemShapeData1()"
#endif

INTEGER(I4B) :: faceElemType, faceXidim, tFaceNodes, nrow, ncol
REAL(DFP) :: faceRefelemCoord(3, 8)

CALL HierarchicalElemShapeData(obj=obj, quad=quad, nsd=nsd, xidim=xidim, &
                               elemType=elemType, refelemCoord=refelemCoord, &
                               domainName=domainName, cellOrder=cellOrder, &
                               faceOrder=faceOrder, edgeOrder=edgeOrder, &
                               cellOrient=cellOrient, faceOrient=faceOrient, &
                               edgeOrient=edgeOrient)

CALL Refelem_GetFaceElemType(elemType=elemType, &
                             localFaceNumber=localFaceNumber, &
                             faceElemType=faceElemType, &
                             opt=1, tFaceNodes=tFaceNodes)

CALL Refelem_RefCoord_(elemType=faceElemType, refElem=domainName, &
                       ans=faceRefelemCoord, nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL AssertError1(.FALSE., myName, &
                  "This is routine is under development")
#endif

faceXidim = xidim - 1
CALL HierarchicalElemShapeData( &
  obj=facetElemsd, quad=facetQuad, nsd=nsd, xidim=faceXidim, &
  elemType=faceElemType, refelemCoord=faceRefelemCoord(1:nrow, 1:ncol), &
  domainName=domainName, cellOrder=faceOrder(:, localFaceNumber))

END PROCEDURE HierarchicalFacetElemShapeData1

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
