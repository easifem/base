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

SUBMODULE(ElemShapeData_Lagrange) Methods
USE BaseType, ONLY: TypePolynomialOpt
USE BaseType, ONLY: TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: Elemsd_Allocate => ALLOCATE
USE ErrorHandling, ONLY: ErrorMsg
USE GlobalData, ONLY: stderr
USE InputUtility, ONLY: Input
USE LagrangePolynomialUtility, ONLY: InterpolationPoint_
USE LagrangePolynomialUtility, ONLY: LagrangeDOF
USE LagrangePolynomialUtility, ONLY: LagrangeEvalAll_
USE LagrangePolynomialUtility, ONLY: LagrangeGradientEvalAll_
USE QuadraturePoint_Method, ONLY: GetQuadratureWeights_
USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
USE ReferenceElement_Method, ONLY: Refelem_GetFaceElemType => GetFaceElemType
USE ReferenceElement_Method, ONLY: Refelem_RefCoord_ => RefCoord_
USE SwapUtility, ONLY: SWAP_
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                             ElemshapeData_InitiateLagrange
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElemShapeData1
REAL(DFP), ALLOCATABLE :: xij(:, :), coeff0(:, :), temp(:, :, :)
INTEGER(I4B) :: ipType0, basisType0, nips, nns, indx(3), ii, jj

ipType0 = Input(default=TypeQuadratureOpt%equidistance, option=ipType)
basisType0 = Input(default=TypePolynomialOpt%Monomial, option=basisType)

nips = GetTotalQuadraturePoints(obj=quad)

nns = LagrangeDOF(order=order, elemType=elemType)

#ifdef DEBUG_VER
IF (nns .EQ. 0) THEN
  CALL ErrorMsg(msg="LagrangeDOF returned zero DOF", &
                routine="LagrangeElemShapeData1()", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  STOP
END IF
#endif

CALL Elemsd_Allocate(obj=obj, nsd=nsd, xidim=xidim, nns=nns, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=obj%ws, tsize=nips)

ALLOCATE (xij(3, nns), temp(nips, nns, 3))

CALL InterpolationPoint_(order=order, elemType=elemType, ipType=ipType0, &
                         layout="VEFC", xij=refelemCoord(1:xidim, :), &
                         alpha=alpha, beta=beta, &
                         lambda=lambda, ans=xij, nrow=indx(1), ncol=indx(2))

IF (PRESENT(coeff)) THEN

  CALL LagrangeEvalAll_(order=order, &
                        elemType=elemType, &
                        x=quad%points(1:quad%txi, 1:nips), &
                        xij=xij(1:xidim, :), &
                        domainName=domainName, &
                        basisType=basisType0, &
                        alpha=alpha, beta=beta, lambda=lambda, &
                        coeff=coeff(1:nns, 1:nns), firstCall=firstCall, &
                        ans=temp(:, :, 1), nrow=indx(1), ncol=indx(2))

  DO CONCURRENT(ii=1:nns, jj=1:nips)
    obj%N(ii, jj) = temp(jj, ii, 1)
  END DO

  CALL LagrangeGradientEvalAll_(order=order, elemType=elemType, &
                                x=quad%points(1:quad%txi, 1:nips), &
                                xij=xij(1:xidim, :), &
                                domainName=domainName, &
                                basisType=basisType0, &
                                alpha=alpha, beta=beta, lambda=lambda, &
                                coeff=coeff(1:nns, 1:nns), &
                                firstCall=.FALSE., &
                                ans=temp, &
                                dim1=indx(1), dim2=indx(2), dim3=indx(3))

ELSE

  ALLOCATE (coeff0(nns, nns))

  CALL LagrangeEvalAll_(order=order, elemType=elemType, &
                        x=quad%points(1:quad%txi, 1:nips), &
                        xij=xij(1:xidim, :), &
                        domainName=domainName, &
                        basisType=basisType0, &
                        alpha=alpha, beta=beta, lambda=lambda, &
                        coeff=coeff0, firstCall=.TRUE., &
                        ans=temp(:, :, 1), nrow=indx(1), ncol=indx(2))

  obj%N(1:nns, 1:nips) = TRANSPOSE(temp(1:nips, 1:nns, 1))

  CALL LagrangeGradientEvalAll_(order=order, elemType=elemType, &
                                x=quad%points(1:quad%txi, 1:nips), &
                                xij=xij(1:xidim, :), &
                                domainName=domainName, &
                                basisType=basisType0, &
                                alpha=alpha, beta=beta, lambda=lambda, &
                                coeff=coeff0, firstCall=.FALSE., &
                                ans=temp, &
                                dim1=indx(1), dim2=indx(2), dim3=indx(3))
END IF

CALL SWAP_(a=obj%dNdXi, b=temp(1:indx(1), 1:indx(2), 1:indx(3)), i1=2, &
           i2=3, i3=1)

IF (ALLOCATED(temp)) DEALLOCATE (temp)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(coeff0)) DEALLOCATE (coeff0)

END PROCEDURE LagrangeElemShapeData1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElemShapeData2
CALL LagrangeElemShapeData(obj=obj, quad=quad, nsd=refelem%nsd, &
                           xidim=refelem%xidimension, elemType=refelem%name, &
                           refelemCoord=refelem%xij, &
                           domainName=refelem%domainName, &
                           order=order, ipType=ipType, &
                           basisType=basisType, coeff=coeff, &
                           firstCall=firstCall, &
                           alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE LagrangeElemShapeData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElemShapeData3
CALL LagrangeElemShapeData(obj=obj, quad=quad, refelem=refelem, &
                           order=order, ipType=ipType, &
                           basisType=basisType, coeff=coeff, &
                           firstCall=firstCall, alpha=alpha, &
                           beta=beta, lambda=lambda)
END PROCEDURE LagrangeElemShapeData3

!----------------------------------------------------------------------------
!                                                 LagrangeFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeFacetElemShapeData1
INTEGER(I4B) :: faceElemType, faceXidim, tFaceNodes, nrow, ncol
REAL(DFP) :: faceRefelemCoord(3, 8)

CALL LagrangeElemShapeData(obj=obj, quad=quad, nsd=nsd, xidim=xidim, &
                           elemType=elemType, refelemCoord=refelemCoord, &
                           domainName=domainName, order=order, &
                           ipType=ipType, basisType=basisType, &
                           coeff=coeff, firstCall=firstCall, &
                           alpha=alpha, beta=beta, lambda=lambda)

CALL Refelem_GetFaceElemType(elemType=elemType, localFaceNumber=localFaceNumber, &
                             faceElemType=faceElemType, &
                             opt=2, tFaceNodes=tFaceNodes)

CALL Refelem_RefCoord_(elemType=faceElemType, refElem=domainName, &
                       ans=faceRefelemCoord, nrow=nrow, ncol=ncol)

faceXidim = xidim - 1
CALL LagrangeElemShapeData(obj=facetElemsd, quad=facetQuad, &
                           nsd=nsd, xidim=faceXidim, &
                           elemType=faceElemType, &
                           refelemCoord=faceRefelemCoord(1:nrow, 1:ncol), &
                           domainName=domainName, order=order, &
                           ipType=ipType, basisType=basisType, &
                           alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE LagrangeFacetElemShapeData1

END SUBMODULE Methods
