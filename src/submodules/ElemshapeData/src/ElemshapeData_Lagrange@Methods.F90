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
USE InputUtility, ONLY: Input

USE ReferenceElement_Method, ONLY: Refelem_Initiate => Initiate

USE ElemShapeData_Method, ONLY: Elemsd_Allocate => ALLOCATE

USE LagrangePolynomialUtility, ONLY: LagrangeDOF, &
                                     InterpolationPoint_, &
                                     LagrangeEvalAll, &
                                     LagrangeEvalAll_, &
                                     LagrangeGradientEvalAll_

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
!                                             ElemshapeData_InitiateLagrange
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElemShapeData1
REAL(DFP), ALLOCATABLE :: xij(:, :), coeff0(:, :), temp(:, :, :)
INTEGER(I4B) :: ipType0, basisType0, nips, nns, indx(10), ii, jj

ipType0 = Input(default=TypeQuadratureOpt%equidistance, option=ipType)
basisType0 = Input(default=TypePolynomialOpt%Monomial, option=basisType)

! CALL DEALLOCATE (obj)

nips = GetTotalQuadraturePoints(obj=quad)
! pt = quad%points(1:quad%txi, 1:nips)
! wt = quad%points(quad%txi + 1, 1:nips)

nns = LagrangeDOF(order=order, elemType=elemType)

#ifdef DEBUG_VER
IF (nns .EQ. 0) THEN
  CALL Display("Error: LagrangeDOF returned zero DOF")
  STOP
END IF
#endif

CALL Elemsd_Allocate(obj=obj, nsd=nsd, xidim=xidim, nns=nns, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=obj%ws, tsize=nips)

ALLOCATE (xij(3, nns), temp(nips, nns, 3))

CALL InterpolationPoint_(order=order, elemType=elemType, ipType=ipType0, &
        layout="VEFC", xij=refelemCoord(1:xidim, :), alpha=alpha, beta=beta, &
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

  ! obj%N = TRANSPOSE(LagrangeEvalAll(order=order, elemType=elemType, &
  CALL LagrangeEvalAll_(order=order, elemType=elemType, &
                        x=quad%points(1:quad%txi, 1:nips), &
                        xij=xij(1:xidim, :), &
                        domainName=domainName, &
                        basisType=basisType0, &
                        alpha=alpha, beta=beta, lambda=lambda, &
                        coeff=coeff0, firstCall=.TRUE., &
                        ans=temp(:, :, 1), nrow=indx(1), ncol=indx(2))

  obj%N(1:nns, 1:nips) = TRANSPOSE(temp(1:nips, 1:nns, 1))

  ! dNdXi = LagrangeGradientEvalAll(order=order, elemType=elemType, &
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
CALL LagrangeElemShapeData1(obj=obj, quad=quad, nsd=refelem%nsd, &
                           xidim=refelem%xidimension, elemType=refelem%name, &
       refelemCoord=refelem%xij, domainName=refelem%domainName, order=order, &
       ipType=ipType, basisType=basisType, coeff=coeff, firstCall=firstCall, &
                            alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE LagrangeElemShapeData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElemShapeData3
CALL LagrangeElemShapeData2(obj=obj, quad=quad, refelem=refelem, &
               order=order, ipType=ipType, basisType=basisType, coeff=coeff, &
                   firstCall=firstCall, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE LagrangeElemShapeData3

END SUBMODULE Methods
