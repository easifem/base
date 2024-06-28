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
                                     LagrangeGradientEvalAll

USE QuadraturePoint_Method, ONLY: GetQuadraturePoints, &
                                  QuadraturePoint_Size => Size

USE BaseType, ONLY: TypeQuadratureOpt, &
                    TypePolynomialOpt

USE SwapUtility, ONLY: SWAP

USE Display_Method, ONLY: Display

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                             ElemshapeData_InitiateLagrange
!----------------------------------------------------------------------------

MODULE PROCEDURE ElemshapeData_InitiateLagrange1
REAL(DFP), ALLOCATABLE :: xij(:, :), dNdXi(:, :, :), coeff0(:, :)
INTEGER(I4B) :: ipType0, basisType0, tsize, nns, nrow, ncol

ipType0 = Input(default=TypeQuadratureOpt%equidistance, option=ipType)
basisType0 = Input(default=TypePolynomialOpt%Monomial, option=basisType)

! CALL DEALLOCATE (obj)

tsize = SIZE(quad%points, 2)
! pt = quad%points(1:obj%txi, 1:tsize)
! wt = quad%points(obj%txi + 1, 1:tsize)

nns = LagrangeDOF(order=order, elemType=elemType)

CALL Elemsd_Allocate(obj=obj, nsd=nsd, xidim=xidim, nns=nns, nips=tsize)

obj%ws = quad%points(quad%txi + 1, 1:tsize)

ncol = LagrangeDOF(order=order, elemType=elemType)
ALLOCATE (xij(3, ncol))

CALL InterpolationPoint_(order=order, elemType=elemType, ipType=ipType0, &
        layout="VEFC", xij=refelemCoord(1:xidim, :), alpha=alpha, beta=beta, &
                         lambda=lambda, ans=xij, nrow=nrow, ncol=ncol)

IF (PRESENT(coeff)) THEN

  obj%N = TRANSPOSE(LagrangeEvalAll(order=order, elemType=elemType, &
                                    x=quad%points(1:quad%txi, 1:tsize), &
                                    xij=xij(1:xidim, :), &
                                    domainName=refelemDomain, &
                                    basisType=basisType0, &
                                    alpha=alpha, beta=beta, lambda=lambda, &
                                    coeff=coeff, firstCall=firstCall))

  dNdXi = LagrangeGradientEvalAll(order=order, elemType=elemType, &
                                  x=quad%points(1:quad%txi, 1:tsize), &
                                  xij=xij(1:xidim, :), &
                                  domainName=refelemDomain, &
                                  basisType=basisType0, &
                                  alpha=alpha, beta=beta, lambda=lambda, &
                                  coeff=coeff, firstCall=.FALSE.)

ELSE

  ALLOCATE (coeff0(SIZE(xij, 2), SIZE(xij, 2)))

  obj%N = TRANSPOSE(LagrangeEvalAll(order=order, elemType=elemType, &
                                    x=quad%points(1:quad%txi, 1:tsize), &
                                    xij=xij(1:xidim, :), &
                                    domainName=refelemDomain, &
                                    basisType=basisType0, &
                                    alpha=alpha, beta=beta, lambda=lambda, &
                                    coeff=coeff0, firstCall=.TRUE.))

  dNdXi = LagrangeGradientEvalAll(order=order, elemType=elemType, &
                                  x=quad%points(1:quad%txi, 1:tsize), &
                                  xij=xij(1:xidim, :), &
                                  domainName=refelemDomain, &
                                  basisType=basisType0, &
                                  alpha=alpha, beta=beta, lambda=lambda, &
                                  coeff=coeff0, firstCall=.FALSE.)
END IF

CALL SWAP(a=obj%dNdXi, b=dNdXi, i1=2, i2=3, i3=1)

IF (ALLOCATED(dNdXi)) DEALLOCATE (dNdXi)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(coeff0)) DEALLOCATE (coeff0)

END PROCEDURE ElemshapeData_InitiateLagrange1

END SUBMODULE Methods
