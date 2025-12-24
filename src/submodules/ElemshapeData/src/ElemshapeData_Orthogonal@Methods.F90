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

SUBMODULE(ElemShapeData_Orthogonal) Methods
USE LagrangePolynomialUtility, ONLY: LagrangeDOF

USE ElemShapeData_Method, ONLY: Elemsd_Allocate => ALLOCATE

USE OrthogonalPolynomialUtility, ONLY: OrthogonalEvalAll_, &
                                       OrthogonalGradientEvalAll_

USE SwapUtility, ONLY: SWAP_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                         ElemshapeData_InitiateOrthogonal
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalElemShapeData1
REAL(DFP), ALLOCATABLE :: temp(:, :, :)
INTEGER(I4B) :: nips, nns, ii, jj, kk

! CALL DEALLOCATE (obj)

nips = SIZE(quad%points, 2)
! INFO:
! pt = quad%points(1:quad%txi, 1:nips)
! wt = quad%points(quad%txi + 1, 1:nips)

nns = LagrangeDOF(elemType=elemType, order=order)

CALL Elemsd_Allocate(obj=obj, nsd=nsd, xidim=xidim, nns=nns, nips=nips)

DO CONCURRENT(jj=1:nips)
  obj%ws(jj) = quad%points(1 + xidim, jj)
END DO

ALLOCATE (temp(nips, nns, 3))

CALL OrthogonalEvalAll_(elemType=elemType, xij=quad%points(1:xidim, 1:nips), &
    ans=temp(:, :, 1), nrow=ii, ncol=jj, domainName=domainName, order=order, &
                   basisType=basisType, alpha=alpha, beta=beta, lambda=lambda)

DO CONCURRENT(ii=1:nns, jj=1:nips)
  obj%N(ii, jj) = temp(jj, ii, 1)
END DO

CALL OrthogonalGradientEvalAll_(elemType=elemType, &
                                xij=quad%points(1:xidim, 1:nips), ans=temp, &
                                dim1=ii, dim2=jj, dim3=kk, &
                                domainName=domainName, order=order, &
                   basisType=basisType, alpha=alpha, beta=beta, lambda=lambda)

CALL SWAP_(a=obj%dNdXi, b=temp(1:ii, 1:jj, 1:kk), i1=2, i2=3, i3=1)
! CALL SWAP_(a=obj%dNdXi, b=temp(1:ii, 1:jj, 1:jj), i1=3, i2=1, i3=2)

DEALLOCATE (temp)

END PROCEDURE OrthogonalElemShapeData1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalElemShapeData2
CALL OrthogonalElemShapeData1(obj=obj, quad=quad, nsd=refelem%nsd, &
 xidim=refelem%xidimension, elemType=refelem%name, refelemCoord=refelem%xij, &
            domainName=refelem%domainName, order=order, basisType=basisType, &
                              alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE OrthogonalElemShapeData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalElemShapeData3
CALL OrthogonalElemShapeData2(obj=obj, quad=quad, refelem=refelem, &
                   order=order, basisType=basisType, alpha=alpha, beta=beta, &
                              lambda=lambda)
END PROCEDURE OrthogonalElemShapeData3

END SUBMODULE Methods
