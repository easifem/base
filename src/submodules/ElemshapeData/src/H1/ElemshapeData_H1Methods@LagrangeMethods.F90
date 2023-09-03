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

SUBMODULE(ElemShapeData_H1Methods) LagrangeMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE H1_Lagrange1
REAL(DFP), ALLOCATABLE :: pt(:, :), xij(:, :), dNdXi(:, :, :), coeff0(:, :)
INTEGER(I4B) :: nsd, xidim, ipType0, basisType0

ipType0 = Input(default=Equidistance, option=ipType)
basisType0 = Input(default=Monomial, option=basisType)

CALL DEALLOCATE (obj)
CALL Initiate(obj%refelem, refelem)
nsd = refelem%nsd
xidim = refelem%xiDimension
CALL GetQuadraturePoints(obj=quad, points=pt, weights=obj%ws)
obj%quad = quad

CALL ALLOCATE ( &
  & obj=obj, &
  & nsd=nsd, &
  & xidim=xidim, &
  & nns=LagrangeDOF(order=order, elemType=refelem%name), &
  & nips=SIZE(quad, 2))

xij = InterpolationPoint( &
  & order=order, &
  & elemType=refelem%name, &
  & ipType=ipType0, &
  & layout="VEFC",  &
  & xij=refelem%xij(1:xidim, :), &
  & alpha=alpha, beta=beta, lambda=lambda)

CALL Reallocate(coeff0, SIZE(xij, 2), SIZE(xij, 2))

IF (PRESENT(coeff)) THEN
  obj%N = TRANSPOSE(LagrangeEvalAll( &
    & order=order,  &
    & elemType=refelem%name, &
    & x=pt(1:xidim, :),  &
    & xij=xij, &
    & domainName=refelem%domainName, &
    & basisType=basisType0,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & lambda=lambda, &
    & coeff=coeff, &
    & firstCall=firstCall))

  dNdXi = LagrangeGradientEvalAll( &
    & order=order,  &
    & elemType=refelem%name, &
    & x=pt(1:xidim, :),  &
    & xij=xij, &
    & domainName=refelem%domainName, &
    & basisType=basisType0,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & lambda=lambda,  &
    & coeff=coeff,  &
    & firstCall=.FALSE.)

  CALL SWAP(  &
    & a=obj%dNdXi, &
    & b=dNdXi,  &
    & i1=2, i2=3, i3=1)

ELSE

  obj%N = TRANSPOSE(LagrangeEvalAll( &
    & order=order,  &
    & elemType=refelem%name, &
    & x=pt(1:xidim, :),  &
    & xij=xij, &
    & domainName=refelem%domainName, &
    & basisType=basisType0,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & lambda=lambda, &
    & coeff=coeff0, &
    & firstCall=.TRUE.))

  dNdXi = LagrangeGradientEvalAll( &
    & order=order,  &
    & elemType=refelem%name, &
    & x=pt(1:xidim, :),  &
    & xij=xij, &
    & domainName=refelem%domainName, &
    & basisType=basisType0,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & lambda=lambda,  &
    & coeff=coeff0,  &
    & firstCall=.FALSE.)

  CALL SWAP( &
    & a=obj%dNdXi, &
    & b=dNdXi,  &
    & i1=2, i2=3, i3=1)

END IF

IF (ALLOCATED(dNdXi)) DEALLOCATE (dNdXi)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(pt)) DEALLOCATE (pt)
IF (ALLOCATED(coeff0)) DEALLOCATE (coeff0)

END PROCEDURE H1_Lagrange1

END SUBMODULE LagrangeMethods
