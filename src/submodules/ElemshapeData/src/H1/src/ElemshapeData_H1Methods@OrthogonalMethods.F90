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

SUBMODULE(ElemShapeData_H1Methods) OrthogonalMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE H1_Orthogonal1
REAL(DFP), ALLOCATABLE :: xij(:, :)
REAL(DFP), ALLOCATABLE :: dNdXi(:, :, :)
REAL(DFP), ALLOCATABLE :: N(:, :)
INTEGER(I4B) :: nsd, xidim, basisType0

basisType0 = Input(option=basisType, default=Legendre)
CALL DEALLOCATE (obj)
nsd = refelem%nsd
xidim = refelem%xiDimension
CALL GetQuadraturePoints(obj=quad, points=xij, weights=obj%ws)

CALL ALLOCATE ( &
  & obj=obj, &
  & nsd=nsd, &
  & xidim=xidim, &
  & nns=LagrangeDOF(order=order, elemType=refelem%name), &
  & nips=SIZE(quad, 2))

SELECT CASE (refelem%name)
CASE (Line)
  N = OrthogonalBasis_Line( &
    & order=order,  &
    & xij=xij, &
    & refLine=refelem%domainName,  &
    & basisType=basisType0,  &
    & alpha=alpha, beta=beta, lambda=lambda)

  dNdXi = OrthogonalBasisGradient_Line( &
    & order=order,  &
    & xij=xij, &
    & refLine=refelem%domainName,  &
    & basisType=basisType0,  &
    & alpha=alpha, beta=beta, lambda=lambda)

CASE (Triangle)
  N = OrthogonalBasis_Triangle( &
    & order=order,  &
    & xij=xij, &
    & refTriangle=refelem%domainName)

  dNdXi = OrthogonalBasisGradient_Triangle( &
    & order=order,  &
    & xij=xij, &
    & refTriangle=refelem%domainName)

CASE (Quadrangle)
  N = OrthogonalBasis_Quadrangle( &
    & p=order, &
    & q=order, &
    & xij=xij, &
    & basisType1=basisType0,  &
    & basisType2=basisType0,  &
    & alpha1=alpha,  &
    & beta1=beta,  &
    & alpha2=alpha,  &
    & beta2=beta,  &
    & lambda1=lambda,  &
    & lambda2=lambda)

  dNdXi = OrthogonalBasisGradient_Quadrangle( &
    & p=order, &
    & q=order, &
    & xij=xij, &
    & basisType1=basisType0,  &
    & basisType2=basisType0,  &
    & alpha1=alpha,  &
    & beta1=beta,  &
    & alpha2=alpha,  &
    & beta2=beta,  &
    & lambda1=lambda,  &
    & lambda2=lambda)

CASE (Tetrahedron)
  N = OrthogonalBasis_Tetrahedron( &
    & order=order, &
    & xij=xij,  &
    & refTetrahedron=refelem%domainName)

  dNdXi = OrthogonalBasisGradient_Tetrahedron( &
    & order=order, &
    & xij=xij,  &
    & refTetrahedron=refelem%domainName)

CASE (Hexahedron)
  N = OrthogonalBasis_Hexahedron( &
    & p=order, &
    & q=order, &
    & r=order, &
    & xij=xij, &
    & basisType1=basisType0,  &
    & basisType2=basisType0,  &
    & basisType3=basisType0,  &
    & alpha1=alpha,  &
    & beta1=beta,  &
    & lambda1=lambda,  &
    & alpha2=alpha,  &
    & beta2=beta,  &
    & lambda2=lambda,  &
    & alpha3=alpha,  &
    & beta3=beta,  &
    & lambda3=lambda  &
    & )

  dNdXi = OrthogonalBasisGradient_Hexahedron( &
    & p=order, &
    & q=order, &
    & r=order, &
    & xij=xij, &
    & basisType1=basisType0,  &
    & basisType2=basisType0,  &
    & basisType3=basisType0,  &
    & alpha1=alpha,  &
    & beta1=beta,  &
    & lambda1=lambda,  &
    & alpha2=alpha,  &
    & beta2=beta,  &
    & lambda2=lambda,  &
    & alpha3=alpha,  &
    & beta3=beta,  &
    & lambda3=lambda  &
    & )

CASE DEFAULT
  CALL Errormsg( &
    & msg="[NO CASE FOUND] no case found for elemType",  &
    & unitno=stderr,  &
    & routine="H1_Hierarchy1()",  &
    & file=__FILE__, &
    & line=__LINE__)
END SELECT

obj%N = TRANSPOSE(N)
CALL SWAP(a=obj%dNdXi, b=dNdXi, i1=2, i2=3, i3=1)

IF (ALLOCATED(dNdXi)) DEALLOCATE (dNdXi)
IF (ALLOCATED(N)) DEALLOCATE (N)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
END PROCEDURE H1_Orthogonal1

END SUBMODULE OrthogonalMethods
