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

SUBMODULE(ElemShapeData_H1Methods) HierarchyMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE H1_Hierarchy1
REAL(DFP), ALLOCATABLE :: xij(:, :)
REAL(DFP), ALLOCATABLE :: dNdXi(:, :, :)
REAL(DFP), ALLOCATABLE :: N(:, :)
INTEGER(I4B) :: nsd, xidim

CALL DEALLOCATE (obj)
CALL Initiate(obj%refelem, refelem)
nsd = refelem%nsd
xidim = refelem%xiDimension
CALL GetQuadraturePoints(obj=quad, points=xij, weights=obj%ws)
obj%quad = quad

CALL ALLOCATE ( &
  & obj=obj, &
  & nsd=nsd, &
  & xidim=xidim, &
  & nns=LagrangeDOF(order=order, elemType=refelem%name), &
  & nips=SIZE(quad, 2))

SELECT CASE (refelem%name)
CASE (Line)
  N = HeirarchicalBasis_Line( &
    & order=order,  &
    & xij=xij, &
    & refLine=refelem%domainName)

  dNdXi = HeirarchicalGradientBasis_Line( &
    & order=order,  &
    & xij=xij, &
    & refLine=refelem%domainName)

CASE (Triangle)
  N = HeirarchicalBasis_Triangle( &
    & order=order,  &
    & pe1=order, &
    & pe2=order, &
    & pe3=order, &
    & xij=xij, &
    & refTriangle=refelem%domainName)

  dNdXi = HeirarchicalBasisGradient_Triangle( &
    & order=order,  &
    & pe1=order, &
    & pe2=order, &
    & pe3=order, &
    & xij=xij, &
    & refTriangle=refelem%domainName)

CASE (Quadrangle)
  N = HeirarchicalBasis_Quadrangle( &
    & p=order, &
    & q=order, &
    & xij=xij)

  dNdXi = HeirarchicalBasisGradient_Quadrangle( &
    & p=order, &
    & q=order, &
    & xij=xij)

CASE (Tetrahedron)
  N = HeirarchicalBasis_Tetrahedron( &
    & order=order, &
    & xij=xij,  &
    & refTetrahedron=refelem%domainName)

  dNdXi = HeirarchicalBasisGradient_Tetrahedron( &
    & order=order, &
    & xij=xij,  &
    & refTetrahedron=refelem%domainName)

CASE (Hexahedron)
  N = HeirarchicalBasis_Hexahedron( &
    & p=order, &
    & q=order, &
    & r=order, &
    & xij=xij)

  dNdXi = HeirarchicalBasisGradient_Hexahedron( &
    & p=order, &
    & q=order, &
    & r=order, &
    & xij=xij)

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
END PROCEDURE H1_Hierarchy1

END SUBMODULE HierarchyMethods
