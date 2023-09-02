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

MODULE H1Lagrange_Triangle_Methods
USE BaseMethod
IMPLICIT NONE
PRIVATE
PUBLIC :: Triangle_H1_Lagrange

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns the shape functions given by Lagrange polynomials

PURE SUBROUTINE Triangle_H1_Lagrange(obj, quad, refelem, &
  & continuityType, interpolType)
  CLASS(ElemshapeData_), INTENT(INOUT) :: obj
  CLASS(QuadraturePoint_), INTENT(IN) :: quad
  CLASS(ReferenceElement_), INTENT(IN) :: refelem
  CLASS(H1_), INTENT(IN) :: continuityType
  CLASS(LagrangeInterpolation_), INTENT(IN) :: interpolType

  INTEGER(I4B) :: nips
  REAL(DFP), ALLOCATABLE :: XiEta(:, :)
  !!
  CALL Initiate(obj%refelem, refelem)
  CALL getQuadraturePoints(obj=quad, points=XiEta, weights=obj%ws)
  obj%quad = quad
  nips = SIZE(obj%ws)
  !!
  SELECT CASE (refelem%order)
  !!
  !! order=1
  !!
  CASE (1)
    BLOCK
      INTEGER(I4B) :: ips
      !!
      CALL ALLOCATE (obj=obj, nsd=refelem%nsd, &
        & xidim=refelem%xidimension, nns=3, nips=nips)
      !!
      obj%N(1, :) = 1.0_DFP - xieta(1, :) - xieta(2, :)
      obj%N(2, :) = xieta(1, :)
      obj%N(3, :) = xieta(2, :)
      !!
      obj%dNdXi(:, :, 1) = RESHAPE([-1.0, 1.0, 0.0, -1.0, 0.0, 1.0], &
        & [3, 2])
      DO ips = 2, nips
        obj%dNdXi(:, :, ips) = obj%dNdXi(:, :, 1)
      END DO
    END BLOCK
  !!
  !! order=2
  !!
  CASE (2)
    BLOCK
      REAL(DFP) :: l1(nips), l2(nips), l3(nips), dldxi(3, 2), &
        & dndl(6, 3)
      !!
      INTEGER(I4B) :: ips
      !!
      CALL ALLOCATE (obj=obj, nsd=refelem%nsd, &
        & xidim=refelem%xidimension, nns=6, nips=nips)
      !!
      dldxi = RESHAPE([-1.0, 1.0, 0.0, -1.0, 0.0, 1.0], [3, 2])
      !!
      l1(:) = 1.0_DFP - xieta(1, :) - xieta(2, :)
      l2(:) = xieta(1, :)
      l3(:) = xieta(2, :)
      !!
      obj%N(1, :) = l1 * (2.0 * l1 - 1.0)
      obj%N(2, :) = l2 * (2.0 * l2 - 1.0)
      obj%N(3, :) = l3 * (2.0 * l3 - 1.0)
      obj%N(4, :) = 4 * l1 * l2
      obj%N(5, :) = 4 * l2 * l3
      obj%N(6, :) = 4 * l3 * l1
      !!
      DO ips = 1, nips
        dndl = RESHAPE( &
          & [4.0_DFP * l1(ips) - 1.0_DFP, 0.0_DFP, 0.0_DFP, &
          & 4.0 * l2(ips), 0.0_DFP, 4.0 * l3(ips),  &
          & 0.0_DFP, 4.0_DFP * l2(ips) - 1.0_DFP, 0.0_DFP, &
          & 4.0 * l1(ips), 4.0 * l3(ips), 0.0_DFP, &
          & 0.0_DFP, 0.0_DFP, 4.0_DFP * l3(ips) - 1.0_DFP, &
          & 0.0_DFP, 4.0 * l2(ips), 4.0 * l1(ips) &
          & ], [6, 3] &
          & )
        obj%dNdXi(:, :, ips) = MATMUL(dndl, dldxi)
      END DO
    END BLOCK
  !!
  !! order=3
  !!
  CASE (3)
    BLOCK
      REAL(DFP) :: l1(nips), l2(nips), l3(nips), dldxi(3, 2), &
        & dndl(10, 3), a1, a2
      INTEGER(I4B) :: ips
      !!
      CALL ALLOCATE (obj=obj, nsd=refelem%nsd, &
        & xidim=refelem%xidimension, nns=10, nips=nips)
      !!
      a1 = 1.0 / 3.0; a2 = 2.0 / 3.0
      dldxi = RESHAPE([-1.0, 1.0, 0.0, -1.0, 0.0, 1.0], [3, 2])
      l1(:) = 1.0_DFP - xieta(1, :) - xieta(2, :)
      l2(:) = xieta(1, :)
      l3(:) = xieta(2, :)
      !!
      obj%N(1, :) = &
        &   (l1 - a2) &
        & * (l1 - a1) &
        & * (l1 - 0.0_DFP) &
        & / (1.0_DFP - a2) &
        & / (1.0_DFP - a1) &
        & / (1.0_DFP - 0.0_DFP)
      !!
      obj%N(2, :) = &
        &   (l2 - a2) &
        & * (l2 - a1) &
        & * (l2 - 0.0_DFP) &
        & / (1.0_DFP - a2) &
        & / (1.0_DFP - a1) &
        & / (1.0_DFP - 0.0_DFP)
      !!
      obj%N(3, :) = &
        &   (l3 - a2) &
        & * (l3 - a1) &
        & * (l3 - 0.0_DFP) &
        & / (1.0_DFP - a2) &
        & / (1.0_DFP - a1) &
        & / (1.0_DFP - 0.0_DFP)
      !!
      obj%N(4, :) = l1 * l2 * (l1 - a1)
      obj%N(5, :) = l1 * l2 * (l2 - a1)
      obj%N(6, :) = l2 * l3 * (l2 - a1)
      obj%N(7, :) = l2 * l3 * (l3 - a1)
      obj%N(8, :) = l3 * l1 * (l3 - a1)
      obj%N(9, :) = l3 * l1 * (l1 - a1)
      obj%N(10, :) = l1 * l2 * l3
      !!
      DO ips = 1, nips
        dndl = RESHAPE( &
          & [ &
          & 9.0_DFP * (l1**2) - 7.0_DFP * l1 + 2.0_DFP, 0.0_DFP, 0.0_DFP, &
          & l2 * (2.0_DFP * l1 - a1), (l2**2) - l2 / 3.0_DFP, 0.0_DFP, &
          & 0.0_DFP, l3**2 - l3 / 3.0_DFP, l3 * (2.0_DFP * l1 - a1), &
          & l2 * l3, &
          & 0.0_DFP, 9.0_DFP * (l2**2) - 7.0_DFP * l2 + 2.0_DFP, 0.0_DFP, &
          & (l1**2) - l1 / 3.0_DFP, l1 * (2.0_DFP * l2 - a1), &
          & l3 * (2.0_DFP * l2 - a1), &
          & (l3**2) - l3 / 3.0_DFP, 0.0_DFP, 0.0_DFP, &
          & l1 * l3, &
          & 0.0_DFP, 0.0_DFP, 9.0_DFP * (l3**2) - 7.0_DFP * l3 + 1.0_DFP,&
          & 0.0_DFP, 0.0_DFP, l2**2 - l2 / 3.0_DFP, &
          & l2 * (2.0_DFP * l3 - a1), l1 * (2.0_DFP * l3 - a1), &
          & l1**2 - l1 / 3.0_DFP, &
          & l1 * l2 &
          & ], &
          & [10, 3] &
        & )
        obj%dNdXi(:, :, ips) = MATMUL(dndl, dldxi)
      END DO
    END BLOCK
  END SELECT

END SUBROUTINE Triangle_H1_Lagrange

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE H1Lagrange_Triangle_Methods
