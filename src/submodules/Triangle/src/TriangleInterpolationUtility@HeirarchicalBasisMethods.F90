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

SUBMODULE(TriangleInterpolationUtility) HeirarchicalBasisMethods
USE LobattoPolynomialUtility, ONLY: LobattoKernelEvalAll_, &
                                    LobattoKernelGradientEvalAll_
USE MappingUtility, ONLY: BarycentricCoordTriangle_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                GetHierarchicalDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetHierarchicalDOF_Triangle
ans = 0

SELECT CASE (opt)

CASE ("v", "V")
  ans = 3

CASE ("e", "E")
  ans = pe1 + pe2 + pe3 - 3

CASE ("c", "C")
  ans = (order - 1) * (order - 2) / 2_I4B

CASE DEFAULT
  ans = pe1 + pe2 + pe3 + (order - 1) * (order - 2) / 2_I4B

END SELECT
END PROCEDURE GetHierarchicalDOF_Triangle

!----------------------------------------------------------------------------
!                                          BarycentricVertexBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on reference Triangle

PURE SUBROUTINE BarycentricVertexBasis_Triangle(lambda, ans, nrow, &
                                                ncol)
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! point of evaluation in terms of barycentrix coords
  !! number of rows = 3 corresponding to three coordinates
  !! number of columns = number of points
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! REAL(DFP) :: ans(SIZE(lambda, 2), 3)
  !! ans(:,v1) basis function of vertex v1 at all points
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! nrow = size(lambda, 2)
  !! ncol = 3

  !! internal variables
  INTEGER(I4B) :: ii, jj

  nrow = SIZE(lambda, 2)
  ncol = SIZE(lambda, 1)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    ans(ii, jj) = lambda(jj, ii)
  END DO

END SUBROUTINE BarycentricVertexBasis_Triangle

!----------------------------------------------------------------------------
!                                                      VertexBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Triangle
INTEGER(I4B) :: nrow, ncol
REAL(DFP) :: lambda(3, SIZE(xij, 2))
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricVertexBasis_Triangle(lambda=lambda, ans=ans, nrow=nrow, &
                                     ncol=ncol)
END PROCEDURE VertexBasis_Triangle

!----------------------------------------------------------------------------
!                                              BarycentricEdgeBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on edge of triangle
!
!# Introduction
!
! Evaluate basis functions on edges of triangle
! pe1, pe2, pe3 should be greater than or equal to 2

PURE SUBROUTINE BarycentricEdgeBasis_Triangle(pe1, pe2, pe3, lambda, ans, &
                                              nrow, ncol)
  INTEGER(I4B), INTENT(IN) :: pe1
  !! order on  edge (e1)
  INTEGER(I4B), INTENT(IN) :: pe2
  !! order on edge (e2)
  INTEGER(I4B), INTENT(IN) :: pe3
  !! order on edge (e3)
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! point of evaluation in terms of barycentric coordinates
  !! Number of rows in lambda is equal to three corresponding to
  !! three coordinates
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! REAL(DFP) :: ans(SIZE(lambda, 2), pe1 + pe2 + pe3 - 3)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! nrow=SIZE(lambda, 2)
  !! ncol=pe1 + pe2 + pe3 - 3

  INTEGER(I4B), PARAMETER :: orient = 1
  REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
  ! REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2))
  REAL(DFP), ALLOCATABLE :: phi(:, :)

  INTEGER(I4B) :: maxP, ii

  nrow = SIZE(lambda, 2)
  ! ncol = pe1 + pe2 + pe3 - 3
  maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2)

  ALLOCATE (phi(1:3 * nrow, 0:maxP))

  DO CONCURRENT(ii=1:nrow)
    ! edge 1 -> 2
    d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
    ! edge 2 -> 3
    d_lambda(ii + nrow) = lambda(3, ii) - lambda(2, ii)
    ! edge 3 -> 1
    d_lambda(ii + 2 * nrow) = lambda(1, ii) - lambda(3, ii)
  END DO

 CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=nrow, ncol=ncol)

  CALL BarycentricEdgeBasis_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
                      lambda=lambda, phi=phi, ans=ans, nrow=nrow, ncol=ncol, &
                   edgeOrient1=orient, edgeOrient2=orient, edgeOrient3=orient)

END SUBROUTINE BarycentricEdgeBasis_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 30 Oct 2022
! summary: Evaluate the edge basis on triangle using barycentric coordinate
! (internal only)

MODULE PURE SUBROUTINE BarycentricEdgeBasis_Triangle2(pe1, pe2, pe3, &
          lambda, phi, ans, nrow, ncol, edgeOrient1, edgeOrient2, edgeOrient3)
  INTEGER(I4B), INTENT(IN) :: pe1
  !! order on  edge (e1)
  INTEGER(I4B), INTENT(IN) :: pe2
  !! order on edge (e2)
  INTEGER(I4B), INTENT(IN) :: pe3
  !! order on edge (e3)
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! point of evaluation in terms of barycentric coordinates
  !! size(lambda,1) = 3
  !! size(lambda,2) = number of points of evaluation
  REAL(DFP), INTENT(IN) :: phi(1:, 0:)
  !! lobatto kernel values
  !! size(phi1, 1) = 3*number of points
  !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
  !! (lambda2-lambda1),
  !! (lambda3-lambda2),
  !! (lambda1-lambda3)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! ans(SIZE(lambda, 2), pe1 + pe2 + pe3 - 3)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! nrow = size(lambda, 2)
  !! ncol = pe1 + pe2 + pe3 - 3
  INTEGER(I4B), INTENT(IN) :: edgeOrient1, edgeOrient2, edgeOrient3

  !! Internal variables
  INTEGER(I4B) :: a, ii, jj
  REAL(DFP) :: temp, areal, o1, o2, o3

  nrow = SIZE(lambda, 2)
  ! tPoints = SIZE(lambda, 2)
  ncol = pe1 + pe2 + pe3 - 3

  o1 = REAL(edgeOrient1, kind=DFP)
  o2 = REAL(edgeOrient2, kind=DFP)
  o3 = REAL(edgeOrient3, kind=DFP)

  ! ans = 0.0_DFP
  a = 0

  ! edge(1) = 1 -> 2
  DO ii = 1, pe1 - 1
    areal = o1**(ii + 1)
    ! ans(1:nrow, a + ii) = areal * temp * phi(1:nrow, ii - 1)

    DO jj = 1, nrow
      temp = lambda(1, jj) * lambda(2, jj) * areal
      ans(jj, a + ii) = temp * phi(jj, ii - 1)
    END DO
  END DO

  ! edge(2) = 2 -> 3
  a = pe1 - 1

  DO ii = 1, pe2 - 1
    areal = o2**(ii + 1)

    DO jj = 1, nrow
      temp = lambda(2, jj) * lambda(3, jj) * areal
      ans(jj, a + ii) = temp * phi(jj + nrow, ii - 1)
    END DO

  END DO

  ! edge(3) = 3 -> 1
  a = pe1 - 1 + pe2 - 1

  DO ii = 1, pe3 - 1
    areal = o3**(ii + 1)

    DO jj = 1, nrow
      temp = areal * lambda(3, jj) * lambda(1, jj)
      ans(jj, a + ii) = temp * phi(jj + 2 * nrow, ii - 1)
    END DO
  END DO
END SUBROUTINE BarycentricEdgeBasis_Triangle2

!----------------------------------------------------------------------------
!                                                         EdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Triangle
REAL(DFP) :: lambda(3, SIZE(xij, 2))
INTEGER(I4B) :: nrow, ncol

CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricEdgeBasis_Triangle(lambda=lambda, ans=ans, pe1=pe1, &
                                   pe2=pe2, pe3=pe3, nrow=nrow, ncol=ncol)
END PROCEDURE EdgeBasis_Triangle

!----------------------------------------------------------------------------
!                                          BarycentricCellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the Cell basis functions on reference Triangle

PURE SUBROUTINE BarycentricCellBasis_Triangle(order, lambda, ans, nrow, ncol)
  INTEGER(I4B), INTENT(IN) :: order
    !! order in this cell, it should be greater than 2
  REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentrix coords
    !! number of rows = 3 corresponding to three coordinates
    !! number of columns = number of points
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  ! ans(SIZE(lambda, 2), INT((order - 1) * (order - 2) / 2))
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! nrow =  SIZE(lambda, 2)
  !! ncol = INT((order - 1) * (order - 2) / 2)

  !! internal variables
  REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
  REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:order - 2)
  INTEGER(I4B) :: maxP, ii
  INTEGER(I4B), PARAMETER :: faceOrient(2) = [0, 1]

  nrow = SIZE(lambda, 2)
  maxP = order - 2

  DO CONCURRENT(ii=1:nrow)
    ! Cell 1 -> 2
    d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
    ! Cell 2 -> 3
    d_lambda(ii + nrow) = lambda(3, ii) - lambda(2, ii)
    ! Cell 3 -> 1
    d_lambda(ii + 2 * nrow) = lambda(1, ii) - lambda(3, ii)
  END DO

  CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=ans, nrow=nrow, &
                             ncol=ncol)

  CALL BarycentricCellBasis_Triangle2(order=order, lambda=lambda, phi=phi, &
                         ans=ans, nrow=nrow, ncol=ncol, faceOrient=faceOrient)

END SUBROUTINE BarycentricCellBasis_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE MakeFaceCase_Triangle(faceOrient, nrow, id, indx)
  INTEGER(I4B), INTENT(IN) :: faceOrient(2)
  INTEGER(I4B), INTENT(IN) :: nrow
  INTEGER(I4B), INTENT(OUT) :: id
  INTEGER(I4B), INTENT(OUT) :: indx(2, 2)
  !! main program

  IF (faceOrient(2) .LT. 0) THEN
    SELECT CASE (faceOrient(1))
    CASE (1)
      id = 2
      indx(1, 1) = 2
      indx(1, 2) = 1

    CASE (2)
      id = 3
      indx(1, 1) = 3
      indx(1, 2) = 2

    CASE DEFAULT
      id = 1
      indx(1, 1) = 1
      indx(1, 2) = 3

    END SELECT

  ELSE

    SELECT CASE (faceOrient(1))
    CASE (1)
      id = 5
      indx(1, 1) = 2
      indx(1, 2) = 3

    CASE (2)
      id = 6
      indx(1, 1) = 1
      indx(1, 2) = 2

    CASE default
      id = 4
      indx(1, 1) = 3
      indx(1, 2) = 1

    END SELECT

  END IF

  indx(1, 1) = nrow * (indx(1, 1) - 1) + 1
  indx(2, 1) = indx(1, 1) + nrow - 1

  indx(1, 2) = nrow * (indx(1, 2) - 1) + 1
  indx(2, 2) = indx(1, 2) + nrow - 1

END SUBROUTINE MakeFaceCase_Triangle

!----------------------------------------------------------------------------
!                                              BarycentricCellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of reference triangle (internal only)

PURE SUBROUTINE BarycentricCellBasis_Triangle2(order, lambda, phi, ans, &
                                               nrow, ncol, faceOrient)
  INTEGER(I4B), INTENT(IN) :: order
  !! order in the cell of triangle, it should be greater than 2
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! point of evaluation in terms of barcentric coordinates
  REAL(DFP), INTENT(IN) :: phi(1:, 0:)
  !! lobatto kernel values
  !! size(phi1, 1) = 3*number of points
  !! (lambda2-lambda1),
  !! (lambda3-lambda2),
  !! (lambda1-lambda3)
  !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  ! REAL(DFP) :: ans(SIZE(lambda, 2), INT((order - 1) * (order - 2) / 2))
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! nrow = size(lambda, 2)
  !! ncol = INT((order - 1) * (order - 2) / 2)
  INTEGER(I4B), INTENT(IN) :: faceOrient(2)

  INTEGER(I4B) :: k1, k2, cnt, id, indx(2, 2), aint, bint, ii
  REAL(DFP) :: temp, areal, breal, o1

  nrow = SIZE(lambda, 2)
  ncol = INT((order - 1) * (order - 2) / 2)

  cnt = 0

  CALL MakeFaceCase_Triangle(faceOrient=faceOrient, nrow=nrow, id=id, &
                             indx=indx)

  aint = indx(1, 1) - 1
  bint = indx(1, 2) - 1

  o1 = REAL(faceOrient(2), kind=DFP)

  DO k1 = 1, order - 2
    areal = o1**(k1 + 1)

    DO k2 = 1, order - 1 - k1
      breal = o1**(k2 + 1)
      breal = breal * areal

      cnt = cnt + 1

      DO ii = 1, nrow

        temp = lambda(1, ii) * lambda(2, ii) * lambda(3, ii) * breal

        ans(ii, cnt) = temp * phi(aint + ii, k1 - 1) * phi(bint + ii, k2 - 1)
      END DO

    END DO
  END DO

END SUBROUTINE BarycentricCellBasis_Triangle2

!----------------------------------------------------------------------------
!                                              CellBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Triangle
REAL(DFP) :: lambda(3, SIZE(xij, 2))
INTEGER(I4B) :: nrow, ncol
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricCellBasis_Triangle(lambda=lambda, ans=ans, order=order, &
                                   nrow=nrow, ncol=ncol)
END PROCEDURE CellBasis_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PURE SUBROUTINE BarycentricHeirarchicalBasis_Triangle(order, &
               pe1, pe2, pe3, lambda, refTriangle, edgeOrient1, edgeOrient2, &
                                     edgeOrient3, faceOrient, ans, nrow, ncol)
  INTEGER(I4B), INTENT(IN) :: order
  !! order in the cell of triangle, it should be greater than 2
  INTEGER(I4B), INTENT(IN) :: pe1
  !! order of interpolation on edge e1
  INTEGER(I4B), INTENT(IN) :: pe2
  !! order of interpolation on edge e2
  INTEGER(I4B), INTENT(IN) :: pe3
  !! order of interpolation on edge e3
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! Barycenteric coordinates
  !! number of rows = 3
  !! number of cols = number of points
  CHARACTER(*), INTENT(IN) :: refTriangle
  !! reference triangle, "BIUNIT", "UNIT"
  INTEGER(I4B), INTENT(IN) :: edgeOrient1, edgeOrient2, edgeOrient3
  !! edge orientation 1 or -1
  INTEGER(I4B), INTENT(IN) :: faceOrient(:)
  !! face orientation
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !!
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! nrow = SIZE(lambda, 2)
  !! ncol = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)

  !! Internal variables
  INTEGER(I4B) :: ii, maxP, indx(3)
  REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
  REAL(DFP), ALLOCATABLE :: phi(:, :)
  LOGICAL(LGT) :: isok

  nrow = SIZE(lambda, 2)
  ! ncol = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
  ncol = 0

  maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2)

  ALLOCATE (phi(1:3 * nrow, 0:maxP))

  DO CONCURRENT(ii=1:nrow)
    ! edge 1 -> 2
    d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
    ! edge 2 -> 3
    d_lambda(ii + nrow) = lambda(3, ii) - lambda(2, ii)
    ! edge 3 -> 1
    d_lambda(ii + 2 * nrow) = lambda(1, ii) - lambda(3, ii)
  END DO

  CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=indx(1), &
                             ncol=indx(2))

  !! Vertex basis function
  CALL BarycentricVertexBasis_Triangle(lambda=lambda, ans=ans(:, 1:3), &
                                       nrow=indx(1), ncol=indx(2))

  !! Edge basis function
  ncol = ncol + indx(2)

  isok = ANY([pe1, pe2, pe3] .GE. 2_I4B)
  IF (isok) THEN
    CALL BarycentricEdgeBasis_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
                lambda=lambda, phi=phi, ans=ans(:, ncol + 1:), nrow=indx(1), &
             ncol=indx(2), edgeOrient1=edgeOrient1, edgeOrient2=edgeOrient2, &
                                        edgeOrient3=edgeOrient3)

    ncol = ncol + indx(2)
  END IF

  !! Cell basis function
  isok = order .GT. 2_I4B
  IF (isok) THEN
    CALL BarycentricCellBasis_Triangle2(order=order, lambda=lambda, phi=phi, &
     ans=ans(:, ncol + 1:), nrow=indx(1), ncol=indx(2), faceOrient=faceOrient)
    ncol = ncol + indx(2)
  END IF

  DEALLOCATE (phi)

END SUBROUTINE BarycentricHeirarchicalBasis_Triangle

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle1
INTEGER(I4B) :: nrow, ncol
CALL HeirarchicalBasis_Triangle1_(order=order, pe1=pe1, pe2=pe2, pe3=pe3, &
              xij=xij, refTriangle=refTriangle, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle1

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle1_
INTEGER(I4B), PARAMETER :: orient = 1, faceOrient(2) = [0, 1]
CALL HeirarchicalBasis_Triangle3_(order=order, pe1=pe1, pe2=pe2, pe3=pe3, &
                       xij=xij, refTriangle=refTriangle, edgeOrient1=orient, &
              edgeOrient2=orient, edgeOrient3=orient, faceOrient=faceOrient, &
                                  ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle1_

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle2
INTEGER(I4B) :: nrow, ncol
CALL HeirarchicalBasis_Triangle2_(order=order, xij=xij, &
                       refTriangle=refTriangle, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle2

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle2_
INTEGER(I4B), PARAMETER :: orient = 1, faceOrient(2) = [0, 1]
CALL HeirarchicalBasis_Triangle3_(order=order, pe1=order, pe2=order, pe3=order, &
                       xij=xij, refTriangle=refTriangle, edgeOrient1=orient, &
              edgeOrient2=orient, edgeOrient3=orient, faceOrient=faceOrient, &
                                  ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle2_

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle3_
REAL(DFP) :: lambda(3, SIZE(xij, 2))
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricHeirarchicalBasis_Triangle(order=order, pe1=pe1, pe2=pe2, &
   pe3=pe3, lambda=lambda, refTriangle=refTriangle, edgeOrient1=edgeOrient1, &
    edgeOrient2=edgeOrient2, edgeOrient3=edgeOrient3, faceOrient=faceOrient, &
                                           ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle3_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2024-04-21
! summary: Evaluate the gradient of the edge basis on triangle
! using barycentric coordinate

PURE SUBROUTINE BarycentricVertexBasisGradient_Triangle(lambda, ans, &
                                                        dim1, dim2, dim3)
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! point of evaluation in terms of barycentric coordinates
  !! size(lambda,1) = 3
  !! size(lambda,2) = number of points of evaluation
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  !! ans(SIZE(lambda, 2), 3, 3)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  !! dim1 = SIZE(lambda, 2)
  !! dim2 = 3
  !! dim3 = 3

  INTEGER(I4B) :: ii

  dim1 = SIZE(lambda, 2)
  dim2 = 3
  dim3 = 3

  ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP
  DO CONCURRENT(ii=1:dim2)
    ans(1:dim1, ii, ii) = 1.0_DFP
  END DO
END SUBROUTINE BarycentricVertexBasisGradient_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-04-21
! summary: Evaluate the gradient of the edge basis on triangle
! using barycentric coordinate

! PURE SUBROUTINE BarycentricEdgeBasisGradient_Triangle(pe1, pe2, pe3, lambda, &
!                                                       ans, dim1, dim2, dim3)
!   INTEGER(I4B), INTENT(IN) :: pe1
!   !! order on  edge (e1)
!   INTEGER(I4B), INTENT(IN) :: pe2
!   !! order on edge (e2)
!   INTEGER(I4B), INTENT(IN) :: pe3
!   !! order on edge (e3)
!   REAL(DFP), INTENT(IN) :: lambda(:, :)
!   !! point of evaluation in terms of barycentric coordinates
!   !! size(lambda,1) = 3
!   !! size(lambda,2) = number of points of evaluation
!   REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
!   !! REAL(DFP) :: ans(SIZE(lambda, 2), pe1 + pe2 + pe3 - 3, 3)
!   INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
!   !! dim1=SIZE(lambda, 2)
!   !! dim2=pe1 + pe2 + pe3 - 3
!   !! dim3=3
!
!   REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
!   REAL(DFP), ALLOCATABLE :: gradientPhi(:, :), phi(:, :)
!   INTEGER(I4B) :: maxP, ii
!
!   dim1 = SIZE(lambda, 2)
!   ! dim2 = pe1 + pe2 + pe3 - 3
!   ! dim3 = 3
!
!   maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2)
!
!   ALLOCATE (gradientPhi(1:3 * dim1, 0:maxP), phi(1:3 * dim1, 0:maxP))
!
!   DO CONCURRENT(ii=1:dim1)
!     ! edge 1 -> 2
!     d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
!     ! edge 2 -> 3
!     d_lambda(ii + dim1) = lambda(3, ii) - lambda(2, ii)
!     ! edge 3 -> 1
!     d_lambda(ii + 2 * dim1) = lambda(1, ii) - lambda(3, ii)
!   END DO
!
!  CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=dim1, ncol=dim2)
!
!   CALL LobattoKernelGradientEvalAll_(n=maxP, x=d_lambda, ans=gradientPhi, &
!                                      nrow=dim1, ncol=dim2)
!
!   CALL BarycentricEdgeBasisGradient_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
!                    lambda=lambda, phi=phi, gradientPhi=gradientPhi, ans=ans, &
!                                               dim1=dim1, dim2=dim2, dim3=dim3)
!
!   DEALLOCATE (gradientPhi, phi)
!
! END SUBROUTINE BarycentricEdgeBasisGradient_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-04-21
! summary: Evaluate the gradient of the edge basis on triangle
! using barycentric coordinate
!
! PURE SUBROUTINE BarycentricCellBasisGradient_Triangle(order, lambda, ans, &
!                                                       dim1, dim2, dim3)
!   INTEGER(I4B), INTENT(IN) :: order
!     !! order on  Cell (e1)
!   REAL(DFP), INTENT(IN) :: lambda(:, :)
!     !! point of evaluation in terms of barycentric coordinates
!     !! size(lambda,1) = 3
!     !! size(lambda,2) = number of points of evaluation
!   REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
!   ! REAL(DFP) :: ans(SIZE(lambda, 2), 3*order - 3, 3)
!   INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
!   !! dim1=SIZE(lambda, 2)
!   !! dim2=3*order - 3
!   !! dim3=3
!
!   !! internal variables
!   INTEGER(I4B) :: a, b, ii, maxP, tp
!   REAL(DFP), ALLOCATABLE :: phi(:, :), gradientPhi(:, :), d_lambda(:)
!
!   dim1 = SIZE(lambda, 2)
!   maxP = order - 2
!
!   a = 3 * dim1; b = maxP
!   ALLOCATE (phi(a, b), gradientPhi(a, b), d_lambda(a))
!
!   DO CONCURRENT(ii=1:dim1)
!     ! edge 1 -> 2
!     d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
!     ! edge 2 -> 3
!     d_lambda(ii + dim1) = lambda(3, ii) - lambda(2, ii)
!     ! edge 3 -> 1
!     d_lambda(ii + 2 * dim1) = lambda(1, ii) - lambda(3, ii)
!   END DO
!
!  CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=dim1, ncol=dim2)
!
!   CALL LobattoKernelGradientEvalAll_(n=maxP, x=d_lambda, ans=gradientPhi, &
!                                      nrow=dim1, ncol=dim2)
!
!   CALL BarycentricCellBasisGradient_Triangle2(order=order, lambda=lambda, &
!             phi=phi, gradientPhi=gradientPhi, ans=ans, dim1=dim1, dim2=dim2, &
!                                              dim3=dim3, faceOrient=faceOrient)
!
! END SUBROUTINE BarycentricCellBasisGradient_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu and Vikas Sharma, Ph. D.
! date:   2024-04-21
! summary: Evaluate the gradient of the edge basis on triangle
! using barycentric coordinate

PURE SUBROUTINE BarycentricEdgeBasisGradient_Triangle2(pe1, pe2, pe3, &
                            lambda, phi, gradientPhi, ans, dim1, dim2, dim3, &
                                        edgeOrient1, edgeOrient2, edgeOrient3)
  INTEGER(I4B), INTENT(IN) :: pe1
  !! order on  edge (e1)
  INTEGER(I4B), INTENT(IN) :: pe2
  !! order on edge (e2)
  INTEGER(I4B), INTENT(IN) :: pe3
  !! order on edge (e3)
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! point of evaluation in terms of barycentric coordinates
  !! size(lambda,1) = 3
  !! size(lambda,2) = number of points of evaluation
  REAL(DFP), INTENT(IN) :: phi(1:, 0:)
  !! lobatto kernel values
  !! size(phi1, 1) = 3*number of points
  !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
  !! (lambda2-lambda1)
  !! (lambda3-lambda2)
  !! (lambda1-lambda3)
  REAL(DFP), INTENT(IN) :: gradientPhi(1:, 0:)
  !! gradients of lobatto kernel functions
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  !! REAL(DFP) :: ans(SIZE(lambda, 2), pe1 + pe2 + pe3 - 3, 3)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  !! dim1=SIZE(lambda, 2)
  !! dim2=pe1 + pe2 + pe3 - 3
  !! dim3=3
  INTEGER(I4B), INTENT(IN) :: edgeOrient1, edgeOrient2, edgeOrient3
  !! edge orientation

  !! Internal variables
  INTEGER(I4B) :: a, ii, jj
  REAL(DFP) :: rr(10), o1, o2, o3

  dim1 = SIZE(lambda, 2)
  dim2 = pe1 + pe2 + pe3 - 3
  dim3 = 3

  o1 = REAL(edgeOrient1, kind=DFP)
  o2 = REAL(edgeOrient2, kind=DFP)
  o3 = REAL(edgeOrient3, kind=DFP)

  a = 0
  ! edge(1) = 1 -> 2

  DO ii = 1, pe1 - 1
    rr(1) = o1**(ii + 1)
    rr(2) = o1**(ii)

    DO jj = 1, dim1
      rr(3) = lambda(1, jj) * lambda(2, jj)

      rr(4) = rr(1) * lambda(2, jj) * phi(jj, ii - 1)

      rr(5) = rr(2) * rr(3) * gradientPhi(jj, ii - 1)

      ans(jj, a + ii, 1) = rr(4) - rr(5)

      rr(4) = rr(1) * lambda(1, jj) * phi(jj, ii - 1)

      rr(5) = rr(2) * rr(3) * gradientPhi(jj, ii - 1)

      ans(jj, a + ii, 2) = rr(4) + rr(5)

      ans(jj, a + ii, 3) = 0.0_DFP

    END DO

  END DO

  ! edge(2) = 2 -> 3
  a = pe1 - 1

  DO ii = 1, pe2 - 1
    rr(1) = o2**(ii + 1)
    rr(2) = o2**(ii)

    DO jj = 1, dim1
      rr(3) = lambda(2, jj) * lambda(3, jj)

      ans(jj, a + ii, 1) = 0.0_DFP

      rr(4) = rr(1) * lambda(3, jj) * phi(jj + dim1, ii - 1)
      rr(5) = rr(2) * rr(3) * gradientPhi(jj + dim1, ii - 1)

      ans(jj, a + ii, 2) = rr(4) - rr(5)

      rr(4) = rr(1) * lambda(2, jj) * phi(jj + dim1, ii - 1)
      rr(5) = rr(2) * rr(3) * gradientPhi(jj + dim1, ii - 1)

      ans(jj, a + ii, 3) = rr(4) + rr(5)

    END DO

  END DO

  ! edge(3) = 3 -> 1
  a = pe1 - 1 + pe2 - 1

  DO ii = 1, pe3 - 1
    rr(1) = o3**(ii + 1)
    rr(2) = o3**(ii)

    DO jj = 1, dim1
      rr(3) = lambda(3, jj) * lambda(1, jj)

      rr(4) = rr(1) * lambda(3, jj) * phi(jj + 2 * dim1, ii - 1)
      rr(5) = rr(2) * rr(3) * gradientPhi(jj + 2 * dim1, ii - 1)

      ans(jj, a + ii, 1) = rr(4) + rr(5)

      ans(jj, a + ii, 2) = 0.0_DFP

      rr(4) = rr(1) * lambda(1, jj) * phi(jj + 2 * dim1, ii - 1)
      rr(5) = rr(2) * rr(3) * gradientPhi(jj + 2 * dim1, ii - 1)

      ans(jj, a + ii, 3) = rr(4) - rr(5)

    END DO

  END DO

END SUBROUTINE BarycentricEdgeBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                     BarycentricCellBasisGradient_Triangle2
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-04-21
! summary:  Evaluate the gradient of the cell basis on triangle

PURE SUBROUTINE BarycentricCellBasisGradient_Triangle2(order, lambda, phi, &
                               gradientPhi, ans, dim1, dim2, dim3, faceOrient)
  INTEGER(I4B), INTENT(IN) :: order
  !! order in the cell of triangle, it should be greater than 2
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! point of evaluation
  REAL(DFP), INTENT(IN) :: phi(1:, 0:)
  !! lobatto kernel values
  !! size(phi1, 1) = 3*number of points (lambda2-lambda1),
  !! (lambda3-lambda1), (lambda3-lambda2)
  !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
  REAL(DFP), INTENT(IN) :: gradientPhi(1:, 0:)
  !! gradients of lobatto kernel functions
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  !! gradient
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  !! dim1 = SIZE(lambda, 2)
  !! dim2 = INT((order - 1) * (order - 2) / 2)
  !! dim3 = 3
  INTEGER(I4B), INTENT(IN) :: faceOrient(2)
  !! face orientation

  ! internal variables
  INTEGER(I4B) :: k1, k2, cnt, ii
  REAL(DFP) :: rr(10)

  dim1 = SIZE(lambda, 2)
  dim2 = INT((order - 1) * (order - 2) / 2)
  dim3 = 3

  cnt = 0

  DO k1 = 1, order - 2
    DO k2 = 1, order - 1 - k1

      cnt = cnt + 1

      DO ii = 1, dim1

        rr(1) = lambda(1, ii) * lambda(2, ii) * lambda(3, ii)
        rr(2) = lambda(2, ii) * lambda(3, ii)
        rr(3) = lambda(1, ii) * lambda(3, ii)
        rr(4) = lambda(1, ii) * lambda(2, ii)

        rr(5) = rr(2) * phi(ii, k1 - 1) * phi(ii + 2 * dim1, k2 - 1)
        rr(6) = phi(ii + 2 * dim1, k2 - 1) * gradientPhi(ii, k1 - 1)
        rr(7) = phi(ii, k1 - 1) * gradientPhi(ii + 2 * dim1, k2 - 1)
        rr(8) = rr(6) - rr(7)
        ans(ii, cnt, 1) = rr(5) - rr(1) * rr(8)

        rr(5) = rr(3) * phi(ii, k1 - 1)
        rr(6) = rr(1) * gradientPhi(ii, k1 - 1)
        rr(7) = rr(5) + rr(6)
        rr(8) = phi(ii + 2 * dim1, k2 - 1)
        ans(ii, cnt, 2) = rr(7) * rr(8)

        rr(5) = rr(4) * phi(ii + 2 * dim1, k2 - 1)
        rr(6) = rr(1) * gradientPhi(ii + 2 * dim1, k2 - 1)
        rr(7) = rr(5) - rr(6)
        rr(8) = phi(ii, k1 - 1)
        ans(ii, cnt, 3) = rr(7) * rr(8)

      END DO

    END DO

  END DO
END SUBROUTINE BarycentricCellBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

PURE SUBROUTINE BarycentricHeirarchicalBasisGradient_Triangle(order, pe1, &
          pe2, pe3, lambda, refTriangle, ans, dim1, dim2, dim3, edgeOrient1, &
                                         edgeOrient2, edgeOrient3, faceOrient)
  INTEGER(I4B), INTENT(IN) :: order
  !! order in the cell of triangle, it should be greater than 2
  INTEGER(I4B), INTENT(IN) :: pe1
  !! order of interpolation on edge e1
  INTEGER(I4B), INTENT(IN) :: pe2
  !! order of interpolation on edge e2
  INTEGER(I4B), INTENT(IN) :: pe3
  !! order of interpolation on edge e3
  REAL(DFP), INTENT(IN) :: lambda(:, :)
  !! Barycenteric coordinates
  !! number of rows = 3
  !! number of cols = number of points
  CHARACTER(*), INTENT(IN) :: refTriangle
  !! reference triangle, "BIUNIT", "UNIT"
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  !! dim1=SIZE(lambda, 2)
  !! dim2=pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
  !! dim3=3
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  !! range of data written in ans
  INTEGER(I4B), INTENT(IN) :: edgeOrient1, edgeOrient2, edgeOrient3
  !! edge orientation
  INTEGER(I4B), INTENT(IN) :: faceOrient(2)
  !! face orientation

  INTEGER(I4B) :: a, b, ii, maxP, indx(3)
  REAL(DFP), ALLOCATABLE :: phi(:, :), gradientPhi(:, :), d_lambda(:)
  LOGICAL(LGT) :: isok

  dim1 = SIZE(lambda, 2)
  dim2 = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
  dim3 = 3

  maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2)

  a = 3 * dim1; b = maxP
  ALLOCATE (phi(a, 0:b), gradientPhi(a, 0:b), d_lambda(a))

  DO CONCURRENT(ii=1:dim1)
    ! edge 1 -> 2
    d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
    ! edge 2 -> 3
    d_lambda(ii + dim1) = lambda(3, ii) - lambda(2, ii)
    ! edge 3 -> 1
    d_lambda(ii + 2 * dim1) = lambda(1, ii) - lambda(3, ii)
  END DO

  CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=indx(1), &
                             ncol=indx(2))

  CALL LobattoKernelGradientEvalAll_(n=maxP, x=d_lambda, ans=gradientPhi, &
                                     nrow=indx(1), ncol=indx(2))

  ! gradient of vertex basis
  ans(1:dim1, 1:3, 1:3) = 0.0_DFP
  DO CONCURRENT(ii=1:3)
    ans(1:dim1, ii, ii) = 1.0_DFP
  END DO

  ! gradient of Edge basis function
  b = 3
  isok = ANY([pe1, pe2, pe3] .GE. 2_I4B)
  IF (isok) THEN
    a = b + 1
    b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
    CALL BarycentricEdgeBasisGradient_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
        lambda=lambda, phi=phi, gradientPhi=gradientPhi, ans=ans(:, a:b, :), &
          dim1=indx(1), dim2=indx(2), dim3=indx(3), edgeOrient1=edgeOrient1, &
                             edgeOrient2=edgeOrient2, edgeOrient3=edgeOrient3)
  END IF

  ! gradient of Cell basis function
  isok = order .GT. 2_I4B
  IF (isok) THEN
    a = b + 1
    b = a - 1 + INT((order - 1) * (order - 2) / 2)
    CALL BarycentricCellBasisGradient_Triangle2(order=order, lambda=lambda, &
                       phi=phi, gradientPhi=gradientPhi, ans=ans(:, a:b, :), &
              dim1=indx(1), dim2=indx(2), dim3=indx(3), faceOrient=faceOrient)
  END IF

  DEALLOCATE (phi, gradientPhi, d_lambda)

END SUBROUTINE BarycentricHeirarchicalBasisGradient_Triangle

!----------------------------------------------------------------------------
!                                         HeirarchicalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Triangle1
INTEGER(I4B) :: s(3)
CALL HeirarchicalBasisGradient_Triangle1_(order=order, pe1=pe1, &
   pe2=pe2, pe3=pe3, xij=xij, refTriangle=refTriangle, ans=ans, tsize1=s(1), &
                                          tsize2=s(2), tsize3=s(3))
END PROCEDURE HeirarchicalBasisGradient_Triangle1

!----------------------------------------------------------------------------
!                                         HeirarchicalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Triangle1_
INTEGER(I4B), PARAMETER :: orient = 1, faceOrient(2) = [0, 1]

CALL HeirarchicalBasisGradient_Triangle2_(order=order, pe1=pe1, pe2=pe2, &
                   pe3=pe3, xij=xij, edgeOrient1=orient, edgeOrient2=orient, &
         edgeOrient3=orient, faceOrient=faceOrient, refTriangle=refTriangle, &
                         ans=ans, tsize1=tsize1, tsize2=tsize2, tsize3=tsize3)
END PROCEDURE HeirarchicalBasisGradient_Triangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Triangle2_
REAL(DFP) :: jac(3, 2)
REAL(DFP), ALLOCATABLE :: lambda(:, :), dPhi(:, :, :)
INTEGER(I4B) :: ii, jj, kk, indx(3)

ii = SIZE(xij, 2)
jj = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
ALLOCATE (lambda(3, ii), dPhi(ii, jj, 3))

tsize1 = SIZE(xij, 2)
tsize2 = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
tsize3 = 2

CALL BarycentricCoordTriangle_(xin=xij, refTriangle=refTriangle, ans=lambda)

CALL BarycentricHeirarchicalBasisGradient_Triangle(order=order, pe1=pe1, &
         pe2=pe2, pe3=pe3, lambda=lambda, refTriangle=refTriangle, ans=dPhi, &
          dim1=indx(1), dim2=indx(2), dim3=indx(3), edgeOrient1=edgeOrient1, &
      edgeOrient2=edgeOrient2, edgeOrient3=edgeOrient3, faceOrient=faceOrient)

SELECT CASE (refTriangle(1:1))
CASE ("B", "b")
  jac(1, :) = [-0.50_DFP, -0.50_DFP]
  jac(2, :) = [0.50_DFP, 0.0_DFP]
  jac(3, :) = [0.0_DFP, 0.50_DFP]
CASE ("U", "u")
  jac(1, :) = [-1.0_DFP, -1.0_DFP]
  jac(2, :) = [1.0_DFP, 0.0_DFP]
  jac(3, :) = [0.0_DFP, 1.0_DFP]
END SELECT

DO CONCURRENT(ii=1:tsize1, jj=1:tsize2, kk=1:tsize3)
  ans(ii, jj, kk) = dPhi(ii, jj, 1) * jac(1, kk) &
                    + dPhi(ii, jj, 2) * jac(2, kk) &
                    + dPhi(ii, jj, 3) * jac(3, kk)
END DO

DEALLOCATE (lambda, dPhi)

END PROCEDURE HeirarchicalBasisGradient_Triangle2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE HeirarchicalBasisMethods
