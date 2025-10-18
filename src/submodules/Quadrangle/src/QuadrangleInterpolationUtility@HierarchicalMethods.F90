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

SUBMODULE(QuadrangleInterpolationUtility) HierarchicalMethods
USE LobattoPolynomialUtility, ONLY: LobattoEvalAll_, &
                                    LobattoGradientEvalAll_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     VertexBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle1
INTEGER(I4B) :: nrow, ncol
CALL VertexBasis_Quadrangle1_(x=x, y=y, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE VertexBasis_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle1_
nrow = SIZE(x)
ncol = 4
ans(1:nrow, 1) = 0.25_DFP * (1.0_DFP - x) * (1.0_DFP - y)
ans(1:nrow, 2) = 0.25_DFP * (1.0_DFP + x) * (1.0_DFP - y)
ans(1:nrow, 3) = 0.25_DFP * (1.0_DFP + x) * (1.0_DFP + y)
ans(1:nrow, 4) = 0.25_DFP * (1.0_DFP - x) * (1.0_DFP + y)
END PROCEDURE VertexBasis_Quadrangle1_

!----------------------------------------------------------------------------
!                                                    VertexBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle2
INTEGER(I4B) :: nrow, ncol
CALL VertexBasis_Quadrangle2_(xij=xij, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE VertexBasis_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle2_
CALL VertexBasis_Quadrangle1_(x=xij(1, :), y=xij(2, :), ans=ans, &
                              nrow=nrow, ncol=ncol)
END PROCEDURE VertexBasis_Quadrangle2_

!----------------------------------------------------------------------------
!                                         VertexBasisGradient_Quadrangle2_
!----------------------------------------------------------------------------

PURE SUBROUTINE VertexBasisGradient_Quadrangle2_(L1, L2, dL1, dL2, &
                                                 ans, dim1, dim2, dim3)
  REAL(DFP), INTENT(IN) :: L1(1:, 0:)
  !! L1 Lobatto polynomial evaluated at x coordinates
  REAL(DFP), INTENT(IN) :: L2(1:, 0:)
  !! L2 is Lobatto polynomial evaluated at y coordinates
  REAL(DFP), INTENT(IN) :: dL1(1:, 0:)
  !! L1 Lobatto polynomial evaluated at x coordinates
  REAL(DFP), INTENT(IN) :: dL2(1:, 0:)
  !! L2 is Lobatto polynomial evaluated at y coordinates
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  !! dim1= SIZE(L1, 1)
  !! dim2= 4
  !! dim3 = 2
  !! Gradient of vertex basis
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3

  dim1 = SIZE(L1, 1)
  dim2 = 4
  dim3 = 2
  ans(1:dim1, 1, 1) = dL1(1:dim1, 0) * L2(1:dim1, 0)
  ans(1:dim1, 2, 1) = dL1(1:dim1, 1) * L2(1:dim1, 0)
  ans(1:dim1, 3, 1) = dL1(1:dim1, 1) * L2(1:dim1, 1)
  ans(1:dim1, 4, 1) = dL1(1:dim1, 0) * L2(1:dim1, 1)
  ans(1:dim1, 1, 2) = L1(1:dim1, 0) * dL2(1:dim1, 0)
  ans(1:dim1, 2, 2) = L1(1:dim1, 1) * dL2(1:dim1, 0)
  ans(1:dim1, 3, 2) = L1(1:dim1, 1) * dL2(1:dim1, 1)
  ans(1:dim1, 4, 2) = L1(1:dim1, 0) * dL2(1:dim1, 1)

END SUBROUTINE VertexBasisGradient_Quadrangle2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE VertexBasis_Quadrangle3_(L1, L2, ans, nrow, ncol)
  REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
  !! L1 Lobatto polynomial evaluated at x coordinates
  !! L2 is Lobatto polynomial evaluated at y coordinates
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! ans(SIZE(L1, 1), 4)
  !! ans(:,v1) basis function of vertex v1 at all points
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  !! internal variable
  INTEGER(I4B) :: ii

  nrow = SIZE(L1, 1)
  ncol = 4

  DO CONCURRENT(ii=1:nrow)
    ans(ii, 1) = L1(ii, 0) * L2(ii, 0)
    ans(ii, 2) = L1(ii, 1) * L2(ii, 0)
    ans(ii, 3) = L1(ii, 1) * L2(ii, 1)
    ans(ii, 4) = L1(ii, 0) * L2(ii, 1)
  END DO
END SUBROUTINE VertexBasis_Quadrangle3_

!----------------------------------------------------------------------------
!                                               VerticalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VerticalEdgeBasis_Quadrangle
INTEGER(I4B) :: nrow, ncol
CALL VerticalEdgeBasis_Quadrangle_(qe1=qe1, qe2=qe2, x=x, y=y, ans=ans, &
                                   nrow=nrow, ncol=ncol)
END PROCEDURE VerticalEdgeBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VerticalEdgeBasis_Quadrangle_
! REAL(DFP) :: L2(1:SIZE(y), 0:MAX(qe1, qe2))
INTEGER(I4B) :: maxQ, aint, bint
INTEGER(I4B), PARAMETER :: maxP = 1, orient = 1
REAL(DFP), ALLOCATABLE :: L2(:, :), L1(:, :)

maxQ = MAX(qe1, qe2)

aint = SIZE(y)
nrow = SIZE(x)
ALLOCATE (L1(1:nrow, 0:maxP), L2(1:aint, 0:maxQ))

CALL LobattoEvalAll_(n=maxP, x=x, ans=L1, nrow=aint, ncol=bint)
CALL LobattoEvalAll_(n=maxQ, x=y, ans=L2, nrow=aint, ncol=bint)

CALL VerticalEdgeBasis_Quadrangle2_( &
  qe1=qe1, qe2=qe2, L1=L1, L2=L2, ans=ans, nrow=nrow, ncol=ncol, &
  qe1Orient=orient, qe2Orient=orient)

DEALLOCATE (L2, L1)

END PROCEDURE VerticalEdgeBasis_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE VerticalEdgeBasis_Quadrangle2_( &
  qe1, qe2, L1, L2, ans, nrow, ncol, qe1Orient, qe2Orient)
  INTEGER(I4B), INTENT(IN) :: qe1
  !! order on left vertical edge (e1), it should be greater than 1
  INTEGER(I4B), INTENT(IN) :: qe2
  !! order on right vertical edge(e2), it should be greater than 1
  REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
  !! Lobatto polynomials in x and y direction.
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! ans(SIZE(L1, 1), qe1 + qe2 - 2)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! number of rows and columns written to ans
  INTEGER(I4B), INTENT(IN), OPTIONAL :: qe1Orient, qe2Orient
  !! orientation of left and right vertical edge
  !! it can be 1 or -1

  INTEGER(I4B) :: k2, cnt, ii
  REAL(DFP) :: o1, o2

  o1 = REAL(-qe1Orient, kind=DFP)
  ! NOTE: Here we multiply by -1 because the left edge is oriented downwards &
  ! in master element
  o2 = REAL(qe2Orient, kind=DFP)

  nrow = SIZE(L1, 1)
  ncol = qe1 + qe2 - 2
  cnt = qe1 - 1

  !! left vertical
  DO CONCURRENT(k2=2:qe1, ii=1:nrow)
    ans(ii, k2 - 1) = (o1**k2) * L1(ii, 0) * L2(ii, k2)
  END DO

  !! right vertical
  DO CONCURRENT(k2=2:qe2, ii=1:nrow)
    ans(ii, cnt + k2 - 1) = (o2**k2) * L1(ii, 1) * L2(ii, k2)
  END DO

END SUBROUTINE VerticalEdgeBasis_Quadrangle2_

!----------------------------------------------------------------------------
!                                       VerticalEdgeBasisGradient_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit quadrangle

PURE SUBROUTINE VerticalEdgeBasisGradient_Quadrangle2_( &
  qe1, qe2, L1, L2, dL1, dL2, ans, dim1, dim2, dim3, qe1Orient, qe2Orient)
  INTEGER(I4B), INTENT(IN) :: qe1
  !! order on left vertical edge (e1), it should be greater than 1
  INTEGER(I4B), INTENT(IN) :: qe2
  !! order on right vertical edge(e2), it should be greater than 1
  REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
  !! Lobatto polynomials in x and y direction.
  REAL(DFP), INTENT(IN) :: dL1(1:, 0:), dL2(1:, 0:)
  !! Lobatto polynomials in x and y direction.
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  !! dim1=SIZE(L1, 1)
  !! dim2=qe1 + qe2 - 2
  !! dim3= 2
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  !! range of data written to ans
  INTEGER(I4B), INTENT(IN) :: qe1Orient, qe2Orient
  !! orientation fo left and write vertical edge
  !! it can be 1 or -1

  INTEGER(I4B) :: k2, cnt, ii
  REAL(DFP) :: o1, o2

  o1 = REAL(-qe1Orient, kind=DFP)
  ! NOTE: Here we multiply by -1 because the left edge is oriented downwards &
  ! in master element
  o2 = REAL(qe2Orient, kind=DFP)

  dim1 = SIZE(L1, 1)
  dim2 = qe1 + qe2 - 2
  dim3 = 2

  cnt = qe1 - 1

  DO CONCURRENT(k2=2:qe1, ii=1:dim1)
    ans(ii, k2 - 1, 1) = (o1**(k2 - 1)) * dL1(ii, 0) * L2(ii, k2)
    ans(ii, k2 - 1, 2) = (o1**(k2 - 1)) * L1(ii, 0) * dL2(ii, k2)
  END DO

  DO CONCURRENT(k2=2:qe2, ii=1:dim1)
    ans(ii, cnt + k2 - 1, 1) = (o2**(k2 - 1)) * dL1(ii, 1) * L2(ii, k2)
    ans(ii, cnt + k2 - 1, 2) = (o2**(k2 - 1)) * L1(ii, 1) * dL2(ii, k2)
  END DO

END SUBROUTINE VerticalEdgeBasisGradient_Quadrangle2_

!----------------------------------------------------------------------------
!                                             HorizontalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HorizontalEdgeBasis_Quadrangle
INTEGER(I4B) :: nrow, ncol
CALL HorizontalEdgeBasis_Quadrangle_(pe3, pe4, x, y, ans, nrow, ncol)
END PROCEDURE HorizontalEdgeBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HorizontalEdgeBasis_Quadrangle_
INTEGER(I4B) :: maxP, aint, bint
INTEGER(I4B), PARAMETER :: maxQ = 1, orient = 1

REAL(DFP), ALLOCATABLE :: L1(:, :), L2(:, :)

maxP = MAX(pe3, pe4)

nrow = SIZE(x)
aint = SIZE(y)

ALLOCATE (L1(1:nrow, 0:maxP), L2(1:aint, 0:maxQ))

CALL LobattoEvalAll_(n=maxP, x=x, ans=L1, nrow=aint, ncol=bint)
CALL LobattoEvalAll_(n=maxQ, x=y, ans=L2, nrow=aint, ncol=bint)

CALL HorizontalEdgeBasis_Quadrangle2_(pe3=pe3, pe4=pe4, L1=L1, L2=L2, &
            ans=ans, nrow=nrow, ncol=ncol, pe3Orient=orient, pe4Orient=orient)

DEALLOCATE (L1, L2)

END PROCEDURE HorizontalEdgeBasis_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE HorizontalEdgeBasis_Quadrangle2_( &
  pe3, pe4, L1, L2, ans, nrow, ncol, pe3Orient, pe4Orient)
  INTEGER(I4B), INTENT(IN) :: pe3
  !! order on bottom vertical edge (e3), it should be greater than 1
  INTEGER(I4B), INTENT(IN) :: pe4
  !! order on top vertical edge(e4), it should be greater than 1
  REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
  !! point of evaluation
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! ans(SIZE(L1, 1), pe3 + pe4 - 2)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! number of rows and columns written to ans
  INTEGER(I4B), INTENT(IN) :: pe3Orient, pe4Orient
  !! orientaion of bottom and top edge

  INTEGER(I4B) :: k1, cnt, ii
  REAL(DFP) :: o1, o2

  o1 = REAL(pe3Orient, kind=DFP)

  o2 = REAL(-pe4Orient, kind=DFP)
  ! NOTE: Here we multiply by -1 because the top edge is oriented leftwards &
  ! in master element

  nrow = SIZE(L1, 1)
  ncol = pe3 + pe4 - 2
  cnt = pe3 - 1

  !! bottom edge
  DO CONCURRENT(k1=2:pe3, ii=1:nrow)
    ans(ii, k1 - 1) = (o1**k1) * L1(ii, k1) * L2(ii, 0)
  END DO

  !! top edge
  DO CONCURRENT(k1=2:pe4, ii=1:nrow)
    ans(ii, cnt + k1 - 1) = (o2**k1) * L1(ii, k1) * L2(ii, 1)
  END DO

END SUBROUTINE HorizontalEdgeBasis_Quadrangle2_

!----------------------------------------------------------------------------
!                                     HorizontalEdgeBasisGradient_Quadrangle
!----------------------------------------------------------------------------

PURE SUBROUTINE HorizontalEdgeBasisGradient_Quadrangle2_( &
  pe3, pe4, L1, L2, dL1, dL2, ans, dim1, dim2, dim3, pe3Orient, pe4Orient)
  INTEGER(I4B), INTENT(IN) :: pe3
  !! order on bottom vertical edge (e3), it should be greater than 1
  INTEGER(I4B), INTENT(IN) :: pe4
  !! order on top vertical edge(e4), it should be greater than 1
  REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
  REAL(DFP), INTENT(IN) :: dL1(1:, 0:), dL2(1:, 0:)
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  !! dim1 = SIZE(L1, 1)
  !! dim2 = pe3 + pe4 - 2
  !! dim3 = 2
  INTEGER(I4B), INTENT(IN) :: pe3Orient, pe4Orient
  !! orientation of bottom and top horizontal edge

  !! internal variable
  INTEGER(I4B) :: k1, cnt, ii
  REAL(DFP) :: o1, o2

  o1 = REAL(pe3Orient, kind=DFP)

  o2 = REAL(-pe4Orient, kind=DFP)
  ! NOTE: Here we multiply by -1 because the top edge is oriented leftwards &
  ! in master element

  dim1 = SIZE(L1, 1)
  dim2 = pe3 + pe4 - 2
  dim3 = 2
  cnt = pe3 - 1

  !! bottom edge
  DO CONCURRENT(k1=2:pe3, ii=1:dim1)
    ans(ii, k1 - 1, 1) = (o1**(k1 - 1)) * dL1(ii, k1) * L2(ii, 0)
    ans(ii, k1 - 1, 2) = (o1**(k1 - 1)) * L1(ii, k1) * dL2(ii, 0)
  END DO

  !! top edge
  DO CONCURRENT(k1=2:pe4, ii=1:dim1)
    ans(ii, cnt + k1 - 1, 1) = (o2**(k1 - 1)) * dL1(ii, k1) * L2(ii, 1)
    ans(ii, cnt + k1 - 1, 2) = (o2**(k1 - 1)) * L1(ii, k1) * dL2(ii, 1)
  END DO

END SUBROUTINE HorizontalEdgeBasisGradient_Quadrangle2_

!----------------------------------------------------------------------------
!                                                      CellBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Quadrangle
INTEGER(I4B) :: nrow, ncol
CALL CellBasis_Quadrangle_(pb=pb, qb=qb, x=x, y=y, ans=ans, nrow=nrow, &
                           ncol=ncol)
END PROCEDURE CellBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Quadrangle_
REAL(DFP) :: L1(1:SIZE(x), 0:pb)
REAL(DFP) :: L2(1:SIZE(y), 0:qb)
INTEGER(I4B), PARAMETER :: faceOrient(3) = [1, 1, 1]

CALL LobattoEvalAll_(n=pb, x=x, ans=L1, nrow=nrow, ncol=ncol)
CALL LobattoEvalAll_(n=qb, x=y, ans=L2, nrow=nrow, ncol=ncol)

CALL CellBasis_Quadrangle2_(pb=pb, qb=qb, L1=L1, L2=L2, ans=ans, nrow=nrow, &
                            ncol=ncol, faceOrient=faceOrient)

END PROCEDURE CellBasis_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CellBasis_Quadrangle2_(pb, qb, L1, L2, ans, nrow, ncol, &
                                       faceOrient)
  INTEGER(I4B), INTENT(IN) :: pb
  !! order on bottom vertical edge (e3), it should be greater than 1
  INTEGER(I4B), INTENT(IN) :: qb
  !! order on top vertical edge(e4), it should be greater than 1
  REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
  !! point of evaluation
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  !! ans(SIZE(L1, 1), (pb - 1) * (qb - 1))
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! number of rows and cols written to ans
  INTEGER(I4B), INTENT(IN) :: faceOrient(3)
  !! face orientation

  !! Internal variables
  INTEGER(I4B) :: k1, k2, ii, p, q
  REAL(DFP) :: o1, o2

  nrow = SIZE(L1, 1)
  ncol = (pb - 1) * (qb - 1)

  o1 = REAL(faceOrient(1), kind=DFP)
  o2 = REAL(faceOrient(2), kind=DFP)

  IF (faceOrient(3) .LT. 0_I4B) THEN
    p = qb
    q = pb
  ELSE
    p = pb
    q = qb
  END IF

  DO CONCURRENT(k1=2:p, k2=2:q, ii=1:nrow)
    ans(ii, (q - 1) * (k1 - 2) + k2 - 1) = &
      (o1**k1) * (o2**k2) * L1(ii, k1) * L2(ii, k2)
  END DO

END SUBROUTINE CellBasis_Quadrangle2_

!----------------------------------------------------------------------------
!                                               CellBasisGradient_Quadrangle
!----------------------------------------------------------------------------

PURE SUBROUTINE CellBasisGradient_Quadrangle2_(pb, qb, L1, L2, &
                                  dL1, dL2, ans, dim1, dim2, dim3, faceOrient)
  INTEGER(I4B), INTENT(IN) :: pb
  !! order on bottom vertical edge (e3), it should be greater than 1
  INTEGER(I4B), INTENT(IN) :: qb
  !! order on top vertical edge(e4), it should be greater than 1
  REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
  REAL(DFP), INTENT(IN) :: dL1(1:, 0:), dL2(1:, 0:)
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  !! dim1=SIZE(L1, 1)
  !! dim2=(pb - 1) * (qb - 1)
  !! dim3=2
  INTEGER(I4B), INTENT(IN) :: faceOrient(3)

  !! internal variables
  INTEGER(I4B) :: k1, k2, ii, p, q
  REAL(DFP) :: o1, o2

  dim1 = SIZE(L1, 1)
  dim2 = (pb - 1) * (qb - 1)
  dim3 = 2

  o1 = REAL(faceOrient(1), kind=DFP)
  o2 = REAL(faceOrient(2), kind=DFP)

  IF (faceOrient(3) .LT. 0_I4B) THEN
    p = qb
    q = pb
  ELSE
    p = pb
    q = qb
  END IF

  DO CONCURRENT(k1=2:p, k2=2:q, ii=1:dim1)

    ans(ii, (q - 1) * (k1 - 2) + k2 - 1, 1) = &
      (o1**(k1 - 1)) * (o2**k2) * dL1(ii, k1) * L2(ii, k2)

    ans(ii, (q - 1) * (k1 - 2) + k2 - 1, 2) = &
      (o1**k1) * (o2**(k2 - 1)) * L1(ii, k1) * dL2(ii, k2)

  END DO

END SUBROUTINE CellBasisGradient_Quadrangle2_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle1
INTEGER(I4B) :: nrow, ncol
CALL HeirarchicalBasis_Quadrangle_( &
  pb=pb, qb=qb, pe3=pe3, pe4=pe4, qe1=qe1, qe2=qe2, xij=xij, ans=ans, &
  nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle1_
INTEGER(I4B), PARAMETER :: orient = 1, faceOrient(2) = [1, 1]
CALL HeirarchicalBasis_Quadrangle_( &
  pb=pb, qb=qb, pe3=pe3, pe4=pe4, qe1=qe1, qe2=qe2, xij=xij, &
  pe3Orient=orient, pe4Orient=orient, qe1Orient=orient, qe2Orient=orient, &
  faceOrient=faceOrient, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Quadrangle1_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle2
INTEGER(I4B) :: nrow, ncol
CALL HeirarchicalBasis_Quadrangle_( &
  pb=p, pe3=p, pe4=p, qb=q, qe1=q, qe2=q, xij=xij, ans=ans, nrow=nrow, &
  ncol=ncol)
END PROCEDURE HeirarchicalBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle2_
CALL HeirarchicalBasis_Quadrangle_( &
  pb=p, pe3=p, pe4=p, qb=q, qe1=q, qe2=q, xij=xij, ans=ans, nrow=nrow, &
  ncol=ncol)
END PROCEDURE HeirarchicalBasis_Quadrangle2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle3
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(xij, 2)
ncol = pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1

ALLOCATE (ans(1:nrow, 1:ncol))

CALL HeirarchicalBasis_Quadrangle_( &
  pb=pb, qb=qb, pe3=pe3, pe4=pe4, qe1=qe1, qe2=qe2, xij=xij, &
  pe3Orient=pe3Orient, pe4Orient=pe4Orient, qe1Orient=qe1Orient, &
  qe2Orient=qe2Orient, faceOrient=faceOrient, ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE HeirarchicalBasis_Quadrangle3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle3_
INTEGER(I4B) :: indx(4), maxP, maxQ
REAL(DFP), ALLOCATABLE :: L1(:, :), L2(:, :)
LOGICAL(LGT) :: isok

nrow = SIZE(xij, 2)
! ncol = pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1
ncol = 0

maxP = MAX(pe3, pe4, pb)
maxQ = MAX(qe1, qe2, qb)

ALLOCATE (L1(1:nrow, 0:maxP), L2(1:nrow, 0:maxQ))

CALL LobattoEvalAll_(n=maxP, x=xij(1, :), ans=L1, nrow=indx(1), ncol=indx(2))
CALL LobattoEvalAll_(n=maxQ, x=xij(2, :), ans=L2, nrow=indx(1), ncol=indx(2))

! Vertex basis function
CALL VertexBasis_Quadrangle3_(L1=L1, L2=L2, ans=ans, nrow=indx(1), &
                              ncol=indx(2))

ncol = indx(2)

! Edge basis function
isok = (qe1 .GE. 2_I4B) .OR. (qe2 .GE. 2_I4B)
IF (isok) THEN
  CALL VerticalEdgeBasis_Quadrangle2_( &
    qe1=qe1, qe2=qe2, L1=L1, L2=L2, ans=ans(:, ncol + 1:), nrow=indx(1), &
    ncol=indx(2), qe1Orient=qe1Orient, qe2Orient=qe2Orient)

  ncol = ncol + indx(2)
END IF

! Edge basis function
isok = (pe3 .GE. 2_I4B) .OR. (pe4 .GE. 2_I4B)
IF (isok) THEN
  CALL HorizontalEdgeBasis_Quadrangle2_( &
    pe3=pe3, pe4=pe4, L1=L1, L2=L2, ans=ans(:, ncol + 1:), nrow=indx(1), &
    ncol=indx(2), pe3Orient=pe3Orient, pe4Orient=pe4Orient)
  ncol = ncol + indx(2)
END IF

! Cell basis function
isok = (pb .GE. 2_I4B) .OR. (qb .GE. 2_I4B)
IF (isok) THEN
  CALL CellBasis_Quadrangle2_( &
    pb=pb, qb=qb, L1=L1, L2=L2, ans=ans(:, ncol + 1:), nrow=indx(1), &
    ncol=indx(2), faceOrient=faceOrient)
  ncol = ncol + indx(2)
END IF

DEALLOCATE (L1, L2)

END PROCEDURE HeirarchicalBasis_Quadrangle3_

!----------------------------------------------------------------------------
!                                       HeirarchicalBasisGradient_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Quadrangle1
INTEGER(I4B) :: dim1, dim2, dim3
CALL HeirarchicalBasisGradient_Quadrangle1_(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
          qe1=qe1, qe2=qe2, xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE HeirarchicalBasisGradient_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Quadrangle1_
INTEGER(I4B), PARAMETER :: orient = 1, faceOrient(3) = [1, 1, 1]

CALL HeirarchicalBasisGradient_Quadrangle3_(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
              qe1=qe1, qe2=qe2, xij=xij, qe1Orient=orient, qe2Orient=orient, &
         pe3Orient=orient, pe4Orient=orient, faceOrient=faceOrient, ans=ans, &
                                            dim1=dim1, dim2=dim2, dim3=dim3)

END PROCEDURE HeirarchicalBasisGradient_Quadrangle1_

!----------------------------------------------------------------------------
!                                       HeirarchicalBasisGradient_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Quadrangle2
INTEGER(I4B) :: dim1, dim2, dim3
CALL HeirarchicalBasisGradient_Quadrangle2_(p=p, q=q, xij=xij, ans=ans, &
                                            dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE HeirarchicalBasisGradient_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Quadrangle2_
CALL HeirarchicalBasisGradient_Quadrangle1_(pb=p, pe3=p, pe4=p, qb=q, qe1=q, &
                     qe2=q, xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE HeirarchicalBasisGradient_Quadrangle2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Quadrangle3
INTEGER(I4B) :: dim1, dim2, dim3
dim1 = SIZE(xij, 2)
dim2 = pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1
dim3 = 2

ALLOCATE (ans(1:dim1, 1:dim2, 1:dim3))

CALL HeirarchicalBasisGradient_Quadrangle3_(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
        qe1=qe1, qe2=qe2, xij=xij, qe1Orient=qe1Orient, qe2Orient=qe2Orient, &
   pe3Orient=pe3Orient, pe4Orient=pe4Orient, faceOrient=faceOrient, ans=ans, &
                                            dim1=dim1, dim2=dim2, dim3=dim3)

END PROCEDURE HeirarchicalBasisGradient_Quadrangle3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Quadrangle3_
INTEGER(I4B) :: maxP, maxQ, indx(3)
REAL(DFP), ALLOCATABLE :: L1(:, :), L2(:, :), dL1(:, :), dL2(:, :)
LOGICAL(LGT) :: isok

dim1 = SIZE(xij, 2)
dim2 = 0
dim3 = 2

maxP = MAX(pe3, pe4, pb)
maxQ = MAX(qe1, qe2, qb)

ALLOCATE (L1(1:dim1, 0:maxP), L2(1:dim1, 0:maxQ), &
          dL1(1:dim1, 0:maxP), dL2(1:dim1, 0:maxQ))

CALL LobattoEvalAll_(n=maxP, x=xij(1, :), ans=L1, nrow=indx(1), ncol=indx(2))
CALL LobattoEvalAll_(n=maxQ, x=xij(2, :), ans=L2, nrow=indx(1), ncol=indx(2))
CALL LobattoGradientEvalAll_(n=maxP, x=xij(1, :), ans=dL1, nrow=indx(1), &
                             ncol=indx(2))
CALL LobattoGradientEvalAll_(n=maxQ, x=xij(2, :), ans=dL2, nrow=indx(1), &
                             ncol=indx(2))

CALL VertexBasisGradient_Quadrangle2_(L1=L1, L2=L2, dL1=dL1, dL2=dL2, &
                            ans=ans, dim1=indx(1), dim2=indx(2), dim3=indx(3))

dim2 = indx(2)

isok = (qe1 .GE. 2_I4B) .OR. (qe2 .GE. 2_I4B)

IF (isok) THEN
  CALL VerticalEdgeBasisGradient_Quadrangle2_(qe1=qe1, qe2=qe2, L1=L1, &
            L2=L2, dL1=dL1, dL2=dL2, ans=ans(:, dim2 + 1:, :), dim1=indx(1), &
         dim2=indx(2), dim3=indx(3), qe1Orient=qe1Orient, qe2Orient=qe2Orient)

  dim2 = dim2 + indx(2)

END IF

! Edge basis function
isok = (pe3 .GE. 2_I4B) .OR. (pe4 .GE. 2_I4B)
IF (isok) THEN
  CALL HorizontalEdgeBasisGradient_Quadrangle2_(pe3=pe3, pe4=pe4, L1=L1, &
            L2=L2, dL1=dL1, dL2=dL2, ans=ans(:, dim2 + 1:, :), dim1=indx(1), &
         dim2=indx(2), dim3=indx(3), pe3Orient=pe3Orient, pe4Orient=pe4Orient)
  dim2 = dim2 + indx(2)
END IF

! Cell basis function
isok = (pb .GE. 2_I4B) .OR. (qb .GE. 2_I4B)
IF (isok) THEN
  CALL CellBasisGradient_Quadrangle2_(pb=pb, qb=qb, L1=L1, L2=L2, dL1=dL1, &
                            dL2=dL2, ans=ans(:, dim2 + 1:, :), dim1=indx(1), &
                            dim2=indx(2), dim3=indx(3), faceOrient=faceOrient)

  dim2 = dim2 + indx(2)
END IF

DEALLOCATE (L1, L2, dL1, dL2)

END PROCEDURE HeirarchicalBasisGradient_Quadrangle3_

END SUBMODULE HierarchicalMethods
