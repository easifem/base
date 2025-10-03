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

SUBMODULE(QuadrangleInterpolationUtility) Methods
USE LineInterpolationUtility, ONLY: QuadratureNumber_Line, &
                                    InterpolationPoint_Line_, &
                                    BasisEvalAll_Line_, &
                                    BasisGradientEvalAll_Line_, &
                                    QuadraturePoint_Line_
USE ReallocateUtility, ONLY: Reallocate
USE MappingUtility, ONLY: FromBiUnitQuadrangle2Quadrangle_, &
                          FromBiUnitQuadrangle2UnitQuadrangle_, &
                          JacobianQuadrangle
USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde_
USE GE_LUMethods, ONLY: GetLU, LUSolve
USE InputUtility, ONLY: Input
USE LegendrePolynomialUtility, ONLY: LegendreEvalAll_, &
                                     LegendreGradientEvalAll_
USE JacobiPolynomialUtility, ONLY: JacobiEvalAll_, &
                                   JacobiGradientEvalAll_
USE LobattoPolynomialUtility, ONLY: LobattoEvalAll_, &
                                    LobattoGradientEvalAll_
USE ErrorHandling, ONLY: Errormsg
USE F95_BLAS, ONLY: GEMM
USE StringUtility, ONLY: UpperCase
USE GE_CompRoutineMethods, ONLY: GetInvMat

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   RefElemDomain_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Quadrangle
ans = "BIUNIT"
END PROCEDURE RefElemDomain_Quadrangle

!----------------------------------------------------------------------------
!                                                       FacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Quadrangle
ans(1:2, 1) = [1, 2]
ans(1:2, 2) = [2, 3]
ans(1:2, 3) = [3, 4]
ans(1:2, 4) = [4, 1]
END PROCEDURE FacetConnectivity_Quadrangle

!----------------------------------------------------------------------------
!                                               QuadratureNumber_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadratureNumber_Quadrangle
ans(1) = QuadratureNumber_Line(order=p, quadType=quadType1)
ans(2) = QuadratureNumber_Line(order=q, quadType=quadType2)
END PROCEDURE QuadratureNumber_Quadrangle

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle1
INTEGER(I4B) :: nrow, ncol
nrow = LagrangeDOF_Quadrangle(order=order)
ALLOCATE (ans(nrow, 2))
CALL LagrangeDegree_Quadrangle1_(ans=ans, nrow=nrow, ncol=ncol, order=order)
END PROCEDURE LagrangeDegree_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle1_
CALL LagrangeDegree_Quadrangle2_(ans=ans, p=order, q=order, nrow=nrow, &
                                 ncol=ncol)
END PROCEDURE LagrangeDegree_Quadrangle1_

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle2
INTEGER(I4B) :: nrow, ncol

nrow = LagrangeDOF_Quadrangle(p=p, q=q)
ALLOCATE (ans(nrow, 2))
CALL LagrangeDegree_Quadrangle2_(ans=ans, nrow=nrow, ncol=ncol, &
                                 p=p, q=q)
END PROCEDURE LagrangeDegree_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle2_
INTEGER(I4B) :: ii, jj, p1

nrow = LagrangeDOF_Quadrangle(p=p, q=q)
ncol = 2
p1 = p + 1

DO CONCURRENT(jj=0:q, ii=0:p)
  ans(p1 * jj + ii + 1, 1) = ii
  ans(p1 * jj + ii + 1, 2) = jj
END DO

END PROCEDURE LagrangeDegree_Quadrangle2_

!----------------------------------------------------------------------------
!                                                     GetTotalDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Quadrangle
ans = (order + 1)**2
END PROCEDURE GetTotalDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                   GetTotalInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Quadrangle1
ans = (order - 1)**2
END PROCEDURE GetTotalInDOF_Quadrangle1

!----------------------------------------------------------------------------
!                                                   GetTotalInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Quadrangle2
ans = (p - 1) * (q - 1)
END PROCEDURE GetTotalInDOF_Quadrangle2

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Quadrangle1
ans = (order + 1)**2
END PROCEDURE LagrangeDOF_Quadrangle1

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Quadrangle2
ans = (p + 1) * (q + 1)
END PROCEDURE LagrangeDOF_Quadrangle2

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Quadrangle1
ans = (order - 1)**2
END PROCEDURE LagrangeInDOF_Quadrangle1

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Quadrangle2
ans = (p - 1) * (q - 1)
END PROCEDURE LagrangeInDOF_Quadrangle2

!----------------------------------------------------------------------------
!                                               EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle1
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 2_I4B
END IF

ncol = LagrangeDOF_Quadrangle(order=order)

ALLOCATE (ans(nrow, ncol))

CALL EquidistancePoint_Quadrangle1_(order=order, ans=ans, nrow=nrow, &
                                    ncol=ncol, xij=xij)

END PROCEDURE EquidistancePoint_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle1_
CALL EquidistancePoint_Quadrangle2_(p=order, q=order, ans=ans, nrow=nrow, &
                                    ncol=ncol, xij=xij)
END PROCEDURE EquidistancePoint_Quadrangle1_

!----------------------------------------------------------------------------
!                                               EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle2
INTEGER(I4B) :: nrow, ncol
nrow = 2; IF (PRESENT(xij)) nrow = SIZE(xij, 1)
ncol = (p + 1) * (q + 1)
ALLOCATE (ans(nrow, ncol))
CALL EquidistancePoint_Quadrangle2_(p=p, q=q, ans=ans, nrow=nrow, ncol=ncol, &
                                    xij=xij)
END PROCEDURE EquidistancePoint_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle2_
CALL InterpolationPoint_Quadrangle2_( &
  p=p, q=q, ipType1=Equidistance, ipType2=Equidistance, ans=ans, &
  nrow=nrow, ncol=ncol, layout="VEFC", xij=xij)
END PROCEDURE EquidistancePoint_Quadrangle2_

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Quadrangle1
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 2
END IF

ncol = LagrangeInDOF_Quadrangle(order=order)

IF (ncol .EQ. 0) THEN
  ALLOCATE (ans(0, 0))
  RETURN
ELSE
  ALLOCATE (ans(nrow, ncol))
  ans(1:nrow, 1:ncol) = EquidistanceInPoint_Quadrangle2(p=order, q=order, &
                                                        xij=xij)
END IF
END PROCEDURE EquidistanceInPoint_Quadrangle1

!----------------------------------------------------------------------------
!                                             EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Quadrangle2
REAL(DFP), ALLOCATABLE :: temp(:, :)
INTEGER(I4B) :: a, b, nrow, ncol

a = LagrangeDOF_Quadrangle(p=p, q=q)
b = LagrangeInDOF_Quadrangle(p=p, q=q)

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 2
END IF

ALLOCATE (temp(nrow, a))

CALL EquidistancePoint_Quadrangle2_(p=p, q=q, xij=xij, ans=temp, &
                                    nrow=nrow, ncol=ncol)

IF (b .EQ. 0) THEN
  ALLOCATE (ans(0, 0))
ELSE
  ALLOCATE (ans(nrow, b))

  ans(1:nrow, 1:b) = temp(1:nrow, a - b + 1:)
END IF

DEALLOCATE (temp)

END PROCEDURE EquidistanceInPoint_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJ2VEFC_Quadrangle
CALL IJ2VEFC_Quadrangle_AntiClockwise(xi, eta, temp, p, q, 1_I4B)
END PROCEDURE IJ2VEFC_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetEdgeConnectivityHelpAntiClock(edgeConnectivity, &
                                                 pointsOrder, startNode)
  INTEGER(I4B), INTENT(INOUT) :: edgeConnectivity(:, :)
  INTEGER(I4B), INTENT(OUT) :: pointsOrder(:)
  INTEGER(I4B), INTENT(IN) :: startNode

  SELECT CASE (startNode)
  CASE (1)
    edgeConnectivity(1:2, 1) = [1, 2]
    edgeConnectivity(1:2, 2) = [2, 3]
    edgeConnectivity(1:2, 3) = [3, 4]
    edgeConnectivity(1:2, 4) = [4, 1]
    pointsOrder = [1, 2, 3, 4]
  CASE (2)
    edgeConnectivity(1:2, 1) = [2, 3]
    edgeConnectivity(1:2, 2) = [3, 4]
    edgeConnectivity(1:2, 3) = [4, 1]
    edgeConnectivity(1:2, 4) = [1, 2]
    pointsOrder = [2, 3, 4, 1]
  CASE (3)
    edgeConnectivity(1:2, 1) = [3, 4]
    edgeConnectivity(1:2, 2) = [4, 1]
    edgeConnectivity(1:2, 3) = [1, 2]
    edgeConnectivity(1:2, 4) = [2, 3]
    pointsOrder = [3, 4, 1, 2]
  CASE (4)
    edgeConnectivity(1:2, 1) = [4, 1]
    edgeConnectivity(1:2, 2) = [1, 2]
    edgeConnectivity(1:2, 3) = [2, 3]
    edgeConnectivity(1:2, 4) = [3, 4]
    pointsOrder = [4, 1, 2, 3]
  END SELECT

END SUBROUTINE GetEdgeConnectivityHelpAntiClock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetEdgeConnectivityHelpClock(edgeConnectivity, pointsOrder, &
                                             startNode)
  INTEGER(I4B), INTENT(INOUT) :: edgeConnectivity(:, :)
  INTEGER(I4B), INTENT(OUT) :: pointsOrder(:)
  INTEGER(I4B), INTENT(IN) :: startNode

  SELECT CASE (startNode)
  CASE (1)
    edgeConnectivity(1:2, 1) = [1, 4]
    edgeConnectivity(1:2, 2) = [4, 3]
    edgeConnectivity(1:2, 3) = [3, 2]
    edgeConnectivity(1:2, 4) = [2, 1]
    pointsOrder = [1, 4, 3, 2]
  CASE (2)
    edgeConnectivity(1:2, 1) = [2, 1]
    edgeConnectivity(1:2, 2) = [1, 4]
    edgeConnectivity(1:2, 3) = [4, 3]
    edgeConnectivity(1:2, 4) = [3, 2]
    pointsOrder = [2, 1, 4, 3]
  CASE (3)
    edgeConnectivity(1:2, 1) = [3, 2]
    edgeConnectivity(1:2, 2) = [2, 1]
    edgeConnectivity(1:2, 3) = [1, 4]
    edgeConnectivity(1:2, 4) = [4, 3]
    pointsOrder = [3, 2, 1, 4]
  CASE (4)
    edgeConnectivity(1:2, 1) = [4, 3]
    edgeConnectivity(1:2, 2) = [3, 2]
    edgeConnectivity(1:2, 3) = [2, 1]
    edgeConnectivity(1:2, 4) = [1, 4]
    pointsOrder = [4, 3, 2, 1]
  END SELECT

END SUBROUTINE GetEdgeConnectivityHelpClock

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJ2VEFC_Quadrangle_Clockwise
! internal variables
INTEGER(I4B) :: cnt, ii, jj, ll, N, ij(2, 4), iedge, p1, p2
INTEGER(I4B), PARAMETER :: tEdges = 4
INTEGER(I4B) :: edgeConnectivity(2, 4), ii1, ii2, dii, jj1, jj2, djj, &
                pointsOrder(4)
REAL(DFP), ALLOCATABLE :: xi_in(:, :), eta_in(:, :), &
                          temp_in(:, :)

LOGICAL(LGT) :: isok, abool

! vertices
N = (p + 1) * (q + 1)
cnt = 0
ll = -1

CALL GetEdgeConnectivityHelpClock(edgeConnectivity, pointsOrder, startNode)

isok = (p .EQ. 0) .AND. (q .EQ. 0)
IF (isok) THEN
  temp(1, 1) = xi(1, 1)
  temp(2, 1) = eta(1, 1)
  RETURN
END IF

! INFO: This case is p = 0 and q .GE. 1
abool = (p .EQ. 0) .AND. (q .GE. 1)
IF (abool) THEN
  DO jj = 1, q + 1
    cnt = cnt + 1
    temp(1, jj) = xi(1, jj)
    temp(2, jj) = eta(1, jj)
  END DO
  RETURN
END IF

! INFO: This case is q = 0 and p .GE. 1
abool = (q .EQ. 0) .AND. (p .GE. 1)
IF (abool) THEN
  DO ii = 1, p + 1
    cnt = cnt + 1
    temp(1, ii) = xi(ii, 1)
    temp(2, ii) = eta(ii, 1)
  END DO
  RETURN
END IF

ij(1, 1) = 1
ij(2, 1) = 1

ij(1, 2) = p + 1
ij(2, 2) = 1

ij(1, 3) = p + 1
ij(2, 3) = q + 1

ij(1, 4) = 1
ij(2, 4) = q + 1

isok = (p .GE. 1) .AND. (q .GE. 1)

IF (isok) THEN

  DO ii = 1, 4
    cnt = cnt + 1
    jj = pointsOrder(ii)
    temp(1, ii) = xi(ij(1, jj), ij(2, jj))

    temp(2, ii) = eta(ij(1, jj), ij(2, jj))

  END DO

END IF

abool = (p .EQ. 1) .AND. (q .EQ. 1)
IF (abool) RETURN

isok = (p .GE. 1) .AND. (q .GE. 1)
IF (.NOT. isok) RETURN

DO iedge = 1, tEdges
  p1 = edgeConnectivity(1, iedge)
  p2 = edgeConnectivity(2, iedge)

  IF (ij(1, p1) .EQ. ij(1, p2)) THEN
    ii1 = ij(1, p1)
    ii2 = ii1
    dii = 1
  ELSE IF (ij(1, p1) .LT. ij(1, p2)) THEN
    ii1 = ij(1, p1) + 1
    ii2 = ij(1, p2) - 1
    dii = 1
  ELSE IF (ij(1, p1) .GT. ij(1, p2)) THEN
    ii1 = ij(1, p1) - 1
    ii2 = ij(1, p2) + 1
    dii = -1
  END IF

  IF (ij(2, p1) .EQ. ij(2, p2)) THEN
    jj1 = ij(2, p1)
    jj2 = jj1
    djj = 1
  ELSE IF (ij(2, p1) .LT. ij(2, p2)) THEN
    jj1 = ij(2, p1) + 1
    jj2 = ij(2, p2) - 1
    djj = 1
  ELSE IF (ij(2, p1) .GT. ij(2, p2)) THEN
    jj1 = ij(2, p1) - 1
    jj2 = ij(2, p2) + 1
    djj = -1
  END IF

  DO ii = ii1, ii2, dii
    DO jj = jj1, jj2, djj
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END DO
  END DO
END DO

! internal nodes
isok = (p .GE. 2) .AND. (q .GE. 2)
IF (.NOT. isok) RETURN

CALL Reallocate(xi_in, MAX(p - 1, 1_I4B), MAX(q - 1_I4B, 1_I4B))
CALL Reallocate(eta_in, SIZE(xi_in, 1), SIZE(xi_in, 2))
CALL Reallocate(temp_in, 2, SIZE(xi_in))

IF (p .LE. 1_I4B) THEN
  ii1 = 1
  ii2 = 1
ELSE
  ii1 = 2
  ii2 = p
END IF

IF (q .LE. 1_I4B) THEN
  jj1 = 1
  jj2 = 1
ELSE
  jj1 = 2
  jj2 = q
END IF

xi_in = xi(ii1:ii2, jj1:jj2)
eta_in = eta(ii1:ii2, jj1:jj2)

CALL IJ2VEFC_Quadrangle_Clockwise(xi=xi_in, &
                                  eta=eta_in, &
                                  temp=temp_in, &
                                  p=MAX(p - 2, 0_I4B), &
                                  q=MAX(q - 2, 0_I4B), &
                                  startNode=startNode)

ii1 = cnt + 1
ii2 = ii1 + SIZE(temp_in, 2) - 1
temp(1:2, ii1:ii2) = temp_in

IF (ALLOCATED(xi_in)) DEALLOCATE (xi_in)
IF (ALLOCATED(eta_in)) DEALLOCATE (eta_in)
IF (ALLOCATED(temp_in)) DEALLOCATE (temp_in)

END PROCEDURE IJ2VEFC_Quadrangle_Clockwise

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJ2VEFC_Quadrangle_AntiClockwise
! internal variables
INTEGER(I4B) :: cnt, ii, jj, ll, N, ij(2, 4), iedge, p1, p2
INTEGER(I4B), PARAMETER :: tEdges = 4
INTEGER(I4B) :: edgeConnectivity(2, 4), ii1, ii2, dii, jj1, jj2, djj, &
                pointsOrder(4)
REAL(DFP), ALLOCATABLE :: xi_in(:, :), eta_in(:, :), &
                          temp_in(:, :)
LOGICAL(LGT) :: isok, abool

! vertices
N = (p + 1) * (q + 1)
cnt = 0
ll = -1

CALL GetEdgeConnectivityHelpAntiClock(edgeConnectivity, pointsOrder, startNode)

isok = (p .EQ. 0) .AND. (q .EQ. 0)
IF (isok) THEN
  temp(1, 1) = xi(1, 1)
  temp(2, 1) = eta(1, 1)
  RETURN
END IF

ij(1:2, 1) = [1, 1]
ij(1:2, 2) = [p + 1, 1]
ij(1:2, 3) = [p + 1, q + 1]
ij(1:2, 4) = [1, q + 1]

isok = (p .GE. 1) .AND. (q .GE. 1)
IF (isok) THEN
  DO ii = 1, 4
    cnt = cnt + 1
    jj = pointsOrder(ii)
    temp(1:2, ii) = [&
      & xi(ij(1, jj), ij(2, jj)), &
      & eta(ij(1, jj), ij(2, jj)) &
      & ]
  END DO

  abool = (p .EQ. 1) .AND. (q .EQ. 1)
  IF (abool) RETURN

ELSE

  DO ii = 1, MIN(p, 1) + 1
    DO jj = 1, MIN(q, 1) + 1
      cnt = cnt + 1
      temp(1:2, cnt) = [&
        & xi(ij(1, cnt), ij(2, cnt)), &
        & eta(ij(1, cnt), ij(2, cnt))]
    END DO
  END DO
END IF

IF (ALL([p, q] .GE. 1_I4B)) THEN
  DO iedge = 1, tEdges
    p1 = edgeConnectivity(1, iedge)
    p2 = edgeConnectivity(2, iedge)

    IF (ij(1, p1) .EQ. ij(1, p2)) THEN
      ii1 = ij(1, p1)
      ii2 = ii1
      dii = 1
    ELSE IF (ij(1, p1) .LT. ij(1, p2)) THEN
      ii1 = ij(1, p1) + 1
      ii2 = ij(1, p2) - 1
      dii = 1
    ELSE IF (ij(1, p1) .GT. ij(1, p2)) THEN
      ii1 = ij(1, p1) - 1
      ii2 = ij(1, p2) + 1
      dii = -1
    END IF

    IF (ij(2, p1) .EQ. ij(2, p2)) THEN
      jj1 = ij(2, p1)
      jj2 = jj1
      djj = 1
    ELSE IF (ij(2, p1) .LT. ij(2, p2)) THEN
      jj1 = ij(2, p1) + 1
      jj2 = ij(2, p2) - 1
      djj = 1
    ELSE IF (ij(2, p1) .GT. ij(2, p2)) THEN
      jj1 = ij(2, p1) - 1
      jj2 = ij(2, p2) + 1
      djj = -1
    END IF

    DO ii = ii1, ii2, dii
      DO jj = jj1, jj2, djj
        cnt = cnt + 1
        temp(:, cnt) = [xi(ii, jj), eta(ii, jj)]
      END DO
    END DO
  END DO

  ! internal nodes
  IF (ALL([p, q] .GE. 2_I4B)) THEN

    CALL Reallocate(xi_in, MAX(p - 1, 1_I4B), MAX(q - 1_I4B, 1_I4B))
    CALL Reallocate(eta_in, SIZE(xi_in, 1), SIZE(xi_in, 2))
    CALL Reallocate(temp_in, 2, SIZE(xi_in))

    IF (p .LE. 1_I4B) THEN
      ii1 = 1
      ii2 = 1
    ELSE
      ii1 = 2
      ii2 = p
    END IF

    IF (q .LE. 1_I4B) THEN
      jj1 = 1
      jj2 = 1
    ELSE
      jj1 = 2
      jj2 = q
    END IF

    xi_in = xi(ii1:ii2, jj1:jj2)
    eta_in = eta(ii1:ii2, jj1:jj2)

    CALL IJ2VEFC_Quadrangle_AntiClockwise( &
      xi=xi_in, eta=eta_in, temp=temp_in, p=MAX(p - 2, 0_I4B), &
      q=MAX(q - 2, 0_I4B), startNode=startNode)

    ii1 = cnt + 1
    ii2 = ii1 + SIZE(temp_in, 2) - 1
    temp(1:2, ii1:ii2) = temp_in
  END IF

END IF

IF (ALLOCATED(xi_in)) DEALLOCATE (xi_in)
IF (ALLOCATED(eta_in)) DEALLOCATE (eta_in)
IF (ALLOCATED(temp_in)) DEALLOCATE (temp_in)

END PROCEDURE IJ2VEFC_Quadrangle_AntiClockwise

!----------------------------------------------------------------------------
!                                              InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle1
ans = InterpolationPoint_Quadrangle2( &
      p=order, q=order, ipType1=ipType, ipType2=ipType, xij=xij, &
      layout=layout, alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, &
      beta2=beta, lambda2=lambda)
END PROCEDURE InterpolationPoint_Quadrangle1

!----------------------------------------------------------------------------
!                                              InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle1_
CALL InterpolationPoint_Quadrangle2_( &
  p=order, q=order, ipType1=ipType, ipType2=ipType, xij=xij, layout=layout, &
  alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, beta2=beta, &
  lambda2=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE InterpolationPoint_Quadrangle1_

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle2
INTEGER(I4B) :: nrow, ncol

nrow = 2; IF (PRESENT(xij)) nrow = SIZE(xij, 1)
ncol = (p + 1) * (q + 1)
ALLOCATE (ans(nrow, ncol))

CALL InterpolationPoint_Quadrangle2_( &
  p=p, q=q, ipType1=ipType1, ipType2=ipType2, ans=ans, nrow=nrow, ncol=ncol, &
  layout=layout, xij=xij, alpha1=alpha1, beta1=beta1, lambda1=lambda1, &
  alpha2=alpha2, beta2=beta2, lambda2=lambda2)

END PROCEDURE InterpolationPoint_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle2_
REAL(DFP), PARAMETER :: biunit_xij(2) = [-1.0_DFP, 1.0_DFP]

REAL(DFP) :: x(p + 1), y(q + 1), xi(p + 1, q + 1), eta(p + 1, q + 1)
INTEGER(I4B) :: ii, jj, kk, tsize

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 2
END IF

ncol = (p + 1) * (q + 1)

CALL InterpolationPoint_Line_( &
  order=p, ipType=ipType1, xij=biunit_xij, layout="INCREASING", &
  alpha=alpha1, beta=beta1, lambda=lambda1, ans=x, tsize=tsize)

CALL InterpolationPoint_Line_( &
  order=q, ipType=ipType2, xij=biunit_xij, layout="INCREASING", &
  alpha=alpha2, beta=beta2, lambda=lambda2, ans=y, tsize=tsize)

kk = 0
DO ii = 1, p + 1
  DO jj = 1, q + 1
    kk = kk + 1
    xi(ii, jj) = x(ii)
    ans(1, kk) = x(ii)

    eta(ii, jj) = y(jj)
    ans(2, kk) = y(jj)
  END DO
END DO

IF (layout(1:4) .EQ. "VEFC") THEN
  CALL IJ2VEFC_Quadrangle(xi=xi, eta=eta, temp=ans(1:2, 1:ncol), p=p, q=q)
END IF

IF (PRESENT(xij)) THEN
  CALL FromBiUnitQuadrangle2Quadrangle_( &
    xin=ans(1:2, 1:ncol), x1=xij(:, 1), x2=xij(:, 2), &
    x3=xij(:, 3), x4=xij(:, 4), ans=ans, nrow=ii, ncol=jj)
END IF

END PROCEDURE InterpolationPoint_Quadrangle2_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle1
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Quadrangle1_(order=order, i=i, xij=xij, ans=ans, &
                                tsize=tsize)
END PROCEDURE LagrangeCoeff_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle1_
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info, nrow, ncol

tsize = SIZE(xij, 2)

ipiv = 0_I4B; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
! V = LagrangeVandermonde(order=order, xij=xij, elemType=Quadrangle)
CALL LagrangeVandermonde_(order=order, xij=xij, elemType=Quadrangle, ans=V, &
                          nrow=nrow, ncol=ncol)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle1_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle2
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Quadrangle2_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                                ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle2_
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

vtemp = v; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle2_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Quadrangle3_(order=order, i=i, v=v, ipiv=ipiv, &
                                ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Quadrangle3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle3_
INTEGER(I4B) :: info
tsize = SIZE(v, 1)
ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle3_

!----------------------------------------------------------------------------
!                                                 LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle4
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Quadrangle4_(order=order, xij=xij, basisType=basisType, &
                             alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                                nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Quadrangle4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle4_
INTEGER(I4B) :: basisType0

basisType0 = Input(default=Monomial, option=basisType)

IF (basisType0 .EQ. Heirarchical) THEN
  CALL HeirarchicalBasis_Quadrangle2_(p=order, q=order, xij=xij, &
                                      ans=ans, nrow=nrow, ncol=ncol)
  CALL GetInvMat(ans(1:nrow, 1:ncol))
  RETURN
END IF

! ans(1:nrow, 1:ncol) = TensorProdBasis_Quadrangle1(p=order, q=order, &
CALL TensorProdBasis_Quadrangle1_(p=order, q=order, &
                      xij=xij, basisType1=basisType0, basisType2=basisType0, &
                     alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, &
                    beta2=beta, lambda2=lambda, ans=ans, nrow=nrow, ncol=ncol)

CALL GetInvMat(ans(1:nrow, 1:ncol))

END PROCEDURE LagrangeCoeff_Quadrangle4_

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle5
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Quadrangle5_(p=p, q=q, xij=xij, basisType1=basisType1, &
         basisType2=basisType2, alpha1=alpha1, beta1=beta1, lambda1=lambda1, &
   alpha2=alpha2, beta2=beta2, lambda2=lambda2, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Quadrangle5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle5_
INTEGER(I4B) :: basisType(2)

basisType(1) = Input(default=Monomial, option=basisType1)
basisType(2) = Input(default=Monomial, option=basisType2)

IF (ALL(basisType .EQ. Heirarchical)) THEN
  ! ans(1:nrow, 1:ncol) = HeirarchicalBasis_Quadrangle2(p=p, q=q, xij=xij)
  CALL HeirarchicalBasis_Quadrangle2_(p=p, q=q, xij=xij, &
                                      ans=ans, nrow=nrow, ncol=ncol)

  CALL GetInvMat(ans(1:nrow, 1:ncol))
  RETURN
END IF

! ans(1:nrow, 1:ncol) = TensorProdBasis_Quadrangle1(p=p, q=q, xij=xij, &
CALL TensorProdBasis_Quadrangle1_(p=p, q=q, xij=xij, &
       basisType1=basisType(1), alpha1=alpha1, beta1=beta1, lambda1=lambda1, &
       basisType2=basisType(2), alpha2=alpha2, beta2=beta2, lambda2=lambda2, &
                                  ans=ans, nrow=nrow, ncol=ncol)

CALL GetInvMat(ans(1:nrow, 1:ncol))

END PROCEDURE LagrangeCoeff_Quadrangle5_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Quadrangle1
INTEGER(I4B) :: nrow, ncol
CALL Dubiner_Quadrangle1_(xij=xij, order=order, ans=ans, nrow=nrow, &
                          ncol=ncol)
END PROCEDURE Dubiner_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Quadrangle1_
#define TP size(xij, 2)

REAL(DFP) :: P1(TP, order + 1), P2(TP, order + 1), temp(TP, 3)

REAL(DFP) :: alpha, beta

INTEGER(I4B) :: k1, k2, max_k2, cnt, indx(2), ii

#undef TP

nrow = SIZE(xij, 2)
ncol = (order + 1) * (order + 2) / 2

CALL LegendreEvalAll_(n=order, x=xij(1, :), ans=P1, nrow=indx(1), &
                      ncol=indx(2))

! we do not need x now, so let store (1-y)/2 in x
DO CONCURRENT(ii=1:nrow)
  temp(ii, 3) = xij(2, ii)
  temp(ii, 1) = 0.5_DFP * (1.0_DFP - temp(ii, 3))
END DO

alpha = 0.0_DFP
beta = 0.0_DFP
cnt = 0

! temp1 = 0.5 * (1.0 - y)
! temp3 = y

DO k1 = 0, order

  !! note here temp1 is
  !! note here x = 0.5_DFP*(1-y)
  DO CONCURRENT(ii=1:nrow)
    temp(ii, 2) = temp(ii, 1)**k1
  END DO

  alpha = 2.0_DFP * k1 + 1.0_DFP

  max_k2 = order - k1

  ! P2(:, 1:max_k2 + 1) = JacobiEvalAll(n=max_k2, x=y, alpha=alpha, beta=beta)
 CALL JacobiEvalAll_(n=max_k2, x=temp(:, 3), alpha=alpha, beta=beta, ans=P2, &
                      nrow=indx(1), ncol=indx(2))

  DO k2 = 0, max_k2
    cnt = cnt + 1

    DO CONCURRENT(ii=1:nrow)
      ans(ii, cnt) = P1(ii, k1 + 1) * temp(ii, 2) * P2(ii, k2 + 1)
    END DO
  END DO

END DO

END PROCEDURE Dubiner_Quadrangle1_

!----------------------------------------------------------------------------
!                                               DubinerGradient_Quadrangle1
!----------------------------------------------------------------------------

MODULE PROCEDURE DubinerGradient_Quadrangle1
INTEGER(I4B) :: s(3)
CALL DubinerGradient_Quadrangle1_(xij=xij, order=order, ans=ans, &
                                  tsize1=s(1), tsize2=s(2), tsize3=s(3))
END PROCEDURE DubinerGradient_Quadrangle1

!----------------------------------------------------------------------------
!                                               DubinerGradient_Quadrangle1
!----------------------------------------------------------------------------

MODULE PROCEDURE DubinerGradient_Quadrangle1_
REAL(DFP), DIMENSION(SIZE(xij, 2), order + 1) :: P1, P2, dP1, dP2
REAL(DFP), DIMENSION(SIZE(xij, 2)) :: avec, bvec, x, y
REAL(DFP) :: alpha, beta, areal
INTEGER(I4B) :: k1, k2, max_k2, cnt, indx(2), ii

tsize1 = SIZE(xij, 2)
tsize2 = (order + 1) * (order + 2) / 2
tsize3 = 2

x = xij(1, :)
y = xij(2, :)

! P1 = LegendreEvalAll(n=order, x=x)
CALL LegendreEvalAll_(n=order, x=x, ans=P1, nrow=indx(1), ncol=indx(2))

! dP1 = LegendreGradientEvalAll(n=order, x=x)
CALL LegendreGradientEvalAll_(n=order, x=x, ans=dP1, nrow=indx(1), &
                              ncol=indx(2))

! we do not need x now, so let store (1-y)/2 in x
x = 0.5_DFP * (1.0_DFP - y)
alpha = 1.0_DFP
beta = 0.0_DFP
cnt = 0

DO k1 = 0, order
  bvec = x**(MAX(k1 - 1_I4B, 0_I4B))
  avec = x * bvec
  alpha = 2.0_DFP * k1 + 1.0_DFP

  max_k2 = order - k1

  CALL JacobiEvalAll_(n=max_k2, x=y, alpha=alpha, beta=beta, &
                      ans=P2, nrow=indx(1), ncol=indx(2))

  CALL JacobiGradientEvalAll_(n=max_k2, x=y, alpha=alpha, beta=beta, &
                              ans=dP2, nrow=indx(1), ncol=indx(2))

  areal = REAL(k1, DFP)

  DO k2 = 0, max_k2
    cnt = cnt + 1

    DO CONCURRENT(ii=1:tsize1)
      ans(ii, cnt, 1) = dP1(ii, k1 + 1) * avec(ii) * P2(ii, k2 + 1)
      ans(ii, cnt, 2) = P1(ii, k1 + 1) * bvec(ii) * &
                  (x(ii) * dP2(ii, k2 + 1) - 0.5_DFP * areal * P2(ii, k2 + 1))
    END DO

  END DO

END DO
END PROCEDURE DubinerGradient_Quadrangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Quadrangle2
INTEGER(I4B) :: nrow, ncol
CALL Dubiner_Quadrangle2_(x=x, y=y, order=order, ans=ans, nrow=nrow, &
                          ncol=ncol)
END PROCEDURE Dubiner_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Quadrangle2_
REAL(DFP) :: xij(2, SIZE(x) * SIZE(y))
INTEGER(I4B) :: ii, jj, cnt

xij = 0.0_DFP
cnt = 0
DO ii = 1, SIZE(x)
  DO jj = 1, SIZE(y)
    cnt = cnt + 1
    xij(1, cnt) = x(ii)
    xij(2, cnt) = y(jj)
  END DO
END DO
CALL Dubiner_Quadrangle1_(order=order, xij=xij, ans=ans, nrow=nrow, &
                          ncol=ncol)
END PROCEDURE Dubiner_Quadrangle2_

!----------------------------------------------------------------------------
!                                              TensorProdOrthoPol_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle1
INTEGER(I4B) :: nrow, ncol
CALL TensorProdBasis_Quadrangle1_(p=p, q=q, xij=xij, ans=ans, nrow=nrow, &
                    ncol=ncol, basisType1=basisType1, basisType2=basisType2, &
                 alpha1=alpha1, beta1=beta1, lambda1=lambda1, alpha2=alpha2, &
                                  beta2=beta2, lambda2=lambda2)
END PROCEDURE TensorProdBasis_Quadrangle1

!----------------------------------------------------------------------------
!                                                 TensorProdBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle1_
REAL(DFP) :: P1(SIZE(xij, 2), p + 1), Q1(SIZE(xij, 2), q + 1)
INTEGER(I4B) :: k1, k2, ii

nrow = SIZE(xij, 2)
ncol = (p + 1) * (q + 1)

CALL BasisEvalAll_Line_(order=p, x=xij(1, :), refLine="BIUNIT", &
     basisType=basisType1, alpha=alpha1, beta=beta1, lambda=lambda1, ans=P1, &
                        nrow=k1, ncol=k2)

CALL BasisEvalAll_Line_(order=q, x=xij(2, :), refLine="BIUNIT", &
     basisType=basisType1, alpha=alpha2, beta=beta2, lambda=lambda2, ans=Q1, &
                        nrow=k1, ncol=k2)

DO CONCURRENT(k1=1:p + 1, k2=1:q + 1, ii=1:nrow)
  ans(ii, (k2 - 1) * (p + 1) + k1) = P1(ii, k1) * Q1(ii, k2)
END DO

END PROCEDURE TensorProdBasis_Quadrangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle2
INTEGER(I4B) :: nrow, ncol
CALL TensorProdBasis_Quadrangle2_(p=p, q=q, x=x, y=y, ans=ans, nrow=nrow, &
                    ncol=ncol, basisType1=basisType1, basisType2=basisType2, &
                 alpha1=alpha1, beta1=beta1, lambda1=lambda1, alpha2=alpha2, &
                                  beta2=beta2, lambda2=lambda2)
END PROCEDURE TensorProdBasis_Quadrangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasis_Quadrangle2_
REAL(DFP) :: xij(2, SIZE(x) * SIZE(y))
INTEGER(I4B) :: ii, jj

nrow = SIZE(x)
ncol = SIZE(y)

DO CONCURRENT(ii=1:nrow, jj=1:ncol)
  xij(1, ncol * (ii - 1) + jj) = x(ii)
  xij(2, ncol * (ii - 1) + jj) = y(jj)
END DO

CALL TensorProdBasis_Quadrangle1_(p=p, q=q, xij=xij, basisType1=basisType1, &
           basisType2=basisType2, alpha1=alpha1, alpha2=alpha2, beta1=beta1, &
                              beta2=beta2, lambda1=lambda1, lambda2=lambda2, &
                                  ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE TensorProdBasis_Quadrangle2_

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

CALL VerticalEdgeBasis_Quadrangle2_(qe1=qe1, qe2=qe2, L1=L1, L2=L2, ans=ans, &
                     nrow=nrow, ncol=ncol, qe1Orient=orient, qe2Orient=orient)

DEALLOCATE (L2, L1)

END PROCEDURE VerticalEdgeBasis_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE VerticalEdgeBasis_Quadrangle2_(qe1, qe2, L1, L2, &
                                        ans, nrow, ncol, qe1Orient, qe2Orient)
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
  !! orientation fo left and write vertical edge
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

PURE SUBROUTINE VerticalEdgeBasisGradient_Quadrangle2_(qe1, qe2, &
                L1, L2, dL1, dL2, ans, dim1, dim2, dim3, qe1Orient, qe2Orient)
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

PURE SUBROUTINE HorizontalEdgeBasis_Quadrangle2_(pe3, pe4, L1, L2, &
                                        ans, nrow, ncol, pe3Orient, pe4Orient)
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

PURE SUBROUTINE HorizontalEdgeBasisGradient_Quadrangle2_(pe3, pe4, &
                L1, L2, dL1, dL2, ans, dim1, dim2, dim3, pe3Orient, pe4Orient)
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
CALL HeirarchicalBasis_Quadrangle1_(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                     qe1=qe1, qe2=qe2, xij=xij, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle1_
INTEGER(I4B), PARAMETER :: orient = 1, faceOrient(2) = [1, 1]
CALL HeirarchicalBasis_Quadrangle3_(pb=pb, qb=qb, pe3=pe3, pe4=pe4, qe1=qe1, &
                       qe2=qe2, xij=xij, pe3Orient=orient, pe4Orient=orient, &
         qe1Orient=orient, qe2Orient=orient, faceOrient=faceOrient, ans=ans, &
                                    nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Quadrangle1_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle2
INTEGER(I4B) :: nrow, ncol

CALL HeirarchicalBasis_Quadrangle1_(pb=p, pe3=p, pe4=p, &
                   qb=q, qe1=q, qe2=q, xij=xij, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle2_
CALL HeirarchicalBasis_Quadrangle1_(pb=p, pe3=p, pe4=p, &
                   qb=q, qe1=q, qe2=q, xij=xij, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Quadrangle2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle3
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(xij, 2)
ncol = pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1

ALLOCATE (ans(1:nrow, 1:ncol))

CALL HeirarchicalBasis_Quadrangle3_(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
        qe1=qe1, qe2=qe2, xij=xij, pe3Orient=pe3Orient, pe4Orient=pe4Orient, &
            qe1Orient=qe1Orient, qe2Orient=qe2Orient, faceOrient=faceOrient, &
                                    ans=ans, nrow=nrow, ncol=ncol)

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
CALL VertexBasis_Quadrangle3_(L1=L1, L2=L2, ans=ans, nrow=indx(1), ncol=indx(2))

ncol = indx(2)

! Edge basis function
isok = (qe1 .GE. 2_I4B) .OR. (qe2 .GE. 2_I4B)
IF (isok) THEN
  CALL VerticalEdgeBasis_Quadrangle2_(qe1=qe1, qe2=qe2, L1=L1, L2=L2, &
     ans=ans(:, ncol + 1:), nrow=indx(1), ncol=indx(2), qe1Orient=qe1Orient, &
                                      qe2Orient=qe2Orient)

  ncol = ncol + indx(2)
END IF

! Edge basis function
isok = (pe3 .GE. 2_I4B) .OR. (pe4 .GE. 2_I4B)
IF (isok) THEN
  CALL HorizontalEdgeBasis_Quadrangle2_(pe3=pe3, pe4=pe4, L1=L1, L2=L2, &
     ans=ans(:, ncol + 1:), nrow=indx(1), ncol=indx(2), pe3Orient=pe3Orient, &
                                        pe4Orient=pe4Orient)
  ncol = ncol + indx(2)
END IF

! Cell basis function
isok = (pb .GE. 2_I4B) .OR. (qb .GE. 2_I4B)
IF (isok) THEN
  CALL CellBasis_Quadrangle2_(pb=pb, qb=qb, L1=L1, L2=L2, &
     ans=ans(:, ncol + 1:), nrow=indx(1), ncol=indx(2), faceOrient=faceOrient)
  ncol = ncol + indx(2)
END IF

DEALLOCATE (L1, L2)

END PROCEDURE HeirarchicalBasis_Quadrangle3_

!----------------------------------------------------------------------------
!                                                LagrangeEvallAll_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle1
INTEGER(I4B) :: tsize
CALL LagrangeEvalAll_Quadrangle1_(order=order, x=x, xij=xij, &
                     ans=ans, tsize=tsize, coeff=coeff, firstCall=firstCall, &
                   basisType=basisType, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE LagrangeEvalAll_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle1_
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, degree(SIZE(xij, 2), 2), indx(2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2)), &
             x21(2, 1)

tsize = SIZE(xij, 2)

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN

  IF (firstCall0) THEN
    CALL LagrangeCoeff_Quadrangle_(order=order, xij=xij, &
                                   basisType=basisType0, alpha=alpha, &
                                   beta=beta, lambda=lambda, &
                                   ans=coeff, nrow=indx(1), ncol=indx(2))
  END IF

  ! coeff0 = TRANSPOSE(coeff)
  coeff0(1:tsize, 1:tsize) = coeff(1:tsize, 1:tsize)

ELSE

  CALL LagrangeCoeff_Quadrangle_(order=order, xij=xij, &
                basisType=basisType0, alpha=alpha, beta=beta, lambda=lambda, &
                                 ans=coeff0, nrow=indx(1), ncol=indx(2))

  ! coeff0 = TRANSPOSE(coeff0)

END IF

SELECT CASE (basisType0)

CASE (Monomial)

  CALL LagrangeDegree_Quadrangle_(order=order, ans=degree, nrow=indx(1), &
                                  ncol=indx(2))
#ifdef DEBUG_VER

  IF (tsize .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(msg="tdof is not same as size(degree,1)", &
                  routine="LagrangeEvalAll_Quadrangle1", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

#endif

  DO ii = 1, tsize
    indx(1:2) = degree(ii, 1:2)
    xx(1, ii) = x(1)**indx(1) * x(2)**indx(2)
  END DO

CASE (Heirarchical)

  ! xx = HeirarchicalBasis_Quadrangle( &
  x21(1:2, 1) = x(1:2)
  CALL HeirarchicalBasis_Quadrangle_(p=order, q=order, &
                                  xij=x21, ans=xx, nrow=indx(1), ncol=indx(2))

CASE DEFAULT

  x21(1:2, 1) = x(1:2)
  CALL TensorProdBasis_Quadrangle_(p=order, q=order, xij=x21, &
     basisType1=basisType0, basisType2=basisType0, alpha1=alpha, beta1=beta, &
           lambda1=lambda, alpha2=alpha, beta2=beta, lambda2=lambda, ans=xx, &
                                   nrow=indx(1), ncol=indx(2))

END SELECT

DO CONCURRENT(ii=1:tsize)
  ans(ii) = DOT_PRODUCT(coeff0(:, ii), xx(1, :))
END DO

END PROCEDURE LagrangeEvalAll_Quadrangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle2
INTEGER(I4B) :: nrow, ncol
CALL LagrangeEvalAll_Quadrangle2_(order=order, x=x, xij=xij, ans=ans, &
                     nrow=nrow, ncol=ncol, coeff=coeff, firstCall=firstCall, &
                   basisType=basisType, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE LagrangeEvalAll_Quadrangle2

!----------------------------------------------------------------------------
!                                               LagrangeEvalAll_Quadrangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Quadrangle2_
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, jj, basisType0, indx(2), degree(SIZE(xij, 2), 2)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)) ,xx(SIZE(x, 2), SIZE(xij, 2)), &
             aval

nrow = SIZE(x, 2)
ncol = SIZE(xij, 2)

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN

  IF (firstCall0) THEN

    ! coeff = LagrangeCoeff_Quadrangle(&
    CALL LagrangeCoeff_Quadrangle_(order=order, xij=xij, &
                basisType=basisType0, alpha=alpha, beta=beta, lambda=lambda, &
                                   ans=coeff, nrow=indx(1), ncol=indx(2))
  END IF

  coeff0(1:ncol, 1:ncol) = coeff(1:ncol, 1:ncol)

ELSE

  ! coeff0 = LagrangeCoeff_Quadrangle(&
  CALL LagrangeCoeff_Quadrangle_(order=order, xij=xij, &
    basisType=basisType0, alpha=alpha, beta=beta, lambda=lambda, ans=coeff0, &
                                 nrow=indx(1), ncol=indx(2))

END IF

SELECT CASE (basisType0)

CASE (Monomial)

  ! degree = LagrangeDegree_Quadrangle(order=order)
  CALL LagrangeDegree_Quadrangle_(order=order, ans=degree, nrow=indx(1), &
                                  ncol=indx(2))

#ifdef DEBUG_VER
  IF (ncol .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(msg="tdof is not same as size(degree,1)", &
        routine="LagrangeEvalAll_Quadrangle1", file=__FILE__, line=__LINE__, &
                  unitno=stderr)
    RETURN
  END IF
#endif

  DO ii = 1, ncol
    indx(1:2) = degree(ii, 1:2)
    DO jj = 1, nrow
      aval = x(1, jj)**indx(1) * x(2, jj)**indx(2)
      xx(jj, ii) = aval
    END DO
  END DO

CASE (Heirarchical)

  ! xx = HeirarchicalBasis_Quadrangle( &
  CALL HeirarchicalBasis_Quadrangle_(p=order, q=order, xij=x, ans=xx, &
                                     nrow=indx(1), ncol=indx(2))

CASE DEFAULT

  ! xx = TensorProdBasis_Quadrangle( &
  CALL TensorProdBasis_Quadrangle_(p=order, q=order, xij=x, &
     basisType1=basisType0, basisType2=basisType0, alpha1=alpha, beta1=beta, &
           lambda1=lambda, alpha2=alpha, beta2=beta, lambda2=lambda, ans=xx, &
                                   nrow=indx(1), ncol=indx(2))

END SELECT

! ans = MATMUL(xx, coeff0)
CALL GEMM(C=ans(1:nrow, 1:ncol), alpha=1.0_DFP, A=xx, B=coeff0)

END PROCEDURE LagrangeEvalAll_Quadrangle2_

!----------------------------------------------------------------------------
!                                              QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle1
INTEGER(I4B) :: nips(1), nrow, ncol

nips(1) = QuadratureNumber_Line(order=order, quadType=quadType)

IF (PRESENT(xij)) THEN
  nrow = MAX(SIZE(xij, 1), 2)
ELSE
  nrow = 2
END IF

nrow = nrow + 1
ncol = nips(1) * nips(1)

ALLOCATE (ans(1:nrow, 1:ncol))

CALL QuadraturePoint_Quadrangle1_(nipsx=nips, nipsy=nips, &
        quadType1=quadType, quadType2=quadType, refQuadrangle=refQuadrangle, &
            xij=xij, alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, &
                    beta2=beta, lambda2=lambda, ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE QuadraturePoint_Quadrangle1

!----------------------------------------------------------------------------
!                                             QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle2
INTEGER(I4B) :: nipsx(1), nipsy(1), nrow, ncol

nipsx(1) = QuadratureNumber_Line(order=p, quadType=quadType1)
nipsy(1) = QuadratureNumber_Line(order=q, quadType=quadType2)

IF (PRESENT(xij)) THEN
  nrow = MAX(SIZE(xij, 1), 2)
ELSE
  nrow = 2
END IF

nrow = nrow + 1
ncol = nipsx(1) * nipsy(1)

ALLOCATE (ans(1:nrow, 1:ncol))

CALL QuadraturePoint_Quadrangle1_(nipsx=nipsx, nipsy=nipsy, &
      quadType1=quadType1, quadType2=quadType2, refQuadrangle=refQuadrangle, &
        xij=xij, alpha1=alpha1, beta1=beta1, lambda1=lambda1, alpha2=alpha2, &
                  beta2=beta2, lambda2=lambda2, ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE QuadraturePoint_Quadrangle2

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle3
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = MAX(SIZE(xij, 1), 2)
ELSE
  nrow = 2
END IF

nrow = nrow + 1
ncol = nips(1) * nips(1)

ALLOCATE (ans(1:nrow, 1:ncol))

CALL QuadraturePoint_Quadrangle1_(nipsx=nips, nipsy=nips, &
        quadType1=quadType, quadType2=quadType, refQuadrangle=refQuadrangle, &
            xij=xij, alpha1=alpha, beta1=beta, lambda1=lambda, alpha2=alpha, &
                    beta2=beta, lambda2=lambda, ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE QuadraturePoint_Quadrangle3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle4
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = MAX(SIZE(xij, 1), 2)
ELSE
  nrow = 2
END IF

nrow = nrow + 1
ncol = nipsx(1) * nipsy(1)

ALLOCATE (ans(1:nrow, 1:ncol))

CALL QuadraturePoint_Quadrangle1_(nipsx=nipsx, nipsy=nipsy, &
      quadType1=quadType1, quadType2=quadType2, refQuadrangle=refQuadrangle, &
        xij=xij, alpha1=alpha1, beta1=beta1, lambda1=lambda1, alpha2=alpha2, &
                  beta2=beta2, lambda2=lambda2, ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE QuadraturePoint_Quadrangle4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Quadrangle1_
! internal variables
REAL(DFP) :: x(4, nipsx(1)), y(2, nipsy(1)), areal
INTEGER(I4B) :: ii, jj, nsd, np, nq
CHARACTER(len=1) :: astr

REAL(DFP), PARAMETER :: x12(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])

IF (PRESENT(xij)) THEN
  nsd = MAX(SIZE(xij, 1), 2)
ELSE
  nsd = 2
END IF

! CALL Reallocate(ans, nsd + 1_I4B, np * nq)
nrow = nsd + 1
ncol = nipsx(1) * nipsy(1)

CALL QuadraturePoint_Line_(nips=nipsx, quadType=quadType1, xij=x12, &
       layout="INCREASING", alpha=alpha1, beta=beta1, lambda=lambda1, ans=x, &
                           nrow=ii, ncol=np)

CALL QuadraturePoint_Line_(nips=nipsy, quadType=quadType2, xij=x12, &
       layout="INCREASING", alpha=alpha2, beta=beta2, lambda=lambda2, ans=y, &
                           nrow=ii, ncol=nq)

DO CONCURRENT(ii=1:np, jj=1:nq)
  ans(1, nq * (ii - 1) + jj) = x(1, ii)
  ans(2, nq * (ii - 1) + jj) = y(1, jj)
  ans(nrow, nq * (ii - 1) + jj) = x(2, ii) * y(2, jj)
END DO

IF (PRESENT(xij)) THEN
  CALL FromBiUnitQuadrangle2Quadrangle_(xin=ans(1:2, :), x1=xij(:, 1), &
          x2=xij(:, 2), x3=xij(:, 3), x4=xij(:, 4), ans=ans, nrow=ii, ncol=jj)

  areal = JacobianQuadrangle(from="BIUNIT", to="QUADRANGLE", xij=xij)

  DO CONCURRENT(ii=1:ncol)
    ans(nrow, ii) = ans(nrow, ii) * areal
  END DO

  RETURN
END IF

astr = UpperCase(refQuadrangle(1:1))
IF (astr .EQ. "U") THEN
  CALL FromBiUnitQuadrangle2UnitQuadrangle_(xin=ans(1:2, :), ans=ans, &
                                            nrow=ii, ncol=jj)

  areal = JacobianQuadrangle(from="BIUNIT", to="UNIT", xij=xij)

  DO CONCURRENT(ii=1:ncol)
    ans(nrow, ii) = ans(nrow, ii) * areal
  END DO

  RETURN
END IF

END PROCEDURE QuadraturePoint_Quadrangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Quadrangle1
INTEGER(I4B) :: dim1, dim2, dim3
CALL LagrangeGradientEvalAll_Quadrangle1_(order=order, x=x, xij=xij, &
                      ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, coeff=coeff, &
           firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
                                          lambda=lambda)
END PROCEDURE LagrangeGradientEvalAll_Quadrangle1

!----------------------------------------------------------------------------
!                                       LagrangeGradientEvalAll_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Quadrangle1_
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, ai, bi, indx(3), degree(SIZE(xij, 2), 2), &
                jj
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), &
             xx(SIZE(x, 2), SIZE(xij, 2), 2), ar, br, areal, breal

dim1 = SIZE(x, 2)
dim2 = SIZE(xij, 2)
dim3 = 2

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN

  IF (firstCall0) THEN

    ! coeff = LagrangeCoeff_Quadrangle(&
    CALL LagrangeCoeff_Quadrangle_(order=order, xij=xij, &
     basisType=basisType0, alpha=alpha, beta=beta, lambda=lambda, ans=coeff, &
                                   nrow=indx(1), ncol=indx(2))
  END IF

  coeff0(1:dim2, 1:dim2) = coeff(1:dim2, 1:dim2)

ELSE

  ! coeff0 = LagrangeCoeff_Quadrangle(&
  CALL LagrangeCoeff_Quadrangle_(order=order, xij=xij, basisType=basisType0, &
            alpha=alpha, beta=beta, lambda=lambda, ans=coeff0, nrow=indx(1), &
                                 ncol=indx(2))
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  ! degree = LagrangeDegree_Quadrangle(order=order)
  CALL LagrangeDegree_Quadrangle_(order=order, ans=degree, nrow=indx(1), &
                                  ncol=indx(2))

#ifdef DEBUG_VER

  IF (dim2 .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(msg="tdof is not same as size(degree,1)", &
                  routine="LagrangeEvalAll_Quadrangle1", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

#endif

  DO ii = 1, dim2
    ai = MAX(degree(ii, 1_I4B) - 1_I4B, 0_I4B)
    bi = MAX(degree(ii, 2_I4B) - 1_I4B, 0_I4B)
    ar = REAL(degree(ii, 1_I4B), DFP)
    br = REAL(degree(ii, 2_I4B), DFP)

    indx(1:2) = degree(ii, 1:2)

    DO jj = 1, dim1
      areal = (ar * x(1, jj)**ai) * x(2, jj)**indx(2)
      breal = x(1, jj)**indx(1) * (br * x(2, jj)**bi)
      xx(jj, ii, 1) = areal
      xx(jj, ii, 2) = breal

    END DO

  END DO

CASE (Heirarchical)

  ! xx = HeirarchicalBasisGradient_Quadrangle( &
  CALL HeirarchicalBasisGradient_Quadrangle_(p=order, q=order, xij=x, &
                             ans=xx, dim1=indx(1), dim2=indx(2), dim3=indx(3))

CASE DEFAULT

  ! xx = OrthogonalBasisGradient_Quadrangle( &
  CALL OrthogonalBasisGradient_Quadrangle_(p=order, q=order, xij=x, &
     basisType1=basisType0, basisType2=basisType0, alpha1=alpha, beta1=beta, &
           lambda1=lambda, alpha2=alpha, beta2=beta, lambda2=lambda, ans=xx, &
                                     dim1=indx(1), dim2=indx(2), dim3=indx(3))

END SELECT

DO ii = 1, 2
  ! ans(:, ii, :) = TRANSPOSE(MATMUL(xx(:, :, ii), coeff0))
  ans(1:dim1, 1:dim2, ii) = MATMUL(xx(1:dim1, 1:dim2, ii), coeff0)
END DO

END PROCEDURE LagrangeGradientEvalAll_Quadrangle1_

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

!----------------------------------------------------------------------------
!                                       TensorProdBasisGradient_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1
INTEGER(I4B) :: dim1, dim2, dim3
CALL TensorProdBasisGradient_Quadrangle1_(p=p, q=q, xij=xij, ans=ans, &
                     dim1=dim1, dim2=dim2, dim3=dim3, basisType1=basisType1, &
         basisType2=basisType2, alpha1=alpha1, beta1=beta1, lambda1=lambda1, &
                                  alpha2=alpha2, beta2=beta2, lambda2=lambda2)
END PROCEDURE TensorProdBasisGradient_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1_
REAL(DFP) :: P1(SIZE(xij, 2), p + 1), Q1(SIZE(xij, 2), q + 1)
REAL(DFP) :: dP1(SIZE(xij, 2), p + 1), dQ1(SIZE(xij, 2), q + 1)
INTEGER(I4B) :: k1, k2, cnt, indx(3)

dim1 = SIZE(xij, 2)
dim2 = (p + 1) * (q + 1)
dim3 = 2

! P1
CALL BasisEvalAll_Line_(order=p, x=xij(1, :), refLine="BIUNIT", &
             basisType=basisType1, alpha=alpha1, beta=beta1, lambda=lambda1, &
                        ans=P1, nrow=indx(1), ncol=indx(2))

! Q1 = BasisEvalAll_Line( &
CALL BasisEvalAll_Line_(order=q, x=xij(2, :), refLine="BIUNIT", &
     basisType=basisType1, alpha=alpha2, beta=beta2, lambda=lambda2, ans=Q1, &
                        nrow=indx(1), ncol=indx(2))

! dP1 = BasisGradientEvalAll_Line( &
CALL BasisGradientEvalAll_Line_(order=p, x=xij(1, :), refLine="BIUNIT", &
    basisType=basisType1, alpha=alpha1, beta=beta1, lambda=lambda1, ans=dP1, &
                                nrow=indx(1), ncol=indx(2))

! dQ1 = BasisGradientEvalAll_Line( &
CALL BasisGradientEvalAll_Line_(order=q, x=xij(2, :), refLine="BIUNIT", &
    basisType=basisType1, alpha=alpha2, beta=beta2, lambda=lambda2, ans=dQ1, &
                                nrow=indx(1), ncol=indx(2))

cnt = 0

DO k2 = 1, q + 1

  DO k1 = 1, p + 1
    cnt = cnt + 1
    ans(1:dim1, cnt, 1) = dP1(1:dim1, k1) * Q1(1:dim1, k2)
    ans(1:dim1, cnt, 2) = P1(1:dim1, k1) * dQ1(1:dim1, k2)
  END DO

END DO

END PROCEDURE TensorProdBasisGradient_Quadrangle1_

!----------------------------------------------------------------------------
!                                               QuadraturePoint_Quadrangle3
!----------------------------------------------------------------------------

END SUBMODULE Methods
