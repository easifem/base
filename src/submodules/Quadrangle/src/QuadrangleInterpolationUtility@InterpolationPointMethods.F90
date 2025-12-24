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

SUBMODULE(QuadrangleInterpolationUtility) InterpolationPointMethods
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line_
USE ReallocateUtility, ONLY: Reallocate
USE MappingUtility, ONLY: FromBiUnitQuadrangle2Quadrangle_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                               EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle1
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

nrow = 2_I4B
isok = PRESENT(xij)
IF (isok) nrow = SIZE(xij, 1)

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
  p=p, q=q, ipType1=TypeInterpolationOpt%equidistance, &
  ipType2=TypeInterpolationOpt%equidistance, ans=ans, &
  nrow=nrow, ncol=ncol, layout="VEFC", xij=xij)
END PROCEDURE EquidistancePoint_Quadrangle2_

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Quadrangle1
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

isok = PRESENT(xij)

IF (isok) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 2
END IF

ncol = LagrangeInDOF_Quadrangle(order=order)

IF (ncol .EQ. 0) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF

ALLOCATE (ans(nrow, ncol))
ans(1:nrow, 1:ncol) = EquidistanceInPoint_Quadrangle2(p=order, q=order, &
                                                      xij=xij)
END PROCEDURE EquidistanceInPoint_Quadrangle1

!----------------------------------------------------------------------------
!                                             EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Quadrangle2
REAL(DFP), ALLOCATABLE :: temp(:, :)
INTEGER(I4B) :: a, b, nrow, ncol
LOGICAL(LGT) :: isok

a = LagrangeDOF_Quadrangle(p=p, q=q)
b = LagrangeInDOF_Quadrangle(p=p, q=q)

isok = PRESENT(xij)
IF (isok) THEN
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
!                                            GetEdgeConnectivityHelpAntiClock
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
!                                                GetEdgeConnectivityHelpClock
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

END SUBMODULE InterpolationPointMethods
