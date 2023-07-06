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

SUBMODULE(QuadrangleInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        RefQuadrangleCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE RefQuadrangleCoord
TYPE(String) :: astr
astr = UpperCase(refQuadrangle)
SELECT CASE (astr%chars())
CASE ("UNIT")
  ans(1, :) = [0.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP]
  ans(2, :) = [0.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP]
CASE ("BIUNIT")
  ans(1, :) = [-1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP]
  ans(2, :) = [-1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP]
END SELECT
END PROCEDURE RefQuadrangleCoord

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle
INTEGER(I4B) :: n, ii, jj, kk
  !!
n = LagrangeDOF_Quadrangle(order=order)
ALLOCATE (ans(n, 2))
  !!
kk = 0
  !!
DO jj = 0, order
  DO ii = 0, order
    kk = kk + 1
    ans(kk, 1) = ii
    ans(kk, 2) = jj
  END DO
END DO
END PROCEDURE LagrangeDegree_Quadrangle

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Quadrangle
ans = (order + 1)**2
END PROCEDURE LagrangeDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Quadrangle
ans = (order - 1)**2
END PROCEDURE LagrangeInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                               EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle
INTEGER(I4B) :: nsd, n, ne, i1, i2
REAL(DFP) :: x(3, 4), xin(3, 4), e1(3), e2(3), lam, avar, mu
  !!
x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP
  !!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:4) = xij(1:nsd, 1:4)
ELSE
  nsd = 3_I4B
  x(1:nsd, 1) = [-1.0, -1.0, 0.0]
  x(1:nsd, 2) = [1.0, -1.0, 0.0]
  x(1:nsd, 3) = [1.0, 1.0, 0.0]
  x(1:nsd, 4) = [-1.0, 1.0, 0.0]
END IF
  !!
n = LagrangeDOF_Quadrangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP
  !!
  !! points on vertex
  !!
ans(1:nsd, 1:4) = x(1:nsd, 1:4)
  !!
  !! points on edge
  !!
ne = LagrangeInDOF_Line(order=order)
  !!
i2 = 4
IF (order .GT. 1_I4B) THEN
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [1, 2]))
    !!
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [2, 3]))
    !!
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [3, 4]))
    !!
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [4, 1]))
    !!
END IF
  !!
  !! points on face
  !!
IF (order .GT. 1_I4B) THEN
    !!
  IF (order .EQ. 2_I4B) THEN
    i1 = i2 + 1
    ans(1:nsd, i1) = SUM(x(1:nsd, :), dim=2_I4B) / 4.0_DFP
  ELSE
      !!
    e1 = x(:, 2) - x(:, 1)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 4) - x(:, 1)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 1) = x(1:nsd, 1) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    e1 = x(:, 3) - x(:, 2)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 1) - x(:, 2)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 2) = x(1:nsd, 2) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    e1 = x(:, 2) - x(:, 3)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 4) - x(:, 3)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 3) = x(1:nsd, 3) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    e1 = x(:, 3) - x(:, 4)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 1) - x(:, 4)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 4) = x(1:nsd, 4) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    i1 = i2 + 1
    ans(1:nsd, i1:) = EquidistancePoint_Quadrangle( &
      & order=order - 2, &
      & xij=xin(1:nsd, 1:4))
      !!
  END IF
END IF
  !!
END PROCEDURE EquidistancePoint_Quadrangle

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Quadrangle
INTEGER(I4B) :: nsd, n, ne, i1, i2
REAL(DFP) :: x(3, 4), xin(3, 4), e1(3), e2(3), lam, avar, mu
!!
IF (order .LT. 2_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF
  !!
x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP
  !!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:4) = xij(1:nsd, 1:4)
ELSE
  nsd = 3_I4B
  x(1:nsd, 1) = [-1.0, -1.0, 0.0]
  x(1:nsd, 2) = [1.0, -1.0, 0.0]
  x(1:nsd, 3) = [1.0, 1.0, 0.0]
  x(1:nsd, 4) = [-1.0, 1.0, 0.0]
END IF
  !!
n = LagrangeInDOF_Quadrangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP
  !!
  !! points on face
  !!
IF (order .EQ. 2_I4B) THEN
  ans(1:nsd, 1) = SUM(x, dim=2_I4B) / 4.0_DFP
ELSE
    !!
  e1 = x(:, 2) - x(:, 1)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 4) - x(:, 1)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 1) = x(1:nsd, 1) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  e1 = x(:, 3) - x(:, 2)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 1) - x(:, 2)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 2) = x(1:nsd, 2) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  e1 = x(:, 2) - x(:, 3)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 4) - x(:, 3)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 3) = x(1:nsd, 3) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  e1 = x(:, 3) - x(:, 4)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 1) - x(:, 4)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 4) = x(1:nsd, 4) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  ans(1:nsd, 1:) = EquidistancePoint_Quadrangle( &
    & order=order - 2, &
    & xij=xin(1:nsd, 1:4))
    !!
END IF
!!
END PROCEDURE EquidistanceInPoint_Quadrangle

!----------------------------------------------------------------------------
!                                                         GLL_IP_Quadrangle
!----------------------------------------------------------------------------

FUNCTION IP_Quadrangle(p, q, ipType1, ipType2, xij) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: p
  INTEGER(I4B), INTENT(IN) :: q
  INTEGER(I4B), INTENT(IN) :: ipType1
  INTEGER(I4B), INTENT(IN) :: ipType2
  REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  REAL(DFP), ALLOCATABLE :: ans(:, :)
  !!
  !! internal variables
  !!
  REAL(DFP) :: x(p + 1), y(q + 1), &
    & xi(p + 1, q + 1), eta(p + 1, q + 1)
  REAL(DFP), ALLOCATABLE :: temp(:, :)
  INTEGER(I4B) :: ii, jj, kk, nsd
  CHARACTER(LEN=*), PARAMETER :: myName = "IP_Quadrangle"
  !!
  x = InterpolationPoint_Line(order=p, ipType=ipType1, &
    & xij=[-1.0_DFP, 1.0_DFP], &
    & layout="INCREASING")
  !!
  y = InterpolationPoint_Line(order=q, ipType=ipType2, &
    & xij=[-1.0_DFP, 1.0_DFP], &
    & layout="INCREASING")
  !!
  IF (PRESENT(xij)) THEN
    nsd = SIZE(xij, 1)
  ELSE
    nsd = 2
  END IF
  !!
  CALL Reallocate(ans, nsd, (p + 1) * (q + 1))
  CALL Reallocate(temp, 2, (p + 1) * (q + 1))
  !!
  xi = 0.0_DFP
  eta = 0.0_DFP
  !!
  DO ii = 1, p + 1
    DO jj = 1, q + 1
      xi(ii, jj) = x(ii)
      eta(ii, jj) = y(jj)
    END DO
  END DO
  !!
  CALL IJ2VEFC(xi=xi, eta=eta, temp=temp, p=p, q=q, myname=myname)
  !!
  IF (PRESENT(xij)) THEN
    ans = FromBiUnitQuadrangle2Quadrangle(xin=temp, x1=xij(:, 1), &
      & x2=xij(:, 2), x3=xij(:, 3), x4=xij(:, 4))
  ELSE
    ans = temp
  END IF
END FUNCTION IP_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE IJ2VEFC(xi, eta, temp, p, q, myname)
  REAL(DFP), INTENT(IN) :: xi(:, :)
  REAL(DFP), INTENT(IN) :: eta(:, :)
  REAL(DFP), INTENT(OUT) :: temp(:, :)
  INTEGER(I4B), INTENT(IN) :: p
  INTEGER(I4B), INTENT(IN) :: q
  CHARACTER(LEN=*), INTENT(IN) :: myname
  !!
  INTEGER(I4B) :: cnt, m, ii, jj, kk, ll, llt, llr, N
  !!
  !! vertices
  !!
  N = (p + 1) * (q + 1)
  cnt = 0
  ll = -1
  !!
  DO
    ll = ll + 1
    !!
    !! v1
    !!
    cnt = cnt + 1
    ii = 1 + ll; jj = 1 + ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
    !!
    !! v2
    !!
    ii = p + 1 - ll
    jj = 1 + ll
    IF (cnt .LT. N) THEN
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END IF
    !!
    !! v3
    !!
    ii = p + 1 - ll
    jj = q + 1 - ll
    IF (cnt .LT. N) THEN
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END IF
    !!
    !! v4
    !!
    ii = 1 + ll
    jj = q + 1 - ll
    IF (cnt .LT. N) THEN
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
    END IF
    !!
    !! nodes on edge 12
    !!
    jj = ll + 1
    IF (cnt .LT. N) THEN
      DO ii = 2 + ll, p - ll
        cnt = cnt + 1
        temp(1, cnt) = xi(ii, jj)
        temp(2, cnt) = eta(ii, jj)
      END DO
    END IF
    !!
    !! nodes on edge 23
    !!
    ii = p + 1 - ll
    IF (cnt .LT. N) THEN
      DO jj = 2 + ll, q - ll
        cnt = cnt + 1
        temp(1, cnt) = xi(ii, jj)
        temp(2, cnt) = eta(ii, jj)
      END DO
    END IF
    !!
    !! nodes on edge 34
    !!
    jj = q + 1 - ll
    IF (cnt .LT. N) THEN
      DO ii = p - ll, 2 + ll, -1
        cnt = cnt + 1
        temp(1, cnt) = xi(ii, jj)
        temp(2, cnt) = eta(ii, jj)
      END DO
    END IF
    !!
    !! nodes on edge 41
    !!
    ii = ll + 1
    IF (cnt .LT. N) THEN
      DO jj = q - ll, 2 + ll, -1
        cnt = cnt + 1
        temp(1, cnt) = xi(ii, jj)
        temp(2, cnt) = eta(ii, jj)
      END DO
    END IF
    !!
    !! internal nodes
    !!
    IF (cnt .EQ. N) EXIT
  END DO
  !!
END SUBROUTINE IJ2VEFC

!----------------------------------------------------------------------------
!                                              InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle1
CHARACTER(LEN=*), PARAMETER :: myName = "InterpolationPoint_Quadrangle1"
!!
SELECT CASE (ipType)
CASE (Equidistance)
  nodecoord = EquidistancePoint_Quadrangle(xij=xij, order=order)
CASE (GaussLegendreLobatto)
  nodecoord = IP_Quadrangle(p=order, q=order, &
    & ipType1=GaussLegendreLobatto, ipType2=GaussLegendreLobatto, &
    & xij=xij)
CASE (GaussChebyshevLobatto)
  nodecoord = IP_Quadrangle(p=order, q=order, &
    & ipType1=GaussChebyshevLobatto, ipType2=GaussChebyshevLobatto, &
    & xij=xij)
CASE DEFAULT
  CALL ErrorMsg(msg="Unknown interpolation point type (ipType)", &
    & file=__FILE__, &
    & routine=myname, &
    & line=__LINE__, &
    & unitno=stderr)
END SELECT
!!
END PROCEDURE InterpolationPoint_Quadrangle1

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info
!!
ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Quadrangle)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans, IPIV=ipiv, info=info)
!!
END PROCEDURE LagrangeCoeff_Quadrangle1

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle2
!!
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info
!!
vtemp = v; ans = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle2

!----------------------------------------------------------------------------
!                                                     LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle3
INTEGER(I4B) :: info
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Quadrangle3

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Quadrangle4
ans = LagrangeVandermonde(order=order, xij=xij, elemType=Quadrangle)
CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Quadrangle4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Quadrangle1
REAL(DFP) :: P1(SIZE(xij, 2), order + 1), P2(SIZE(xij, 2), order + 1)
REAL(DFP) :: x(SIZE(xij, 2)), y(SIZE(xij, 2))
REAL(DFP) :: avec(SIZE(xij, 2)), alpha, beta
INTEGER(I4B) :: k1, k2, max_k2, cnt
!!
x = xij(1, :)
y = xij(2, :)
!!
P1 = LegendreEvalAll(n=order, x=x)
!!
!! we do not need x now, so let store (1-y)/2 in x
!!
x = 0.5_DFP * (1.0_DFP - y)
!!
alpha = 0.0_DFP
beta = 0.0_DFP
cnt = 0
!!
DO k1 = 0, order
  !!
  avec = (x)**k1 !! note here x = 0.5_DFP*(1-y)
  alpha = 2.0_DFP * k1 + 1.0_DFP
  !!
  max_k2 = order - k1
  !!
  P2(:, 1:max_k2 + 1) = JacobiEvalAll(n=max_k2, x=y, alpha=alpha, beta=beta)
  !!
  DO k2 = 0, max_k2
    cnt = cnt + 1
    ans(:, cnt) = P1(:, k1 + 1) * avec * P2(:, k2 + 1)
  END DO
  !!
END DO

END PROCEDURE Dubiner_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Quadrangle2
REAL(DFP) :: xij(2, SIZE(x) * SIZE(y))
INTEGER(I4B) :: ii, jj, cnt
!!
xij = 0.0_DFP
cnt = 0
DO ii = 1, SIZE(x)
  DO jj = 1, SIZE(y)
    cnt = cnt + 1
    xij(1, cnt) = x(ii)
    xij(2, cnt) = y(jj)
  END DO
END DO
!!
ans = Dubiner_Quadrangle1(order=order, xij=xij)
!!
END PROCEDURE Dubiner_Quadrangle2

!----------------------------------------------------------------------------
!                                                TensorProdOrthoPol_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdOrthoPol_Quadrangle1
REAL(DFP) :: x(SIZE(xij, 2)), y(SIZE(xij, 2))
REAL(DFP) :: P1(SIZE(xij, 2), p + 1), Q1(SIZE(xij, 2), q + 1)
INTEGER(I4B) :: ii, k1, k2, cnt
!!
x = xij(1, :)
y = xij(2, :)
!!
P1 = EvalAllOrthopol(n=p, x=x, orthopol=orthopol1, &
  & alpha=alpha1, beta=beta1, lambda=lambda1)
Q1 = EvalAllOrthopol(n=q, x=y, orthopol=orthopol2, &
  & alpha=alpha2, beta=beta2, lambda=lambda2)
!!
cnt = 0
!!
DO k1 = 1, p + 1
  DO k2 = 1, q + 1
    cnt = cnt + 1
    ans(:, cnt) = P1(:, k1) * Q1(:, k2)
  END DO
END DO
!!
END PROCEDURE TensorProdOrthoPol_Quadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorProdOrthoPol_Quadrangle2
REAL(DFP) :: xij(2, SIZE(x) * SIZE(y))
INTEGER(I4B) :: ii, jj, cnt
!!
xij = 0.0_DFP
cnt = 0
DO ii = 1, SIZE(x)
  DO jj = 1, SIZE(y)
    cnt = cnt + 1
    xij(1, cnt) = x(ii)
    xij(2, cnt) = y(jj)
  END DO
END DO
!!
ans = TensorProdOrthopol_Quadrangle1( &
  & p=p, q=q, xij=xij, orthopol1=orthopol1, orthopol2=orthopol2, &
  & alpha1=alpha1, beta1=beta1, beta2=beta2, lambda1=lambda1, &
  & lambda2=lambda2)
!!
END PROCEDURE TensorProdOrthoPol_Quadrangle2

!----------------------------------------------------------------------------
!                                                     VertexBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle
ans(:, 1) = 0.25_DFP * (1.0_DFP - x) * (1.0_DFP - y)
ans(:, 2) = 0.25_DFP * (1.0_DFP + x) * (1.0_DFP - y)
ans(:, 3) = 0.25_DFP * (1.0_DFP + x) * (1.0_DFP + y)
ans(:, 4) = 0.25_DFP * (1.0_DFP - x) * (1.0_DFP + y)
END PROCEDURE VertexBasis_Quadrangle

!----------------------------------------------------------------------------
!                                                    VertexBasis_Quadrangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Quadrangle2
ans(:, 1) = L1(:, 0) * L2(:, 0)
ans(:, 2) = L1(:, 1) * L2(:, 0)
ans(:, 3) = L1(:, 1) * L2(:, 1)
ans(:, 4) = L1(:, 0) * L2(:, 1)
END PROCEDURE VertexBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                               VerticalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VerticalEdgeBasis_Quadrangle
REAL(DFP) :: L2(1:SIZE(y), 0:MAX(qe1, qe2))
INTEGER(I4B) :: maxQ, k2, cnt
!!
maxQ = MAX(qe1, qe2)
!!
L2 = LobattoEvalAll(n=maxQ, x=y)
!!
cnt = 0
!!
DO k2 = 2, qe1
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * (1.0_DFP - x) * L2(:, k2)
END DO
!!
DO k2 = 2, qe2
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * (1.0_DFP + x) * L2(:, k2)
END DO
!!
END PROCEDURE VerticalEdgeBasis_Quadrangle

!----------------------------------------------------------------------------
!                                             VerticalEdgeBasis_Quadrangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE VerticalEdgeBasis_Quadrangle2
INTEGER(I4B) :: k2, cnt
!!
cnt = 0
!!
DO k2 = 2, qe1
  cnt = cnt + 1
  ans(:, cnt) = L1(:, 0) * L2(:, k2)
END DO
!!
DO k2 = 2, qe2
  cnt = cnt + 1
  ans(:, cnt) = L1(:, 1) * L2(:, k2)
END DO
!!
END PROCEDURE VerticalEdgeBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                             HorizontalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HorizontalEdgeBasis_Quadrangle
REAL(DFP) :: L1(1:SIZE(x), 0:MAX(pe3, pe4))
INTEGER(I4B) :: maxP, k1, cnt
!!
maxP = MAX(pe3, pe4)
!!
L1 = LobattoEvalAll(n=maxP, x=x)
!!
cnt = 0
!!
DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * (1.0_DFP - y) * L1(:, k1)
END DO
!!
DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = 0.25_DFP * (1.0_DFP + y) * L1(:, k1)
END DO
!!
END PROCEDURE HorizontalEdgeBasis_Quadrangle

!----------------------------------------------------------------------------
!                                             HorizontalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HorizontalEdgeBasis_Quadrangle2
INTEGER(I4B) :: k1, cnt
!!
cnt = 0
!!
DO k1 = 2, pe3
  cnt = cnt + 1
  ans(:, cnt) = L1(:, k1) * L2(:, 0)
END DO
!!
DO k1 = 2, pe4
  cnt = cnt + 1
  ans(:, cnt) = L1(:, k1) * L2(:, 1)
END DO
!!
END PROCEDURE HorizontalEdgeBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                                      CellBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Quadrangle
REAL(DFP) :: L1(1:SIZE(x), 0:pb)
REAL(DFP) :: L2(1:SIZE(y), 0:qb)
INTEGER(I4B) :: k1, k2, cnt
!!
L1 = LobattoEvalAll(n=pb, x=x)
L2 = LobattoEvalAll(n=qb, x=y)
!!
cnt = 0
!!
DO k1 = 2, pb
  DO k2 = 2, qb
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L2(:, k2)
  END DO
END DO
!!
END PROCEDURE CellBasis_Quadrangle

!----------------------------------------------------------------------------
!                                                      CellBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Quadrangle2
INTEGER(I4B) :: k1, k2, cnt
!!
cnt = 0
!!
DO k1 = 2, pb
  DO k2 = 2, qb
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1) * L2(:, k2)
  END DO
END DO
!!
END PROCEDURE CellBasis_Quadrangle2

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle1
INTEGER(I4B) :: a, b, maxP, maxQ
REAL(DFP) :: L1(1:SIZE(xij, 2), 0:MAX(pe3, pe4, pb))
REAL(DFP) :: L2(1:SIZE(xij, 2), 0:MAX(qe1, qe2, qb))
!!
maxP = MAX(pe3, pe4, pb)
maxQ = MAX(qe1, qe2, qb)
!!
L1 = LobattoEvalAll(n=maxP, x=xij(1, :))
L2 = LobattoEvalAll(n=maxQ, x=xij(2, :))
!!
!! Vertex basis function
!!
ans(:, 1:4) = VertexBasis_Quadrangle2(L1=L1, L2=L2)
!!
!! Edge basis function
!!
b = 4
!!
IF (qe1 .GE. 2_I4B .OR. qe2 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + qe1 + qe2 - 2 !!4+qe1 + qe2 - 2
  ans(:, a:b) = VerticalEdgeBasis_Quadrangle2( &
    & qe1=qe1, qe2=qe2, L1=L1, L2=L2)
END IF
!!
!! Edge basis function
!!
IF (pe3 .GE. 2_I4B .OR. pe4 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe3 + pe4 - 2 !!4+pe3 + pe4 - 2
  ans(:, a:b) = HorizontalEdgeBasis_Quadrangle2( &
    & pe3=pe3, pe4=pe4, L1=L1, L2=L2)
END IF
!!
!! Cell basis function
!!
IF (pb .GE. 2_I4B .OR. qb .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + (pb - 1) * (qb - 1)
  ans(:, a:b) = CellBasis_Quadrangle2(pb=pb, qb=qb, L1=L1, L2=L2)
END IF
END PROCEDURE HeirarchicalBasis_Quadrangle1

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Quadrangle2
ans = HeirarchicalBasis_Quadrangle1(pb=p, pe3=p, pe4=p, &
  & qb=q, qe1=q, qe2=q, xij=xij)
END PROCEDURE HeirarchicalBasis_Quadrangle2

END SUBMODULE Methods
