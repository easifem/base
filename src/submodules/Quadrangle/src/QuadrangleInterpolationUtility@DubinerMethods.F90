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

SUBMODULE(QuadrangleInterpolationUtility) DubinerMethods
USE LegendrePolynomialUtility, ONLY: LegendreEvalAll_, &
                                     LegendreGradientEvalAll_
USE JacobiPolynomialUtility, ONLY: JacobiEvalAll_, &
                                   JacobiGradientEvalAll_

IMPLICIT NONE
CONTAINS

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
REAL(DFP) :: P1(SIZE(xij, 2), order + 1), P2(SIZE(xij, 2), order + 1), &
             temp(SIZE(xij, 2), 3)
REAL(DFP) :: alpha, beta
INTEGER(I4B) :: k1, k2, max_k2, cnt, indx(2), ii

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

END SUBMODULE DubinerMethods
