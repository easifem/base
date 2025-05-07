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

SUBMODULE(TriangleInterpolationUtility) Methods
USE BaseType, ONLY: ipopt => TypeInterpolationOpt

USE StringUtility, ONLY: UpperCase

USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line, &
                                    EquidistanceInPoint_Line_, &
                                    LagrangeInDOF_Line, &
                                    InterpolationPoint_Line_

USE MappingUtility, ONLY: FromUnitTriangle2Triangle_

USE ErrorHandling, ONLY: Errormsg

USE RecursiveNodesUtility, ONLY: RecursiveNode2D_

USE IntegerUtility, ONLY: Size

USE Display_Method, ONLY: ToString

USE GlobalData, ONLY: stderr

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                      GetTotalDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Triangle
ans = (order + 1) * (order + 2) / 2_I4B
END PROCEDURE GetTotalDOF_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Triangle
ans = (order - 1) * (order - 2) / 2_I4B
END PROCEDURE GetTotalInDOF_Triangle

!----------------------------------------------------------------------------
!                                                   RefElemDomain_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Triangle
CHARACTER(2) :: bc
CHARACTER(3) :: bi

bc = UpperCase(baseContinuity(1:2))
bi = UpperCase(baseInterpol(1:3))

SELECT CASE (bc)

CASE ("H1")

  SELECT CASE (bi)

  !! Lagrange
  CASE ("LAG", "SER", "HER")
    ans = "UNIT"

  CASE ("HIE", "HEI")
    ans = "BIUNIT"

  CASE ("ORT")
    ans = "BIUNIT"

  CASE DEFAULT

    CALL Errormsg( &
      msg="No case found for given baseInterpol="//TRIM(baseInterpol), &
      routine="RefElemDomain_Triangle()", file=__FILE__, line=__LINE__, &
      unitno=stderr)

  END SELECT

CASE DEFAULT

  CALL Errormsg( &
    msg="No case found for given baseContinuity="//TRIM(baseContinuity), &
    file=__FILE__, line=__LINE__, routine="RefElemDomain_Triangle()", &
    unitno=stderr)

END SELECT

END PROCEDURE RefElemDomain_Triangle

!----------------------------------------------------------------------------
!                                                       FacetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Triangle
CHARACTER(3) :: bi

bi = UpperCase(baseInterpol(1:3))

SELECT CASE (bi)
CASE ("HIE", "HEI", "ORT")
  ans(:, 1) = [1, 2]
  ans(:, 2) = [1, 3]
  ans(:, 3) = [2, 3]

CASE DEFAULT
  ans(:, 1) = [1, 2]
  ans(:, 2) = [2, 3]
  ans(:, 3) = [3, 1]

END SELECT
END PROCEDURE FacetConnectivity_Triangle

!----------------------------------------------------------------------------
!                                                 EquidistancePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Triangle
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 2_I4B
END IF

ncol = LagrangeDOF_Triangle(order=order)
ALLOCATE (ans(nrow, ncol))

CALL EquidistancePoint_Triangle_(order=order, xij=xij, ans=ans, nrow=nrow, &
                                 ncol=ncol)

END PROCEDURE EquidistancePoint_Triangle

!----------------------------------------------------------------------------
!                                                 EquidistancePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Triangle_
INTEGER(I4B) :: i1, i2, aint, bint
REAL(DFP) :: x(3, 3), xin(3, 3), e1(3), e2(3), lam, avar, mu

x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
  x(1:nrow, 1:3) = xij(1:nrow, 1:3)
ELSE
  nrow = 2_I4B
  x(1:nrow, 1) = [0.0, 0.0]
  x(1:nrow, 2) = [1.0, 0.0]
  x(1:nrow, 3) = [0.0, 1.0]
END IF

ncol = LagrangeDOF_Triangle(order=order)
! ALLOCATE (ans(nrow, n))
! ans = 0.0_DFP

!! points on vertex
ans(1:nrow, 1:3) = x(1:nrow, 1:3)

!! points on edge
! ne = LagrangeInDOF_Line(order=order)
i2 = 3
IF (order .GT. 1_I4B) THEN
  i1 = i2 + 1
  ! i1 = i2 + 1; i2 = i1 + ne - 1
  ! ans(1:nrow, i1:i2) = EquidistanceInPoint_Line( &
  !                      order=order, &
  !                      xij=x(1:nrow, [1, 2]))
  CALL EquidistanceInPoint_Line_(order=order, xij=x(1:nrow, [1, 2]), &
                                 ans=ans(:, i1:), nrow=aint, ncol=bint)

  i1 = i1 + bint
  ! i1 = i2 + 1; i2 = i1 + ne - 1
  ! ans(1:nrow, i1:i2) = EquidistanceInPoint_Line( &
  !                      order=order, &
  !                      xij=x(1:nrow, [2, 3]))
  CALL EquidistanceInPoint_Line_(order=order, xij=x(1:nrow, [2, 3]), &
                                 ans=ans(:, i1:), nrow=aint, ncol=bint)

  i1 = i1 + bint
  ! i1 = i2 + 1; i2 = i1 + ne - 1
  ! ans(1:nrow, i1:i2) = EquidistanceInPoint_Line( &
  !                      order=order, &
  !                      xij=x(1:nrow, [3, 1]))
  CALL EquidistanceInPoint_Line_(order=order, xij=x(1:nrow, [3, 1]), &
                                 ans=ans(:, i1:), nrow=aint, ncol=bint)
  i2 = i1 + bint - 1

END IF

IF (order .LE. 2_I4B) RETURN

!! points on face
IF (order .EQ. 3_I4B) THEN
  i1 = i2 + 1
  ans(1:nrow, i1) = (x(1:nrow, 1) + x(1:nrow, 2) + x(1:nrow, 3)) / 3.0_DFP
  RETURN
END IF

e1 = x(:, 2) - x(:, 1)
avar = NORM2(e1)
e1 = e1 / avar
lam = avar / order
e2 = x(:, 3) - x(:, 1)
avar = NORM2(e2)
e2 = e2 / avar
mu = avar / order
xin(1:nrow, 1) = x(1:nrow, 1) + lam * e1(1:nrow) + mu * e2(1:nrow)

e1 = x(:, 3) - x(:, 2)
avar = NORM2(e1)
e1 = e1 / avar
lam = avar / order
e2 = x(:, 1) - x(:, 2)
avar = NORM2(e2)
e2 = e2 / avar
mu = avar / order
xin(1:nrow, 2) = x(1:nrow, 2) + lam * e1(1:nrow) + mu * e2(1:nrow)

e1 = x(:, 1) - x(:, 3)
avar = NORM2(e1)
e1 = e1 / avar
lam = avar / order
e2 = x(:, 2) - x(:, 3)
avar = NORM2(e2)
e2 = e2 / avar
mu = avar / order
xin(1:nrow, 3) = x(1:nrow, 3) + lam * e1(1:nrow) + mu * e2(1:nrow)

i1 = i2 + 1
! ans(1:nrow, i1:) = EquidistancePoint_Triangle(order=order - 3, &
!                                               xij=xin(1:nrow, 1:3))
CALL EquidistancePoint_Triangle_(order=order - 3, xij=xin(1:nrow, 1:3), &
                                 ans=ans(1:nrow, i1:), nrow=aint, ncol=bint)

END PROCEDURE EquidistancePoint_Triangle_

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Triangle
INTEGER(I4B) :: nrow, ncol

IF (order .LT. 3_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 2_I4B
END IF

ncol = LagrangeInDOF_Triangle(order=order)

CALL EquidistanceInPoint_Triangle_(order=order, ans=ans, nrow=nrow, &
                                   ncol=ncol)

END PROCEDURE EquidistanceInPoint_Triangle

!----------------------------------------------------------------------------
!                                               EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Triangle_
REAL(DFP) :: x(3, 3), xin(3, 3), e1(3), e2(3), lam, avar, mu
INTEGER(I4B) :: aint, bint

nrow = 0; ncol = 0
IF (order .LT. 3_I4B) RETURN

x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
  x(1:nrow, 1:3) = xij(1:nrow, 1:3)
ELSE
  nrow = 2_I4B
  x(1:nrow, 1) = [0.0, 0.0]
  x(1:nrow, 2) = [1.0, 0.0]
  x(1:nrow, 3) = [0.0, 1.0]
END IF

ncol = LagrangeInDOF_Triangle(order=order)
! ALLOCATE (ans(nrow, n))
! ans = 0.0_DFP

!! points on face
IF (order .EQ. 3_I4B) THEN
  ans(1:nrow, 1) = (x(1:nrow, 1) + x(1:nrow, 2) + x(1:nrow, 3)) / 3.0_DFP
  RETURN
END IF

e1 = x(:, 2) - x(:, 1)
avar = NORM2(e1)
e1 = e1 / avar
lam = avar / order
e2 = x(:, 3) - x(:, 1)
avar = NORM2(e2)
e2 = e2 / avar
mu = avar / order
xin(1:nrow, 1) = x(1:nrow, 1) + lam * e1(1:nrow) + mu * e2(1:nrow)

e1 = x(:, 3) - x(:, 2)
avar = NORM2(e1)
e1 = e1 / avar
lam = avar / order
e2 = x(:, 1) - x(:, 2)
avar = NORM2(e2)
e2 = e2 / avar
mu = avar / order
xin(1:nrow, 2) = x(1:nrow, 2) + lam * e1(1:nrow) + mu * e2(1:nrow)

e1 = x(:, 1) - x(:, 3)
avar = NORM2(e1)
e1 = e1 / avar
lam = avar / order
e2 = x(:, 2) - x(:, 3)
avar = NORM2(e2)
e2 = e2 / avar
mu = avar / order
xin(1:nrow, 3) = x(1:nrow, 3) + lam * e1(1:nrow) + mu * e2(1:nrow)

CALL EquidistancePoint_Triangle_(order=order - 3, xij=xin(1:nrow, 1:3), &
                                 ans=ans, nrow=aint, ncol=bint)

END PROCEDURE EquidistanceInPoint_Triangle_

!----------------------------------------------------------------------------
!                                                   BlythPozrikidis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BlythPozrikidis_Triangle
INTEGER(I4B) :: nrow, ncol
ncol = LagrangeDOF_Triangle(order=order)
nrow = 2; IF (PRESENT(xij)) nrow = SIZE(xij, 1)
ALLOCATE (ans(nrow, ncol))
CALL BlythPozrikidis_Triangle_(order=order, ipType=ipType, ans=ans,nrow=nrow,&
     ncol=ncol, layout=layout, xij=xij, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE BlythPozrikidis_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BlythPozrikidis_Triangle_
INTEGER(I4B), PARAMETER :: max_order = 30
CHARACTER(*), PARAMETER :: myName = "BlythPozrikidis_Triangle()"
REAL(DFP), PARAMETER :: x(2) = [0.0_DFP, 1.0_DFP]

REAL(DFP) :: v(max_order + 1), xi(max_order + 1, max_order + 1), &
             eta(max_order + 1, max_order + 1), temp(2, 512)

INTEGER(I4B) :: ii, jj, kk, tsize

LOGICAL(LGT) :: isx

CALL InterpolationPoint_Line_(order=order, ipType=ipType, xij=x, &
                              layout="INCREASING", lambda=lambda, &
                              beta=beta, alpha=alpha, ans=v, tsize=tsize)

ncol = LagrangeDOF_Triangle(order=order)
nrow = 2

isx = .FALSE.; IF (PRESENT(xij)) isx = .TRUE.
IF (isx) nrow = SIZE(xij, 1)

xi(1:order + 1, 1:order + 1) = 0.0_DFP
eta(1:order + 1, 1:order + 1) = 0.0_DFP

DO ii = 1, order + 1
  DO jj = 1, order + 2 - ii
    kk = order + 3 - ii - jj
    xi(ii, jj) = (1.0 + 2.0 * v(ii) - v(jj) - v(kk)) / 3.0_DFP
    eta(ii, jj) = (1.0 + 2.0 * v(jj) - v(ii) - v(kk)) / 3.0_DFP
  END DO
END DO

SELECT CASE (layout)

CASE ("VEFC")

  CALL IJ2VEFC_Triangle(xi=xi, eta=eta, temp=temp, order=order, N=ncol)

  IF (isx) THEN
    CALL FromUnitTriangle2Triangle_(xin=temp(1:2, 1:ncol), x1=xij(:, 1), &
                    x2=xij(:, 2), x3=xij(:, 3), ans=ans, nrow=nrow, ncol=ncol)
    RETURN
  END IF

  ans(1:2, 1:ncol) = temp(1:2, 1:ncol)

CASE DEFAULT

 CALL ErrorMsg(msg="layout=VEFC is allowed, found layout is "//TRIM(layout), &
                file=__FILE__, routine=myname, line=__LINE__, unitno=stderr)

END SELECT

END PROCEDURE BlythPozrikidis_Triangle_

!----------------------------------------------------------------------------
!                                                            Isaac_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Isaac_Triangle
INTEGER(I4B) :: nrow, ncol

ncol = SIZE(n=order, d=2)
nrow = 2; IF (PRESENT(xij)) nrow = SIZE(xij, 1)

ALLOCATE (ans(nrow, ncol))

CALL Isaac_Triangle_(order=order, ipType=ipType, ans=ans, nrow=nrow, &
                     ncol=ncol, layout=layout, xij=xij, alpha=alpha, &
                     beta=beta, lambda=lambda)

END PROCEDURE Isaac_Triangle

!----------------------------------------------------------------------------
!                                                             Isaac_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Isaac_Triangle_
CHARACTER(*), PARAMETER :: myName = "Isaac_Triangle()"
INTEGER(I4B), PARAMETER :: max_order = 30
REAL(DFP) :: xi(max_order + 1, max_order + 1), &
             eta(max_order + 1, max_order + 1), &
             temp(2, 512)

! REAL(DFP), ALLOCATABLE :: temp(:, :), rPoints(:, :)
INTEGER(I4B) :: cnt, ii, jj
INTEGER(I4B) :: nn

nn = 1 + order

CALL RecursiveNode2D_(order=order, ipType=ipType, domain="UNIT", &
                      alpha=alpha, beta=beta, lambda=lambda, ans=temp, &
                      nrow=nrow, ncol=ncol)

IF (PRESENT(xij)) nrow = SIZE(xij, 1)

!! convert from rPoints to xi and eta
cnt = 0
xi(1:nn, 1:nn) = 0.0_DFP
eta(1:nn, 1:nn) = 0.0_DFP

DO ii = 1, nn
  DO jj = 1, nn + 1 - ii
    cnt = cnt + 1
    xi(ii, jj) = temp(1, cnt)
    eta(ii, jj) = temp(2, cnt)
  END DO
END DO

IF (layout .EQ. "VEFC") THEN
  ! CALL Reallocate(temp, 2, N)
  CALL IJ2VEFC_Triangle(xi=xi, eta=eta, temp=temp, order=order, N=ncol)

  IF (PRESENT(xij)) THEN
    CALL FromUnitTriangle2Triangle_(xin=temp(:, 1:ncol), ans=ans, &
               nrow=nrow, ncol=ncol, x1=xij(:, 1), x2=xij(:, 2), x3=xij(:, 3))
    RETURN
  END IF

  ans(1:nrow, 1:ncol) = temp(1:nrow, 1:ncol)
  RETURN
END IF

CALL ErrorMsg(file=__FILE__, routine=myname, line=__LINE__, unitno=stderr, &
              msg="Only layout=VEFC is allowed, found layout is "//layout)

END PROCEDURE Isaac_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE IJ2VEFC_Triangle
INTEGER(I4B) :: cnt, m, ii, jj, ll, llt, llr

cnt = 0
m = order
llt = INT((m - 1) / 3)
llr = MOD(m - 1, 3)
DO ll = 0, llt
  !! v1
  cnt = cnt + 1
  ii = 1 + ll; jj = 1 + ll
  temp(1, cnt) = xi(ii, jj)
  temp(2, cnt) = eta(ii, jj)
  !! v2
  cnt = cnt + 1
  ii = m + 1 - 2 * ll; jj = 1 + ll
  temp(1, cnt) = xi(ii, jj)
  temp(2, cnt) = eta(ii, jj)
  !! v3
  cnt = cnt + 1
  ii = 1 + ll; jj = m + 1 - 2 * ll
  temp(1, cnt) = xi(ii, jj)
  temp(2, cnt) = eta(ii, jj)
  !! nodes on edge 12
  jj = ll + 1
  DO ii = 2 + ll, m - 2 * ll
    cnt = cnt + 1
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
  END DO
  !! nodes on edge 23
  DO jj = 2 + ll, m - 2 * ll
    cnt = cnt + 1
    ii = m - ll + 2 - jj
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
  END DO
  !! nodes on edge 31
  ii = ll + 1
  DO jj = m - 2 * ll, 2 + ll, -1
    cnt = cnt + 1
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
  END DO
  !! internal nodes
END DO

IF (llr .EQ. 2_I4B) THEN
  !! a internal point
  cnt = cnt + 1
  ll = llt + 1
  ii = 1 + ll; jj = 1 + ll
  temp(1, cnt) = xi(ii, jj)
  temp(2, cnt) = eta(ii, jj)
END IF

IF (cnt .NE. N) THEN
  CALL ErrorMsg(file=__FILE__, routine="IJ2VEFC_Triangle()", &
                line=__LINE__, unitno=stderr, &
                msg="cnt="//ToString(cnt)//" not equal to total DOF, N=" &
                //ToString(N))
  RETURN
END IF

END PROCEDURE IJ2VEFC_Triangle

!----------------------------------------------------------------------------
!                                               InterpolationPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Triangle
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 2
END IF

SELECT CASE (ipType)
CASE (ipopt%Equidistance, ipopt%BlythPozChebyshev, ipopt%BlythPozLegendre)
  ncol = LagrangeDOF_Triangle(order=order)

CASE (ipopt%IsaacLegendre, ipopt%IsaacChebyshev, &
      ipopt%GaussLegendreLobatto, ipopt%GaussChebyshevLobatto)
  ncol = SIZE(n=order, d=2)

END SELECT

ALLOCATE (ans(nrow, ncol))

CALL InterpolationPoint_Triangle_(order=order, ipType=ipType, ans=ans, &
                                 nrow=nrow, ncol=ncol, xij=xij, alpha=alpha, &
                                  beta=beta, lambda=lambda, layout=layout)

END PROCEDURE InterpolationPoint_Triangle

!----------------------------------------------------------------------------
!                                               InterpolationPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Triangle_
CHARACTER(*), PARAMETER :: myName = "InterpolationPoint_Triangle_()"

SELECT CASE (ipType)
CASE (ipopt%Equidistance)
  CALL EquidistancePoint_Triangle_(xij=xij, order=order, ans=ans, &
                                   nrow=nrow, ncol=ncol)

CASE (ipopt%BlythPozLegendre)
  CALL BlythPozrikidis_Triangle_(order=order, ans=ans, nrow=nrow, ncol=ncol, &
                  ipType=ipopt%GaussLegendreLobatto, layout="VEFC", xij=xij, &
                                 alpha=alpha, beta=beta, lambda=lambda)

CASE (ipopt%BlythPozChebyshev)
  CALL BlythPozrikidis_Triangle_(order=order, &
                                 ipType=ipopt%GaussChebyshevLobatto, &
              layout="VEFC", xij=xij, alpha=alpha, beta=beta, lambda=lambda, &
                                 ans=ans, nrow=nrow, ncol=ncol)

CASE (ipopt%IsaacLegendre, ipopt%GaussLegendreLobatto)
  CALL Isaac_Triangle_(order=order, &
                       ipType=ipopt%GaussLegendreLobatto, &
              layout="VEFC", xij=xij, alpha=alpha, beta=beta, lambda=lambda, &
                       ans=ans, nrow=nrow, ncol=ncol)

CASE (ipopt%IsaacChebyshev, ipopt%GaussChebyshevLobatto)
  CALL Isaac_Triangle_(order=order, ipType=ipopt%GaussChebyshevLobatto, &
              layout="VEFC", xij=xij, alpha=alpha, beta=beta, lambda=lambda, &
                       ans=ans, nrow=nrow, ncol=ncol)

CASE (ipopt%Feket, ipopt%Hesthaven, ipopt%ChenBabuska)
  CALL ErrorMsg(msg="Feket, Hesthaven, ChenBabuska nodes not available", &
                file=__FILE__, routine=myname, line=__LINE__, unitno=stderr)

CASE DEFAULT
  CALL Isaac_Triangle_(order=order, ipType=ipType, layout="VEFC", &
                       xij=xij, alpha=alpha, beta=beta, lambda=lambda, &
                       ans=ans, nrow=nrow, ncol=ncol)
END SELECT

END PROCEDURE InterpolationPoint_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
