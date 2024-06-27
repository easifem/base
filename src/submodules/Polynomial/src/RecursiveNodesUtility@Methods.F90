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

SUBMODULE(RecursiveNodesUtility) Methods
USE BaseMethod
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION NumberofRows(d, domain) RESULT(nrow)
  INTEGER(I4B), INTENT(IN) :: d
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: domain
  INTEGER(I4B) :: nrow

  LOGICAL(LGT) :: isdomain
  CHARACTER(2) :: mydomain

  isdomain = PRESENT(domain)
  mydomain = "BA"
  IF (isdomain) mydomain = UpperCase(domain(1:2))

  IF (mydomain .EQ. "BA") THEN
    nrow = d + 1
  ELSE
    nrow = d
  END IF
END FUNCTION NumberofRows

!----------------------------------------------------------------------------
!                                               RecursiveNode1D
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode1D
INTEGER(I4B) :: nrow, ncol

nrow = NumberofRows(d=1_I4B, domain=domain)
ncol = SIZE(n=order, d=1_I4B)

ALLOCATE (ans(nrow, ncol))

CALL RecursiveNode1D_(order=order, ipType=ipType, ans=ans, nrow=nrow, &
              ncol=ncol, alpha=alpha, beta=beta, lambda=lambda, domain=domain)

END PROCEDURE RecursiveNode1D

!----------------------------------------------------------------------------
!                                                            RecursiveNode1D
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode1D_
INTEGER(I4B), PARAMETER :: d = 1_I4B, max_order = 99_I4B
INTEGER(I4B) :: jj, tsize, i1, i2, aint, bint
REAL(DFP) :: avar, x(max_order + 1), xij(2, 1)
LOGICAL(LGT) :: isdomain
CHARACTER(2) :: mydomain

INTEGER(I4B), ALLOCATABLE :: indices(:, :)

isdomain = PRESENT(domain)
mydomain = "BA"
IF (isdomain) mydomain = domain(1:2)

xij(1, 1) = 0.0_DFP
xij(2, 1) = 1.0_DFP

CALL InterpolationPoint_Line_(order=order, ipType=ipType, xij=xij(:, 1), &
          ans=x, layout="INCREASING", alpha=alpha, beta=beta, lambda=lambda, &
                              tsize=tsize)

nrow = d + 1
ncol = SIZE(n=order, d=d)

ALLOCATE (indices(nrow, ncol))

CALL GetMultiIndices_(n=order, d=d, ans=indices, nrow=nrow, ncol=ncol)

SELECT CASE (mydomain)
CASE ("BA", "Ba", "ba")
  DO jj = 1, ncol
    i1 = indices(1, jj) + 1
    i2 = indices(2, jj) + 1

    avar = x(i1) + x(i2)

    ans(1, jj) = x(i1) / avar
    ans(2, jj) = x(i2) / avar
  END DO

CASE default
  nrow = nrow - 1

  DO jj = 1, ncol
    i1 = indices(1, jj) + 1
    i2 = indices(2, jj) + 1

    avar = x(i1) + x(i2)

    xij(1, 1) = x(i1) / avar
    xij(2, 1) = x(i2) / avar

    CALL Coord_Map_(x=xij, from="BARYCENTRIC", to=mydomain, &
                    ans=ans(:, jj:), nrow=aint, ncol=bint)
  END DO

END SELECT

DEALLOCATE (indices)

END PROCEDURE RecursiveNode1D_

!----------------------------------------------------------------------------
!                                               RecursiveNode2D
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode2D
INTEGER(I4B) :: nrow, ncol
nrow = NumberofRows(d=2_I4B, domain=domain)
ncol = SIZE(n=order, d=2_I4B)
ALLOCATE (ans(nrow, ncol))
CALL RecursiveNode2D_(order=order, iptype=iptype, ans=ans, nrow=nrow, &
              ncol=ncol, domain=domain, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE RecursiveNode2D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode2D_
INTEGER(I4B), PARAMETER :: d = 2_I4B, dp1 = 3_I4B
INTEGER(I4B), PARAMETER :: max_order = 100 !! max_order + 1

INTEGER(I4B) :: aindx(dp1), indx(d), aint, bint, jj, ii

REAL(DFP) :: xi, xt, b(dp1), bs(d), Xn(max_order), &
             BX(d, max_order, max_order), xij(dp1, 1), &
             bxn(d, max_order)

INTEGER(I4B), ALLOCATABLE :: indices(:, :)

CHARACTER(2) :: mydomain
LOGICAL(LGT) :: isdomain

isdomain = PRESENT(domain)
mydomain = "BA"; IF (isdomain) mydomain = UpperCase(domain(1:2))

nrow = d + 1
ncol = SIZE(n=order, d=d)
ALLOCATE (indices(nrow, ncol))

CALL BarycentericNodeFamily1D(order=order, ipType=ipType, ans=BX, &
                              Xn=Xn, alpha=alpha, beta=beta, lambda=lambda, &
                              indices=indices, bxn=bxn)

CALL GetMultiIndices_(n=order, d=d, ans=indices, nrow=nrow, ncol=ncol)

IF (mydomain .NE. "BA") nrow = d

DO jj = 1, ncol
  aindx = indices(:, jj)

  xt = 0.0_DFP
  xij = 0.0_DFP

  DO ii = 1, d + 1
    indx = Pop(aindx, ii)
    bs = BX(:, indx(1) + 1, indx(2) + 1)
    b = Push(vec=bs, VALUE=0.0_DFP, pos=ii)
    xi = Xn(SUM(indx) + 1)
    xt = xt + xi
    xij(:, 1) = xij(:, 1) + xi * b
  END DO

  xij = xij / xt

  CALL Coord_Map_(x=xij, from="BARYCENTRIC", to=mydomain, &
                  ans=ans(:, jj:), nrow=aint, ncol=bint)

END DO

IF (ALLOCATED(indices)) DEALLOCATE (indices)
END PROCEDURE RecursiveNode2D_

!----------------------------------------------------------------------------
!                                                          RecursiveNode3D
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode3D
INTEGER(I4B) :: nrow, ncol
nrow = NumberofRows(d=3_I4B, domain=domain)
ncol = SIZE(n=order, d=3_I4B)
ALLOCATE (ans(nrow, ncol))
CALL RecursiveNode3D_(order=order, iptype=iptype, ans=ans, nrow=nrow, &
              ncol=ncol, domain=domain, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE RecursiveNode3D

!----------------------------------------------------------------------------
!                                                           RecursiveNode3D_
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode3D_
INTEGER(I4B), PARAMETER :: d = 3_I4B, dp1 = 4_I4B, max_order = 26

INTEGER(I4B) :: jj, ii, aint, bint, aindx(dp1), indx(d)

REAL(DFP) :: xi, xt, b(dp1), bs(d), xn(max_order), &
             bx(d, max_order, max_order, max_order), xij(dp1, 1)

INTEGER(I4B), ALLOCATABLE :: indices(:, :)
REAL(DFP), ALLOCATABLE :: bxn(:, :)

CHARACTER(2) :: mydomain
LOGICAL(LGT) :: isdomain

isdomain = PRESENT(domain)
mydomain = "BA"; IF (isdomain) mydomain = UpperCase(domain(1:2))

nrow = d + 1
ncol = SIZE(n=order, d=d)
ALLOCATE (indices(nrow, ncol), bxn(d, ncol))

CALL BarycentericNodeFamily2D(order=order, ipType=ipType, ans=bx, Xn=Xn, &
              alpha=alpha, beta=beta, lambda=lambda, indices=indices, bxn=bxn)

CALL GetMultiIndices_(n=order, d=d, ans=indices, nrow=nrow, ncol=ncol)

IF (mydomain .NE. "BA") nrow = d

DO jj = 1, ncol

  aindx = indices(:, jj)
  xt = 0.0_DFP
  xij = 0.0_DFP

  DO ii = 1, dp1

    indx = Pop(aindx, ii)
    bs = bx(:, indx(1) + 1, indx(2) + 1, indx(3) + 1)
    b = Push(vec=bs, VALUE=0.0_DFP, pos=ii)
    xi = xn(SUM(indx) + 1)
    xt = xt + xi
    xij(:, 1) = xij(:, 1) + xi * b

  END DO

  xij = xij / xt

  CALL Coord_Map_(x=xij, from="BARYCENTRIC", to=mydomain, &
                  ans=ans(:, jj:), nrow=aint, ncol=bint)

END DO

IF (ALLOCATED(indices)) DEALLOCATE (indices)
IF (ALLOCATED(bxn)) DEALLOCATE (bxn)

END PROCEDURE RecursiveNode3D_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE BarycentericNodeFamily1D(order, ipType, ans, Xn, indices, bxn, &
                                    alpha, beta, lambda)
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B), INTENT(IN) :: ipType
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  !! ans(2, order + 1, order + 1)
  REAL(DFP), INTENT(INOUT) :: Xn(:)
  !! Xn(order + 1)
  INTEGER(I4B), INTENT(INOUT) :: indices(:, :)
  !!
  REAL(DFP), INTENT(INOUT) :: bxn(:, :)
  !!
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
  !! Jacobi polynomial parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta
  !! Jacobi polynomial parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
  !! Ultraspherical polynomial parameter

  INTEGER(I4B), PARAMETER :: d = 1_I4B, dp1 = 2_I4B
  INTEGER(I4B) :: ii, jj, nrow, ncol

  DO ii = 0, order
    ! indices = GetMultiIndices(n=ii, d=d)
    CALL GetMultiIndices_(n=ii, d=d, ans=indices, nrow=nrow, ncol=ncol)

    CALL RecursiveNode1D_(order=ii, ipType=ipType, ans=bxn, nrow=nrow, &
                          ncol=ncol, alpha=alpha, beta=beta, lambda=lambda)

    DO jj = 1, ii + 1
      ans(1:dp1, indices(1, jj) + 1, indices(2, jj) + 1) = bxn(1:dp1, jj)
    END DO

  END DO

  Xn(1:order + 1) = bxn(1, 1:order + 1)

END SUBROUTINE BarycentericNodeFamily1D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE BarycentericNodeFamily2D(order, ipType, ans, Xn, alpha, beta, &
                                    lambda, indices, bxn)

  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B), INTENT(IN) :: ipType
  REAL(DFP), INTENT(inout) :: ans(:, :, :, :)
  !! ans(3, order + 1, order + 1, order + 1)
  REAL(DFP), INTENT(OUT) :: xn(:)
  !! Xn(order + 1)
  INTEGER(I4B), INTENT(INOUT) :: indices(:, :)
  !!
  REAL(DFP), INTENT(INOUT) :: bxn(:, :)
  !!
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
  !! Jacobi polynomial parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta
  !! Jacobi polynomial parameter
  REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
  !! Ultraspherical polynomial parameter

  !! Internal varible

  REAL(DFP), PARAMETER :: xij(2) = [0.0_DFP, 1.0_DFP]
  INTEGER(I4B), PARAMETER :: d = 2_I4B
  INTEGER(I4B) :: ii, jj, nrow, ncol

  DO ii = 0, order
    CALL GetMultiIndices_(n=ii, d=d, ans=indices, nrow=nrow, ncol=ncol)

    CALL RecursiveNode2D_(order=ii, ipType=ipType, alpha=alpha, &
                      beta=beta, lambda=lambda, ans=bxn, nrow=nrow, ncol=ncol)

    DO jj = 1, ncol
      ans(1:3, indices(1, jj) + 1, indices(2, jj) + 1, indices(3, jj) + 1) = &
        bxn(1:3, jj)
    END DO

  END DO

  CALL InterpolationPoint_Line_(ans=xn, tsize=nrow, order=order, &
                   ipType=ipType, xij=xij, layout="INCREASING", alpha=alpha, &
                                beta=beta, lambda=lambda)

END SUBROUTINE BarycentericNodeFamily2D

!----------------------------------------------------------------------------
!                                                          Unit2Equilateral
!----------------------------------------------------------------------------

MODULE PROCEDURE Unit2Equilateral
INTEGER(I4B) :: ii
!!
IF (d .GT. 1_I4B) THEN
  ! Move the top vertex over the centroid
  DO ii = 1, d - 1
    x(ii, :) = x(ii, :) + x(d, :) / d
  END DO
  ! Make the projection onto the lesser dimensions equilateral
  CALL Unit2Equilateral(d - 1, x(1:d - 1, :))
  ! scale the vertical dimension
  x(d, :) = x(d, :) * SQRT((d + 1.0_DFP) / (2.0_DFP * d))
END IF
END PROCEDURE Unit2Equilateral

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Equilateral2Unit
INTEGER(I4B) :: ii
!!
IF (d .GT. 1_I4B) THEN
  x(d, :) = x(d, :) / SQRT((d + 1.0_DFP) / (2.0_DFP * d))
  CALL Equilateral2Unit(d=d - 1, x=x(1:d - 1, :))
  DO ii = 1, d - 1
    x(ii, :) = x(ii, :) - x(d, :) / d
  END DO
END IF
END PROCEDURE Equilateral2Unit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ToUnit
TYPE(String) :: astr
INTEGER(I4B) :: d
astr = UpperCase(TRIM(domain))
SELECT CASE (astr%chars())
CASE ("UNIT")
  ans = x
CASE ("BIUNIT")
  ans = 0.5_DFP * (x + 1.0_DFP)
CASE ("BARYCENTRIC")
  d = SIZE(x, 1)
  ans = x(1:d - 1, :)
CASE ("EQUILATERAL")
  d = SIZE(x, 1)
  ans = x
  ans = ans / 2.0_DFP
  CALL Equilateral2Unit(d=d, x=ans)
  ans = ans + 1.0_DFP / (d + 1.0_DFP)
END SELECT
END PROCEDURE ToUnit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ToUnit_
nrow = SIZE(x, 1)
ncol = SIZE(x, 2)

SELECT CASE (domain(1:2))
CASE ("UN", "un", "Un")
  ans(1:nrow, 1:ncol) = x

CASE ("BI", "bi", "Bi")
  ans(1:nrow, 1:ncol) = 0.5_DFP * (x + 1.0_DFP)

CASE ("BA", "ba", "Ba")
  nrow = nrow - 1
  ans(1:nrow, 1:ncol) = x(1:nrow, :)

CASE ("EQ", "eq", "Eq")
  ans(1:nrow, 1:ncol) = x

  ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) * 0.5_DFP

  CALL Equilateral2Unit(d=nrow, x=ans)

  ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + 1.0_DFP / &
                        (REAL(nrow, kind=dfp) + 1.0_DFP)

END SELECT
END PROCEDURE ToUnit_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnit
TYPE(String) :: astr
INTEGER(I4B) :: d
astr = UpperCase(TRIM(domain))
SELECT CASE (astr%chars())
CASE ("UNIT")
  ans = x
CASE ("BIUNIT")
  ans = 2.0_DFP * x - 1
CASE ("BARYCENTRIC")
  ans = x.ROWCONCAT. (1.0_DFP - SUM(x, dim=1))
CASE ("EQUILATERAL")
  d = SIZE(x, 1)
  ans = x
  ans = ans - 1.0_DFP / (d + 1.0_DFP)
  CALL Unit2Equilateral(d=d, x=ans)
  ans = ans * 2.0_DFP
END SELECT
END PROCEDURE FromUnit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnit_
nrow = SIZE(x, 1)
ncol = SIZE(x, 2)

SELECT CASE (domain(1:2))
CASE ("UN", "Un", "un")
  ans(1:nrow, 1:ncol) = x

CASE ("BI", "Bi", "bi")
  ans(1:nrow, 1:ncol) = 2.0_DFP * x - 1.0_DFP

CASE ("BA", "Ba", "ba")
  ans(1:nrow, 1:ncol) = x
  nrow = nrow + 1
  ans(nrow, 1:ncol) = (1.0_DFP - SUM(x, dim=1))

CASE ("EQ", "Eq", "eq")
  ans(1:nrow, 1:ncol) = x - 1.0_DFP / (REAL(nrow, kind=DFP) + 1.0_DFP)

  CALL Unit2Equilateral(d=nrow, x=ans)

  ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) * 2.0_DFP

END SELECT
END PROCEDURE FromUnit_

!----------------------------------------------------------------------------
!                                                                 Coord_Map
!----------------------------------------------------------------------------

MODULE PROCEDURE Coord_Map
ans = FromUnit(x=(ToUnit(x=x, domain=from)), domain=to)
END PROCEDURE Coord_Map

!----------------------------------------------------------------------------
!                                                                 Coord_Map
!----------------------------------------------------------------------------

MODULE PROCEDURE Coord_Map_
INTEGER(I4B) :: aint, bint
CALL ToUnit_(x=x, domain=from, ans=ans, nrow=aint, ncol=bint)
CALL FromUnit_(x=ans(1:aint, 1:bint), domain=to, ans=ans, &
               nrow=nrow, ncol=ncol)
END PROCEDURE Coord_Map_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
