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

SUBMODULE(TriangleInterpolationUtility) QuadratureMethods
USE BaseMethod
USE Triangle_QuadraturePoint_Solin, ONLY: QuadraturePointTriangleSolin, &
                                          QuadraturePointTriangleSolin_, &
                                          QuadratureNumberTriangleSolin
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadratureNumber_Triangle
ans = QuadratureNumberTriangleSolin(order=order)

IF (ans .LE. 0) THEN
  ans = 1_I4B + INT(order / 2, kind=I4B)
  ans = ans * (ans + 1)
END IF
END PROCEDURE QuadratureNumber_Triangle

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Triangle1
INTEGER(I4B) :: nipsx(1), nipsy(1), nrow, ncol

nrow = 1_I4B + INT(order / 2, kind=I4B)
nipsx(1) = nrow + 1
nipsy(1) = nrow

IF (PRESENT(xij)) THEN
  nrow = MAX(SIZE(xij, 1), 2_I4B)
ELSE
  nrow = 2_I4B
END IF

nrow = nrow + 1_I4B
ncol = nipsx(1) * nipsy(1)

ALLOCATE (ans(nrow, ncol))

CALL TensorQuadraturePoint_Triangle2_(nipsx=nipsx, nipsy=nipsy, &
    quadType=quadType, refTriangle=refTriangle, xij=xij, ans=ans, nrow=nrow, &
                                      ncol=ncol)
END PROCEDURE TensorQuadraturePoint_Triangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Triangle1_
INTEGER(I4B) :: nipsx(1), nipsy(1), n

n = 1_I4B + INT(order / 2, kind=I4B)
nipsx(1) = n + 1
nipsy(1) = n

CALL TensorQuadraturePoint_Triangle2_(nipsx=nipsx, nipsy=nipsy, &
               quadType=quadType, refTriangle=refTriangle, xij=xij, ans=ans, &
                                      nrow=nrow, ncol=ncol)
END PROCEDURE TensorQuadraturePoint_Triangle1_

!----------------------------------------------------------------------------
!                                           TensorQuadraturePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Triangle2
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = MAX(SIZE(xij, 1), 2_I4B)
ELSE
  nrow = 2_I4B
END IF

nrow = nrow + 1_I4B
ncol = nipsx(1) * nipsy(1)

ALLOCATE (ans(nrow, ncol))

CALL TensorQuadraturePoint_Triangle2_(nipsx=nipsx, nipsy=nipsy, &
    quadType=quadType, refTriangle=refTriangle, xij=xij, ans=ans, nrow=nrow, &
                                      ncol=ncol)
END PROCEDURE TensorQuadraturePoint_Triangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Triangle2_
INTEGER(I4B) :: nsd, ii, jj
REAL(DFP), ALLOCATABLE :: temp(:, :)
REAL(DFP) :: areal
REAL(DFP), PARAMETER :: oneby8 = 1.0_DFP / 8.0_DFP

CHARACTER(1) :: astr

IF (PRESENT(xij)) THEN
  nsd = MAX(SIZE(xij, 1), 2_I4B)
ELSE
  nsd = 2_I4B
END IF

nrow = nsd + 1_I4B
ncol = nipsx(1) * nipsy(1)

! ALLOCATE (temp(nrow, ncol))

CALL QuadraturePoint_Quadrangle_(nipsx=nipsx, nipsy=nipsy, &
             quadType1=GaussLegendreLobatto, quadType2=GaussJacobiRadauLeft, &
             refQuadrangle="BIUNIT", alpha2=1.0_DFP, beta2=0.0_DFP, ans=ans, &
                                 nrow=ii, ncol=jj)

! temp_t(1:2, :) = FromBiUnitSqr2UnitTriangle(xin=temp_q(1:2, :))
CALL FromSquare2Triangle_(xin=ans(1:2, :), ans=ans, nrow=ii, ncol=jj, &
                          from="BIUNIT", to="UNIT")

DO CONCURRENT(ii=1:ncol)
  ans(nrow, ii) = ans(nrow, ii) * oneby8
END DO

IF (PRESENT(xij)) THEN
  CALL FromUnitTriangle2Triangle_(xin=ans(1:2, :), x1=xij(:, 1), &
                        x2=xij(:, 2), x3=xij(:, 3), ans=ans, nrow=ii, ncol=jj)

  areal = JacobianTriangle(from="UNIT", to="TRIANGLE", xij=xij)

  DO CONCURRENT(ii=1:ncol)
    ans(nrow, ii) = ans(nrow, ii) * areal
  END DO

  RETURN
END IF

astr = UpperCase(refTriangle(1:1))

IF (astr .EQ. "B") THEN
  CALL FromTriangle2Triangle_(xin=ans(1:2, :), ans=ans, nrow=ii, &
                              ncol=jj, from="UNIT", to="BIUNIT")

  areal = JacobianTriangle(from="UNIT", to="BIUNIT")

  DO CONCURRENT(ii=1:ncol)
    ans(nrow, ii) = ans(nrow, ii) * areal
  END DO

  RETURN

END IF

END PROCEDURE TensorQuadraturePoint_Triangle2_

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Triangle1
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: abool

ncol = QuadratureNumberTriangleSolin(order=order)

nrow = 2_I4B
abool = PRESENT(xij)
IF (abool) nrow = SIZE(xij, 1)
nrow = nrow + 1

ALLOCATE (ans(nrow, ncol))

CALL QuadraturePoint_Triangle1_(order=order, quadType=quadType, &
              refTriangle=refTriangle, xij=xij, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE QuadraturePoint_Triangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Triangle1_
INTEGER(I4B) :: nips(1)

nips(1) = QuadratureNumberTriangleSolin(order=order)

IF (nips(1) .LE. 0) THEN
  CALL TensorQuadraturepoint_Triangle_(order=order, quadtype=quadtype, &
                                       reftriangle=reftriangle, xij=xij, &
                                       ans=ans, nrow=nrow, ncol=ncol)
  RETURN
END IF

CALL QuadraturePoint_Triangle2_(nips=nips, quadType=quadType, &
              refTriangle=refTriangle, xij=xij, ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE QuadraturePoint_Triangle1_

!----------------------------------------------------------------------------
!                                               QuadraturePoint_Triangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Triangle2
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: abool

nrow = 2_I4B
abool = PRESENT(xij)
IF (abool) nrow = SIZE(xij, 1)

nrow = nrow + 1
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

CALL QuadraturePoint_Triangle2_(nips=nips, quadType=quadType, &
              refTriangle=refTriangle, xij=xij, ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE QuadraturePoint_Triangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Triangle2_
INTEGER(I4B) :: nsd, ii, jj
LOGICAL(LGT) :: abool
REAL(DFP) :: areal
CHARACTER(1) :: astr

nrow = 0
ncol = 0

ii = QuadratureNumberTriangleSolin(order=20)
abool = nips(1) .GT. ii
IF (abool) THEN
  CALL Errormsg(msg="This routine should be called for economical &
    & quadrature points only, otherwise call QuadraturePoint_Triangle1()", &
    routine="QuadraturePoint_Triangle2()", &
    file=__FILE__, line=__LINE__, unitNo=stdout)
  RETURN
END IF

nsd = 2_I4B
abool = PRESENT(xij)
IF (abool) nsd = SIZE(xij, 1)

nrow = nsd + 1
ncol = nips(1)

CALL QuadraturePointTriangleSolin_(nips=nips, ans=ans, nrow=ii, ncol=jj)

IF (abool) THEN
  CALL FromTriangle2Triangle_(xin=ans(1:2, 1:ncol), x1=xij(1:nsd, 1), &
                              x2=xij(1:nsd, 2), x3=xij(1:nsd, 3), ans=ans, &
                              from="U", to="T", nrow=ii, ncol=jj)

  areal = JacobianTriangle(from="UNIT", to="TRIANGLE", xij=xij)

  DO CONCURRENT(ii=1:ncol)
    ans(nrow, ii) = ans(nrow, ii) * areal
  END DO

  RETURN

END IF

astr = UpperCase(reftriangle(1:1))
abool = astr == "B"

IF (abool) THEN
  CALL FromTriangle2Triangle_(xin=ans(1:2, 1:ncol), ans=ans, &
                              from="U", to="B", nrow=ii, ncol=jj)

  areal = JacobianTriangle(from="UNIT", to="BIUNIT")

  DO CONCURRENT(ii=1:ncol)
    ans(nrow, ii) = ans(nrow, ii) * areal
  END DO

  RETURN
END IF

END PROCEDURE QuadraturePoint_Triangle2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE QuadratureMethods
