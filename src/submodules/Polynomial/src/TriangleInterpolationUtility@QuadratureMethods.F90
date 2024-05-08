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
USE QuadraturePoint_Triangle_Solin, ONLY: QuadraturePointTriangleSolin, &
                                          QuadraturePointTriangleSolin_, &
                                          QuadratureNumberTriangleSolin
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Triangle1
INTEGER(I4B) :: np(1), nq(1), n
n = 1_I4B + INT(order / 2, kind=I4B)
np(1) = n + 1
nq(1) = n
ans = TensorQuadraturePoint_Triangle2( &
  & nipsx=np, &
  & nipsy=nq, &
  & quadType=quadType, &
  & refTriangle=refTriangle, &
  & xij=xij)
END PROCEDURE TensorQuadraturePoint_Triangle1

!----------------------------------------------------------------------------
!                                           TensorQuadraturePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Triangle2
INTEGER(I4B) :: np(1), nq(1), nsd
REAL(DFP), ALLOCATABLE :: temp_q(:, :), temp_t(:, :)
TYPE(String) :: astr

astr = TRIM(UpperCase(refTriangle))
np(1) = nipsx(1)
nq(1) = nipsy(1)

temp_q = QuadraturePoint_Quadrangle(&
  & nipsx=np,  &
  & nipsy=nq,  &
  & quadType1=GaussLegendreLobatto, &
  & quadType2=GaussJacobiRadauLeft, &
  & refQuadrangle="BIUNIT", &
  & alpha2=1.0_DFP, &
  & beta2=0.0_DFP)

CALL Reallocate(temp_t, SIZE(temp_q, 1, kind=I4B), SIZE(temp_q, 2, kind=I4B))
temp_t(1:2, :) = FromBiUnitSqr2UnitTriangle(xin=temp_q(1:2, :))
temp_t(3, :) = temp_q(3, :) / 8.0_DFP

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 2_I4B
END IF

CALL Reallocate(ans, nsd + 1_I4B, SIZE(temp_q, 2, kind=I4B))

IF (PRESENT(xij)) THEN
  ans(1:nsd, :) = FromUnitTriangle2Triangle(  &
    & xin=temp_t(1:2, :), &
    & x1=xij(:, 1), &
    & x2=xij(:, 2), &
    & x3=xij(:, 3))
  ans(nsd + 1, :) = temp_t(3, :) * JacobianTriangle( &
    & from="UNIT", &
    & to="TRIANGLE", &
    & xij=xij)
ELSE
  IF (astr%chars() .EQ. "BIUNIT") THEN
    ans(1:nsd, :) = FromUnitTriangle2BiUnitTriangle(xin=temp_t(1:2, :))

    ans(nsd + 1, :) = temp_t(3, :) * JacobianTriangle( &
      & from="UNIT", &
      & to="BIUNIT")

  ELSE
    ans = temp_t
  END IF
END IF

IF (ALLOCATED(temp_q)) DEALLOCATE (temp_q)
IF (ALLOCATED(temp_t)) DEALLOCATE (temp_t)

END PROCEDURE TensorQuadraturePoint_Triangle2

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Triangle1
INTEGER(I4B) :: nips(1), nsd, ii, jj
REAL(DFP), ALLOCATABLE :: temp_t(:, :)
LOGICAL(LGT) :: abool

nips(1) = QuadratureNumberTriangleSolin(order=order)

IF (nips(1) .LE. 0) THEN
  ans = TensorQuadraturepoint_Triangle(order=order, quadtype=quadtype, &
                                       reftriangle=reftriangle, xij=xij)
  RETURN
END IF

ALLOCATE (temp_t(3, nips(1)))
CALL QuadraturePointTriangleSolin_(nips=nips, ans=temp_t, nrow=ii, &
                                   ncol=jj)

nsd = 2_I4B
abool = PRESENT(xij)
IF (abool) nsd = SIZE(xij, 1)

ii = nsd + 1
ALLOCATE (ans(ii, jj))

IF (abool) THEN

  CALL FromTriangle2Triangle_(xin=temp_t(1:2, :), x1=xij(1:nsd, 1), &
                      x2=xij(1:nsd, 2), x3=xij(1:nsd, 3), ans=ans(1:nsd, :), &
                              from="U", to="T")

  ans(nsd + 1, :) = temp_t(3, :) * JacobianTriangle(from="UNIT", &
                                                    to="TRIANGLE", xij=xij)

  RETURN

END IF

abool = reftriangle(1:1) == "B" .OR. reftriangle(1:1) == "b"

IF (abool) THEN
  ans(1:nsd, :) = FromUnitTriangle2BiUnitTriangle(xin=temp_t(1:2, :))
  ans(nsd + 1, :) = temp_t(3, :) * JacobianTriangle(from="UNIT", to="BIUNIT")
  RETURN
END IF

ans = temp_t

IF (ALLOCATED(temp_t)) DEALLOCATE (temp_t)

END PROCEDURE QuadraturePoint_Triangle1

!----------------------------------------------------------------------------
!                                               QuadraturePoint_Triangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Triangle2
INTEGER(I4B) :: nsd
REAL(DFP), ALLOCATABLE :: temp_t(:, :)
TYPE(string) :: astr

IF (nips(1) .LE. QuadratureNumberTriangleSolin(order=20_I4B)) THEN
  astr = TRIM(UpperCase(refTriangle))
  temp_t = QuadraturePointTriangleSolin(nips=nips)

  IF (PRESENT(xij)) THEN
    nsd = SIZE(xij, 1)
  ELSE
    nsd = 2_I4B
  END IF

  CALL Reallocate(ans, nsd + 1_I4B, SIZE(temp_t, 2, kind=I4B))

  IF (PRESENT(xij)) THEN
    ans(1:nsd, :) = FromUnitTriangle2Triangle(  &
      & xin=temp_t(1:2, :), &
      & x1=xij(:, 1), &
      & x2=xij(:, 2), &
      & x3=xij(:, 3))
    ans(nsd + 1, :) = temp_t(3, :) * JacobianTriangle( &
      & from="UNIT", &
      & to="TRIANGLE", &
      & xij=xij)
  ELSE
    IF (astr%chars() .EQ. "BIUNIT") THEN
      ans(1:nsd, :) = FromUnitTriangle2BiUnitTriangle(xin=temp_t(1:2, :))
      ans(nsd + 1, :) = temp_t(3, :) * JacobianTriangle( &
        & from="UNIT", &
        & to="BIUNIT")

    ELSE
      ans = temp_t
    END IF
  END IF

  IF (ALLOCATED(temp_t)) DEALLOCATE (temp_t)
ELSE
  CALL Errormsg( &
    & msg="This routine should be called for economical"// &
    & " quadrature points only, otherwise call QuadraturePoint_Triangle1()", &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="QuadraturePoint_Triangle2()", &
    & unitNo=stdout)
  RETURN
END IF
END PROCEDURE QuadraturePoint_Triangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE QuadratureMethods
