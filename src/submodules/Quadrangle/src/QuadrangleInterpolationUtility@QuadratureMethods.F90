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

SUBMODULE(QuadrangleInterpolationUtility) QuadratureMethods
USE LineInterpolationUtility, ONLY: QuadratureNumber_Line, &
                                    QuadraturePoint_Line_
USE MappingUtility, ONLY: FromBiUnitQuadrangle2Quadrangle_, &
                          FromBiUnitQuadrangle2UnitQuadrangle_, &
                          JacobianQuadrangle
USE StringUtility, ONLY: UpperCase

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                               QuadratureNumber_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadratureNumber_Quadrangle
ans(1) = QuadratureNumber_Line(order=p, quadType=quadType1)
ans(2) = QuadratureNumber_Line(order=q, quadType=quadType2)
END PROCEDURE QuadratureNumber_Quadrangle

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

END SUBMODULE QuadratureMethods
