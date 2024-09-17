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

SUBMODULE(LineInterpolationUtility) Methods
USE BaseType, ONLY: ipopt => TypeInterpolationOpt, &
                    qpopt => TypeQuadratureOpt, &
                    polyopt => TypePolynomialOpt, &
                    elmopt => TypeElemNameOpt

USE GlobalData, ONLY: stderr

USE StringUtility, ONLY: UpperCase

USE MappingUtility, ONLY: FromBiunitLine2Segment_, &
                          FromBiunitLine2Segment, &
                          FromUnitLine2BiUnitLine, &
                          FromUnitLine2BiUnitLine_

USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol, &
                                       GradientEvalAllOrthopol_, &
                                       EvalAllOrthopol, &
                                       EvalAllOrthopol_

USE InputUtility, ONLY: Input

USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde, &
                                     LagrangeCoeff, &
                                     LagrangeVandermonde_

USE ErrorHandling, ONLY: ErrorMsg

USE LegendrePolynomialUtility, ONLY: LegendreQuadrature

USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Quadrature

USE JacobiPolynomialUtility, ONLY: JacobiQuadrature

USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalQuadrature

USE Lapack_Method, ONLY: GetLU, LUSolve, GetInvMat

USE SortUtility, ONLY: HeapSort

USE F95_BLAS, ONLY: GEMM

#ifndef USE_BLAS95

USE SwapUtility, ONLY: Swap

#else

USE F95_BLAS, ONLY: Swap

#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       RefElemDomain_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Line
ans = "BIUNIT"
END PROCEDURE RefElemDomain_Line

!----------------------------------------------------------------------------
!                                                     QuadratureNumber_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadratureNumber_Line
SELECT CASE (quadType)
CASE (qpopt%GaussLegendre, qpopt%GaussChebyshev, &
      qpopt%GaussJacobi, qpopt%GaussUltraspherical)
  ans = 1_I4B + INT(order / 2, kind=I4B)
CASE DEFAULT
  ans = 2_I4B + INT(order / 2, kind=I4B)
END SELECT
END PROCEDURE QuadratureNumber_Line

!----------------------------------------------------------------------------
!                                                           ToVEFC_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE ToVEFC_Line
REAL(DFP) :: t1
INTEGER(I4B) :: np
np = SIZE(pt)
t1 = pt(np)
IF (np .GT. 2) THEN
  pt(3:np) = pt(2:np - 1)
  pt(2) = t1
END IF
END PROCEDURE ToVEFC_Line

!----------------------------------------------------------------------------
!                                                       LagrangeDegree_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Line
INTEGER(I4B) :: ii, n
n = LagrangeDOF_Line(order=order)
ALLOCATE (ans(n, 1))
DO ii = 1, n
  ans(ii, 1) = ii - 1
END DO
END PROCEDURE LagrangeDegree_Line

!----------------------------------------------------------------------------
!                                                          LagrangeDOF_Point
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Point
ans = 1_I4B
END PROCEDURE LagrangeDOF_Point

!----------------------------------------------------------------------------
!                                                         LagrangeDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Line
ans = order + 1
END PROCEDURE LagrangeDOF_Line

!----------------------------------------------------------------------------
!                                                         LagrangeInDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Line
ans = order - 1_I4B
END PROCEDURE LagrangeInDOF_Line

!----------------------------------------------------------------------------
!                                                         GetTotalDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Line
ans = order + 1
END PROCEDURE GetTotalDOF_Line

!----------------------------------------------------------------------------
!                                                         LagrangeInDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Line
ans = order - 1_I4B
END PROCEDURE GetTotalInDOF_Line

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line1
INTEGER(I4B) :: tsize

IF (order .LE. 1_I4B) THEN
  ALLOCATE (ans(0))
  RETURN
END IF

tsize = LagrangeInDOF_Line(order=order)
ALLOCATE (ans(tsize))
CALL EquidistanceInPoint_Line1_(order=order, xij=xij, ans=ans, tsize=tsize)

END PROCEDURE EquidistanceInPoint_Line1

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line1_
INTEGER(I4B) :: ii
REAL(DFP) :: avar

tsize = 0
IF (order .LE. 1_I4B) RETURN

tsize = LagrangeInDOF_Line(order=order)

avar = (xij(2) - xij(1)) / order

DO ii = 1, tsize
  ans(ii) = xij(1) + REAL(ii, kind=dfp) * avar
END DO

END PROCEDURE EquidistanceInPoint_Line1_

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line2
INTEGER(I4B) :: nrow, ncol

IF (order .LE. 1_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 1_I4B
END IF

ncol = LagrangeInDOF_Line(order=order)

ALLOCATE (ans(nrow, ncol))

CALL EquidistanceInPoint_Line2_(order=order, xij=xij, ans=ans, nrow=nrow, &
                                ncol=ncol)

END PROCEDURE EquidistanceInPoint_Line2

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line2_
INTEGER(I4B) :: ii
REAL(DFP) :: x0(3, 3)

nrow = 0; ncol = 0
IF (order .LE. 1_I4B) RETURN

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
  x0(1:nrow, 1) = xij(1:nrow, 1)
  x0(1:nrow, 2) = xij(1:nrow, 2)
ELSE
  nrow = 1_I4B
  x0(1, 1) = -1.0
  x0(1, 2) = 1.0
END IF

ncol = LagrangeInDOF_Line(order=order)

x0(1:nrow, 3) = (x0(1:nrow, 2) - x0(1:nrow, 1)) / order

DO ii = 1, ncol
  ans(1:nrow, ii) = x0(1:nrow, 1) + ii * x0(1:nrow, 3)
END DO
END PROCEDURE EquidistanceInPoint_Line2_

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line1
INTEGER(I4B) :: tsize

tsize = order + 1
ALLOCATE (ans(tsize))
CALL EquidistancePoint_Line1_(order=order, xij=xij, ans=ans, tsize=tsize)
END PROCEDURE EquidistancePoint_Line1

!----------------------------------------------------------------------------
!                                                   EquidistancePoint_Line_
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line1_
INTEGER(I4B) :: tempint

tsize = order + 1

SELECT CASE (order)
CASE (0)
  ans(1) = 0.5_DFP * (xij(1) + xij(2))

CASE (1)
  ans(1) = xij(1)
  ans(2) = xij(2)

CASE DEFAULT
  ans(1) = xij(1)
  ans(2) = xij(2)
  CALL EquidistanceInPoint_Line_(order=order, xij=xij, ans=ans(3:), &
                                 tsize=tempint)
END SELECT

END PROCEDURE EquidistancePoint_Line1_

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line2
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 1_I4B
END IF

ncol = order + 1
ALLOCATE (ans(nrow, ncol))

CALL EquidistancePoint_Line2_(order=order, xij=xij, ans=ans, nrow=nrow, &
                              ncol=ncol)
END PROCEDURE EquidistancePoint_Line2

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line2_
INTEGER(I4B) :: tempint

ncol = order + 1

SELECT CASE (order)

CASE (0)

  IF (PRESENT(xij)) THEN
    nrow = SIZE(xij, 1)
    ans(1:nrow, 1) = 0.5_DFP * (xij(1:nrow, 1) + xij(1:nrow, 2))
    RETURN
  END IF

  nrow = 1_I4B
  ans(1, 1) = 0.0_DFP

CASE (1)

  IF (PRESENT(xij)) THEN
    nrow = SIZE(xij, 1)
    ans(1:nrow, 1:2) = xij(1:nrow, 1:2)
    RETURN
  END IF

  nrow = 1
  ans(1, 1) = -1.0_DFP
  ans(1, 2) = 1.0_DFP

CASE DEFAULT

  IF (PRESENT(xij)) THEN
    nrow = SIZE(xij, 1)
    ans(1:nrow, 1:2) = xij(1:nrow, 1:2)
  ELSE
    nrow = 1
    ans(1, 1) = -1.0_DFP
    ans(1, 2) = 1.0_DFP
  END IF

  CALL EquidistanceInPoint_Line2_(order=order, xij=xij, ans=ans(:, 3:), &
                                  nrow=nrow, ncol=tempint)

END SELECT

END PROCEDURE EquidistancePoint_Line2_

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line1
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 1
END IF

ncol = order + 1

ALLOCATE (ans(nrow, ncol))

CALL InterpolationPoint_Line1_(order=order, ipType=ipType, ans=ans, &
                  nrow=nrow, ncol=ncol, layout=layout, xij=xij, alpha=alpha, &
                               beta=beta, lambda=lambda)

END PROCEDURE InterpolationPoint_Line1

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line2
INTEGER(I4B) :: tsize
tsize = order + 1
ALLOCATE (ans(tsize))
CALL InterpolationPoint_Line2_(order=order, ipType=ipType, &
              xij=xij, layout=layout, alpha=alpha, beta=beta, lambda=lambda, &
                               ans=ans, tsize=tsize)
END PROCEDURE InterpolationPoint_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line1_
REAL(DFP) :: temp(64)

IF (order .EQ. 0_I4B) THEN
  CALL EquidistancePoint_Line_(xij=xij, order=order, ans=ans, nrow=nrow, &
                               ncol=ncol)
  RETURN
END IF

CALL handle_error
!! handle_error is defined in this routine, see below

ncol = order + 1

SELECT CASE (ipType)

CASE (ipopt%Equidistance)
  CALL EquidistancePoint_Line_(xij=xij, order=order, nrow=nrow, ncol=ncol, &
                               ans=ans)
  CALL handle_increasing

CASE (ipopt%GaussLegendre)
  CALL LegendreQuadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%Gauss)
  CALL handle_non_equidistance

CASE (ipopt%GaussChebyshev)
  CALL Chebyshev1Quadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%Gauss)
  CALL handle_non_equidistance

CASE (ipopt%GaussLegendreLobatto)
 CALL LegendreQuadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%GaussLobatto)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%GaussLobatto)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussJacobi)
  CALL JacobiQuadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%Gauss, &
                        alpha=alpha, beta=beta)
  CALL handle_non_equidistance

CASE (ipopt%GaussJacobiLobatto)
 CALL JacobiQuadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%GaussLobatto, &
                        alpha=alpha, beta=beta)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussUltraspherical)
CALL UltrasphericalQuadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%Gauss, &
                                lambda=lambda)
  CALL handle_non_equidistance

CASE (ipopt%GaussUltrasphericalLobatto)
  CALL UltrasphericalQuadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%GaussLobatto, &
                                lambda=lambda)

  CALL handle_vefc
  CALL handle_non_equidistance

CASE DEFAULT
  CALL ErrorMsg(msg="Unknown iptype", routine="InterpolationPoint_Line1_()", &
                file=__FILE__, line=__LINE__, unitno=stderr)
END SELECT

CONTAINS

SUBROUTINE handle_vefc
  REAL(DFP) :: t1

  IF (layout(1:2) .EQ. "VE") THEN
    t1 = temp(order + 1)
    IF (order .GE. 2) THEN
      temp(3:order + 1) = temp(2:order)
    END IF
    temp(2) = t1
  END IF

END SUBROUTINE handle_vefc

SUBROUTINE handle_increasing
  INTEGER(I4B) :: ii

  IF (layout(1:2) .EQ. "IN") THEN
    DO ii = 1, nrow
      CALL HeapSort(ans(ii, :))
    END DO
  END IF
END SUBROUTINE

SUBROUTINE handle_non_equidistance
  IF (PRESENT(xij)) THEN
  CALL FromBiunitLine2Segment_(xin=temp(1:ncol), x1=xij(:, 1), x2=xij(:, 2), &
                                 ans=ans, nrow=nrow, ncol=ncol)
  ELSE
    nrow = 1
    ans(1, 1:ncol) = temp(1:ncol)
  END IF

END SUBROUTINE handle_non_equidistance

SUBROUTINE handle_error

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok
  CHARACTER(:), ALLOCATABLE :: msg

  SELECT CASE (ipType)
  CASE (ipopt%GaussJacobi, ipopt%GaussJacobiLobatto)
    isok = PRESENT(alpha) .AND. PRESENT(beta)
    IF (.NOT. isok) THEN
      msg = "alpha and beta should be present for ipType=GaussJacobi"

      CALL ErrorMsg(msg=msg, file=__FILE__, &
                    routine="InterpolationPoint_Line1_()", &
                    line=__LINE__, unitno=stderr)
    END IF

  CASE (ipopt%GaussUltraSpherical, ipopt%GaussUltraSphericalLobatto)
    isok = PRESENT(lambda)
    IF (.NOT. isok) THEN
      msg = "lambda should be present for ipType=GaussUltraSpherical"
      CALL ErrorMsg(msg=msg, file=__FILE__, &
                    routine="InterpolationPoint_Line1_()", &
                    line=__LINE__, unitno=stderr)
    END IF
  END SELECT

#endif

END SUBROUTINE handle_error

END PROCEDURE InterpolationPoint_Line1_

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line2_
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line2_
tsize = order + 1
IF (order .EQ. 0_I4B) THEN
  ans(1) = 0.5_DFP * (xij(1) + xij(2))
  RETURN
END IF

CALL handle_error

SELECT CASE (ipType)

CASE (ipopt%Equidistance)
  CALL EquidistancePoint_Line_(xij=xij, order=order, tsize=tsize, ans=ans)

  IF (layout(1:2) .EQ. "IN") CALL HeapSort(ans(1:tsize))

CASE (ipopt%GaussLegendre)
  CALL LegendreQuadrature(n=tsize, pt=ans, quadType=ipopt%Gauss)
  CALL handle_non_equidistance

CASE (ipopt%GaussChebyshev)
  CALL Chebyshev1Quadrature(n=tsize, pt=ans, quadType=ipopt%Gauss)
  CALL handle_non_equidistance

CASE (ipopt%GaussJacobi)
  CALL JacobiQuadrature(n=tsize, pt=ans, quadType=ipopt%Gauss, alpha=alpha, &
                        beta=beta)
  CALL handle_non_equidistance

CASE (ipopt%GaussUltraspherical)
  CALL UltrasphericalQuadrature(n=tsize, pt=ans, quadType=ipopt%Gauss, &
                                lambda=lambda)
  CALL handle_non_equidistance

CASE (ipopt%GaussLegendreLobatto)
  CALL LegendreQuadrature(n=tsize, pt=ans, quadType=ipopt%GaussLobatto)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=tsize, pt=ans, quadType=ipopt%GaussLobatto)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussJacobiLobatto)
  CALL JacobiQuadrature(n=tsize, pt=ans, quadType=ipopt%GaussLobatto, alpha=alpha, &
                        beta=beta)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussUltrasphericalLobatto)
 CALL UltrasphericalQuadrature(n=tsize, pt=ans, quadType=ipopt%GaussLobatto, &
                                lambda=lambda)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE DEFAULT
  CALL ErrorMsg(msg="Unknown iptype", routine="InterpolationPoint_Line2", &
                file=__FILE__, line=__LINE__, unitno=stderr)
END SELECT

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE handle_vefc
  REAL(DFP) :: t1

  IF (layout(1:2) .EQ. "VE") THEN
    t1 = ans(order + 1)
    IF (order .GE. 2) THEN
      ans(3:) = ans(2:order)
    END IF
    ans(2) = t1
  END IF

END SUBROUTINE handle_vefc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE handle_non_equidistance
  CALL FromBiunitLine2Segment_(xin=ans, x1=xij(1), x2=xij(2), &
                               ans=ans, tsize=tsize)

END SUBROUTINE handle_non_equidistance

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE handle_error

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok
  CHARACTER(:), ALLOCATABLE :: msg

  SELECT CASE (ipType)
  CASE (ipopt%GaussJacobi, ipopt%GaussJacobiLobatto)
    isok = PRESENT(alpha) .AND. PRESENT(beta)
    IF (.NOT. isok) THEN
      msg = "alpha and beta should be present for ipType=GaussJacobi"

      CALL ErrorMsg(msg=msg, file=__FILE__, &
                    routine="InterpolationPoint_Line1_()", &
                    line=__LINE__, unitno=stderr)
    END IF

  CASE (ipopt%GaussUltraSpherical, ipopt%GaussUltraSphericalLobatto)
    isok = PRESENT(lambda)
    IF (.NOT. isok) THEN
      msg = "lambda should be present for ipType=GaussUltraSpherical"
      CALL ErrorMsg(msg=msg, file=__FILE__, &
                    routine="InterpolationPoint_Line1_()", &
                    line=__LINE__, unitno=stderr)
    END IF
  END SELECT

#endif

END SUBROUTINE handle_error

END PROCEDURE InterpolationPoint_Line2_

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line1
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Line1_(order=order, i=i, xij=xij, ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Line1

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line1_
REAL(DFP) :: v(SIZE(xij, 2), SIZE(xij, 2))
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info, nrow, ncol

tsize = order + 1
CALL LagrangeVandermonde_(order=order, xij=xij, elemType=elmopt%Line, &
                          ans=v, nrow=nrow, ncol=ncol)

CALL GetLU(A=v, IPIV=ipiv, info=info)

ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP

CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)

END PROCEDURE LagrangeCoeff_Line1_

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line2
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Line2_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                          ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Line2

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line2_
REAL(DFP) :: vtemp(SIZE(v, 1), SIZE(v, 2))
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

tsize = order + 1

vtemp = v
! ipiv = 0

CALL GetLU(A=vtemp, IPIV=ipiv, info=info)

ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP

CALL LUSolve(A=vtemp, B=ans(1:tsize), IPIV=ipiv, info=info)

END PROCEDURE LagrangeCoeff_Line2_

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Line3_(order=order, i=i, v=v, ipiv=ipiv, ans=ans, &
                          tsize=tsize)
END PROCEDURE LagrangeCoeff_Line3

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line3_
INTEGER(I4B) :: info
tsize = 1 + order
ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Line3_

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line4
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Line4_(order=order, xij=xij, ans=ans, nrow=nrow, &
                          ncol=ncol)
END PROCEDURE LagrangeCoeff_Line4

!----------------------------------------------------------------------------
!                                                        LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line4_
CALL LagrangeVandermonde_(order=order, xij=xij, elemType=elmopt%Line, &
                          ans=ans, nrow=nrow, ncol=ncol)
CALL GetInvMat(ans(1:nrow, 1:ncol))
END PROCEDURE LagrangeCoeff_Line4_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line5
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff_Line5_(order=order, xij=xij, basisType=basisType, &
         alpha=alpha, beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeCoeff_Line5

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Line5_
IF (basisType .EQ. polyopt%Monomial) THEN
  CALL LagrangeCoeff_Line_(order=order, xij=xij, ans=ans, nrow=nrow, &
                           ncol=ncol)
  RETURN
END IF

CALL EvalAllOrthopol_(n=order, x=xij(1, :), &
                      orthopol=basisType, &
                      alpha=alpha, beta=beta, lambda=lambda, &
                      ans=ans, nrow=nrow, ncol=ncol)

CALL GetInvMat(ans(1:nrow, 1:ncol))
END PROCEDURE LagrangeCoeff_Line5_

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line1
INTEGER(I4B) :: tsize
CALL LagrangeEvalAll_Line1_(order=order, x=x, xij=xij, coeff=coeff, &
           firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
                            lambda=lambda, ans=ans, tsize=tsize)
END PROCEDURE LagrangeEvalAll_Line1

!----------------------------------------------------------------------------
!                                                     LagrangeEvalAll_Line_
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line1_
LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2)), x1(1)
INTEGER(I4B) :: ii, orthopol0, nrow, ncol

tsize = SIZE(xij, 2)

#ifdef DEBUG_VER

IF (tsize .NE. order + 1) THEN
  CALL Errormsg(msg="Size(xij, 1) .NE. order+1 ", &
                routine="LagrangeEvalAll_Line2", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN
END IF

#endif

orthopol0 = Input(default=polyopt%Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

! make coeff0

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    CALL LagrangeCoeff_Line_(order=order, xij=xij, &
                 basisType=orthopol0, alpha=alpha, beta=beta, lambda=lambda, &
                             ans=coeff, nrow=nrow, ncol=ncol)
  END IF

  ! coeff0(1:nrow, 1:ncol) = TRANSPOSE(coeff(1:nrow, 1:ncol))
  coeff0(1:tsize, 1:tsize) = coeff(1:tsize, 1:tsize)

ELSE

  CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                           alpha=alpha, beta=beta, lambda=lambda, &
                           ans=coeff0, nrow=nrow, ncol=ncol)

  ! coeff0(1:nrow, 1:ncol) = TRANSPOSE(coeff0(1:nrow, 1:ncol))
END IF

IF (orthopol0 .EQ. polyopt%monomial) THEN

  xx(1, 1) = 1.0_DFP
  DO ii = 1, order
    xx(1, ii + 1) = xx(1, ii) * x
  END DO

ELSE

  x1(1) = x
  CALL EvalAllOrthopol_(n=order, x=x1, orthopol=orthopol0, &
                        alpha=alpha, beta=beta, lambda=lambda, &
                        ans=xx, nrow=nrow, ncol=ncol)

END IF

DO CONCURRENT(ii=1:tsize)
  ans(ii) = DOT_PRODUCT(coeff0(:, ii), xx(1, :))
END DO

END PROCEDURE LagrangeEvalAll_Line1_

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line2
INTEGER(I4B) :: nrow, ncol
CALL LagrangeEvalAll_Line2_(order=order, x=x, xij=xij, coeff=coeff, &
           firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
                            lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeEvalAll_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Line2_
LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(SIZE(x, 2), SIZE(xij, 2))
INTEGER(I4B) :: ii, orthopol0, aint, bint

nrow = SIZE(x, 2)
ncol = SIZE(xij, 2)

#ifdef DEBUG_VER

IF (ncol .NE. order + 1) THEN
  CALL Errormsg(msg="Size(xij, 1) .NE. order+1 ", &
                routine="LagrangeEvalAll_Line2", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN
END IF

#endif

orthopol0 = Input(default=polyopt%Monomial, option=basisType)
firstCall0 = Input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN

  IF (firstCall0) THEN
    ! coeff = LagrangeCoeff_Line(&
    CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
       alpha=alpha, beta=beta, lambda=lambda, ans=coeff, nrow=aint, ncol=bint)
  END IF

  coeff0(1:ncol, 1:ncol) = coeff(1:ncol, 1:ncol)

ELSE

  ! coeff0 = LagrangeCoeff_Line(&
  CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
      alpha=alpha, beta=beta, lambda=lambda, ans=coeff0, nrow=aint, ncol=bint)

END IF

IF (orthopol0 .EQ. polyopt%monomial) THEN

  xx(:, 1) = 1.0_DFP
  DO ii = 1, order
    xx(:, ii + 1) = xx(:, ii) * x(1, :)
  END DO

ELSE

  ! xx = EvalAllOrthopol(
  CALL EvalAllOrthopol_(n=order, x=x(1, :), orthopol=orthopol0, alpha=alpha, &
                       beta=beta, lambda=lambda, ans=xx, nrow=aint, ncol=bint)

END IF

! ans = MATMUL(xx, coeff0)
CALL GEMM(C=ans, alpha=1.0_DFP, A=xx, B=coeff0)

END PROCEDURE LagrangeEvalAll_Line2_

!----------------------------------------------------------------------------
!                                                               EvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line1
INTEGER(I4B) :: tsize
CALL BasisEvalAll_Line1_(order=order, x=x, ans=ans, tsize=tsize, &
               refline=refline, basistype=basistype, alpha=alpha, beta=beta, &
                         lambda=lambda)
END PROCEDURE BasisEvalAll_Line1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line1_
#ifdef DEBUG_VER
CHARACTER(1) :: astr
#endif

INTEGER(I4B) :: ii, basisType0, nrow, ncol
REAL(DFP) :: temp(1, 100), x1(1)

tsize = order + 1

#ifdef DEBUG_VER

astr = UpperCase(refLine(1:1))
IF (astr .EQ. "U") THEN
  CALL Errormsg(msg="refLine should be BIUNIT", &
                routine="BasisEvalAll_Line1", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN
END IF

#endif

basisType0 = Input(default=polyopt%Monomial, option=basisType)

SELECT CASE (basisType0)

CASE (polyopt%Monomial)
  ans(1) = 1.0_DFP
  DO ii = 1, order
    ans(ii + 1) = ans(ii) * x
  END DO

CASE DEFAULT

#ifdef DEBUG_VER

  IF (basisType0 .EQ. polyopt%Jacobi) THEN
    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg( &
        msg="alpha and beta should be present for basisType=Jacobi", &
        routine="BasisEvalAll_Line1", &
        file=__FILE__, line=__LINE__, unitno=stderr)
      RETURN
    END IF
  END IF

  IF (basisType0 .EQ. polyopt%Ultraspherical) THEN
    IF (.NOT. PRESENT(lambda)) THEN
      CALL Errormsg( &
        msg="lambda should be present for basisType=Ultraspherical", &
        routine="BasisEvalAll_Line1", &
        file=__FILE__, line=__LINE__, unitno=stderr)
      RETURN
    END IF
  END IF

  IF (order + 1 .GT. SIZE(temp, 2)) THEN
    CALL Errormsg( &
      msg="order+1 is greater than number of col in temp", &
      routine="BasisEvalAll_Line1_", &
      file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

#endif

  x1(1) = x
  CALL EvalAllOrthopol_(n=order, x=x1, orthopol=basisType0, alpha=alpha, &
                     beta=beta, lambda=lambda, ans=temp, nrow=nrow, ncol=ncol)

  ans(1:tsize) = temp(1, 1:tsize)

END SELECT

END PROCEDURE BasisEvalAll_Line1_

!----------------------------------------------------------------------------
!                                                 BasisGradientEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line1
INTEGER(I4B) :: tsize
CALL BasisGradientEvalAll_Line1_(order=order, x=x, refLine=refLine, &
        basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                                 tsize=tsize)
END PROCEDURE BasisGradientEvalAll_Line1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line1_
INTEGER(I4B) :: ii, basisType0
CHARACTER(:), ALLOCATABLE :: astr
REAL(DFP) :: areal, breal, x1(1), temp(1, order + 1)

astr = UpperCase(refline)

tsize = order + 1

#ifdef DEBUG_VER

IF (astr .EQ. "UNIT") THEN
  CALL Errormsg(msg="refLine should be BIUNIT", &
                routine="BasisGradientEvalAll_Line1", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN
END IF

#endif

basisType0 = Input(default=polyopt%Monomial, option=basisType)
SELECT CASE (basisType0)

CASE (polyopt%Monomial)

  ans(1) = 0.0_DFP
  DO ii = 1, order
    areal = REAL(ii, kind=DFP)
    breal = x**(ii - 1)
    ans(ii + 1) = areal * breal
  END DO

CASE DEFAULT

#ifdef DEBUG_VER

  IF (basisType0 .EQ. polyopt%Jacobi) THEN

    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg( &
        msg="alpha and beta should be present for basisType=Jacobi", &
        routine="BasisGradientEvalAll_Line1", &
        file=__FILE__, line=__LINE__, unitno=stderr)
      RETURN
    END IF

  END IF

  IF (basisType0 .EQ. polyopt%Ultraspherical) THEN

    IF (.NOT. PRESENT(lambda)) THEN
      CALL Errormsg( &
        msg="lambda should be present for basisType=Ultraspherical", &
        routine="BasisGradientEvalAll_Line1", &
        file=__FILE__, line=__LINE__, unitno=stderr)
      RETURN
    END IF

  END IF

#endif

  x1(1) = x
  CALL GradientEvalAllOrthopol_(n=order, x=x1, orthopol=basisType0, &
         alpha=alpha, beta=beta, lambda=lambda, ans=temp, nrow=ii, ncol=tsize)

  ans(1:tsize) = temp(1, 1:tsize)

END SELECT

END PROCEDURE BasisGradientEvalAll_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line2
INTEGER(I4B) :: nrow, ncol
CALL BasisGradientEvalAll_Line2_(order=order, x=x, ans=ans, nrow=nrow, &
    ncol=ncol, refLine=refLine, basisType=basisType, alpha=alpha, beta=beta, &
                                 lambda=lambda)
END PROCEDURE BasisGradientEvalAll_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisGradientEvalAll_Line2_
INTEGER(I4B) :: ii, basisType0, jj
REAL(DFP) :: areal, breal
CHARACTER(:), ALLOCATABLE :: astr

nrow = SIZE(x)
ncol = 1 + order

astr = UpperCase(refLine)

#ifdef DEBUG_VER

IF (astr .EQ. "UNIT") THEN
  CALL Errormsg(msg="refLine should be BIUNIT", &
                routine="BasisGradientEvalAll_Line2", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN
END IF

#endif

basisType0 = Input(default=polyopt%Monomial, option=basisType)
SELECT CASE (basisType0)

CASE (polyopt%Monomial)

  ans(1:nrow, 1) = 0.0_DFP

  DO ii = 1, order
    areal = REAL(ii, kind=dfp)

    DO jj = 1, nrow

      breal = x(jj)**(ii - 1)
      ans(jj, ii + 1) = areal * breal

    END DO

  END DO

CASE DEFAULT

#ifdef DEBUG_VER

  IF (basisType0 .EQ. polyopt%Jacobi) THEN
    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg( &
        msg="alpha and beta should be present for basisType=Jacobi", &
        routine="BasisGradientEvalAll_Line2", &
        file=__FILE__, line=__LINE__, unitno=stderr)
      RETURN
    END IF
  END IF

  IF (basisType0 .EQ. polyopt%Ultraspherical) THEN
    IF (.NOT. PRESENT(lambda)) THEN
      CALL Errormsg( &
        msg="lambda should be present for basisType=Ultraspherical", &
        routine="BasisGradientEvalAll_Line2", &
        file=__FILE__, line=__LINE__, unitno=stderr)
      RETURN
    END IF
  END IF

#endif

  ! ans = GradientEvalAllOrthopol(&
  CALL GradientEvalAllOrthopol_(n=order, x=x, orthopol=basisType0, &
         alpha=alpha, beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)

END SELECT

END PROCEDURE BasisGradientEvalAll_Line2_

!----------------------------------------------------------------------------
!                                                        BasisEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line2
INTEGER(I4B) :: nrow, ncol
CALL BasisEvalAll_Line2_(order=order, x=x, ans=ans, nrow=nrow, ncol=ncol, &
                         refline=refline, basistype=basistype, &
                         alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE BasisEvalAll_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BasisEvalAll_Line2_
#ifdef DEBUG_VER
CHARACTER(1) :: astr

#endif

INTEGER(I4B) :: ii, basisType0

nrow = SIZE(x)
ncol = order + 1

#ifdef DEBUG_VER

astr = UpperCase(refline(1:1))

IF (astr .EQ. "U") THEN
  CALL Errormsg(msg="refLine should be BIUNIT", &
                routine="BasisEvalAll_Line2", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN
END IF

#endif

basisType0 = Input(default=polyopt%Monomial, option=basisType)

SELECT CASE (basisType0)

CASE (polyopt%Monomial)
  ans(1:nrow, 1) = 1.0_DFP
  DO ii = 1, order
    ans(1:nrow, ii + 1) = ans(1:nrow, ii) * x
  END DO

CASE DEFAULT

#ifdef DEBUG_VER

  IF (basisType0 .EQ. polyopt%Jacobi) THEN
    IF (.NOT. PRESENT(alpha) .OR. .NOT. PRESENT(beta)) THEN
      CALL Errormsg( &
        msg="alpha and beta should be present for basisType=Jacobi", &
        routine="BasisEvalAll_Line2", &
        file=__FILE__, line=__LINE__, unitno=stderr)
      RETURN
    END IF
  END IF

  IF (basisType0 .EQ. polyopt%Ultraspherical) THEN
    IF (.NOT. PRESENT(lambda)) THEN
      CALL Errormsg( &
        msg="lambda should be present for basisType=Ultraspherical", &
        routine="BasisEvalAll_Line2", &
        file=__FILE__, line=__LINE__, unitno=stderr)
      RETURN
    END IF
  END IF

#endif

  CALL EvalAllOrthopol_(n=order, x=x, orthopol=basisType0, alpha=alpha, &
                        beta=beta, lambda=lambda, ans=ans, nrow=nrow, &
                        ncol=ncol)
END SELECT

END PROCEDURE BasisEvalAll_Line2_

!----------------------------------------------------------------------------
!                                                  BasisGradientEvalAll_Line
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line1
INTEGER(I4B) :: nips(1), nrow, ncol

nips(1) = QuadratureNumber_Line(order=order, quadType=quadType)

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 1
END IF

nrow = nrow + 1
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

CALL QuadraturePoint_Line1_(nips=nips, quadType=quadType, layout=layout, &
         xij=xij, alpha=alpha, beta=beta, lambda=lambda, ans=ans, nrow=nrow, &
                            ncol=ncol)

END PROCEDURE QuadraturePoint_Line1

!----------------------------------------------------------------------------
!                                                       QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line2
INTEGER(I4B) :: nips(1), nrow, ncol
REAL(DFP) :: x12(1, 2)

nips(1) = QuadratureNumber_Line(order=order, quadType=quadType)
nrow = 2
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

x12(1, 1:2) = xij(1:2)

CALL QuadraturePoint_Line1_(nips=nips, quadType=quadType, layout=layout, &
         xij=x12, alpha=alpha, beta=beta, lambda=lambda, ans=ans, nrow=nrow, &
                            ncol=ncol)
END PROCEDURE QuadraturePoint_Line2

!----------------------------------------------------------------------------
!                                                       QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line3
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 1
END IF

nrow = nrow + 1
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

CALL QuadraturePoint_Line1_(nips=nips, quadType=quadType, layout=layout, &
         xij=xij, alpha=alpha, beta=beta, lambda=lambda, ans=ans, nrow=nrow, &
                            ncol=ncol)

END PROCEDURE QuadraturePoint_Line3

!----------------------------------------------------------------------------
!                                                        QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line4
REAL(DFP) :: x12(1, 2)
INTEGER(I4B) :: nrow, ncol

nrow = 2
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

x12(1, 1:2) = xij(1:2)

CALL QuadraturePoint_Line1_(nips=nips, quadType=quadType, layout=layout, &
         xij=x12, alpha=alpha, beta=beta, lambda=lambda, ans=ans, nrow=nrow, &
                            ncol=ncol)

END PROCEDURE QuadraturePoint_Line4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line1_
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: np, nsd, ii, jj
REAL(DFP) :: areal
LOGICAL(LGT) :: changeLayout

nrow = 0
ncol = 0

#ifdef DEBUG_VER

SELECT CASE (quadType)
CASE (ipopt%GaussJacobi, ipopt%GaussJacobiLobatto, &
      ipopt%GaussJacobiRadauLeft, ipopt%GaussJacobiRadauRight)

  isok = PRESENT(alpha) .AND. PRESENT(beta)

  IF (.NOT. isok) THEN
    CALL ErrorMsg(routine="QuadraturePoint_Line3", &
      msg="alpha and beta should be present for quadType=ipopt%GaussJacobi", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

CASE (ipopt%GaussUltraSpherical, ipopt%GaussUltraSphericalLobatto, &
      ipopt%GaussUltraSphericalRadauLeft, ipopt%GaussUltraSphericalRadauRight)

  isok = PRESENT(lambda)

  IF (.NOT. isok) THEN
    CALL ErrorMsg(routine="QuadraturePoint_Line3", &
      msg="lambda should be present for quadType=ipopt%GaussUltraspherical", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

END SELECT

#endif

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
ELSE
  nsd = 1
END IF

np = nips(1)
nrow = nsd + 1
ncol = nips(1)

changeLayout = .FALSE.
IF (layout(1:1) .EQ. "V") changeLayout = .TRUE.

SELECT CASE (quadType)

CASE (ipopt%GaussLegendre)
  CALL LegendreQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                          quadType=ipopt%Gauss)

CASE (ipopt%GaussLegendreRadauLeft)
  CALL LegendreQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                          quadType=ipopt%GaussRadauLeft)

CASE (ipopt%GaussLegendreRadauRight)
  CALL LegendreQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                          quadType=ipopt%GaussRadauRight)

CASE (ipopt%GaussLegendreLobatto)
  CALL LegendreQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                          quadType=ipopt%GaussLobatto)

CASE (ipopt%GaussChebyshev)
  CALL Chebyshev1Quadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                            quadType=ipopt%Gauss)

CASE (ipopt%GaussChebyshevRadauLeft)
  CALL Chebyshev1Quadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                            quadType=ipopt%GaussRadauLeft)

CASE (ipopt%GaussChebyshevRadauRight)
  CALL Chebyshev1Quadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                            quadType=ipopt%GaussRadauRight)

CASE (ipopt%GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                            quadType=ipopt%GaussLobatto)

CASE (ipopt%GaussJacobi)
  CALL JacobiQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                        quadType=ipopt%Gauss, alpha=alpha, beta=beta)

CASE (ipopt%GaussJacobiRadauLeft)
  CALL JacobiQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                        quadType=ipopt%GaussRadauLeft, alpha=alpha, beta=beta)

CASE (ipopt%GaussJacobiRadauRight)
  CALL JacobiQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                       quadType=ipopt%GaussRadauRight, alpha=alpha, beta=beta)

CASE (ipopt%GaussJacobiLobatto)
  CALL JacobiQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                        quadType=ipopt%GaussLobatto, alpha=alpha, beta=beta)

CASE (ipopt%GaussUltraspherical)
CALL UltrasphericalQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                                quadType=ipopt%Gauss, lambda=lambda)

CASE (ipopt%GaussUltrasphericalRadauLeft)
CALL UltrasphericalQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                                quadType=ipopt%GaussRadauLeft, lambda=lambda)

CASE (ipopt%GaussUltrasphericalRadauRight)
CALL UltrasphericalQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                                quadType=ipopt%GaussRadauRight, lambda=lambda)

CASE (ipopt%GaussUltrasphericalLobatto)
CALL UltrasphericalQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                                quadType=ipopt%GaussLobatto, lambda=lambda)

CASE DEFAULT
  CALL ErrorMsg(msg="Unknown iptype", routine="QuadraturePoint_Line3", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN
END SELECT

IF (changeLayout) THEN
  CALL ToVEFC_Line(ans(1, 1:ncol))
  CALL ToVEFC_Line(ans(nrow, 1:ncol))
END IF

IF (PRESENT(xij)) THEN
  CALL FromBiunitLine2Segment_(xin=ans(1, 1:nrow), x1=xij(:, 1), &
                               x2=xij(:, 2), ans=ans, nrow=ii, ncol=jj)

  areal = NORM2(xij(:, 2) - xij(:, 1)) / 2.0_DFP

  DO CONCURRENT(ii=1:ncol)
    ans(nrow, ii) = ans(nrow, ii) * areal
  END DO

  RETURN
END IF

END PROCEDURE QuadraturePoint_Line1_

!----------------------------------------------------------------------------
!                                              LagrangeGradientEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Line1
LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(order + 1, order + 1), xx(SIZE(x, 2), order + 1)
INTEGER(I4B) :: ii, orthopol0

orthopol0 = input(default=polyopt%Monomial, option=basisType)
firstCall0 = input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Line(&
      & order=order, &
      & xij=xij, &
      & basisType=orthopol0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda)
  END IF
  coeff0 = coeff
ELSE
  coeff0 = LagrangeCoeff_Line(&
    & order=order, &
    & xij=xij, &
    & basisType=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda &
    & )
END IF

SELECT CASE (orthopol0)
CASE (polyopt%Monomial)

  IF (SIZE(xij, 2) .NE. order + 1) THEN
    CALL Errormsg(&
      & msg="size(xij, 2) is not same as order+1", &
      & file=__FILE__, &
      & routine="LagrangeGradientEvalAll_Line1", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF

  DO ii = 0, order
    xx(:, ii + 1) = REAL(ii, kind=DFP) * x(1, :)**(MAX(ii - 1_I4B, 0_I4B))
  END DO

CASE DEFAULT
  xx = GradientEvalAllOrthopol(&
    & n=order, &
    & x=x(1, :), &
    & orthopol=orthopol0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda)
END SELECT

ans(:, :, 1) = MATMUL(xx, coeff0)

END PROCEDURE LagrangeGradientEvalAll_Line1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Line1_
LOGICAL(LGT) :: firstCall0
REAL(DFP) :: coeff0(order + 1, order + 1), xx(SIZE(x, 2), order + 1), areal
INTEGER(I4B) :: ii, orthopol0, indx(2), jj

dim1 = SIZE(x, 2)
dim2 = SIZE(xij, 2)
dim3 = 1
!! ans(SIZE(x, 2), SIZE(xij, 2), 1)

orthopol0 = input(default=polyopt%Monomial, option=basisType)
firstCall0 = input(default=.TRUE., option=firstCall)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN

    ! coeff = LagrangeCoeff_Line(&
    CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                           alpha=alpha, beta=beta, lambda=lambda, ans=coeff, &
                             nrow=indx(1), ncol=indx(2))

  END IF

  coeff0(1:dim2, 1:dim2) = coeff(1:dim2, 1:dim2)

ELSE

  ! coeff0 = LagrangeCoeff_Line(&
  CALL LagrangeCoeff_Line_(order=order, xij=xij, basisType=orthopol0, &
                          alpha=alpha, beta=beta, lambda=lambda, ans=coeff0, &
                           nrow=indx(1), ncol=indx(2))
END IF

SELECT CASE (orthopol0)
CASE (polyopt%Monomial)

#ifdef DEBUG_VER

  IF (dim2 .NE. order + 1) THEN
    CALL Errormsg(msg="size(xij, 2) is not same as order+1", &
                  routine="LagrangeGradientEvalAll_Line1", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

#endif

  DO ii = 0, order
    indx(1) = MAX(ii - 1_I4B, 0_I4B)
    areal = REAL(ii, kind=DFP)
    DO jj = 1, dim1
      xx(jj, ii + 1) = areal * (x(1, jj)**(indx(1)))
    END DO
  END DO

CASE DEFAULT

  ! xx(1:dim1, 1:dim2) = GradientEvalAllOrthopol(n=order, x=x(1, :), &
  CALL GradientEvalAllOrthopol_(n=order, x=x(1, :), orthopol=orthopol0, &
          alpha=alpha, beta=beta, lambda=lambda, ans=xx, nrow=dim1, ncol=dim2)

END SELECT

! ans(:, :, 1) = MATMUL(xx, coeff0)
CALL GEMM(C=ans(1:dim1, 1:dim2, 1), alpha=1.0_DFP, A=xx, B=coeff0)

END PROCEDURE LagrangeGradientEvalAll_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Line1
INTEGER(I4B) :: nrow, ncol
CALL HeirarchicalBasis_Line1_(order=order, xij=xij, refLine=refLine, &
                              ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Line1

!----------------------------------------------------------------------------
!                                                        BasisEvalAll_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Line1_
INTEGER(I4B), PARAMETER :: orient = 1
CALL HeirarchicalBasis_Line2_(order=order, xij=xij, refLine=refLine, &
                              orient=orient, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Line2_
CHARACTER(1) :: astr
REAL(DFP) :: temp(SIZE(xij, 2)), o1
INTEGER(I4B) :: ii, k

o1 = REAL(orient, kind=DFP)
astr = UpperCase(refLine(1:1))

! nrow = SIZE(xij, 2)
! ncol = order + 1

SELECT CASE (astr)
CASE ("U")
  CALL FromUnitLine2BiUnitLine_(xin=xij(1, :), ans=temp, tsize=nrow)
  CALL EvalAllOrthopol_(n=order, x=temp, orthopol=polyopt%Lobatto, ans=ans, &
                        nrow=nrow, ncol=ncol)

CASE ("B")
  CALL EvalAllOrthopol_(n=order, x=xij(1, :), orthopol=polyopt%Lobatto, &
                        ans=ans, nrow=nrow, ncol=ncol)

CASE DEFAULT
  nrow = 0
  ncol = 0
END SELECT

DO CONCURRENT(k=2:order, ii=1:nrow)
  ans(ii, k + 1) = (o1**k) * ans(ii, k + 1)
END DO

END PROCEDURE HeirarchicalBasis_Line2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line1
INTEGER(I4B) :: dim1, dim2, dim3
CALL HeirarchicalGradientBasis_Line1_(order=order, xij=xij, refLine=refLine, &
                                     ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE HeirarchicalGradientBasis_Line1

!----------------------------------------------------------------------------
!                                            HeirarchicalGradientBasis_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line1_
INTEGER(I4B), PARAMETER :: orient = 1
CALL HeirarchicalGradientBasis_Line2_(order=order, xij=xij, refLine=refLine, &
                      orient=orient, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE HeirarchicalGradientBasis_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line2
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(xij, 2)
dim2 = order + 1
dim3 = 1

ALLOCATE (ans(dim1, dim2, dim3))

CALL HeirarchicalGradientBasis_Line2_(order=order, xij=xij, refLine=refLine, &
                      orient=orient, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

END PROCEDURE HeirarchicalGradientBasis_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalGradientBasis_Line2_
CHARACTER(1) :: astr
REAL(DFP) :: temp(SIZE(xij, 2)), o1
INTEGER(I4B) :: ii, jj, k

o1 = REAL(orient, kind=DFP)
astr = UpperCase(refLine(1:1))

dim3 = 1

SELECT CASE (astr)

CASE ("U")
  CALL FromUnitLine2BiUnitLine_(xin=xij(1, :), ans=temp, tsize=dim1)
  CALL GradientEvalAllOrthopol_(n=order, x=temp, orthopol=polyopt%Lobatto, &
                                ans=ans(:, :, 1), nrow=dim1, ncol=dim2)

  DO CONCURRENT(ii=1:dim1, jj=1:dim2)
    ans(ii, jj, 1) = ans(ii, jj, 1) * 2.0_DFP
  END DO

CASE ("B")
  CALL GradientEvalAllOrthopol_(n=order, x=xij(1, :), &
             orthopol=polyopt%Lobatto, ans=ans(:, :, 1), nrow=dim1, ncol=dim2)

CASE DEFAULT
  dim1 = 0; dim2 = 0; dim3 = 0
  RETURN

END SELECT

DO CONCURRENT(k=2:order, ii=1:dim1)
  ans(ii, k + 1, 1) = (o1**(k - 1)) * ans(ii, k + 1, 1)
END DO

END PROCEDURE HeirarchicalGradientBasis_Line2_

!----------------------------------------------------------------------------
!                                                        OrthogonalBasis_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasis_Line1
INTEGER(I4B) :: nrow, ncol
CALL OrthogonalBasis_Line1_(order=order, xij=xij, refline=refline, &
                         basisType=basisType, ans=ans, nrow=nrow, ncol=ncol, &
                            alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE OrthogonalBasis_Line1

!----------------------------------------------------------------------------
!                                                     OrthogonalBasis_Line1_
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasis_Line1_
LOGICAL(LGT) :: isok, abool
#ifdef DEBUG_VER
#endif

CHARACTER(1) :: astr
REAL(DFP) :: x(SIZE(xij, 2))

nrow = SIZE(xij, 2)
ncol = order + 1

#ifdef DEBUG_VER

ans(1:nrow, 1:ncol) = 0.0_DFP

isok = basisType .EQ. polyopt%Jacobi

IF (isok) THEN
  abool = (.NOT. PRESENT(alpha)) .OR. (.NOT. PRESENT(beta))

  IF (abool) THEN
    CALL Errormsg(routine="OrthogonalBasis_Line1()", &
                msg="alpha and beta should be present for basisType=Jacobi", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    RETURN
  END IF

END IF

isok = basisType .EQ. polyopt%Ultraspherical
IF (isok) THEN

  abool = .NOT. PRESENT(lambda)

  IF (abool) THEN
    CALL Errormsg(routine="OrthogonalBasis_Line1()", file=__FILE__, &
                msg="lambda should be present for basisType=Ultraspherical", &
                  line=__LINE__, unitno=stderr)
    RETURN
  END IF

END IF

#endif

astr = UpperCase(refLine(1:1))

SELECT CASE (astr)
CASE ("U")
  CALL FromUnitLine2BiUnitLine_(xin=xij(1, :), ans=x, tsize=nrow)
  CALL EvalAllOrthopol_(n=order, x=x, orthopol=basisType, alpha=alpha, &
                      beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)

CASE ("B")
  CALL EvalAllOrthopol_(n=order, x=xij(1, :), orthopol=basisType, &
                        alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                        nrow=nrow, ncol=ncol)

CASE DEFAULT

  ans(1:nrow, 1:ncol) = 0.0_DFP
  CALL Errormsg(msg="No case found for refLine.", &
                routine="OrthogonalBasis_Line1()", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN

END SELECT

END PROCEDURE OrthogonalBasis_Line1_

!----------------------------------------------------------------------------
!                                            OrthogonalBasisGradient_Line1
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Line1
INTEGER(I4B) :: dim1, dim2, dim3
CALL OrthogonalBasisGradient_Line1_(order=order, xij=xij, refline=refline, &
              basisType=basisType, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
                                    alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE OrthogonalBasisGradient_Line1

!----------------------------------------------------------------------------
!                                                OrthogonalBasisGradient_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Line1_
CHARACTER(1) :: astr
REAL(DFP) :: x(SIZE(xij, 2))
INTEGER(I4B) :: ii, jj

astr = UpperCase(refline(1:1))
dim1 = SIZE(xij, 2)
dim2 = order + 1
dim3 = 1

SELECT CASE (astr)
CASE ("U")

  CALL FromUnitLine2BiUnitLine_(xin=xij(1, :), ans=x, tsize=dim1)
  CALL GradientEvalAllOrthopol_(n=order, x=x, orthopol=basisType, &
                                ans=ans(:, :, 1), nrow=dim1, ncol=dim2)

  DO CONCURRENT(ii=1:dim1, jj=1:dim2)
    ans(ii, jj, 1) = ans(ii, jj, 1) * 2.0_DFP
  END DO

CASE ("B")
  CALL GradientEvalAllOrthopol_(n=order, x=xij(1, :), orthopol=basisType, &
                                ans=ans(:, :, 1), nrow=dim1, ncol=dim2)

CASE DEFAULT

  ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP
  CALL Errormsg(msg="No case found for refline.", &
                routine=" OrthogonalBasisGradient_Line1_", &
                file=__FILE__, line=__LINE__, unitno=stderr)
  RETURN

END SELECT
END PROCEDURE OrthogonalBasisGradient_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
