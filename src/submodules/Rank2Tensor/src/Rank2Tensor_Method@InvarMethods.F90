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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Invariants, eigenvalues and polar decomposition of
!   [[Rank2Tensor_]]

SUBMODULE(Rank2Tensor_Method) InvarMethods
USE EigenUtility, ONLY: GetSymEigenJacobi
USE ErrorHandling, ONLY: Errormsg
#ifdef USE_LAPACK95
USE F95_LAPACK, ONLY: GEEV
#endif
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: SineLode
USE GlobalData, ONLY: stdout
USE InputUtility, ONLY: Input
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: maxJacobiIter = 100

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Trace
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Trace
INTEGER(I4B) :: n, ii
REAL(DFP) :: P(3, 3)

n = Input(option=power, default=1_I4B)

SELECT CASE (n)
CASE (:0)
  ans = 3.0_DFP
CASE (1)
  ans = obj%T(1, 1) + obj%T(2, 2) + obj%T(3, 3)
CASE (2)
  ans = SUM(obj%T * TRANSPOSE(obj%T))
CASE (3)
  ans = SUM(MATMUL(obj%T, obj%T) * TRANSPOSE(obj%T))
CASE DEFAULT
  P = obj%T
  DO ii = 2, n
    P = MATMUL(P, obj%T)
  END DO
  ans = P(1, 1) + P(2, 2) + P(3, 3)
END SELECT
END PROCEDURE Rank2Tensor_Trace

!----------------------------------------------------------------------------
!                                                                         J2
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_J2
LOGICAL(LGT) :: isDev

isDev = Input(option=isDeviatoric, default=.FALSE.)

IF (isDev) THEN
  ans = 0.5_DFP * Trace(obj=obj, power=2)
ELSE
  ASSOCIATE (T => obj%T)
    ans = ((T(1, 1) - T(2, 2))**2 &
           + (T(2, 2) - T(3, 3))**2 &
           + (T(3, 3) - T(1, 1))**2) / 6.0_DFP &
          + T(1, 2) * T(2, 1) + T(2, 3) * T(3, 2) + T(1, 3) * T(3, 1)
  END ASSOCIATE
END IF
END PROCEDURE Rank2Tensor_J2

!----------------------------------------------------------------------------
!                                                                         J3
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_J3
LOGICAL(LGT) :: isDev

isDev = Input(option=isDeviatoric, default=.FALSE.)

IF (isDev) THEN
  ans = Det(obj)
ELSE
  ans = Det(Deviatoric(obj))
END IF
END PROCEDURE Rank2Tensor_J3

!----------------------------------------------------------------------------
!                                                                        Det
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Det
ASSOCIATE (T => obj%T)
  ans = T(1, 1) * (T(2, 2) * T(3, 3) - T(2, 3) * T(3, 2)) &
        - T(1, 2) * (T(2, 1) * T(3, 3) - T(2, 3) * T(3, 1)) &
        + T(1, 3) * (T(2, 1) * T(3, 2) - T(3, 1) * T(2, 2))
END ASSOCIATE
END PROCEDURE Rank2Tensor_Det

!----------------------------------------------------------------------------
!                                                                  LodeAngle
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_LodeAngle1
REAL(DFP) :: arg

! J2 is non-negative for a real symmetric tensor. A non-positive value
! means a zero (or non-symmetric) deviator, for which the angle is
! undefined, so return zero instead of a NaN.
IF (J2 .LE. TINY(1.0_DFP)) THEN
  ans = 0.0_DFP
  RETURN
END IF

arg = 1.5_DFP * SQRT(3.0_DFP) * J3 / (J2 * SQRT(J2))
arg = MAX(-1.0_DFP, MIN(1.0_DFP, arg))

IF (lodeType .EQ. SineLode) THEN
  ans = ASIN(-arg) / 3.0_DFP
ELSE
  ans = ACOS(arg) / 3.0_DFP
END IF
END PROCEDURE Rank2Tensor_LodeAngle1

!----------------------------------------------------------------------------
!                                                                  LodeAngle
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_LodeAngle2
ans = LodeAngle(lodeType=lodeType, &
                J2=J2(obj, isDeviatoric), &
                J3=J3(obj, isDeviatoric))
END PROCEDURE Rank2Tensor_LodeAngle2

!----------------------------------------------------------------------------
!                                                                  Isotropic
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Isotropic
CALL IsotropicTensor(obj=ans, lambda=Trace(obj) / 3.0_DFP)
END PROCEDURE Rank2Tensor_Isotropic

!----------------------------------------------------------------------------
!                                                                 Deviatoric
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Deviatoric
ans%T = obj%T - (Trace(obj) / 3.0_DFP) * Eye3
ans%isSym = obj%isSym
END PROCEDURE Rank2Tensor_Deviatoric

!----------------------------------------------------------------------------
!                                                                 Invariants
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Invariants
LOGICAL(LGT) :: isDev

isDev = Input(option=isDeviatoric, default=.FALSE.)

IF (isDev) THEN
  ans(1) = 0.0_DFP
ELSE
  ans(1) = Trace(obj)
END IF

ans(2) = 0.5_DFP * (ans(1)**2 - Trace(obj, power=2))
ans(3) = Det(obj)
END PROCEDURE Rank2Tensor_Invariants

!----------------------------------------------------------------------------
!                                                                      Eigen
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_Eigen
REAL(DFP) :: qi0(3, 3), wi0(3)

IF (obj%isSym) THEN
  CALL GetSymEigenJacobi(mat=obj%T, eigenValues=WR, eigenVectors=QR, &
                         maxIter=maxJacobiIter)
  qi0 = 0.0_DFP
  wi0 = 0.0_DFP
ELSE
  CALL GeneralEigen(T=obj%T, QR=QR, WR=WR, QI=qi0, WI=wi0)
END IF

IF (PRESENT(QI)) QI = qi0
IF (PRESENT(WI)) WI = wi0
END PROCEDURE Rank2Tensor_Eigen

!----------------------------------------------------------------------------
!                                                               GeneralEigen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Eigenvalues and eigenvectors of a general 3x3 matrix
!
! Wraps LAPACK GEEV. For a complex conjugate pair GEEV returns the real
! and imaginary parts of the eigenvector in two consecutive columns
! `j` and `j+1`. This routine expands them into `QR` and `QI` so that
! column `k` holds eigenvector `k`.
!
! Formula:
!   $$T (q_R + i q_I) = (w_R + i w_I)(q_R + i q_I)$$

SUBROUTINE GeneralEigen(T, QR, WR, QI, WI)
  REAL(DFP), INTENT(IN) :: T(3, 3)
  !! general (non-symmetric) matrix
  REAL(DFP), INTENT(INOUT) :: QR(3, 3)
  !! real part of the eigenvectors, column k is eigenvector k
  REAL(DFP), INTENT(INOUT) :: WR(3)
  !! real part of the eigenvalues
  REAL(DFP), INTENT(INOUT) :: QI(3, 3)
  !! imaginary part of the eigenvectors
  REAL(DFP), INTENT(INOUT) :: WI(3)
  !! imaginary part of the eigenvalues

#if defined(DEBUG_VER) || !defined(USE_LAPACK95)
  CHARACTER(*), PARAMETER :: myName = "GeneralEigen()"
#endif
#ifdef USE_LAPACK95
  REAL(DFP) :: mat(3, 3), vr(3, 3)
  INTEGER(I4B) :: info, jj

  mat = T
  CALL GEEV(A=mat, WR=WR, WI=WI, VR=vr, INFO=info)

#ifdef DEBUG_VER
  IF (info .NE. 0) THEN
    CALL Errormsg(msg="[INTERNAL ERROR] :: GEEV failed", &
                  file=__FILE__, routine=myName, line=__LINE__, &
                  unitno=stdout)
    RETURN
  END IF
#endif

  QR = vr
  QI = 0.0_DFP
  jj = 1
  DO WHILE (jj .LE. 3)
    IF (ABS(WI(jj)) .GT. 0.0_DFP .AND. jj .LT. 3) THEN
      QR(:, jj) = vr(:, jj)
      QI(:, jj) = vr(:, jj + 1)
      QR(:, jj + 1) = vr(:, jj)
      QI(:, jj + 1) = -vr(:, jj + 1)
      jj = jj + 2
    ELSE
      jj = jj + 1
    END IF
  END DO
#else
  QR = 0.0_DFP
  WR = 0.0_DFP
  QI = 0.0_DFP
  WI = 0.0_DFP
  CALL Errormsg(msg="[CONFIG ERROR] :: eigen decomposition of a "// &
                "non-symmetric tensor needs LAPACK95 (USE_LAPACK95)", &
                file=__FILE__, routine=myName, line=__LINE__, &
                unitno=stdout)
  STOP
#endif
END SUBROUTINE GeneralEigen

!----------------------------------------------------------------------------
!                                                                PolarDecomp
!----------------------------------------------------------------------------

MODULE PROCEDURE Rank2Tensor_PolarDecomp
CALL PD(mat=obj%T, R=R%T, U=U%T, V=V%T)
R%isSym = .FALSE.
U%isSym = .TRUE.
V%isSym = .TRUE.
END PROCEDURE Rank2Tensor_PolarDecomp

!----------------------------------------------------------------------------
!                                                                         PD
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Polar decomposition of a 3x3 matrix
!
! Algorithm 3.1 of Higham and Noferini (2015), "An algorithm to
! compute the polar decomposition of a 3x3 matrix", Numer. Algor.
! The rotation is obtained from the unit quaternion `q` that is the
! eigenvector of the symmetric 4x4 matrix `B` for its largest
! eigenvalue. The input must have a positive determinant.
!
! Formula:
!   $$A = R U = V R,\quad U = R^{T} A,\quad V = A R^{T}$$

PURE SUBROUTINE PD(mat, R, U, V)
  REAL(DFP), INTENT(IN) :: mat(3, 3)
  !! matrix A with a positive determinant
  REAL(DFP), INTENT(INOUT) :: R(3, 3)
  !! rotation
  REAL(DFP), INTENT(INOUT) :: U(3, 3)
  !! right stretch
  REAL(DFP), INTENT(INOUT) :: V(3, 3)
  !! left stretch

  REAL(DFP) :: B(4, 4), eigVecs(4, 4), eigVals(4), q(4)
  INTEGER(I4B) :: imax

  ASSOCIATE (A => mat)
    B(1, 1) = A(1, 1) + A(2, 2) + A(3, 3)
    B(1, 2) = A(2, 3) - A(3, 2)
    B(1, 3) = A(3, 1) - A(1, 3)
    B(1, 4) = A(1, 2) - A(2, 1)
    B(2, 2) = A(1, 1) - A(2, 2) - A(3, 3)
    B(2, 3) = A(1, 2) + A(2, 1)
    B(2, 4) = A(3, 1) + A(1, 3)
    B(3, 3) = A(2, 2) - A(1, 1) - A(3, 3)
    B(3, 4) = A(2, 3) + A(3, 2)
    B(4, 4) = A(3, 3) - A(1, 1) - A(2, 2)
  END ASSOCIATE

  B(2, 1) = B(1, 2)
  B(3, 1) = B(1, 3)
  B(4, 1) = B(1, 4)
  B(3, 2) = B(2, 3)
  B(4, 2) = B(2, 4)
  B(4, 3) = B(3, 4)

  CALL GetSymEigenJacobi(mat=B, eigenValues=eigVals, &
                         eigenVectors=eigVecs, maxIter=maxJacobiIter)

  ! The rotation corresponds to the largest (algebraic) eigenvalue
  imax = MAXLOC(eigVals, 1)
  q = eigVecs(:, imax) / NORM2(eigVecs(:, imax))

  R = QuaternionToRotation(q)

  U = MATMUL(TRANSPOSE(R), mat)
  V = MATMUL(mat, TRANSPOSE(R))

  ! Remove round-off asymmetry
  U = 0.5_DFP * (U + TRANSPOSE(U))
  V = 0.5_DFP * (V + TRANSPOSE(V))
END SUBROUTINE PD

!----------------------------------------------------------------------------
!                                                       QuaternionToRotation
!----------------------------------------------------------------------------

! Rotation matrix of a unit quaternion q = [w, x, y, z]

PURE FUNCTION QuaternionToRotation(q) RESULT(R)
  REAL(DFP), INTENT(IN) :: q(4)
  !! unit quaternion [w, x, y, z]
  REAL(DFP) :: R(3, 3)
  !! rotation matrix

  ASSOCIATE (w => q(1), x => q(2), y => q(3), z => q(4))
    R(1, 1) = 1.0_DFP - 2.0_DFP * (y * y + z * z)
    R(1, 2) = 2.0_DFP * (x * y + w * z)
    R(1, 3) = 2.0_DFP * (x * z - w * y)
    R(2, 1) = 2.0_DFP * (x * y - w * z)
    R(2, 2) = 1.0_DFP - 2.0_DFP * (x * x + z * z)
    R(2, 3) = 2.0_DFP * (y * z + w * x)
    R(3, 1) = 2.0_DFP * (x * z + w * y)
    R(3, 2) = 2.0_DFP * (y * z - w * x)
    R(3, 3) = 1.0_DFP - 2.0_DFP * (x * x + y * y)
  END ASSOCIATE
END FUNCTION QuaternionToRotation

END SUBMODULE InvarMethods
