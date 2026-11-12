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


!> Test: 2D reductions of a Voigt tangent
! summary: plane strain, plane stress (condensation), axisymmetric

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: GetTangent
USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: E = 200.0_DFP, nu = 0.3_DFP
TYPE(MaterialJacobian_) :: obj
REAL(DFP), ALLOCATABLE :: C2(:, :)
REAL(DFP) :: lam, mu, a, B(6, 6), C6(6, 6), ei(3), e6(6), s6(6), eo(3)
REAL(DFP) :: Coo(3, 3)
INTEGER(I4B) :: i, j

lam = E * nu / ((1.0_DFP + nu) * (1.0_DFP - 2.0_DFP * nu))
mu = E / (2.0_DFP * (1.0_DFP + nu))
obj = MaterialJacobian(IsotropicTangent(lam, mu))

C2 = GetTangent(obj)
CALL OK(SIZE(C2, 1) .EQ. 6, "default form is 3D")

! Plane strain, as in classes Get_PlaneStrain_C_InvC
a = E / ((1.0_DFP + nu) * (1.0_DFP - 2.0_DFP * nu))
C2 = GetTangent(obj, VoigtFormPlaneStrain)
CALL OK(SIZE(C2, 1) .EQ. 3 .AND. ABS(C2(1, 1) - a * (1.0_DFP - nu)) &
        .LT. tol .AND. ABS(C2(1, 2) - a * nu) .LT. tol .AND. &
        ABS(C2(3, 3) - a * (1.0_DFP - 2.0_DFP * nu) / 2.0_DFP) .LT. tol, &
        "plane strain")

! Plane stress, as in classes Get_PlaneStress_C_InvC
a = E / (1.0_DFP - nu * nu)
C2 = GetTangent(obj, VoigtFormPlaneStress)
CALL OK(ABS(C2(1, 1) - a) .LT. tol .AND. ABS(C2(1, 2) - a * nu) &
        .LT. tol .AND. ABS(C2(3, 3) - E / (2.0_DFP * (1.0_DFP + nu))) &
        .LT. tol .AND. ABS(C2(1, 3)) .LT. tol, "plane stress")

C2 = GetTangent(obj, VoigtFormAxisymmetric)
CALL OK(SIZE(C2, 1) .EQ. 4 .AND. ABS(C2(4, 4) - (lam + 2.0_DFP * mu)) &
        .LT. tol .AND. ABS(C2(1, 4) - lam) .LT. tol .AND. &
        ABS(C2(3, 3) - mu) .LT. tol, "axisymmetric")

! Anisotropic plane stress: out-of-plane stresses vanish
DO j = 1, 6
  DO i = 1, 6
    B(i, j) = REAL(MOD(3 * i + 5 * j, 7), DFP) / 7.0_DFP
  END DO
END DO
C6 = MATMUL(TRANSPOSE(B), B)
DO i = 1, 6
  C6(i, i) = C6(i, i) + 5.0_DFP
END DO
C2 = ReduceVoigtTangent(C6, VoigtFormPlaneStress)

ei = [1.0E-3_DFP, -2.0E-3_DFP, 0.5E-3_DFP]
Coo = C6([3, 5, 6], [3, 5, 6])
eo = -MATMUL(InvertSmall(Coo), MATMUL(C6([3, 5, 6], [1, 2, 4]), ei))
e6 = 0.0_DFP
e6([1, 2, 4]) = ei
e6([3, 5, 6]) = eo
s6 = MATMUL(C6, e6)
CALL OK(MAXVAL(ABS(s6([3, 5, 6]))) .LT. tol, "sigma_33 = s23 = s13 = 0")
CALL OK(MAXVAL(ABS(s6([1, 2, 4]) - MATMUL(C2, ei))) .LT. tol, &
        "condensed tangent gives in-plane stress")

CONTAINS

FUNCTION InvertSmall(A) RESULT(X)
  REAL(DFP), INTENT(IN) :: A(3, 3)
  REAL(DFP) :: X(3, 3), d

  X(1, 1) = A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)
  X(1, 2) = A(1, 3) * A(3, 2) - A(1, 2) * A(3, 3)
  X(1, 3) = A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2)
  X(2, 1) = A(2, 3) * A(3, 1) - A(2, 1) * A(3, 3)
  X(2, 2) = A(1, 1) * A(3, 3) - A(1, 3) * A(3, 1)
  X(2, 3) = A(1, 3) * A(2, 1) - A(1, 1) * A(2, 3)
  X(3, 1) = A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1)
  X(3, 2) = A(1, 2) * A(3, 1) - A(1, 1) * A(3, 2)
  X(3, 3) = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
  d = A(1, 1) * X(1, 1) + A(1, 2) * X(2, 1) + A(1, 3) * X(3, 1)
  X = X / d
END FUNCTION InvertSmall

END PROGRAM main
