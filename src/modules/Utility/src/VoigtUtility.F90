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

!> author: Shion Shimizu
! date: 2026-11-17
! summary: PURE routines on plain 3D Voigt arrays
!
! Description:
!   A symmetric second order tensor is stored as a plain array
!   `REAL(DFP) :: v(6)` in the order `[11, 22, 33, 12, 23, 13]`, the same
!   order as [[VoigtRank2Tensor_]] and [[MaterialJacobian_]]. The routines
!   of this module work on such arrays; they are PURE, have fixed sizes
!   and do not allocate. They are meant for code that keeps its tensors
!   in arrays, for example the stress update of a constitutive model at a
!   quadrature point.
!
!   The routines do not know whether `v` is strain-like or stress-like.
!   [[VoigtTrace]], [[VoigtIdentity]] and [[VoigtDev]] work for both. The
!   stress-like routines [[VoigtStressNorm]] and [[VoigtSymEigen]] expect
!   tensor shear components; for an engineering strain use the tensor
!   operations of [[VoigtRank2Tensor_]] (FrobeniusNorm, Eigen), which
!   convert the shear entries with the Voigt type.
!
!   - [[VoigtIdentity]]: second order identity
!   - [[VoigtTrace]], [[VoigtDev]]: trace and deviatoric part
!   - [[VoigtStressNorm]]: Frobenius norm
!   - [[VoigtOuter]]: outer product, a rank 4 tensor in Voigt form
!   - [[VoigtSymEigen]]: eigenvalues and eigenvectors
!   - [[VoigtStrainFromGradient]]: small strain from a displacement
!     gradient
!
!   The rank 4 constants in Voigt form are in MaterialJacobian_Method
!   (Rank4IdentityOtimesIdentity, Rank4DeviatoricProjector,
!   Rank4SymIdentity).
!
! Changes:
!   - 2026-11-17: moved from the elastoplasticity kernel
!     (easifem/base#355)

MODULE VoigtUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: VoigtIdentity
PUBLIC :: VoigtTrace
PUBLIC :: VoigtDev
PUBLIC :: VoigtStressNorm
PUBLIC :: VoigtOuter
PUBLIC :: VoigtSymEigen
PUBLIC :: VoigtStrainFromGradient

!----------------------------------------------------------------------------
!                                                       VoigtIdentity@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Second order identity tensor as a 3D Voigt vector
!
! Returns [1, 1, 1, 0, 0, 0]. It is the same for strain-like and
! stress-like vectors. The rank 4 tensors in Voigt form are in
! MaterialJacobian_Method (Rank4IdentityOtimesIdentity,
! Rank4DeviatoricProjector, Rank4SymIdentity).
!
! Returns:
!   ans : REAL(DFP), SHAPE(6) - identity tensor in Voigt form
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: Rank4DeviatoricProjector
! USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: VoigtDev
! USE VoigtUtility, ONLY: VoigtIdentity
! USE VoigtUtility, ONLY: VoigtOuter
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
!
! ! Test 1: identity in Voigt form
! BLOCK
!   REAL(DFP) :: expected(6)
!
!   expected = [1.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(MAXVAL(ABS(VoigtIdentity() - expected)) .LT. tol, &
!           "VoigtIdentity: values")
! END BLOCK
!
! ! Test 2: I otimes I
! BLOCK
!   CALL OK(MAXVAL(ABS(VoigtOuter(VoigtIdentity(), VoigtIdentity()) &
!                      - Rank4IdentityOtimesIdentity())) .LT. tol, &
!           "VoigtIdentity: I otimes I")
! END BLOCK
!
! ! Test 3: the deviatoric projector maps an engineering strain to the
! ! deviator of the strain tensor
! BLOCK
!   REAL(DFP) :: eps(6), epsTensor(6)
!
!   eps = [1.0_DFP, 2.0_DFP, 6.0_DFP, 4.0_DFP, 5.0_DFP, 7.0_DFP]
!   epsTensor = [eps(1:3), 0.5_DFP * eps(4:6)]
!   CALL OK(MAXVAL(ABS(MATMUL(Rank4DeviatoricProjector(), eps) &
!                      - VoigtDev(epsTensor))) .LT. tol, &
!           "VoigtIdentity: deviator of the strain tensor")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION VoigtIdentity() RESULT(ans)
    REAL(DFP) :: ans(6)
    !! identity tensor in Voigt form
  END FUNCTION VoigtIdentity
END INTERFACE

!----------------------------------------------------------------------------
!                                                          VoigtTrace@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Trace of a 3D Voigt vector
!
! The trace is the sum of the normal components, v(1) + v(2) + v(3). It is
! the same for strain-like and stress-like vectors.
!
! Returns:
!   ans : REAL(DFP) - trace
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: VoigtTrace
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
!
! ! Test 1: trace is the sum of the normal components
! BLOCK
!   REAL(DFP) :: v(6)
!
!   v = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!   CALL OK(ABS(VoigtTrace(v) - 6.0_DFP) .LT. tol, "VoigtTrace: 1 + 2 + 3")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtDev]]

INTERFACE
  MODULE PURE FUNCTION VoigtTrace(v) RESULT(ans)
    REAL(DFP), INTENT(IN) :: v(6)
    !! 3D Voigt vector [11, 22, 33, 12, 23, 13]
    REAL(DFP) :: ans
    !! trace
  END FUNCTION VoigtTrace
END INTERFACE

!----------------------------------------------------------------------------
!                                                            VoigtDev@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Deviatoric part of a stress-like 3D Voigt vector
!
! The mean of the normal components is subtracted from the normal
! components; the shear components are not changed. For a strain-like
! vector the result is the deviator in engineering form as well.
!
! Returns:
!   ans : REAL(DFP), SHAPE(6) - deviatoric part
!
! Formula:
!   $$ s = v - \frac{1}{3} \mathrm{tr}(v) \, I $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: VoigtDev
! USE VoigtUtility, ONLY: VoigtTrace
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
!
! ! Test 1: mean of the normal components is removed, shear is unchanged
! BLOCK
!   REAL(DFP) :: v(6), s(6), expected(6)
!
!   v = [1.0_DFP, 2.0_DFP, 6.0_DFP, 4.0_DFP, 5.0_DFP, 7.0_DFP]
!   expected = [-2.0_DFP, -1.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 7.0_DFP]
!   s = VoigtDev(v)
!   CALL OK(MAXVAL(ABS(s - expected)) .LT. tol, "VoigtDev: values")
!   CALL OK(ABS(VoigtTrace(s)) .LT. tol, "VoigtDev: trace is zero")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtTrace]], [[Dev]]

INTERFACE
  MODULE PURE FUNCTION VoigtDev(v) RESULT(ans)
    REAL(DFP), INTENT(IN) :: v(6)
    !! 3D Voigt vector [11, 22, 33, 12, 23, 13]
    REAL(DFP) :: ans(6)
    !! deviatoric part, same Voigt type as v
  END FUNCTION VoigtDev
END INTERFACE

!----------------------------------------------------------------------------
!                                                     VoigtStressNorm@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Frobenius norm of a stress-like 3D Voigt vector
!
! The shear components are tensor components, so each of them appears
! twice in the tensor. Do not use it for an engineering strain vector;
! use [[FrobeniusNorm]] of a [[VoigtRank2Tensor_]] instead.
!
! Returns:
!   ans : REAL(DFP) - norm of the tensor
!
! Formula:
!   $$ \| v \| = \sqrt{v_1^2 + v_2^2 + v_3^2 + 2 (v_4^2 + v_5^2 + v_6^2)} $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: VoigtStressNorm
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-13_DFP
!
! ! Test 1: same as SQRT(SUM(T * T)) of the 3 x 3 tensor
! BLOCK
!   REAL(DFP) :: v(6), T(3, 3)
!
!   v = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!   T = RESHAPE([v(1), v(4), v(6), &
!                v(4), v(2), v(5), &
!                v(6), v(5), v(3)], [3, 3])
!   CALL OK(ABS(VoigtStressNorm(v) - SQRT(SUM(T * T))) .LT. tol, &
!           "VoigtStressNorm: Frobenius norm")
!   CALL OK(ABS(VoigtStressNorm(v) - SQRT(168.0_DFP)) .LT. tol, &
!           "VoigtStressNorm: sqrt(168)")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION VoigtStressNorm(v) RESULT(ans)
    REAL(DFP), INTENT(IN) :: v(6)
    !! stress-like 3D Voigt vector (tensor shear components)
    REAL(DFP) :: ans
    !! Frobenius norm
  END FUNCTION VoigtStressNorm
END INTERFACE

!----------------------------------------------------------------------------
!                                                          VoigtOuter@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Outer product of two 3D Voigt vectors
!
! ans(i, j) = a(i) b(j). If a and b are stress-like vectors, ans is the
! Voigt form of the rank 4 tensor a otimes b that maps an engineering
! strain to a stress, (a otimes b) : eps = a (b : eps), in the convention
! of MaterialJacobian_. The size is fixed, so no temporary arrays are
! made (see also OuterProd of ProductUtility for general sizes).
!
! Returns:
!   ans : REAL(DFP), SHAPE(6, 6) - outer product
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: VoigtIdentity
! USE VoigtUtility, ONLY: VoigtOuter
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
!
! ! Test 1: I otimes I
! BLOCK
!   CALL OK(MAXVAL(ABS(VoigtOuter(VoigtIdentity(), VoigtIdentity()) &
!                      - Rank4IdentityOtimesIdentity())) .LT. tol, &
!           "VoigtOuter: I otimes I")
! END BLOCK
!
! ! Test 2: ans(i, j) = a(i) b(j)
! BLOCK
!   REAL(DFP) :: a(6), b(6), C(6, 6)
!
!   a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!   b = [6.0_DFP, 5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, 1.0_DFP]
!   C = VoigtOuter(a, b)
!   CALL OK(ABS(C(2, 5) - 4.0_DFP) .LT. tol, "VoigtOuter: C(2, 5)")
!   CALL OK(ABS(C(6, 1) - 36.0_DFP) .LT. tol, "VoigtOuter: C(6, 1)")
!   CALL OK(MAXVAL(ABS(MATMUL(C, a) - a * DOT_PRODUCT(b, a))) .LT. tol, &
!           "VoigtOuter: (a b^T) x = a (b . x)")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[Otimes]], [[VoigtIdentity]]

INTERFACE
  MODULE PURE FUNCTION VoigtOuter(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(6)
    !! first 3D Voigt vector (rows)
    REAL(DFP), INTENT(IN) :: b(6)
    !! second 3D Voigt vector (columns)
    REAL(DFP) :: ans(6, 6)
    !! outer product, ans(i, j) = a(i) b(j)
  END FUNCTION VoigtOuter
END INTERFACE

!----------------------------------------------------------------------------
!                                                       VoigtSymEigen@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Eigenvalues and eigenvectors of a stress-like 3D Voigt vector
!
! v is a symmetric tensor in Voigt form with tensor shear components.
! The routine uses the cyclic Jacobi method on the 3 x 3 tensor. A sweep
! ends the iteration when the sum of the absolute off-diagonal entries is
! at most 1e-15 times the largest absolute entry, so the tolerance does
! not depend on the units (at most 50 sweeps). The eigenvalues are sorted
! in descending order, vals(1) >= vals(2) >= vals(3), and vecs(:, i) is
! the unit eigenvector of vals(i). The routine is PURE and does not
! allocate, so it can be used in a stress update.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE VoigtUtility, ONLY: VoigtSymEigen
! IMPLICIT NONE
! REAL(DFP) :: v(6), vals(3), vecs(3, 3)
!
! v = [4.0_DFP, 1.0_DFP, 2.0_DFP, 0.5_DFP, 0.3_DFP, 0.2_DFP]
! CALL VoigtSymEigen(v=v, vals=vals, vecs=vecs)
! END PROGRAM main
!```
!
! Full example: src/modules/Utility/examples/VoigtSymEigen_test_1.F90

INTERFACE
  MODULE PURE SUBROUTINE VoigtSymEigen(v, vals, vecs)
    REAL(DFP), INTENT(IN) :: v(6)
    !! stress-like 3D Voigt vector (tensor shear components)
    REAL(DFP), INTENT(OUT) :: vals(3)
    !! eigenvalues in descending order
    REAL(DFP), INTENT(OUT) :: vecs(3, 3)
    !! unit eigenvectors, vecs(:, i) belongs to vals(i)
  END SUBROUTINE VoigtSymEigen
END INTERFACE

!----------------------------------------------------------------------------
!                                             VoigtStrainFromGradient@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Small strain in 3D Voigt form from a displacement gradient
!
! grad(i, j) = du_i / dx_j; only grad(1:nsd, 1:nsd) is read. The strain
! uses the engineering shear strain, gamma_ij = du_i/dx_j + du_j/dx_i.
! The components that do not exist in 1D (uniaxial strain) and 2D (plane
! strain) are zero.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: VoigtStrainFromGradient
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
! REAL(DFP), PARAMETER :: grad(3, 3) = RESHAPE( &
!                         [1.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          4.0_DFP, 5.0_DFP, 6.0_DFP, &
!                          7.0_DFP, 8.0_DFP, 9.0_DFP], [3, 3])
! !! grad(i, j) = du_i / dx_j
!
! ! Test 1: 3D, engineering shear strain
! BLOCK
!   REAL(DFP) :: eps(6), expected(6)
!
!   CALL VoigtStrainFromGradient(grad=grad, nsd=3, ans=eps)
!   expected = [1.0_DFP, 5.0_DFP, 9.0_DFP, 6.0_DFP, 14.0_DFP, 10.0_DFP]
!   CALL OK(MAXVAL(ABS(eps - expected)) .LT. tol, &
!           "VoigtStrainFromGradient: 3D")
! END BLOCK
!
! ! Test 2: 2D (plane strain), out-of-plane components are zero
! BLOCK
!   REAL(DFP) :: eps(6), expected(6)
!
!   CALL VoigtStrainFromGradient(grad=grad, nsd=2, ans=eps)
!   expected = [1.0_DFP, 5.0_DFP, 0.0_DFP, 6.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(MAXVAL(ABS(eps - expected)) .LT. tol, &
!           "VoigtStrainFromGradient: 2D")
! END BLOCK
!
! ! Test 3: 1D (uniaxial strain)
! BLOCK
!   REAL(DFP) :: eps(6), expected(6)
!
!   CALL VoigtStrainFromGradient(grad=grad, nsd=1, ans=eps)
!   expected = [1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(MAXVAL(ABS(eps - expected)) .LT. tol, &
!           "VoigtStrainFromGradient: 1D")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE VoigtStrainFromGradient(grad, nsd, ans)
    REAL(DFP), INTENT(IN) :: grad(:, :)
    !! displacement gradient, at least SHAPE(nsd, nsd)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! number of spatial dimensions, 1, 2 or 3
    REAL(DFP), INTENT(OUT) :: ans(6)
    !! engineering strain [11, 22, 33, 12, 23, 13]
  END SUBROUTINE VoigtStrainFromGradient
END INTERFACE

END MODULE VoigtUtility
