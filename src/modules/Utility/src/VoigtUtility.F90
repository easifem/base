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
!   - [[GetVoigtIndex_]], [[VoigtSize]]: components of a Voigt form
!     (VoigtForm* constants of GlobalData, for example the 2D forms)
!   - [[GetVoigtVector]], [[GetVoigtVector_]]: vector of a Voigt form
!     from a 3D Voigt array, and [[VoigtFromForm]] for the inverse
!
!   The rank 4 constants in Voigt form are in MaterialJacobian_Method
!   (Rank4IdentityOtimesIdentity, Rank4DeviatoricProjector,
!   Rank4SymIdentity).
!
! Changes:
!   - 2026-11-17: moved from the elastoplasticity kernel
!     (easifem/base#355)
!   - 2026-11-17: Voigt forms (GetVoigtIndex_, VoigtSize, GetVoigtVector,
!     GetVoigtVector_, VoigtFromForm), easifem/base#356

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
PUBLIC :: GetVoigtIndex_
PUBLIC :: VoigtSize
PUBLIC :: GetVoigtVector
PUBLIC :: GetVoigtVector_
PUBLIC :: VoigtFromForm

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

!----------------------------------------------------------------------------
!                                                      GetVoigtIndex_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: 3D Voigt indices of the components of a Voigt form
!
! Description:
!   Writes in `ans(1:tsize)` the indices in the 3D Voigt order
!   [11, 22, 33, 12, 23, 13] of the components of `voigtForm`, in the
!   order of the form. This is the only table of the Voigt forms; the
!   other routines of this module and ReduceVoigtTangent use it.
!
!   | voigtForm | components |
!   |---|---|
!   | VoigtForm3D | [11, 22, 33, 12, 23, 13] |
!   | VoigtFormPlaneStrain, VoigtFormPlaneStress | [11, 22, 12] |
!   | VoigtFormAxisymmetric | [11, 22, 12, 33] |
!   | VoigtFormUniaxialStrain | [11] |
!   | VoigtFormPlaneStrainFull | [11, 22, 12, 33] |
!   | VoigtFormPlaneStressFull | [11, 22, 12, 33] |
!   | VoigtFormUniaxialStrainFull | [11, 22, 33] |
!
!   Any other value is treated as VoigtForm3D.
!
!   The Full forms keep the out-of-plane components, for example
!   sigma_33 in plane strain or eps_33 in plane stress. The same form is
!   used for stress and strain; a component that is zero for one of them
!   (eps_33 in plane strain) is kept as zero.
!
! Arguments:
!   voigtForm : INTEGER(I4B) - one of the VoigtForm* constants
!   ans : INTEGER(I4B), rank(1) - indices, SIZE(ans) >= 6 is enough
!   tsize : INTEGER(I4B) - number of components of the form
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormAxisymmetric
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStrainFull
! USE GlobalData, ONLY: VoigtFormPlaneStress
! USE GlobalData, ONLY: VoigtFormPlaneStressFull
! USE GlobalData, ONLY: VoigtFormUniaxialStrain
! USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: GetVoigtIndex_
! IMPLICIT NONE
!
! ! Test 1: 3D Voigt indices of each form
! CALL Check(VoigtForm3D, [1, 2, 3, 4, 5, 6], "3D")
! CALL Check(VoigtFormPlaneStrain, [1, 2, 4], "plane strain")
! CALL Check(VoigtFormPlaneStress, [1, 2, 4], "plane stress")
! CALL Check(VoigtFormAxisymmetric, [1, 2, 4, 3], "axisymmetric")
! CALL Check(VoigtFormUniaxialStrain, [1], "uniaxial strain")
! CALL Check(VoigtFormPlaneStrainFull, [1, 2, 4, 3], "plane strain full")
! CALL Check(VoigtFormPlaneStressFull, [1, 2, 4, 3], "plane stress full")
! CALL Check(VoigtFormUniaxialStrainFull, [1, 2, 3], &
!            "uniaxial strain full")
!
! ! Test 2: an unknown form is treated as 3D
! CALL Check(-1_I4B, [1, 2, 3, 4, 5, 6], "unknown form")
!
! CONTAINS
!
! ! Compares the indices of a form with the expected ones
! SUBROUTINE Check(voigtForm, expected, name)
!   INTEGER(I4B), INTENT(IN) :: voigtForm
!   INTEGER(I4B), INTENT(IN) :: expected(:)
!   CHARACTER(*), INTENT(IN) :: name
!
!   INTEGER(I4B) :: ans(6), tsize
!
!   ans = 0
!   CALL GetVoigtIndex_(voigtForm=voigtForm, ans=ans, tsize=tsize)
!   CALL OK(tsize .EQ. SIZE(expected), name//": tsize")
!   CALL OK(ALL(ans(1:SIZE(expected)) .EQ. expected), name//": index")
! END SUBROUTINE Check
!
! END PROGRAM main
!```
!
! See also: [[VoigtSize]], [[GetVoigtVector_]], [[VoigtFromForm]]

INTERFACE
  MODULE PURE SUBROUTINE GetVoigtIndex_(voigtForm, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! 3D Voigt indices, only ans(1:tsize) is written
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of components of the form
  END SUBROUTINE GetVoigtIndex_
END INTERFACE

!----------------------------------------------------------------------------
!                                                           VoigtSize@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Number of components of a Voigt form
!
! Description:
!   Returns the length of the vector of `voigtForm`; see
!   [[GetVoigtIndex_]] for the forms. Unknown forms give 6.
!
! Returns:
!   ans : INTEGER(I4B) - length of the vector of the form
!
! Changes:
!   - 2026-11-17: moved from VoigtRank2Tensor_Method; added the Full and
!     uniaxial strain forms
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormAxisymmetric
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStrainFull
! USE GlobalData, ONLY: VoigtFormPlaneStress
! USE GlobalData, ONLY: VoigtFormPlaneStressFull
! USE GlobalData, ONLY: VoigtFormUniaxialStrain
! USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: VoigtSize
! IMPLICIT NONE
!
! ! Test 1: size of each Voigt form
! BLOCK
!   CALL OK(VoigtSize(VoigtForm3D) .EQ. 6, "3D: 6")
!   CALL OK(VoigtSize(VoigtFormPlaneStrain) .EQ. 3, "plane strain: 3")
!   CALL OK(VoigtSize(VoigtFormPlaneStress) .EQ. 3, "plane stress: 3")
!   CALL OK(VoigtSize(VoigtFormAxisymmetric) .EQ. 4, "axisymmetric: 4")
!   CALL OK(VoigtSize(VoigtFormUniaxialStrain) .EQ. 1, "uniaxial strain: 1")
! END BLOCK
!
! ! Test 2: size of the Full forms
! BLOCK
!   CALL OK(VoigtSize(VoigtFormPlaneStrainFull) .EQ. 4, &
!           "plane strain full: 4")
!   CALL OK(VoigtSize(VoigtFormPlaneStressFull) .EQ. 4, &
!           "plane stress full: 4")
!   CALL OK(VoigtSize(VoigtFormUniaxialStrainFull) .EQ. 3, &
!           "uniaxial strain full: 3")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION VoigtSize(voigtForm) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    INTEGER(I4B) :: ans
    !! length of the vector of the form
  END FUNCTION VoigtSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetVoigtVector@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Vector of a Voigt form from a 3D Voigt array
!
! Description:
!   Returns the components of the 3D Voigt array `v` that belong to
!   `voigtForm`, in the order of the form (see [[GetVoigtIndex_]]). The
!   entries are copied, so the Voigt type of `v` (stress-like or
!   strain-like) is kept. The same generic name is used for
!   [[VoigtRank2Tensor_]].
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE - SIZE(ans) = VoigtSize(voigtForm)
!
! Formula:
!   $$ \mathrm{ans}_i = v_{k_i}, \quad i = 1, \dots, \mathrm{tsize} $$
!   where $k$ are the indices of [[GetVoigtIndex_]].
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormPlaneStressFull
! USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: GetVoigtVector
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
! REAL(DFP), PARAMETER :: v(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, &
!                                 4.0_DFP, 5.0_DFP, 6.0_DFP]
!
! ! Test 1: 3D form returns all 6 entries
! BLOCK
!   REAL(DFP), ALLOCATABLE :: ans(:)
!
!   ans = GetVoigtVector(v=v, voigtForm=VoigtForm3D)
!   CALL OK(SIZE(ans) .EQ. 6, "3D: size")
!   CALL OK(MAXVAL(ABS(ans - v)) .LT. tol, "3D: values")
! END BLOCK
!
! ! Test 2: plane stress full keeps the 33 component, [11, 22, 12, 33]
! BLOCK
!   REAL(DFP), ALLOCATABLE :: ans(:)
!
!   ans = GetVoigtVector(v=v, voigtForm=VoigtFormPlaneStressFull)
!   CALL OK(SIZE(ans) .EQ. 4, "plane stress full: size")
!   CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
!           .LT. tol, "plane stress full: values")
! END BLOCK
!
! ! Test 3: uniaxial strain full returns the normal components
! BLOCK
!   REAL(DFP), ALLOCATABLE :: ans(:)
!
!   ans = GetVoigtVector(v=v, voigtForm=VoigtFormUniaxialStrainFull)
!   CALL OK(SIZE(ans) .EQ. 3, "uniaxial strain full: size")
!   CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 3.0_DFP])) .LT. tol, &
!           "uniaxial strain full: values")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetVoigtVector_]], [[VoigtFromForm]]

INTERFACE GetVoigtVector
  MODULE PURE FUNCTION Array_GetVoigtVector(v, voigtForm) RESULT(ans)
    REAL(DFP), INTENT(IN) :: v(6)
    !! 3D Voigt vector [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! vector of the form, SIZE VoigtSize(voigtForm)
  END FUNCTION Array_GetVoigtVector
END INTERFACE GetVoigtVector

!----------------------------------------------------------------------------
!                                                     GetVoigtVector_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Vector of a Voigt form from a 3D Voigt array (no allocation)
!
! Description:
!   Same as [[GetVoigtVector]] but the result is written in `ans`, which
!   is allocated by the caller. Only `ans(1:tsize)` is written. It is
!   meant for the quadrature point loops of finite element kernels, for
!   example to store a stress in plane strain with
!   VoigtFormPlaneStrainFull.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStrainFull
! USE GlobalData, ONLY: VoigtFormUniaxialStrain
! USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: GetVoigtVector_
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
! REAL(DFP), PARAMETER :: v(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, &
!                                 4.0_DFP, 5.0_DFP, 6.0_DFP]
!
! ! Test 1: 3D form writes all 6 entries
! BLOCK
!   REAL(DFP) :: ans(6)
!   INTEGER(I4B) :: tsize
!
!   CALL GetVoigtVector_(v=v, voigtForm=VoigtForm3D, ans=ans, tsize=tsize)
!   CALL OK(tsize .EQ. 6, "3D: tsize")
!   CALL OK(MAXVAL(ABS(ans - v)) .LT. tol, "3D: values")
! END BLOCK
!
! ! Test 2: plane strain writes [11, 22, 12], the rest is untouched
! BLOCK
!   REAL(DFP) :: ans(6)
!   INTEGER(I4B) :: tsize
!
!   ans = -1.0_DFP
!   CALL GetVoigtVector_(v=v, voigtForm=VoigtFormPlaneStrain, ans=ans, &
!                        tsize=tsize)
!   CALL OK(tsize .EQ. 3, "plane strain: tsize")
!   CALL OK(MAXVAL(ABS(ans(1:3) - [1.0_DFP, 2.0_DFP, 4.0_DFP])) .LT. tol, &
!           "plane strain: values")
!   CALL OK(ALL(ans(4:6) .EQ. -1.0_DFP), "plane strain: rest untouched")
! END BLOCK
!
! ! Test 3: plane strain full keeps the 33 component, [11, 22, 12, 33]
! BLOCK
!   REAL(DFP) :: ans(4)
!   INTEGER(I4B) :: tsize
!
!   CALL GetVoigtVector_(v=v, voigtForm=VoigtFormPlaneStrainFull, ans=ans, &
!                        tsize=tsize)
!   CALL OK(tsize .EQ. 4, "plane strain full: tsize")
!   CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
!           .LT. tol, "plane strain full: values")
! END BLOCK
!
! ! Test 4: uniaxial strain [11] and uniaxial strain full [11, 22, 33]
! BLOCK
!   REAL(DFP) :: ans(3)
!   INTEGER(I4B) :: tsize
!
!   CALL GetVoigtVector_(v=v, voigtForm=VoigtFormUniaxialStrain, ans=ans, &
!                        tsize=tsize)
!   CALL OK(tsize .EQ. 1, "uniaxial strain: tsize")
!   CALL OK(ABS(ans(1) - 1.0_DFP) .LT. tol, "uniaxial strain: values")
!   CALL GetVoigtVector_(v=v, voigtForm=VoigtFormUniaxialStrainFull, &
!                        ans=ans, tsize=tsize)
!   CALL OK(tsize .EQ. 3, "uniaxial strain full: tsize")
!   CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 3.0_DFP])) .LT. tol, &
!           "uniaxial strain full: values")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetVoigtVector]], [[VoigtFromForm]]

INTERFACE GetVoigtVector_
  MODULE PURE SUBROUTINE Array_GetVoigtVector_(v, voigtForm, ans, tsize)
    REAL(DFP), INTENT(IN) :: v(6)
    !! 3D Voigt vector [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! vector of the form, SIZE(ans) >= VoigtSize(voigtForm)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans, VoigtSize(voigtForm)
  END SUBROUTINE Array_GetVoigtVector_
END INTERFACE GetVoigtVector_

!----------------------------------------------------------------------------
!                                                       VoigtFromForm@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: 3D Voigt array from the vector of a Voigt form
!
! Description:
!   Inverse of [[GetVoigtVector_]]. The entries of `vec` are put at
!   their 3D Voigt positions (see [[GetVoigtIndex_]]). The components
!   that are not in the form are zero, for example sigma_33 for
!   VoigtFormPlaneStrain, or the out-of-plane shear components for every
!   2D form.
!
! Formula:
!   $$ \mathrm{ans}_{k_i} = \mathrm{vec}_i, \quad
!   \text{other entries are zero} $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStrainFull
! USE GlobalData, ONLY: VoigtFormUniaxialStrain
! USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
! USE Test_Method, ONLY: OK
! USE VoigtUtility, ONLY: GetVoigtVector_
! USE VoigtUtility, ONLY: VoigtFromForm
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
!
! ! Test 1: plane strain full [11, 22, 12, 33] to 3D
! BLOCK
!   REAL(DFP) :: ans(6), expected(6)
!
!   CALL VoigtFromForm(vec=[1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP], &
!                      voigtForm=VoigtFormPlaneStrainFull, ans=ans)
!   expected = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "plane strain full")
! END BLOCK
!
! ! Test 2: plane strain [11, 22, 12], the 33 component is zero
! BLOCK
!   REAL(DFP) :: ans(6), expected(6)
!
!   CALL VoigtFromForm(vec=[1.0_DFP, 2.0_DFP, 4.0_DFP], &
!                      voigtForm=VoigtFormPlaneStrain, ans=ans)
!   expected = [1.0_DFP, 2.0_DFP, 0.0_DFP, 4.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "plane strain")
! END BLOCK
!
! ! Test 3: uniaxial strain [11] and uniaxial strain full [11, 22, 33]
! BLOCK
!   REAL(DFP) :: ans(6), expected(6)
!
!   CALL VoigtFromForm(vec=[1.0_DFP], voigtForm=VoigtFormUniaxialStrain, &
!                      ans=ans)
!   expected = [1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "uniaxial strain")
!   CALL VoigtFromForm(vec=[1.0_DFP, 2.0_DFP, 3.0_DFP], &
!                      voigtForm=VoigtFormUniaxialStrainFull, ans=ans)
!   expected = [1.0_DFP, 2.0_DFP, 3.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
!   CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "uniaxial strain full")
! END BLOCK
!
! ! Test 4: round trip through GetVoigtVector_ in 3D
! BLOCK
!   REAL(DFP) :: v(6), vec(6), ans(6)
!   INTEGER(I4B) :: tsize
!
!   v = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!   CALL GetVoigtVector_(v=v, voigtForm=VoigtForm3D, ans=vec, tsize=tsize)
!   CALL VoigtFromForm(vec=vec(1:tsize), voigtForm=VoigtForm3D, ans=ans)
!   CALL OK(MAXVAL(ABS(ans - v)) .LT. tol, "3D: round trip")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetVoigtVector_]], [[GetVoigtIndex_]]

INTERFACE
  MODULE PURE SUBROUTINE VoigtFromForm(vec, voigtForm, ans)
    REAL(DFP), INTENT(IN) :: vec(:)
    !! vector of the form, SIZE(vec) >= VoigtSize(voigtForm)
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), INTENT(OUT) :: ans(6)
    !! 3D Voigt vector [11, 22, 33, 12, 23, 13]
  END SUBROUTINE VoigtFromForm
END INTERFACE

END MODULE VoigtUtility
