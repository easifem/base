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
! date: 2021-03-10
! summary: Methods for [[VoigtRank2Tensor_]]
!
! A symmetric rank-2 tensor $T$ is stored in Voigt notation as
! `V(6) = [T11, T22, T33, s*T12, s*T23, s*T13]`, where the factor `s`
! depends on `voigtType`:
!
! - `StressTypeVoigt`: `s = 1`, the shear entries are the tensor
!   components, and `scale = 1`.
! - `StrainTypeVoigt`: `s = 2`, the shear entries are the engineering
!   shear strains $\gamma_{ij} = 2\varepsilon_{ij}$, and `scale = 0.5`.
!
! In both cases the tensor component is recovered by
! $T_{ij} = \mathrm{scale}\, V_k$ for the shear entries. An unknown
! `voigtType` is treated as stress-like.
!
! Reduced forms (`VoigtForm*` constants in GlobalData) are available
! through [[GetVoigtSize]], [[GetVoigtVector]], [[GetVoigtVector_]] and
! the 4-argument [[Initiate]] and 3-argument [[VoigtRank2Tensor]]. The
! orderings are given by GetVoigtIndex_ of VoigtUtility, for example
! plane strain `[11,22,12]` and plane strain full `[11,22,12,33]`.
! [[VoigtSize]] is in VoigtUtility.
!
! Tensor operations on [[VoigtRank2Tensor_]] have the same generic names
! as for Rank2Tensor_: [[Trace]], [[Dev]], [[FrobeniusNorm]], [[Eigen]]
! and [[Otimes]] (TensorMethods). They use the Voigt type of the tensor,
! so a stress-like tensor and the same tensor written as an engineering
! strain give the same result.
!
! Each of them calls a PURE routine of VoigtUtility, which works on a
! plain 3D Voigt array `REAL(DFP) :: v(6)` in the same order:
! [[VoigtTrace]], [[VoigtDev]], [[VoigtStressNorm]], [[VoigtSymEigen]]
! and [[VoigtOuter]]. Use those routines directly in code that keeps its
! tensors in arrays, for example the stress update of a constitutive
! model.
!
! Changes:
!   - 2026-10-21: renamed the specific procedures, an unknown voigtType
!     is treated as stress-like, added the reduced 2D Voigt forms
!   - 2026-11-02: added GetVoigtVector_
!   - 2026-11-17: specific procedures renamed to obj_, interfaces
!     reordered, Display rewritten (array version, orient)
!   - 2026-11-17: added the tensor operations (TensorMethods),
!     easifem/base#355
!   - 2026-11-17: Voigt forms through VoigtUtility (VoigtSize moved
!     there and is no longer exported here), added GetVoigtSize and the
!     Full forms, easifem/base#356

MODULE VoigtRank2Tensor_Method
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate
PUBLIC :: VoigtRank2Tensor
PUBLIC :: VoigtRank2Tensor_Pointer
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: Display
PUBLIC :: GetVoigtSize
PUBLIC :: GetVoigtVector
PUBLIC :: GetVoigtVector_
PUBLIC :: Trace
PUBLIC :: Dev
PUBLIC :: FrobeniusNorm
PUBLIC :: Eigen
PUBLIC :: Otimes

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Initiate [[VoigtRank2Tensor_]] from a Voigt vector
!
! The Voigt vector is copied as it is. The scale is set from voigtType.
!
! Changes:
!   - 2026-10-21: an unknown voigtType is treated as stress-like
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: Initiate
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: from a Voigt vector
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   REAL(DFP) :: vec(6)
!
!   vec = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!   CALL Initiate(obj, vec, StressTypeVoigt)
!   CALL OK(MAXVAL(ABS(obj%V - vec)) .LT. tol, "vector: V")
!   CALL OK(obj%voigtType .EQ. StressTypeVoigt, "vector: voigtType")
!   CALL OK(ABS(obj%scale - 1.0_DFP) .LT. tol, "vector: scale is 1")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtRank2Tensor]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate1(obj, vec, voigtType)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    REAL(DFP), INTENT(IN) :: vec(6)
    !! Voigt vector [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Initiate [[VoigtRank2Tensor_]] from a 3x3 matrix
!
! The shear entries use the symmetric part of `T`, so `T` does not need
! to be symmetric.
!
! Changes:
!   - 2026-10-21: an unknown voigtType is treated as stress-like
!
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right),\quad
!   s = 1 \text{ (stress)},\ s = 2 \text{ (strain)}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: Initiate
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: T(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                            4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                            6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                           [3, 3])
!
! ! Test 2: from a matrix, strain-like
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!
!   CALL Initiate(obj, T, StrainTypeVoigt)
!   CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
!                               10.0_DFP, 12.0_DFP])) .LT. tol, &
!           "matrix: engineering shear")
!   CALL OK(ABS(obj%scale - 0.5_DFP) .LT. tol, "matrix: scale is 0.5")
! END BLOCK
!
! ! Test 3: a non-symmetric matrix uses its symmetric part
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   REAL(DFP) :: T2(3, 3)
!
!   T2 = T
!   T2(1, 2) = 2.0_DFP
!   CALL Initiate(obj, T2, StrainTypeVoigt)
!   CALL OK(ABS(obj%V(4) - 6.0_DFP) .LT. tol, "non-symmetric: V4")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtRank2Tensor]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate2(obj, T, voigtType)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    REAL(DFP), INTENT(IN) :: T(3, 3)
    !! tensor components, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 Initiate@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate [[VoigtRank2Tensor_]] from a reduced Voigt vector
!
! Only the first `VoigtSize(voigtForm)` entries of `vec` are read.
! Components that are not part of the reduced form are set to zero (see
! VoigtFromForm of VoigtUtility). For VoigtFormPlaneStrain and
! VoigtFormPlaneStress this includes the out-of-plane normal component,
! so the 3x3 tensor from `T = obj` has `T33 = 0` even when the physical
! value is not zero (for example $\sigma_{33}$ in plane strain). Use
! VoigtFormPlaneStrainFull or VoigtFormPlaneStressFull to keep it.
!
! Formula:
!   $$\text{plane strain}: V = [v_1, v_2, 0, v_3, 0, 0],\quad
!   \text{plane strain full}: V = [v_1, v_2, v_4, v_3, 0, 0]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressTypeVoigt
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStrainFull
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: Initiate
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 5: from a plane strain vector [11,22,12]
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!
!   CALL Initiate(obj, [1.0_DFP, 2.0_DFP, 4.0_DFP], StressTypeVoigt, &
!                 VoigtFormPlaneStrain)
!   CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 0.0_DFP, 4.0_DFP, &
!                               0.0_DFP, 0.0_DFP])) .LT. tol, &
!           "plane strain: V")
! END BLOCK
!
! ! Test 6: from a plane strain full vector [11,22,12,33], T33 is kept
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!
!   CALL Initiate(obj, [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP], &
!                 StressTypeVoigt, VoigtFormPlaneStrainFull)
!   CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
!                               0.0_DFP, 0.0_DFP])) .LT. tol, &
!           "plane strain full: V")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtRank2Tensor]], [[VoigtSize]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE obj_Initiate3(obj, vec, voigtType, voigtForm)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    REAL(DFP), INTENT(IN) :: vec(:)
    !! reduced Voigt vector, SIZE(vec) >= VoigtSize(voigtForm)
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
  END SUBROUTINE obj_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                        VoigtRank2Tensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Returns [[VoigtRank2Tensor_]] from a Voigt vector
!
! Function form of [[Initiate]] with a Voigt vector.
!
! Changes:
!   - 2026-10-21: an unknown voigtType is treated as stress-like
!
! Returns:
!   ans : VoigtRank2Tensor_
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: T(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                            4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                            6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                           [3, 3])
!
! ! Test 3: strain-like tensor from a Voigt vector
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   REAL(DFP) :: T2(3, 3)
!
!   obj = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
!                           10.0_DFP, 12.0_DFP], StrainTypeVoigt)
!   T2 = obj
!   CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "vector: tensor components")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE VoigtRank2Tensor
  MODULE PURE FUNCTION obj_VoigtRank2Tensor1(vec, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(6)
    !! Voigt vector [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    TYPE(VoigtRank2Tensor_) :: ans
    !! Voigt tensor
  END FUNCTION obj_VoigtRank2Tensor1
END INTERFACE VoigtRank2Tensor

!----------------------------------------------------------------------------
!                                        VoigtRank2Tensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Returns [[VoigtRank2Tensor_]] from a 3x3 matrix
!
! Function form of [[Initiate]] with a matrix. The shear entries use the
! symmetric part of `T`.
!
! Changes:
!   - 2026-10-21: an unknown voigtType is treated as stress-like
!
! Returns:
!   ans : VoigtRank2Tensor_
!
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE VoigtRank2Tensor_Method, ONLY: Display
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: T(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                            4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                            6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                           [3, 3])
!
! ! Test 1: stress-like tensor from a matrix, shear entries are T_ij
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   REAL(DFP) :: vec(6), T2(3, 3)
!
!   obj = VoigtRank2Tensor(T, StressTypeVoigt)
!   CALL Display(obj, "stress-like tensor")
!   vec = obj
!   CALL OK(MAXVAL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
!                             5.0_DFP, 6.0_DFP])) .LT. tol, &
!           "stress: Voigt vector")
!   CALL OK(ABS(obj%scale - 1.0_DFP) .LT. tol, "stress: scale is 1")
!   T2 = obj
!   CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "stress: round trip")
! END BLOCK
!
! ! Test 2: strain-like tensor from a matrix, shear entries are 2*e_ij
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   REAL(DFP) :: vec(6), T2(3, 3)
!
!   obj = VoigtRank2Tensor(T, StrainTypeVoigt)
!   vec = obj
!   CALL OK(MAXVAL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
!                             10.0_DFP, 12.0_DFP])) .LT. tol, &
!           "strain: engineering shear")
!   CALL OK(ABS(obj%scale - 0.5_DFP) .LT. tol, "strain: scale is 0.5")
!   T2 = obj
!   CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "strain: round trip")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE VoigtRank2Tensor
  MODULE PURE FUNCTION obj_VoigtRank2Tensor2(T, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: T(3, 3)
    !! tensor components, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    TYPE(VoigtRank2Tensor_) :: ans
    !! Voigt tensor
  END FUNCTION obj_VoigtRank2Tensor2
END INTERFACE VoigtRank2Tensor

!----------------------------------------------------------------------------
!                                         VoigtRank2Tensor@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns [[VoigtRank2Tensor_]] from a reduced Voigt vector
!
! Function form of the 4-argument [[Initiate]]. Components that are not
! part of the reduced form are set to zero.
!
! Returns:
!   ans : VoigtRank2Tensor_
!
! Formula:
!   $$\text{axisymmetric}: V = [v_1, v_2, v_4, v_3, 0, 0]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: VoigtFormAxisymmetric
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 4: strain-like tensor from an axisymmetric vector [11,22,12,33]
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   REAL(DFP) :: T2(3, 3)
!
!   obj = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP], &
!                          StrainTypeVoigt, VoigtFormAxisymmetric)
!   T2 = obj
!   CALL OK(ABS(T2(1, 2) - 2.0_DFP) .LT. tol, "axisymmetric: T12")
!   CALL OK(ABS(T2(3, 3) - 3.0_DFP) .LT. tol, "axisymmetric: T33")
!   CALL OK(ABS(T2(2, 3)) .LT. tol, "axisymmetric: T23 is zero")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE VoigtRank2Tensor
  MODULE PURE FUNCTION obj_VoigtRank2Tensor3(vec, voigtType, &
                                             voigtForm) RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(:)
    !! reduced Voigt vector, SIZE(vec) >= VoigtSize(voigtForm)
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    TYPE(VoigtRank2Tensor_) :: ans
    !! Voigt tensor
  END FUNCTION obj_VoigtRank2Tensor3
END INTERFACE VoigtRank2Tensor

!----------------------------------------------------------------------------
!                                VoigtRank2Tensor_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Returns a pointer to a new [[VoigtRank2Tensor_]]
!
! Pointer form of [[VoigtRank2Tensor]] with a Voigt vector. The caller
! must deallocate the pointer.
!
! Changes:
!   - 2026-10-21: an unknown voigtType is treated as stress-like
!
! Returns:
!   ans : CLASS(VoigtRank2Tensor_), POINTER
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor_Pointer
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: pointer from a stress-like Voigt vector
! BLOCK
!   CLASS(VoigtRank2Tensor_), POINTER :: ptr
!   REAL(DFP) :: vec(6)
!
!   vec = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
!   ptr => VoigtRank2Tensor_Pointer(vec, StressTypeVoigt)
!   CALL OK(ASSOCIATED(ptr), "vector: associated")
!   CALL OK(MAXVAL(ABS(ptr%V - vec)) .LT. tol, "vector: V")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE VoigtRank2Tensor_Pointer
  MODULE PURE FUNCTION obj_VoigtRank2Tensor_Pointer1(vec, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(6)
    !! Voigt vector [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    CLASS(VoigtRank2Tensor_), POINTER :: ans
    !! new Voigt tensor, the caller must deallocate it
  END FUNCTION obj_VoigtRank2Tensor_Pointer1
END INTERFACE VoigtRank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                VoigtRank2Tensor_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Returns a pointer to a new [[VoigtRank2Tensor_]]
!
! Pointer form of [[VoigtRank2Tensor]] with a matrix. The caller must
! deallocate the pointer.
!
! Changes:
!   - 2026-10-21: an unknown voigtType is treated as stress-like
!
! Returns:
!   ans : CLASS(VoigtRank2Tensor_), POINTER
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor_Pointer
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 2: pointer from a strain-like matrix
! BLOCK
!   CLASS(VoigtRank2Tensor_), POINTER :: ptr
!   REAL(DFP) :: T(3, 3)
!
!   T = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                6.0_DFP, 5.0_DFP, 3.0_DFP], [3, 3])
!   ptr => VoigtRank2Tensor_Pointer(T, StrainTypeVoigt)
!   CALL OK(MAXVAL(ABS(ptr%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
!                               10.0_DFP, 12.0_DFP])) .LT. tol, &
!           "matrix: engineering shear")
!   CALL OK(ABS(ptr%scale - 0.5_DFP) .LT. tol, "matrix: scale is 0.5")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE VoigtRank2Tensor_Pointer
  MODULE PURE FUNCTION obj_VoigtRank2Tensor_Pointer2(T, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: T(3, 3)
    !! tensor components, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    CLASS(VoigtRank2Tensor_), POINTER :: ans
    !! new Voigt tensor, the caller must deallocate it
  END FUNCTION obj_VoigtRank2Tensor_Pointer2
END INTERFACE VoigtRank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Matrix(3,3) = [[VoigtRank2Tensor_]]
!
! Returns the symmetric 3x3 tensor. The shear entries are multiplied by
! the scale of obj.
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{scale}\, V_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: T(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                            4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                            6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                           [3, 3])
!
! ! Test 2: strain-like tensor from a matrix, shear entries are 2*e_ij
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   REAL(DFP) :: vec(6), T2(3, 3)
!
!   obj = VoigtRank2Tensor(T, StrainTypeVoigt)
!   vec = obj
!   CALL OK(MAXVAL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
!                             10.0_DFP, 12.0_DFP])) .LT. tol, &
!           "strain: engineering shear")
!   CALL OK(ABS(obj%scale - 0.5_DFP) .LT. tol, "strain: scale is 0.5")
!   T2 = obj
!   CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "strain: round trip")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[obj_Assign2]]

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_Assign1(T, obj)
    REAL(DFP), INTENT(INOUT) :: T(3, 3)
    !! symmetric tensor components
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
  END SUBROUTINE obj_Assign1
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Vector(6) = [[VoigtRank2Tensor_]]
!
! Returns the Voigt vector `obj%V` as it is stored, so the shear entries
! of a strain-like tensor are engineering strains.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: T(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                            4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                            6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                           [3, 3])
!
! ! Test 2: strain-like tensor from a matrix, shear entries are 2*e_ij
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   REAL(DFP) :: vec(6), T2(3, 3)
!
!   obj = VoigtRank2Tensor(T, StrainTypeVoigt)
!   vec = obj
!   CALL OK(MAXVAL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
!                             10.0_DFP, 12.0_DFP])) .LT. tol, &
!           "strain: engineering shear")
!   CALL OK(ABS(obj%scale - 0.5_DFP) .LT. tol, "strain: scale is 0.5")
!   T2 = obj
!   CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "strain: round trip")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[obj_Assign1]]

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE obj_Assign2(vec, obj)
    REAL(DFP), INTENT(INOUT) :: vec(6)
    !! Voigt vector [11, 22, 33, 12, 23, 13]
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
  END SUBROUTINE obj_Assign2
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Display the content of [[VoigtRank2Tensor_]]
!
! Description:
!   Prints the message, the Voigt type (stress-like, strain-like, or
!   unknown which is treated as stress-like), the scale of the shear
!   entries and the Voigt vector V.
!
! Arguments:
!   obj : VoigtRank2Tensor_ - tensor
!   msg : CHARACTER(*) - header message
!   unitNo : INTEGER(I4B), OPTIONAL - output unit, default is stdout
!   orient : CHARACTER(*), OPTIONAL - "row" (default) or "col" for V
!
! Changes:
!   - 2026-11-17: rewritten after IntVector_IOMethod; prints each
!     component, added orient and the array version
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: Display
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: vec(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, &
!                                   4.0_DFP, 5.0_DFP, 6.0_DFP]
!
! ! Test 1: stress-like tensor
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   INTEGER(I4B) :: unitNo
!
!   obj = VoigtRank2Tensor(vec, StressTypeVoigt)
!   OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
!   CALL Display(obj, "my tensor", unitNo=unitNo)
!   CALL OK(Found(unitNo, "my tensor"), "stress: message")
!   CALL OK(Found(unitNo, "voigtType: stress-like"), "stress: voigtType")
!   CALL OK(Found(unitNo, "scale: "), "stress: scale")
!   CALL OK(Found(unitNo, "V: "), "stress: V")
!   CLOSE (unitNo)
! END BLOCK
!
! ! Test 2: strain-like tensor, column orientation
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!   INTEGER(I4B) :: unitNo
!
!   obj = VoigtRank2Tensor(vec, StrainTypeVoigt)
!   OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
!   CALL Display(obj, "my strain", unitNo=unitNo, orient="col")
!   CALL OK(Found(unitNo, "voigtType: strain-like"), "strain: voigtType")
!   CLOSE (unitNo)
! END BLOCK
!
! ! Test 3: array of tensors
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj(2)
!   INTEGER(I4B) :: unitNo
!
!   obj(1) = VoigtRank2Tensor(vec, StressTypeVoigt)
!   obj(2) = VoigtRank2Tensor(vec, StrainTypeVoigt)
!   OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
!   CALL Display(obj, "my tensors", unitNo=unitNo)
!   CALL OK(Found(unitNo, "my tensors"), "array: message")
!   CALL OK(Found(unitNo, "size: 2"), "array: size")
!   CALL OK(Found(unitNo, "(1)"), "array: first entry")
!   CALL OK(Found(unitNo, "(2)"), "array: second entry")
!   CLOSE (unitNo)
! END BLOCK
!
! CONTAINS
!
! ! Looks for text in the lines written to unitNo
! FUNCTION Found(unitNo, text) RESULT(ans)
!   INTEGER(I4B), INTENT(IN) :: unitNo
!   CHARACTER(*), INTENT(IN) :: text
!   LOGICAL :: ans
!
!   CHARACTER(256) :: line, errmsg
!   INTEGER(I4B) :: ios
!
!   REWIND (unitNo)
!   ans = .FALSE.
!   scan: DO
!     READ (unitNo, "(A)", iostat=ios, iomsg=errmsg) line
!     IF (ios .NE. 0) EXIT scan
!     ans = ans .OR. (INDEX(line, text) .GT. 0)
!   END DO scan
! END FUNCTION Found
!
! END PROGRAM main
!```

INTERFACE Display
  MODULE SUBROUTINE obj_Display1(obj, msg, unitNo, orient)
    TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! tensor to display
    CHARACTER(*), INTENT(IN) :: msg
    !! header message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! output unit, default is stdout
    CHARACTER(*), OPTIONAL, INTENT(IN) :: orient
    !! orientation of V, "row" (default) or "col"
  END SUBROUTINE obj_Display1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Display an array of [[VoigtRank2Tensor_]]
!
! Description:
!   Prints the message and the size of the array, then each tensor with
!   the message "(j)".
!
! Arguments:
!   obj : VoigtRank2Tensor_, rank(1) - tensors
!   msg : CHARACTER(*) - header message
!   unitNo : INTEGER(I4B), OPTIONAL - output unit, default is stdout
!   orient : CHARACTER(*), OPTIONAL - "row" (default) or "col" for V
!
! Examples
!
! See the example of the scalar version (Test 3).

INTERFACE Display
  MODULE SUBROUTINE obj_Display2(obj, msg, unitNo, orient)
    TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj(:)
    !! tensors to display
    CHARACTER(*), INTENT(IN) :: msg
    !! header message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! output unit, default is stdout
    CHARACTER(*), OPTIONAL, INTENT(IN) :: orient
    !! orientation of V, "row" (default) or "col"
  END SUBROUTINE obj_Display2
END INTERFACE Display

!----------------------------------------------------------------------------
!                                               GetVoigtSize@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Length of the vector of a [[VoigtRank2Tensor_]] in a Voigt form
!
! Description:
!   Returns the size of [[GetVoigtVector]] of `obj` for `voigtForm`. The
!   tensor itself always stores the 6 components of the 3D form, so the
!   result is `VoigtSize(voigtForm)` of VoigtUtility.
!
! Returns:
!   ans : INTEGER(I4B) - length of the vector of the form
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressTypeVoigt
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStrainFull
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: GetVoigtSize
! USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! ! Test 1: size of the vector of obj in a Voigt form
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: obj
!
!   obj = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
!                           6.0_DFP], StressTypeVoigt)
!   CALL OK(GetVoigtSize(obj, VoigtForm3D) .EQ. 6, "3D: 6")
!   CALL OK(GetVoigtSize(obj, VoigtFormPlaneStrain) .EQ. 3, &
!           "plane strain: 3")
!   CALL OK(GetVoigtSize(obj, VoigtFormPlaneStrainFull) .EQ. &
!           SIZE(GetVoigtVector(obj, VoigtFormPlaneStrainFull)), &
!           "plane strain full: same as SIZE(GetVoigtVector)")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetVoigtVector]], [[VoigtSize]]

INTERFACE GetVoigtSize
  MODULE PURE FUNCTION obj_GetVoigtSize(obj, voigtForm) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    INTEGER(I4B) :: ans
    !! length of the vector of the form
  END FUNCTION obj_GetVoigtSize
END INTERFACE GetVoigtSize

!----------------------------------------------------------------------------
!                                           GetVoigtVector@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Reduced Voigt vector of a [[VoigtRank2Tensor_]]
!
! The orderings are given by GetVoigtIndex_ of VoigtUtility, for
! example 3D `[11,22,33,12,23,13]`, plane strain and plane stress
! `[11,22,12]`, axisymmetric and plane strain full `[11,22,12,33]`. For
! VoigtFormPlaneStrain and VoigtFormPlaneStress the out-of-plane normal
! component is not returned (for example $\sigma_{33}$ in plane strain);
! the Full forms keep it. Unknown forms give the 3D vector.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE - SIZE(ans) = VoigtSize(voigtForm)
!
! Formula:
!   $$\text{plane strain}: [V_1, V_2, V_4],\quad
!   \text{axisymmetric}: [V_1, V_2, V_4, V_3]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressTypeVoigt
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormAxisymmetric
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStrainFull
! USE GlobalData, ONLY: VoigtFormPlaneStress
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
!                                  5.0_DFP, 6.0_DFP]
! TYPE(VoigtRank2Tensor_) :: obj
!
! obj = VoigtRank2Tensor(v6, StressTypeVoigt)
!
! ! Test 1: 3D form returns all 6 entries
! BLOCK
!   REAL(DFP), ALLOCATABLE :: v(:)
!
!   v = GetVoigtVector(obj, VoigtForm3D)
!   CALL OK(SIZE(v) .EQ. 6, "3D: size")
!   CALL OK(MAXVAL(ABS(v - v6)) .LT. tol, "3D: values")
! END BLOCK
!
! ! Test 2: plane strain and plane stress return [11,22,12]
! BLOCK
!   REAL(DFP), ALLOCATABLE :: v(:)
!
!   v = GetVoigtVector(obj, VoigtFormPlaneStrain)
!   CALL OK(SIZE(v) .EQ. 3, "plane strain: size")
!   CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP])) .LT. tol, &
!           "plane strain: values")
!   v = GetVoigtVector(obj, VoigtFormPlaneStress)
!   CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP])) .LT. tol, &
!           "plane stress: values")
! END BLOCK
!
! ! Test 3: axisymmetric returns [11,22,12,33]
! BLOCK
!   REAL(DFP), ALLOCATABLE :: v(:)
!
!   v = GetVoigtVector(obj, VoigtFormAxisymmetric)
!   CALL OK(SIZE(v) .EQ. 4, "axisymmetric: size")
!   CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
!           .LT. tol, "axisymmetric: values")
! END BLOCK
!
! ! Test 4: plane strain full keeps the 33 component, [11,22,12,33]
! BLOCK
!   REAL(DFP), ALLOCATABLE :: v(:)
!
!   v = GetVoigtVector(obj, VoigtFormPlaneStrainFull)
!   CALL OK(SIZE(v) .EQ. 4, "plane strain full: size")
!   CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
!           .LT. tol, "plane strain full: values")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetVoigtVector_]], [[VoigtSize]]

INTERFACE GetVoigtVector
  MODULE PURE FUNCTION obj_GetVoigtVector(obj, voigtForm) &
    RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! reduced Voigt vector, SIZE VoigtSize(voigtForm)
  END FUNCTION obj_GetVoigtVector
END INTERFACE GetVoigtVector

!----------------------------------------------------------------------------
!                                          GetVoigtVector_@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-02
! summary: Reduced Voigt vector of a [[VoigtRank2Tensor_]] (no allocation)
!
! Same as [[GetVoigtVector]] but the result is written in `ans`, which
! is allocated by the caller. Only `ans(1:tsize)` is written. This
! routine is intended for the quadrature point loops of finite element
! kernels.
!
! Formula:
!   $$\text{plane strain}: [V_1, V_2, V_4],\quad
!   \text{axisymmetric}: [V_1, V_2, V_4, V_3]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: StressTypeVoigt
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormAxisymmetric
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector_
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
!                                  5.0_DFP, 6.0_DFP]
! TYPE(VoigtRank2Tensor_) :: obj
!
! obj = VoigtRank2Tensor(v6, StressTypeVoigt)
!
! ! Test 1: 3D form writes 6 entries
! BLOCK
!   REAL(DFP) :: ans(6)
!   INTEGER(I4B) :: tsize
!
!   CALL GetVoigtVector_(obj, VoigtForm3D, ans, tsize)
!   CALL OK(tsize .EQ. 6, "3D: tsize")
!   CALL OK(MAXVAL(ABS(ans - v6)) .LT. tol, "3D: values")
! END BLOCK
!
! ! Test 2: plane strain writes [11,22,12], the rest of ans is untouched
! BLOCK
!   REAL(DFP) :: ans(6)
!   INTEGER(I4B) :: tsize
!
!   ans = -1.0_DFP
!   CALL GetVoigtVector_(obj, VoigtFormPlaneStrain, ans, tsize)
!   CALL OK(tsize .EQ. 3, "plane strain: tsize")
!   CALL OK(MAXVAL(ABS(ans(1:3) - [1.0_DFP, 2.0_DFP, 4.0_DFP])) .LT. tol, &
!           "plane strain: values")
!   CALL OK(ALL(ans(4:6) .EQ. -1.0_DFP), "plane strain: rest untouched")
! END BLOCK
!
! ! Test 3: axisymmetric writes [11,22,12,33]
! BLOCK
!   REAL(DFP) :: ans(4)
!   INTEGER(I4B) :: tsize
!
!   CALL GetVoigtVector_(obj, VoigtFormAxisymmetric, ans, tsize)
!   CALL OK(tsize .EQ. 4, "axisymmetric: tsize")
!   CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
!           .LT. tol, "axisymmetric: values")
! END BLOCK
!
! ! Test 4: uniaxial strain full writes the normal components [11,22,33]
! BLOCK
!   REAL(DFP) :: ans(3)
!   INTEGER(I4B) :: tsize
!
!   CALL GetVoigtVector_(obj, VoigtFormUniaxialStrainFull, ans, tsize)
!   CALL OK(tsize .EQ. 3, "uniaxial strain full: tsize")
!   CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 3.0_DFP])) .LT. tol, &
!           "uniaxial strain full: values")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetVoigtVector]], [[VoigtSize]]

INTERFACE GetVoigtVector_
  MODULE PURE SUBROUTINE obj_GetVoigtVector_(obj, voigtForm, ans, tsize)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! reduced Voigt vector, SIZE(ans) >= VoigtSize(voigtForm)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans, VoigtSize(voigtForm)
  END SUBROUTINE obj_GetVoigtVector_
END INTERFACE GetVoigtVector_

!----------------------------------------------------------------------------
!                                                         Trace@TensorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Trace of a [[VoigtRank2Tensor_]]
!
! Same value for stress-like and strain-like tensors (VoigtTrace of V).
!
! Returns:
!   ans : REAL(DFP) - trace
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: Trace
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
! REAL(DFP), PARAMETER :: v(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
!                                 5.0_DFP, 6.0_DFP]
!
! ! Test 1: stress-like and strain-like tensors have the same trace
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: sig, eps
!
!   sig = VoigtRank2Tensor(v, StressTypeVoigt)
!   eps = VoigtRank2Tensor(v, StrainTypeVoigt)
!   CALL OK(ABS(Trace(sig) - 6.0_DFP) .LT. tol, "Trace: stress")
!   CALL OK(ABS(Trace(eps) - 6.0_DFP) .LT. tol, "Trace: strain")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtTrace]]

INTERFACE Trace
  MODULE PURE FUNCTION obj_Trace(obj) RESULT(ans)
    TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    REAL(DFP) :: ans
    !! trace
  END FUNCTION obj_Trace
END INTERFACE Trace

!----------------------------------------------------------------------------
!                                                           Dev@TensorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Deviatoric part of a [[VoigtRank2Tensor_]]
!
! The mean of the normal components is removed (VoigtDev of V). The shear
! entries, the Voigt type and the scale are kept, so the deviator of an
! engineering strain is again an engineering strain.
!
! Returns:
!   ans : TYPE(VoigtRank2Tensor_) - deviatoric part
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: Dev
! USE VoigtRank2Tensor_Method, ONLY: Trace
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
!
! ! Test 1: the mean is removed, the shear entries and the Voigt type are
! ! kept (an engineering strain stays an engineering strain)
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: eps, e
!   REAL(DFP) :: expected(6)
!
!   eps = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 6.0_DFP, 4.0_DFP, 5.0_DFP, &
!                           7.0_DFP], StrainTypeVoigt)
!   e = Dev(eps)
!   expected = [-2.0_DFP, -1.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 7.0_DFP]
!   CALL OK(MAXVAL(ABS(e%V - expected)) .LT. tol, "Dev: values")
!   CALL OK(ABS(Trace(e)) .LT. tol, "Dev: trace is zero")
!   CALL OK(e%VoigtType .EQ. StrainTypeVoigt, "Dev: Voigt type is kept")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtDev]], [[Trace]]

INTERFACE Dev
  MODULE PURE FUNCTION obj_Dev(obj) RESULT(ans)
    TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    TYPE(VoigtRank2Tensor_) :: ans
    !! deviatoric part, same Voigt type
  END FUNCTION obj_Dev
END INTERFACE Dev

!----------------------------------------------------------------------------
!                                                 FrobeniusNorm@TensorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Frobenius norm of a [[VoigtRank2Tensor_]]
!
! The shear entries are converted to tensor components with the scale of
! the tensor (1 for stress, 1/2 for an engineering strain), then
! VoigtStressNorm is used. So a stress and the same tensor written as an
! engineering strain have the same norm.
!
! Returns:
!   ans : REAL(DFP) - norm of the tensor
!
! Formula:
!   $$ \| T \| = \sqrt{T_{ij} T_{ij}} $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: FrobeniusNorm
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-13_DFP
!
! ! Test 1: stress-like tensor, shear entries are tensor components
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: sig
!
!   sig = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
!                           6.0_DFP], StressTypeVoigt)
!   CALL OK(ABS(FrobeniusNorm(sig) - SQRT(168.0_DFP)) .LT. tol, &
!           "FrobeniusNorm: stress")
! END BLOCK
!
! ! Test 2: the same tensor as an engineering strain gives the same norm
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: eps
!
!   eps = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, 10.0_DFP, &
!                           12.0_DFP], StrainTypeVoigt)
!   CALL OK(ABS(FrobeniusNorm(eps) - SQRT(168.0_DFP)) .LT. tol, &
!           "FrobeniusNorm: engineering strain")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtStressNorm]]

INTERFACE FrobeniusNorm
  MODULE PURE FUNCTION obj_FrobeniusNorm(obj) RESULT(ans)
    TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    REAL(DFP) :: ans
    !! Frobenius norm
  END FUNCTION obj_FrobeniusNorm
END INTERFACE FrobeniusNorm

!----------------------------------------------------------------------------
!                                                         Eigen@TensorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Eigenvalues and eigenvectors of a [[VoigtRank2Tensor_]]
!
! The shear entries are converted to tensor components with the scale of
! the tensor, then VoigtSymEigen is used (cyclic Jacobi, PURE, relative
! tolerance). The eigenvalues are in descending order. The argument names
! follow Eigen of Rank2Tensor_.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: Eigen
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: diagonal stress, eigenvalues in descending order
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: sig
!   REAL(DFP) :: QR(3, 3), WR(3)
!
!   sig = VoigtRank2Tensor([1.0_DFP, 3.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, &
!                           0.0_DFP], StressTypeVoigt)
!   CALL Eigen(obj=sig, QR=QR, WR=WR)
!   CALL OK(MAXVAL(ABS(WR - [3.0_DFP, 2.0_DFP, 1.0_DFP])) .LT. tol, &
!           "Eigen: values")
!   CALL OK(ABS(ABS(QR(2, 1)) - 1.0_DFP) .LT. tol, &
!           "Eigen: first vector is e2")
! END BLOCK
!
! ! Test 2: an engineering strain has the eigenvalues of the strain tensor
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: sig, eps
!   REAL(DFP) :: QR1(3, 3), WR1(3), QR2(3, 3), WR2(3)
!   INTEGER(I4B) :: ii
!
!   sig = VoigtRank2Tensor([4.0_DFP, 1.0_DFP, 2.0_DFP, 0.5_DFP, 0.3_DFP, &
!                           0.2_DFP], StressTypeVoigt)
!   eps = VoigtRank2Tensor([4.0_DFP, 1.0_DFP, 2.0_DFP, 1.0_DFP, 0.6_DFP, &
!                           0.4_DFP], StrainTypeVoigt)
!   CALL Eigen(obj=sig, QR=QR1, WR=WR1)
!   CALL Eigen(obj=eps, QR=QR2, WR=WR2)
!   CALL OK(MAXVAL(ABS(WR1 - WR2)) .LT. tol, "Eigen: strain, values")
!   DO ii = 1, 3
!     CALL OK(ABS(ABS(DOT_PRODUCT(QR1(:, ii), QR2(:, ii))) - 1.0_DFP) &
!             .LT. tol, "Eigen: strain, vectors")
!   END DO
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtSymEigen]]

INTERFACE Eigen
  MODULE PURE SUBROUTINE obj_Eigen(obj, QR, WR)
    TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    REAL(DFP), INTENT(INOUT) :: QR(3, 3)
    !! unit eigenvectors in columns, QR(:, i) belongs to WR(i)
    REAL(DFP), INTENT(INOUT) :: WR(3)
    !! eigenvalues in descending order
  END SUBROUTINE obj_Eigen
END INTERFACE Eigen

!----------------------------------------------------------------------------
!                                                        Otimes@TensorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Dyadic product of two [[VoigtRank2Tensor_]] in Voigt form
!
! Both tensors are converted to tensor shear components with their
! scale, then VoigtOuter is used. The result C(6, 6) is the rank 4 tensor
! A otimes B in the convention of MaterialJacobian_: it maps an
! engineering strain to a stress, C eps = A (B : eps).
!
! Returns:
!   ans : REAL(DFP), SHAPE(6, 6) - A otimes B in Voigt form
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: Otimes
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! USE VoigtUtility, ONLY: VoigtIdentity
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
!
! ! Test 1: I otimes I
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: eye
!
!   eye = VoigtRank2Tensor(VoigtIdentity(), StressTypeVoigt)
!   CALL OK(MAXVAL(ABS(Otimes(eye, eye) - Rank4IdentityOtimesIdentity())) &
!           .LT. tol, "Otimes: I otimes I")
! END BLOCK
!
! ! Test 2: (a otimes b) : eps = a (b : eps) for an engineering strain eps,
! ! also when b is given as an engineering strain
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: a, b, bEng
!   REAL(DFP) :: eps(6), bDotEps
!
!   a = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
!                         6.0_DFP], StressTypeVoigt)
!   b = VoigtRank2Tensor([6.0_DFP, 5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, &
!                         1.0_DFP], StressTypeVoigt)
!   bEng = VoigtRank2Tensor([6.0_DFP, 5.0_DFP, 4.0_DFP, 6.0_DFP, 4.0_DFP, &
!                            2.0_DFP], StrainTypeVoigt)
!   eps = [0.1_DFP, 0.2_DFP, 0.3_DFP, 0.4_DFP, 0.5_DFP, 0.6_DFP]
!   bDotEps = DOT_PRODUCT(b%V, eps)
!   CALL OK(MAXVAL(ABS(MATMUL(Otimes(a, b), eps) - a%V * bDotEps)) &
!           .LT. tol, "Otimes: (a otimes b) : eps")
!   CALL OK(MAXVAL(ABS(Otimes(a, bEng) - Otimes(a, b))) .LT. tol, &
!           "Otimes: b as an engineering strain")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[VoigtOuter]]

INTERFACE Otimes
  MODULE PURE FUNCTION obj_Otimes(obj1, obj2) RESULT(ans)
    TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    REAL(DFP) :: ans(6, 6)
    !! A otimes B in Voigt form
  END FUNCTION obj_Otimes
END INTERFACE Otimes

END MODULE VoigtRank2Tensor_Method
