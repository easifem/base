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
! Voigt notation keeps a symmetric rank-2 tensor $T$ as
! `V(6) = [T11, T22, T33, s*T12, s*T23, s*T13]`, where the factor `s`
! depends on `voigtType`:
!
! - `StressTypeVoigt`: `s = 1`, the shear entries are the tensor
!   components, and `scale = 1`.
! - `StrainTypeVoigt`: `s = 2`, the shear entries are the engineering
!   shear strains $\gamma_{ij} = 2\varepsilon_{ij}$, and `scale = 0.5`.
!
! In both cases you get a shear component back with
! $T_{ij} = \mathrm{scale}\, V_k$. An unknown `voigtType` acts as
! stress-like.
!
! Reduced forms (`VoigtForm*` constants in GlobalData) are available
! through [[GetVoigtSize]], [[GetVoigtVector]], [[GetVoigtVector_]], the
! 4-argument [[Initiate]] and the 3-argument [[VoigtRank2Tensor]].
! `GetVoigtIndex_` of VoigtUtility gives the orderings. For example,
! plane strain is `[11,22,12]` and plane strain full is
! `[11,22,12,33]`. [[VoigtSize]] is in VoigtUtility.
!
! The tensor operations on [[VoigtRank2Tensor_]] have the same generic
! names as for `Rank2Tensor_`: [[Trace]], [[Dev]], [[FrobeniusNorm]],
! [[Eigen]] and [[Otimes]]. They use the Voigt type of the tensor. So a
! stress-like tensor and the same tensor written as an engineering
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
!   - 2026-10-21: renamed the specific procedures. An unknown
!     `voigtType` now acts as stress-like. Added the reduced 2D Voigt
!     forms.
!   - 2026-11-02: added `GetVoigtVector_`.
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
! Sets `obj` from a Voigt vector. The routine copies the vector as it is.
!
! The routine stores `voigtType` in `obj%voigtType`. It sets
! `obj%scale` to 0.5 for `StrainTypeVoigt` and to 1 for every other
! value, so an unknown `voigtType` acts as stress-like.
!
! Changes:
!   - 2026-10-21: an unknown `voigtType` now acts as stress-like
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Initiate_test_1.F90)
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
! Sets `obj` from a 3x3 matrix `T`. The shear entries use the symmetric
! part of `T`, so `T` does not need to be symmetric.
!
! The routine stores `voigtType` in `obj%voigtType`. It sets
! `obj%scale` to 0.5 for `StrainTypeVoigt` and to 1 for every other
! value, so an unknown `voigtType` acts as stress-like.
!
! Changes:
!   - 2026-10-21: an unknown `voigtType` now acts as stress-like
!
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right),\quad
!   s = 1 \text{ (stress)},\ s = 2 \text{ (strain)}$$
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Initiate_test_1.F90)
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
! Initiates `obj` from a reduced Voigt vector `vec`.
! The routine reads only the first `VoigtSize(voigtForm)` entries of
! `vec`.
!
! The routine sets components that are not part of the reduced form to
! zero (see `VoigtFromForm` of VoigtUtility). For
! `VoigtFormPlaneStrain` and `VoigtFormPlaneStress` this includes the
! out-of-plane normal component. Hence the 3x3 tensor from `T = obj`
! has `T33 = 0` even when the physical value is not zero. Examples are
! $\sigma_{33}$ in plane strain and $\varepsilon_{33}$ in plane stress.
! Use `VoigtFormPlaneStrainFull` or `VoigtFormPlaneStressFull` to keep
! it.
!
! @note
! `SIZE(vec)` must be at least `VoigtSize(voigtForm)`. The routine does
! not check this. With a shorter vector, it reads out of bounds.
! @endnote
!
! Formula:
!   With `v_k = vec(k)`, the Voigt vector `V` of `obj` is:
!   $$\text{plane strain}: V = [v_1, v_2, 0, v_3, 0, 0],\quad
!   \text{plane strain full}: V = [v_1, v_2, v_4, v_3, 0, 0]$$
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Initiate_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/Initiate_test_2.F90)
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
! Returns a new tensor made from a Voigt vector. It is the function
! form of [[Initiate]].
!
! Changes:
!   - 2026-10-21: an unknown `voigtType` now acts as stress-like
!
! Returns:
!   ans : `VoigtRank2Tensor_`
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_test_2.F90)
!
! See also: [[Initiate]]

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
! Returns a new tensor made from a 3x3 matrix `T`. It is the function
! form of [[Initiate]]. The shear entries use the symmetric part of `T`.
!
! Changes:
!   - 2026-10-21: an unknown `voigtType` now acts as stress-like
!
! Returns:
!   ans : `VoigtRank2Tensor_`
!
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right)$$
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_test_2.F90)
!
! See also: [[Initiate]]

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
! Returns a tensor built from a reduced Voigt vector `vec`.
! It is the function form of the 4-argument [[Initiate]].
!
! The routine sets components that are not part of the reduced form to
! zero. An unknown `voigtForm` acts as `VoigtForm3D`.
!
! @note
! `SIZE(vec)` must be at least `VoigtSize(voigtForm)`. The routine does
! not check this. With a shorter vector, it reads out of bounds.
! @endnote
!
! Returns:
!   ans : `VoigtRank2Tensor_`
!
! Formula:
!   With `v_k = vec(k)`, the Voigt vector `V` of `ans` is, for the
!   axisymmetric form:
!   $$V = [v_1, v_2, v_4, v_3, 0, 0]$$
!   Thus `vec = [11, 22, 12, 33]` gives `V(3) = v_4` and `V(4) = v_3`.
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_test_2.F90)
!
! See also: [[Initiate]], [[VoigtSize]]

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
! Allocates a new tensor from a Voigt vector and returns a pointer to
! it. It is the pointer form of [[VoigtRank2Tensor]].
!
! @note
! The caller must deallocate the pointer.
! @endnote
!
! Changes:
!   - 2026-10-21: an unknown `voigtType` now acts as stress-like
!
! Returns:
!   ans : `CLASS(VoigtRank2Tensor_), POINTER`
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_Pointer_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_Pointer_test_2.F90)
!
! See also: [[VoigtRank2Tensor]]

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
! Allocates a new tensor from a 3x3 matrix `T` and returns a pointer to
! it. It is the pointer form of [[VoigtRank2Tensor]]. The shear entries
! use the symmetric part of `T`.
!
! @note
! The caller must deallocate the pointer.
! @endnote
!
! Changes:
!   - 2026-10-21: an unknown `voigtType` now acts as stress-like
!
! Returns:
!   ans : `CLASS(VoigtRank2Tensor_), POINTER`
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_Pointer_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_Pointer_test_2.F90)
!
! See also: [[VoigtRank2Tensor]]

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
! Sets the 3x3 matrix `T` from a Voigt tensor `obj`. The matrix is
! symmetric.
!
! The diagonal entries are `obj%V(1:3)`. The off-diagonal entries are
! the shear entries `obj%V(4:6)` multiplied by `obj%scale`.
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{scale}\, V_4$$
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Assignment_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_test_1.F90)
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
! Copies the Voigt vector `obj%V` into `vec`. The routine applies no
! scaling.
!
! For a strain-like tensor, the shear entries are engineering strains.
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Assignment_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/VoigtRank2Tensor_test_1.F90)
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
! Writes the tensor to the unit `unitNo`. The output has four parts.
!
! 1. The message `msg`.
! 2. The Voigt type, as `voigtType: stress-like`,
!    `voigtType: strain-like` or
!    `voigtType: unknown (treated as stress-like)`.
! 3. The scale of the shear entries, as `scale: `.
! 4. The Voigt vector `obj%V`, as `V: `. It is a row by default; use
!    `orient="col"` for a column.
!
! If `unitNo` is absent, the routine writes to the standard output.
!
! Changes:
!   - 2026-11-17: rewritten after IntVector_IOMethod; prints each
!     component, added orient and the array version
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Display_test_1.F90)
!
! See also: [[obj_Display2]]

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
! Writes the message `msg` and the size of the array, as `size: `.
! Then it writes each tensor like [[obj_Display1]], with the message
! `(j)` for entry `j`.
!
! If `unitNo` is absent, the routine writes to the standard output.
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Display_test_1.F90)
!
! See also: [[obj_Display1]]

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
! Returns the size of [[GetVoigtVector]] of `obj` for `voigtForm`.
! The tensor always stores the 6 components of the 3D form. So the
! result is `VoigtSize(voigtForm)` of VoigtUtility.
!
! Returns:
!   ans : INTEGER(I4B) - length of the vector of the form
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/GetVoigtSize_test_1.F90)
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
! Returns the Voigt vector of `obj` for the given `voigtForm`.
! `GetVoigtIndex_` of VoigtUtility gives the orderings, for example:
!
! - 3D: `[11,22,33,12,23,13]`
! - plane strain and plane stress: `[11,22,12]`
! - axisymmetric and plane strain full: `[11,22,12,33]`
!
! In plane strain and plane stress, the out-of-plane normal component
! is not returned. An example is $\sigma_{33}$ in plane strain.
! The Full forms keep it. An unknown `voigtForm` gives the 3D vector.
!
! Returns:
!   ans : `REAL(DFP), ALLOCATABLE` - `SIZE(ans) = VoigtSize(voigtForm)`
!
! Formula:
!   With `V` the 6-entry Voigt vector of `obj`:
!   $$\text{plane strain}: [V_1, V_2, V_4],\quad
!   \text{axisymmetric}: [V_1, V_2, V_4, V_3]$$
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/GetVoigtVector_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/GetVoigtVector_test_2.F90)
!
! See also: [[GetVoigtVector_]], [[GetVoigtSize]], [[VoigtSize]]

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
! Writes the reduced Voigt vector of `obj` into `ans`.
! It gives the same values as [[GetVoigtVector]].
! The caller owns `ans`, so the routine allocates nothing. This suits
! quadrature point loops of finite element kernels.
!
! The routine writes `ans(1:tsize)` and leaves the rest of `ans`
! unchanged. An unknown `voigtForm` acts as `VoigtForm3D`.
!
! @note
! `SIZE(ans)` must be at least `VoigtSize(voigtForm)`. The routine does
! not check this. With a shorter array, it writes out of bounds.
! @endnote
!
! Formula:
!   With `V` the 6-entry Voigt vector of `obj`, the entries written are:
!   $$\text{plane strain}: [V_1, V_2, V_4],\quad
!   \text{axisymmetric}: [V_1, V_2, V_4, V_3]$$
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/GetVoigtVector__test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/GetVoigtVector__test_2.F90)
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
! Returns the trace of `obj`, from [[VoigtTrace]] of `obj%V`.
! Stress-like and strain-like tensors give the same value.
!
! Returns:
!   ans : REAL(DFP) - trace
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Trace_test_1.F90)
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
! Returns the deviatoric part of `obj`, from [[VoigtDev]] of `obj%V`.
! It removes the mean of the normal components.
!
! The result keeps the shear entries, the Voigt type and the scale. So
! the deviator of an engineering strain is again an engineering strain.
!
! Returns:
!   ans : TYPE(VoigtRank2Tensor_) - deviatoric part
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Dev_test_1.F90)
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
! Returns the Frobenius norm of `obj`. The routine converts the shear
! entries to tensor components with the scale of the tensor (1 for
! stress, 1/2 for an engineering strain). Then it uses
! [[VoigtStressNorm]].
!
! So a stress and the same tensor written as an engineering strain have
! the same norm.
!
! Returns:
!   ans : REAL(DFP) - norm of the tensor
!
! Formula:
!   $$ \| T \| = \sqrt{T_{ij} T_{ij}} $$
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/FrobeniusNorm_test_1.F90)
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
! Returns the eigenvalues `WR` and the unit eigenvectors `QR` of `obj`.
! The routine converts the shear entries to tensor components with the
! scale of the tensor. Then it uses [[VoigtSymEigen]], a PURE cyclic
! Jacobi method with a relative tolerance.
!
! The eigenvalues are in descending order. The argument names follow
! `Eigen` of `Rank2Tensor_`.
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Eigen_test_1.F90)
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
! Returns the dyadic product of `obj1` and `obj2` as a 6x6 matrix.
! The routine converts the shear entries of both tensors to tensor
! components with their scale. Then it uses [[VoigtOuter]].
!
! The result `C(6, 6)` is the rank 4 tensor A otimes B in the
! convention of `MaterialJacobian_`. It maps an engineering strain to a
! stress: `C eps = A (B : eps)`.
!
! Returns:
!   ans : REAL(DFP), SHAPE(6, 6) - A otimes B in Voigt form
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Otimes_test_1.F90)
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
