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
! Reduced 2D forms (`VoigtForm*` constants in GlobalData) are available
! through [[VoigtSize]], [[GetVoigtVector]], [[GetVoigtVector_]] and the
! 4-argument [[Initiate]] and 3-argument [[VoigtRank2Tensor]]. Their
! orderings are plane strain and plane stress `[11,22,12]` and
! axisymmetric `[11,22,12,33]`.
!
! Changes:
!   - 2026-10-21: renamed the specific procedures. An unknown
!     `voigtType` now acts as stress-like. Added the reduced 2D Voigt
!     forms.
!   - 2026-11-02: added `GetVoigtVector_`.

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
PUBLIC :: VoigtSize
PUBLIC :: GetVoigtVector
PUBLIC :: GetVoigtVector_

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
  MODULE PURE SUBROUTINE VoigtRank2Tensor_Initiate1(obj, vec, voigtType)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    REAL(DFP), INTENT(IN) :: vec(6)
    !! Voigt vector [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
  END SUBROUTINE VoigtRank2Tensor_Initiate1
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
  MODULE PURE SUBROUTINE VoigtRank2Tensor_Initiate2(obj, T, voigtType)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    REAL(DFP), INTENT(IN) :: T(3, 3)
    !! tensor components, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
  END SUBROUTINE VoigtRank2Tensor_Initiate2
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
  MODULE PURE FUNCTION VoigtRank2Tensor_Constructor1(vec, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(6)
    !! Voigt vector [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    TYPE(VoigtRank2Tensor_) :: ans
    !! Voigt tensor
  END FUNCTION VoigtRank2Tensor_Constructor1
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
  MODULE PURE FUNCTION VoigtRank2Tensor_Constructor2(T, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: T(3, 3)
    !! tensor components, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    TYPE(VoigtRank2Tensor_) :: ans
    !! Voigt tensor
  END FUNCTION VoigtRank2Tensor_Constructor2
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
  MODULE PURE FUNCTION VoigtRank2Tensor_Pointer1(vec, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(6)
    !! Voigt vector [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    CLASS(VoigtRank2Tensor_), POINTER :: ans
    !! new Voigt tensor, the caller must deallocate it
  END FUNCTION VoigtRank2Tensor_Pointer1
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
  MODULE PURE FUNCTION VoigtRank2Tensor_Pointer2(T, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: T(3, 3)
    !! tensor components, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    CLASS(VoigtRank2Tensor_), POINTER :: ans
    !! new Voigt tensor, the caller must deallocate it
  END FUNCTION VoigtRank2Tensor_Pointer2
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
! See also: [[VoigtRank2Tensor_VecEqObj]]

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE VoigtRank2Tensor_MatEqObj(T, obj)
    REAL(DFP), INTENT(INOUT) :: T(3, 3)
    !! symmetric tensor components
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
  END SUBROUTINE VoigtRank2Tensor_MatEqObj
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
! See also: [[VoigtRank2Tensor_MatEqObj]]

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE VoigtRank2Tensor_VecEqObj(vec, obj)
    REAL(DFP), INTENT(INOUT) :: vec(6)
    !! Voigt vector [11, 22, 33, 12, 23, 13]
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
  END SUBROUTINE VoigtRank2Tensor_VecEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Display the content of [[VoigtRank2Tensor_]]
!
! Writes the tensor to the unit `unitNo`. The output has three parts.
!
! 1. The line `# msg`.
! 2. A line with the Voigt type. It reads `Stress Like Voigt Type`,
!    `Strain Like Voigt Type` or
!    `Unknown Voigt Type (treated as stress like)`.
! 3. The Voigt vector `obj%V`, written as a row.
!
! If `unitNo` is absent, the routine writes to the standard output.
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Display_test_1.F90)

INTERFACE Display
  MODULE SUBROUTINE VoigtRank2Tensor_Display(obj, msg, unitNo)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! tensor to display
    CHARACTER(*), INTENT(IN) :: msg
    !! header message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! output unit, default is stdout
  END SUBROUTINE VoigtRank2Tensor_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                VoigtSize@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Length of the Voigt vector of a Voigt form
!
! Returns the number of entries in the Voigt vector of `voigtForm`.
!
! The length is 6 for `VoigtForm3D`, 3 for `VoigtFormPlaneStrain` and
! `VoigtFormPlaneStress`, and 4 for `VoigtFormAxisymmetric`. An unknown
! form gives 6.
!
! Returns:
!   ans : INTEGER(I4B) - length of the Voigt vector
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/VoigtSize_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/VoigtSize_test_2.F90)

INTERFACE VoigtSize
  MODULE PURE FUNCTION VoigtRank2Tensor_VoigtSize(voigtForm) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    INTEGER(I4B) :: ans
    !! length of the Voigt vector
  END FUNCTION VoigtRank2Tensor_VoigtSize
END INTERFACE VoigtSize

!----------------------------------------------------------------------------
!                                           GetVoigtVector@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Reduced Voigt vector of a [[VoigtRank2Tensor_]]
!
! Returns the Voigt vector of `obj` for the given `voigtForm`.
! The orderings are:
!
! - 3D: `[11,22,33,12,23,13]`
! - plane strain and plane stress: `[11,22,12]`
! - axisymmetric: `[11,22,12,33]`
!
! In plane strain and plane stress, the out-of-plane normal component
! is not returned. An example is $\sigma_{33}$ in plane strain.
! An unknown `voigtForm` gives the 3D vector.
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
! See also: [[GetVoigtVector_]], [[VoigtSize]]

INTERFACE GetVoigtVector
  MODULE PURE FUNCTION VoigtRank2Tensor_GetVoigtVector(obj, voigtForm) &
    RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! reduced Voigt vector, size `VoigtSize(voigtForm)`
  END FUNCTION VoigtRank2Tensor_GetVoigtVector
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
  MODULE PURE SUBROUTINE VoigtRank2Tensor_GetVoigtVector_(obj, voigtForm, &
                                                          ans, tsize)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    !! Voigt tensor
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! reduced Voigt vector, `SIZE(ans) >= VoigtSize(voigtForm)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in `ans`, `VoigtSize(voigtForm)`
  END SUBROUTINE VoigtRank2Tensor_GetVoigtVector_
END INTERFACE GetVoigtVector_

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
! zero. This includes the out-of-plane normal component in plane strain
! and plane stress. Hence the 3x3 tensor from `T = obj` has `T33 = 0`
! even when the physical value is not zero. Examples are $\sigma_{33}$
! in plane strain and $\varepsilon_{33}$ in plane stress.
!
! An unknown `voigtForm` acts as `VoigtForm3D`. Then the routine
! reads `vec(1:6)`.
!
! @note
! `SIZE(vec)` must be at least `VoigtSize(voigtForm)`. The routine does
! not check this. With a shorter vector, it reads out of bounds.
! @endnote
!
! Formula:
!   With `v_k = vec(k)`, the Voigt vector `V` of `obj` is, for plane
!   strain and plane stress:
!   $$V = [v_1, v_2, 0, v_3, 0, 0]$$
!
! Examples
!
! - [test 1](|base|VoigtRank2Tensor/examples/Initiate_test_1.F90)
! - [test 2](|base|VoigtRank2Tensor/examples/Initiate_test_2.F90)
!
! See also: [[VoigtRank2Tensor]], [[VoigtSize]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE VoigtRank2Tensor_Initiate3(obj, vec, voigtType, &
                                                    voigtForm)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    REAL(DFP), INTENT(IN) :: vec(:)
    !! reduced Voigt vector, `SIZE(vec) >= VoigtSize(voigtForm)`
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
  END SUBROUTINE VoigtRank2Tensor_Initiate3
END INTERFACE Initiate

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
  MODULE PURE FUNCTION VoigtRank2Tensor_Constructor3(vec, voigtType, &
                                                     voigtForm) RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(:)
    !! reduced Voigt vector, `SIZE(vec) >= VoigtSize(voigtForm)`
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    TYPE(VoigtRank2Tensor_) :: ans
    !! Voigt tensor
  END FUNCTION VoigtRank2Tensor_Constructor3
END INTERFACE VoigtRank2Tensor

END MODULE VoigtRank2Tensor_Method
