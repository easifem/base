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
! date: 2026-10-21
! summary: Methods for [[MaterialJacobian_]] (material tangent)
!
! This module holds the methods of [[MaterialJacobian_]]. A material
! Jacobian is the tangent $\mathbb{C}$ of a constitutive model. It
! relates stress and strain changes by $d\sigma = \mathbb{C} : d\varepsilon$.
!
! The tangent has minor symmetry. The type stores it as the 3D Voigt
! matrix `C(6, 6)`. The Voigt order is `[11, 22, 33, 12, 23, 13]`.
!
! The matrix maps the engineering strain vector to the stress vector.
! The shear part of that vector is $\gamma = 2\varepsilon$. Hence
! `C(I, J)` equals $\mathbb{C}_{ijkl}$, and the element stiffness is
! $K = \int B^{T} C B$.
!
! [[GetTangent]] and [[ReduceVoigtTangent]] return 2D matrices on
! request. Pass one of the `VoigtForm*` constants to choose the form.
! Plane strain and plane stress use `[11, 22, 12]`. The axisymmetric
! form uses `[11, 22, 12, 33]`. Plane stress uses static condensation.
!
! The submodules are
!
! - `@ConstructorMethods`: Initiate, MaterialJacobian, assignment
! - `@GetMethods`: measure tags, Cijkl, GetRank4, GetTangent
! - `@SetMethods`: SetMeasureTypes
! - `@VoigtFormMethods`: ReduceVoigtTangent
! - `@ContractionMethods`: Contraction ($\mathbb{C} : \varepsilon$)
! - `@Rank4Methods`: IsotropicTangent and rank-4 identities
! - `@IOMethods`: Display

MODULE MaterialJacobian_Method
USE BaseType, ONLY: MaterialJacobian_
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate
PUBLIC :: MaterialJacobian
PUBLIC :: MaterialJacobian_Pointer
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: GetStressType
PUBLIC :: GetStrainType
PUBLIC :: GetRateType
PUBLIC :: SetMeasureTypes
PUBLIC :: Cijkl
PUBLIC :: GetRank4
PUBLIC :: GetTangent
PUBLIC :: ReduceVoigtTangent
PUBLIC :: ReduceVoigtTangent_
PUBLIC :: Contraction
PUBLIC :: IsotropicTangent
PUBLIC :: Rank4SymIdentity
PUBLIC :: Rank4IdentityOtimesIdentity
PUBLIC :: Rank4DeviatoricProjector
PUBLIC :: Display

!----------------------------------------------------------------------------
!                                           Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate [[MaterialJacobian_]] from a 6x6 Voigt matrix
!
! Copies the Voigt matrix `C` into `obj` without change.
!
! Each absent tag is reset to its default. This holds even if `obj`
! had other tags before the call. The defaults are `StressMeasureCauchy`,
! `StrainMeasureSmall` and `StressRateNone`.
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Initiate_test_1.F90) absent tags take the default
! - [test 2](|base|MaterialJacobian/examples/Initiate_test_2.F90) a second `Initiate` resets absent tags to the defaults
!
! See also: [[MaterialJacobian]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE MaterialJacobian_Initiate1(obj, C, stressType, &
                                                    strainType, rateType)
    CLASS(MaterialJacobian_), INTENT(INOUT) :: obj
    !! tangent to initiate
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, order [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, one of the `StressMeasure*` constants
    !! default `StressMeasureCauchy`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, one of the `StrainMeasure*` constants
    !! default `StrainMeasureSmall`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, one of the `StressRate*` constants
    !! default `StressRateNone`
  END SUBROUTINE MaterialJacobian_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                           Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a material tangent from a rank-4 array
!
! Stores the minor-symmetric part of `C4` in `obj`. For example, you can
! pass the result of `Otimes(A, B)` directly.
!
! Each absent tag is reset to its default. This holds even if `obj`
! had other tags before the call. The defaults are `StressMeasureCauchy`,
! `StrainMeasureSmall` and `StressRateNone`.
!
! Formula:
!   $$C_{IJ} = \frac{1}{4}\left(\mathbb{C}_{ijkl} + \mathbb{C}_{jikl}
!   + \mathbb{C}_{ijlk} + \mathbb{C}_{jilk}\right)$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Initiate_test_1.F90) keeps only the minor-symmetric part
! - [test 2](|base|MaterialJacobian/examples/Initiate_test_2.F90) a minor-symmetric rank-4 array round trip
!
! See also: [[MaterialJacobian]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE MaterialJacobian_Initiate2(obj, C4, stressType, &
                                                    strainType, rateType)
    CLASS(MaterialJacobian_), INTENT(INOUT) :: obj
    !! tangent to initiate
    REAL(DFP), INTENT(IN) :: C4(3, 3, 3, 3)
    !! rank-4 tensor, need not have minor symmetry
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, one of the `StressMeasure*` constants
    !! default `StressMeasureCauchy`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, one of the `StrainMeasure*` constants
    !! default `StrainMeasureSmall`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, one of the `StressRate*` constants
    !! default `StressRateNone`
  END SUBROUTINE MaterialJacobian_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                   MaterialJacobian@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a material tangent from a 6x6 Voigt matrix
!
! Function form of [[Initiate]] with a Voigt matrix.
!
! Returns:
!   ans : `MaterialJacobian_`
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/MaterialJacobian_test_1.F90) default tags, and explicit tags
!
! See also: [[Initiate]]

INTERFACE MaterialJacobian
  MODULE PURE FUNCTION MaterialJacobian_Constructor1(C, stressType, &
                                                     strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, order [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, one of the `StressMeasure*` constants
    !! default `StressMeasureCauchy`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, one of the `StrainMeasure*` constants
    !! default `StrainMeasureSmall`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, one of the `StressRate*` constants
    !! default `StressRateNone`
    TYPE(MaterialJacobian_) :: ans
    !! material tangent
  END FUNCTION MaterialJacobian_Constructor1
END INTERFACE MaterialJacobian

!----------------------------------------------------------------------------
!                                   MaterialJacobian@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a material tangent from a rank-4 array
!
! Function form of [[Initiate]] with a rank-4 array. It stores the
! minor-symmetric part of `C4`.
!
! Returns:
!   ans : `MaterialJacobian_`
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/MaterialJacobian_test_1.F90) builds the symmetric identity from Kronecker deltas
!
! See also: [[Initiate]]

INTERFACE MaterialJacobian
  MODULE PURE FUNCTION MaterialJacobian_Constructor2(C4, stressType, &
                                                     strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C4(3, 3, 3, 3)
    !! rank-4 tensor, need not have minor symmetry
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, one of the `StressMeasure*` constants
    !! default `StressMeasureCauchy`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, one of the `StrainMeasure*` constants
    !! default `StrainMeasureSmall`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, one of the `StressRate*` constants
    !! default `StressRateNone`
    TYPE(MaterialJacobian_) :: ans
    !! material tangent
  END FUNCTION MaterialJacobian_Constructor2
END INTERFACE MaterialJacobian

!----------------------------------------------------------------------------
!                           MaterialJacobian_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new material tangent
!
! Pointer form of [[MaterialJacobian]] with a Voigt matrix. The caller
! must deallocate the pointer.
!
! Returns:
!   ans : `CLASS(MaterialJacobian_), POINTER`
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/MaterialJacobian_Pointer_test_1.F90) pointer from a 6x6 Voigt matrix with a rate tag
!
! See also: [[MaterialJacobian]]

INTERFACE MaterialJacobian_Pointer
  MODULE PURE FUNCTION MaterialJacobian_Pointer1(C, stressType, &
                                                 strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, order [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, one of the `StressMeasure*` constants
    !! default `StressMeasureCauchy`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, one of the `StrainMeasure*` constants
    !! default `StrainMeasureSmall`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, one of the `StressRate*` constants
    !! default `StressRateNone`
    CLASS(MaterialJacobian_), POINTER :: ans
    !! new material tangent, the caller must deallocate it
  END FUNCTION MaterialJacobian_Pointer1
END INTERFACE MaterialJacobian_Pointer

!----------------------------------------------------------------------------
!                           MaterialJacobian_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new material tangent from rank-4
!
! Pointer form of [[MaterialJacobian]] with a rank-4 array. The caller
! must deallocate the pointer.
!
! Returns:
!   ans : `CLASS(MaterialJacobian_), POINTER`
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/MaterialJacobian_Pointer_test_1.F90) pointer from a rank-4 array, built with `GetRank4`
!
! See also: [[MaterialJacobian]]

INTERFACE MaterialJacobian_Pointer
  MODULE PURE FUNCTION MaterialJacobian_Pointer2(C4, stressType, &
                                                 strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C4(3, 3, 3, 3)
    !! rank-4 tensor, need not have minor symmetry
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, one of the `StressMeasure*` constants
    !! default `StressMeasureCauchy`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, one of the `StrainMeasure*` constants
    !! default `StrainMeasureSmall`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, one of the `StressRate*` constants
    !! default `StressRateNone`
    CLASS(MaterialJacobian_), POINTER :: ans
    !! new material tangent, the caller must deallocate it
  END FUNCTION MaterialJacobian_Pointer2
END INTERFACE MaterialJacobian_Pointer

!----------------------------------------------------------------------------
!                                         Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Matrix(6,6) = [[MaterialJacobian_]]
!
! Copies the 3D Voigt tangent of `obj` into the matrix `C`.
! The tags of `obj` are not copied.
!
! The assignment `obj = obj2` between two tangents is the intrinsic
! assignment. It copies the tags too.
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/MaterialJacobian_test_1.F90) copies the tangent into a 6x6 matrix

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE MaterialJacobian_MatEqObj(C, obj)
    REAL(DFP), INTENT(INOUT) :: C(6, 6)
    !! 3D Voigt tangent
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
  END SUBROUTINE MaterialJacobian_MatEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                    GetStressType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Stress measure of a [[MaterialJacobian_]]
!
! Returns the `stressType` tag. The tangent is not converted.
!
! Returns:
!   ans : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/SetMeasureTypes_test_1.F90) reads the default tags, then the tags set by `SetMeasureTypes`
!
! See also: [[SetMeasureTypes]]

INTERFACE GetStressType
  MODULE PURE FUNCTION MaterialJacobian_GetStressType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B) :: ans
    !! one of the `StressMeasure*` constants
  END FUNCTION MaterialJacobian_GetStressType
END INTERFACE GetStressType

!----------------------------------------------------------------------------
!                                                    GetStrainType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Strain measure of a [[MaterialJacobian_]]
!
! Returns the `strainType` tag. The tangent is not converted.
!
! Returns:
!   ans : INTEGER(I4B) - one of the `StrainMeasure*` constants
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/SetMeasureTypes_test_1.F90) reads the default tags, then the tags set by `SetMeasureTypes`
!
! See also: [[SetMeasureTypes]]

INTERFACE GetStrainType
  MODULE PURE FUNCTION MaterialJacobian_GetStrainType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B) :: ans
    !! one of the `StrainMeasure*` constants
  END FUNCTION MaterialJacobian_GetStrainType
END INTERFACE GetStrainType

!----------------------------------------------------------------------------
!                                                      GetRateType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Objective stress rate of a [[MaterialJacobian_]]
!
! Returns the `rateType` tag. The tangent is not converted.
!
! Returns:
!   ans : INTEGER(I4B) - one of the `StressRate*` constants
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/SetMeasureTypes_test_1.F90) reads the default tags, then the tags set by `SetMeasureTypes`
!
! See also: [[SetMeasureTypes]]

INTERFACE GetRateType
  MODULE PURE FUNCTION MaterialJacobian_GetRateType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B) :: ans
    !! one of the `StressRate*` constants
  END FUNCTION MaterialJacobian_GetRateType
END INTERFACE GetRateType

!----------------------------------------------------------------------------
!                                                  SetMeasureTypes@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Set the measure tags of a material tangent
!
! Sets the stress, strain and rate tags of `obj`.
! Only the arguments that are present change.
! The tangent values stay as they are.
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/SetMeasureTypes_test_1.F90) only the present tags change
!
! See also: [[GetStressType]], [[GetStrainType]], [[GetRateType]]

INTERFACE SetMeasureTypes
  MODULE PURE SUBROUTINE MaterialJacobian_SetMeasureTypes(obj, &
                                          stressType, strainType, rateType)
    CLASS(MaterialJacobian_), INTENT(INOUT) :: obj
    !! material tangent
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! new stress measure, one of the `StressMeasure*` constants
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! new strain measure, one of the `StrainMeasure*` constants
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! new objective stress rate, one of the `StressRate*` constants
  END SUBROUTINE MaterialJacobian_SetMeasureTypes
END INTERFACE SetMeasureTypes

!----------------------------------------------------------------------------
!                                                       Cijkl@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Component of the rank-4 tangent
!
! Returns the component $\mathbb{C}_{ijkl}$ of the rank-4 tangent.
! It takes the value from the 6x6 Voigt tangent of `obj`.
! Here V(i, j) is the Voigt index of the pair (i, j).
!
! Returns:
!   ans : REAL(DFP) - the component $\mathbb{C}_{ijkl}$
!
! Formula:
!   $$\mathbb{C}_{ijkl} = C_{IJ},\quad I = V(i, j),\ J = V(k, l)$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Cijkl_test_1.F90) isotropic tangent with `lambda = 2` and `mu = 3`

INTERFACE Cijkl
  MODULE PURE FUNCTION MaterialJacobian_Cijkl(obj, i, j, k, l) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B), INTENT(IN) :: i
    !! first tensor index, 1 to 3
    INTEGER(I4B), INTENT(IN) :: j
    !! second tensor index, 1 to 3
    INTEGER(I4B), INTENT(IN) :: k
    !! third tensor index, 1 to 3
    INTEGER(I4B), INTENT(IN) :: l
    !! fourth tensor index, 1 to 3
    REAL(DFP) :: ans
    !! component `C_ijkl`
  END FUNCTION MaterialJacobian_Cijkl
END INTERFACE Cijkl

!----------------------------------------------------------------------------
!                                                    GetRank4@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: The tangent as a rank-4 array
!
! Returns the tangent of `obj` as a rank-4 array of shape (3, 3, 3, 3).
! The result has minor symmetry.
! Passing it to [[MaterialJacobian]] gives back the same tangent.
!
! Returns:
!   ans : REAL(DFP), SHAPE(3,3,3,3) - minor-symmetric rank-4 tensor
!
! Formula:
!   $$\mathrm{ans}_{ijkl} = C_{V(i,j), V(k,l)}$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/GetRank4_test_1.F90) entries of the array and the round trip

INTERFACE GetRank4
  MODULE PURE FUNCTION MaterialJacobian_GetRank4(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    REAL(DFP) :: ans(3, 3, 3, 3)
    !! minor-symmetric rank-4 tensor
  END FUNCTION MaterialJacobian_GetRank4
END INTERFACE GetRank4

!----------------------------------------------------------------------------
!                                                  GetTangent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: The Voigt tangent in a given Voigt form
!
! Returns the tangent of `obj` in the Voigt form `voigtForm`.
! The default form is `VoigtForm3D`, which gives the full 6x6 tangent.
!
! Any value of `voigtForm` that is not a `VoigtForm*` constant gives the
! full 6x6 tangent too, without an error.
!
! For `VoigtFormPlaneStress`, the routine condenses out the out-of-plane
! components `[33, 23, 13]`. Their 3x3 block of the tangent must be
! invertible.
!
! Returns:
!   ans : `REAL(DFP), ALLOCATABLE`, shape `(n, n)`, `n = VoigtSize(voigtForm)`
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/GetTangent_test_1.F90) default 3D form, plane strain, plane stress and axisymmetric
!
! See also: [[ReduceVoigtTangent]]

INTERFACE GetTangent
  MODULE PURE FUNCTION MaterialJacobian_GetTangent(obj, voigtForm) &
    RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: voigtForm
    !! one of the `VoigtForm*` constants, default `VoigtForm3D`,
    !! other values give the 6x6 tangent
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Voigt tangent, SHAPE (n, n) with `n = VoigtSize(voigtForm)`
  END FUNCTION MaterialJacobian_GetTangent
END INTERFACE GetTangent

!----------------------------------------------------------------------------
!                                    ReduceVoigtTangent@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Reduce a 3D Voigt tangent to a 2D Voigt form
!
! Reduces the 3D Voigt tangent `C` to the Voigt form `voigtForm`.
! The result depends on the form.
!
! - `VoigtFormPlaneStrain`: rows and columns `[1, 2, 4]` of `C`.
! - `VoigtFormAxisymmetric`: rows and columns `[1, 2, 4, 3]` of `C`.
! - `VoigtFormPlaneStress`: static condensation of the out-of-plane
!   components `[33, 23, 13]`, which have zero stress. The in-plane set
!   is $i = [1, 2, 4]$ and the out-of-plane set is $o = [3, 5, 6]$.
! - `VoigtForm3D`: the full 6x6 matrix.
!
! Any value of `voigtForm` that is not a `VoigtForm*` constant gives the
! full 6x6 matrix too, without an error.
!
! @note
! For `VoigtFormPlaneStress`, the 3x3 out-of-plane block of `C` must be
! invertible.
! @endnote
!
! Returns:
!   ans : `REAL(DFP), ALLOCATABLE`, shape `(n, n)`, `n = VoigtSize(voigtForm)`
!
! Formula:
!   $$C^{ps} = C_{ii} - C_{io} C_{oo}^{-1} C_{oi}$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/ReduceVoigtTangent_test_1.F90) anisotropic tangent, plane strain and plane stress
! - [test 2](|base|MaterialJacobian/examples/ReduceVoigtTangent_test_2.F90) isotropic closed form for plane stress, axisymmetric order, and `VoigtForm3D`
!
! See also: [[ReduceVoigtTangent_]], [[GetTangent]]

INTERFACE ReduceVoigtTangent
  MODULE PURE FUNCTION MaterialJacobian_ReduceVoigtTangent(C, voigtForm) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the `VoigtForm*` constants, other values give the 6x6
    !! tangent
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! reduced tangent, SHAPE (n, n) with `n = VoigtSize(voigtForm)`
  END FUNCTION MaterialJacobian_ReduceVoigtTangent
END INTERFACE ReduceVoigtTangent

!----------------------------------------------------------------------------
!                                     ReduceVoigtTangent_@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-02
! summary: Reduce a 3D Voigt tangent to a 2D Voigt form (no allocation)
!
! Reduces the 3D Voigt tangent `C` to the Voigt form `voigtForm`.
! It gives the same values as [[ReduceVoigtTangent]].
! The result goes into the array `ans`, which the caller provides.
!
! The routine writes only `ans(1:nrow, 1:ncol)`. It also returns `nrow`
! and `ncol`. The matrix `C` need not be symmetric.
!
! For `VoigtForm3D`, the result is the full 6x6 tangent. Any other value
! of `voigtForm` gives the same result, without an error.
!
! For `VoigtFormPlaneStress`, the routine condenses out the out-of-plane
! components `[33, 23, 13]`. The 3x3 block of `C` for these components
! must be invertible.
!
! @warning
! The routine does not check the size of `ans`. It must hold at least
! `nrow` rows and `ncol` columns.
! @endwarning
!
! Formula:
!   $$C^{ps} = C_{ii} - C_{io} C_{oo}^{-1} C_{oi}$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/ReduceVoigtTangent__test_1.F90) `nrow`, `ncol` and values in each Voigt form, compared with `ReduceVoigtTangent`
!
! See also: [[ReduceVoigtTangent]]

INTERFACE ReduceVoigtTangent_
  MODULE PURE SUBROUTINE MaterialJacobian_ReduceVoigtTangent_(C, voigtForm, &
                                                              ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the `VoigtForm*` constants, other values give the 6x6 tangent
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! reduced tangent, size at least (n, n) with n the Voigt size of
    !! `voigtForm`, the size is not checked
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in `ans`
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in `ans`
  END SUBROUTINE MaterialJacobian_ReduceVoigtTangent_
END INTERFACE ReduceVoigtTangent_

!----------------------------------------------------------------------------
!                                          Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Stress from tangent and strain tensor
!
! Returns the stress `ans` for the strain `eps` and the tangent `obj`.
! The stress is symmetric.
! If `eps` is not symmetric, the product uses only its symmetric part.
!
! Returns:
!   ans : `Rank2Tensor_` - symmetric stress (increment)
!
! Formula:
!   $$\sigma_{ij} = \mathbb{C}_{ijkl}\, \varepsilon_{kl}$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Contraction_test_1.F90) isotropic tangent
! - [test 2](|base|MaterialJacobian/examples/Contraction_test_2.F90) anisotropic tangent against a rank-4 sum, and a non-symmetric strain

INTERFACE Contraction
  MODULE PURE FUNCTION MaterialJacobian_Contraction1(obj, eps) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    CLASS(Rank2Tensor_), INTENT(IN) :: eps
    !! strain (increment), need not be symmetric
    TYPE(Rank2Tensor_) :: ans
    !! symmetric stress (increment)
  END FUNCTION MaterialJacobian_Contraction1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                          Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Stress from tangent and Voigt strain
!
! Returns the stress `ans` for the Voigt strain `eps` and the tangent
! `obj`. The result is a stress-like Voigt tensor.
!
! The input `eps` may be strain-like or stress-like. Its shear
! components become engineering shear strains before the product.
!
! Returns:
!   ans : `VoigtRank2Tensor_` - stress (increment), `StressTypeVoigt`
!
! Formula:
!   $$\sigma_{I} = C_{IJ}\, \varepsilon^{eng}_{J}$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Contraction_test_1.F90) isotropic tangent
! - [test 2](|base|MaterialJacobian/examples/Contraction_test_2.F90) anisotropic tangent against a rank-4 sum

INTERFACE Contraction
  MODULE PURE FUNCTION MaterialJacobian_Contraction2(obj, eps) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: eps
    !! strain (increment), stress-like or strain-like
    TYPE(VoigtRank2Tensor_) :: ans
    !! stress (increment), `StressTypeVoigt`
  END FUNCTION MaterialJacobian_Contraction2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            IsotropicTangent@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Isotropic elastic tangent in Voigt form
!
! Returns the isotropic elastic tangent as a 6x6 Voigt matrix.
! The inputs are the two Lame parameters `lambda` and `mu`.
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6)
!
! Formula:
!   $$\mathbb{C} = \lambda\, I \otimes I + 2\mu\, \mathbb{I}^{sym}$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/IsotropicTangent_test_1.F90) entries for `lambda = 2` and `mu = 1`
!
! See also: [[Rank4SymIdentity]], [[Rank4IdentityOtimesIdentity]]

INTERFACE IsotropicTangent
  MODULE PURE FUNCTION MaterialJacobian_IsotropicTangent(lambda, mu) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda
    !! first Lame parameter
    REAL(DFP), INTENT(IN) :: mu
    !! shear modulus
    REAL(DFP) :: ans(6, 6)
    !! 3D Voigt tangent
  END FUNCTION MaterialJacobian_IsotropicTangent
END INTERFACE IsotropicTangent

!----------------------------------------------------------------------------
!                                            Rank4SymIdentity@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Symmetric rank-4 identity in Voigt form
!
! Returns the symmetric rank-4 identity as a 6x6 Voigt matrix.
! The matrix is diagonal. The first three entries are 1.
! The last three entries are 1/2.
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6) - diag(1, 1, 1, 1/2, 1/2, 1/2)
!
! Formula:
!   $$\mathbb{I}^{sym}_{ijkl} = \frac{1}{2}\left(\delta_{ik}\delta_{jl}
!   + \delta_{il}\delta_{jk}\right)$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Rank4SymIdentity_test_1.F90) diagonal entries, and the identity acts on a symmetric strain
!
! See also: [[IsotropicTangent]]

INTERFACE Rank4SymIdentity
  MODULE PURE FUNCTION MaterialJacobian_Rank4SymIdentity() RESULT(ans)
    REAL(DFP) :: ans(6, 6)
    !! 3D Voigt matrix
  END FUNCTION MaterialJacobian_Rank4SymIdentity
END INTERFACE Rank4SymIdentity

!----------------------------------------------------------------------------
!                                 Rank4IdentityOtimesIdentity@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: $I \otimes I$ in Voigt form
!
! Returns the tensor product of the identity with itself as a 6x6
! Voigt matrix.
! The first three rows and columns (the normal block) hold ones.
! All other entries are zero.
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6) - ones in the normal 3x3 block
!
! Formula:
!   $$(I \otimes I)_{ijkl} = \delta_{ij}\delta_{kl}$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Rank4IdentityOtimesIdentity_test_1.F90) ones in the normal block, zero elsewhere, and equal to `Otimes` of two identity tensors
!
! See also: [[Rank4SymIdentity]], [[Rank4DeviatoricProjector]]

INTERFACE Rank4IdentityOtimesIdentity
  MODULE PURE FUNCTION MaterialJacobian_Rank4IdentityOtimesIdentity() &
    RESULT(ans)
    REAL(DFP) :: ans(6, 6)
    !! 3D Voigt matrix
  END FUNCTION MaterialJacobian_Rank4IdentityOtimesIdentity
END INTERFACE Rank4IdentityOtimesIdentity

!----------------------------------------------------------------------------
!                                    Rank4DeviatoricProjector@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Deviatoric projector in Voigt form
!
! Returns the deviatoric projector as a 6x6 Voigt matrix.
! It is the symmetric rank-4 identity minus one third of
! `Rank4IdentityOtimesIdentity`.
! Applied to a strain, it returns the deviatoric part of that strain.
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6)
!
! Formula:
!   $$\mathbb{P}^{dev} = \mathbb{I}^{sym} - \frac{1}{3} I \otimes I$$
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Rank4DeviatoricProjector_test_1.F90) equals the symmetric identity minus `Rank4IdentityOtimesIdentity` / 3, and maps a strain to its deviator
!
! See also: [[Rank4SymIdentity]], [[Rank4IdentityOtimesIdentity]]

INTERFACE Rank4DeviatoricProjector
  MODULE PURE FUNCTION MaterialJacobian_Rank4DeviatoricProjector() &
    RESULT(ans)
    REAL(DFP) :: ans(6, 6)
    !! 3D Voigt matrix
  END FUNCTION MaterialJacobian_Rank4DeviatoricProjector
END INTERFACE Rank4DeviatoricProjector

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Display the content of [[MaterialJacobian_]]
!
! Prints the 6x6 Voigt tangent, headed by `msg`.
! Then it prints three more lines, one for each measure tag:
!
! 1. `stress measure: <name>`
! 2. `strain measure: <name>`
! 3. `stress rate: <name>`
!
! A tag value that is not one of the measure constants prints as
! `unknown`.
!
! Examples
!
! - [test 1](|base|MaterialJacobian/examples/Display_test_1.F90) tag names, and `unknown` for values outside the constants

INTERFACE Display
  MODULE SUBROUTINE MaterialJacobian_Display(obj, msg, unitNo)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    CHARACTER(*), INTENT(IN) :: msg
    !! header message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! output unit, default is stdout
  END SUBROUTINE MaterialJacobian_Display
END INTERFACE Display

END MODULE MaterialJacobian_Method
