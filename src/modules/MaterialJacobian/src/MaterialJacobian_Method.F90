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
! Description:
!   A material Jacobian is the tangent $\mathbb{C}$ of a constitutive
!   model, $d\sigma = \mathbb{C} : d\varepsilon$. It has minor symmetry,
!   so it is stored as the 3D Voigt matrix `C(6, 6)` with the order
!   `[11, 22, 33, 12, 23, 13]`. The matrix maps the engineering strain
!   vector (shear $\gamma = 2\varepsilon$) to the stress vector, so
!   `C(I, J)` equals $\mathbb{C}_{ijkl}$ and the element stiffness is
!   $K = \int B^{T} C B$.
!
!   2D matrices are produced on request by [[GetTangent]] and
!   [[ReduceVoigtTangent]] with one of the `VoigtForm*` constants:
!   plane strain and plane stress `[11, 22, 12]`, axisymmetric
!   `[11, 22, 12, 33]`. Plane stress uses static condensation.
!
!   Submodules:
!
!   - `@ConstructorMethods`: Initiate, MaterialJacobian, measure tags
!   - `@GetMethods`: Cijkl, GetRank4, GetTangent
!   - `@VoigtFormMethods`: ReduceVoigtTangent
!   - `@ContractionMethods`: Contraction ($\mathbb{C} : \varepsilon$)
!   - `@Rank4Methods`: IsotropicTangent and rank-4 identities
!   - `@IOMethods`: Display

MODULE MaterialJacobian_Method
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: MaterialJacobian_
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
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
! Arguments:
!   obj : MaterialJacobian_ - tangent to initiate
!   C : REAL(DFP), SHAPE(6,6) - Voigt tangent
!   stressType : INTEGER(I4B), OPTIONAL - default `StressMeasureCauchy`
!   strainType : INTEGER(I4B), OPTIONAL - default `StrainMeasureSmall`
!   rateType : INTEGER(I4B), OPTIONAL - default `StressRateNone`
!
! Formula:
!   $$\mathrm{obj\%C} = C$$

INTERFACE Initiate
  MODULE PURE SUBROUTINE MaterialJacobian_Initiate1(obj, C, stressType, &
                                                    strainType, rateType)
    CLASS(MaterialJacobian_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: C(6, 6)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
  END SUBROUTINE MaterialJacobian_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                           Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate [[MaterialJacobian_]] from a rank-4 array
!
! Description:
!   The minor-symmetric part of `C4` is stored, so for example the result
!   of `Otimes(A, B)` can be used directly.
!
! Arguments:
!   obj : MaterialJacobian_ - tangent to initiate
!   C4 : REAL(DFP), SHAPE(3,3,3,3) - rank-4 tensor
!   stressType : INTEGER(I4B), OPTIONAL - default `StressMeasureCauchy`
!   strainType : INTEGER(I4B), OPTIONAL - default `StrainMeasureSmall`
!   rateType : INTEGER(I4B), OPTIONAL - default `StressRateNone`
!
! Formula:
!   $$C_{IJ} = \frac{1}{4}\left(\mathbb{C}_{ijkl} + \mathbb{C}_{jikl}
!   + \mathbb{C}_{ijlk} + \mathbb{C}_{jilk}\right)$$

INTERFACE Initiate
  MODULE PURE SUBROUTINE MaterialJacobian_Initiate2(obj, C4, stressType, &
                                                    strainType, rateType)
    CLASS(MaterialJacobian_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: C4(3, 3, 3, 3)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
  END SUBROUTINE MaterialJacobian_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                   MaterialJacobian@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[MaterialJacobian_]] from a 6x6 Voigt matrix
!
! Arguments:
!   C : REAL(DFP), SHAPE(6,6) - Voigt tangent
!   stressType, strainType, rateType : INTEGER(I4B), OPTIONAL - tags
!
! Returns:
!   ans : MaterialJacobian_
!
! Formula:
!   $$\mathrm{ans\%C} = C$$

INTERFACE MaterialJacobian
  MODULE PURE FUNCTION MaterialJacobian_Constructor1(C, stressType, &
                                                     strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    TYPE(MaterialJacobian_) :: ans
  END FUNCTION MaterialJacobian_Constructor1
END INTERFACE MaterialJacobian

!----------------------------------------------------------------------------
!                                   MaterialJacobian@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[MaterialJacobian_]] from a rank-4 array
!
! Arguments:
!   C4 : REAL(DFP), SHAPE(3,3,3,3) - rank-4 tensor
!   stressType, strainType, rateType : INTEGER(I4B), OPTIONAL - tags
!
! Returns:
!   ans : MaterialJacobian_ - minor-symmetric part of C4
!
! Formula:
!   $$C_{IJ} = \mathrm{minorsym}(\mathbb{C})_{ijkl}$$

INTERFACE MaterialJacobian
  MODULE PURE FUNCTION MaterialJacobian_Constructor2(C4, stressType, &
                                                     strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C4(3, 3, 3, 3)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    TYPE(MaterialJacobian_) :: ans
  END FUNCTION MaterialJacobian_Constructor2
END INTERFACE MaterialJacobian

!----------------------------------------------------------------------------
!                           MaterialJacobian_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[MaterialJacobian_]]
!
! Arguments:
!   C : REAL(DFP), SHAPE(6,6) - Voigt tangent
!   stressType, strainType, rateType : INTEGER(I4B), OPTIONAL - tags
!
! Returns:
!   ans : CLASS(MaterialJacobian_), POINTER - caller must deallocate
!
! Formula:
!   $$\mathrm{ans\%C} = C$$

INTERFACE MaterialJacobian_Pointer
  MODULE PURE FUNCTION MaterialJacobian_Pointer1(C, stressType, &
                                                 strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    CLASS(MaterialJacobian_), POINTER :: ans
  END FUNCTION MaterialJacobian_Pointer1
END INTERFACE MaterialJacobian_Pointer

!----------------------------------------------------------------------------
!                           MaterialJacobian_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[MaterialJacobian_]] from rank-4
!
! Arguments:
!   C4 : REAL(DFP), SHAPE(3,3,3,3) - rank-4 tensor
!   stressType, strainType, rateType : INTEGER(I4B), OPTIONAL - tags
!
! Returns:
!   ans : CLASS(MaterialJacobian_), POINTER - caller must deallocate
!
! Formula:
!   $$C_{IJ} = \mathrm{minorsym}(\mathbb{C})_{ijkl}$$

INTERFACE MaterialJacobian_Pointer
  MODULE PURE FUNCTION MaterialJacobian_Pointer2(C4, stressType, &
                                                 strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C4(3, 3, 3, 3)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    CLASS(MaterialJacobian_), POINTER :: ans
  END FUNCTION MaterialJacobian_Pointer2
END INTERFACE MaterialJacobian_Pointer

!----------------------------------------------------------------------------
!                                         Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Matrix(6,6) = [[MaterialJacobian_]]
!
! Description:
!   `obj = obj2` is the intrinsic assignment and copies the tags too.
!
! Arguments:
!   C : REAL(DFP), SHAPE(6,6) - Voigt tangent (output)
!   obj : MaterialJacobian_ - tangent
!
! Formula:
!   $$C = \mathrm{obj\%C}$$

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE MaterialJacobian_MatEqObj(C, obj)
    REAL(DFP), INTENT(INOUT) :: C(6, 6)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
  END SUBROUTINE MaterialJacobian_MatEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                      GetStressType@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Stress measure of a [[MaterialJacobian_]]
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!
! Returns:
!   ans : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Formula:
!   $$\mathrm{ans} = \mathrm{obj\%stressType}$$

INTERFACE GetStressType
  MODULE PURE FUNCTION MaterialJacobian_GetStressType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION MaterialJacobian_GetStressType
END INTERFACE GetStressType

!----------------------------------------------------------------------------
!                                      GetStrainType@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Strain measure of a [[MaterialJacobian_]]
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!
! Returns:
!   ans : INTEGER(I4B) - one of the `StrainMeasure*` constants
!
! Formula:
!   $$\mathrm{ans} = \mathrm{obj\%strainType}$$

INTERFACE GetStrainType
  MODULE PURE FUNCTION MaterialJacobian_GetStrainType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION MaterialJacobian_GetStrainType
END INTERFACE GetStrainType

!----------------------------------------------------------------------------
!                                        GetRateType@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Objective stress rate of a [[MaterialJacobian_]]
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!
! Returns:
!   ans : INTEGER(I4B) - one of the `StressRate*` constants
!
! Formula:
!   $$\mathrm{ans} = \mathrm{obj\%rateType}$$

INTERFACE GetRateType
  MODULE PURE FUNCTION MaterialJacobian_GetRateType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION MaterialJacobian_GetRateType
END INTERFACE GetRateType

!----------------------------------------------------------------------------
!                                    SetMeasureTypes@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Set the measure tags of a [[MaterialJacobian_]]
!
! Description:
!   Only the present arguments are changed. The tangent is not
!   converted.
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!   stressType, strainType, rateType : INTEGER(I4B), OPTIONAL - tags
!
! Formula:
!   $$\mathrm{obj\%stressType} = \mathrm{stressType}$$

INTERFACE SetMeasureTypes
  MODULE PURE SUBROUTINE MaterialJacobian_SetMeasureTypes(obj, &
                                          stressType, strainType, rateType)
    CLASS(MaterialJacobian_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
  END SUBROUTINE MaterialJacobian_SetMeasureTypes
END INTERFACE SetMeasureTypes

!----------------------------------------------------------------------------
!                                                       Cijkl@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Component of the rank-4 tangent
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!   i, j, k, l : INTEGER(I4B) - tensor indices in 1..3
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\mathbb{C}_{ijkl} = C_{IJ},\quad I = V(i, j),\ J = V(k, l)$$

INTERFACE Cijkl
  MODULE PURE FUNCTION MaterialJacobian_Cijkl(obj, i, j, k, l) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    INTEGER(I4B), INTENT(IN) :: j
    INTEGER(I4B), INTENT(IN) :: k
    INTEGER(I4B), INTENT(IN) :: l
    REAL(DFP) :: ans
  END FUNCTION MaterialJacobian_Cijkl
END INTERFACE Cijkl

!----------------------------------------------------------------------------
!                                                    GetRank4@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: The tangent as a rank-4 array
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!
! Returns:
!   ans : REAL(DFP), SHAPE(3,3,3,3) - minor-symmetric rank-4 tensor
!
! Formula:
!   $$\mathrm{ans}_{ijkl} = C_{V(i,j), V(k,l)}$$

INTERFACE GetRank4
  MODULE PURE FUNCTION MaterialJacobian_GetRank4(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    REAL(DFP) :: ans(3, 3, 3, 3)
  END FUNCTION MaterialJacobian_GetRank4
END INTERFACE GetRank4

!----------------------------------------------------------------------------
!                                                  GetTangent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: The Voigt tangent in a given Voigt form
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!   voigtForm : INTEGER(I4B), OPTIONAL - `VoigtForm*` constant, default
!     `VoigtForm3D`
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE, SHAPE(n, n) - n = VoigtSize(voigtForm)
!
! Formula:
!   $$\mathrm{ans} = \mathrm{ReduceVoigtTangent}(C, \mathrm{voigtForm})$$

INTERFACE GetTangent
  MODULE PURE FUNCTION MaterialJacobian_GetTangent(obj, voigtForm) &
    RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: voigtForm
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MaterialJacobian_GetTangent
END INTERFACE GetTangent

!----------------------------------------------------------------------------
!                                    ReduceVoigtTangent@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Reduce a 3D Voigt tangent to a 2D Voigt form
!
! Description:
!   - `VoigtFormPlaneStrain`: rows and columns `[1, 2, 4]`
!   - `VoigtFormAxisymmetric`: rows and columns `[1, 2, 4, 3]`
!   - `VoigtFormPlaneStress`: static condensation with
!     $\sigma_{33} = \sigma_{23} = \sigma_{13} = 0$, using the in-plane
!     set $i = [1, 2, 4]$ and out-of-plane set $o = [3, 5, 6]$
!   - `VoigtForm3D` or unknown: the 6x6 matrix itself
!
! Arguments:
!   C : REAL(DFP), SHAPE(6,6) - 3D Voigt tangent
!   voigtForm : INTEGER(I4B) - one of the `VoigtForm*` constants
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE, SHAPE(n, n)
!
! Formula:
!   $$C^{ps} = C_{ii} - C_{io} C_{oo}^{-1} C_{oi}$$

INTERFACE ReduceVoigtTangent
  MODULE PURE FUNCTION MaterialJacobian_ReduceVoigtTangent(C, voigtForm) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    INTEGER(I4B), INTENT(IN) :: voigtForm
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION MaterialJacobian_ReduceVoigtTangent
END INTERFACE ReduceVoigtTangent

!----------------------------------------------------------------------------
!                                     ReduceVoigtTangent_@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-02
! summary: Reduce a 3D Voigt tangent to a 2D Voigt form (no allocation)
!
! Description:
!   Same as [[ReduceVoigtTangent]] but the result is written in `ans`,
!   which should be allocated by the caller. The tangent `C` is not
!   assumed to be symmetric.
!
! Arguments:
!   C : REAL(DFP), SHAPE(6,6) - 3D Voigt tangent
!   voigtForm : INTEGER(I4B) - one of the `VoigtForm*` constants
!   ans : REAL(DFP), SHAPE >= (n, n) - reduced tangent
!   nrow, ncol : INTEGER(I4B) - number of rows and columns written in ans
!
! Formula:
!   $$C^{ps} = C_{ii} - C_{io} C_{oo}^{-1} C_{oi}$$

INTERFACE ReduceVoigtTangent_
  MODULE PURE SUBROUTINE MaterialJacobian_ReduceVoigtTangent_(C, voigtForm, &
                                                              ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    INTEGER(I4B), INTENT(IN) :: voigtForm
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE MaterialJacobian_ReduceVoigtTangent_
END INTERFACE ReduceVoigtTangent_

!----------------------------------------------------------------------------
!                                          Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Stress from tangent and strain tensor
!
! Description:
!   The symmetric part of `eps` is used.
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!   eps : Rank2Tensor_ - strain (increment)
!
! Returns:
!   ans : Rank2Tensor_ - symmetric stress (increment)
!
! Formula:
!   $$\sigma_{ij} = \mathbb{C}_{ijkl}\, \varepsilon_{kl}$$

INTERFACE Contraction
  MODULE PURE FUNCTION MaterialJacobian_Contraction1(obj, eps) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    CLASS(Rank2Tensor_), INTENT(IN) :: eps
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION MaterialJacobian_Contraction1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                          Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Stress from tangent and Voigt strain
!
! Description:
!   `eps` may be stress-like or strain-like; it is converted to the
!   engineering strain vector before the product.
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!   eps : VoigtRank2Tensor_ - strain (increment)
!
! Returns:
!   ans : VoigtRank2Tensor_ - stress (increment), `StressTypeVoigt`
!
! Formula:
!   $$\sigma_{I} = C_{IJ}\, \varepsilon^{eng}_{J}$$

INTERFACE Contraction
  MODULE PURE FUNCTION MaterialJacobian_Contraction2(obj, eps) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: eps
    TYPE(VoigtRank2Tensor_) :: ans
  END FUNCTION MaterialJacobian_Contraction2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            IsotropicTangent@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Isotropic elastic tangent in Voigt form
!
! Arguments:
!   lambda : REAL(DFP) - first Lame parameter
!   mu : REAL(DFP) - shear modulus
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6)
!
! Formula:
!   $$\mathbb{C} = \lambda\, I \otimes I + 2\mu\, \mathbb{I}^{sym}$$

INTERFACE IsotropicTangent
  MODULE PURE FUNCTION MaterialJacobian_IsotropicTangent(lambda, mu) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda
    REAL(DFP), INTENT(IN) :: mu
    REAL(DFP) :: ans(6, 6)
  END FUNCTION MaterialJacobian_IsotropicTangent
END INTERFACE IsotropicTangent

!----------------------------------------------------------------------------
!                                            Rank4SymIdentity@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Symmetric rank-4 identity in Voigt form
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6) - diag(1, 1, 1, 1/2, 1/2, 1/2)
!
! Formula:
!   $$\mathbb{I}^{sym}_{ijkl} = \frac{1}{2}\left(\delta_{ik}\delta_{jl}
!   + \delta_{il}\delta_{jk}\right)$$

INTERFACE Rank4SymIdentity
  MODULE PURE FUNCTION MaterialJacobian_Rank4SymIdentity() RESULT(ans)
    REAL(DFP) :: ans(6, 6)
  END FUNCTION MaterialJacobian_Rank4SymIdentity
END INTERFACE Rank4SymIdentity

!----------------------------------------------------------------------------
!                                 Rank4IdentityOtimesIdentity@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: $I \otimes I$ in Voigt form
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6) - ones in the normal 3x3 block
!
! Formula:
!   $$(I \otimes I)_{ijkl} = \delta_{ij}\delta_{kl}$$

INTERFACE Rank4IdentityOtimesIdentity
  MODULE PURE FUNCTION MaterialJacobian_Rank4IdentityOtimesIdentity() &
    RESULT(ans)
    REAL(DFP) :: ans(6, 6)
  END FUNCTION MaterialJacobian_Rank4IdentityOtimesIdentity
END INTERFACE Rank4IdentityOtimesIdentity

!----------------------------------------------------------------------------
!                                    Rank4DeviatoricProjector@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Deviatoric projector in Voigt form
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6)
!
! Formula:
!   $$\mathbb{P}^{dev} = \mathbb{I}^{sym} - \frac{1}{3} I \otimes I$$

INTERFACE Rank4DeviatoricProjector
  MODULE PURE FUNCTION MaterialJacobian_Rank4DeviatoricProjector() &
    RESULT(ans)
    REAL(DFP) :: ans(6, 6)
  END FUNCTION MaterialJacobian_Rank4DeviatoricProjector
END INTERFACE Rank4DeviatoricProjector

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Display the content of [[MaterialJacobian_]]
!
! Arguments:
!   obj : MaterialJacobian_ - tangent
!   msg : CHARACTER(*) - header message
!   unitNo : INTEGER(I4B), OPTIONAL - output unit, default is stdout

INTERFACE Display
  MODULE SUBROUTINE MaterialJacobian_Display(obj, msg, unitNo)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE MaterialJacobian_Display
END INTERFACE Display

END MODULE MaterialJacobian_Method
