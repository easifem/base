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
! A material Jacobian is the tangent $\mathbb{C}$ of a constitutive
! model, $d\sigma = \mathbb{C} : d\varepsilon$. It has minor symmetry,
! so it is stored as the 3D Voigt matrix `C(6, 6)` with the order
! `[11, 22, 33, 12, 23, 13]`. The matrix maps the engineering strain
! vector (shear $\gamma = 2\varepsilon$) to the stress vector, so
! `C(I, J)` equals $\mathbb{C}_{ijkl}$ and the element stiffness is
! $K = \int B^{T} C B$.
!
! 2D matrices are produced on request by [[GetTangent]] and
! [[ReduceVoigtTangent]] with one of the `VoigtForm*` constants:
! plane strain and plane stress `[11, 22, 12]`, axisymmetric
! `[11, 22, 12, 33]`. Plane stress uses static condensation.
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
! The Voigt matrix is copied as it is. Absent tags take their default.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainMeasureSmall
! USE GlobalData, ONLY: StressMeasurePK2
! USE GlobalData, ONLY: StressRateJaumann
! USE MaterialJacobian_Method, ONLY: Initiate
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: from a 6x6 Voigt matrix, absent tags take the default
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!
!   CALL Initiate(obj, IsotropicTangent(2.0_DFP, 3.0_DFP), &
!                 stressType=StressMeasurePK2, rateType=StressRateJaumann)
!   CALL OK(MAXVAL(ABS(obj%C - IsotropicTangent(2.0_DFP, 3.0_DFP))) &
!           .LT. tol, "matrix: C")
!   CALL OK(obj%stressType .EQ. StressMeasurePK2, "matrix: stressType")
!   CALL OK(obj%strainType .EQ. StrainMeasureSmall, "matrix: strainType")
!   CALL OK(obj%rateType .EQ. StressRateJaumann, "matrix: rateType")
! END BLOCK
!
! END PROGRAM main
!```
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
    !! stress measure, StressMeasure*, default StressMeasureCauchy
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, StrainMeasure*, default StrainMeasureSmall
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, StressRate*, default StressRateNone
  END SUBROUTINE MaterialJacobian_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                           Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate [[MaterialJacobian_]] from a rank-4 array
!
! The minor-symmetric part of `C4` is stored, so for example the result
! of `Otimes(A, B)` can be used directly.
!
! Formula:
!   $$C_{IJ} = \frac{1}{4}\left(\mathbb{C}_{ijkl} + \mathbb{C}_{jikl}
!   + \mathbb{C}_{ijlk} + \mathbb{C}_{jilk}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: Initiate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 2: from a rank-4 array, only the minor-symmetric part is kept
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!   REAL(DFP) :: C4(3, 3, 3, 3)
!
!   C4 = 0.0_DFP
!   C4(1, 2, 1, 1) = 1.0_DFP
!   CALL Initiate(obj, C4)
!   CALL OK(ABS(obj%C(4, 1) - 0.5_DFP) .LT. tol, "rank-4: average C(4,1)")
!   CALL OK(ABS(SUM(ABS(obj%C)) - 0.5_DFP) .LT. tol, &
!           "rank-4: other entries zero")
! END BLOCK
!
! END PROGRAM main
!```
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
    !! stress measure, StressMeasure*, default StressMeasureCauchy
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, StrainMeasure*, default StrainMeasureSmall
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, StressRate*, default StressRateNone
  END SUBROUTINE MaterialJacobian_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                   MaterialJacobian@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[MaterialJacobian_]] from a 6x6 Voigt matrix
!
! Function form of [[Initiate]] with a Voigt matrix.
!
! Returns:
!   ans : MaterialJacobian_
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainMeasureGreen
! USE GlobalData, ONLY: StrainMeasureSmall
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasurePK2
! USE GlobalData, ONLY: StressRateNone
! USE MaterialJacobian_Method, ONLY: ASSIGNMENT(=)
! USE MaterialJacobian_Method, ONLY: Display
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: from a 6x6 Voigt matrix with the default tags
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!   REAL(DFP) :: C(6, 6)
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   CALL Display(obj, "isotropic tangent")
!   C = obj
!   CALL OK(MAXVAL(ABS(C - IsotropicTangent(2.0_DFP, 3.0_DFP))) .LT. tol, &
!           "matrix: C = obj")
!   CALL OK(obj%stressType .EQ. StressMeasureCauchy, "default stressType")
!   CALL OK(obj%strainType .EQ. StrainMeasureSmall, "default strainType")
!   CALL OK(obj%rateType .EQ. StressRateNone, "default rateType")
! END BLOCK
!
! ! Test 2: from a 6x6 Voigt matrix with tags
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP), &
!                          stressType=StressMeasurePK2, &
!                          strainType=StrainMeasureGreen)
!   CALL OK(obj%stressType .EQ. StressMeasurePK2, "tags: stressType")
!   CALL OK(obj%strainType .EQ. StrainMeasureGreen, "tags: strainType")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE MaterialJacobian
  MODULE PURE FUNCTION MaterialJacobian_Constructor1(C, stressType, &
                                                     strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, order [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, StressMeasure*, default StressMeasureCauchy
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, StrainMeasure*, default StrainMeasureSmall
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, StressRate*, default StressRateNone
    TYPE(MaterialJacobian_) :: ans
    !! material tangent
  END FUNCTION MaterialJacobian_Constructor1
END INTERFACE MaterialJacobian

!----------------------------------------------------------------------------
!                                   MaterialJacobian@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[MaterialJacobian_]] from a rank-4 array
!
! Function form of [[Initiate]] with a rank-4 array. The
! minor-symmetric part of `C4` is stored.
!
! Returns:
!   ans : MaterialJacobian_
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE GlobalData, ONLY: I4B
! USE MaterialJacobian_Method, ONLY: ASSIGNMENT(=)
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 3: from a rank-4 array, symmetric identity from Kronecker deltas
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!   REAL(DFP) :: S4(3, 3, 3, 3)
!   INTEGER(I4B) :: i, j, k, l
!
!   DO l = 1, 3
!     DO k = 1, 3
!       DO j = 1, 3
!         DO i = 1, 3
!           S4(i, j, k, l) = 0.5_DFP * (Eye3(i, k) * Eye3(j, l) &
!                                       + Eye3(i, l) * Eye3(j, k))
!         END DO
!       END DO
!     END DO
!   END DO
!   obj = MaterialJacobian(S4)
!   CALL OK(MAXVAL(ABS(obj%C - Rank4SymIdentity())) .LT. tol, &
!           "rank-4: Isym from deltas")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE MaterialJacobian
  MODULE PURE FUNCTION MaterialJacobian_Constructor2(C4, stressType, &
                                                     strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C4(3, 3, 3, 3)
    !! rank-4 tensor, need not have minor symmetry
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, StressMeasure*, default StressMeasureCauchy
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, StrainMeasure*, default StrainMeasureSmall
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, StressRate*, default StressRateNone
    TYPE(MaterialJacobian_) :: ans
    !! material tangent
  END FUNCTION MaterialJacobian_Constructor2
END INTERFACE MaterialJacobian

!----------------------------------------------------------------------------
!                           MaterialJacobian_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[MaterialJacobian_]]
!
! Pointer form of [[MaterialJacobian]] with a Voigt matrix. The caller
! must deallocate the pointer.
!
! Returns:
!   ans : CLASS(MaterialJacobian_), POINTER
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressRateJaumann
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian_Pointer
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: pointer from a 6x6 Voigt matrix with a rate tag
! BLOCK
!   CLASS(MaterialJacobian_), POINTER :: ptr
!
!   ptr => MaterialJacobian_Pointer(IsotropicTangent(2.0_DFP, 3.0_DFP), &
!                                   rateType=StressRateJaumann)
!   CALL OK(ASSOCIATED(ptr), "matrix: associated")
!   CALL OK(MAXVAL(ABS(ptr%C - IsotropicTangent(2.0_DFP, 3.0_DFP))) &
!           .LT. tol, "matrix: C")
!   CALL OK(ptr%rateType .EQ. StressRateJaumann, "matrix: rateType")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE MaterialJacobian_Pointer
  MODULE PURE FUNCTION MaterialJacobian_Pointer1(C, stressType, &
                                                 strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, order [11, 22, 33, 12, 23, 13]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, StressMeasure*, default StressMeasureCauchy
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, StrainMeasure*, default StrainMeasureSmall
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, StressRate*, default StressRateNone
    CLASS(MaterialJacobian_), POINTER :: ans
    !! new material tangent, the caller must deallocate it
  END FUNCTION MaterialJacobian_Pointer1
END INTERFACE MaterialJacobian_Pointer

!----------------------------------------------------------------------------
!                           MaterialJacobian_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[MaterialJacobian_]] from rank-4
!
! Pointer form of [[MaterialJacobian]] with a rank-4 array. The caller
! must deallocate the pointer.
!
! Returns:
!   ans : CLASS(MaterialJacobian_), POINTER
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: GetRank4
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE MaterialJacobian_Method, ONLY: MaterialJacobian_Pointer
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 2: pointer from a rank-4 array
! BLOCK
!   CLASS(MaterialJacobian_), POINTER :: ptr
!   TYPE(MaterialJacobian_) :: obj
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   ptr => MaterialJacobian_Pointer(GetRank4(obj))
!   CALL OK(MAXVAL(ABS(ptr%C - obj%C)) .LT. tol, "rank-4: C")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE MaterialJacobian_Pointer
  MODULE PURE FUNCTION MaterialJacobian_Pointer2(C4, stressType, &
                                                 strainType, rateType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C4(3, 3, 3, 3)
    !! rank-4 tensor, need not have minor symmetry
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! stress measure, StressMeasure*, default StressMeasureCauchy
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! strain measure, StrainMeasure*, default StrainMeasureSmall
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! objective stress rate, StressRate*, default StressRateNone
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
! Copies the 3D Voigt tangent. `obj = obj2` is the intrinsic
! assignment and copies the tags too.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainMeasureSmall
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressRateNone
! USE MaterialJacobian_Method, ONLY: ASSIGNMENT(=)
! USE MaterialJacobian_Method, ONLY: Display
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: from a 6x6 Voigt matrix with the default tags
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!   REAL(DFP) :: C(6, 6)
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   CALL Display(obj, "isotropic tangent")
!   C = obj
!   CALL OK(MAXVAL(ABS(C - IsotropicTangent(2.0_DFP, 3.0_DFP))) .LT. tol, &
!           "matrix: C = obj")
!   CALL OK(obj%stressType .EQ. StressMeasureCauchy, "default stressType")
!   CALL OK(obj%strainType .EQ. StrainMeasureSmall, "default strainType")
!   CALL OK(obj%rateType .EQ. StressRateNone, "default rateType")
! END BLOCK
!
! END PROGRAM main
!```

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
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainMeasureSmall
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressRateNone
! USE MaterialJacobian_Method, ONLY: GetRateType
! USE MaterialJacobian_Method, ONLY: GetStrainType
! USE MaterialJacobian_Method, ONLY: GetStressType
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: the getters return the default tags
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   CALL OK(GetStressType(obj) .EQ. StressMeasureCauchy, "GetStressType")
!   CALL OK(GetStrainType(obj) .EQ. StrainMeasureSmall, "GetStrainType")
!   CALL OK(GetRateType(obj) .EQ. StressRateNone, "GetRateType")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[SetMeasureTypes]]

INTERFACE GetStressType
  MODULE PURE FUNCTION MaterialJacobian_GetStressType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B) :: ans
    !! one of the StressMeasure* constants
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
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainMeasureSmall
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressRateNone
! USE MaterialJacobian_Method, ONLY: GetRateType
! USE MaterialJacobian_Method, ONLY: GetStrainType
! USE MaterialJacobian_Method, ONLY: GetStressType
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: the getters return the default tags
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   CALL OK(GetStressType(obj) .EQ. StressMeasureCauchy, "GetStressType")
!   CALL OK(GetStrainType(obj) .EQ. StrainMeasureSmall, "GetStrainType")
!   CALL OK(GetRateType(obj) .EQ. StressRateNone, "GetRateType")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[SetMeasureTypes]]

INTERFACE GetStrainType
  MODULE PURE FUNCTION MaterialJacobian_GetStrainType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B) :: ans
    !! one of the StrainMeasure* constants
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
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainMeasureSmall
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressRateNone
! USE MaterialJacobian_Method, ONLY: GetRateType
! USE MaterialJacobian_Method, ONLY: GetStrainType
! USE MaterialJacobian_Method, ONLY: GetStressType
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: the getters return the default tags
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   CALL OK(GetStressType(obj) .EQ. StressMeasureCauchy, "GetStressType")
!   CALL OK(GetStrainType(obj) .EQ. StrainMeasureSmall, "GetStrainType")
!   CALL OK(GetRateType(obj) .EQ. StressRateNone, "GetRateType")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[SetMeasureTypes]]

INTERFACE GetRateType
  MODULE PURE FUNCTION MaterialJacobian_GetRateType(obj) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B) :: ans
    !! one of the StressRate* constants
  END FUNCTION MaterialJacobian_GetRateType
END INTERFACE GetRateType

!----------------------------------------------------------------------------
!                                                  SetMeasureTypes@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Set the measure tags of a [[MaterialJacobian_]]
!
! Only the present arguments are changed. The tangent is not
! converted.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainMeasureGreen
! USE GlobalData, ONLY: StressMeasurePK2
! USE GlobalData, ONLY: StressRateNone
! USE MaterialJacobian_Method, ONLY: GetRateType
! USE MaterialJacobian_Method, ONLY: GetStrainType
! USE MaterialJacobian_Method, ONLY: GetStressType
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE MaterialJacobian_Method, ONLY: SetMeasureTypes
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 2: only the present tags are changed
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   CALL SetMeasureTypes(obj, stressType=StressMeasurePK2, &
!                        strainType=StrainMeasureGreen)
!   CALL OK(GetStressType(obj) .EQ. StressMeasurePK2, "set stressType")
!   CALL OK(GetStrainType(obj) .EQ. StrainMeasureGreen, "set strainType")
!   CALL OK(GetRateType(obj) .EQ. StressRateNone, "rateType unchanged")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetStressType]], [[GetStrainType]], [[GetRateType]]

INTERFACE SetMeasureTypes
  MODULE PURE SUBROUTINE MaterialJacobian_SetMeasureTypes(obj, &
                                          stressType, strainType, rateType)
    CLASS(MaterialJacobian_), INTENT(INOUT) :: obj
    !! material tangent
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: stressType
    !! new stress measure, StressMeasure*
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: strainType
    !! new strain measure, StrainMeasure*
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rateType
    !! new objective stress rate, StressRate*
  END SUBROUTINE MaterialJacobian_SetMeasureTypes
END INTERFACE SetMeasureTypes

!----------------------------------------------------------------------------
!                                                       Cijkl@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Component of the rank-4 tangent
!
! V(i, j) is the Voigt index of the pair (i, j).
!
! Returns:
!   ans : REAL(DFP) - the component $\mathbb{C}_{ijkl}$
!
! Formula:
!   $$\mathbb{C}_{ijkl} = C_{IJ},\quad I = V(i, j),\ J = V(k, l)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: Cijkl
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: lambda = 2 and mu = 3
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   CALL OK(ABS(Cijkl(obj, 1, 1, 1, 1) - 8.0_DFP) .LT. tol, &
!           "C1111 = lambda + 2 mu")
!   CALL OK(ABS(Cijkl(obj, 1, 1, 2, 2) - 2.0_DFP) .LT. tol, &
!           "C1122 = lambda")
!   CALL OK(ABS(Cijkl(obj, 1, 2, 1, 2) - 3.0_DFP) .LT. tol, "C1212 = mu")
!   CALL OK(ABS(Cijkl(obj, 2, 1, 1, 2) - Cijkl(obj, 1, 2, 2, 1)) .LT. tol, &
!           "minor symmetry")
! END BLOCK
!
! END PROGRAM main
!```

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
    !! component C_ijkl
  END FUNCTION MaterialJacobian_Cijkl
END INTERFACE Cijkl

!----------------------------------------------------------------------------
!                                                    GetRank4@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: The tangent as a rank-4 array
!
! The result has minor symmetry. [[MaterialJacobian]] with the result
! gives back the same tangent.
!
! Returns:
!   ans : REAL(DFP), SHAPE(3,3,3,3) - minor-symmetric rank-4 tensor
!
! Formula:
!   $$\mathrm{ans}_{ijkl} = C_{V(i,j), V(k,l)}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: GetRank4
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: entries of the rank-4 array and the round trip
! BLOCK
!   TYPE(MaterialJacobian_) :: obj, obj2
!   REAL(DFP) :: C4(3, 3, 3, 3)
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   C4 = GetRank4(obj)
!   CALL OK(ABS(C4(1, 2, 2, 1) - 3.0_DFP) .LT. tol, "C4(1,2,2,1) = mu")
!   CALL OK(ABS(C4(3, 3, 1, 1) - 2.0_DFP) .LT. tol, &
!           "C4(3,3,1,1) = lambda")
!   obj2 = MaterialJacobian(C4)
!   CALL OK(MAXVAL(ABS(obj2%C - obj%C)) .LT. tol, "round trip")
! END BLOCK
!
! END PROGRAM main
!```

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
! Calls [[ReduceVoigtTangent]] with the Voigt tangent of obj.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE, SHAPE(n, n) - n = VoigtSize(voigtForm)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: VoigtFormAxisymmetric
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStress
! USE MaterialJacobian_Method, ONLY: GetTangent
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: E = 200.0_DFP
! REAL(DFP), PARAMETER :: nu = 0.3_DFP
! REAL(DFP), PARAMETER :: lam = E * nu / ((1.0_DFP + nu) &
!                                         * (1.0_DFP - 2.0_DFP * nu))
! REAL(DFP), PARAMETER :: mu = E / (2.0_DFP * (1.0_DFP + nu))
! TYPE(MaterialJacobian_) :: obj
!
! obj = MaterialJacobian(IsotropicTangent(lam, mu))
!
! ! Test 1: the default form is 3D
! BLOCK
!   REAL(DFP), ALLOCATABLE :: C2(:, :)
!
!   C2 = GetTangent(obj)
!   CALL OK(SIZE(C2, 1) .EQ. 6 .AND. SIZE(C2, 2) .EQ. 6, "3D: 6x6")
! END BLOCK
!
! ! Test 2: plane strain
! BLOCK
!   REAL(DFP), ALLOCATABLE :: C2(:, :)
!   REAL(DFP) :: a
!
!   a = E / ((1.0_DFP + nu) * (1.0_DFP - 2.0_DFP * nu))
!   C2 = GetTangent(obj, VoigtFormPlaneStrain)
!   CALL OK(SIZE(C2, 1) .EQ. 3, "plane strain: 3x3")
!   CALL OK(ABS(C2(1, 1) - a * (1.0_DFP - nu)) .LT. tol, &
!           "plane strain: C11")
!   CALL OK(ABS(C2(1, 2) - a * nu) .LT. tol, "plane strain: C12")
!   CALL OK(ABS(C2(3, 3) - a * (1.0_DFP - 2.0_DFP * nu) / 2.0_DFP) &
!           .LT. tol, "plane strain: C33")
! END BLOCK
!
! ! Test 3: plane stress
! BLOCK
!   REAL(DFP), ALLOCATABLE :: C2(:, :)
!   REAL(DFP) :: a
!
!   a = E / (1.0_DFP - nu * nu)
!   C2 = GetTangent(obj, VoigtFormPlaneStress)
!   CALL OK(ABS(C2(1, 1) - a) .LT. tol, "plane stress: C11")
!   CALL OK(ABS(C2(1, 2) - a * nu) .LT. tol, "plane stress: C12")
!   CALL OK(ABS(C2(3, 3) - mu) .LT. tol, "plane stress: C33")
!   CALL OK(ABS(C2(1, 3)) .LT. tol, "plane stress: C13 is zero")
! END BLOCK
!
! ! Test 4: axisymmetric [11, 22, 12, 33]
! BLOCK
!   REAL(DFP), ALLOCATABLE :: C2(:, :)
!
!   C2 = GetTangent(obj, VoigtFormAxisymmetric)
!   CALL OK(SIZE(C2, 1) .EQ. 4, "axisymmetric: 4x4")
!   CALL OK(ABS(C2(4, 4) - (lam + 2.0_DFP * mu)) .LT. tol, &
!           "axisymmetric: C44")
!   CALL OK(ABS(C2(1, 4) - lam) .LT. tol, "axisymmetric: C14")
!   CALL OK(ABS(C2(3, 3) - mu) .LT. tol, "axisymmetric: C33")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[ReduceVoigtTangent]]

INTERFACE GetTangent
  MODULE PURE FUNCTION MaterialJacobian_GetTangent(obj, voigtForm) &
    RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants, default VoigtForm3D
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Voigt tangent, SHAPE (n, n) with n = VoigtSize(voigtForm)
  END FUNCTION MaterialJacobian_GetTangent
END INTERFACE GetTangent

!----------------------------------------------------------------------------
!                                    ReduceVoigtTangent@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Reduce a 3D Voigt tangent to a 2D Voigt form
!
! The reduction depends on the Voigt form.
!
! - `VoigtFormPlaneStrain`: rows and columns `[1, 2, 4]`
! - `VoigtFormAxisymmetric`: rows and columns `[1, 2, 4, 3]`
! - `VoigtFormPlaneStress`: static condensation with
!   $\sigma_{33} = \sigma_{23} = \sigma_{13} = 0$, using the in-plane
!   set $i = [1, 2, 4]$ and out-of-plane set $o = [3, 5, 6]$
! - `VoigtForm3D` or unknown: the 6x6 matrix itself
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE, SHAPE(n, n) - n = VoigtSize(voigtForm)
!
! Formula:
!   $$C^{ps} = C_{ii} - C_{io} C_{oo}^{-1} C_{oi}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStress
! USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: C6(6, 6), B(6, 6)
! INTEGER(I4B) :: i, j
!
! ! symmetric positive definite anisotropic tangent
! DO j = 1, 6
!   DO i = 1, 6
!     B(i, j) = REAL(MOD(3 * i + 5 * j, 7), DFP) / 7.0_DFP
!   END DO
! END DO
! C6 = MATMUL(TRANSPOSE(B), B)
! DO i = 1, 6
!   C6(i, i) = C6(i, i) + 5.0_DFP
! END DO
!
! ! Test 1: plane strain is the sub-matrix [1, 2, 4]
! BLOCK
!   REAL(DFP), ALLOCATABLE :: C2(:, :)
!
!   C2 = ReduceVoigtTangent(C6, VoigtFormPlaneStrain)
!   CALL OK(MAXVAL(ABS(C2 - C6([1, 2, 4], [1, 2, 4]))) .LT. tol, &
!           "plane strain: sub-matrix")
! END BLOCK
!
! ! Test 2: plane stress, out-of-plane stresses vanish
! BLOCK
!   REAL(DFP), ALLOCATABLE :: C2(:, :)
!   REAL(DFP) :: ei(3), eo(3), e6(6), s6(6)
!
!   C2 = ReduceVoigtTangent(C6, VoigtFormPlaneStress)
!   ei = [1.0E-3_DFP, -2.0E-3_DFP, 0.5E-3_DFP]
!   eo = -MATMUL(Inverse3(C6([3, 5, 6], [3, 5, 6])), &
!                MATMUL(C6([3, 5, 6], [1, 2, 4]), ei))
!   e6([1, 2, 4]) = ei
!   e6([3, 5, 6]) = eo
!   s6 = MATMUL(C6, e6)
!   CALL OK(MAXVAL(ABS(s6([3, 5, 6]))) .LT. tol, &
!           "plane stress: s33 = s23 = s13 = 0")
!   CALL OK(MAXVAL(ABS(s6([1, 2, 4]) - MATMUL(C2, ei))) .LT. tol, &
!           "plane stress: condensed tangent")
! END BLOCK
!
! CONTAINS
!
! ! Inverse of a 3x3 matrix by cofactors
! FUNCTION Inverse3(A) RESULT(X)
!   REAL(DFP), INTENT(IN) :: A(3, 3)
!   REAL(DFP) :: X(3, 3)
!
!   X(1, 1) = A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)
!   X(1, 2) = A(1, 3) * A(3, 2) - A(1, 2) * A(3, 3)
!   X(1, 3) = A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2)
!   X(2, 1) = A(2, 3) * A(3, 1) - A(2, 1) * A(3, 3)
!   X(2, 2) = A(1, 1) * A(3, 3) - A(1, 3) * A(3, 1)
!   X(2, 3) = A(1, 3) * A(2, 1) - A(1, 1) * A(2, 3)
!   X(3, 1) = A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1)
!   X(3, 2) = A(1, 2) * A(3, 1) - A(1, 1) * A(3, 2)
!   X(3, 3) = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
!   X = X / (A(1, 1) * X(1, 1) + A(1, 2) * X(2, 1) + A(1, 3) * X(3, 1))
! END FUNCTION Inverse3
!
! END PROGRAM main
!```
!
! See also: [[ReduceVoigtTangent_]], [[GetTangent]]

INTERFACE ReduceVoigtTangent
  MODULE PURE FUNCTION MaterialJacobian_ReduceVoigtTangent(C, voigtForm) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! reduced tangent, SHAPE (n, n) with n = VoigtSize(voigtForm)
  END FUNCTION MaterialJacobian_ReduceVoigtTangent
END INTERFACE ReduceVoigtTangent

!----------------------------------------------------------------------------
!                                     ReduceVoigtTangent_@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-02
! summary: Reduce a 3D Voigt tangent to a 2D Voigt form (no allocation)
!
! Same as [[ReduceVoigtTangent]] but the result is written in `ans`,
! which is allocated by the caller. Only `ans(1:nrow, 1:ncol)` is
! written. The tangent `C` is not assumed to be symmetric.
!
! Formula:
!   $$C^{ps} = C_{ii} - C_{io} C_{oo}^{-1} C_{oi}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormAxisymmetric
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStress
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent
! USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
! INTEGER(I4B), PARAMETER :: forms(4) = [VoigtForm3D, &
!                                        VoigtFormPlaneStrain, &
!                                        VoigtFormPlaneStress, &
!                                        VoigtFormAxisymmetric]
! CHARACTER(*), PARAMETER :: names(4) = ["3D          ", &
!                                        "plane strain", &
!                                        "plane stress", &
!                                        "axisymmetric"]
! REAL(DFP) :: C(6, 6)
!
! C = IsotropicTangent(lambda=1.5_DFP, mu=0.5_DFP)
! C(1, 4) = 0.1_DFP
! C(5, 2) = 0.2_DFP
!
! ! Test 1: size and values in each Voigt form
! BLOCK
!   REAL(DFP) :: Cr(6, 6)
!   REAL(DFP), ALLOCATABLE :: Cref(:, :)
!   INTEGER(I4B) :: ii, nrow, ncol
!
!   DO ii = 1, 4
!     Cref = ReduceVoigtTangent(C, forms(ii))
!     CALL ReduceVoigtTangent_(C, forms(ii), Cr, nrow, ncol)
!     CALL OK(nrow .EQ. SIZE(Cref, 1) .AND. ncol .EQ. SIZE(Cref, 2), &
!             TRIM(names(ii))//": nrow and ncol")
!     CALL OK(MAXVAL(ABS(Cr(1:nrow, 1:ncol) - Cref)) .LT. tol, &
!             TRIM(names(ii))//": values")
!   END DO
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[ReduceVoigtTangent]]

INTERFACE ReduceVoigtTangent_
  MODULE PURE SUBROUTINE MaterialJacobian_ReduceVoigtTangent_(C, voigtForm, &
                                                              ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: C(6, 6)
    !! 3D Voigt tangent, need not be symmetric
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! reduced tangent, SIZE at least (n, n), n = VoigtSize(voigtForm)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE MaterialJacobian_ReduceVoigtTangent_
END INTERFACE ReduceVoigtTangent_

!----------------------------------------------------------------------------
!                                          Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Stress from tangent and strain tensor
!
! The symmetric part of `eps` is used.
!
! Returns:
!   ans : Rank2Tensor_ - symmetric stress (increment)
!
! Formula:
!   $$\sigma_{ij} = \mathbb{C}_{ijkl}\, \varepsilon_{kl}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE MaterialJacobian_Method, ONLY: Contraction
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: lam = 2.0_DFP
! REAL(DFP), PARAMETER :: mu = 3.0_DFP
! REAL(DFP), PARAMETER :: e0(3, 3) = RESHAPE( &
!                         [1.0_DFP, 0.2_DFP, -0.3_DFP, &
!                          0.2_DFP, 2.0_DFP, 0.4_DFP, &
!                          -0.3_DFP, 0.4_DFP, -1.0_DFP], [3, 3])
! TYPE(MaterialJacobian_) :: obj
! REAL(DFP) :: expected(3, 3)
!
! obj = MaterialJacobian(IsotropicTangent(lam, mu))
! expected = lam * (e0(1, 1) + e0(2, 2) + e0(3, 3)) * Eye3 &
!            + 2.0_DFP * mu * e0
!
! ! Test 1: strain as a Rank2Tensor_
! BLOCK
!   TYPE(Rank2Tensor_) :: sig
!
!   sig = Contraction(obj, Rank2Tensor(e0, .TRUE.))
!   CALL OK(MAXVAL(ABS(sig%T - expected)) .LT. tol, "Rank2Tensor: stress")
!   CALL OK(sig%isSym, "Rank2Tensor: stress is symmetric")
! END BLOCK
!
! END PROGRAM main
!```

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
! `eps` may be stress-like or strain-like; it is converted to the
! engineering strain vector before the product.
!
! Returns:
!   ans : VoigtRank2Tensor_ - stress (increment), `StressTypeVoigt`
!
! Formula:
!   $$\sigma_{I} = C_{IJ}\, \varepsilon^{eng}_{J}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE MaterialJacobian_Method, ONLY: Contraction
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: lam = 2.0_DFP
! REAL(DFP), PARAMETER :: mu = 3.0_DFP
! REAL(DFP), PARAMETER :: e0(3, 3) = RESHAPE( &
!                         [1.0_DFP, 0.2_DFP, -0.3_DFP, &
!                          0.2_DFP, 2.0_DFP, 0.4_DFP, &
!                          -0.3_DFP, 0.4_DFP, -1.0_DFP], [3, 3])
! TYPE(MaterialJacobian_) :: obj
! REAL(DFP) :: expected(3, 3)
!
! obj = MaterialJacobian(IsotropicTangent(lam, mu))
! expected = lam * (e0(1, 1) + e0(2, 2) + e0(3, 3)) * Eye3 &
!            + 2.0_DFP * mu * e0
!
! ! Test 2: strain as a strain-like or stress-like VoigtRank2Tensor_
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: vsig
!   REAL(DFP) :: T(3, 3)
!
!   vsig = Contraction(obj, VoigtRank2Tensor(e0, StrainTypeVoigt))
!   T = vsig
!   CALL OK(MAXVAL(ABS(T - expected)) .LT. tol, "Voigt strain-like")
!   vsig = Contraction(obj, VoigtRank2Tensor(e0, StressTypeVoigt))
!   T = vsig
!   CALL OK(MAXVAL(ABS(T - expected)) .LT. tol, "Voigt stress-like")
!   CALL OK(vsig%voigtType .EQ. StressTypeVoigt, "Voigt: result type")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Contraction
  MODULE PURE FUNCTION MaterialJacobian_Contraction2(obj, eps) RESULT(ans)
    CLASS(MaterialJacobian_), INTENT(IN) :: obj
    !! material tangent
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: eps
    !! strain (increment), stress-like or strain-like
    TYPE(VoigtRank2Tensor_) :: ans
    !! stress (increment), StressTypeVoigt
  END FUNCTION MaterialJacobian_Contraction2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            IsotropicTangent@Rank4Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Isotropic elastic tangent in Voigt form
!
! Returns:
!   ans : REAL(DFP), SHAPE(6,6)
!
! Formula:
!   $$\mathbb{C} = \lambda\, I \otimes I + 2\mu\, \mathbb{I}^{sym}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
! USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: lambda = 2 and mu = 1
! BLOCK
!   REAL(DFP) :: C(6, 6)
!
!   C = IsotropicTangent(lambda=2.0_DFP, mu=1.0_DFP)
!   CALL OK(ABS(C(1, 1) - 4.0_DFP) .LT. tol, "C11 = lambda + 2 mu")
!   CALL OK(ABS(C(1, 2) - 2.0_DFP) .LT. tol, "C12 = lambda")
!   CALL OK(ABS(C(4, 4) - 1.0_DFP) .LT. tol, "C44 = mu")
!   CALL OK(MAXVAL(ABS(C - 2.0_DFP * Rank4IdentityOtimesIdentity() &
!                      - 2.0_DFP * Rank4SymIdentity())) .LT. tol, &
!           "C = lambda I x I + 2 mu Isym")
! END BLOCK
!
! END PROGRAM main
!```

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
! Returns:
!   ans : REAL(DFP), SHAPE(6,6) - diag(1, 1, 1, 1/2, 1/2, 1/2)
!
! Formula:
!   $$\mathbb{I}^{sym}_{ijkl} = \frac{1}{2}\left(\delta_{ik}\delta_{jl}
!   + \delta_{il}\delta_{jk}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: Contraction
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: diagonal matrix diag(1, 1, 1, 1/2, 1/2, 1/2)
! BLOCK
!   REAL(DFP) :: S(6, 6)
!
!   S = Rank4SymIdentity()
!   CALL OK(ABS(S(1, 1) - 1.0_DFP) .LT. tol, "S11 = 1")
!   CALL OK(ABS(S(4, 4) - 0.5_DFP) .LT. tol, "S44 = 1/2")
!   CALL OK(ABS(SUM(S) - 4.5_DFP) .LT. tol, "off-diagonal entries zero")
! END BLOCK
!
! ! Test 2: Isym : eps = eps
! BLOCK
!   TYPE(Rank2Tensor_) :: sig
!   REAL(DFP) :: e0(3, 3)
!
!   e0 = RESHAPE([1.0_DFP, 0.2_DFP, -0.3_DFP, &
!                 0.2_DFP, 2.0_DFP, 0.4_DFP, &
!                 -0.3_DFP, 0.4_DFP, -1.0_DFP], [3, 3])
!   sig = Contraction(MaterialJacobian(Rank4SymIdentity()), &
!                     Rank2Tensor(e0, .TRUE.))
!   CALL OK(MAXVAL(ABS(sig%T - e0)) .LT. tol, "Isym : eps = eps")
! END BLOCK
!
! END PROGRAM main
!```

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
! Returns:
!   ans : REAL(DFP), SHAPE(6,6) - ones in the normal 3x3 block
!
! Formula:
!   $$(I \otimes I)_{ijkl} = \delta_{ij}\delta_{kl}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
! USE Rank2Tensor_Method, ONLY: Otimes
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: ones in the normal 3x3 block, zero elsewhere
! BLOCK
!   REAL(DFP) :: A(6, 6)
!
!   A = Rank4IdentityOtimesIdentity()
!   CALL OK(ALL(ABS(A(1:3, 1:3) - 1.0_DFP) .LT. tol), "normal block")
!   CALL OK(ABS(SUM(A) - 9.0_DFP) .LT. tol, "other entries zero")
! END BLOCK
!
! ! Test 2: equals Otimes(I, I) of Rank2Tensor_
! BLOCK
!   TYPE(Rank2Tensor_) :: I2
!   TYPE(MaterialJacobian_) :: obj
!
!   I2 = Rank2Tensor(Eye3, .TRUE.)
!   obj = MaterialJacobian(Otimes(I2, I2))
!   CALL OK(MAXVAL(ABS(obj%C - Rank4IdentityOtimesIdentity())) .LT. tol, &
!           "Otimes(I, I)")
! END BLOCK
!
! END PROGRAM main
!```

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
! Returns:
!   ans : REAL(DFP), SHAPE(6,6)
!
! Formula:
!   $$\mathbb{P}^{dev} = \mathbb{I}^{sym} - \frac{1}{3} I \otimes I$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE MaterialJacobian_Method, ONLY: Contraction
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE MaterialJacobian_Method, ONLY: Rank4DeviatoricProjector
! USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
! USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
! USE Rank2Tensor_Method, ONLY: Dev
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: Pdev = Isym - I x I / 3
! BLOCK
!   REAL(DFP) :: P(6, 6)
!
!   P = Rank4DeviatoricProjector()
!   CALL OK(MAXVAL(ABS(P - Rank4SymIdentity() &
!                      + Rank4IdentityOtimesIdentity() / 3.0_DFP)) &
!           .LT. tol, "Pdev = Isym - I x I / 3")
! END BLOCK
!
! ! Test 2: Pdev : eps = dev(eps)
! BLOCK
!   TYPE(Rank2Tensor_) :: eps, sig, D
!   REAL(DFP) :: e0(3, 3)
!
!   e0 = RESHAPE([1.0_DFP, 0.2_DFP, -0.3_DFP, &
!                 0.2_DFP, 2.0_DFP, 0.4_DFP, &
!                 -0.3_DFP, 0.4_DFP, -1.0_DFP], [3, 3])
!   eps = Rank2Tensor(e0, .TRUE.)
!   sig = Contraction(MaterialJacobian(Rank4DeviatoricProjector()), eps)
!   D = Dev(eps)
!   CALL OK(MAXVAL(ABS(sig%T - D%T)) .LT. tol, "Pdev : eps = dev eps")
! END BLOCK
!
! END PROGRAM main
!```

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
! Prints the Voigt tangent and the names of the measure tags.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: MaterialJacobian_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainMeasureSmall
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressRateNone
! USE MaterialJacobian_Method, ONLY: ASSIGNMENT(=)
! USE MaterialJacobian_Method, ONLY: Display
! USE MaterialJacobian_Method, ONLY: IsotropicTangent
! USE MaterialJacobian_Method, ONLY: MaterialJacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: from a 6x6 Voigt matrix with the default tags
! BLOCK
!   TYPE(MaterialJacobian_) :: obj
!   REAL(DFP) :: C(6, 6)
!
!   obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
!   CALL Display(obj, "isotropic tangent")
!   C = obj
!   CALL OK(MAXVAL(ABS(C - IsotropicTangent(2.0_DFP, 3.0_DFP))) .LT. tol, &
!           "matrix: C = obj")
!   CALL OK(obj%stressType .EQ. StressMeasureCauchy, "default stressType")
!   CALL OK(obj%strainType .EQ. StrainMeasureSmall, "default strainType")
!   CALL OK(obj%rateType .EQ. StressRateNone, "default rateType")
! END BLOCK
!
! END PROGRAM main
!```

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
