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
! summary: Methods for [[Rank2Tensor_]] and its kinematic extensions
!
! Description:
!   This module collects the methods for second order tensors in three
!   dimensions. The implementations live in the following submodules:
!
!   - `@ConstructorMethods`: Initiate, Rank2Tensor, assignment, special
!     tensors, Sym, SkewSym, Transpose, Inv
!   - `@IOMethods`: Display
!   - `@InvarMethods`: Trace, Det, invariants, Lode angle, Iso, Dev,
!     Eigen, PolarDecomp
!   - `@ContractionMethods`: double contraction with Voigt tensors
!   - `@OperatorMethods`: `+`, `-`, `*`, `/`, MATMUL
!   - `@PullbackMethods`, `@PushForwardMethods`
!   - `@UtilityMethods`: Otimes, PrincipalValues, SpectralRadius,
!     RotationPart, Exp
!   - `@DeformationGradientMethods`: DeformationGradient, Jacobian,
!     RightStretch, LeftStretch, Rotation, PrincipalStretch
!   - `@CauchyGreenMethods`: LeftCauchyGreen, RightCauchyGreen
!   - `@StrainMethods`: GreenStrain, AlmansiStrain, SmallStrain
!   - `@VelocityGradientMethods`: VelocityGradient, StrainRate,
!     ContinuumSpin
!   - `@StressMethods`: Stress, GetStressType, SetStressType,
!     ConvertStress, CauchyStress, KirchhoffStress, PK1Stress, PK2Stress,
!     MandelStress
!   - `@StressInvarMethods`: MeanStress, EquivalentStress,
!     HaighWestergaard
!   - `@StressDerivativeMethods`: MeanStressDerivative, J2Derivative,
!     J3Derivative, EquivalentStressDerivative
!
!   Note that `*` and `/` between two tensors act element-wise
!   (Hadamard). Use `MATMUL` for the single contraction $A \cdot B$ and
!   `Contraction` for the double contraction $A : B$.

MODULE Rank2Tensor_Method
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: LeftCauchyGreen_
USE BaseType, ONLY: RightCauchyGreen_
USE BaseType, ONLY: GreenStrain_
USE BaseType, ONLY: AlmansiStrain_
USE BaseType, ONLY: SmallStrain_
USE BaseType, ONLY: VelocityGradient_
USE BaseType, ONLY: StrainRate_
USE BaseType, ONLY: ContinuumSpin_
USE BaseType, ONLY: Stress_
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: Rank2Tensor
PUBLIC :: Rank2Tensor_Pointer
PUBLIC :: IdentityTensor
PUBLIC :: getOnes
PUBLIC :: getZeros
PUBLIC :: IsotropicTensor
PUBLIC :: isSym
PUBLIC :: isDeviatoric
PUBLIC :: Inv
PUBLIC :: TRANSPOSE
PUBLIC :: Sym
PUBLIC :: SkewSym
PUBLIC :: Display
PUBLIC :: Trace
PUBLIC :: J2
PUBLIC :: J3
PUBLIC :: Det
PUBLIC :: LodeAngle
PUBLIC :: Isotropic
PUBLIC :: Iso
PUBLIC :: Deviatoric
PUBLIC :: Dev
PUBLIC :: Invariants
PUBLIC :: Eigen
PUBLIC :: PolarDecomp
PUBLIC :: Contraction
PUBLIC :: OPERATOR(+)
PUBLIC :: OPERATOR(-)
PUBLIC :: OPERATOR(*)
PUBLIC :: OPERATOR(/)
PUBLIC :: MATMUL
PUBLIC :: Pullback
PUBLIC :: PushForward
PUBLIC :: Otimes
PUBLIC :: PrincipalValues
PUBLIC :: SpectralRadius
PUBLIC :: RotationPart
PUBLIC :: Exp
PUBLIC :: DeformationGradient
PUBLIC :: DeformationGradient_Pointer
PUBLIC :: Jacobian
PUBLIC :: RightStretch
PUBLIC :: LeftStretch
PUBLIC :: Rotation
PUBLIC :: PrincipalStretch
PUBLIC :: LeftCauchyGreen
PUBLIC :: LeftCauchyGreen_Pointer
PUBLIC :: RightCauchyGreen
PUBLIC :: RightCauchyGreen_Pointer
PUBLIC :: GreenStrain
PUBLIC :: GreenStrain_Pointer
PUBLIC :: AlmansiStrain
PUBLIC :: AlmansiStrain_Pointer
PUBLIC :: SmallStrain
PUBLIC :: SmallStrain_Pointer
PUBLIC :: VelocityGradient
PUBLIC :: VelocityGradient_Pointer
PUBLIC :: StrainRate
PUBLIC :: StrainRate_Pointer
PUBLIC :: ContinuumSpin
PUBLIC :: ContinuumSpin_Pointer
PUBLIC :: Stress
PUBLIC :: Stress_Pointer
PUBLIC :: GetStressType
PUBLIC :: SetStressType
PUBLIC :: ConvertStress
PUBLIC :: CauchyStress
PUBLIC :: KirchhoffStress
PUBLIC :: PK1Stress
PUBLIC :: PK2Stress
PUBLIC :: MandelStress
PUBLIC :: MeanStress
PUBLIC :: EquivalentStress
PUBLIC :: HaighWestergaard
PUBLIC :: MeanStressDerivative
PUBLIC :: J2Derivative
PUBLIC :: J3Derivative
PUBLIC :: EquivalentStressDerivative

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Initiate [[Rank2Tensor_]] from another [[Rank2Tensor_]]
!
! Description:
!   This routine is also used for `obj = obj2`.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor to initiate
!   obj2 : Rank2Tensor_ - source tensor
!
! Formula:
!   $$\mathrm{obj} = \mathrm{obj2}$$

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_Initiate1(obj, obj2)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
  END SUBROUTINE Rank2Tensor_Initiate1
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE Rank2Tensor_Initiate1
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Initiate [[Rank2Tensor_]] from a 3x3 matrix
!
! Arguments:
!   obj : Rank2Tensor_ - tensor to initiate
!   mat : REAL(DFP), SHAPE(3,3) - tensor components
!   isSym : LOGICAL(LGT), OPTIONAL - symmetry flag, default is false
!
! Formula:
!   $$T_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! TYPE(Rank2Tensor_) :: obj
! REAL(DFP) :: mat(3, 3)
! CALL RANDOM_NUMBER(mat)
! CALL Initiate(obj, mat)
! CALL Initiate(obj, 0.5_DFP * (mat + TRANSPOSE(mat)), .TRUE.)
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_Initiate2(obj, mat, isSym)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSym
  END SUBROUTINE Rank2Tensor_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Initiate [[Rank2Tensor_]] from a [[VoigtRank2Tensor_]]
!
! Description:
!   The result is symmetric. This routine is also used for `obj = V`.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor to initiate
!   V : VoigtRank2Tensor_ - source tensor in Voigt notation
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{Scale}\, V_4$$

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_Initiate3(obj, V)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
  END SUBROUTINE Rank2Tensor_Initiate3
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE Rank2Tensor_Initiate3
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Initiate [[VoigtRank2Tensor_]] from a [[Rank2Tensor_]]
!
! Arguments:
!   obj : VoigtRank2Tensor_ - tensor to initiate
!   T : Rank2Tensor_ - source tensor
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right)$$

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_Initiate4(obj, T, voigtType)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    INTEGER(I4B), INTENT(IN) :: voigtType
  END SUBROUTINE Rank2Tensor_Initiate4
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                             Rank2Tensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Returns a [[Rank2Tensor_]] from a 3x3 matrix
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - tensor components
!   isSym : LOGICAL(LGT), OPTIONAL - symmetry flag, default is false
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$T_{ij} = \mathrm{mat}_{ij}$$

INTERFACE Rank2Tensor
  MODULE PURE FUNCTION Rank2Tensor_Constructor1(mat, isSym) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSym
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Constructor1
END INTERFACE Rank2Tensor

!----------------------------------------------------------------------------
!                                             Rank2Tensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Returns a [[Rank2Tensor_]] from a [[VoigtRank2Tensor_]]
!
! Arguments:
!   V : VoigtRank2Tensor_ - source tensor in Voigt notation
!
! Returns:
!   ans : Rank2Tensor_ - symmetric tensor
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{Scale}\, V_4$$

INTERFACE Rank2Tensor
  MODULE PURE FUNCTION Rank2Tensor_Constructor2(V) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Constructor2
END INTERFACE Rank2Tensor

!----------------------------------------------------------------------------
!                                     Rank2Tensor_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Returns a pointer to a new [[Rank2Tensor_]]
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - tensor components
!   isSym : LOGICAL(LGT), OPTIONAL - symmetry flag, default is false
!
! Returns:
!   ans : CLASS(Rank2Tensor_), POINTER - caller must deallocate
!
! Formula:
!   $$T_{ij} = \mathrm{mat}_{ij}$$

INTERFACE Rank2Tensor_Pointer
  MODULE PURE FUNCTION Rank2Tensor_Pointer1(mat, isSym) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSym
    CLASS(Rank2Tensor_), POINTER :: ans
  END FUNCTION Rank2Tensor_Pointer1
END INTERFACE Rank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                     Rank2Tensor_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Returns a pointer to a new [[Rank2Tensor_]]
!
! Arguments:
!   V : VoigtRank2Tensor_ - source tensor in Voigt notation
!
! Returns:
!   ans : CLASS(Rank2Tensor_), POINTER - caller must deallocate
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{Scale}\, V_4$$

INTERFACE Rank2Tensor_Pointer
  MODULE PURE FUNCTION Rank2Tensor_Pointer2(V) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    CLASS(Rank2Tensor_), POINTER :: ans
  END FUNCTION Rank2Tensor_Pointer2
END INTERFACE Rank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: [[Rank2Tensor_]] = Matrix(3,3)
!
! Description:
!   The resulting tensor is marked as not symmetric.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor (output)
!   mat : REAL(DFP), SHAPE(3,3) - tensor components
!
! Formula:
!   $$T_{ij} = \mathrm{mat}_{ij}$$

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE Rank2Tensor_ObjEqMat(obj, mat)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: mat(3, 3)
  END SUBROUTINE Rank2Tensor_ObjEqMat
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Matrix(3,3) = [[Rank2Tensor_]]
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - tensor components (output)
!   obj : Rank2Tensor_ - tensor
!
! Formula:
!   $$\mathrm{mat}_{ij} = T_{ij}$$

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE Rank2Tensor_MatEqObj(mat, obj)
    REAL(DFP), INTENT(INOUT) :: mat(3, 3)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
  END SUBROUTINE Rank2Tensor_MatEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: [[VoigtRank2Tensor_]] = [[Rank2Tensor_]]
!
! Description:
!   The Voigt type of the result is `StressTypeVoigt`.
!
! Arguments:
!   V : VoigtRank2Tensor_ - Voigt tensor (output)
!   obj : Rank2Tensor_ - tensor
!
! Formula:
!   $$V_4 = \frac{1}{2}\left(T_{12} + T_{21}\right)$$

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE Rank2Tensor_VoigtEqObj(V, obj)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: V
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
  END SUBROUTINE Rank2Tensor_VoigtEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                          IdentityTensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Set obj to the rank-2 identity tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor (output)
!
! Formula:
!   $$T_{ij} = \delta_{ij}$$

INTERFACE IdentityTensor
  MODULE PURE SUBROUTINE Rank2Tensor_IdentityTensor(obj)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
  END SUBROUTINE Rank2Tensor_IdentityTensor
END INTERFACE IdentityTensor

!----------------------------------------------------------------------------
!                                                 getOnes@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Set every component of obj to one
!
! Arguments:
!   obj : Rank2Tensor_ - tensor (output)
!
! Formula:
!   $$T_{ij} = 1$$

INTERFACE getOnes
  MODULE PURE SUBROUTINE Rank2Tensor_GetOnes(obj)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
  END SUBROUTINE Rank2Tensor_GetOnes
END INTERFACE getOnes

!----------------------------------------------------------------------------
!                                                getZeros@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Set every component of obj to zero
!
! Arguments:
!   obj : Rank2Tensor_ - tensor (output)
!
! Formula:
!   $$T_{ij} = 0$$

INTERFACE getZeros
  MODULE PURE SUBROUTINE Rank2Tensor_GetZeros(obj)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
  END SUBROUTINE Rank2Tensor_GetZeros
END INTERFACE getZeros

!----------------------------------------------------------------------------
!                                         IsotropicTensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Set obj to a second order isotropic tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor (output)
!   lambda : REAL(DFP) - scaling of the identity
!
! Formula:
!   $$T_{ij} = \lambda\, \delta_{ij}$$

INTERFACE IsotropicTensor
  MODULE PURE SUBROUTINE Rank2Tensor_IsotropicTensor(obj, lambda)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lambda
  END SUBROUTINE Rank2Tensor_IsotropicTensor
END INTERFACE IsotropicTensor

!----------------------------------------------------------------------------
!                                                   isSym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Returns the symmetry flag of the tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : LOGICAL(LGT) - value of `obj%isSym`
!
! Formula:
!   $$\mathrm{ans} = (T = T^{T})$$

INTERFACE isSym
  MODULE PURE FUNCTION Rank2Tensor_IsSym(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION Rank2Tensor_IsSym
END INTERFACE isSym

!----------------------------------------------------------------------------
!                                            isDeviatoric@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Returns true if the tensor is traceless
!
! Description:
!   The trace is compared with a tolerance relative to the largest
!   absolute component, so the test does not depend on the magnitude of
!   the tensor.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : LOGICAL(LGT)
!
! Formula:
!   $$|T_{ii}| \le 10^{-12} \max_{ij} |T_{ij}|$$

INTERFACE isDeviatoric
  MODULE PURE FUNCTION Rank2Tensor_IsDeviatoric(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION Rank2Tensor_IsDeviatoric
END INTERFACE isDeviatoric

!----------------------------------------------------------------------------
!                                                     Inv@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Inverse of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!   invObj : Rank2Tensor_ - inverse (output)
!
! Formula:
!   $$T^{-1} T = I$$

INTERFACE Inv
  MODULE PURE SUBROUTINE Rank2Tensor_Inv(obj, invObj)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    CLASS(Rank2Tensor_), INTENT(INOUT) :: invObj
  END SUBROUTINE Rank2Tensor_Inv
END INTERFACE Inv

!----------------------------------------------------------------------------
!                                               Transpose@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Transpose of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = T_{ji}$$

INTERFACE TRANSPOSE
  MODULE PURE FUNCTION Rank2Tensor_Transpose(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Transpose
END INTERFACE TRANSPOSE

!----------------------------------------------------------------------------
!                                                     Sym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Symmetric part of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : Rank2Tensor_ - symmetric tensor
!
! Formula:
!   $$\mathrm{sym}(T) = \frac{1}{2}\left(T + T^{T}\right)$$

INTERFACE Sym
  MODULE PURE FUNCTION Rank2Tensor_Sym(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Sym
END INTERFACE Sym

!----------------------------------------------------------------------------
!                                                 SkewSym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Skew-symmetric part of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : Rank2Tensor_ - skew-symmetric tensor
!
! Formula:
!   $$\mathrm{skew}(T) = \frac{1}{2}\left(T - T^{T}\right)$$

INTERFACE SkewSym
  MODULE PURE FUNCTION Rank2Tensor_SkewSym(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_SkewSym
END INTERFACE SkewSym

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Display the content of [[Rank2Tensor_]]
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!   msg : CHARACTER(*) - header message
!   unitNo : INTEGER(I4B), OPTIONAL - output unit, default is stdout

INTERFACE Display
  MODULE SUBROUTINE Rank2Tensor_Display(obj, msg, unitNo)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE Rank2Tensor_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                        Trace@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Trace of a power of the tensor
!
! Description:
!   Returns $\mathrm{tr}(T^n)$ where $n$ is `power` (default 1). For
!   `power < 1` the result is $\mathrm{tr}(I) = 3$.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!   power : INTEGER(I4B), OPTIONAL - power n, default is 1
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\mathrm{tr}(T) = T_{ii},\quad \mathrm{tr}(T^2) = T_{ij} T_{ji},
!   \quad \mathrm{tr}(T^3) = T_{ij} T_{jk} T_{ki}$$

INTERFACE Trace
  MODULE PURE FUNCTION Rank2Tensor_Trace(obj, power) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: power
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_Trace
END INTERFACE Trace

!----------------------------------------------------------------------------
!                                                           J2@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Second invariant of the deviatoric part
!
! Description:
!   If `isDeviatoric` is true, then the tensor is assumed to be
!   traceless. Otherwise the deviatoric part is used implicitly.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!   isDeviatoric : LOGICAL(LGT), OPTIONAL - default is false
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$J_2 = \frac{1}{2}\mathrm{tr}\left(\mathrm{dev}(T)^2\right)$$

INTERFACE J2
  MODULE PURE FUNCTION Rank2Tensor_J2(obj, isDeviatoric) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isDeviatoric
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_J2
END INTERFACE J2

!----------------------------------------------------------------------------
!                                                           J3@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Third invariant of the deviatoric part
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!   isDeviatoric : LOGICAL(LGT), OPTIONAL - default is false
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$J_3 = \det\left(\mathrm{dev}(T)\right)$$

INTERFACE J3
  MODULE PURE FUNCTION Rank2Tensor_J3(obj, isDeviatoric) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isDeviatoric
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_J3
END INTERFACE J3

!----------------------------------------------------------------------------
!                                                          Det@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Determinant of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\det(T) = \epsilon_{ijk} T_{1i} T_{2j} T_{3k}$$

INTERFACE Det
  MODULE PURE FUNCTION Rank2Tensor_Det(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_Det
END INTERFACE Det

!----------------------------------------------------------------------------
!                                                    LodeAngle@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Lode angle from the invariants J2 and J3
!
! Description:
!   `lodeType` is `SineLode` or `CosineLode`. Any other value is treated
!   as `CosineLode`. If $J_2 \le 0$, then the result is zero.
!
! Arguments:
!   lodeType : INTEGER(I4B) - `SineLode` or `CosineLode`
!   J2 : REAL(DFP) - second deviatoric invariant
!   J3 : REAL(DFP) - third deviatoric invariant
!
! Returns:
!   ans : REAL(DFP) - Lode angle in radians
!
! Formula:
!   $$\cos 3\theta = \frac{3\sqrt{3}}{2}\frac{J_3}{J_2^{3/2}},\quad
!   \sin 3\theta = -\frac{3\sqrt{3}}{2}\frac{J_3}{J_2^{3/2}}$$

INTERFACE LodeAngle
  MODULE PURE FUNCTION Rank2Tensor_LodeAngle1(lodeType, J2, J3) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: lodeType
    REAL(DFP), INTENT(IN) :: J2
    REAL(DFP), INTENT(IN) :: J3
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_LodeAngle1
END INTERFACE LodeAngle

!----------------------------------------------------------------------------
!                                                    LodeAngle@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Lode angle of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!   lodeType : INTEGER(I4B) - `SineLode` or `CosineLode`
!   isDeviatoric : LOGICAL(LGT), OPTIONAL - default is false
!
! Returns:
!   ans : REAL(DFP) - Lode angle in radians
!
! Formula:
!   $$\cos 3\theta = \frac{3\sqrt{3}}{2}\frac{J_3}{J_2^{3/2}}$$

INTERFACE LodeAngle
  MODULE PURE FUNCTION Rank2Tensor_LodeAngle2(obj, lodeType, &
                                              isDeviatoric) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: lodeType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isDeviatoric
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_LodeAngle2
END INTERFACE LodeAngle

!----------------------------------------------------------------------------
!                                                    Isotropic@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Isotropic (hydrostatic) part of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : Rank2Tensor_ - symmetric tensor
!
! Formula:
!   $$\mathrm{iso}(T) = \frac{1}{3}\mathrm{tr}(T)\, I$$

INTERFACE Isotropic
  MODULE PURE FUNCTION Rank2Tensor_Isotropic(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Isotropic
END INTERFACE Isotropic

INTERFACE Iso
  MODULE PROCEDURE Rank2Tensor_Isotropic
END INTERFACE Iso

!----------------------------------------------------------------------------
!                                                   Deviatoric@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Deviatoric part of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : Rank2Tensor_ - traceless tensor, symmetric if obj is symmetric
!
! Formula:
!   $$\mathrm{dev}(T) = T - \frac{1}{3}\mathrm{tr}(T)\, I$$

INTERFACE Deviatoric
  MODULE PURE FUNCTION Rank2Tensor_Deviatoric(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Deviatoric
END INTERFACE Deviatoric

INTERFACE Dev
  MODULE PROCEDURE Rank2Tensor_Deviatoric
END INTERFACE Dev

!----------------------------------------------------------------------------
!                                                   Invariants@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Principal invariants of a tensor
!
! Description:
!   If `isDeviatoric` is true, then the tensor is assumed to be
!   traceless and $I_1$ is set to zero. Both branches return the same
!   principal invariants.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!   isDeviatoric : LOGICAL(LGT), OPTIONAL - default is false
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - $[I_1, I_2, I_3]$
!
! Formula:
!   $$I_1 = \mathrm{tr}(T),\quad
!   I_2 = \frac{1}{2}\left(I_1^2 - \mathrm{tr}(T^2)\right),\quad
!   I_3 = \det(T)$$

INTERFACE Invariants
  MODULE PURE FUNCTION Rank2Tensor_Invariants(obj, isDeviatoric) &
    RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isDeviatoric
    REAL(DFP) :: ans(3)
  END FUNCTION Rank2Tensor_Invariants
END INTERFACE Invariants

!----------------------------------------------------------------------------
!                                                        Eigen@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Eigenvalues and eigenvectors of a tensor
!
! Description:
!   If the tensor is symmetric (`obj%isSym`), then the Jacobi method is
!   used and the imaginary parts are zero. Otherwise LAPACK GEEV is used.
!   A complex conjugate pair is returned in two consecutive columns.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!   QR : REAL(DFP), SHAPE(3,3) - real part of the eigenvectors (columns)
!   WR : REAL(DFP), SIZE(3) - real part of the eigenvalues
!   QI : REAL(DFP), SHAPE(3,3), OPTIONAL - imaginary part of eigenvectors
!   WI : REAL(DFP), SIZE(3), OPTIONAL - imaginary part of eigenvalues
!
! Formula:
!   $$T q_k = w_k q_k$$

INTERFACE Eigen
  MODULE SUBROUTINE Rank2Tensor_Eigen(obj, QR, WR, QI, WI)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: QR(3, 3)
    REAL(DFP), INTENT(INOUT) :: WR(3)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: QI(3, 3)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: WI(3)
  END SUBROUTINE Rank2Tensor_Eigen
END INTERFACE Eigen

!----------------------------------------------------------------------------
!                                                  PolarDecomp@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Polar decomposition of a tensor
!
! Description:
!   Computes the right and left polar decompositions of a tensor with
!   positive determinant. The method is Algorithm 3.1 of Higham and
!   Noferini (2015).
!
! Arguments:
!   obj : Rank2Tensor_ - tensor T with det(T) > 0
!   R : Rank2Tensor_ - rotation (output)
!   U : Rank2Tensor_ - right stretch, symmetric (output)
!   V : Rank2Tensor_ - left stretch, symmetric (output)
!
! Formula:
!   $$T = R U = V R,\quad R^{T} R = I$$

INTERFACE PolarDecomp
  MODULE PURE SUBROUTINE Rank2Tensor_PolarDecomp(obj, R, U, V)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    CLASS(Rank2Tensor_), INTENT(INOUT) :: R
    CLASS(Rank2Tensor_), INTENT(INOUT) :: U
    CLASS(Rank2Tensor_), INTENT(INOUT) :: V
  END SUBROUTINE Rank2Tensor_PolarDecomp
END INTERFACE PolarDecomp

!----------------------------------------------------------------------------
!                                            Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Double contraction of two tensors
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : Rank2Tensor_ - tensor B
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$A : B = A_{ij} B_{ij}$$

INTERFACE Contraction
  MODULE PURE FUNCTION Rank2Tensor_Contraction1(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_Contraction1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Double contraction of a tensor and a Voigt tensor
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : VoigtRank2Tensor_ - symmetric tensor B in Voigt notation
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$A : B = A_{ij} B_{ij}$$

INTERFACE Contraction
  MODULE PURE FUNCTION Rank2Tensor_Contraction2(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj2
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_Contraction2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Double contraction of a Voigt tensor and a tensor
!
! Arguments:
!   obj1 : VoigtRank2Tensor_ - symmetric tensor A in Voigt notation
!   obj2 : Rank2Tensor_ - tensor B
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$A : B = A_{ij} B_{ij}$$

INTERFACE Contraction
  MODULE PURE FUNCTION Rank2Tensor_Contraction3(obj1, obj2) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_Contraction3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Double contraction of two Voigt tensors
!
! Arguments:
!   obj1 : VoigtRank2Tensor_ - symmetric tensor A in Voigt notation
!   obj2 : VoigtRank2Tensor_ - symmetric tensor B in Voigt notation
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$A : B = A_{ij} B_{ij}$$

INTERFACE Contraction
  MODULE PURE FUNCTION Rank2Tensor_Contraction4(obj1, obj2) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj1
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj2
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_Contraction4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                       +@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Addition of two tensors
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : Rank2Tensor_ - tensor B
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if both A and B are symmetric
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} + B_{ij}$$

INTERFACE OPERATOR(+)
  MODULE PURE FUNCTION Rank2Tensor_AddObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_AddObjObj
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                                       +@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Addition of a tensor and a scalar
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : REAL(DFP) - scalar s
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} + s$$

INTERFACE OPERATOR(+)
  MODULE PURE FUNCTION Rank2Tensor_AddObjScalar(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_AddObjScalar
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                                       +@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Addition of a scalar and a tensor
!
! Arguments:
!   obj1 : REAL(DFP) - scalar s
!   obj2 : Rank2Tensor_ - tensor A
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s + A_{ij}$$

INTERFACE OPERATOR(+)
  MODULE PURE FUNCTION Rank2Tensor_AddScalarObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_AddScalarObj
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                                       -@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Subtraction of two tensors
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : Rank2Tensor_ - tensor B
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if both A and B are symmetric
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} - B_{ij}$$

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION Rank2Tensor_MinusObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_MinusObjObj
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                                       -@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Subtraction of a scalar from a tensor
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : REAL(DFP) - scalar s
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} - s$$

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION Rank2Tensor_MinusObjScalar(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_MinusObjScalar
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                                       -@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Subtraction of a tensor from a scalar
!
! Arguments:
!   obj1 : REAL(DFP) - scalar s
!   obj2 : Rank2Tensor_ - tensor A
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s - A_{ij}$$

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION Rank2Tensor_MinusScalarObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_MinusScalarObj
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                                       *@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Element-wise (Hadamard) product of two tensors
!
! Description:
!   This is not the single contraction. Use `MATMUL` for $A \cdot B$.
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : Rank2Tensor_ - tensor B
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if both A and B are symmetric
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} B_{ij}$$

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION Rank2Tensor_TimesObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_TimesObjObj
END INTERFACE OPERATOR(*)

!----------------------------------------------------------------------------
!                                                       *@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Tensor times scalar
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : REAL(DFP) - scalar s
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s A_{ij}$$

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION Rank2Tensor_TimesObjScalar(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_TimesObjScalar
END INTERFACE OPERATOR(*)

!----------------------------------------------------------------------------
!                                                       *@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Scalar times tensor
!
! Arguments:
!   obj1 : REAL(DFP) - scalar s
!   obj2 : Rank2Tensor_ - tensor A
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s A_{ij}$$

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION Rank2Tensor_TimesScalarObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_TimesScalarObj
END INTERFACE OPERATOR(*)

!----------------------------------------------------------------------------
!                                                       /@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Element-wise division of two tensors
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : Rank2Tensor_ - tensor B
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if both A and B are symmetric
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} / B_{ij}$$

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION Rank2Tensor_DivObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_DivObjObj
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                       /@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Tensor divided by scalar
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : REAL(DFP) - scalar s
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} / s$$

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION Rank2Tensor_DivObjScalar(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_DivObjScalar
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                       /@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Scalar divided element-wise by a tensor
!
! Arguments:
!   obj1 : REAL(DFP) - scalar s
!   obj2 : Rank2Tensor_ - tensor A
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s / A_{ij}$$

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION Rank2Tensor_DivScalarObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_DivScalarObj
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                   MATMUL@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Single contraction of two tensors
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : Rank2Tensor_ - tensor B
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$(A \cdot B)_{ij} = A_{ik} B_{kj}$$

INTERFACE MATMUL
  MODULE PURE FUNCTION Rank2Tensor_MatmulObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_MatmulObjObj
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                   MATMUL@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Tensor times vector
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : REAL(DFP), SIZE(3) - vector v
!
! Returns:
!   ans : REAL(DFP), SIZE(3)
!
! Formula:
!   $$(A v)_{i} = A_{ij} v_{j}$$

INTERFACE MATMUL
  MODULE PURE FUNCTION Rank2Tensor_MatmulObjVec(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    REAL(DFP), INTENT(IN) :: obj2(3)
    REAL(DFP) :: ans(3)
  END FUNCTION Rank2Tensor_MatmulObjVec
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                   MATMUL@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Vector times tensor
!
! Arguments:
!   obj1 : REAL(DFP), SIZE(3) - vector v
!   obj2 : Rank2Tensor_ - tensor A
!
! Returns:
!   ans : REAL(DFP), SIZE(3)
!
! Formula:
!   $$(v A)_{j} = v_{i} A_{ij}$$

INTERFACE MATMUL
  MODULE PURE FUNCTION Rank2Tensor_MatmulVecObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1(3)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    REAL(DFP) :: ans(3)
  END FUNCTION Rank2Tensor_MatmulVecObj
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                   Pullback@PullbackMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Pull back of a tensor to the reference configuration
!
! Description:
!   `indx1` and `indx2` describe the variance of the first and second
!   index. Each is one of `"CONTRAVAR"`, `"COVAR"` or `"NA"` (no
!   transformation). An unknown value is treated as `"NA"`.
!
! Arguments:
!   T : Rank2Tensor_ - spatial tensor
!   F : Rank2Tensor_ - deformation gradient
!   indx1 : CHARACTER(*) - variance of the first index
!   indx2 : CHARACTER(*) - variance of the second index
!
! Returns:
!   ans : Rank2Tensor_ - material tensor
!
! Formula:
!   $$\text{CONTRAVAR, CONTRAVAR}: F^{-1} T F^{-T},\quad
!   \text{COVAR, COVAR}: F^{T} T F$$

INTERFACE Pullback
  MODULE PURE FUNCTION Rank2Tensor_Pullback1(T, F, indx1, indx2) &
    RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    CLASS(Rank2Tensor_), INTENT(IN) :: F
    CHARACTER(*), INTENT(IN) :: indx1
    CHARACTER(*), INTENT(IN) :: indx2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Pullback1
END INTERFACE Pullback

!----------------------------------------------------------------------------
!                                                   Pullback@PullbackMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Pull back of a vector to the reference configuration
!
! Arguments:
!   vec : REAL(DFP), SIZE(3) - spatial vector
!   F : Rank2Tensor_ - deformation gradient
!   indx1 : CHARACTER(*) - `"CONTRAVAR"`, `"COVAR"` or `"NA"`
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - material vector
!
! Formula:
!   $$\text{CONTRAVAR}: F^{-1} v,\quad \text{COVAR}: F^{T} v$$

INTERFACE Pullback
  MODULE PURE FUNCTION Rank2Tensor_Pullback2(vec, F, indx1) RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(3)
    CLASS(Rank2Tensor_), INTENT(IN) :: F
    CHARACTER(*), INTENT(IN) :: indx1
    REAL(DFP) :: ans(3)
  END FUNCTION Rank2Tensor_Pullback2
END INTERFACE Pullback

!----------------------------------------------------------------------------
!                                             PushForward@PushForwardMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Push forward of a tensor to the current configuration
!
! Description:
!   `indx1` and `indx2` describe the variance of the first and second
!   index. Each is one of `"CONTRAVAR"`, `"COVAR"` or `"NA"` (no
!   transformation). An unknown value is treated as `"NA"`.
!
! Arguments:
!   T : Rank2Tensor_ - material tensor
!   F : Rank2Tensor_ - deformation gradient
!   indx1 : CHARACTER(*) - variance of the first index
!   indx2 : CHARACTER(*) - variance of the second index
!
! Returns:
!   ans : Rank2Tensor_ - spatial tensor
!
! Formula:
!   $$\text{CONTRAVAR, CONTRAVAR}: F T F^{T},\quad
!   \text{COVAR, COVAR}: F^{-T} T F^{-1}$$

INTERFACE PushForward
  MODULE PURE FUNCTION Rank2Tensor_PushForward1(T, F, indx1, indx2) &
    RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    CLASS(Rank2Tensor_), INTENT(IN) :: F
    CHARACTER(*), INTENT(IN) :: indx1
    CHARACTER(*), INTENT(IN) :: indx2
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_PushForward1
END INTERFACE PushForward

!----------------------------------------------------------------------------
!                                             PushForward@PushForwardMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Push forward of a vector to the current configuration
!
! Arguments:
!   vec : REAL(DFP), SIZE(3) - material vector
!   F : Rank2Tensor_ - deformation gradient
!   indx1 : CHARACTER(*) - `"CONTRAVAR"`, `"COVAR"` or `"NA"`
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - spatial vector
!
! Formula:
!   $$\text{CONTRAVAR}: F v,\quad \text{COVAR}: F^{-T} v$$

INTERFACE PushForward
  MODULE PURE FUNCTION Rank2Tensor_PushForward2(vec, F, indx1) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(3)
    CLASS(Rank2Tensor_), INTENT(IN) :: F
    CHARACTER(*), INTENT(IN) :: indx1
    REAL(DFP) :: ans(3)
  END FUNCTION Rank2Tensor_PushForward2
END INTERFACE PushForward

!----------------------------------------------------------------------------
!                                                    Otimes@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Dyadic product of two vectors
!
! Arguments:
!   a : REAL(DFP), SIZE(3) - vector a
!   b : REAL(DFP), SIZE(3) - vector b
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$(a \otimes b)_{ij} = a_{i} b_{j}$$

INTERFACE Otimes
  MODULE PURE FUNCTION Rank2Tensor_OtimesVecVec(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(3)
    REAL(DFP), INTENT(IN) :: b(3)
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_OtimesVecVec
END INTERFACE Otimes

!----------------------------------------------------------------------------
!                                                    Otimes@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Dyadic product of two rank-2 tensors
!
! Arguments:
!   obj1 : Rank2Tensor_ - tensor A
!   obj2 : Rank2Tensor_ - tensor B
!
! Returns:
!   ans : REAL(DFP), SHAPE(3,3,3,3) - rank-4 tensor
!
! Formula:
!   $$(A \otimes B)_{ijkl} = A_{ij} B_{kl}$$

INTERFACE Otimes
  MODULE PURE FUNCTION Rank2Tensor_OtimesObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    REAL(DFP) :: ans(3, 3, 3, 3)
  END FUNCTION Rank2Tensor_OtimesObjObj
END INTERFACE Otimes

!----------------------------------------------------------------------------
!                                           PrincipalValues@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Principal values of a tensor
!
! Description:
!   Returns the real parts of the eigenvalues sorted in descending order.
!   For a symmetric tensor these are the principal values.
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - $\lambda_1 \ge \lambda_2 \ge \lambda_3$
!
! Formula:
!   $$\det(T - \lambda I) = 0$$

INTERFACE PrincipalValues
  MODULE FUNCTION Rank2Tensor_PrincipalValues(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    REAL(DFP) :: ans(3)
  END FUNCTION Rank2Tensor_PrincipalValues
END INTERFACE PrincipalValues

!----------------------------------------------------------------------------
!                                            SpectralRadius@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Spectral radius of a tensor
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : REAL(DFP) - largest modulus of the (complex) eigenvalues
!
! Formula:
!   $$\rho(T) = \max_{k} |\lambda_k|$$

INTERFACE SpectralRadius
  MODULE FUNCTION Rank2Tensor_SpectralRadius(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_SpectralRadius
END INTERFACE SpectralRadius

!----------------------------------------------------------------------------
!                                              RotationPart@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rotation part of the polar decomposition
!
! Arguments:
!   obj : Rank2Tensor_ - tensor T with det(T) > 0
!
! Returns:
!   ans : Rank2Tensor_ - proper orthogonal tensor R
!
! Formula:
!   $$T = R U,\quad R^{T} R = I,\quad \det R = 1$$

INTERFACE RotationPart
  MODULE PURE FUNCTION Rank2Tensor_RotationPart(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_RotationPart
END INTERFACE RotationPart

!----------------------------------------------------------------------------
!                                                       Exp@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Exponential of a tensor
!
! Description:
!   Uses scaling and squaring with a diagonal Pade approximant of order
!   6 (Moler and Van Loan, 2003).
!
! Arguments:
!   obj : Rank2Tensor_ - tensor
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if obj is symmetric
!
! Formula:
!   $$\exp(T) = \sum_{k=0}^{\infty} \frac{T^{k}}{k!}$$

INTERFACE Exp
  MODULE PURE FUNCTION Rank2Tensor_Exp(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Exp
END INTERFACE Exp

!----------------------------------------------------------------------------
!                            DeformationGradient@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a [[DeformationGradient_]]
!
! Arguments:
!   obj : Rank2Tensor_, OPTIONAL - components; if absent F is zero
!
! Returns:
!   ans : DeformationGradient_
!
! Formula:
!   $$F = \frac{\partial x}{\partial X}$$

INTERFACE DeformationGradient
  MODULE PURE FUNCTION Rank2Tensor_DeformationGradient1(obj) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: obj
    TYPE(DeformationGradient_) :: ans
  END FUNCTION Rank2Tensor_DeformationGradient1
END INTERFACE DeformationGradient

!----------------------------------------------------------------------------
!                            DeformationGradient@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[DeformationGradient_]] from a 3x3 matrix
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - components of F
!
! Returns:
!   ans : DeformationGradient_
!
! Formula:
!   $$F_{ij} = \mathrm{mat}_{ij}$$

INTERFACE DeformationGradient
  MODULE PURE FUNCTION Rank2Tensor_DeformationGradient2(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    TYPE(DeformationGradient_) :: ans
  END FUNCTION Rank2Tensor_DeformationGradient2
END INTERFACE DeformationGradient

!----------------------------------------------------------------------------
!                    DeformationGradient_Pointer@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a pointer to a new [[DeformationGradient_]]
!
! Arguments:
!   obj : Rank2Tensor_, OPTIONAL - components; if absent F is zero
!
! Returns:
!   ans : CLASS(DeformationGradient_), POINTER - caller must deallocate
!
! Formula:
!   $$F = \frac{\partial x}{\partial X}$$

INTERFACE DeformationGradient_Pointer
  MODULE PURE FUNCTION Rank2Tensor_DeformationGradientPtr1(obj) &
    RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: obj
    CLASS(DeformationGradient_), POINTER :: ans
  END FUNCTION Rank2Tensor_DeformationGradientPtr1
END INTERFACE DeformationGradient_Pointer

!----------------------------------------------------------------------------
!                    DeformationGradient_Pointer@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[DeformationGradient_]]
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - components of F
!
! Returns:
!   ans : CLASS(DeformationGradient_), POINTER - caller must deallocate
!
! Formula:
!   $$F_{ij} = \mathrm{mat}_{ij}$$

INTERFACE DeformationGradient_Pointer
  MODULE PURE FUNCTION Rank2Tensor_DeformationGradientPtr2(mat) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    CLASS(DeformationGradient_), POINTER :: ans
  END FUNCTION Rank2Tensor_DeformationGradientPtr2
END INTERFACE DeformationGradient_Pointer

!----------------------------------------------------------------------------
!                                       Jacobian@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Volume ratio of a deformation gradient
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$J = \det F$$

INTERFACE Jacobian
  MODULE PURE FUNCTION Rank2Tensor_Jacobian(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_Jacobian
END INTERFACE Jacobian

!----------------------------------------------------------------------------
!                                   RightStretch@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Right stretch tensor of a deformation gradient
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : Rank2Tensor_ - symmetric positive definite U
!
! Formula:
!   $$F = R U,\quad U^{2} = F^{T} F$$

INTERFACE RightStretch
  MODULE PURE FUNCTION Rank2Tensor_RightStretch(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_RightStretch
END INTERFACE RightStretch

!----------------------------------------------------------------------------
!                                    LeftStretch@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Left stretch tensor of a deformation gradient
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : Rank2Tensor_ - symmetric positive definite V
!
! Formula:
!   $$F = V R,\quad V^{2} = F F^{T}$$

INTERFACE LeftStretch
  MODULE PURE FUNCTION Rank2Tensor_LeftStretch(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_LeftStretch
END INTERFACE LeftStretch

!----------------------------------------------------------------------------
!                                       Rotation@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rotation tensor of a deformation gradient
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : Rank2Tensor_ - proper orthogonal R
!
! Formula:
!   $$F = R U = V R$$

INTERFACE Rotation
  MODULE PURE FUNCTION Rank2Tensor_Rotation(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_Rotation
END INTERFACE Rotation

!----------------------------------------------------------------------------
!                               PrincipalStretch@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Principal stretches of a deformation gradient
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - stretches in descending order
!
! Formula:
!   $$\lambda_k = \sqrt{\mathrm{eig}_k(F^{T} F)}$$

INTERFACE PrincipalStretch
  MODULE PURE FUNCTION Rank2Tensor_PrincipalStretch(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    REAL(DFP) :: ans(3)
  END FUNCTION Rank2Tensor_PrincipalStretch
END INTERFACE PrincipalStretch

!----------------------------------------------------------------------------
!                                    LeftCauchyGreen@CauchyGreenMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a [[LeftCauchyGreen_]] tensor
!
! Description:
!   If `F` is present it is used, else if `V` is present it is used,
!   otherwise the identity is returned.
!
! Arguments:
!   F : Rank2Tensor_, OPTIONAL - deformation gradient
!   V : Rank2Tensor_, OPTIONAL - left stretch tensor
!
! Returns:
!   ans : LeftCauchyGreen_ - symmetric tensor
!
! Formula:
!   $$b = F F^{T} = V^{2}$$

INTERFACE LeftCauchyGreen
  MODULE PURE FUNCTION Rank2Tensor_LeftCauchyGreen(F, V) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: F
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: V
    TYPE(LeftCauchyGreen_) :: ans
  END FUNCTION Rank2Tensor_LeftCauchyGreen
END INTERFACE LeftCauchyGreen

!----------------------------------------------------------------------------
!                            LeftCauchyGreen_Pointer@CauchyGreenMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a pointer to a new [[LeftCauchyGreen_]] tensor
!
! Arguments:
!   F : Rank2Tensor_, OPTIONAL - deformation gradient
!   V : Rank2Tensor_, OPTIONAL - left stretch tensor
!
! Returns:
!   ans : CLASS(LeftCauchyGreen_), POINTER - caller must deallocate
!
! Formula:
!   $$b = F F^{T} = V^{2}$$

INTERFACE LeftCauchyGreen_Pointer
  MODULE PURE FUNCTION Rank2Tensor_LeftCauchyGreenPtr(F, V) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: F
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: V
    CLASS(LeftCauchyGreen_), POINTER :: ans
  END FUNCTION Rank2Tensor_LeftCauchyGreenPtr
END INTERFACE LeftCauchyGreen_Pointer

!----------------------------------------------------------------------------
!                                   RightCauchyGreen@CauchyGreenMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a [[RightCauchyGreen_]] tensor
!
! Description:
!   If `F` is present it is used, else if `U` is present it is used,
!   otherwise the identity is returned.
!
! Arguments:
!   F : Rank2Tensor_, OPTIONAL - deformation gradient
!   U : Rank2Tensor_, OPTIONAL - right stretch tensor
!
! Returns:
!   ans : RightCauchyGreen_ - symmetric tensor
!
! Formula:
!   $$C = F^{T} F = U^{2}$$

INTERFACE RightCauchyGreen
  MODULE PURE FUNCTION Rank2Tensor_RightCauchyGreen(F, U) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: F
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: U
    TYPE(RightCauchyGreen_) :: ans
  END FUNCTION Rank2Tensor_RightCauchyGreen
END INTERFACE RightCauchyGreen

!----------------------------------------------------------------------------
!                           RightCauchyGreen_Pointer@CauchyGreenMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a pointer to a new [[RightCauchyGreen_]] tensor
!
! Arguments:
!   F : Rank2Tensor_, OPTIONAL - deformation gradient
!   U : Rank2Tensor_, OPTIONAL - right stretch tensor
!
! Returns:
!   ans : CLASS(RightCauchyGreen_), POINTER - caller must deallocate
!
! Formula:
!   $$C = F^{T} F = U^{2}$$

INTERFACE RightCauchyGreen_Pointer
  MODULE PURE FUNCTION Rank2Tensor_RightCauchyGreenPtr(F, U) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: F
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: U
    CLASS(RightCauchyGreen_), POINTER :: ans
  END FUNCTION Rank2Tensor_RightCauchyGreenPtr
END INTERFACE RightCauchyGreen_Pointer

!----------------------------------------------------------------------------
!                                               GreenStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Green-Lagrange strain from a deformation gradient
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : GreenStrain_ - symmetric tensor
!
! Formula:
!   $$E = \frac{1}{2}\left(F^{T} F - I\right)$$

INTERFACE GreenStrain
  MODULE PURE FUNCTION Rank2Tensor_GreenStrain1(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(GreenStrain_) :: ans
  END FUNCTION Rank2Tensor_GreenStrain1
END INTERFACE GreenStrain

!----------------------------------------------------------------------------
!                                               GreenStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Green-Lagrange strain from the right Cauchy-Green tensor
!
! Arguments:
!   C : RightCauchyGreen_ - right Cauchy-Green tensor
!
! Returns:
!   ans : GreenStrain_ - symmetric tensor
!
! Formula:
!   $$E = \frac{1}{2}\left(C - I\right)$$

INTERFACE GreenStrain
  MODULE PURE FUNCTION Rank2Tensor_GreenStrain2(C) RESULT(ans)
    CLASS(RightCauchyGreen_), INTENT(IN) :: C
    TYPE(GreenStrain_) :: ans
  END FUNCTION Rank2Tensor_GreenStrain2
END INTERFACE GreenStrain

!----------------------------------------------------------------------------
!                                       GreenStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new Green-Lagrange strain from F
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : CLASS(GreenStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$E = \frac{1}{2}\left(F^{T} F - I\right)$$

INTERFACE GreenStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_GreenStrainPtr1(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    CLASS(GreenStrain_), POINTER :: ans
  END FUNCTION Rank2Tensor_GreenStrainPtr1
END INTERFACE GreenStrain_Pointer

!----------------------------------------------------------------------------
!                                       GreenStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new Green-Lagrange strain from C
!
! Arguments:
!   C : RightCauchyGreen_ - right Cauchy-Green tensor
!
! Returns:
!   ans : CLASS(GreenStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$E = \frac{1}{2}\left(C - I\right)$$

INTERFACE GreenStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_GreenStrainPtr2(C) RESULT(ans)
    CLASS(RightCauchyGreen_), INTENT(IN) :: C
    CLASS(GreenStrain_), POINTER :: ans
  END FUNCTION Rank2Tensor_GreenStrainPtr2
END INTERFACE GreenStrain_Pointer

!----------------------------------------------------------------------------
!                                             AlmansiStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Euler-Almansi strain from a deformation gradient
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : AlmansiStrain_ - symmetric tensor
!
! Formula:
!   $$e = \frac{1}{2}\left(I - F^{-T} F^{-1}\right)$$

INTERFACE AlmansiStrain
  MODULE PURE FUNCTION Rank2Tensor_AlmansiStrain1(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(AlmansiStrain_) :: ans
  END FUNCTION Rank2Tensor_AlmansiStrain1
END INTERFACE AlmansiStrain

!----------------------------------------------------------------------------
!                                             AlmansiStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Euler-Almansi strain from the left Cauchy-Green tensor
!
! Arguments:
!   B : LeftCauchyGreen_ - left Cauchy-Green tensor
!
! Returns:
!   ans : AlmansiStrain_ - symmetric tensor
!
! Formula:
!   $$e = \frac{1}{2}\left(I - b^{-1}\right)$$

INTERFACE AlmansiStrain
  MODULE PURE FUNCTION Rank2Tensor_AlmansiStrain2(B) RESULT(ans)
    CLASS(LeftCauchyGreen_), INTENT(IN) :: B
    TYPE(AlmansiStrain_) :: ans
  END FUNCTION Rank2Tensor_AlmansiStrain2
END INTERFACE AlmansiStrain

!----------------------------------------------------------------------------
!                                     AlmansiStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new Euler-Almansi strain from F
!
! Arguments:
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : CLASS(AlmansiStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$e = \frac{1}{2}\left(I - F^{-T} F^{-1}\right)$$

INTERFACE AlmansiStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_AlmansiStrainPtr1(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    CLASS(AlmansiStrain_), POINTER :: ans
  END FUNCTION Rank2Tensor_AlmansiStrainPtr1
END INTERFACE AlmansiStrain_Pointer

!----------------------------------------------------------------------------
!                                     AlmansiStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new Euler-Almansi strain from b
!
! Arguments:
!   B : LeftCauchyGreen_ - left Cauchy-Green tensor
!
! Returns:
!   ans : CLASS(AlmansiStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$e = \frac{1}{2}\left(I - b^{-1}\right)$$

INTERFACE AlmansiStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_AlmansiStrainPtr2(B) RESULT(ans)
    CLASS(LeftCauchyGreen_), INTENT(IN) :: B
    CLASS(AlmansiStrain_), POINTER :: ans
  END FUNCTION Rank2Tensor_AlmansiStrainPtr2
END INTERFACE AlmansiStrain_Pointer

!----------------------------------------------------------------------------
!                                               SmallStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Infinitesimal strain from a displacement gradient
!
! Arguments:
!   gradU : REAL(DFP), SHAPE(3,3) - displacement gradient
!     $\partial u_i / \partial x_j$
!
! Returns:
!   ans : SmallStrain_ - symmetric tensor
!
! Formula:
!   $$\varepsilon = \frac{1}{2}\left(\nabla u + \nabla u^{T}\right)$$

INTERFACE SmallStrain
  MODULE PURE FUNCTION Rank2Tensor_SmallStrain(gradU) RESULT(ans)
    REAL(DFP), INTENT(IN) :: gradU(3, 3)
    TYPE(SmallStrain_) :: ans
  END FUNCTION Rank2Tensor_SmallStrain
END INTERFACE SmallStrain

!----------------------------------------------------------------------------
!                                       SmallStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new infinitesimal strain
!
! Arguments:
!   gradU : REAL(DFP), SHAPE(3,3) - displacement gradient
!
! Returns:
!   ans : CLASS(SmallStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$\varepsilon = \frac{1}{2}\left(\nabla u + \nabla u^{T}\right)$$

INTERFACE SmallStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_SmallStrainPtr(gradU) RESULT(ans)
    REAL(DFP), INTENT(IN) :: gradU(3, 3)
    CLASS(SmallStrain_), POINTER :: ans
  END FUNCTION Rank2Tensor_SmallStrainPtr
END INTERFACE SmallStrain_Pointer

!----------------------------------------------------------------------------
!                                 VelocityGradient@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[VelocityGradient_]]
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - components $\partial v_i/\partial x_j$
!
! Returns:
!   ans : VelocityGradient_
!
! Formula:
!   $$L_{ij} = \frac{\partial v_i}{\partial x_j}$$

INTERFACE VelocityGradient
  MODULE PURE FUNCTION Rank2Tensor_VelocityGradient(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    TYPE(VelocityGradient_) :: ans
  END FUNCTION Rank2Tensor_VelocityGradient
END INTERFACE VelocityGradient

!----------------------------------------------------------------------------
!                         VelocityGradient_Pointer@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[VelocityGradient_]]
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - components $\partial v_i/\partial x_j$
!
! Returns:
!   ans : CLASS(VelocityGradient_), POINTER - caller must deallocate
!
! Formula:
!   $$L_{ij} = \frac{\partial v_i}{\partial x_j}$$

INTERFACE VelocityGradient_Pointer
  MODULE PURE FUNCTION Rank2Tensor_VelocityGradientPtr(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    CLASS(VelocityGradient_), POINTER :: ans
  END FUNCTION Rank2Tensor_VelocityGradientPtr
END INTERFACE VelocityGradient_Pointer

!----------------------------------------------------------------------------
!                                       StrainRate@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rate of deformation from a velocity gradient
!
! Arguments:
!   L : VelocityGradient_ - velocity gradient
!
! Returns:
!   ans : StrainRate_ - symmetric tensor
!
! Formula:
!   $$D = \frac{1}{2}\left(L + L^{T}\right)$$

INTERFACE StrainRate
  MODULE PURE FUNCTION Rank2Tensor_StrainRate(L) RESULT(ans)
    CLASS(VelocityGradient_), INTENT(IN) :: L
    TYPE(StrainRate_) :: ans
  END FUNCTION Rank2Tensor_StrainRate
END INTERFACE StrainRate

!----------------------------------------------------------------------------
!                               StrainRate_Pointer@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new rate of deformation tensor
!
! Arguments:
!   L : VelocityGradient_ - velocity gradient
!
! Returns:
!   ans : CLASS(StrainRate_), POINTER - caller must deallocate
!
! Formula:
!   $$D = \frac{1}{2}\left(L + L^{T}\right)$$

INTERFACE StrainRate_Pointer
  MODULE PURE FUNCTION Rank2Tensor_StrainRatePtr(L) RESULT(ans)
    CLASS(VelocityGradient_), INTENT(IN) :: L
    CLASS(StrainRate_), POINTER :: ans
  END FUNCTION Rank2Tensor_StrainRatePtr
END INTERFACE StrainRate_Pointer

!----------------------------------------------------------------------------
!                                    ContinuumSpin@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Spin tensor from a velocity gradient
!
! Arguments:
!   L : VelocityGradient_ - velocity gradient
!
! Returns:
!   ans : ContinuumSpin_ - skew-symmetric tensor
!
! Formula:
!   $$W = \frac{1}{2}\left(L - L^{T}\right)$$

INTERFACE ContinuumSpin
  MODULE PURE FUNCTION Rank2Tensor_ContinuumSpin(L) RESULT(ans)
    CLASS(VelocityGradient_), INTENT(IN) :: L
    TYPE(ContinuumSpin_) :: ans
  END FUNCTION Rank2Tensor_ContinuumSpin
END INTERFACE ContinuumSpin

!----------------------------------------------------------------------------
!                            ContinuumSpin_Pointer@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new spin tensor
!
! Arguments:
!   L : VelocityGradient_ - velocity gradient
!
! Returns:
!   ans : CLASS(ContinuumSpin_), POINTER - caller must deallocate
!
! Formula:
!   $$W = \frac{1}{2}\left(L - L^{T}\right)$$

INTERFACE ContinuumSpin_Pointer
  MODULE PURE FUNCTION Rank2Tensor_ContinuumSpinPtr(L) RESULT(ans)
    CLASS(VelocityGradient_), INTENT(IN) :: L
    CLASS(ContinuumSpin_), POINTER :: ans
  END FUNCTION Rank2Tensor_ContinuumSpinPtr
END INTERFACE ContinuumSpin_Pointer

!----------------------------------------------------------------------------
!                                                    Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[Stress_]] from a 3x3 matrix
!
! Description:
!   `isSym` of the result is true when `mat` is symmetric within a
!   relative tolerance of 1e-12.
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - stress components
!   stressType : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Returns:
!   ans : Stress_
!
! Formula:
!   $$\sigma_{ij} = \mathrm{mat}_{ij}$$

INTERFACE Stress
  MODULE PURE FUNCTION Rank2Tensor_Stress1(mat, stressType) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    INTEGER(I4B), INTENT(IN) :: stressType
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_Stress1
END INTERFACE Stress

!----------------------------------------------------------------------------
!                                                    Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[Stress_]] from a [[Rank2Tensor_]]
!
! Arguments:
!   T : Rank2Tensor_ - stress components
!   stressType : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Returns:
!   ans : Stress_
!
! Formula:
!   $$\sigma_{ij} = T_{ij}$$

INTERFACE Stress
  MODULE PURE FUNCTION Rank2Tensor_Stress2(T, stressType) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    INTEGER(I4B), INTENT(IN) :: stressType
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_Stress2
END INTERFACE Stress

!----------------------------------------------------------------------------
!                                                    Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a symmetric [[Stress_]] from a [[VoigtRank2Tensor_]]
!
! Arguments:
!   V : VoigtRank2Tensor_ - stress in Voigt notation
!   stressType : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Returns:
!   ans : Stress_ - symmetric
!
! Formula:
!   $$\sigma_{12} = \sigma_{21} = \mathrm{Scale}\, V_4$$

INTERFACE Stress
  MODULE PURE FUNCTION Rank2Tensor_Stress3(V, stressType) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    INTEGER(I4B), INTENT(IN) :: stressType
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_Stress3
END INTERFACE Stress

!----------------------------------------------------------------------------
!                                            Stress_Pointer@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[Stress_]] from a 3x3 matrix
!
! Arguments:
!   mat : REAL(DFP), SHAPE(3,3) - stress components
!   stressType : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Returns:
!   ans : CLASS(Stress_), POINTER - caller must deallocate
!
! Formula:
!   $$\sigma_{ij} = \mathrm{mat}_{ij}$$

INTERFACE Stress_Pointer
  MODULE PURE FUNCTION Rank2Tensor_StressPtr1(mat, stressType) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    INTEGER(I4B), INTENT(IN) :: stressType
    CLASS(Stress_), POINTER :: ans
  END FUNCTION Rank2Tensor_StressPtr1
END INTERFACE Stress_Pointer

!----------------------------------------------------------------------------
!                                            Stress_Pointer@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[Stress_]] from a tensor
!
! Arguments:
!   T : Rank2Tensor_ - stress components
!   stressType : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Returns:
!   ans : CLASS(Stress_), POINTER - caller must deallocate
!
! Formula:
!   $$\sigma_{ij} = T_{ij}$$

INTERFACE Stress_Pointer
  MODULE PURE FUNCTION Rank2Tensor_StressPtr2(T, stressType) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    INTEGER(I4B), INTENT(IN) :: stressType
    CLASS(Stress_), POINTER :: ans
  END FUNCTION Rank2Tensor_StressPtr2
END INTERFACE Stress_Pointer

!----------------------------------------------------------------------------
!                                            Stress_Pointer@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[Stress_]] from a Voigt tensor
!
! Arguments:
!   V : VoigtRank2Tensor_ - stress in Voigt notation
!   stressType : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Returns:
!   ans : CLASS(Stress_), POINTER - caller must deallocate
!
! Formula:
!   $$\sigma_{12} = \sigma_{21} = \mathrm{Scale}\, V_4$$

INTERFACE Stress_Pointer
  MODULE PURE FUNCTION Rank2Tensor_StressPtr3(V, stressType) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    INTEGER(I4B), INTENT(IN) :: stressType
    CLASS(Stress_), POINTER :: ans
  END FUNCTION Rank2Tensor_StressPtr3
END INTERFACE Stress_Pointer

!----------------------------------------------------------------------------
!                                                  Initiate@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a [[Stress_]] from a 3x3 matrix
!
! Arguments:
!   obj : Stress_ - stress to initiate
!   mat : REAL(DFP), SHAPE(3,3) - stress components
!   stressType : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Formula:
!   $$\sigma_{ij} = \mathrm{mat}_{ij}$$

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_InitiateStress(obj, mat, stressType)
    CLASS(Stress_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    INTEGER(I4B), INTENT(IN) :: stressType
  END SUBROUTINE Rank2Tensor_InitiateStress
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                             GetStressType@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns the stress measure of a [[Stress_]]
!
! Arguments:
!   obj : Stress_ - stress
!
! Returns:
!   ans : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Formula:
!   $$\mathrm{ans} = \mathrm{obj\%stressType}$$

INTERFACE GetStressType
  MODULE PURE FUNCTION Rank2Tensor_GetStressType(obj) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Rank2Tensor_GetStressType
END INTERFACE GetStressType

!----------------------------------------------------------------------------
!                                             SetStressType@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Sets the stress measure of a [[Stress_]]
!
! Description:
!   Only the label is changed, the components are not converted. Use
!   [[ConvertStress]] to change the measure.
!
! Arguments:
!   obj : Stress_ - stress
!   stressType : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Formula:
!   $$\mathrm{obj\%stressType} = \mathrm{stressType}$$

INTERFACE SetStressType
  MODULE PURE SUBROUTINE Rank2Tensor_SetStressType(obj, stressType)
    CLASS(Stress_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: stressType
  END SUBROUTINE Rank2Tensor_SetStressType
END INTERFACE SetStressType

!----------------------------------------------------------------------------
!                                             ConvertStress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Converts a stress to another stress measure
!
! Description:
!   The conversion goes through the Kirchhoff stress $\tau$. An unknown
!   stress measure (of obj or of the target) stops the program.
!
!   | measure   | to $\tau$             | from $\tau$            |
!   | --------- | --------------------- | ---------------------- |
!   | Cauchy    | $J\sigma$             | $\tau / J$             |
!   | Kirchhoff | $\tau$                | $\tau$                 |
!   | PK1       | $P F^{T}$             | $\tau F^{-T}$          |
!   | PK2       | $F S F^{T}$           | $F^{-1}\tau F^{-T}$    |
!   | Mandel    | $F^{-T} M F^{T}$      | $F^{T}\tau F^{-T}$     |
!
! Arguments:
!   obj : Stress_ - stress in any measure
!   F : DeformationGradient_ - deformation gradient
!   stressType : INTEGER(I4B) - target `StressMeasure*` constant
!
! Returns:
!   ans : Stress_ - stress in the target measure
!
! Formula:
!   $$\tau = J\sigma = P F^{T} = F S F^{T},\quad M = F^{T} P = C S$$

INTERFACE ConvertStress
  MODULE FUNCTION Rank2Tensor_ConvertStress(obj, F, stressType) &
    RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    CLASS(DeformationGradient_), INTENT(IN) :: F
    INTEGER(I4B), INTENT(IN) :: stressType
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_ConvertStress
END INTERFACE ConvertStress

!----------------------------------------------------------------------------
!                                              CauchyStress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Cauchy stress from any stress measure
!
! Arguments:
!   obj : Stress_ - stress in any measure
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : Stress_ - Cauchy stress
!
! Formula:
!   $$\sigma = J^{-1}\tau$$

INTERFACE CauchyStress
  MODULE FUNCTION Rank2Tensor_CauchyStress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_CauchyStress
END INTERFACE CauchyStress

!----------------------------------------------------------------------------
!                                           KirchhoffStress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Kirchhoff stress from any stress measure
!
! Arguments:
!   obj : Stress_ - stress in any measure
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : Stress_ - Kirchhoff stress
!
! Formula:
!   $$\tau = J\sigma$$

INTERFACE KirchhoffStress
  MODULE FUNCTION Rank2Tensor_KirchhoffStress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_KirchhoffStress
END INTERFACE KirchhoffStress

!----------------------------------------------------------------------------
!                                                 PK1Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: First Piola-Kirchhoff stress from any stress measure
!
! Arguments:
!   obj : Stress_ - stress in any measure
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : Stress_ - first Piola-Kirchhoff stress (not symmetric)
!
! Formula:
!   $$P = \tau F^{-T}$$

INTERFACE PK1Stress
  MODULE FUNCTION Rank2Tensor_PK1Stress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_PK1Stress
END INTERFACE PK1Stress

!----------------------------------------------------------------------------
!                                                 PK2Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Second Piola-Kirchhoff stress from any stress measure
!
! Arguments:
!   obj : Stress_ - stress in any measure
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : Stress_ - second Piola-Kirchhoff stress
!
! Formula:
!   $$S = F^{-1}\tau F^{-T}$$

INTERFACE PK2Stress
  MODULE FUNCTION Rank2Tensor_PK2Stress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_PK2Stress
END INTERFACE PK2Stress

!----------------------------------------------------------------------------
!                                              MandelStress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Mandel stress from any stress measure
!
! Description:
!   This is the "Eshelby-like" stress of the old Stress_Class.
!
! Arguments:
!   obj : Stress_ - stress in any measure
!   F : DeformationGradient_ - deformation gradient
!
! Returns:
!   ans : Stress_ - Mandel stress (not symmetric in general)
!
! Formula:
!   $$M = F^{T}\tau F^{-T} = F^{T} P = C S$$

INTERFACE MandelStress
  MODULE FUNCTION Rank2Tensor_MandelStress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    CLASS(DeformationGradient_), INTENT(IN) :: F
    TYPE(Stress_) :: ans
  END FUNCTION Rank2Tensor_MandelStress
END INTERFACE MandelStress

!----------------------------------------------------------------------------
!                                           MeanStress@StressInvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Mean (hydrostatic) stress
!
! Arguments:
!   obj : Rank2Tensor_ - stress
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\sigma_m = \frac{1}{3}\mathrm{tr}(\sigma)$$

INTERFACE MeanStress
  MODULE PURE FUNCTION Rank2Tensor_MeanStress(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_MeanStress
END INTERFACE MeanStress

!----------------------------------------------------------------------------
!                                     EquivalentStress@StressInvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: von Mises equivalent stress
!
! Arguments:
!   obj : Rank2Tensor_ - stress
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\bar{\sigma} = \sqrt{3 J_2}$$

INTERFACE EquivalentStress
  MODULE PURE FUNCTION Rank2Tensor_EquivalentStress(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION Rank2Tensor_EquivalentStress
END INTERFACE EquivalentStress

!----------------------------------------------------------------------------
!                                     HaighWestergaard@StressInvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Haigh-Westergaard coordinates of a stress
!
! Description:
!   `lodeType` is `SineLode` or `CosineLode` (default).
!
! Arguments:
!   obj : Rank2Tensor_ - stress
!   lodeType : INTEGER(I4B), OPTIONAL - Lode angle convention
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - $[\xi, \rho, \theta]$
!
! Formula:
!   $$\xi = \frac{I_1}{\sqrt{3}},\quad \rho = \sqrt{2 J_2},\quad
!   \theta = \mathrm{LodeAngle}(\sigma)$$

INTERFACE HaighWestergaard
  MODULE PURE FUNCTION Rank2Tensor_HaighWestergaard(obj, lodeType) &
    RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: lodeType
    REAL(DFP) :: ans(3)
  END FUNCTION Rank2Tensor_HaighWestergaard
END INTERFACE HaighWestergaard

!----------------------------------------------------------------------------
!                             MeanStressDerivative@StressDerivativeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivative of the mean stress with respect to stress
!
! Description:
!   For the Voigt form that is work conjugate to the engineering strain,
!   use `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.
!
! Arguments:
!   obj : Rank2Tensor_ - stress
!
! Returns:
!   ans : Rank2Tensor_ - symmetric
!
! Formula:
!   $$\frac{\partial \sigma_m}{\partial \sigma} = \frac{1}{3} I$$

INTERFACE MeanStressDerivative
  MODULE PURE FUNCTION Rank2Tensor_MeanStressDerivative(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_MeanStressDerivative
END INTERFACE MeanStressDerivative

!----------------------------------------------------------------------------
!                                     J2Derivative@StressDerivativeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivative of J2 with respect to stress
!
! Description:
!   The stress is assumed symmetric. For the Voigt form that is work
!   conjugate to the engineering strain, use
!   `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.
!
! Arguments:
!   obj : Rank2Tensor_ - stress
!
! Returns:
!   ans : Rank2Tensor_ - symmetric
!
! Formula:
!   $$\frac{\partial J_2}{\partial \sigma} = s = \mathrm{dev}(\sigma)$$

INTERFACE J2Derivative
  MODULE PURE FUNCTION Rank2Tensor_J2Derivative(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_J2Derivative
END INTERFACE J2Derivative

!----------------------------------------------------------------------------
!                                     J3Derivative@StressDerivativeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivative of J3 with respect to stress (Hill tensor)
!
! Description:
!   The stress is assumed symmetric. For the Voigt form that is work
!   conjugate to the engineering strain, use
!   `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.
!
! Arguments:
!   obj : Rank2Tensor_ - stress
!
! Returns:
!   ans : Rank2Tensor_ - symmetric
!
! Formula:
!   $$\frac{\partial J_3}{\partial \sigma} = s s - \frac{2}{3} J_2 I$$

INTERFACE J3Derivative
  MODULE PURE FUNCTION Rank2Tensor_J3Derivative(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_J3Derivative
END INTERFACE J3Derivative

!----------------------------------------------------------------------------
!                       EquivalentStressDerivative@StressDerivativeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivative of the equivalent stress with respect to stress
!
! Description:
!   The stress is assumed symmetric. The result is zero when the
!   equivalent stress is zero. For the Voigt form that is work conjugate
!   to the engineering strain, use
!   `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.
!
! Arguments:
!   obj : Rank2Tensor_ - stress
!
! Returns:
!   ans : Rank2Tensor_ - symmetric
!
! Formula:
!   $$\frac{\partial \bar{\sigma}}{\partial \sigma} =
!   \frac{3}{2\bar{\sigma}} s$$

INTERFACE EquivalentStressDerivative
  MODULE PURE FUNCTION Rank2Tensor_EquivalentStressDeriv(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    TYPE(Rank2Tensor_) :: ans
  END FUNCTION Rank2Tensor_EquivalentStressDeriv
END INTERFACE EquivalentStressDerivative

END MODULE Rank2Tensor_Method
