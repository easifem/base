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
! This module collects the methods for second order tensors in three
! dimensions. The implementations live in the following submodules:
!
! - `@ConstructorMethods`: Initiate, Rank2Tensor, assignment, special
!   tensors, Sym, SkewSym, Transpose, Inv
! - `@IOMethods`: Display
! - `@InvarMethods`: Trace, Det, invariants, Lode angle, Iso, Dev,
!   Eigen, PolarDecomp
! - `@ContractionMethods`: double contraction with Voigt tensors
! - `@OperatorMethods`: `+`, `-`, `*`, `/`, MATMUL
! - `@PullbackMethods`, `@PushForwardMethods`
! - `@UtilityMethods`: Otimes, PrincipalValues, SpectralRadius,
!   RotationPart, Exp
! - `@DeformationGradientMethods`: DeformationGradient, Jacobian,
!   RightStretch, LeftStretch, Rotation, PrincipalStretch
! - `@CauchyGreenMethods`: LeftCauchyGreen, RightCauchyGreen
! - `@StrainMethods`: GreenStrain, AlmansiStrain, SmallStrain
! - `@VelocityGradientMethods`: VelocityGradient, StrainRate,
!   ContinuumSpin
! - `@StressMethods`: Stress, GetStressType, SetStressType,
!   ConvertStress, CauchyStress, KirchhoffStress, PK1Stress, PK2Stress,
!   MandelStress
! - `@StressInvarMethods`: MeanStress, EquivalentStress,
!   HaighWestergaard
! - `@StressDerivativeMethods`: MeanStressDerivative, J2Derivative,
!   J3Derivative, EquivalentStressDerivative
!
! Note that `*` and `/` between two tensors act element-wise
! (Hadamard). Use `MATMUL` for the single contraction $A \cdot B$ and
! `Contraction` for the double contraction $A : B$.

MODULE Rank2Tensor_Method
USE BaseType, ONLY: AlmansiStrain_
USE BaseType, ONLY: ContinuumSpin_
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: GreenStrain_
USE BaseType, ONLY: LeftCauchyGreen_
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: RightCauchyGreen_
USE BaseType, ONLY: SmallStrain_
USE BaseType, ONLY: StrainRate_
USE BaseType, ONLY: Stress_
USE BaseType, ONLY: VelocityGradient_
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
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
! This routine is also used for `obj = obj2`.
!
! Formula:
!   $$\mathrm{obj} = \mathrm{obj2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Initiate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: from another Rank2Tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, obj2
!
!   CALL Initiate(obj2, S0, .TRUE.)
!   CALL Initiate(obj, obj2)
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "copy: T")
!   CALL OK(obj%isSym, "copy: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_Initiate1(obj, obj2)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! source tensor
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
! Formula:
!   $$T_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Initiate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: from a matrix, with and without the symmetry flag
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   CALL Initiate(obj, S0)
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "matrix: T")
!   CALL OK(.NOT. obj%isSym, "matrix: isSym defaults to false")
!   CALL Initiate(obj, S0, .TRUE.)
!   CALL OK(obj%isSym, "matrix: isSym is true")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_Initiate2(obj, mat, isSym)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! tensor components, SHAPE(3,3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSym
    !! symmetry flag, default is false
  END SUBROUTINE Rank2Tensor_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Initiate [[Rank2Tensor_]] from a [[VoigtRank2Tensor_]]
!
! The result is symmetric. This routine is also used for `obj = V`.
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{Scale}\, V_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Rank2Tensor_Method, ONLY: Initiate
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 3: from a strain-like Voigt tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   CALL Initiate(obj, VoigtRank2Tensor(S0, StrainTypeVoigt))
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "from Voigt: T")
!   CALL OK(obj%isSym, "from Voigt: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_Initiate3(obj, V)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    !! source tensor in Voigt notation
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
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Rank2Tensor_Method, ONLY: Initiate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: a Voigt tensor from a Rank2Tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   TYPE(VoigtRank2Tensor_) :: v
!
!   CALL Initiate(obj, S0, .TRUE.)
!   CALL Initiate(v, obj, StrainTypeVoigt)
!   CALL OK(MAXVAL(ABS(v%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
!                             10.0_DFP, 12.0_DFP])) .LT. tol, &
!           "to Voigt, strain-like: V")
!   CALL OK(ABS(v%scale - 0.5_DFP) .LT. tol, "to Voigt, strain-like: scale")
!   CALL Initiate(v, obj, StressTypeVoigt)
!   CALL OK(MAXVAL(ABS(v%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
!                             5.0_DFP, 6.0_DFP])) .LT. tol, &
!           "to Voigt, stress-like: V")
!   CALL OK(v%voigtType .EQ. StressTypeVoigt, "to Voigt: voigtType")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_Initiate4(obj, T, voigtType)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    !! source tensor
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! `StressTypeVoigt` or `StrainTypeVoigt`
  END SUBROUTINE Rank2Tensor_Initiate4
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                             Rank2Tensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Returns a [[Rank2Tensor_]] from a 3x3 matrix
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$T_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE Rank2Tensor_Method, ONLY: Display
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: from a matrix, with and without the symmetry flag
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(S0)
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "matrix: T")
!   CALL OK(.NOT. obj%isSym, "matrix: isSym defaults to false")
!   obj = Rank2Tensor(S0, .TRUE.)
!   CALL OK(obj%isSym, "matrix: isSym is true")
!   CALL Display(obj, "obj: ")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Rank2Tensor
  MODULE PURE FUNCTION Rank2Tensor_Constructor1(mat, isSym) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! tensor components, SHAPE(3,3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSym
    !! symmetry flag, default is false
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_Constructor1
END INTERFACE Rank2Tensor

!----------------------------------------------------------------------------
!                                             Rank2Tensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Returns a [[Rank2Tensor_]] from a [[VoigtRank2Tensor_]]
!
! Returns:
!   ans : Rank2Tensor_ - symmetric tensor
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{Scale}\, V_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: from a stress-like and a strain-like Voigt tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(VoigtRank2Tensor(S0, StressTypeVoigt))
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "Voigt stress-like: T")
!   CALL OK(obj%isSym, "Voigt stress-like: isSym")
!   obj = Rank2Tensor(VoigtRank2Tensor(S0, StrainTypeVoigt))
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "Voigt strain-like: T")
!   CALL OK(obj%isSym, "Voigt strain-like: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Rank2Tensor
  MODULE PURE FUNCTION Rank2Tensor_Constructor2(V) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    !! source tensor in Voigt notation
    TYPE(Rank2Tensor_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_Constructor2
END INTERFACE Rank2Tensor

!----------------------------------------------------------------------------
!                                     Rank2Tensor_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Returns a pointer to a new [[Rank2Tensor_]]
!
! Returns:
!   ans : CLASS(Rank2Tensor_), POINTER - caller must deallocate
!
! Formula:
!   $$T_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor_Pointer
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 3: pointer from a matrix and from a Voigt tensor
! BLOCK
!   CLASS(Rank2Tensor_), POINTER :: ptr
!
!   ptr => Rank2Tensor_Pointer(S0, .TRUE.)
!   CALL OK(MAXVAL(ABS(ptr%T - S0)) .LT. tol, "pointer, matrix: T")
!   CALL OK(ptr%isSym, "pointer, matrix: isSym")
!   DEALLOCATE (ptr)
!   ptr => Rank2Tensor_Pointer(VoigtRank2Tensor(S0, StrainTypeVoigt))
!   CALL OK(MAXVAL(ABS(ptr%T - S0)) .LT. tol, "pointer, Voigt: T")
!   CALL OK(ptr%isSym, "pointer, Voigt: isSym")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Rank2Tensor_Pointer
  MODULE PURE FUNCTION Rank2Tensor_Pointer1(mat, isSym) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! tensor components, SHAPE(3,3)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSym
    !! symmetry flag, default is false
    CLASS(Rank2Tensor_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_Pointer1
END INTERFACE Rank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                     Rank2Tensor_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Returns a pointer to a new [[Rank2Tensor_]]
!
! Returns:
!   ans : CLASS(Rank2Tensor_), POINTER - caller must deallocate
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{Scale}\, V_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor_Pointer
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 3: pointer from a matrix and from a Voigt tensor
! BLOCK
!   CLASS(Rank2Tensor_), POINTER :: ptr
!
!   ptr => Rank2Tensor_Pointer(S0, .TRUE.)
!   CALL OK(MAXVAL(ABS(ptr%T - S0)) .LT. tol, "pointer, matrix: T")
!   CALL OK(ptr%isSym, "pointer, matrix: isSym")
!   DEALLOCATE (ptr)
!   ptr => Rank2Tensor_Pointer(VoigtRank2Tensor(S0, StrainTypeVoigt))
!   CALL OK(MAXVAL(ABS(ptr%T - S0)) .LT. tol, "pointer, Voigt: T")
!   CALL OK(ptr%isSym, "pointer, Voigt: isSym")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Rank2Tensor_Pointer
  MODULE PURE FUNCTION Rank2Tensor_Pointer2(V) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    !! source tensor in Voigt notation
    CLASS(Rank2Tensor_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_Pointer2
END INTERFACE Rank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: [[Rank2Tensor_]] = Matrix(3,3)
!
! The resulting tensor is marked as not symmetric.
!
! Formula:
!   $$T_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: obj = mat and mat = obj
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3)
!
!   obj = S0
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "obj = mat: T")
!   CALL OK(.NOT. obj%isSym, "obj = mat: not symmetric")
!   mat = 0.0_DFP
!   mat = obj
!   CALL OK(MAXVAL(ABS(mat - S0)) .LT. tol, "mat = obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE Rank2Tensor_ObjEqMat(obj, mat)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    !! tensor (output)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! tensor components, SHAPE(3,3)
  END SUBROUTINE Rank2Tensor_ObjEqMat
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Matrix(3,3) = [[Rank2Tensor_]]
!
! Formula:
!   $$\mathrm{mat}_{ij} = T_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: obj = mat and mat = obj
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3)
!
!   obj = S0
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "obj = mat: T")
!   CALL OK(.NOT. obj%isSym, "obj = mat: not symmetric")
!   mat = 0.0_DFP
!   mat = obj
!   CALL OK(MAXVAL(ABS(mat - S0)) .LT. tol, "mat = obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE Rank2Tensor_MatEqObj(mat, obj)
    REAL(DFP), INTENT(INOUT) :: mat(3, 3)
    !! tensor components (output), SHAPE(3,3)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
  END SUBROUTINE Rank2Tensor_MatEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: [[VoigtRank2Tensor_]] = [[Rank2Tensor_]]
!
! The Voigt type of the result is `StressTypeVoigt`.
!
! Formula:
!   $$V_4 = \frac{1}{2}\left(T_{12} + T_{21}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressTypeVoigt
! USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 5: VoigtRank2Tensor_ = obj is stress-like
! BLOCK
!   TYPE(VoigtRank2Tensor_) :: v
!
!   v = Rank2Tensor(S0, .TRUE.)
!   CALL OK(MAXVAL(ABS(v%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
!                             5.0_DFP, 6.0_DFP])) .LT. tol, "Voigt = obj: V")
!   CALL OK(v%voigtType .EQ. StressTypeVoigt, "Voigt = obj: voigtType")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE Rank2Tensor_VoigtEqObj(V, obj)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: V
    !! voigt tensor (output)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
  END SUBROUTINE Rank2Tensor_VoigtEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                          IdentityTensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Set obj to the rank-2 identity tensor
!
! Formula:
!   $$T_{ij} = \delta_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: IdentityTensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: identity tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   CALL IdentityTensor(obj)
!   CALL OK(MAXVAL(ABS(obj%T - Eye3)) .LT. tol, "IdentityTensor: T")
!   CALL OK(obj%isSym, "IdentityTensor: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE IdentityTensor
  MODULE PURE SUBROUTINE Rank2Tensor_IdentityTensor(obj)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    !! tensor (output)
  END SUBROUTINE Rank2Tensor_IdentityTensor
END INTERFACE IdentityTensor

!----------------------------------------------------------------------------
!                                                 getOnes@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Set every component of obj to one
!
! Formula:
!   $$T_{ij} = 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: getOnes
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 2: all components equal to one
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   CALL getOnes(obj)
!   CALL OK(MAXVAL(ABS(obj%T - 1.0_DFP)) .LT. tol, "getOnes: T")
!   CALL OK(obj%isSym, "getOnes: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE getOnes
  MODULE PURE SUBROUTINE Rank2Tensor_GetOnes(obj)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    !! tensor (output)
  END SUBROUTINE Rank2Tensor_GetOnes
END INTERFACE getOnes

!----------------------------------------------------------------------------
!                                                getZeros@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Set every component of obj to zero
!
! Formula:
!   $$T_{ij} = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: getOnes
! USE Rank2Tensor_Method, ONLY: getZeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 3: all components equal to zero
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   CALL getOnes(obj)
!   CALL getZeros(obj)
!   CALL OK(MAXVAL(ABS(obj%T)) .LT. tol, "getZeros: T")
!   CALL OK(obj%isSym, "getZeros: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE getZeros
  MODULE PURE SUBROUTINE Rank2Tensor_GetZeros(obj)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    !! tensor (output)
  END SUBROUTINE Rank2Tensor_GetZeros
END INTERFACE getZeros

!----------------------------------------------------------------------------
!                                         IsotropicTensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Set obj to a second order isotropic tensor
!
! Formula:
!   $$T_{ij} = \lambda\, \delta_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: IsotropicTensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 4: isotropic tensor lambda * I
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   CALL IsotropicTensor(obj, 2.5_DFP)
!   CALL OK(MAXVAL(ABS(obj%T - 2.5_DFP * Eye3)) .LT. tol, &
!           "IsotropicTensor: T")
!   CALL OK(obj%isSym, "IsotropicTensor: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE IsotropicTensor
  MODULE PURE SUBROUTINE Rank2Tensor_IsotropicTensor(obj, lambda)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: obj
    !! tensor (output)
    REAL(DFP), INTENT(IN) :: lambda
    !! scaling of the identity
  END SUBROUTINE Rank2Tensor_IsotropicTensor
END INTERFACE IsotropicTensor

!----------------------------------------------------------------------------
!                                                   isSym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Returns the symmetry flag of the tensor
!
! Returns:
!   ans : LOGICAL(LGT) - value of `obj%isSym`
!
! Formula:
!   $$\mathrm{ans} = (T = T^{T})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: isSym
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: isSym returns the symmetry flag
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(A0)
!   CALL OK(.NOT. isSym(obj), "isSym: false by default")
!   obj = Rank2Tensor(Eye3, .TRUE.)
!   CALL OK(isSym(obj), "isSym: true when the flag is set")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE isSym
  MODULE PURE FUNCTION Rank2Tensor_IsSym(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    LOGICAL(LGT) :: ans
    !! value of `obj%isSym`
  END FUNCTION Rank2Tensor_IsSym
END INTERFACE isSym

!----------------------------------------------------------------------------
!                                            isDeviatoric@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Returns true if the tensor is traceless
!
! The trace is compared with a tolerance relative to the largest
! absolute component, so the test does not depend on the magnitude of
! the tensor.
!
! Returns:
!   ans : LOGICAL(LGT)
!
! Formula:
!   $$|T_{ii}| \le 10^{-12} \max_{ij} |T_{ij}|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: isDeviatoric
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: isDeviatoric checks the trace
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3)
!
!   obj = Rank2Tensor(A0)
!   CALL OK(.NOT. isDeviatoric(obj), "isDeviatoric: trace is 9")
!   mat = A0 - 3.0_DFP * Eye3
!   obj = Rank2Tensor(mat)
!   CALL OK(isDeviatoric(obj), "isDeviatoric: traceless")
!   obj = Rank2Tensor(1.0E+6_DFP * mat)
!   CALL OK(isDeviatoric(obj), "isDeviatoric: scale independent")
!   obj = Rank2Tensor(1.0E-6_DFP * Eye3)
!   CALL OK(.NOT. isDeviatoric(obj), "isDeviatoric: small identity")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE isDeviatoric
  MODULE PURE FUNCTION Rank2Tensor_IsDeviatoric(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    LOGICAL(LGT) :: ans
    !! LOGICAL(LGT)
  END FUNCTION Rank2Tensor_IsDeviatoric
END INTERFACE isDeviatoric

!----------------------------------------------------------------------------
!                                                     Inv@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Inverse of a tensor
!
! Formula:
!   $$T^{-1} T = I$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: Inv
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: inverse of a non-symmetric tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, invObj
!
!   obj = Rank2Tensor(A0)
!   CALL Inv(obj, invObj)
!   CALL OK(MAXVAL(ABS(MATMUL(invObj%T, obj%T) - Eye3)) .LT. tol, &
!           "Inv: inv(A) A = I")
!   CALL OK(MAXVAL(ABS(MATMUL(obj%T, invObj%T) - Eye3)) .LT. tol, &
!           "Inv: A inv(A) = I")
!   CALL OK(.NOT. invObj%isSym, "Inv: not symmetric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Inv
  MODULE PURE SUBROUTINE Rank2Tensor_Inv(obj, invObj)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    CLASS(Rank2Tensor_), INTENT(INOUT) :: invObj
    !! inverse (output)
  END SUBROUTINE Rank2Tensor_Inv
END INTERFACE Inv

!----------------------------------------------------------------------------
!                                               Transpose@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Transpose of a tensor
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = T_{ji}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: TRANSPOSE
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: transpose of a non-symmetric tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, ans
!
!   obj = Rank2Tensor(A0)
!   ans = TRANSPOSE(obj)
!   CALL OK(ABS(ans%T(1, 2) - 1.0_DFP) .LT. tol, "TRANSPOSE: T12")
!   CALL OK(ABS(ans%T(2, 1) + 1.0_DFP) .LT. tol, "TRANSPOSE: T21")
!   CALL OK(MAXVAL(ABS(ans%T - TRANSPOSE(A0))) .LT. tol, "TRANSPOSE: T")
!   CALL OK(.NOT. ans%isSym, "TRANSPOSE: isSym is kept")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE TRANSPOSE
  MODULE PURE FUNCTION Rank2Tensor_Transpose(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_Transpose
END INTERFACE TRANSPOSE

!----------------------------------------------------------------------------
!                                                     Sym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Symmetric part of a tensor
!
! Returns:
!   ans : Rank2Tensor_ - symmetric tensor
!
! Formula:
!   $$\mathrm{sym}(T) = \frac{1}{2}\left(T + T^{T}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: Sym
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: symmetric part
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, ans
!
!   obj = Rank2Tensor(A0)
!   ans = Sym(obj)
!   CALL OK(ABS(ans%T(1, 2)) .LT. tol, "Sym: T12")
!   CALL OK(ABS(ans%T(2, 3) - 0.875_DFP) .LT. tol, "Sym: T23")
!   CALL OK(MAXVAL(ABS(ans%T - TRANSPOSE(ans%T))) .LT. tol, &
!           "Sym: symmetric")
!   CALL OK(ans%isSym, "Sym: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Sym
  MODULE PURE FUNCTION Rank2Tensor_Sym(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    TYPE(Rank2Tensor_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_Sym
END INTERFACE Sym

!----------------------------------------------------------------------------
!                                                 SkewSym@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Skew-symmetric part of a tensor
!
! Returns:
!   ans : Rank2Tensor_ - skew-symmetric tensor
!
! Formula:
!   $$\mathrm{skew}(T) = \frac{1}{2}\left(T - T^{T}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: SkewSym
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: skew-symmetric part
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, ans
!
!   obj = Rank2Tensor(A0)
!   ans = SkewSym(obj)
!   CALL OK(ABS(ans%T(2, 1) - 1.0_DFP) .LT. tol, "SkewSym: T21")
!   CALL OK(MAXVAL(ABS(ans%T + TRANSPOSE(ans%T))) .LT. tol, &
!           "SkewSym: skew-symmetric")
!   CALL OK(.NOT. ans%isSym, "SkewSym: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE SkewSym
  MODULE PURE FUNCTION Rank2Tensor_SkewSym(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    TYPE(Rank2Tensor_) :: ans
    !! skew-symmetric tensor
  END FUNCTION Rank2Tensor_SkewSym
END INTERFACE SkewSym

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Display the content of [[Rank2Tensor_]]
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: ASSIGNMENT(=)
! USE Rank2Tensor_Method, ONLY: Display
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: from a matrix, with and without the symmetry flag
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(S0)
!   CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "matrix: T")
!   CALL OK(.NOT. obj%isSym, "matrix: isSym defaults to false")
!   obj = Rank2Tensor(S0, .TRUE.)
!   CALL OK(obj%isSym, "matrix: isSym is true")
!   CALL Display(obj, "obj: ")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Display
  MODULE SUBROUTINE Rank2Tensor_Display(obj, msg, unitNo)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    CHARACTER(*), INTENT(IN) :: msg
    !! header message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! output unit, default is stdout
  END SUBROUTINE Rank2Tensor_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                        Trace@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Trace of a power of the tensor
!
! Returns $\mathrm{tr}(T^n)$ where $n$ is `power` (default 1). For
! `power < 1` the result is $\mathrm{tr}(I) = 3$.
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\mathrm{tr}(T) = T_{ii},\quad \mathrm{tr}(T^2) = T_{ij} T_{ji},
!   \quad \mathrm{tr}(T^3) = T_{ij} T_{jk} T_{ki}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: Trace
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 1.0_DFP, &
!                                             0.0_DFP, 1.0_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: trace without power
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(A0)
!   CALL OK(ABS(Trace(obj) - 9.0_DFP) .LT. tol, "tr(A) = 9")
! END BLOCK
!
! ! Test 2: trace of the square and of the cube
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(A0)
!   CALL OK(ABS(Trace(obj, power=2) - 29.0_DFP) .LT. tol, "tr(A^2) = 29")
!   CALL OK(ABS(Trace(obj, power=3) - 105.0_DFP) .LT. tol, &
!           "tr(A^3) = 105")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Trace
  MODULE PURE FUNCTION Rank2Tensor_Trace(obj, power) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: power
    !! power n, default is 1
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_Trace
END INTERFACE Trace

!----------------------------------------------------------------------------
!                                                           J2@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Second invariant of the deviatoric part
!
! If `isDeviatoric` is true, then the tensor is assumed to be
! traceless. Otherwise the deviatoric part is used implicitly.
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$J_2 = \frac{1}{2}\mathrm{tr}\left(\mathrm{dev}(T)^2\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: Dev
! USE Rank2Tensor_Method, ONLY: J2
! USE Rank2Tensor_Method, ONLY: J3
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: uniaxial stress 3 plus a pressure, J2 = 3 and J3 = 2
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3)
!
!   mat = 5.0_DFP * Eye3
!   mat(1, 1) = mat(1, 1) + 3.0_DFP
!   obj = Rank2Tensor(mat, .TRUE.)
!   CALL OK(ABS(J2(obj) - 3.0_DFP) .LT. tol, "uniaxial: J2 = 3")
!   CALL OK(ABS(J3(obj) - 2.0_DFP) .LT. tol, "uniaxial: J3 = 2")
! END BLOCK
!
! ! Test 2: isDeviatoric with a traceless tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3)
!
!   mat = 5.0_DFP * Eye3
!   mat(1, 1) = mat(1, 1) + 3.0_DFP
!   obj = Dev(Rank2Tensor(mat, .TRUE.))
!   CALL OK(ABS(J2(obj, isDeviatoric=.TRUE.) - 3.0_DFP) .LT. tol, &
!           "deviatoric: J2 = 3")
!   CALL OK(ABS(J3(obj, isDeviatoric=.TRUE.) - 2.0_DFP) .LT. tol, &
!           "deviatoric: J3 = 2")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE J2
  MODULE PURE FUNCTION Rank2Tensor_J2(obj, isDeviatoric) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isDeviatoric
    !! default is false
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_J2
END INTERFACE J2

!----------------------------------------------------------------------------
!                                                           J3@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Third invariant of the deviatoric part
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$J_3 = \det\left(\mathrm{dev}(T)\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: J2
! USE Rank2Tensor_Method, ONLY: J3
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: uniaxial stress 3 plus a pressure, J2 = 3 and J3 = 2
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3)
!
!   mat = 5.0_DFP * Eye3
!   mat(1, 1) = mat(1, 1) + 3.0_DFP
!   obj = Rank2Tensor(mat, .TRUE.)
!   CALL OK(ABS(J2(obj) - 3.0_DFP) .LT. tol, "uniaxial: J2 = 3")
!   CALL OK(ABS(J3(obj) - 2.0_DFP) .LT. tol, "uniaxial: J3 = 2")
! END BLOCK
!
! ! Test 3: pure shear 3, J2 = 9 and J3 = 0
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3)
!
!   mat = 0.0_DFP
!   mat(1, 2) = 3.0_DFP
!   mat(2, 1) = 3.0_DFP
!   obj = Rank2Tensor(mat, .TRUE.)
!   CALL OK(ABS(J2(obj) - 9.0_DFP) .LT. tol, "shear: J2 = 9")
!   CALL OK(ABS(J3(obj)) .LT. tol, "shear: J3 = 0")
!   CALL OK(ABS(J2(obj, isDeviatoric=.TRUE.) - 9.0_DFP) .LT. tol, &
!           "shear deviatoric: J2 = 9")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE J3
  MODULE PURE FUNCTION Rank2Tensor_J3(obj, isDeviatoric) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isDeviatoric
    !! default is false
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_J3
END INTERFACE J3

!----------------------------------------------------------------------------
!                                                          Det@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Determinant of a tensor
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\det(T) = \epsilon_{ijk} T_{1i} T_{2j} T_{3k}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Det
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 1.0_DFP, &
!                                             0.0_DFP, 1.0_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: determinant of a non-symmetric tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(A0)
!   CALL OK(ABS(Det(obj) - 26.0_DFP) .LT. tol, "det(A) = 26")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Det
  MODULE PURE FUNCTION Rank2Tensor_Det(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_Det
END INTERFACE Det

!----------------------------------------------------------------------------
!                                                    LodeAngle@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Lode angle from the invariants J2 and J3
!
! `lodeType` is `SineLode` or `CosineLode`. Any other value is treated
! as `CosineLode`. If $J_2 \le 0$, then the result is zero.
!
! Returns:
!   ans : REAL(DFP) - Lode angle in radians
!
! Formula:
!   $$\cos 3\theta = \frac{3\sqrt{3}}{2}\frac{J_3}{J_2^{3/2}},\quad
!   \sin 3\theta = -\frac{3\sqrt{3}}{2}\frac{J_3}{J_2^{3/2}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: CosineLode
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: SineLode
! USE Rank2Tensor_Method, ONLY: LodeAngle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: pi = 4.0_DFP * ATAN(1.0_DFP)
!
! ! Test 1: from J2 and J3, uniaxial tension (J2 = 3, J3 = 2)
! BLOCK
!   CALL OK(ABS(LodeAngle(CosineLode, 3.0_DFP, 2.0_DFP)) .LT. tol, &
!           "J2, J3 uniaxial: cos form = 0")
!   CALL OK(ABS(LodeAngle(SineLode, 3.0_DFP, 2.0_DFP) + pi / 6.0_DFP) &
!           .LT. tol, "J2, J3 uniaxial: sin form = -pi/6")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE LodeAngle
  MODULE PURE FUNCTION Rank2Tensor_LodeAngle1(lodeType, J2, J3) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: lodeType
    !! `SineLode` or `CosineLode`
    REAL(DFP), INTENT(IN) :: J2
    !! second deviatoric invariant
    REAL(DFP), INTENT(IN) :: J3
    !! third deviatoric invariant
    REAL(DFP) :: ans
    !! lode angle in radians
  END FUNCTION Rank2Tensor_LodeAngle1
END INTERFACE LodeAngle

!----------------------------------------------------------------------------
!                                                    LodeAngle@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Lode angle of a tensor
!
! Returns:
!   ans : REAL(DFP) - Lode angle in radians
!
! Formula:
!   $$\cos 3\theta = \frac{3\sqrt{3}}{2}\frac{J_3}{J_2^{3/2}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: CosineLode
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE GlobalData, ONLY: SineLode
! USE Rank2Tensor_Method, ONLY: Dev
! USE Rank2Tensor_Method, ONLY: LodeAngle
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: pi = 4.0_DFP * ATAN(1.0_DFP)
!
! ! Test 3: from a tensor, uniaxial tension with a pressure
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3)
!
!   mat = 5.0_DFP * Eye3
!   mat(1, 1) = mat(1, 1) + 3.0_DFP
!   obj = Rank2Tensor(mat, .TRUE.)
!   CALL OK(ABS(LodeAngle(obj, CosineLode)) .LT. 1.0E-6_DFP, &
!           "tensor uniaxial: cos form = 0")
!   CALL OK(ABS(LodeAngle(obj, SineLode) + pi / 6.0_DFP) .LT. 1.0E-6_DFP, &
!           "tensor uniaxial: sin form = -pi/6")
!   obj = Dev(obj)
!   CALL OK(ABS(LodeAngle(obj, SineLode, isDeviatoric=.TRUE.) &
!               + pi / 6.0_DFP) .LT. 1.0E-6_DFP, &
!           "tensor deviatoric: sin form = -pi/6")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE LodeAngle
  MODULE PURE FUNCTION Rank2Tensor_LodeAngle2(obj, lodeType, &
                                              isDeviatoric) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    INTEGER(I4B), INTENT(IN) :: lodeType
    !! `SineLode` or `CosineLode`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isDeviatoric
    !! default is false
    REAL(DFP) :: ans
    !! lode angle in radians
  END FUNCTION Rank2Tensor_LodeAngle2
END INTERFACE LodeAngle

!----------------------------------------------------------------------------
!                                                    Isotropic@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Isotropic (hydrostatic) part of a tensor
!
! Returns:
!   ans : Rank2Tensor_ - symmetric tensor
!
! Formula:
!   $$\mathrm{iso}(T) = \frac{1}{3}\mathrm{tr}(T)\, I$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: Dev
! USE Rank2Tensor_Method, ONLY: Deviatoric
! USE Rank2Tensor_Method, ONLY: Iso
! USE Rank2Tensor_Method, ONLY: Isotropic
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 1.0_DFP, &
!                                             0.0_DFP, 1.0_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: Isotropic part is tr(A)/3 I and is symmetric
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, isoObj
!
!   obj = Rank2Tensor(A0)
!   isoObj = Isotropic(obj)
!   CALL OK(MAXVAL(ABS(isoObj%T - 3.0_DFP * Eye3)) .LT. tol, &
!           "Isotropic = 3 I")
!   CALL OK(isoObj%isSym, "Isotropic: symmetric")
! END BLOCK
!
! ! Test 4: Iso is the same as Isotropic, Dev is the same as Deviatoric
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, t1, t2
!
!   obj = Rank2Tensor(A0)
!   t1 = Iso(obj)
!   t2 = Isotropic(obj)
!   CALL OK(MAXVAL(ABS(t1%T - t2%T)) .LT. tol, "Iso == Isotropic")
!   t1 = Dev(obj)
!   t2 = Deviatoric(obj)
!   CALL OK(MAXVAL(ABS(t1%T - t2%T)) .LT. tol, "Dev == Deviatoric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Isotropic
  MODULE PURE FUNCTION Rank2Tensor_Isotropic(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    TYPE(Rank2Tensor_) :: ans
    !! symmetric tensor
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
! Returns:
!   ans : Rank2Tensor_ - traceless tensor, symmetric if obj is symmetric
!
! Formula:
!   $$\mathrm{dev}(T) = T - \frac{1}{3}\mathrm{tr}(T)\, I$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: Dev
! USE Rank2Tensor_Method, ONLY: Deviatoric
! USE Rank2Tensor_Method, ONLY: Iso
! USE Rank2Tensor_Method, ONLY: Isotropic
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: Trace
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 1.0_DFP, &
!                                             0.0_DFP, 1.0_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: Deviatoric part is A - tr(A)/3 I and is traceless
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, devObj
!
!   obj = Rank2Tensor(A0)
!   devObj = Deviatoric(obj)
!   CALL OK(MAXVAL(ABS(devObj%T - (A0 - 3.0_DFP * Eye3))) .LT. tol, &
!           "Deviatoric = A - 3 I")
!   CALL OK(ABS(Trace(devObj)) .LT. tol, "Deviatoric: trace is 0")
!   CALL OK(.NOT. devObj%isSym, "Deviatoric: keeps isSym")
! END BLOCK
!
! ! Test 4: Iso is the same as Isotropic, Dev is the same as Deviatoric
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, t1, t2
!
!   obj = Rank2Tensor(A0)
!   t1 = Iso(obj)
!   t2 = Isotropic(obj)
!   CALL OK(MAXVAL(ABS(t1%T - t2%T)) .LT. tol, "Iso == Isotropic")
!   t1 = Dev(obj)
!   t2 = Deviatoric(obj)
!   CALL OK(MAXVAL(ABS(t1%T - t2%T)) .LT. tol, "Dev == Deviatoric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Deviatoric
  MODULE PURE FUNCTION Rank2Tensor_Deviatoric(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    TYPE(Rank2Tensor_) :: ans
    !! traceless tensor, symmetric if obj is symmetric
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
! If `isDeviatoric` is true, then the tensor is assumed to be
! traceless and $I_1$ is set to zero. Both branches return the same
! principal invariants.
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - $[I_1, I_2, I_3]$
!
! Formula:
!   $$I_1 = \mathrm{tr}(T),\quad
!   I_2 = \frac{1}{2}\left(I_1^2 - \mathrm{tr}(T^2)\right),\quad
!   I_3 = \det(T)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Invariants
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 1.0_DFP, &
!                                             0.0_DFP, 1.0_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: invariants of a non-symmetric tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: inv(3)
!
!   obj = Rank2Tensor(A0)
!   inv = Invariants(obj)
!   CALL OK(MAXVAL(ABS(inv - [9.0_DFP, 26.0_DFP, 26.0_DFP])) .LT. tol, &
!           "I1, I2, I3 = 9, 26, 26")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Invariants
  MODULE PURE FUNCTION Rank2Tensor_Invariants(obj, isDeviatoric) &
    RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isDeviatoric
    !! default is false
    REAL(DFP) :: ans(3)
    !! $[I_1, I_2, I_3]$, SIZE(3)
  END FUNCTION Rank2Tensor_Invariants
END INTERFACE Invariants

!----------------------------------------------------------------------------
!                                                        Eigen@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Eigenvalues and eigenvectors of a tensor
!
! If the tensor is symmetric (`obj%isSym`), then the Jacobi method is
! used and the imaginary parts are zero. Otherwise LAPACK GEEV is used.
! A complex conjugate pair is returned in two consecutive columns.
!
! Formula:
!   $$T q_k = w_k q_k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Rank2Tensor_Method, ONLY: Eigen
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! ! Test 1: symmetric tensor, eigenvalues 5, 10, -15
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3), QR(3, 3), WR(3), QI(3, 3), WI(3)
!   INTEGER(I4B) :: k
!
!   mat = RESHAPE([5.0_DFP, 0.0_DFP, 0.0_DFP, &
!                  0.0_DFP, -6.0_DFP, -12.0_DFP, &
!                  0.0_DFP, -12.0_DFP, 1.0_DFP], [3, 3])
!   obj = Rank2Tensor(mat, .TRUE.)
!   CALL OK(obj%isSym, "symmetric: isSym is true")
!   CALL Eigen(obj, QR, WR, QI, WI)
!   DO k = 1, 3
!     CALL OK(MAXVAL(ABS(MATMUL(mat, QR(:, k)) - WR(k) * QR(:, k))) &
!             .LT. tol, "symmetric: A q = w q")
!   END DO
!   CALL OK(ABS(SUM(WR)) .LT. tol, "symmetric: sum of w = 0")
!   CALL OK(ABS(PRODUCT(WR) + 750.0_DFP) .LT. tol, &
!           "symmetric: product of w = -750")
!   CALL OK(MAXVAL(ABS(WI)) .LT. tol, "symmetric: WI = 0")
!   CALL OK(MAXVAL(ABS(QI)) .LT. tol, "symmetric: QI = 0")
! END BLOCK
!
! ! Test 2: non-symmetric tensor with a complex pair, w = 1, 2i, -2i
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: mat(3, 3), QR(3, 3), WR(3), QI(3, 3), WI(3)
!   COMPLEX(DFP) :: q(3), r(3)
!   INTEGER(I4B) :: k
!
!   mat = 0.0_DFP
!   mat(1, 2) = -2.0_DFP
!   mat(2, 1) = 2.0_DFP
!   mat(3, 3) = 1.0_DFP
!   obj = Rank2Tensor(mat)
!   CALL Eigen(obj, QR, WR, QI, WI)
!   DO k = 1, 3
!     q = CMPLX(QR(:, k), QI(:, k), DFP)
!     r = MATMUL(mat, q) - CMPLX(WR(k), WI(k), DFP) * q
!     CALL OK(MAXVAL(ABS(r)) .LT. tol, "non-symmetric: A q = w q")
!   END DO
!   CALL OK(ABS(SUM(WR) - 1.0_DFP) .LT. tol, "non-symmetric: sum WR = 1")
!   CALL OK(ABS(MAXVAL(WI) - 2.0_DFP) .LT. tol .AND. &
!           ABS(MINVAL(WI) + 2.0_DFP) .LT. tol, "non-symmetric: WI = +-2")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Eigen
  MODULE SUBROUTINE Rank2Tensor_Eigen(obj, QR, WR, QI, WI)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    REAL(DFP), INTENT(INOUT) :: QR(3, 3)
    !! real part of the eigenvectors (columns), SHAPE(3,3)
    REAL(DFP), INTENT(INOUT) :: WR(3)
    !! real part of the eigenvalues, SIZE(3)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: QI(3, 3)
    !! imaginary part of eigenvectors, SHAPE(3,3)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: WI(3)
    !! imaginary part of eigenvalues, SIZE(3)
  END SUBROUTINE Rank2Tensor_Eigen
END INTERFACE Eigen

!----------------------------------------------------------------------------
!                                                  PolarDecomp@InvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Polar decomposition of a tensor
!
! Computes the right and left polar decompositions of a tensor with
! positive determinant. The method is Algorithm 3.1 of Higham and
! Noferini (2015).
!
! Formula:
!   $$T = R U = V R,\quad R^{T} R = I$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: PolarDecomp
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! ! Test 2: rotation by 90 deg about z times U = diag(2, 1, 3)
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, R, U, V
!   REAL(DFP) :: mat(3, 3), R0(3, 3), U0(3, 3), V0(3, 3)
!
!   R0 = RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, -1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                 0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3])
!   U0 = 0.0_DFP
!   U0(1, 1) = 2.0_DFP
!   U0(2, 2) = 1.0_DFP
!   U0(3, 3) = 3.0_DFP
!   V0 = 0.0_DFP
!   V0(1, 1) = 1.0_DFP
!   V0(2, 2) = 2.0_DFP
!   V0(3, 3) = 3.0_DFP
!   mat = MATMUL(R0, U0)
!   obj = Rank2Tensor(mat)
!   CALL PolarDecomp(obj, R, U, V)
!   CALL OK(MAXVAL(ABS(R%T - R0)) .LT. tol, "rotation: R")
!   CALL OK(MAXVAL(ABS(U%T - U0)) .LT. tol, "rotation: U = diag(2,1,3)")
!   CALL OK(MAXVAL(ABS(V%T - V0)) .LT. tol, "rotation: V = diag(1,2,3)")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE PolarDecomp
  MODULE PURE SUBROUTINE Rank2Tensor_PolarDecomp(obj, R, U, V)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor T with det(T) > 0
    CLASS(Rank2Tensor_), INTENT(INOUT) :: R
    !! rotation (output)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: U
    !! right stretch, symmetric (output)
    CLASS(Rank2Tensor_), INTENT(INOUT) :: V
    !! left stretch, symmetric (output)
  END SUBROUTINE Rank2Tensor_PolarDecomp
END INTERFACE PolarDecomp

!----------------------------------------------------------------------------
!                                            Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Double contraction of two tensors
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$A : B = A_{ij} B_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Contraction
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: two Rank2Tensors
! BLOCK
!   REAL(DFP) :: ans
!
!   ans = Contraction(Rank2Tensor(A0), Rank2Tensor(S0))
!   CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "obj : obj")
!   CALL OK(ABS(ans - SUM(A0 * S0)) .LT. tol, "obj : obj = SUM(A*B)")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Contraction
  MODULE PURE FUNCTION Rank2Tensor_Contraction1(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_Contraction1
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Double contraction of a tensor and a Voigt tensor
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$A : B = A_{ij} B_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Rank2Tensor_Method, ONLY: Contraction
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: a Rank2Tensor and a Voigt tensor
! BLOCK
!   REAL(DFP) :: ans
!
!   ans = Contraction(Rank2Tensor(A0), &
!                     VoigtRank2Tensor(S0, StressTypeVoigt))
!   CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "obj : Voigt stress-like")
!   ans = Contraction(Rank2Tensor(A0), &
!                     VoigtRank2Tensor(S0, StrainTypeVoigt))
!   CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "obj : Voigt strain-like")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Contraction
  MODULE PURE FUNCTION Rank2Tensor_Contraction2(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj2
    !! symmetric tensor B in Voigt notation
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_Contraction2
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Double contraction of a Voigt tensor and a tensor
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$A : B = A_{ij} B_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Rank2Tensor_Method, ONLY: Contraction
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 3: a Voigt tensor and a Rank2Tensor
! BLOCK
!   REAL(DFP) :: ans
!
!   ans = Contraction(VoigtRank2Tensor(S0, StressTypeVoigt), &
!                     Rank2Tensor(A0))
!   CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "Voigt stress-like : obj")
!   ans = Contraction(VoigtRank2Tensor(S0, StrainTypeVoigt), &
!                     Rank2Tensor(A0))
!   CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "Voigt strain-like : obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Contraction
  MODULE PURE FUNCTION Rank2Tensor_Contraction3(obj1, obj2) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj1
    !! symmetric tensor A in Voigt notation
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_Contraction3
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                            Contraction@ContractionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-14
! summary: Double contraction of two Voigt tensors
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$A : B = A_{ij} B_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressTypeVoigt
! USE Rank2Tensor_Method, ONLY: Contraction
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: two Voigt tensors of any type
! BLOCK
!   REAL(DFP) :: ans
!
!   ans = Contraction(VoigtRank2Tensor(S0, StressTypeVoigt), &
!                     VoigtRank2Tensor(S0, StressTypeVoigt))
!   CALL OK(ABS(ans - SUM(S0 * S0)) .LT. tol, "Voigt : Voigt, stress")
!   ans = Contraction(VoigtRank2Tensor(S0, StressTypeVoigt), &
!                     VoigtRank2Tensor(S0, StrainTypeVoigt))
!   CALL OK(ABS(ans - SUM(S0 * S0)) .LT. tol, "Voigt : Voigt, mixed")
!   ans = Contraction(VoigtRank2Tensor(S0, StrainTypeVoigt), &
!                     VoigtRank2Tensor(S0, StrainTypeVoigt))
!   CALL OK(ABS(ans - SUM(S0 * S0)) .LT. tol, "Voigt : Voigt, strain")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Contraction
  MODULE PURE FUNCTION Rank2Tensor_Contraction4(obj1, obj2) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj1
    !! symmetric tensor A in Voigt notation
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj2
    !! symmetric tensor B in Voigt notation
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_Contraction4
END INTERFACE Contraction

!----------------------------------------------------------------------------
!                                                       +@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Addition of two tensors
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if both A and B are symmetric
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} + B_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: addition
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a + b
!   CALL OK(MAXVAL(ABS(c%T - (A0 + B0))) .LT. tol, "obj + obj")
!   CALL OK(.NOT. c%isSym, "obj + obj: isSym")
!   c = b + s
!   CALL OK(MAXVAL(ABS(c%T - (B0 + s))) .LT. tol, "obj + scalar")
!   CALL OK(c%isSym, "obj + scalar: isSym")
!   c = s + a
!   CALL OK(MAXVAL(ABS(c%T - (s + A0))) .LT. tol, "scalar + obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(+)
  MODULE PURE FUNCTION Rank2Tensor_AddObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    TYPE(Rank2Tensor_) :: ans
    !! symmetric if both A and B are symmetric
  END FUNCTION Rank2Tensor_AddObjObj
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                                       +@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Addition of a tensor and a scalar
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} + s$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: addition
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a + b
!   CALL OK(MAXVAL(ABS(c%T - (A0 + B0))) .LT. tol, "obj + obj")
!   CALL OK(.NOT. c%isSym, "obj + obj: isSym")
!   c = b + s
!   CALL OK(MAXVAL(ABS(c%T - (B0 + s))) .LT. tol, "obj + scalar")
!   CALL OK(c%isSym, "obj + scalar: isSym")
!   c = s + a
!   CALL OK(MAXVAL(ABS(c%T - (s + A0))) .LT. tol, "scalar + obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(+)
  MODULE PURE FUNCTION Rank2Tensor_AddObjScalar(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    REAL(DFP), INTENT(IN) :: obj2
    !! scalar s
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_AddObjScalar
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                                       +@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Addition of a scalar and a tensor
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s + A_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: addition
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a + b
!   CALL OK(MAXVAL(ABS(c%T - (A0 + B0))) .LT. tol, "obj + obj")
!   CALL OK(.NOT. c%isSym, "obj + obj: isSym")
!   c = b + s
!   CALL OK(MAXVAL(ABS(c%T - (B0 + s))) .LT. tol, "obj + scalar")
!   CALL OK(c%isSym, "obj + scalar: isSym")
!   c = s + a
!   CALL OK(MAXVAL(ABS(c%T - (s + A0))) .LT. tol, "scalar + obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(+)
  MODULE PURE FUNCTION Rank2Tensor_AddScalarObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1
    !! scalar s
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor A
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_AddScalarObj
END INTERFACE OPERATOR(+)

!----------------------------------------------------------------------------
!                                                       -@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Subtraction of two tensors
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if both A and B are symmetric
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} - B_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: subtraction
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a - b
!   CALL OK(MAXVAL(ABS(c%T - (A0 - B0))) .LT. tol, "obj - obj")
!   c = a - s
!   CALL OK(MAXVAL(ABS(c%T - (A0 - s))) .LT. tol, "obj - scalar")
!   c = s - a
!   CALL OK(MAXVAL(ABS(c%T - (s - A0))) .LT. tol, "scalar - obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION Rank2Tensor_MinusObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    TYPE(Rank2Tensor_) :: ans
    !! symmetric if both A and B are symmetric
  END FUNCTION Rank2Tensor_MinusObjObj
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                                       -@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Subtraction of a scalar from a tensor
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} - s$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: subtraction
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a - b
!   CALL OK(MAXVAL(ABS(c%T - (A0 - B0))) .LT. tol, "obj - obj")
!   c = a - s
!   CALL OK(MAXVAL(ABS(c%T - (A0 - s))) .LT. tol, "obj - scalar")
!   c = s - a
!   CALL OK(MAXVAL(ABS(c%T - (s - A0))) .LT. tol, "scalar - obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION Rank2Tensor_MinusObjScalar(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    REAL(DFP), INTENT(IN) :: obj2
    !! scalar s
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_MinusObjScalar
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                                       -@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Subtraction of a tensor from a scalar
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s - A_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: subtraction
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a - b
!   CALL OK(MAXVAL(ABS(c%T - (A0 - B0))) .LT. tol, "obj - obj")
!   c = a - s
!   CALL OK(MAXVAL(ABS(c%T - (A0 - s))) .LT. tol, "obj - scalar")
!   c = s - a
!   CALL OK(MAXVAL(ABS(c%T - (s - A0))) .LT. tol, "scalar - obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(-)
  MODULE PURE FUNCTION Rank2Tensor_MinusScalarObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1
    !! scalar s
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor A
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_MinusScalarObj
END INTERFACE OPERATOR(-)

!----------------------------------------------------------------------------
!                                                       *@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Element-wise (Hadamard) product of two tensors
!
! This is not the single contraction. Use `MATMUL` for $A \cdot B$.
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if both A and B are symmetric
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} B_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(*)
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 3: element-wise product, not the matrix product
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a * b
!   CALL OK(ABS(c%T(1, 2) + 2.0_DFP) .LT. tol, "obj * obj: T12")
!   CALL OK(MAXVAL(ABS(c%T - A0 * B0)) .LT. tol, "obj * obj")
!   c = a * s
!   CALL OK(MAXVAL(ABS(c%T - A0 * s)) .LT. tol, "obj * scalar")
!   c = s * b
!   CALL OK(MAXVAL(ABS(c%T - s * B0)) .LT. tol, "scalar * obj")
!   CALL OK(c%isSym, "scalar * obj: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION Rank2Tensor_TimesObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    TYPE(Rank2Tensor_) :: ans
    !! symmetric if both A and B are symmetric
  END FUNCTION Rank2Tensor_TimesObjObj
END INTERFACE OPERATOR(*)

!----------------------------------------------------------------------------
!                                                       *@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Tensor times scalar
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s A_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(*)
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 3: element-wise product, not the matrix product
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a * b
!   CALL OK(ABS(c%T(1, 2) + 2.0_DFP) .LT. tol, "obj * obj: T12")
!   CALL OK(MAXVAL(ABS(c%T - A0 * B0)) .LT. tol, "obj * obj")
!   c = a * s
!   CALL OK(MAXVAL(ABS(c%T - A0 * s)) .LT. tol, "obj * scalar")
!   c = s * b
!   CALL OK(MAXVAL(ABS(c%T - s * B0)) .LT. tol, "scalar * obj")
!   CALL OK(c%isSym, "scalar * obj: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION Rank2Tensor_TimesObjScalar(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    REAL(DFP), INTENT(IN) :: obj2
    !! scalar s
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_TimesObjScalar
END INTERFACE OPERATOR(*)

!----------------------------------------------------------------------------
!                                                       *@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Scalar times tensor
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s A_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(*)
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 3: element-wise product, not the matrix product
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a * b
!   CALL OK(ABS(c%T(1, 2) + 2.0_DFP) .LT. tol, "obj * obj: T12")
!   CALL OK(MAXVAL(ABS(c%T - A0 * B0)) .LT. tol, "obj * obj")
!   c = a * s
!   CALL OK(MAXVAL(ABS(c%T - A0 * s)) .LT. tol, "obj * scalar")
!   c = s * b
!   CALL OK(MAXVAL(ABS(c%T - s * B0)) .LT. tol, "scalar * obj")
!   CALL OK(c%isSym, "scalar * obj: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(*)
  MODULE PURE FUNCTION Rank2Tensor_TimesScalarObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1
    !! scalar s
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor A
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_TimesScalarObj
END INTERFACE OPERATOR(*)

!----------------------------------------------------------------------------
!                                                       /@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Element-wise division of two tensors
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if both A and B are symmetric
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} / B_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: OPERATOR(/)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: element-wise division
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a / b
!   CALL OK(ABS(c%T(1, 2) + 0.5_DFP) .LT. tol, "obj / obj: T12")
!   CALL OK(MAXVAL(ABS(c%T - A0 / B0)) .LT. tol, "obj / obj")
!   c = a / s
!   CALL OK(MAXVAL(ABS(c%T - A0 / s)) .LT. tol, "obj / scalar")
!   c = s / b
!   CALL OK(MAXVAL(ABS(c%T - s / B0)) .LT. tol, "scalar / obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION Rank2Tensor_DivObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    TYPE(Rank2Tensor_) :: ans
    !! symmetric if both A and B are symmetric
  END FUNCTION Rank2Tensor_DivObjObj
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                       /@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Tensor divided by scalar
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = A_{ij} / s$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: OPERATOR(/)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: element-wise division
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a / b
!   CALL OK(ABS(c%T(1, 2) + 0.5_DFP) .LT. tol, "obj / obj: T12")
!   CALL OK(MAXVAL(ABS(c%T - A0 / B0)) .LT. tol, "obj / obj")
!   c = a / s
!   CALL OK(MAXVAL(ABS(c%T - A0 / s)) .LT. tol, "obj / scalar")
!   c = s / b
!   CALL OK(MAXVAL(ABS(c%T - s / B0)) .LT. tol, "scalar / obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION Rank2Tensor_DivObjScalar(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    REAL(DFP), INTENT(IN) :: obj2
    !! scalar s
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_DivObjScalar
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                       /@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Scalar divided element-wise by a tensor
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$\mathrm{ans}_{ij} = s / A_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: OPERATOR(+)
! USE Rank2Tensor_Method, ONLY: OPERATOR(-)
! USE Rank2Tensor_Method, ONLY: OPERATOR(/)
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s = 2.0_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.5_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: B0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 4.0_DFP, &
!                                             2.0_DFP, 5.0_DFP, 0.5_DFP, &
!                                             4.0_DFP, 0.5_DFP, 8.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: element-wise division
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(B0, .TRUE.)
!   c = a / b
!   CALL OK(ABS(c%T(1, 2) + 0.5_DFP) .LT. tol, "obj / obj: T12")
!   CALL OK(MAXVAL(ABS(c%T - A0 / B0)) .LT. tol, "obj / obj")
!   c = a / s
!   CALL OK(MAXVAL(ABS(c%T - A0 / s)) .LT. tol, "obj / scalar")
!   c = s / b
!   CALL OK(MAXVAL(ABS(c%T - s / B0)) .LT. tol, "scalar / obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(/)
  MODULE PURE FUNCTION Rank2Tensor_DivScalarObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1
    !! scalar s
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor A
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_DivScalarObj
END INTERFACE OPERATOR(/)

!----------------------------------------------------------------------------
!                                                   MATMUL@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Single contraction of two tensors
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$(A \cdot B)_{ij} = A_{ik} B_{kj}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: MATMUL
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: tensor times tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: a, b, c
!
!   a = Rank2Tensor(A0)
!   b = Rank2Tensor(TRANSPOSE(A0))
!   c = MATMUL(a, b)
!   CALL OK(ABS(c%T(1, 1) - 5.0_DFP) .LT. tol, "obj obj: T11")
!   CALL OK(MAXVAL(ABS(c%T - MATMUL(A0, TRANSPOSE(A0)))) .LT. tol, &
!           "obj obj: T")
!   CALL OK(.NOT. c%isSym, "obj obj: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE MATMUL
  MODULE PURE FUNCTION Rank2Tensor_MatmulObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_MatmulObjObj
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                   MATMUL@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Tensor times vector
!
! Returns:
!   ans : REAL(DFP), SIZE(3)
!
! Formula:
!   $$(A v)_{i} = A_{ij} v_{j}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: MATMUL
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: tensor times vector
! BLOCK
!   TYPE(Rank2Tensor_) :: a
!   REAL(DFP) :: v(3)
!
!   a = Rank2Tensor(A0)
!   v = MATMUL(a, [1.0_DFP, 1.0_DFP, 1.0_DFP])
!   CALL OK(MAXVAL(ABS(v - [1.0_DFP, 5.5_DFP, 4.75_DFP])) .LT. tol, &
!           "obj vec")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE MATMUL
  MODULE PURE FUNCTION Rank2Tensor_MatmulObjVec(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    REAL(DFP), INTENT(IN) :: obj2(3)
    !! vector v, SIZE(3)
    REAL(DFP) :: ans(3)
    !! REAL(DFP), SIZE(3)
  END FUNCTION Rank2Tensor_MatmulObjVec
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                   MATMUL@OperatorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Vector times tensor
!
! Returns:
!   ans : REAL(DFP), SIZE(3)
!
! Formula:
!   $$(v A)_{j} = v_{i} A_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: MATMUL
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
!                                             -1.0_DFP, 3.0_DFP, 0.25_DFP, &
!                                             0.0_DFP, 1.5_DFP, 4.0_DFP], &
!                                            [3, 3])
!
! ! Test 3: vector times tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: a
!   REAL(DFP) :: v(3)
!
!   a = Rank2Tensor(A0)
!   v = MATMUL([1.0_DFP, 1.0_DFP, 1.0_DFP], a)
!   CALL OK(MAXVAL(ABS(v - [3.5_DFP, 2.25_DFP, 5.5_DFP])) .LT. tol, &
!           "vec obj")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE MATMUL
  MODULE PURE FUNCTION Rank2Tensor_MatmulVecObj(obj1, obj2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: obj1(3)
    !! vector v, SIZE(3)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor A
    REAL(DFP) :: ans(3)
    !! REAL(DFP), SIZE(3)
  END FUNCTION Rank2Tensor_MatmulVecObj
END INTERFACE MATMUL

!----------------------------------------------------------------------------
!                                                   Pullback@PullbackMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Pull back of a tensor to the reference configuration
!
! `indx1` and `indx2` describe the variance of the first and second
! index. Each is one of `"CONTRAVAR"`, `"COVAR"` or `"NA"` (no
! transformation). An unknown value is treated as `"NA"`.
!
! Returns:
!   ans : Rank2Tensor_ - material tensor
!
! Formula:
!   $$\text{CONTRAVAR, CONTRAVAR}: F^{-1} T F^{-T},\quad
!   \text{COVAR, COVAR}: F^{T} T F$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Pullback
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                                             2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             0.0_DFP, 0.0_DFP, 2.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: invF0(3, 3) = RESHAPE( &
!                         [1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                          -2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                          0.0_DFP, 0.0_DFP, 0.5_DFP], [3, 3])
! REAL(DFP), PARAMETER :: T0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, &
!                                             4.0_DFP, 5.0_DFP, 6.0_DFP, &
!                                             7.0_DFP, 8.0_DFP, 10.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: tensor pull back with known values
! BLOCK
!   TYPE(Rank2Tensor_) :: F, T, P
!
!   F = Rank2Tensor(F0)
!   T = Rank2Tensor(T0)
!   P = Pullback(T, F, "CONTRAVAR", "CONTRAVAR")
!   CALL OK(MAXVAL(ABS(P%T - MATMUL(invF0, MATMUL(T0, TRANSPOSE(invF0))))) &
!           .LT. tol, "CONTRAVAR, CONTRAVAR: inv(F) T inv(F)^T")
!   P = Pullback(T, F, "COVAR", "COVAR")
!   CALL OK(MAXVAL(ABS(P%T - MATMUL(TRANSPOSE(F0), MATMUL(T0, F0)))) &
!           .LT. tol, "COVAR, COVAR: F^T T F")
!   P = Pullback(T, F, "NA", "NA")
!   CALL OK(MAXVAL(ABS(P%T - T0)) .LT. tol, "NA, NA: T")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Pullback
  MODULE PURE FUNCTION Rank2Tensor_Pullback1(T, F, indx1, indx2) &
    RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    !! spatial tensor
    CLASS(Rank2Tensor_), INTENT(IN) :: F
    !! deformation gradient
    CHARACTER(*), INTENT(IN) :: indx1
    !! variance of the first index
    CHARACTER(*), INTENT(IN) :: indx2
    !! variance of the second index
    TYPE(Rank2Tensor_) :: ans
    !! material tensor
  END FUNCTION Rank2Tensor_Pullback1
END INTERFACE Pullback

!----------------------------------------------------------------------------
!                                                   Pullback@PullbackMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Pull back of a vector to the reference configuration
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - material vector
!
! Formula:
!   $$\text{CONTRAVAR}: F^{-1} v,\quad \text{COVAR}: F^{T} v$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Pullback
! USE Rank2Tensor_Method, ONLY: PushForward
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                                             2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             0.0_DFP, 0.0_DFP, 2.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: vector pull back and push forward
! BLOCK
!   TYPE(Rank2Tensor_) :: F
!   REAL(DFP) :: v(3), w(3)
!
!   F = Rank2Tensor(F0)
!   v = [1.0_DFP, -2.0_DFP, 0.5_DFP]
!   w = Pullback(v, F, "CONTRAVAR")
!   CALL OK(MAXVAL(ABS(w - [5.0_DFP, -2.0_DFP, 0.25_DFP])) .LT. tol, &
!           "vector CONTRAVAR: inv(F) v")
!   w = Pullback(v, F, "COVAR")
!   CALL OK(MAXVAL(ABS(w - [1.0_DFP, 0.0_DFP, 1.0_DFP])) .LT. tol, &
!           "vector COVAR: F^T v")
!   w = PushForward(Pullback(v, F, "CONTRAVAR"), F, "CONTRAVAR")
!   CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector push(pull) CONTRAVAR")
!   w = PushForward(Pullback(v, F, "COVAR"), F, "COVAR")
!   CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector push(pull) COVAR")
!   w = PushForward(v, F, "NA")
!   CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector NA: v")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Pullback
  MODULE PURE FUNCTION Rank2Tensor_Pullback2(vec, F, indx1) RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(3)
    !! spatial vector, SIZE(3)
    CLASS(Rank2Tensor_), INTENT(IN) :: F
    !! deformation gradient
    CHARACTER(*), INTENT(IN) :: indx1
    !! `"CONTRAVAR"`, `"COVAR"` or `"NA"`
    REAL(DFP) :: ans(3)
    !! material vector, SIZE(3)
  END FUNCTION Rank2Tensor_Pullback2
END INTERFACE Pullback

!----------------------------------------------------------------------------
!                                             PushForward@PushForwardMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Push forward of a tensor to the current configuration
!
! `indx1` and `indx2` describe the variance of the first and second
! index. Each is one of `"CONTRAVAR"`, `"COVAR"` or `"NA"` (no
! transformation). An unknown value is treated as `"NA"`.
!
! Returns:
!   ans : Rank2Tensor_ - spatial tensor
!
! Formula:
!   $$\text{CONTRAVAR, CONTRAVAR}: F T F^{T},\quad
!   \text{COVAR, COVAR}: F^{-T} T F^{-1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: PushForward
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                                             2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             0.0_DFP, 0.0_DFP, 2.0_DFP], &
!                                            [3, 3])
! REAL(DFP), PARAMETER :: invF0(3, 3) = RESHAPE( &
!                         [1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                          -2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                          0.0_DFP, 0.0_DFP, 0.5_DFP], [3, 3])
! REAL(DFP), PARAMETER :: T0(3, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, &
!                                             4.0_DFP, 5.0_DFP, 6.0_DFP, &
!                                             7.0_DFP, 8.0_DFP, 10.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: tensor push forward with known values
! BLOCK
!   TYPE(Rank2Tensor_) :: F, T, P
!
!   F = Rank2Tensor(F0)
!   T = Rank2Tensor(T0)
!   P = PushForward(T, F, "CONTRAVAR", "CONTRAVAR")
!   CALL OK(MAXVAL(ABS(P%T - MATMUL(F0, MATMUL(T0, TRANSPOSE(F0))))) &
!           .LT. tol, "push CONTRAVAR, CONTRAVAR: F T F^T")
!   P = PushForward(T, F, "COVAR", "COVAR")
!   CALL OK(MAXVAL(ABS(P%T - MATMUL(TRANSPOSE(invF0), MATMUL(T0, invF0)))) &
!           .LT. tol, "push COVAR, COVAR: inv(F)^T T inv(F)")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE PushForward
  MODULE PURE FUNCTION Rank2Tensor_PushForward1(T, F, indx1, indx2) &
    RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    !! material tensor
    CLASS(Rank2Tensor_), INTENT(IN) :: F
    !! deformation gradient
    CHARACTER(*), INTENT(IN) :: indx1
    !! variance of the first index
    CHARACTER(*), INTENT(IN) :: indx2
    !! variance of the second index
    TYPE(Rank2Tensor_) :: ans
    !! spatial tensor
  END FUNCTION Rank2Tensor_PushForward1
END INTERFACE PushForward

!----------------------------------------------------------------------------
!                                             PushForward@PushForwardMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-17
! summary: Push forward of a vector to the current configuration
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - spatial vector
!
! Formula:
!   $$\text{CONTRAVAR}: F v,\quad \text{COVAR}: F^{-T} v$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Pullback
! USE Rank2Tensor_Method, ONLY: PushForward
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                                             2.0_DFP, 1.0_DFP, 0.0_DFP, &
!                                             0.0_DFP, 0.0_DFP, 2.0_DFP], &
!                                            [3, 3])
!
! ! Test 4: vector pull back and push forward
! BLOCK
!   TYPE(Rank2Tensor_) :: F
!   REAL(DFP) :: v(3), w(3)
!
!   F = Rank2Tensor(F0)
!   v = [1.0_DFP, -2.0_DFP, 0.5_DFP]
!   w = Pullback(v, F, "CONTRAVAR")
!   CALL OK(MAXVAL(ABS(w - [5.0_DFP, -2.0_DFP, 0.25_DFP])) .LT. tol, &
!           "vector CONTRAVAR: inv(F) v")
!   w = Pullback(v, F, "COVAR")
!   CALL OK(MAXVAL(ABS(w - [1.0_DFP, 0.0_DFP, 1.0_DFP])) .LT. tol, &
!           "vector COVAR: F^T v")
!   w = PushForward(Pullback(v, F, "CONTRAVAR"), F, "CONTRAVAR")
!   CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector push(pull) CONTRAVAR")
!   w = PushForward(Pullback(v, F, "COVAR"), F, "COVAR")
!   CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector push(pull) COVAR")
!   w = PushForward(v, F, "NA")
!   CALL OK(MAXVAL(ABS(w - v)) .LT. tol, "vector NA: v")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE PushForward
  MODULE PURE FUNCTION Rank2Tensor_PushForward2(vec, F, indx1) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(3)
    !! material vector, SIZE(3)
    CLASS(Rank2Tensor_), INTENT(IN) :: F
    !! deformation gradient
    CHARACTER(*), INTENT(IN) :: indx1
    !! `"CONTRAVAR"`, `"COVAR"` or `"NA"`
    REAL(DFP) :: ans(3)
    !! spatial vector, SIZE(3)
  END FUNCTION Rank2Tensor_PushForward2
END INTERFACE PushForward

!----------------------------------------------------------------------------
!                                                    Otimes@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Dyadic product of two vectors
!
! Returns:
!   ans : Rank2Tensor_
!
! Formula:
!   $$(a \otimes b)_{ij} = a_{i} b_{j}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Otimes
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: dyadic product of two vectors
! BLOCK
!   TYPE(Rank2Tensor_) :: ans
!   REAL(DFP) :: a(3), b(3)
!
!   a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
!   b = [4.0_DFP, 5.0_DFP, 6.0_DFP]
!   ans = Otimes(a, b)
!   CALL OK(ABS(ans%T(1, 2) - 5.0_DFP) .LT. tol, "vec vec: T12")
!   CALL OK(ABS(ans%T(3, 1) - 12.0_DFP) .LT. tol, "vec vec: T31")
!   CALL OK(MAXVAL(ABS(ans%T - SPREAD(a, 2, 3) * SPREAD(b, 1, 3))) &
!           .LT. tol, "vec vec: T")
!   CALL OK(.NOT. ans%isSym, "vec vec: isSym")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Otimes
  MODULE PURE FUNCTION Rank2Tensor_OtimesVecVec(a, b) RESULT(ans)
    REAL(DFP), INTENT(IN) :: a(3)
    !! vector a, SIZE(3)
    REAL(DFP), INTENT(IN) :: b(3)
    !! vector b, SIZE(3)
    TYPE(Rank2Tensor_) :: ans
    !! rank2Tensor_
  END FUNCTION Rank2Tensor_OtimesVecVec
END INTERFACE Otimes

!----------------------------------------------------------------------------
!                                                    Otimes@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Dyadic product of two rank-2 tensors
!
! Returns:
!   ans : REAL(DFP), SHAPE(3,3,3,3) - rank-4 tensor
!
! Formula:
!   $$(A \otimes B)_{ijkl} = A_{ij} B_{kl}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: Otimes
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 2: dyadic product of two tensors
! BLOCK
!   REAL(DFP) :: ans(3, 3, 3, 3), B0(3, 3)
!
!   B0 = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
!                 6.0_DFP, 7.0_DFP, 8.0_DFP, 9.0_DFP], [3, 3])
!   ans = Otimes(Rank2Tensor(Eye3), Rank2Tensor(B0))
!   CALL OK(ABS(ans(1, 1, 2, 3) - 8.0_DFP) .LT. tol, "obj obj: 1123")
!   CALL OK(ABS(ans(1, 2, 2, 3)) .LT. tol, "obj obj: 1223")
!   CALL OK(ABS(ans(3, 3, 3, 1) - 3.0_DFP) .LT. tol, "obj obj: 3331")
!   CALL OK(ABS(SUM(ans) - 3.0_DFP * SUM(B0)) .LT. tol, "obj obj: sum")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Otimes
  MODULE PURE FUNCTION Rank2Tensor_OtimesObjObj(obj1, obj2) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj1
    !! tensor A
    CLASS(Rank2Tensor_), INTENT(IN) :: obj2
    !! tensor B
    REAL(DFP) :: ans(3, 3, 3, 3)
    !! rank-4 tensor, SHAPE(3,3,3,3)
  END FUNCTION Rank2Tensor_OtimesObjObj
END INTERFACE Otimes

!----------------------------------------------------------------------------
!                                           PrincipalValues@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Principal values of a tensor
!
! Returns the real parts of the eigenvalues sorted in descending order.
! For a symmetric tensor these are the principal values.
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - $\lambda_1 \ge \lambda_2 \ge \lambda_3$
!
! Formula:
!   $$\det(T - \lambda I) = 0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: PrincipalValues
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([5.0_DFP, 0.0_DFP, 0.0_DFP, &
!                                             0.0_DFP, -6.0_DFP, -12.0_DFP, &
!                                             0.0_DFP, -12.0_DFP, 1.0_DFP], &
!                                            [3, 3])
!
! ! Test 1: principal values of a symmetric tensor, descending order
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!   REAL(DFP) :: pv(3)
!
!   obj = Rank2Tensor(S0, .TRUE.)
!   pv = PrincipalValues(obj)
!   CALL OK(MAXVAL(ABS(pv - [10.0_DFP, 5.0_DFP, -15.0_DFP])) .LT. tol, &
!           "PrincipalValues = 10, 5, -15")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE PrincipalValues
  MODULE FUNCTION Rank2Tensor_PrincipalValues(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    REAL(DFP) :: ans(3)
    !! $\lambda_1 \ge \lambda_2 \ge \lambda_3$, SIZE(3)
  END FUNCTION Rank2Tensor_PrincipalValues
END INTERFACE PrincipalValues

!----------------------------------------------------------------------------
!                                            SpectralRadius@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Spectral radius of a tensor
!
! Returns:
!   ans : REAL(DFP) - largest modulus of the (complex) eigenvalues
!
! Formula:
!   $$\rho(T) = \max_{k} |\lambda_k|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: SpectralRadius
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([5.0_DFP, 0.0_DFP, 0.0_DFP, &
!                                             0.0_DFP, -6.0_DFP, -12.0_DFP, &
!                                             0.0_DFP, -12.0_DFP, 1.0_DFP], &
!                                            [3, 3])
!
! ! Test 2: spectral radius of a symmetric tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(S0, .TRUE.)
!   CALL OK(ABS(SpectralRadius(obj) - 15.0_DFP) .LT. tol, &
!           "SpectralRadius symmetric = 15")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE SpectralRadius
  MODULE FUNCTION Rank2Tensor_SpectralRadius(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    REAL(DFP) :: ans
    !! largest modulus of the (complex) eigenvalues
  END FUNCTION Rank2Tensor_SpectralRadius
END INTERFACE SpectralRadius

!----------------------------------------------------------------------------
!                                              RotationPart@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rotation part of the polar decomposition
!
! Returns:
!   ans : Rank2Tensor_ - proper orthogonal tensor R
!
! Formula:
!   $$T = R U,\quad R^{T} R = I,\quad \det R = 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: PolarDecomp
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: RotationPart
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([1.0_DFP, -0.333_DFP, &
!                                             0.959_DFP, 0.495_DFP, &
!                                             1.0_DFP, 0.0_DFP, 0.5_DFP, &
!                                             -0.247_DFP, 1.5_DFP], [3, 3])
!
! ! Test 3: RotationPart is the R of PolarDecomp
! BLOCK
!   TYPE(Rank2Tensor_) :: obj, R, U, V, R2
!
!   obj = Rank2Tensor(A0)
!   CALL PolarDecomp(obj, R, U, V)
!   R2 = RotationPart(obj)
!   CALL OK(MAXVAL(ABS(R2%T - R%T)) .LT. tol, "RotationPart = R")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE RotationPart
  MODULE PURE FUNCTION Rank2Tensor_RotationPart(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor T with det(T) > 0
    TYPE(Rank2Tensor_) :: ans
    !! proper orthogonal tensor R
  END FUNCTION Rank2Tensor_RotationPart
END INTERFACE RotationPart

!----------------------------------------------------------------------------
!                                                       Exp@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Exponential of a tensor
!
! Uses scaling and squaring with a diagonal Pade approximant of order
! 6 (Moler and Van Loan, 2003).
!
! Returns:
!   ans : Rank2Tensor_ - symmetric if obj is symmetric
!
! Formula:
!   $$\exp(T) = \sum_{k=0}^{\infty} \frac{T^{k}}{k!}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: Exp
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! ! Test 1: exp(0) = I
! BLOCK
!   TYPE(Rank2Tensor_) :: E
!
!   E = Exp(Rank2Tensor(0.0_DFP * Eye3, .TRUE.))
!   CALL OK(MAXVAL(ABS(E%T - Eye3)) .LT. tol, "exp(0) = I")
! END BLOCK
!
! ! Test 2: exponential of a diagonal tensor
! BLOCK
!   TYPE(Rank2Tensor_) :: E
!   REAL(DFP) :: mat(3, 3), expected(3, 3)
!
!   mat = 0.0_DFP
!   mat(1, 1) = 1.0_DFP
!   mat(2, 2) = -2.0_DFP
!   mat(3, 3) = 5.0_DFP
!   expected = 0.0_DFP
!   expected(1, 1) = EXP(1.0_DFP)
!   expected(2, 2) = EXP(-2.0_DFP)
!   expected(3, 3) = EXP(5.0_DFP)
!   E = Exp(Rank2Tensor(mat, .TRUE.))
!   CALL OK(MAXVAL(ABS(E%T - expected)) .LT. tol * EXP(5.0_DFP), &
!           "exp of diagonal")
!   CALL OK(E%isSym, "exp of symmetric is symmetric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Exp
  MODULE PURE FUNCTION Rank2Tensor_Exp(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! tensor
    TYPE(Rank2Tensor_) :: ans
    !! symmetric if obj is symmetric
  END FUNCTION Rank2Tensor_Exp
END INTERFACE Exp

!----------------------------------------------------------------------------
!                            DeformationGradient@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a [[DeformationGradient_]]
!
! Returns:
!   ans : DeformationGradient_
!
! Formula:
!   $$F = \frac{\partial x}{\partial X}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 2: from a Rank2Tensor_
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Rank2Tensor_) :: obj
!
!   obj = Rank2Tensor(F0)
!   F = DeformationGradient(obj)
!   CALL OK(MAXVAL(ABS(F%T - F0)) .LT. tol, "obj: components")
!   CALL OK(.NOT. F%isSym, "obj: not symmetric")
! END BLOCK
!
! ! Test 3: with no argument F is zero
! BLOCK
!   TYPE(DeformationGradient_) :: F
!
!   F = DeformationGradient()
!   CALL OK(MAXVAL(ABS(F%T)) .LT. tol, "no argument: F is zero")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE DeformationGradient
  MODULE PURE FUNCTION Rank2Tensor_DeformationGradient1(obj) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: obj
    !! components; if absent F is zero
    TYPE(DeformationGradient_) :: ans
    !! deformationGradient_
  END FUNCTION Rank2Tensor_DeformationGradient1
END INTERFACE DeformationGradient

!----------------------------------------------------------------------------
!                            DeformationGradient@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[DeformationGradient_]] from a 3x3 matrix
!
! Returns:
!   ans : DeformationGradient_
!
! Formula:
!   $$F_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 1: from a matrix
! BLOCK
!   TYPE(DeformationGradient_) :: F
!
!   F = DeformationGradient(F0)
!   CALL OK(MAXVAL(ABS(F%T - F0)) .LT. tol, "mat: components")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE DeformationGradient
  MODULE PURE FUNCTION Rank2Tensor_DeformationGradient2(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! components of F, SHAPE(3,3)
    TYPE(DeformationGradient_) :: ans
    !! deformationGradient_
  END FUNCTION Rank2Tensor_DeformationGradient2
END INTERFACE DeformationGradient

!----------------------------------------------------------------------------
!                    DeformationGradient_Pointer@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a pointer to a new [[DeformationGradient_]]
!
! Returns:
!   ans : CLASS(DeformationGradient_), POINTER - caller must deallocate
!
! Formula:
!   $$F = \frac{\partial x}{\partial X}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient_Pointer
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 5: pointer from a Rank2Tensor_ and with no argument
! BLOCK
!   CLASS(DeformationGradient_), POINTER :: ptr
!
!   ptr => DeformationGradient_Pointer(Rank2Tensor(F0))
!   CALL OK(MAXVAL(ABS(ptr%T - F0)) .LT. tol, "pointer obj: components")
!   DEALLOCATE (ptr)
!   ptr => DeformationGradient_Pointer()
!   CALL OK(MAXVAL(ABS(ptr%T)) .LT. tol, "pointer no argument: zero")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE DeformationGradient_Pointer
  MODULE PURE FUNCTION Rank2Tensor_DeformationGradientPtr1(obj) &
    RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: obj
    !! components; if absent F is zero
    CLASS(DeformationGradient_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_DeformationGradientPtr1
END INTERFACE DeformationGradient_Pointer

!----------------------------------------------------------------------------
!                    DeformationGradient_Pointer@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[DeformationGradient_]]
!
! Returns:
!   ans : CLASS(DeformationGradient_), POINTER - caller must deallocate
!
! Formula:
!   $$F_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient_Pointer
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 4: pointer from a matrix
! BLOCK
!   CLASS(DeformationGradient_), POINTER :: ptr
!
!   ptr => DeformationGradient_Pointer(F0)
!   CALL OK(ASSOCIATED(ptr), "pointer mat: associated")
!   CALL OK(MAXVAL(ABS(ptr%T - F0)) .LT. tol, "pointer mat: components")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE DeformationGradient_Pointer
  MODULE PURE FUNCTION Rank2Tensor_DeformationGradientPtr2(mat) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! components of F, SHAPE(3,3)
    CLASS(DeformationGradient_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_DeformationGradientPtr2
END INTERFACE DeformationGradient_Pointer

!----------------------------------------------------------------------------
!                                       Jacobian@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Volume ratio of a deformation gradient
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$J = \det F$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: Jacobian
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 2: upper triangular F, J is the product of the diagonal
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   REAL(DFP) :: mat(3, 3)
!
!   mat = RESHAPE([2.0_DFP, 0.0_DFP, 0.0_DFP, &
!                  1.0_DFP, 3.0_DFP, 0.0_DFP, &
!                  0.5_DFP, -1.0_DFP, 4.0_DFP], [3, 3])
!   F = DeformationGradient(mat)
!   CALL OK(ABS(Jacobian(F) - 24.0_DFP) .LT. tol, "triangular: J = 24")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Jacobian
  MODULE PURE FUNCTION Rank2Tensor_Jacobian(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_Jacobian
END INTERFACE Jacobian

!----------------------------------------------------------------------------
!                                   RightStretch@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Right stretch tensor of a deformation gradient
!
! Returns:
!   ans : Rank2Tensor_ - symmetric positive definite U
!
! Formula:
!   $$F = R U,\quad U^{2} = F^{T} F$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftStretch
! USE Rank2Tensor_Method, ONLY: RightStretch
! USE Rank2Tensor_Method, ONLY: Rotation
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! ! Test 1: F = R U with R a 90 degree rotation about z
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Rank2Tensor_) :: U, V, R
!   REAL(DFP) :: rot(3, 3), stretch(3, 3)
!
!   rot = RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, &
!                  -1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                  0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3])
!   stretch = 0.0_DFP
!   stretch(1, 1) = 2.0_DFP
!   stretch(2, 2) = 3.0_DFP
!   stretch(3, 3) = 1.0_DFP
!   F = DeformationGradient(MATMUL(rot, stretch))
!   U = RightStretch(F)
!   V = LeftStretch(F)
!   R = Rotation(F)
!   CALL OK(MAXVAL(ABS(U%T - stretch)) .LT. tol, "rotated: U")
!   CALL OK(MAXVAL(ABS(R%T - rot)) .LT. tol, "rotated: R")
!   CALL OK(MAXVAL(ABS(V%T - MATMUL(rot, MATMUL(stretch, &
!           TRANSPOSE(rot))))) .LT. tol, "rotated: V = R U R^T")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE RightStretch
  MODULE PURE FUNCTION Rank2Tensor_RightStretch(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(Rank2Tensor_) :: ans
    !! symmetric positive definite U
  END FUNCTION Rank2Tensor_RightStretch
END INTERFACE RightStretch

!----------------------------------------------------------------------------
!                                    LeftStretch@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Left stretch tensor of a deformation gradient
!
! Returns:
!   ans : Rank2Tensor_ - symmetric positive definite V
!
! Formula:
!   $$F = V R,\quad V^{2} = F F^{T}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftStretch
! USE Rank2Tensor_Method, ONLY: RightStretch
! USE Rank2Tensor_Method, ONLY: Rotation
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! ! Test 1: F = R U with R a 90 degree rotation about z
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Rank2Tensor_) :: U, V, R
!   REAL(DFP) :: rot(3, 3), stretch(3, 3)
!
!   rot = RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, &
!                  -1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                  0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3])
!   stretch = 0.0_DFP
!   stretch(1, 1) = 2.0_DFP
!   stretch(2, 2) = 3.0_DFP
!   stretch(3, 3) = 1.0_DFP
!   F = DeformationGradient(MATMUL(rot, stretch))
!   U = RightStretch(F)
!   V = LeftStretch(F)
!   R = Rotation(F)
!   CALL OK(MAXVAL(ABS(U%T - stretch)) .LT. tol, "rotated: U")
!   CALL OK(MAXVAL(ABS(R%T - rot)) .LT. tol, "rotated: R")
!   CALL OK(MAXVAL(ABS(V%T - MATMUL(rot, MATMUL(stretch, &
!           TRANSPOSE(rot))))) .LT. tol, "rotated: V = R U R^T")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE LeftStretch
  MODULE PURE FUNCTION Rank2Tensor_LeftStretch(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(Rank2Tensor_) :: ans
    !! symmetric positive definite V
  END FUNCTION Rank2Tensor_LeftStretch
END INTERFACE LeftStretch

!----------------------------------------------------------------------------
!                                       Rotation@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rotation tensor of a deformation gradient
!
! Returns:
!   ans : Rank2Tensor_ - proper orthogonal R
!
! Formula:
!   $$F = R U = V R$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftStretch
! USE Rank2Tensor_Method, ONLY: RightStretch
! USE Rank2Tensor_Method, ONLY: Rotation
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! ! Test 1: F = R U with R a 90 degree rotation about z
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Rank2Tensor_) :: U, V, R
!   REAL(DFP) :: rot(3, 3), stretch(3, 3)
!
!   rot = RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, &
!                  -1.0_DFP, 0.0_DFP, 0.0_DFP, &
!                  0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3])
!   stretch = 0.0_DFP
!   stretch(1, 1) = 2.0_DFP
!   stretch(2, 2) = 3.0_DFP
!   stretch(3, 3) = 1.0_DFP
!   F = DeformationGradient(MATMUL(rot, stretch))
!   U = RightStretch(F)
!   V = LeftStretch(F)
!   R = Rotation(F)
!   CALL OK(MAXVAL(ABS(U%T - stretch)) .LT. tol, "rotated: U")
!   CALL OK(MAXVAL(ABS(R%T - rot)) .LT. tol, "rotated: R")
!   CALL OK(MAXVAL(ABS(V%T - MATMUL(rot, MATMUL(stretch, &
!           TRANSPOSE(rot))))) .LT. tol, "rotated: V = R U R^T")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Rotation
  MODULE PURE FUNCTION Rank2Tensor_Rotation(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(Rank2Tensor_) :: ans
    !! proper orthogonal R
  END FUNCTION Rank2Tensor_Rotation
END INTERFACE Rotation

!----------------------------------------------------------------------------
!                               PrincipalStretch@DeformationGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Principal stretches of a deformation gradient
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - stretches in descending order
!
! Formula:
!   $$\lambda_k = \sqrt{\mathrm{eig}_k(F^{T} F)}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: PrincipalStretch
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! ! Test 3: principal stretches are sorted in descending order
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   REAL(DFP) :: mat(3, 3), lam(3)
!
!   mat = 0.0_DFP
!   mat(1, 1) = 1.0_DFP
!   mat(2, 2) = 3.0_DFP
!   mat(3, 3) = 2.0_DFP
!   F = DeformationGradient(mat)
!   lam = PrincipalStretch(F)
!   CALL OK(MAXVAL(ABS(lam - [3.0_DFP, 2.0_DFP, 1.0_DFP])) .LT. tol, &
!           "diagonal: stretches 3, 2, 1")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE PrincipalStretch
  MODULE PURE FUNCTION Rank2Tensor_PrincipalStretch(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    REAL(DFP) :: ans(3)
    !! stretches in descending order, SIZE(3)
  END FUNCTION Rank2Tensor_PrincipalStretch
END INTERFACE PrincipalStretch

!----------------------------------------------------------------------------
!                                    LeftCauchyGreen@CauchyGreenMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a [[LeftCauchyGreen_]] tensor
!
! If `F` is present it is used, else if `V` is present it is used,
! otherwise the identity is returned.
!
! Returns:
!   ans : LeftCauchyGreen_ - symmetric tensor
!
! Formula:
!   $$b = F F^{T} = V^{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: LeftCauchyGreen_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftCauchyGreen
! USE Rank2Tensor_Method, ONLY: LeftStretch
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 1: b = F F^T and b = V^2
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(LeftCauchyGreen_) :: b
!
!   F = DeformationGradient(F0)
!   b = LeftCauchyGreen(F=F)
!   CALL OK(MAXVAL(ABS(b%T - MATMUL(F0, TRANSPOSE(F0)))) .LT. tol, &
!           "b from F")
!   CALL OK(b%isSym, "b from F: symmetric")
!   b = LeftCauchyGreen(V=LeftStretch(F))
!   CALL OK(MAXVAL(ABS(b%T - MATMUL(F0, TRANSPOSE(F0)))) .LT. tol, &
!           "b from V")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE LeftCauchyGreen
  MODULE PURE FUNCTION Rank2Tensor_LeftCauchyGreen(F, V) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: F
    !! deformation gradient
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: V
    !! left stretch tensor
    TYPE(LeftCauchyGreen_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_LeftCauchyGreen
END INTERFACE LeftCauchyGreen

!----------------------------------------------------------------------------
!                            LeftCauchyGreen_Pointer@CauchyGreenMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a pointer to a new [[LeftCauchyGreen_]] tensor
!
! Returns:
!   ans : CLASS(LeftCauchyGreen_), POINTER - caller must deallocate
!
! Formula:
!   $$b = F F^{T} = V^{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: LeftCauchyGreen_
! USE BaseType, ONLY: RightCauchyGreen_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftCauchyGreen_Pointer
! USE Rank2Tensor_Method, ONLY: RightCauchyGreen_Pointer
! USE Rank2Tensor_Method, ONLY: RightStretch
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 4: pointers to b and C
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   CLASS(LeftCauchyGreen_), POINTER :: pb
!   CLASS(RightCauchyGreen_), POINTER :: pc
!
!   F = DeformationGradient(F0)
!   pb => LeftCauchyGreen_Pointer(F=F)
!   CALL OK(MAXVAL(ABS(pb%T - MATMUL(F0, TRANSPOSE(F0)))) .LT. tol, &
!           "pointer: b from F")
!   CALL OK(pb%isSym, "pointer: b symmetric")
!   DEALLOCATE (pb)
!   pc => RightCauchyGreen_Pointer(U=RightStretch(F))
!   CALL OK(MAXVAL(ABS(pc%T - MATMUL(TRANSPOSE(F0), F0))) .LT. tol, &
!           "pointer: C from U")
!   CALL OK(pc%isSym, "pointer: C symmetric")
!   DEALLOCATE (pc)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE LeftCauchyGreen_Pointer
  MODULE PURE FUNCTION Rank2Tensor_LeftCauchyGreenPtr(F, V) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: F
    !! deformation gradient
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: V
    !! left stretch tensor
    CLASS(LeftCauchyGreen_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_LeftCauchyGreenPtr
END INTERFACE LeftCauchyGreen_Pointer

!----------------------------------------------------------------------------
!                                   RightCauchyGreen@CauchyGreenMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a [[RightCauchyGreen_]] tensor
!
! If `F` is present it is used, else if `U` is present it is used,
! otherwise the identity is returned.
!
! Returns:
!   ans : RightCauchyGreen_ - symmetric tensor
!
! Formula:
!   $$C = F^{T} F = U^{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: RightCauchyGreen_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: RightCauchyGreen
! USE Rank2Tensor_Method, ONLY: RightStretch
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 2: C = F^T F and C = U^2
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(RightCauchyGreen_) :: C
!
!   F = DeformationGradient(F0)
!   C = RightCauchyGreen(F=F)
!   CALL OK(MAXVAL(ABS(C%T - MATMUL(TRANSPOSE(F0), F0))) .LT. tol, &
!           "C from F")
!   CALL OK(C%isSym, "C from F: symmetric")
!   C = RightCauchyGreen(U=RightStretch(F))
!   CALL OK(MAXVAL(ABS(C%T - MATMUL(TRANSPOSE(F0), F0))) .LT. tol, &
!           "C from U")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE RightCauchyGreen
  MODULE PURE FUNCTION Rank2Tensor_RightCauchyGreen(F, U) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: F
    !! deformation gradient
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: U
    !! right stretch tensor
    TYPE(RightCauchyGreen_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_RightCauchyGreen
END INTERFACE RightCauchyGreen

!----------------------------------------------------------------------------
!                           RightCauchyGreen_Pointer@CauchyGreenMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Returns a pointer to a new [[RightCauchyGreen_]] tensor
!
! Returns:
!   ans : CLASS(RightCauchyGreen_), POINTER - caller must deallocate
!
! Formula:
!   $$C = F^{T} F = U^{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: LeftCauchyGreen_
! USE BaseType, ONLY: RightCauchyGreen_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftCauchyGreen_Pointer
! USE Rank2Tensor_Method, ONLY: RightCauchyGreen_Pointer
! USE Rank2Tensor_Method, ONLY: RightStretch
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 4: pointers to b and C
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   CLASS(LeftCauchyGreen_), POINTER :: pb
!   CLASS(RightCauchyGreen_), POINTER :: pc
!
!   F = DeformationGradient(F0)
!   pb => LeftCauchyGreen_Pointer(F=F)
!   CALL OK(MAXVAL(ABS(pb%T - MATMUL(F0, TRANSPOSE(F0)))) .LT. tol, &
!           "pointer: b from F")
!   CALL OK(pb%isSym, "pointer: b symmetric")
!   DEALLOCATE (pb)
!   pc => RightCauchyGreen_Pointer(U=RightStretch(F))
!   CALL OK(MAXVAL(ABS(pc%T - MATMUL(TRANSPOSE(F0), F0))) .LT. tol, &
!           "pointer: C from U")
!   CALL OK(pc%isSym, "pointer: C symmetric")
!   DEALLOCATE (pc)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE RightCauchyGreen_Pointer
  MODULE PURE FUNCTION Rank2Tensor_RightCauchyGreenPtr(F, U) RESULT(ans)
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: F
    !! deformation gradient
    CLASS(Rank2Tensor_), OPTIONAL, INTENT(IN) :: U
    !! right stretch tensor
    CLASS(RightCauchyGreen_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_RightCauchyGreenPtr
END INTERFACE RightCauchyGreen_Pointer

!----------------------------------------------------------------------------
!                                               GreenStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Green-Lagrange strain from a deformation gradient
!
! Returns:
!   ans : GreenStrain_ - symmetric tensor
!
! Formula:
!   $$E = \frac{1}{2}\left(F^{T} F - I\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: GreenStrain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: GreenStrain
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: uniaxial stretch, E11 = (2^2 - 1) / 2
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(GreenStrain_) :: E
!   REAL(DFP) :: mat(3, 3), expected(3, 3)
!
!   mat = Eye3
!   mat(1, 1) = 2.0_DFP
!   F = DeformationGradient(mat)
!   E = GreenStrain(F)
!   expected = 0.0_DFP
!   expected(1, 1) = 1.5_DFP
!   CALL OK(MAXVAL(ABS(E%T - expected)) .LT. tol, "uniaxial: E11 = 1.5")
!   CALL OK(E%isSym, "uniaxial: symmetric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE GreenStrain
  MODULE PURE FUNCTION Rank2Tensor_GreenStrain1(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(GreenStrain_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_GreenStrain1
END INTERFACE GreenStrain

!----------------------------------------------------------------------------
!                                               GreenStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Green-Lagrange strain from the right Cauchy-Green tensor
!
! Returns:
!   ans : GreenStrain_ - symmetric tensor
!
! Formula:
!   $$E = \frac{1}{2}\left(C - I\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: GreenStrain_
! USE BaseType, ONLY: RightCauchyGreen_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: GreenStrain
! USE Rank2Tensor_Method, ONLY: RightCauchyGreen
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 2: E = (F^T F - I) / 2 and E(C) = E(F)
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(RightCauchyGreen_) :: C
!   TYPE(GreenStrain_) :: E
!   REAL(DFP) :: expected(3, 3)
!
!   expected = 0.5_DFP * (MATMUL(TRANSPOSE(F0), F0) - Eye3)
!   F = DeformationGradient(F0)
!   E = GreenStrain(F)
!   CALL OK(MAXVAL(ABS(E%T - expected)) .LT. tol, "general: E from F")
!   C = RightCauchyGreen(F=F)
!   E = GreenStrain(C)
!   CALL OK(MAXVAL(ABS(E%T - expected)) .LT. tol, "general: E from C")
!   CALL OK(E%isSym, "general: E from C symmetric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE GreenStrain
  MODULE PURE FUNCTION Rank2Tensor_GreenStrain2(C) RESULT(ans)
    CLASS(RightCauchyGreen_), INTENT(IN) :: C
    !! right Cauchy-Green tensor
    TYPE(GreenStrain_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_GreenStrain2
END INTERFACE GreenStrain

!----------------------------------------------------------------------------
!                                       GreenStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new Green-Lagrange strain from F
!
! Returns:
!   ans : CLASS(GreenStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$E = \frac{1}{2}\left(F^{T} F - I\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: GreenStrain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: GreenStrain_Pointer
! USE Rank2Tensor_Method, ONLY: RightCauchyGreen
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 3: pointers from F and from C
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   CLASS(GreenStrain_), POINTER :: ptr
!   REAL(DFP) :: expected(3, 3)
!
!   expected = 0.5_DFP * (MATMUL(TRANSPOSE(F0), F0) - Eye3)
!   F = DeformationGradient(F0)
!   ptr => GreenStrain_Pointer(F)
!   CALL OK(MAXVAL(ABS(ptr%T - expected)) .LT. tol, "pointer: from F")
!   CALL OK(ptr%isSym, "pointer: from F symmetric")
!   DEALLOCATE (ptr)
!   ptr => GreenStrain_Pointer(RightCauchyGreen(F=F))
!   CALL OK(MAXVAL(ABS(ptr%T - expected)) .LT. tol, "pointer: from C")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE GreenStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_GreenStrainPtr1(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    CLASS(GreenStrain_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_GreenStrainPtr1
END INTERFACE GreenStrain_Pointer

!----------------------------------------------------------------------------
!                                       GreenStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new Green-Lagrange strain from C
!
! Returns:
!   ans : CLASS(GreenStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$E = \frac{1}{2}\left(C - I\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: GreenStrain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: GreenStrain_Pointer
! USE Rank2Tensor_Method, ONLY: RightCauchyGreen
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 3: pointers from F and from C
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   CLASS(GreenStrain_), POINTER :: ptr
!   REAL(DFP) :: expected(3, 3)
!
!   expected = 0.5_DFP * (MATMUL(TRANSPOSE(F0), F0) - Eye3)
!   F = DeformationGradient(F0)
!   ptr => GreenStrain_Pointer(F)
!   CALL OK(MAXVAL(ABS(ptr%T - expected)) .LT. tol, "pointer: from F")
!   CALL OK(ptr%isSym, "pointer: from F symmetric")
!   DEALLOCATE (ptr)
!   ptr => GreenStrain_Pointer(RightCauchyGreen(F=F))
!   CALL OK(MAXVAL(ABS(ptr%T - expected)) .LT. tol, "pointer: from C")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE GreenStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_GreenStrainPtr2(C) RESULT(ans)
    CLASS(RightCauchyGreen_), INTENT(IN) :: C
    !! right Cauchy-Green tensor
    CLASS(GreenStrain_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_GreenStrainPtr2
END INTERFACE GreenStrain_Pointer

!----------------------------------------------------------------------------
!                                             AlmansiStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Euler-Almansi strain from a deformation gradient
!
! Returns:
!   ans : AlmansiStrain_ - symmetric tensor
!
! Formula:
!   $$e = \frac{1}{2}\left(I - F^{-T} F^{-1}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: AlmansiStrain_
! USE BaseType, ONLY: DeformationGradient_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: AlmansiStrain
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: uniaxial stretch, e11 = (1 - 1 / 2^2) / 2
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(AlmansiStrain_) :: e
!   REAL(DFP) :: mat(3, 3), expected(3, 3)
!
!   mat = Eye3
!   mat(1, 1) = 2.0_DFP
!   F = DeformationGradient(mat)
!   e = AlmansiStrain(F)
!   expected = 0.0_DFP
!   expected(1, 1) = 0.375_DFP
!   CALL OK(MAXVAL(ABS(e%T - expected)) .LT. tol, "uniaxial: e11")
!   CALL OK(e%isSym, "uniaxial: symmetric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE AlmansiStrain
  MODULE PURE FUNCTION Rank2Tensor_AlmansiStrain1(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(AlmansiStrain_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_AlmansiStrain1
END INTERFACE AlmansiStrain

!----------------------------------------------------------------------------
!                                             AlmansiStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Euler-Almansi strain from the left Cauchy-Green tensor
!
! Returns:
!   ans : AlmansiStrain_ - symmetric tensor
!
! Formula:
!   $$e = \frac{1}{2}\left(I - b^{-1}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: AlmansiStrain_
! USE BaseType, ONLY: DeformationGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: AlmansiStrain
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftCauchyGreen
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 3: e(b) = e(F)
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(AlmansiStrain_) :: e1, e2
!
!   F = DeformationGradient(F0)
!   e1 = AlmansiStrain(F)
!   e2 = AlmansiStrain(LeftCauchyGreen(F=F))
!   CALL OK(MAXVAL(ABS(e2%T - e1%T)) .LT. tol, "general: e(b) = e(F)")
!   CALL OK(e2%isSym, "general: e(b) symmetric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE AlmansiStrain
  MODULE PURE FUNCTION Rank2Tensor_AlmansiStrain2(B) RESULT(ans)
    CLASS(LeftCauchyGreen_), INTENT(IN) :: B
    !! left Cauchy-Green tensor
    TYPE(AlmansiStrain_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_AlmansiStrain2
END INTERFACE AlmansiStrain

!----------------------------------------------------------------------------
!                                     AlmansiStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new Euler-Almansi strain from F
!
! Returns:
!   ans : CLASS(AlmansiStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$e = \frac{1}{2}\left(I - F^{-T} F^{-1}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: AlmansiStrain_
! USE BaseType, ONLY: DeformationGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: AlmansiStrain
! USE Rank2Tensor_Method, ONLY: AlmansiStrain_Pointer
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftCauchyGreen
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 4: pointers from F and from b
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(AlmansiStrain_) :: e
!   CLASS(AlmansiStrain_), POINTER :: ptr
!
!   F = DeformationGradient(F0)
!   e = AlmansiStrain(F)
!   ptr => AlmansiStrain_Pointer(F)
!   CALL OK(MAXVAL(ABS(ptr%T - e%T)) .LT. tol, "pointer: from F")
!   CALL OK(ptr%isSym, "pointer: from F symmetric")
!   DEALLOCATE (ptr)
!   ptr => AlmansiStrain_Pointer(LeftCauchyGreen(F=F))
!   CALL OK(MAXVAL(ABS(ptr%T - e%T)) .LT. tol, "pointer: from b")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE AlmansiStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_AlmansiStrainPtr1(F) RESULT(ans)
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    CLASS(AlmansiStrain_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_AlmansiStrainPtr1
END INTERFACE AlmansiStrain_Pointer

!----------------------------------------------------------------------------
!                                     AlmansiStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new Euler-Almansi strain from b
!
! Returns:
!   ans : CLASS(AlmansiStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$e = \frac{1}{2}\left(I - b^{-1}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: AlmansiStrain_
! USE BaseType, ONLY: DeformationGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: AlmansiStrain
! USE Rank2Tensor_Method, ONLY: AlmansiStrain_Pointer
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: LeftCauchyGreen
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: F0(3, 3) = RESHAPE( &
!                         [1.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, 0.9_DFP, 0.2_DFP, &
!                          0.0_DFP, -0.1_DFP, 1.1_DFP], [3, 3])
!
! ! Test 4: pointers from F and from b
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(AlmansiStrain_) :: e
!   CLASS(AlmansiStrain_), POINTER :: ptr
!
!   F = DeformationGradient(F0)
!   e = AlmansiStrain(F)
!   ptr => AlmansiStrain_Pointer(F)
!   CALL OK(MAXVAL(ABS(ptr%T - e%T)) .LT. tol, "pointer: from F")
!   CALL OK(ptr%isSym, "pointer: from F symmetric")
!   DEALLOCATE (ptr)
!   ptr => AlmansiStrain_Pointer(LeftCauchyGreen(F=F))
!   CALL OK(MAXVAL(ABS(ptr%T - e%T)) .LT. tol, "pointer: from b")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE AlmansiStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_AlmansiStrainPtr2(B) RESULT(ans)
    CLASS(LeftCauchyGreen_), INTENT(IN) :: B
    !! left Cauchy-Green tensor
    CLASS(AlmansiStrain_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_AlmansiStrainPtr2
END INTERFACE AlmansiStrain_Pointer

!----------------------------------------------------------------------------
!                                               SmallStrain@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Infinitesimal strain from a displacement gradient
!
! Returns:
!   ans : SmallStrain_ - symmetric tensor
!
! Formula:
!   $$\varepsilon = \frac{1}{2}\left(\nabla u + \nabla u^{T}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: SmallStrain_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: SmallStrain
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: gradU(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, -0.1_DFP, 0.2_DFP, &
!                          0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
! REAL(DFP), PARAMETER :: eps(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.2_DFP, 0.2_DFP, &
!                          0.2_DFP, -0.1_DFP, 0.05_DFP, &
!                          0.2_DFP, 0.05_DFP, 0.1_DFP], [3, 3])
!
! ! Test 1: symmetric part of the displacement gradient
! BLOCK
!   TYPE(SmallStrain_) :: e
!
!   e = SmallStrain(gradU)
!   CALL OK(MAXVAL(ABS(e%T - eps)) .LT. tol, "SmallStrain: components")
!   CALL OK(e%isSym, "SmallStrain: symmetric")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE SmallStrain
  MODULE PURE FUNCTION Rank2Tensor_SmallStrain(gradU) RESULT(ans)
    REAL(DFP), INTENT(IN) :: gradU(3, 3)
    !! displacement gradient $\partial u_i / \partial x_j$, SHAPE(3,3)
    TYPE(SmallStrain_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_SmallStrain
END INTERFACE SmallStrain

!----------------------------------------------------------------------------
!                                       SmallStrain_Pointer@StrainMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new infinitesimal strain
!
! Returns:
!   ans : CLASS(SmallStrain_), POINTER - caller must deallocate
!
! Formula:
!   $$\varepsilon = \frac{1}{2}\left(\nabla u + \nabla u^{T}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: SmallStrain_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: SmallStrain_Pointer
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: gradU(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, -0.1_DFP, 0.2_DFP, &
!                          0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
! REAL(DFP), PARAMETER :: eps(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.2_DFP, 0.2_DFP, &
!                          0.2_DFP, -0.1_DFP, 0.05_DFP, &
!                          0.2_DFP, 0.05_DFP, 0.1_DFP], [3, 3])
!
! ! Test 3: pointer
! BLOCK
!   CLASS(SmallStrain_), POINTER :: ptr
!
!   ptr => SmallStrain_Pointer(gradU)
!   CALL OK(MAXVAL(ABS(ptr%T - eps)) .LT. tol, "pointer: components")
!   CALL OK(ptr%isSym, "pointer: symmetric")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE SmallStrain_Pointer
  MODULE PURE FUNCTION Rank2Tensor_SmallStrainPtr(gradU) RESULT(ans)
    REAL(DFP), INTENT(IN) :: gradU(3, 3)
    !! displacement gradient, SHAPE(3,3)
    CLASS(SmallStrain_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_SmallStrainPtr
END INTERFACE SmallStrain_Pointer

!----------------------------------------------------------------------------
!                                 VelocityGradient@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[VelocityGradient_]]
!
! Returns:
!   ans : VelocityGradient_
!
! Formula:
!   $$L_{ij} = \frac{\partial v_i}{\partial x_j}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VelocityGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: VelocityGradient
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: L0(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, -0.1_DFP, 0.2_DFP, &
!                          0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
!
! ! Test 1: from a matrix
! BLOCK
!   TYPE(VelocityGradient_) :: L
!
!   L = VelocityGradient(L0)
!   CALL OK(MAXVAL(ABS(L%T - L0)) .LT. tol, "VelocityGradient: components")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE VelocityGradient
  MODULE PURE FUNCTION Rank2Tensor_VelocityGradient(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! components $\partial v_i/\partial x_j$, SHAPE(3,3)
    TYPE(VelocityGradient_) :: ans
    !! velocityGradient_
  END FUNCTION Rank2Tensor_VelocityGradient
END INTERFACE VelocityGradient

!----------------------------------------------------------------------------
!                         VelocityGradient_Pointer@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[VelocityGradient_]]
!
! Returns:
!   ans : CLASS(VelocityGradient_), POINTER - caller must deallocate
!
! Formula:
!   $$L_{ij} = \frac{\partial v_i}{\partial x_j}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VelocityGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: VelocityGradient_Pointer
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: L0(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, -0.1_DFP, 0.2_DFP, &
!                          0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
!
! ! Test 2: pointer from a matrix
! BLOCK
!   CLASS(VelocityGradient_), POINTER :: ptr
!
!   ptr => VelocityGradient_Pointer(L0)
!   CALL OK(ASSOCIATED(ptr), "pointer: associated")
!   CALL OK(MAXVAL(ABS(ptr%T - L0)) .LT. tol, "pointer: components")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE VelocityGradient_Pointer
  MODULE PURE FUNCTION Rank2Tensor_VelocityGradientPtr(mat) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! components $\partial v_i/\partial x_j$, SHAPE(3,3)
    CLASS(VelocityGradient_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_VelocityGradientPtr
END INTERFACE VelocityGradient_Pointer

!----------------------------------------------------------------------------
!                                       StrainRate@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rate of deformation from a velocity gradient
!
! Returns:
!   ans : StrainRate_ - symmetric tensor
!
! Formula:
!   $$D = \frac{1}{2}\left(L + L^{T}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: StrainRate_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: StrainRate
! USE Rank2Tensor_Method, ONLY: VelocityGradient
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: L0(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, -0.1_DFP, 0.2_DFP, &
!                          0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
!
! ! Test 1: D is the symmetric part of L
! BLOCK
!   TYPE(StrainRate_) :: D
!   REAL(DFP) :: expected(3, 3)
!
!   expected = RESHAPE([0.2_DFP, 0.2_DFP, 0.2_DFP, &
!                       0.2_DFP, -0.1_DFP, 0.05_DFP, &
!                       0.2_DFP, 0.05_DFP, 0.1_DFP], [3, 3])
!   D = StrainRate(VelocityGradient(L0))
!   CALL OK(MAXVAL(ABS(D%T - expected)) .LT. tol, "D: components")
!   CALL OK(D%isSym, "D: symmetric flag")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE StrainRate
  MODULE PURE FUNCTION Rank2Tensor_StrainRate(L) RESULT(ans)
    CLASS(VelocityGradient_), INTENT(IN) :: L
    !! velocity gradient
    TYPE(StrainRate_) :: ans
    !! symmetric tensor
  END FUNCTION Rank2Tensor_StrainRate
END INTERFACE StrainRate

!----------------------------------------------------------------------------
!                               StrainRate_Pointer@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new rate of deformation tensor
!
! Returns:
!   ans : CLASS(StrainRate_), POINTER - caller must deallocate
!
! Formula:
!   $$D = \frac{1}{2}\left(L + L^{T}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: ContinuumSpin_
! USE BaseType, ONLY: StrainRate_
! USE BaseType, ONLY: VelocityGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: ContinuumSpin_Pointer
! USE Rank2Tensor_Method, ONLY: StrainRate_Pointer
! USE Rank2Tensor_Method, ONLY: VelocityGradient
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: L0(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, -0.1_DFP, 0.2_DFP, &
!                          0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
!
! ! Test 4: pointers to D and W
! BLOCK
!   TYPE(VelocityGradient_) :: L
!   CLASS(StrainRate_), POINTER :: pD
!   CLASS(ContinuumSpin_), POINTER :: pW
!
!   L = VelocityGradient(L0)
!   pD => StrainRate_Pointer(L)
!   pW => ContinuumSpin_Pointer(L)
!   CALL OK(MAXVAL(ABS(pD%T - 0.5_DFP * (L0 + TRANSPOSE(L0)))) .LT. tol, &
!           "pointer: D")
!   CALL OK(pD%isSym, "pointer: D symmetric flag")
!   CALL OK(MAXVAL(ABS(pW%T - 0.5_DFP * (L0 - TRANSPOSE(L0)))) .LT. tol, &
!           "pointer: W")
!   CALL OK(.NOT. pW%isSym, "pointer: W not symmetric flag")
!   DEALLOCATE (pD)
!   DEALLOCATE (pW)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE StrainRate_Pointer
  MODULE PURE FUNCTION Rank2Tensor_StrainRatePtr(L) RESULT(ans)
    CLASS(VelocityGradient_), INTENT(IN) :: L
    !! velocity gradient
    CLASS(StrainRate_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_StrainRatePtr
END INTERFACE StrainRate_Pointer

!----------------------------------------------------------------------------
!                                    ContinuumSpin@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Spin tensor from a velocity gradient
!
! Returns:
!   ans : ContinuumSpin_ - skew-symmetric tensor
!
! Formula:
!   $$W = \frac{1}{2}\left(L - L^{T}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: ContinuumSpin_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: ContinuumSpin
! USE Rank2Tensor_Method, ONLY: VelocityGradient
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: L0(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, -0.1_DFP, 0.2_DFP, &
!                          0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
!
! ! Test 2: W is the skew part of L
! BLOCK
!   TYPE(ContinuumSpin_) :: W
!   REAL(DFP) :: expected(3, 3)
!
!   expected = RESHAPE([0.0_DFP, -0.1_DFP, -0.2_DFP, &
!                       0.1_DFP, 0.0_DFP, 0.15_DFP, &
!                       0.2_DFP, -0.15_DFP, 0.0_DFP], [3, 3])
!   W = ContinuumSpin(VelocityGradient(L0))
!   CALL OK(MAXVAL(ABS(W%T - expected)) .LT. tol, "W: components")
!   CALL OK(MAXVAL(ABS(W%T + TRANSPOSE(W%T))) .LT. tol, "W: skew")
!   CALL OK(.NOT. W%isSym, "W: not symmetric flag")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE ContinuumSpin
  MODULE PURE FUNCTION Rank2Tensor_ContinuumSpin(L) RESULT(ans)
    CLASS(VelocityGradient_), INTENT(IN) :: L
    !! velocity gradient
    TYPE(ContinuumSpin_) :: ans
    !! skew-symmetric tensor
  END FUNCTION Rank2Tensor_ContinuumSpin
END INTERFACE ContinuumSpin

!----------------------------------------------------------------------------
!                            ContinuumSpin_Pointer@VelocityGradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Pointer to a new spin tensor
!
! Returns:
!   ans : CLASS(ContinuumSpin_), POINTER - caller must deallocate
!
! Formula:
!   $$W = \frac{1}{2}\left(L - L^{T}\right)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: ContinuumSpin_
! USE BaseType, ONLY: StrainRate_
! USE BaseType, ONLY: VelocityGradient_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: ContinuumSpin_Pointer
! USE Rank2Tensor_Method, ONLY: StrainRate_Pointer
! USE Rank2Tensor_Method, ONLY: VelocityGradient
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: L0(3, 3) = RESHAPE( &
!                         [0.2_DFP, 0.1_DFP, 0.0_DFP, &
!                          0.3_DFP, -0.1_DFP, 0.2_DFP, &
!                          0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])
!
! ! Test 4: pointers to D and W
! BLOCK
!   TYPE(VelocityGradient_) :: L
!   CLASS(StrainRate_), POINTER :: pD
!   CLASS(ContinuumSpin_), POINTER :: pW
!
!   L = VelocityGradient(L0)
!   pD => StrainRate_Pointer(L)
!   pW => ContinuumSpin_Pointer(L)
!   CALL OK(MAXVAL(ABS(pD%T - 0.5_DFP * (L0 + TRANSPOSE(L0)))) .LT. tol, &
!           "pointer: D")
!   CALL OK(pD%isSym, "pointer: D symmetric flag")
!   CALL OK(MAXVAL(ABS(pW%T - 0.5_DFP * (L0 - TRANSPOSE(L0)))) .LT. tol, &
!           "pointer: W")
!   CALL OK(.NOT. pW%isSym, "pointer: W not symmetric flag")
!   DEALLOCATE (pD)
!   DEALLOCATE (pW)
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE ContinuumSpin_Pointer
  MODULE PURE FUNCTION Rank2Tensor_ContinuumSpinPtr(L) RESULT(ans)
    CLASS(VelocityGradient_), INTENT(IN) :: L
    !! velocity gradient
    CLASS(ContinuumSpin_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_ContinuumSpinPtr
END INTERFACE ContinuumSpin_Pointer

!----------------------------------------------------------------------------
!                                                    Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[Stress_]] from a 3x3 matrix
!
! `isSym` of the result is true when `mat` is symmetric within a
! relative tolerance of 1e-12.
!
! Returns:
!   ans : Stress_
!
! Formula:
!   $$\sigma_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureCauchy
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 1: from a symmetric matrix
! BLOCK
!   TYPE(Stress_) :: sig
!
!   sig = Stress(s0, StressMeasureCauchy)
!   CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "mat: components")
!   CALL OK(sig%isSym, "mat: isSym is true")
!   CALL OK(sig%stressType .EQ. StressMeasureCauchy, "mat: stressType")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/Stress_test_1.F90

INTERFACE Stress
  MODULE PURE FUNCTION Rank2Tensor_Stress1(mat, stressType) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! stress components, SHAPE(3,3)
    INTEGER(I4B), INTENT(IN) :: stressType
    !! one of the `StressMeasure*` constants
    TYPE(Stress_) :: ans
    !! stress_
  END FUNCTION Rank2Tensor_Stress1
END INTERFACE Stress

!----------------------------------------------------------------------------
!                                                    Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a [[Stress_]] from a [[Rank2Tensor_]]
!
! Returns:
!   ans : Stress_
!
! Formula:
!   $$\sigma_{ij} = T_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasurePK2
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 3: from a Rank2Tensor_
! BLOCK
!   TYPE(Stress_) :: sig
!
!   sig = Stress(Rank2Tensor(s0, .TRUE.), StressMeasurePK2)
!   CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "Rank2Tensor: components")
!   CALL OK(sig%isSym, "Rank2Tensor: isSym is true")
!   CALL OK(sig%stressType .EQ. StressMeasurePK2, "Rank2Tensor: stressType")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/Stress_test_1.F90

INTERFACE Stress
  MODULE PURE FUNCTION Rank2Tensor_Stress2(T, stressType) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    !! stress components
    INTEGER(I4B), INTENT(IN) :: stressType
    !! one of the `StressMeasure*` constants
    TYPE(Stress_) :: ans
    !! stress_
  END FUNCTION Rank2Tensor_Stress2
END INTERFACE Stress

!----------------------------------------------------------------------------
!                                                    Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a symmetric [[Stress_]] from a [[VoigtRank2Tensor_]]
!
! Returns:
!   ans : Stress_ - symmetric
!
! Formula:
!   $$\sigma_{12} = \sigma_{21} = \mathrm{Scale}\, V_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StrainTypeVoigt
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasureMandel
! USE GlobalData, ONLY: StressTypeVoigt
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 4: from a stress-like and a strain-like VoigtRank2Tensor_
! BLOCK
!   TYPE(Stress_) :: sig
!
!   sig = Stress(VoigtRank2Tensor(s0, StressTypeVoigt), StressMeasureMandel)
!   CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "Voigt stress: components")
!   CALL OK(sig%isSym, "Voigt stress: isSym is true")
!   CALL OK(sig%stressType .EQ. StressMeasureMandel, "Voigt: stressType")
!   sig = Stress(VoigtRank2Tensor(s0, StrainTypeVoigt), StressMeasureCauchy)
!   CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "Voigt strain: components")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/Stress_test_1.F90

INTERFACE Stress
  MODULE PURE FUNCTION Rank2Tensor_Stress3(V, stressType) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    !! stress in Voigt notation
    INTEGER(I4B), INTENT(IN) :: stressType
    !! one of the `StressMeasure*` constants
    TYPE(Stress_) :: ans
    !! symmetric
  END FUNCTION Rank2Tensor_Stress3
END INTERFACE Stress

!----------------------------------------------------------------------------
!                                            Stress_Pointer@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[Stress_]] from a 3x3 matrix
!
! Returns:
!   ans : CLASS(Stress_), POINTER - caller must deallocate
!
! Formula:
!   $$\sigma_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureKirchhoff
! USE Rank2Tensor_Method, ONLY: Stress_Pointer
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 5: Stress_Pointer from a matrix
! BLOCK
!   CLASS(Stress_), POINTER :: ptr
!
!   ptr => Stress_Pointer(s0, StressMeasureKirchhoff)
!   CALL OK(ASSOCIATED(ptr), "pointer mat: associated")
!   CALL OK(MAXVAL(ABS(ptr%T - s0)) .LT. tol, "pointer mat: components")
!   CALL OK(ptr%isSym, "pointer mat: isSym is true")
!   CALL OK(ptr%stressType .EQ. StressMeasureKirchhoff, &
!           "pointer mat: stressType")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/Stress_test_1.F90

INTERFACE Stress_Pointer
  MODULE PURE FUNCTION Rank2Tensor_StressPtr1(mat, stressType) RESULT(ans)
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! stress components, SHAPE(3,3)
    INTEGER(I4B), INTENT(IN) :: stressType
    !! one of the `StressMeasure*` constants
    CLASS(Stress_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_StressPtr1
END INTERFACE Stress_Pointer

!----------------------------------------------------------------------------
!                                            Stress_Pointer@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[Stress_]] from a tensor
!
! Returns:
!   ans : CLASS(Stress_), POINTER - caller must deallocate
!
! Formula:
!   $$\sigma_{ij} = T_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasurePK2
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Rank2Tensor_Method, ONLY: Stress_Pointer
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 6: Stress_Pointer from a Rank2Tensor_
! BLOCK
!   CLASS(Stress_), POINTER :: ptr
!
!   ptr => Stress_Pointer(Rank2Tensor(s0, .TRUE.), StressMeasurePK2)
!   CALL OK(MAXVAL(ABS(ptr%T - s0)) .LT. tol, "pointer T: components")
!   CALL OK(ptr%stressType .EQ. StressMeasurePK2, "pointer T: stressType")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/Stress_test_1.F90

INTERFACE Stress_Pointer
  MODULE PURE FUNCTION Rank2Tensor_StressPtr2(T, stressType) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: T
    !! stress components
    INTEGER(I4B), INTENT(IN) :: stressType
    !! one of the `StressMeasure*` constants
    CLASS(Stress_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_StressPtr2
END INTERFACE Stress_Pointer

!----------------------------------------------------------------------------
!                                            Stress_Pointer@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns a pointer to a new [[Stress_]] from a Voigt tensor
!
! Returns:
!   ans : CLASS(Stress_), POINTER - caller must deallocate
!
! Formula:
!   $$\sigma_{12} = \sigma_{21} = \mathrm{Scale}\, V_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasurePK1
! USE GlobalData, ONLY: StressTypeVoigt
! USE Rank2Tensor_Method, ONLY: Stress_Pointer
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 7: Stress_Pointer from a VoigtRank2Tensor_
! BLOCK
!   CLASS(Stress_), POINTER :: ptr
!
!   ptr => Stress_Pointer(VoigtRank2Tensor(s0, StressTypeVoigt), &
!                         StressMeasurePK1)
!   CALL OK(MAXVAL(ABS(ptr%T - s0)) .LT. tol, "pointer Voigt: components")
!   CALL OK(ptr%isSym, "pointer Voigt: isSym is true")
!   CALL OK(ptr%stressType .EQ. StressMeasurePK1, &
!           "pointer Voigt: stressType")
!   DEALLOCATE (ptr)
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/Stress_test_1.F90

INTERFACE Stress_Pointer
  MODULE PURE FUNCTION Rank2Tensor_StressPtr3(V, stressType) RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: V
    !! stress in Voigt notation
    INTEGER(I4B), INTENT(IN) :: stressType
    !! one of the `StressMeasure*` constants
    CLASS(Stress_), POINTER :: ans
    !! caller must deallocate
  END FUNCTION Rank2Tensor_StressPtr3
END INTERFACE Stress_Pointer

!----------------------------------------------------------------------------
!                                                  Initiate@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a [[Stress_]] from a 3x3 matrix
!
! Formula:
!   $$\sigma_{ij} = \mathrm{mat}_{ij}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasurePK1
! USE Rank2Tensor_Method, ONLY: Initiate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
!                                             4.0_DFP, 2.0_DFP, 5.0_DFP, &
!                                             6.0_DFP, 5.0_DFP, 3.0_DFP], &
!                                            [3, 3])
!
! ! Test 5: a Stress from a matrix and a stress measure
! BLOCK
!   TYPE(Stress_) :: sig
!   REAL(DFP) :: mat(3, 3)
!
!   CALL Initiate(sig, S0, StressMeasureCauchy)
!   CALL OK(MAXVAL(ABS(sig%T - S0)) .LT. tol, "stress: T")
!   CALL OK(sig%isSym, "stress: symmetric matrix gives isSym")
!   CALL OK(sig%stressType .EQ. StressMeasureCauchy, "stress: stressType")
!   mat = S0
!   mat(1, 2) = 0.0_DFP
!   CALL Initiate(sig, mat, StressMeasurePK1)
!   CALL OK(.NOT. sig%isSym, "stress: non-symmetric matrix")
!   CALL OK(sig%stressType .EQ. StressMeasurePK1, "stress: PK1")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Rank2Tensor_InitiateStress(obj, mat, stressType)
    CLASS(Stress_), INTENT(INOUT) :: obj
    !! stress to initiate
    REAL(DFP), INTENT(IN) :: mat(3, 3)
    !! stress components, SHAPE(3,3)
    INTEGER(I4B), INTENT(IN) :: stressType
    !! one of the `StressMeasure*` constants
  END SUBROUTINE Rank2Tensor_InitiateStress
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                             GetStressType@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns the stress measure of a [[Stress_]]
!
! Returns:
!   ans : INTEGER(I4B) - one of the `StressMeasure*` constants
!
! Formula:
!   $$\mathrm{ans} = \mathrm{obj\%stressType}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureKirchhoff
! USE GlobalData, ONLY: StressMeasurePK1
! USE Rank2Tensor_Method, ONLY: GetStressType
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 1: GetStressType returns the measure given to the constructor
! BLOCK
!   TYPE(Stress_) :: sig
!
!   sig = Stress(s0, StressMeasureKirchhoff)
!   CALL OK(GetStressType(sig) .EQ. StressMeasureKirchhoff, &
!           "GetStressType: Kirchhoff")
!   sig = Stress(s0, StressMeasurePK1)
!   CALL OK(GetStressType(sig) .EQ. StressMeasurePK1, &
!           "GetStressType: PK1")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE GetStressType
  MODULE PURE FUNCTION Rank2Tensor_GetStressType(obj) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    !! stress
    INTEGER(I4B) :: ans
    !! one of the `StressMeasure*` constants
  END FUNCTION Rank2Tensor_GetStressType
END INTERFACE GetStressType

!----------------------------------------------------------------------------
!                                             SetStressType@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Sets the stress measure of a [[Stress_]]
!
! Only the label is changed, the components are not converted. Use
! [[ConvertStress]] to change the measure.
!
! Formula:
!   $$\mathrm{obj\%stressType} = \mathrm{stressType}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasureMandel
! USE Rank2Tensor_Method, ONLY: GetStressType
! USE Rank2Tensor_Method, ONLY: SetStressType
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 3: SetStressType changes the label but not the components
! BLOCK
!   TYPE(Stress_) :: sig
!
!   sig = Stress(s0, StressMeasureCauchy)
!   CALL SetStressType(sig, StressMeasureMandel)
!   CALL OK(GetStressType(sig) .EQ. StressMeasureMandel, &
!           "SetStressType: new measure")
!   CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, &
!           "SetStressType: components unchanged")
!   CALL OK(sig%isSym, "SetStressType: isSym unchanged")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE SetStressType
  MODULE PURE SUBROUTINE Rank2Tensor_SetStressType(obj, stressType)
    CLASS(Stress_), INTENT(INOUT) :: obj
    !! stress
    INTEGER(I4B), INTENT(IN) :: stressType
    !! one of the `StressMeasure*` constants
  END SUBROUTINE Rank2Tensor_SetStressType
END INTERFACE SetStressType

!----------------------------------------------------------------------------
!                                             ConvertStress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Converts a stress to another stress measure
!
! The conversion goes through the Kirchhoff stress $\tau$. In debug
! mode an unknown stress measure (of obj or of the target) stops the
! program; in release mode it is not checked.
!
! | measure   | to $\tau$             | from $\tau$            |
! | --------- | --------------------- | ---------------------- |
! | Cauchy    | $J\sigma$             | $\tau / J$             |
! | Kirchhoff | $\tau$                | $\tau$                 |
! | PK1       | $P F^{T}$             | $\tau F^{-T}$          |
! | PK2       | $F S F^{T}$           | $F^{-1}\tau F^{-T}$    |
! | Mandel    | $F^{-T} M F^{T}$      | $F^{T}\tau F^{-T}$     |
!
! Changes:
!   - 2026-11-13: the check of the stress measure is done only in debug
!     mode
!
! Returns:
!   ans : Stress_ - stress in the target measure
!
! Formula:
!   $$\tau = J\sigma = P F^{T} = F S F^{T},\quad M = F^{T} P = C S$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasureMandel
! USE Rank2Tensor_Method, ONLY: ConvertStress
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
!
! ! Test 5: with F = I all measures are equal
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Stress_) :: sig, X
!   INTEGER(I4B) :: a
!   LOGICAL(LGT) :: isok
!
!   F = DeformationGradient(Eye3)
!   sig = Stress(s0, StressMeasureCauchy)
!   isok = .TRUE.
!   DO a = StressMeasureCauchy, StressMeasureMandel
!     X = ConvertStress(sig, F, a)
!     isok = isok .AND. MAXVAL(ABS(X%T - s0)) .LT. tol
!     isok = isok .AND. X%stressType .EQ. a
!   END DO
!   CALL OK(isok, "F = I: all measures equal sigma")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/ConvertStress_test_1.F90

INTERFACE ConvertStress
  MODULE FUNCTION Rank2Tensor_ConvertStress(obj, F, stressType) &
    RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    !! stress in any measure
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    INTEGER(I4B), INTENT(IN) :: stressType
    !! target `StressMeasure*` constant
    TYPE(Stress_) :: ans
    !! stress in the target measure
  END FUNCTION Rank2Tensor_ConvertStress
END INTERFACE ConvertStress

!----------------------------------------------------------------------------
!                                              CauchyStress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Cauchy stress from any stress measure
!
! Returns:
!   ans : Stress_ - Cauchy stress
!
! Formula:
!   $$\sigma = J^{-1}\tau$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureCauchy
! USE Rank2Tensor_Method, ONLY: CauchyStress
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: PK2Stress
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
! REAL(DFP), PARAMETER :: f0(3, 3) = RESHAPE( &
!                         [2.0_DFP, 0.0_DFP, 0.0_DFP, &
!                          0.5_DFP, 1.0_DFP, 0.0_DFP, &
!                          0.0_DFP, 0.2_DFP, 1.0_DFP], [3, 3])
!
! ! Test 4: Cauchy stress back from PK2
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Stress_) :: S, sig
!
!   F = DeformationGradient(f0)
!   S = PK2Stress(Stress(s0, StressMeasureCauchy), F)
!   sig = CauchyStress(S, F)
!   CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "S -> sigma: components")
!   CALL OK(sig%stressType .EQ. StressMeasureCauchy, &
!           "S -> sigma: stressType")
!   CALL OK(sig%isSym, "S -> sigma: isSym is true")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/ConvertStress_test_1.F90

INTERFACE CauchyStress
  MODULE FUNCTION Rank2Tensor_CauchyStress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    !! stress in any measure
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(Stress_) :: ans
    !! cauchy stress
  END FUNCTION Rank2Tensor_CauchyStress
END INTERFACE CauchyStress

!----------------------------------------------------------------------------
!                                           KirchhoffStress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Kirchhoff stress from any stress measure
!
! Returns:
!   ans : Stress_ - Kirchhoff stress
!
! Formula:
!   $$\tau = J\sigma$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasureKirchhoff
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: KirchhoffStress
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
! REAL(DFP), PARAMETER :: f0(3, 3) = RESHAPE( &
!                         [2.0_DFP, 0.0_DFP, 0.0_DFP, &
!                          0.5_DFP, 1.0_DFP, 0.0_DFP, &
!                          0.0_DFP, 0.2_DFP, 1.0_DFP], [3, 3])
!
! ! Test 1: Kirchhoff stress tau = J sigma
! BLOCK
!   TYPE(Stress_) :: tau
!
!   tau = KirchhoffStress(Stress(s0, StressMeasureCauchy), &
!                         DeformationGradient(f0))
!   CALL OK(MAXVAL(ABS(tau%T - 2.0_DFP * s0)) .LT. tol, "tau = J sigma")
!   CALL OK(tau%stressType .EQ. StressMeasureKirchhoff, "tau: stressType")
!   CALL OK(tau%isSym, "tau: isSym is true")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/ConvertStress_test_1.F90

INTERFACE KirchhoffStress
  MODULE FUNCTION Rank2Tensor_KirchhoffStress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    !! stress in any measure
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(Stress_) :: ans
    !! kirchhoff stress
  END FUNCTION Rank2Tensor_KirchhoffStress
END INTERFACE KirchhoffStress

!----------------------------------------------------------------------------
!                                                 PK1Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: First Piola-Kirchhoff stress from any stress measure
!
! Returns:
!   ans : Stress_ - first Piola-Kirchhoff stress (not symmetric)
!
! Formula:
!   $$P = \tau F^{-T}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasurePK1
! USE GlobalData, ONLY: StressMeasurePK2
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: PK1Stress
! USE Rank2Tensor_Method, ONLY: PK2Stress
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
! REAL(DFP), PARAMETER :: f0(3, 3) = RESHAPE( &
!                         [2.0_DFP, 0.0_DFP, 0.0_DFP, &
!                          0.5_DFP, 1.0_DFP, 0.0_DFP, &
!                          0.0_DFP, 0.2_DFP, 1.0_DFP], [3, 3])
!
! ! Test 2: first Piola-Kirchhoff stress P = F S
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Stress_) :: sig, P, S
!
!   F = DeformationGradient(f0)
!   sig = Stress(s0, StressMeasureCauchy)
!   P = PK1Stress(sig, F)
!   S = PK2Stress(sig, F)
!   CALL OK(MAXVAL(ABS(P%T - MATMUL(f0, S%T))) .LT. tol, "P = F S")
!   CALL OK(P%stressType .EQ. StressMeasurePK1, "P: stressType")
!   CALL OK(.NOT. P%isSym, "P: isSym is false")
!   CALL OK(S%stressType .EQ. StressMeasurePK2, "S: stressType")
!   CALL OK(S%isSym, "S: isSym is true")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/ConvertStress_test_1.F90

INTERFACE PK1Stress
  MODULE FUNCTION Rank2Tensor_PK1Stress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    !! stress in any measure
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(Stress_) :: ans
    !! first Piola-Kirchhoff stress (not symmetric)
  END FUNCTION Rank2Tensor_PK1Stress
END INTERFACE PK1Stress

!----------------------------------------------------------------------------
!                                                 PK2Stress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Second Piola-Kirchhoff stress from any stress measure
!
! Returns:
!   ans : Stress_ - second Piola-Kirchhoff stress
!
! Formula:
!   $$S = F^{-1}\tau F^{-T}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasurePK1
! USE GlobalData, ONLY: StressMeasurePK2
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: PK1Stress
! USE Rank2Tensor_Method, ONLY: PK2Stress
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
! REAL(DFP), PARAMETER :: f0(3, 3) = RESHAPE( &
!                         [2.0_DFP, 0.0_DFP, 0.0_DFP, &
!                          0.5_DFP, 1.0_DFP, 0.0_DFP, &
!                          0.0_DFP, 0.2_DFP, 1.0_DFP], [3, 3])
!
! ! Test 2: first Piola-Kirchhoff stress P = F S
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Stress_) :: sig, P, S
!
!   F = DeformationGradient(f0)
!   sig = Stress(s0, StressMeasureCauchy)
!   P = PK1Stress(sig, F)
!   S = PK2Stress(sig, F)
!   CALL OK(MAXVAL(ABS(P%T - MATMUL(f0, S%T))) .LT. tol, "P = F S")
!   CALL OK(P%stressType .EQ. StressMeasurePK1, "P: stressType")
!   CALL OK(.NOT. P%isSym, "P: isSym is false")
!   CALL OK(S%stressType .EQ. StressMeasurePK2, "S: stressType")
!   CALL OK(S%isSym, "S: isSym is true")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/ConvertStress_test_1.F90

INTERFACE PK2Stress
  MODULE FUNCTION Rank2Tensor_PK2Stress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    !! stress in any measure
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(Stress_) :: ans
    !! second Piola-Kirchhoff stress
  END FUNCTION Rank2Tensor_PK2Stress
END INTERFACE PK2Stress

!----------------------------------------------------------------------------
!                                              MandelStress@StressMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Mandel stress from any stress measure
!
! This is the "Eshelby-like" stress of the old Stress_Class.
!
! Returns:
!   ans : Stress_ - Mandel stress (not symmetric in general)
!
! Formula:
!   $$M = F^{T}\tau F^{-T} = F^{T} P = C S$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: DeformationGradient_
! USE BaseType, ONLY: Stress_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressMeasureCauchy
! USE GlobalData, ONLY: StressMeasureMandel
! USE Rank2Tensor_Method, ONLY: DeformationGradient
! USE Rank2Tensor_Method, ONLY: MandelStress
! USE Rank2Tensor_Method, ONLY: PK1Stress
! USE Rank2Tensor_Method, ONLY: PK2Stress
! USE Rank2Tensor_Method, ONLY: Stress
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, 20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
! REAL(DFP), PARAMETER :: f0(3, 3) = RESHAPE( &
!                         [2.0_DFP, 0.0_DFP, 0.0_DFP, &
!                          0.5_DFP, 1.0_DFP, 0.0_DFP, &
!                          0.0_DFP, 0.2_DFP, 1.0_DFP], [3, 3])
!
! ! Test 3: Mandel stress M = C S = F^T P
! BLOCK
!   TYPE(DeformationGradient_) :: F
!   TYPE(Stress_) :: sig, P, S, M
!
!   F = DeformationGradient(f0)
!   sig = Stress(s0, StressMeasureCauchy)
!   P = PK1Stress(sig, F)
!   S = PK2Stress(sig, F)
!   M = MandelStress(sig, F)
!   CALL OK(MAXVAL(ABS(M%T - MATMUL(MATMUL(TRANSPOSE(f0), f0), S%T))) &
!           .LT. tol, "M = C S")
!   CALL OK(MAXVAL(ABS(M%T - MATMUL(TRANSPOSE(f0), P%T))) .LT. tol, &
!           "M = F^T P")
!   CALL OK(M%stressType .EQ. StressMeasureMandel, "M: stressType")
!   CALL OK(.NOT. M%isSym, "M: isSym is false")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/ConvertStress_test_1.F90

INTERFACE MandelStress
  MODULE FUNCTION Rank2Tensor_MandelStress(obj, F) RESULT(ans)
    CLASS(Stress_), INTENT(IN) :: obj
    !! stress in any measure
    CLASS(DeformationGradient_), INTENT(IN) :: F
    !! deformation gradient
    TYPE(Stress_) :: ans
    !! mandel stress (not symmetric in general)
  END FUNCTION Rank2Tensor_MandelStress
END INTERFACE MandelStress

!----------------------------------------------------------------------------
!                                           MeanStress@StressInvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Mean (hydrostatic) stress
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\sigma_m = \frac{1}{3}\mathrm{tr}(\sigma)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: EquivalentStress
! USE Rank2Tensor_Method, ONLY: MeanStress
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: uniaxial stress diag(6, 0, 0)
! BLOCK
!   TYPE(Rank2Tensor_) :: sig
!   REAL(DFP) :: s0(3, 3)
!
!   s0 = 0.0_DFP
!   s0(1, 1) = 6.0_DFP
!   sig = Rank2Tensor(s0, .TRUE.)
!   CALL OK(ABS(MeanStress(sig) - 2.0_DFP) .LT. tol, "uniaxial: mean = 2")
!   CALL OK(ABS(EquivalentStress(sig) - 6.0_DFP) .LT. tol, &
!           "uniaxial: equivalent = 6")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE MeanStress
  MODULE PURE FUNCTION Rank2Tensor_MeanStress(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! stress
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_MeanStress
END INTERFACE MeanStress

!----------------------------------------------------------------------------
!                                     EquivalentStress@StressInvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: von Mises equivalent stress
!
! Returns:
!   ans : REAL(DFP)
!
! Formula:
!   $$\bar{\sigma} = \sqrt{3 J_2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: EquivalentStress
! USE Rank2Tensor_Method, ONLY: MeanStress
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 1: uniaxial stress diag(6, 0, 0)
! BLOCK
!   TYPE(Rank2Tensor_) :: sig
!   REAL(DFP) :: s0(3, 3)
!
!   s0 = 0.0_DFP
!   s0(1, 1) = 6.0_DFP
!   sig = Rank2Tensor(s0, .TRUE.)
!   CALL OK(ABS(MeanStress(sig) - 2.0_DFP) .LT. tol, "uniaxial: mean = 2")
!   CALL OK(ABS(EquivalentStress(sig) - 6.0_DFP) .LT. tol, &
!           "uniaxial: equivalent = 6")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE EquivalentStress
  MODULE PURE FUNCTION Rank2Tensor_EquivalentStress(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! stress
    REAL(DFP) :: ans
    !! REAL(DFP)
  END FUNCTION Rank2Tensor_EquivalentStress
END INTERFACE EquivalentStress

!----------------------------------------------------------------------------
!                                     HaighWestergaard@StressInvarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Haigh-Westergaard coordinates of a stress
!
! `lodeType` is `SineLode` or `CosineLode` (default).
!
! Returns:
!   ans : REAL(DFP), SIZE(3) - $[\xi, \rho, \theta]$
!
! Formula:
!   $$\xi = \frac{I_1}{\sqrt{3}},\quad \rho = \sqrt{2 J_2},\quad
!   \theta = \mathrm{LodeAngle}(\sigma)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: HaighWestergaard
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: atol = 1.0E-6_DFP
!
! ! Test 1: uniaxial tension diag(6, 0, 0), default (cosine) Lode angle
! BLOCK
!   TYPE(Rank2Tensor_) :: sig
!   REAL(DFP) :: s0(3, 3), hw(3)
!
!   s0 = 0.0_DFP
!   s0(1, 1) = 6.0_DFP
!   sig = Rank2Tensor(s0, .TRUE.)
!   hw = HaighWestergaard(sig)
!   CALL OK(ABS(hw(1) - 6.0_DFP / SQRT(3.0_DFP)) .LT. tol, &
!           "tension: xi = I1/sqrt(3)")
!   CALL OK(ABS(hw(2) - SQRT(24.0_DFP)) .LT. tol, &
!           "tension: rho = sqrt(2 J2)")
!   CALL OK(ABS(hw(3)) .LT. atol, "tension: cosine theta = 0")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE HaighWestergaard
  MODULE PURE FUNCTION Rank2Tensor_HaighWestergaard(obj, lodeType) &
    RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! stress
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: lodeType
    !! lode angle convention
    REAL(DFP) :: ans(3)
    !! $[\xi, \rho, \theta]$, SIZE(3)
  END FUNCTION Rank2Tensor_HaighWestergaard
END INTERFACE HaighWestergaard

!----------------------------------------------------------------------------
!                             MeanStressDerivative@StressDerivativeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivative of the mean stress with respect to stress
!
! For the Voigt form that is work conjugate to the engineering strain,
! use `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.
!
! Returns:
!   ans : Rank2Tensor_ - symmetric
!
! Formula:
!   $$\frac{\partial \sigma_m}{\partial \sigma} = \frac{1}{3} I$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: MeanStress
! USE Rank2Tensor_Method, ONLY: MeanStressDerivative
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
! REAL(DFP), PARAMETER :: fdTol = 1.0E-6_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, -20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
! REAL(DFP), PARAMETER :: E(3, 3) = RESHAPE( &
!                         [0.3_DFP, -0.2_DFP, 0.5_DFP, &
!                          -0.2_DFP, 0.7_DFP, 0.1_DFP, &
!                          0.5_DFP, 0.1_DFP, -0.4_DFP], [3, 3])
!
! ! Test 1: MeanStressDerivative is I/3 and matches finite differences
! BLOCK
!   TYPE(Rank2Tensor_) :: D
!   REAL(DFP) :: fd
!
!   D = MeanStressDerivative(Rank2Tensor(s0, .TRUE.))
!   CALL OK(MAXVAL(ABS(D%T - Eye3 / 3.0_DFP)) .LT. tol, "mean: D = I/3")
!   CALL OK(D%isSym, "mean: isSym is true")
!   fd = (MeanStress(Rank2Tensor(s0 + h * E, .TRUE.)) &
!         - MeanStress(Rank2Tensor(s0 - h * E, .TRUE.))) / (2.0_DFP * h)
!   CALL OK(ABS(SUM(D%T * E) - fd) .LT. fdTol, "mean: D:E = finite diff")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/J2Derivative_test_1.F90

INTERFACE MeanStressDerivative
  MODULE PURE FUNCTION Rank2Tensor_MeanStressDerivative(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! stress
    TYPE(Rank2Tensor_) :: ans
    !! symmetric
  END FUNCTION Rank2Tensor_MeanStressDerivative
END INTERFACE MeanStressDerivative

!----------------------------------------------------------------------------
!                                     J2Derivative@StressDerivativeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivative of J2 with respect to stress
!
! The stress is assumed symmetric. For the Voigt form that is work
! conjugate to the engineering strain, use
! `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.
!
! Returns:
!   ans : Rank2Tensor_ - symmetric
!
! Formula:
!   $$\frac{\partial J_2}{\partial \sigma} = s = \mathrm{dev}(\sigma)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: Eye3
! USE Rank2Tensor_Method, ONLY: J2
! USE Rank2Tensor_Method, ONLY: J2Derivative
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
! REAL(DFP), PARAMETER :: fdTol = 1.0E-6_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, -20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
! REAL(DFP), PARAMETER :: E(3, 3) = RESHAPE( &
!                         [0.3_DFP, -0.2_DFP, 0.5_DFP, &
!                          -0.2_DFP, 0.7_DFP, 0.1_DFP, &
!                          0.5_DFP, 0.1_DFP, -0.4_DFP], [3, 3])
!
! ! Test 2: J2Derivative is dev(sigma) and matches finite differences
! BLOCK
!   TYPE(Rank2Tensor_) :: D
!   REAL(DFP) :: fd, dev(3, 3)
!
!   dev = s0 - (s0(1, 1) + s0(2, 2) + s0(3, 3)) / 3.0_DFP * Eye3
!   D = J2Derivative(Rank2Tensor(s0, .TRUE.))
!   CALL OK(MAXVAL(ABS(D%T - dev)) .LT. tol, "J2: D = dev(sigma)")
!   CALL OK(D%isSym, "J2: isSym is true")
!   fd = (J2(Rank2Tensor(s0 + h * E, .TRUE.)) &
!         - J2(Rank2Tensor(s0 - h * E, .TRUE.))) / (2.0_DFP * h)
!   CALL OK(ABS(SUM(D%T * E) - fd) .LT. fdTol * ABS(fd), &
!           "J2: D:E = finite diff")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/J2Derivative_test_1.F90

INTERFACE J2Derivative
  MODULE PURE FUNCTION Rank2Tensor_J2Derivative(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! stress
    TYPE(Rank2Tensor_) :: ans
    !! symmetric
  END FUNCTION Rank2Tensor_J2Derivative
END INTERFACE J2Derivative

!----------------------------------------------------------------------------
!                                     J3Derivative@StressDerivativeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivative of J3 with respect to stress (Hill tensor)
!
! The stress is assumed symmetric. For the Voigt form that is work
! conjugate to the engineering strain, use
! `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.
!
! Returns:
!   ans : Rank2Tensor_ - symmetric
!
! Formula:
!   $$\frac{\partial J_3}{\partial \sigma} = s s - \frac{2}{3} J_2 I$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: J3
! USE Rank2Tensor_Method, ONLY: J3Derivative
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
! REAL(DFP), PARAMETER :: fdTol = 1.0E-6_DFP
! REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
!                         [10.0_DFP, 2.0_DFP, 3.0_DFP, &
!                          2.0_DFP, -20.0_DFP, 4.0_DFP, &
!                          3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
! REAL(DFP), PARAMETER :: E(3, 3) = RESHAPE( &
!                         [0.3_DFP, -0.2_DFP, 0.5_DFP, &
!                          -0.2_DFP, 0.7_DFP, 0.1_DFP, &
!                          0.5_DFP, 0.1_DFP, -0.4_DFP], [3, 3])
!
! ! Test 3: J3Derivative matches finite differences and is deviatoric
! BLOCK
!   TYPE(Rank2Tensor_) :: D
!   REAL(DFP) :: fd
!
!   D = J3Derivative(Rank2Tensor(s0, .TRUE.))
!   CALL OK(ABS(D%T(1, 1) + D%T(2, 2) + D%T(3, 3)) .LT. 1.0E-9_DFP, &
!           "J3: trace of D = 0")
!   CALL OK(MAXVAL(ABS(D%T - TRANSPOSE(D%T))) .LT. tol, "J3: D symmetric")
!   fd = (J3(Rank2Tensor(s0 + h * E, .TRUE.)) &
!         - J3(Rank2Tensor(s0 - h * E, .TRUE.))) / (2.0_DFP * h)
!   CALL OK(ABS(SUM(D%T * E) - fd) .LT. fdTol * ABS(fd), &
!           "J3: D:E = finite diff")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/J2Derivative_test_1.F90

INTERFACE J3Derivative
  MODULE PURE FUNCTION Rank2Tensor_J3Derivative(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! stress
    TYPE(Rank2Tensor_) :: ans
    !! symmetric
  END FUNCTION Rank2Tensor_J3Derivative
END INTERFACE J3Derivative

!----------------------------------------------------------------------------
!                       EquivalentStressDerivative@StressDerivativeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Derivative of the equivalent stress with respect to stress
!
! The stress is assumed symmetric. The result is zero when the
! equivalent stress is zero. For the Voigt form that is work conjugate
! to the engineering strain, use
! `VoigtRank2Tensor(ans%T, StrainTypeVoigt)`.
!
! Returns:
!   ans : Rank2Tensor_ - symmetric
!
! Formula:
!   $$\frac{\partial \bar{\sigma}}{\partial \sigma} =
!   \frac{3}{2\bar{\sigma}} s$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: Rank2Tensor_
! USE GlobalData, ONLY: DFP
! USE Rank2Tensor_Method, ONLY: EquivalentStressDerivative
! USE Rank2Tensor_Method, ONLY: Rank2Tensor
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! ! Test 5: uniaxial stress diag(6, 0, 0) gives D = diag(1, -1/2, -1/2)
! BLOCK
!   TYPE(Rank2Tensor_) :: D
!   REAL(DFP) :: a0(3, 3)
!
!   a0 = 0.0_DFP
!   a0(1, 1) = 6.0_DFP
!   D = EquivalentStressDerivative(Rank2Tensor(a0, .TRUE.))
!   a0 = 0.0_DFP
!   a0(1, 1) = 1.0_DFP
!   a0(2, 2) = -0.5_DFP
!   a0(3, 3) = -0.5_DFP
!   CALL OK(MAXVAL(ABS(D%T - a0)) .LT. tol, "equivalent: uniaxial D")
! END BLOCK
!
! END PROGRAM main
!```
!
! Full example:
! src/modules/Rank2Tensor/examples/J2Derivative_test_1.F90

INTERFACE EquivalentStressDerivative
  MODULE PURE FUNCTION Rank2Tensor_EquivalentStressDeriv(obj) RESULT(ans)
    CLASS(Rank2Tensor_), INTENT(IN) :: obj
    !! stress
    TYPE(Rank2Tensor_) :: ans
    !! symmetric
  END FUNCTION Rank2Tensor_EquivalentStressDeriv
END INTERFACE EquivalentStressDerivative

END MODULE Rank2Tensor_Method
