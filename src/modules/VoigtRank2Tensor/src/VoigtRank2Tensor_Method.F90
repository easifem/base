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
! Reduced 2D forms (`VoigtForm*` constants in GlobalData) are available
! through [[VoigtSize]], [[GetVoigtVector]], [[GetVoigtVector_]] and the
! 4-argument [[Initiate]] and 3-argument [[VoigtRank2Tensor]]. Their
! orderings are plane strain and plane stress `[11,22,12]` and
! axisymmetric `[11,22,12,33]`.
!
! Changes:
!   - 2026-10-21: renamed the specific procedures, an unknown voigtType
!     is treated as stress-like, added the reduced 2D Voigt forms
!   - 2026-11-02: added GetVoigtVector_

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
! Prints the message, the Voigt type and the Voigt vector as a row.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
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
! END PROGRAM main
!```

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
! The length is 6 for VoigtForm3D, 3 for VoigtFormPlaneStrain and
! VoigtFormPlaneStress, and 4 for VoigtFormAxisymmetric. Unknown forms
! give 6.
!
! Returns:
!   ans : INTEGER(I4B) - length of the Voigt vector
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: VoigtForm3D
! USE GlobalData, ONLY: VoigtFormAxisymmetric
! USE GlobalData, ONLY: VoigtFormPlaneStrain
! USE GlobalData, ONLY: VoigtFormPlaneStress
! USE Test_Method, ONLY: OK
! USE VoigtRank2Tensor_Method, ONLY: VoigtSize
! IMPLICIT NONE
!
! ! Test 1: size of each Voigt form
! BLOCK
!   CALL OK(VoigtSize(VoigtForm3D) .EQ. 6, "3D: 6")
!   CALL OK(VoigtSize(VoigtFormPlaneStrain) .EQ. 3, "plane strain: 3")
!   CALL OK(VoigtSize(VoigtFormPlaneStress) .EQ. 3, "plane stress: 3")
!   CALL OK(VoigtSize(VoigtFormAxisymmetric) .EQ. 4, "axisymmetric: 4")
! END BLOCK
!
! END PROGRAM main
!```

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
! The orderings are 3D `[11,22,33,12,23,13]`, plane strain and plane
! stress `[11,22,12]` and axisymmetric `[11,22,12,33]`. For plane
! strain and plane stress the out-of-plane normal component is not
! returned (for example $\sigma_{33}$ in plane strain). Unknown forms
! give the 3D vector.
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
! END PROGRAM main
!```
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
    !! reduced Voigt vector, SIZE VoigtSize(voigtForm)
  END FUNCTION VoigtRank2Tensor_GetVoigtVector
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
! END PROGRAM main
!```
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
    !! reduced Voigt vector, SIZE(ans) >= VoigtSize(voigtForm)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans, VoigtSize(voigtForm)
  END SUBROUTINE VoigtRank2Tensor_GetVoigtVector_
END INTERFACE GetVoigtVector_

!----------------------------------------------------------------------------
!                                                 Initiate@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate [[VoigtRank2Tensor_]] from a reduced Voigt vector
!
! Only the first `VoigtSize(voigtForm)` entries of `vec` are read.
! Components that are not part of the reduced form are set to zero.
! This includes the out-of-plane normal component in plane strain and
! plane stress, so the 3x3 tensor from `T = obj` has `T33 = 0` even when
! the physical value is not zero (for example $\sigma_{33}$ in plane
! strain or $\varepsilon_{33}$ in plane stress).
!
! Formula:
!   $$\text{plane strain}: V = [v_1, v_2, 0, v_3, 0, 0]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: VoigtRank2Tensor_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: StressTypeVoigt
! USE GlobalData, ONLY: VoigtFormPlaneStrain
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
! END PROGRAM main
!```
!
! See also: [[VoigtRank2Tensor]], [[VoigtSize]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE VoigtRank2Tensor_Initiate3(obj, vec, voigtType, &
                                                    voigtForm)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    !! tensor to initiate
    REAL(DFP), INTENT(IN) :: vec(:)
    !! reduced Voigt vector, SIZE(vec) >= VoigtSize(voigtForm)
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
  MODULE PURE FUNCTION VoigtRank2Tensor_Constructor3(vec, voigtType, &
                                                     voigtForm) RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(:)
    !! reduced Voigt vector, SIZE(vec) >= VoigtSize(voigtForm)
    INTEGER(I4B), INTENT(IN) :: voigtType
    !! StressTypeVoigt or StrainTypeVoigt
    INTEGER(I4B), INTENT(IN) :: voigtForm
    !! one of the VoigtForm* constants
    TYPE(VoigtRank2Tensor_) :: ans
    !! Voigt tensor
  END FUNCTION VoigtRank2Tensor_Constructor3
END INTERFACE VoigtRank2Tensor

END MODULE VoigtRank2Tensor_Method
