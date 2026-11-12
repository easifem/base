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
! Description:
!   A symmetric rank-2 tensor $T$ is stored in Voigt notation as
!   `V(6) = [T11, T22, T33, s*T12, s*T23, s*T13]`, where the factor
!   `s` depends on `VoigtType`:
!
!   - `StressTypeVoigt`: `s = 1`, the shear entries are the tensor
!     components, and `Scale = 1`.
!   - `StrainTypeVoigt`: `s = 2`, the shear entries are the engineering
!     shear strains $\gamma_{ij} = 2\varepsilon_{ij}$, and `Scale = 0.5`.
!
!   In both cases the tensor component is recovered by
!   $T_{ij} = \mathrm{Scale}\, V_k$ for the shear entries.
!
!   An unknown `VoigtType` is treated as stress-like.
!
!   Reduced 2D forms (`VoigtForm*` constants in GlobalData) are available
!   through [[VoigtSize]], [[GetVoigtVector]] and the 4-argument
!   [[Initiate]] and [[VoigtRank2Tensor]]. Their orderings are plane
!   strain and plane stress `[11,22,12]` and axisymmetric `[11,22,12,33]`.

MODULE VoigtRank2Tensor_Method
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: VoigtRank2Tensor_
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
! Arguments:
!   obj : VoigtRank2Tensor_ - tensor to initiate
!   vec : REAL(DFP), SIZE(6) - Voigt vector
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!
! Formula:
!   $$\mathrm{obj\%V} = \mathrm{vec}$$
!
! Examples
!
!```fortran
! TYPE(VoigtRank2Tensor_) :: obj
! CALL Initiate(obj, [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
!   6.0_DFP], StressTypeVoigt)
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE VoigtRank2Tensor_Initiate1(obj, vec, voigtType)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: vec(6)
    INTEGER(I4B), INTENT(IN) :: voigtType
  END SUBROUTINE VoigtRank2Tensor_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Initiate [[VoigtRank2Tensor_]] from a 3x3 matrix
!
! Description:
!   The shear entries use the symmetric part of `T`, so `T` does not need
!   to be symmetric.
!
! Arguments:
!   obj : VoigtRank2Tensor_ - tensor to initiate
!   T : REAL(DFP), SHAPE(3,3) - tensor components
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right),\quad
!   s = 1 \text{ (stress)},\ s = 2 \text{ (strain)}$$

INTERFACE Initiate
  MODULE PURE SUBROUTINE VoigtRank2Tensor_Initiate2(obj, T, voigtType)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: T(3, 3)
    INTEGER(I4B), INTENT(IN) :: voigtType
  END SUBROUTINE VoigtRank2Tensor_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                        VoigtRank2Tensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Returns [[VoigtRank2Tensor_]] from a Voigt vector
!
! Arguments:
!   vec : REAL(DFP), SIZE(6) - Voigt vector
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!
! Returns:
!   ans : VoigtRank2Tensor_
!
! Formula:
!   $$\mathrm{ans\%V} = \mathrm{vec}$$

INTERFACE VoigtRank2Tensor
  MODULE PURE FUNCTION VoigtRank2Tensor_Constructor1(vec, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(6)
    INTEGER(I4B), INTENT(IN) :: voigtType
    TYPE(VoigtRank2Tensor_) :: ans
  END FUNCTION VoigtRank2Tensor_Constructor1
END INTERFACE VoigtRank2Tensor

!----------------------------------------------------------------------------
!                                        VoigtRank2Tensor@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Returns [[VoigtRank2Tensor_]] from a 3x3 matrix
!
! Arguments:
!   T : REAL(DFP), SHAPE(3,3) - tensor components
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!
! Returns:
!   ans : VoigtRank2Tensor_
!
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right)$$

INTERFACE VoigtRank2Tensor
  MODULE PURE FUNCTION VoigtRank2Tensor_Constructor2(T, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: T(3, 3)
    INTEGER(I4B), INTENT(IN) :: voigtType
    TYPE(VoigtRank2Tensor_) :: ans
  END FUNCTION VoigtRank2Tensor_Constructor2
END INTERFACE VoigtRank2Tensor

!----------------------------------------------------------------------------
!                                VoigtRank2Tensor_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Returns a pointer to a new [[VoigtRank2Tensor_]]
!
! Arguments:
!   vec : REAL(DFP), SIZE(6) - Voigt vector
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!
! Returns:
!   ans : CLASS(VoigtRank2Tensor_), POINTER - caller must deallocate
!
! Formula:
!   $$\mathrm{ans\%V} = \mathrm{vec}$$

INTERFACE VoigtRank2Tensor_Pointer
  MODULE PURE FUNCTION VoigtRank2Tensor_Pointer1(vec, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(6)
    INTEGER(I4B), INTENT(IN) :: voigtType
    CLASS(VoigtRank2Tensor_), POINTER :: ans
  END FUNCTION VoigtRank2Tensor_Pointer1
END INTERFACE VoigtRank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                VoigtRank2Tensor_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Returns a pointer to a new [[VoigtRank2Tensor_]]
!
! Arguments:
!   T : REAL(DFP), SHAPE(3,3) - tensor components
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!
! Returns:
!   ans : CLASS(VoigtRank2Tensor_), POINTER - caller must deallocate
!
! Formula:
!   $$V_4 = \frac{s}{2}\left(T_{12} + T_{21}\right)$$

INTERFACE VoigtRank2Tensor_Pointer
  MODULE PURE FUNCTION VoigtRank2Tensor_Pointer2(T, voigtType) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: T(3, 3)
    INTEGER(I4B), INTENT(IN) :: voigtType
    CLASS(VoigtRank2Tensor_), POINTER :: ans
  END FUNCTION VoigtRank2Tensor_Pointer2
END INTERFACE VoigtRank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Matrix(3,3) = [[VoigtRank2Tensor_]]
!
! Arguments:
!   T : REAL(DFP), SHAPE(3,3) - symmetric tensor components (output)
!   obj : VoigtRank2Tensor_ - Voigt tensor
!
! Formula:
!   $$T_{12} = T_{21} = \mathrm{Scale}\, V_4$$

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE VoigtRank2Tensor_MatEqObj(T, obj)
    REAL(DFP), INTENT(INOUT) :: T(3, 3)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
  END SUBROUTINE VoigtRank2Tensor_MatEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                              Assignment@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Vector(6) = [[VoigtRank2Tensor_]]
!
! Arguments:
!   vec : REAL(DFP), SIZE(6) - Voigt vector (output)
!   obj : VoigtRank2Tensor_ - Voigt tensor
!
! Formula:
!   $$\mathrm{vec} = \mathrm{obj\%V}$$

INTERFACE ASSIGNMENT(=)
  MODULE PURE SUBROUTINE VoigtRank2Tensor_VecEqObj(vec, obj)
    REAL(DFP), INTENT(INOUT) :: vec(6)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
  END SUBROUTINE VoigtRank2Tensor_VecEqObj
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-13
! summary: Display the content of [[VoigtRank2Tensor_]]
!
! Arguments:
!   obj : VoigtRank2Tensor_ - tensor to display
!   msg : CHARACTER(*) - header message
!   unitNo : INTEGER(I4B), OPTIONAL - output unit, default is stdout

INTERFACE Display
  MODULE SUBROUTINE VoigtRank2Tensor_Display(obj, msg, unitNo)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE VoigtRank2Tensor_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                VoigtSize@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Length of the Voigt vector of a Voigt form
!
! Arguments:
!   voigtForm : INTEGER(I4B) - one of the `VoigtForm*` constants
!
! Returns:
!   ans : INTEGER(I4B) - 6 (3D), 3 (plane strain or plane stress) or
!     4 (axisymmetric); unknown forms give 6
!
! Formula:
!   $$\mathrm{ans} = \mathrm{SIZE}(\mathrm{GetVoigtVector}(obj, form))$$

INTERFACE VoigtSize
  MODULE PURE FUNCTION VoigtRank2Tensor_VoigtSize(voigtForm) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: voigtForm
    INTEGER(I4B) :: ans
  END FUNCTION VoigtRank2Tensor_VoigtSize
END INTERFACE VoigtSize

!----------------------------------------------------------------------------
!                                           GetVoigtVector@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Reduced Voigt vector of a [[VoigtRank2Tensor_]]
!
! Description:
!   The orderings are 3D `[11,22,33,12,23,13]`, plane strain and plane
!   stress `[11,22,12]` and axisymmetric `[11,22,12,33]`. For plane
!   strain and plane stress the out-of-plane normal component is not
!   returned (for example $\sigma_{33}$ in plane strain).
!
! Arguments:
!   obj : VoigtRank2Tensor_ - tensor
!   voigtForm : INTEGER(I4B) - one of the `VoigtForm*` constants
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE, SIZE(VoigtSize(voigtForm))
!
! Formula:
!   $$\text{plane strain}: [V_1, V_2, V_4],\quad
!   \text{axisymmetric}: [V_1, V_2, V_4, V_3]$$

INTERFACE GetVoigtVector
  MODULE PURE FUNCTION VoigtRank2Tensor_GetVoigtVector(obj, voigtForm) &
    RESULT(ans)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: voigtForm
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION VoigtRank2Tensor_GetVoigtVector
END INTERFACE GetVoigtVector

!----------------------------------------------------------------------------
!                                          GetVoigtVector_@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-02
! summary: Reduced Voigt vector of a [[VoigtRank2Tensor_]] (no allocation)
!
! Description:
!   Same as [[GetVoigtVector]] but the result is written in `ans`, which
!   should be allocated by the caller. This routine is intended for the
!   quadrature point loops of finite element kernels.
!
! Arguments:
!   obj : VoigtRank2Tensor_ - tensor
!   voigtForm : INTEGER(I4B) - one of the `VoigtForm*` constants
!   ans : REAL(DFP), SIZE >= VoigtSize(voigtForm) - reduced Voigt vector
!   tsize : INTEGER(I4B) - number of entries written in ans
!
! Formula:
!   $$\text{plane strain}: [V_1, V_2, V_4],\quad
!   \text{axisymmetric}: [V_1, V_2, V_4, V_3]$$

INTERFACE GetVoigtVector_
  MODULE PURE SUBROUTINE VoigtRank2Tensor_GetVoigtVector_(obj, voigtForm, &
                                                          ans, tsize)
    CLASS(VoigtRank2Tensor_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: voigtForm
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE VoigtRank2Tensor_GetVoigtVector_
END INTERFACE GetVoigtVector_

!----------------------------------------------------------------------------
!                                                 Initiate@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate [[VoigtRank2Tensor_]] from a reduced Voigt vector
!
! Description:
!   Components that are not part of the reduced form are set to zero.
!   Only the first `VoigtSize(voigtForm)` entries of `vec` are read.
!
! Arguments:
!   obj : VoigtRank2Tensor_ - tensor to initiate
!   vec : REAL(DFP), SIZE >= VoigtSize(voigtForm) - reduced Voigt vector
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!   voigtForm : INTEGER(I4B) - one of the `VoigtForm*` constants
!
! Formula:
!   $$\text{plane strain}: V = [v_1, v_2, 0, v_3, 0, 0]$$

INTERFACE Initiate
  MODULE PURE SUBROUTINE VoigtRank2Tensor_Initiate3(obj, vec, voigtType, &
                                                    voigtForm)
    CLASS(VoigtRank2Tensor_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: voigtType
    INTEGER(I4B), INTENT(IN) :: voigtForm
  END SUBROUTINE VoigtRank2Tensor_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                         VoigtRank2Tensor@VoigtFormMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Returns [[VoigtRank2Tensor_]] from a reduced Voigt vector
!
! Arguments:
!   vec : REAL(DFP), SIZE >= VoigtSize(voigtForm) - reduced Voigt vector
!   voigtType : INTEGER(I4B) - `StressTypeVoigt` or `StrainTypeVoigt`
!   voigtForm : INTEGER(I4B) - one of the `VoigtForm*` constants
!
! Returns:
!   ans : VoigtRank2Tensor_
!
! Formula:
!   $$\text{axisymmetric}: V = [v_1, v_2, v_4, v_3, 0, 0]$$

INTERFACE VoigtRank2Tensor
  MODULE PURE FUNCTION VoigtRank2Tensor_Constructor3(vec, voigtType, &
                                                     voigtForm) RESULT(ans)
    REAL(DFP), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: voigtType
    INTEGER(I4B), INTENT(IN) :: voigtForm
    TYPE(VoigtRank2Tensor_) :: ans
  END FUNCTION VoigtRank2Tensor_Constructor3
END INTERFACE VoigtRank2Tensor

END MODULE VoigtRank2Tensor_Method
