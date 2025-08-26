!-----------------------------------------------------------------
! FPL (Fortran Parameter List)
! Copyright (c) 2015 Santiago Badia, Alberto F. Martín,
! Javier Principe and Víctor Sande.
! All rights reserved.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library.
!-----------------------------------------------------------------

MODULE I4PWrapperFactory

USE WrapperFactory
USE PENF, ONLY: I1P, I4P
USE DimensionsWrapper
USE DimensionsWrapper0D_I4P
USE DimensionsWrapper1D_I4P
USE DimensionsWrapper2D_I4P
USE DimensionsWrapper3D_I4P
USE DimensionsWrapper4D_I4P
USE DimensionsWrapper5D_I4P
USE DimensionsWrapper6D_I4P
USE DimensionsWrapper7D_I4P

IMPLICIT NONE
PRIVATE

TYPE, EXTENDS(WrapperFactory_t) :: I4PWrapperFactory_t
  PRIVATE

CONTAINS
  PROCEDURE :: Wrap0D => I4PWrapperFactory_Wrap0D
  PROCEDURE :: Wrap1D => I4PWrapperFactory_Wrap1D
  PROCEDURE :: Wrap2D => I4PWrapperFactory_Wrap2D
  PROCEDURE :: Wrap3D => I4PWrapperFactory_Wrap3D
  PROCEDURE :: Wrap4D => I4PWrapperFactory_Wrap4D
  PROCEDURE :: Wrap5D => I4PWrapperFactory_Wrap5D
  PROCEDURE :: Wrap6D => I4PWrapperFactory_Wrap6D
  PROCEDURE :: Wrap7D => I4PWrapperFactory_Wrap7D
  PROCEDURE :: UnWrap0D => I4PWrapperFactory_UnWrap0D
  PROCEDURE :: UnWrap1D => I4PWrapperFactory_UnWrap1D
  PROCEDURE :: UnWrap2D => I4PWrapperFactory_UnWrap2D
  PROCEDURE :: UnWrap3D => I4PWrapperFactory_UnWrap3D
  PROCEDURE :: UnWrap4D => I4PWrapperFactory_UnWrap4D
  PROCEDURE :: UnWrap5D => I4PWrapperFactory_UnWrap5D
  PROCEDURE :: UnWrap6D => I4PWrapperFactory_UnWrap6D
  PROCEDURE :: UnWrap7D => I4PWrapperFactory_UnWrap7D
  PROCEDURE, PUBLIC :: hasSameType => I4PWrapperFactory_hasSameType
END TYPE

TYPE(I4PWrapperFactory_t), SAVE, PUBLIC :: WrapperFactoryI4P
!$OMP THREADPRIVATE(WrapperFactoryI4P)

CONTAINS

FUNCTION I4PWrapperFactory_hasSameType(this, VALUE) RESULT(hasSameType)
  !-----------------------------------------------------------------
  !< Check if Value type agrees with wrapper type
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE
  LOGICAL :: hasSameType
  !-----------------------------------------------------------------
  hasSameType = .FALSE.
  SELECT TYPE (VALUE)
  TYPE is (INTEGER(I4P))
    hasSameType = .TRUE.
  END SELECT
END FUNCTION I4PWrapperFactory_hasSameType

FUNCTION I4PWrapperFactory_Wrap0D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create I4P 0D Wrapper
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE)) THEN
    ALLOCATE (DimensionsWrapper0D_I4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=0_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper0D_I4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION I4PWrapperFactory_Wrap0D

FUNCTION I4PWrapperFactory_Wrap1D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create I4P 1D Wrapper
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1))) THEN
    ALLOCATE (DimensionsWrapper1D_I4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=1_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper1D_I4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION I4PWrapperFactory_Wrap1D

FUNCTION I4PWrapperFactory_Wrap2D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create I4P 2D Wrapper
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1))) THEN
    ALLOCATE (DimensionsWrapper2D_I4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=2_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper2D_I4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION I4PWrapperFactory_Wrap2D

FUNCTION I4PWrapperFactory_Wrap3D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create I4P 3D Wrapper
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper3D_I4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=3_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper3D_I4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION I4PWrapperFactory_Wrap3D

FUNCTION I4PWrapperFactory_Wrap4D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create I4P 4D Wrapper
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper4D_I4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=4_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper4D_I4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION I4PWrapperFactory_Wrap4D

FUNCTION I4PWrapperFactory_Wrap5D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create I4P 5D Wrapper
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper5D_I4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=5_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper5D_I4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION I4PWrapperFactory_Wrap5D

FUNCTION I4PWrapperFactory_Wrap6D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create I4P 6D Wrapper
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1, 1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper6D_I4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=6_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper6D_I4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION I4PWrapperFactory_Wrap6D

FUNCTION I4PWrapperFactory_Wrap7D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create I4P 7D Wrapper
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1, 1, 1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper7D_I4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=7_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper7D_I4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION I4PWrapperFactory_Wrap7D

SUBROUTINE I4PWrapperFactory_UnWrap0D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the I4P 0D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper0D_I4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE I4PWrapperFactory_UnWrap1D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the I4P 1D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper1D_I4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE I4PWrapperFactory_UnWrap2D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the I4P 2D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper2D_I4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE I4PWrapperFactory_UnWrap3D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the I4P 3D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper3D_I4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE I4PWrapperFactory_UnWrap4D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the I4P 4D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper4D_I4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE I4PWrapperFactory_UnWrap5D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the I4P 5D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper5D_I4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE I4PWrapperFactory_UnWrap6D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the I4P 6D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper6D_I4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE I4PWrapperFactory_UnWrap7D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the I4P 7D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(I4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper7D_I4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

END MODULE I4PWrapperFactory
