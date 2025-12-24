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

MODULE R4PWrapperFactory

USE WrapperFactory
USE PENF, ONLY: I1P, R4P
USE DimensionsWrapper
USE DimensionsWrapper0D_R4P
USE DimensionsWrapper1D_R4P
USE DimensionsWrapper2D_R4P
USE DimensionsWrapper3D_R4P
USE DimensionsWrapper4D_R4P
USE DimensionsWrapper5D_R4P
USE DimensionsWrapper6D_R4P
USE DimensionsWrapper7D_R4P

IMPLICIT NONE
PRIVATE

TYPE, EXTENDS(WrapperFactory_t) :: R4PWrapperFactory_t
  PRIVATE

CONTAINS
  PROCEDURE :: Wrap0D => R4PWrapperFactory_Wrap0D
  PROCEDURE :: Wrap1D => R4PWrapperFactory_Wrap1D
  PROCEDURE :: Wrap2D => R4PWrapperFactory_Wrap2D
  PROCEDURE :: Wrap3D => R4PWrapperFactory_Wrap3D
  PROCEDURE :: Wrap4D => R4PWrapperFactory_Wrap4D
  PROCEDURE :: Wrap5D => R4PWrapperFactory_Wrap5D
  PROCEDURE :: Wrap6D => R4PWrapperFactory_Wrap6D
  PROCEDURE :: Wrap7D => R4PWrapperFactory_Wrap7D
  PROCEDURE :: UnWrap0D => R4PWrapperFactory_UnWrap0D
  PROCEDURE :: UnWrap1D => R4PWrapperFactory_UnWrap1D
  PROCEDURE :: UnWrap2D => R4PWrapperFactory_UnWrap2D
  PROCEDURE :: UnWrap3D => R4PWrapperFactory_UnWrap3D
  PROCEDURE :: UnWrap4D => R4PWrapperFactory_UnWrap4D
  PROCEDURE :: UnWrap5D => R4PWrapperFactory_UnWrap5D
  PROCEDURE :: UnWrap6D => R4PWrapperFactory_UnWrap6D
  PROCEDURE :: UnWrap7D => R4PWrapperFactory_UnWrap7D
  PROCEDURE, PUBLIC :: hasSameType => R4PWrapperFactory_hasSameType
END TYPE

TYPE(R4PWrapperFactory_t), SAVE, PUBLIC :: WrapperFactoryR4P
!$OMP THREADPRIVATE(WrapperFactoryR4P)

CONTAINS

FUNCTION R4PWrapperFactory_hasSameType(this, VALUE) RESULT(hasSameType)
  !-----------------------------------------------------------------
  !< Check if Value type agrees with wrapper type
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE
  LOGICAL :: hasSameType
  !-----------------------------------------------------------------
  hasSameType = .FALSE.
  SELECT TYPE (VALUE)
  TYPE is (REAL(R4P))
    hasSameType = .TRUE.
  END SELECT
END FUNCTION R4PWrapperFactory_hasSameType

FUNCTION R4PWrapperFactory_Wrap0D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create R4P 0D Wrapper
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE)) THEN
    ALLOCATE (DimensionsWrapper0D_R4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=0_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper0D_R4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION R4PWrapperFactory_Wrap0D

FUNCTION R4PWrapperFactory_Wrap1D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create R4P 1D Wrapper
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1))) THEN
    ALLOCATE (DimensionsWrapper1D_R4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=1_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper1D_R4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION R4PWrapperFactory_Wrap1D

FUNCTION R4PWrapperFactory_Wrap2D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create R4P 2D Wrapper
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1))) THEN
    ALLOCATE (DimensionsWrapper2D_R4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=2_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper2D_R4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION R4PWrapperFactory_Wrap2D

FUNCTION R4PWrapperFactory_Wrap3D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create R4P 3D Wrapper
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper3D_R4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=3_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper3D_R4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION R4PWrapperFactory_Wrap3D

FUNCTION R4PWrapperFactory_Wrap4D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create R4P 4D Wrapper
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper4D_R4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=4_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper4D_R4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION R4PWrapperFactory_Wrap4D

FUNCTION R4PWrapperFactory_Wrap5D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create R4P 5D Wrapper
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper5D_R4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=5_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper5D_R4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION R4PWrapperFactory_Wrap5D

FUNCTION R4PWrapperFactory_Wrap6D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create R4P 6D Wrapper
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1, 1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper6D_R4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=6_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper6D_R4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION R4PWrapperFactory_Wrap6D

FUNCTION R4PWrapperFactory_Wrap7D(this, VALUE) RESULT(Wrapper)
  !-----------------------------------------------------------------
  !< Create R4P 7D Wrapper
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:, 1:)
  CLASS(DimensionsWrapper_t), POINTER :: Wrapper
  !-----------------------------------------------------------------
  IF (this%hasSameType(VALUE(1, 1, 1, 1, 1, 1, 1))) THEN
    ALLOCATE (DimensionsWrapper7D_R4P_t :: Wrapper)
    CALL Wrapper%SetDimensions(Dimensions=7_I1P)
    SELECT TYPE (Wrapper)
    TYPE is (DimensionsWrapper7D_R4P_t)
      CALL Wrapper%Set(VALUE=VALUE)
    END SELECT
  END IF
END FUNCTION R4PWrapperFactory_Wrap7D

SUBROUTINE R4PWrapperFactory_UnWrap0D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the R4P 0D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper0D_R4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE R4PWrapperFactory_UnWrap1D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the R4P 1D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper1D_R4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE R4PWrapperFactory_UnWrap2D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the R4P 2D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper2D_R4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE R4PWrapperFactory_UnWrap3D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the R4P 3D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper3D_R4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE R4PWrapperFactory_UnWrap4D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the R4P 4D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper4D_R4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE R4PWrapperFactory_UnWrap5D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the R4P 5D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper5D_R4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE R4PWrapperFactory_UnWrap6D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the R4P 6D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper6D_R4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

SUBROUTINE R4PWrapperFactory_UnWrap7D(this, Wrapper, VALUE)
  !-----------------------------------------------------------------
  !< Return the R4P 7D Wrapped Value
  !-----------------------------------------------------------------
  CLASS(R4PWrapperFactory_t), INTENT(IN) :: this
  CLASS(DimensionsWrapper_t), POINTER, INTENT(IN) :: Wrapper
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :, :, :)
  !-----------------------------------------------------------------
  SELECT TYPE (Wrapper)
  TYPE is (DimensionsWrapper7D_R4P_t)
    CALL Wrapper%Get(VALUE=VALUE)
  END SELECT
END SUBROUTINE

END MODULE R4PWrapperFactory
