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

MODULE DimensionsWrapper0D_I4P

USE DimensionsWrapper0D
USE PENF, ONLY: I4P, str, byte_size
USE ErrorMessages

IMPLICIT NONE
PRIVATE

TYPE, EXTENDS(DimensionsWrapper0D_t) :: DimensionsWrapper0D_I4P_t
  INTEGER(I4P), ALLOCATABLE :: VALUE
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC :: Set => DimensionsWrapper0D_I4P_Set
  PROCEDURE, PUBLIC :: Get => DimensionsWrapper0D_I4P_Get
  PROCEDURE, PUBLIC :: GetShape => DimensionsWrapper0D_I4P_GetShape
  PROCEDURE, PUBLIC :: GetPointer => DimensionsWrapper0D_I4P_GetPointer
 PROCEDURE, PUBLIC :: GetPolymorphic => DimensionsWrapper0D_I4P_GetPolymorphic
procedure, public :: DataSizeInBytes=> DimensionsWrapper0D_I4P_DataSizeInBytes
  PROCEDURE, PUBLIC :: isOfDataType => DimensionsWrapper0D_I4P_isOfDataType
  PROCEDURE, PUBLIC :: toString => DimensionsWrapper0D_I4P_toString
  PROCEDURE, PUBLIC :: Free => DimensionsWrapper0D_I4P_Free
  PROCEDURE, PUBLIC :: PRINT => DimensionsWrapper0D_I4P_Print
  FINAL :: DimensionsWrapper0D_I4P_Final
END TYPE

PUBLIC :: DimensionsWrapper0D_I4P_t

CONTAINS

SUBROUTINE DimensionsWrapper0D_I4P_Final(this)
  !-----------------------------------------------------------------
  !< Final procedure of DimensionsWrapper0D
  !-----------------------------------------------------------------
  TYPE(DimensionsWrapper0D_I4P_t), INTENT(INOUT) :: this
  !-----------------------------------------------------------------
  CALL this%Free()
END SUBROUTINE

SUBROUTINE DimensionsWrapper0D_I4P_Set(this, VALUE)
  !-----------------------------------------------------------------
  !< Set I4P Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(INOUT) :: this
  CLASS(*), INTENT(IN) :: VALUE
  INTEGER :: err
  !-----------------------------------------------------------------
  SELECT TYPE (VALUE)
  TYPE is (INTEGER(I4P))
    ALLOCATE (this%VALUE, stat=err)
    this%VALUE = VALUE
    IF (err /= 0) CALL msg%Error(txt='Setting Value: Allocation error ('// &
                                 str(no_sign=.TRUE., n=err)//')', &
                                 file=__FILE__, line=__LINE__)
  CLASS Default
    CALL msg%Warn(txt='Setting value: Expected data type (I4P)', &
                  file=__FILE__, line=__LINE__)
  END SELECT
END SUBROUTINE

SUBROUTINE DimensionsWrapper0D_I4P_Get(this, VALUE)
  !-----------------------------------------------------------------
  !< Get I4P Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(IN) :: this
  CLASS(*), INTENT(OUT) :: VALUE
  !-----------------------------------------------------------------
  SELECT TYPE (VALUE)
  TYPE is (INTEGER(I4P))
    VALUE = this%VALUE
  CLASS Default
    CALL msg%Warn(txt='Getting value: Expected data type (I4P)', &
                  file=__FILE__, line=__LINE__)
  END SELECT
END SUBROUTINE

SUBROUTINE DimensionsWrapper0D_I4P_GetShape(this, ValueShape)
  !-----------------------------------------------------------------
  !< Return the shape of the Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(IN) :: this
  INTEGER(I4P), ALLOCATABLE, INTENT(INOUT) :: ValueShape(:)
  !-----------------------------------------------------------------
  IF (ALLOCATED(ValueShape)) DEALLOCATE (ValueShape)
  ALLOCATE (ValueShape(this%GetDimensions()))
  ValueShape = SHAPE(this%VALUE, kind=I4P)
END SUBROUTINE

FUNCTION DimensionsWrapper0D_I4P_GetPointer(this) RESULT(VALUE)
  !-----------------------------------------------------------------
  !< Get Unlimited Polymorphic pointer to Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), TARGET, INTENT(IN) :: this
  CLASS(*), POINTER :: VALUE
  !-----------------------------------------------------------------
  VALUE => this%VALUE
END FUNCTION

SUBROUTINE DimensionsWrapper0D_I4P_GetPolymorphic(this, VALUE)
  !-----------------------------------------------------------------
  !< Get Unlimited Polymorphic Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(IN) :: this
  CLASS(*), ALLOCATABLE, INTENT(OUT) :: VALUE
  !-----------------------------------------------------------------
  ALLOCATE (VALUE, source=this%VALUE)
END SUBROUTINE

SUBROUTINE DimensionsWrapper0D_I4P_Free(this)
  !-----------------------------------------------------------------
  !< Free a DimensionsWrapper0D
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(INOUT) :: this
  INTEGER :: err
  !-----------------------------------------------------------------
  IF (ALLOCATED(this%VALUE)) THEN
    DEALLOCATE (this%VALUE, stat=err)
    IF (err /= 0) CALL msg%Error(txt='Freeing Value: Deallocation error ('// &
                                 str(no_sign=.TRUE., n=err)//')', &
                                 file=__FILE__, line=__LINE__)
  END IF
END SUBROUTINE

FUNCTION DimensionsWrapper0D_I4P_DataSizeInBytes(this) RESULT(DataSizeInBytes)
  !-----------------------------------------------------------------
  !< Return the size in bytes of the stored value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(IN) :: this !< Dimensions wrapper 0D
  INTEGER(I4P) :: DataSizeInBytes !< Size in bytes of the stored value
  !-----------------------------------------------------------------
  DataSizeInBytes = byte_size(this%VALUE)
END FUNCTION DimensionsWrapper0D_I4P_DataSizeInBytes

FUNCTION DimensionsWrapper0D_I4P_isOfDataType(this, Mold) RESULT(isOfDataType)
  !-----------------------------------------------------------------
  !< Check if Mold and Value are of the same datatype
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(IN) :: this !< Dimensions wrapper 0D
  CLASS(*), INTENT(IN) :: Mold !< Mold for data type comparison
  LOGICAL :: isOfDataType !< Boolean flag to check if Value is of the same data type as Mold
  !-----------------------------------------------------------------
  isOfDataType = .FALSE.
  SELECT TYPE (Mold)
  TYPE is (INTEGER(I4P))
    isOfDataType = .TRUE.
  END SELECT
END FUNCTION DimensionsWrapper0D_I4P_isOfDataType

SUBROUTINE DimensionsWrapper0D_I4P_toString(this, String, Separator)
  !-----------------------------------------------------------------
  !< Return the wrapper value as a string
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(IN) :: this
  CHARACTER(len=:), ALLOCATABLE, INTENT(INOUT) :: String
  CHARACTER(len=1), OPTIONAL, INTENT(IN) :: Separator
  !-----------------------------------------------------------------
  String = ''
  IF (ALLOCATED(this%VALUE)) String = TRIM(str(n=this%VALUE))
END SUBROUTINE

SUBROUTINE DimensionsWrapper0D_I4P_Print(this, unit, prefix, iostat, iomsg)
  !-----------------------------------------------------------------
  !< Print Wrapper
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper0D_I4P_t), INTENT(IN) :: this !< DimensionsWrapper
  INTEGER(I4P), INTENT(IN) :: unit !< Logic unit.
  CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix !< Prefixing string.
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: iostat !< IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg !< IO error message.
  CHARACTER(len=:), ALLOCATABLE :: prefd !< Prefixing string.
  CHARACTER(len=:), ALLOCATABLE :: strvalue !< String value
  INTEGER(I4P) :: iostatd !< IO error.
  CHARACTER(500) :: iomsgd !< Temporary variable for IO error message.
  !-----------------------------------------------------------------
  prefd = ''; IF (PRESENT(prefix)) prefd = prefix
  CALL this%toString(strvalue)
        write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = I4P'//&
    ', Dimensions = '//TRIM(str(no_sign=.TRUE., n=this%GetDimensions()))// &
    ', Bytes = '//TRIM(str(no_sign=.TRUE., n=this%DataSizeInBytes()))// &
    ', Value = '//strvalue
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE DimensionsWrapper0D_I4P_Print

END MODULE DimensionsWrapper0D_I4P
