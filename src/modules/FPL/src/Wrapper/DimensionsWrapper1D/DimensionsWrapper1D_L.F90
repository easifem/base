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

MODULE DimensionsWrapper1D_L

USE DimensionsWrapper1D
USE FPL_Utils
USE PENF, ONLY: I4P, str
USE ErrorMessages

IMPLICIT NONE
PRIVATE

TYPE, EXTENDS(DimensionsWrapper1D_t) :: DimensionsWrapper1D_L_t
  LOGICAL, ALLOCATABLE :: VALUE(:)
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC :: Set => DimensionsWrapper1D_L_Set
  PROCEDURE, PUBLIC :: Get => DimensionsWrapper1D_L_Get
  PROCEDURE, PUBLIC :: GetShape => DimensionsWrapper1D_L_GetShape
  PROCEDURE, PUBLIC :: GetPointer => DimensionsWrapper1D_L_GetPointer
  PROCEDURE, PUBLIC :: GetPolymorphic => DimensionsWrapper1D_L_GetPolymorphic
  PROCEDURE, PUBLIC :: isOfDataType => DimensionsWrapper1D_L_isOfDataType
  PROCEDURE, PUBLIC :: DataSizeInBytes => &
    DimensionsWrapper1D_L_DataSizeInBytes
  PROCEDURE, PUBLIC :: toString => DimensionsWrapper1D_L_toString
  PROCEDURE, PUBLIC :: Free => DimensionsWrapper1D_L_Free
  PROCEDURE, PUBLIC :: PRINT => DimensionsWrapper1D_L_Print
  FINAL :: DimensionsWrapper1D_L_Final
END TYPE

PUBLIC :: DimensionsWrapper1D_L_t

CONTAINS

SUBROUTINE DimensionsWrapper1D_L_Final(this)
  !-----------------------------------------------------------------
  !< Final procedure of DimensionsWrapper1D
  !-----------------------------------------------------------------
  TYPE(DimensionsWrapper1D_L_t), INTENT(INOUT) :: this
  !-----------------------------------------------------------------
  CALL this%Free()
END SUBROUTINE

SUBROUTINE DimensionsWrapper1D_L_Set(this, VALUE)
  !-----------------------------------------------------------------
  !< Set logical Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(INOUT) :: this
  CLASS(*), INTENT(IN) :: VALUE(:)
  INTEGER :: err
  !-----------------------------------------------------------------
  SELECT TYPE (VALUE)
  TYPE is (LOGICAL)
    ALLOCATE (this%VALUE(SIZE(VALUE, dim=1)), stat=err)
    this%VALUE = VALUE
    IF (err /= 0) CALL msg%Error(txt='Setting Value: Allocation error ('// &
                                 str(no_sign=.TRUE., n=err)//')', &
                                 file=__FILE__, line=__LINE__)
  CLASS Default
    CALL msg%Warn(txt='Setting value: Expected data type (logical)', &
                  file=__FILE__, line=__LINE__)
  END SELECT
END SUBROUTINE

SUBROUTINE DimensionsWrapper1D_L_Get(this, VALUE)
  !-----------------------------------------------------------------
  !< Get logical Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(IN) :: this
  CLASS(*), INTENT(OUT) :: VALUE(:)
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:)
  !-----------------------------------------------------------------
  SELECT TYPE (VALUE)
  TYPE is (LOGICAL)
    CALL this%GetShape(ValueShape)
    IF (ALL(ValueShape == SHAPE(VALUE))) THEN
      VALUE = this%VALUE
    ELSE
      CALL msg%Warn(txt='Getting value: Wrong shape ('// &
                    str(no_sign=.TRUE., n=ValueShape)//'/='// &
                    str(no_sign=.TRUE., n=SHAPE(VALUE))//')', &
                    file=__FILE__, line=__LINE__)
    END IF
  CLASS Default
    CALL msg%Warn(txt='Getting value: Expected data type (L)', &
                  file=__FILE__, line=__LINE__)
  END SELECT
END SUBROUTINE

SUBROUTINE DimensionsWrapper1D_L_GetShape(this, ValueShape)
  !-----------------------------------------------------------------
  !< Get Wrapper Value Shape
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(IN) :: this
  INTEGER(I4P), ALLOCATABLE, INTENT(INOUT) :: ValueShape(:)
  !-----------------------------------------------------------------
  IF (ALLOCATED(ValueShape)) DEALLOCATE (ValueShape)
  ALLOCATE (ValueShape(this%GetDimensions()))
  ValueShape = SHAPE(this%VALUE, kind=I4P)
END SUBROUTINE

FUNCTION DimensionsWrapper1D_L_GetPointer(this) RESULT(VALUE)
  !-----------------------------------------------------------------
  !< Get Unlimited Polymorphic W2apper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), TARGET, INTENT(IN) :: this
  CLASS(*), POINTER :: VALUE(:)
  !-----------------------------------------------------------------
  VALUE => this%VALUE
END FUNCTION

SUBROUTINE DimensionsWrapper1D_L_GetPolymorphic(this, VALUE)
  !-----------------------------------------------------------------
  !< Get Unlimited Polymorphic Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(IN) :: this
  CLASS(*), ALLOCATABLE, INTENT(OUT) :: VALUE(:)
  !-----------------------------------------------------------------
  ALLOCATE (VALUE(SIZE(this%VALUE, dim=1)), source=this%VALUE)
END SUBROUTINE

SUBROUTINE DimensionsWrapper1D_L_Free(this)
  !-----------------------------------------------------------------
  !< Free a DimensionsWrapper1D
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(INOUT) :: this
  INTEGER :: err
  !-----------------------------------------------------------------
  IF (ALLOCATED(this%VALUE)) THEN
    DEALLOCATE (this%VALUE, stat=err)
    IF (err /= 0) CALL msg%Error(txt='Freeing Value: Deallocation error ('// &
                                 str(no_sign=.TRUE., n=err)//')', &
                                 file=__FILE__, line=__LINE__)
  END IF
END SUBROUTINE

FUNCTION DimensionsWrapper1D_L_DataSizeInBytes(this) RESULT(DataSizeInBytes)
  !-----------------------------------------------------------------
  !< Return the size of the stored data in bytes
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(IN) :: this !< Dimensions wrapper 1D
  INTEGER(I4P) :: DataSizeInBytes !< Size in bytes of the stored data
  !-----------------------------------------------------------------
  DataSizeInBytes = byte_size_logical(this%VALUE(1)) * SIZE(this%VALUE)
END FUNCTION DimensionsWrapper1D_L_DataSizeInBytes

FUNCTION DimensionsWrapper1D_L_isOfDataType(this, Mold) RESULT(isOfDataType)
  !-----------------------------------------------------------------
  !< Check if Mold and Value are of the same datatype
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(IN) :: this !< Dimensions wrapper 1D
  CLASS(*), INTENT(IN) :: Mold !< Mold for data type comparison
  LOGICAL :: isOfDataType !< Boolean flag to check if Value is of the same data type as Mold
  !-----------------------------------------------------------------
  isOfDataType = .FALSE.
  SELECT TYPE (Mold)
  TYPE is (LOGICAL)
    isOfDataType = .TRUE.
  END SELECT
END FUNCTION DimensionsWrapper1D_L_isOfDataType

SUBROUTINE DimensionsWrapper1D_L_toString(this, String, Separator)
  !-----------------------------------------------------------------
  !< Return the wrapper value as a string
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(IN) :: this
  CHARACTER(len=:), ALLOCATABLE, INTENT(INOUT) :: String
  CHARACTER(len=1), OPTIONAL, INTENT(IN) :: Separator
  CHARACTER(len=1) :: Sep
  INTEGER(I4P) :: idx
  !-----------------------------------------------------------------
  String = ''
  Sep = ','
  IF (ALLOCATED(this%VALUE)) THEN
    IF (PRESENT(Separator)) Sep = Separator
    DO idx = 1, SIZE(this%VALUE)
      String = String//TRIM(str(n=this%VALUE(idx)))//Sep
    END DO
  END IF
END SUBROUTINE

SUBROUTINE DimensionsWrapper1D_L_Print(this, unit, prefix, iostat, iomsg)
  !-----------------------------------------------------------------
  !< Print Wrapper
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper1D_L_t), INTENT(IN) :: this !< DimensionsWrapper
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
        write(unit=unit,fmt='(A)', advance="no",iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = L'//&
    ', Dimensions = '//TRIM(str(no_sign=.TRUE., n=this%GetDimensions()))// &
    ', Bytes = '//TRIM(str(no_sign=.TRUE., n=this%DataSizeInBytes()))// &
    ', Value = '
  CALL this%toString(strvalue)
  WRITE (unit=unit, fmt=*, iostat=iostatd, iomsg=iomsgd) strvalue
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE DimensionsWrapper1D_L_Print

END MODULE DimensionsWrapper1D_L
