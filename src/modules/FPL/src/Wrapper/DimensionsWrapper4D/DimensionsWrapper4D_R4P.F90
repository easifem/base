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

MODULE DimensionsWrapper4D_R4P
USE DimensionsWrapper4D
USE PENF, ONLY: I4P, R4P, str, byte_size
USE ErrorMessages

IMPLICIT NONE
PRIVATE

TYPE, EXTENDS(DimensionsWrapper4D_t) :: DimensionsWrapper4D_R4P_t
  REAL(R4P), ALLOCATABLE :: VALUE(:, :, :, :)
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC :: Set => DimensionsWrapper4D_R4P_Set
  PROCEDURE, PUBLIC :: Get => DimensionsWrapper4D_R4P_Get
  PROCEDURE, PUBLIC :: GetShape => DimensionsWrapper4D_R4P_GetShape
  PROCEDURE, PUBLIC :: GetPointer => DimensionsWrapper4D_R4P_GetPointer
  PROCEDURE, PUBLIC :: GetPolymorphic => &
    DimensionsWrapper4D_R4P_GetPolymorphic
  PROCEDURE, PUBLIC :: DataSizeInBytes => &
    DimensionsWrapper4D_R4P_DataSizeInBytes
  PROCEDURE, PUBLIC :: isOfDataType => DimensionsWrapper4D_R4P_isOfDataType
  PROCEDURE, PUBLIC :: toString => DimensionsWrapper4D_R4P_toString
  PROCEDURE, PUBLIC :: Free => DimensionsWrapper4D_R4P_Free
  PROCEDURE, PUBLIC :: PRINT => DimensionsWrapper4D_R4P_Print
  FINAL :: DimensionsWrapper4D_R4P_Final
END TYPE

PUBLIC :: DimensionsWrapper4D_R4P_t

CONTAINS

SUBROUTINE DimensionsWrapper4D_R4P_Final(this)
  !-----------------------------------------------------------------
  !< Final procedure of DimensionsWrapper4D
  !-----------------------------------------------------------------
  TYPE(DimensionsWrapper4D_R4P_t), INTENT(INOUT) :: this
  !-----------------------------------------------------------------
  CALL this%Free()
END SUBROUTINE

SUBROUTINE DimensionsWrapper4D_R4P_Set(this, VALUE)
  !-----------------------------------------------------------------
  !< Set R4P Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(INOUT) :: this
  CLASS(*), INTENT(IN) :: VALUE(:, :, :, :)
  INTEGER :: err
  !-----------------------------------------------------------------
  SELECT TYPE (VALUE)
  TYPE is (REAL(R4P))
    ALLOCATE (this%VALUE(SIZE(VALUE, dim=1), &
                         SIZE(VALUE, dim=2), &
                         SIZE(VALUE, dim=3), &
                         SIZE(VALUE, dim=4)), &
              stat=err)
    this%VALUE = VALUE
    IF (err /= 0) &
      CALL msg%Error(txt='Setting Value: Allocation error ('// &
                     str(no_sign=.TRUE., n=err)//')', &
                     file=__FILE__, line=__LINE__)
  CLASS Default
    CALL msg%Warn(txt='Setting value: Expected data type (R4P)', &
                  file=__FILE__, line=__LINE__)
  END SELECT
END SUBROUTINE

SUBROUTINE DimensionsWrapper4D_R4P_Get(this, VALUE)
  !-----------------------------------------------------------------
  !< Get R4P Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(IN) :: this
  CLASS(*), INTENT(OUT) :: VALUE(:, :, :, :)
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:)
  !-----------------------------------------------------------------
  SELECT TYPE (VALUE)
  TYPE is (REAL(R4P))
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
    CALL msg%Warn(txt='Getting value: Expected data type (R4P)', &
                  file=__FILE__, line=__LINE__)
  END SELECT
END SUBROUTINE

SUBROUTINE DimensionsWrapper4D_R4P_GetShape(this, ValueShape)
  !-----------------------------------------------------------------
  !< Get Wrapper Value Shape
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(IN) :: this
  INTEGER(I4P), ALLOCATABLE, INTENT(INOUT) :: ValueShape(:)
  !-----------------------------------------------------------------
  IF (ALLOCATED(ValueShape)) DEALLOCATE (ValueShape)
  ALLOCATE (ValueShape(this%GetDimensions()))
  ValueShape = SHAPE(this%VALUE, kind=I4P)
END SUBROUTINE

FUNCTION DimensionsWrapper4D_R4P_GetPointer(this) RESULT(VALUE)
  !-----------------------------------------------------------------
  !< Get Unlimited Polymorphic pointer to Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), TARGET, INTENT(IN) :: this
  CLASS(*), POINTER :: VALUE(:, :, :, :)
  !-----------------------------------------------------------------
  VALUE => this%VALUE
END FUNCTION

SUBROUTINE DimensionsWrapper4D_R4P_GetPolymorphic(this, VALUE)
  !-----------------------------------------------------------------
  !< Get Unlimited Polymorphic Wrapper Value
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(IN) :: this
  CLASS(*), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :, :, :)
  !-----------------------------------------------------------------
  ALLOCATE (VALUE(SIZE(this%VALUE, dim=1), &
                  SIZE(this%VALUE, dim=2), &
                  SIZE(this%VALUE, dim=3), &
                  SIZE(this%VALUE, dim=4)), &
            source=this%VALUE)
END SUBROUTINE

SUBROUTINE DimensionsWrapper4D_R4P_Free(this)
  !-----------------------------------------------------------------
  !< Free a DimensionsWrapper4D
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(INOUT) :: this
  INTEGER :: err
  !-----------------------------------------------------------------
  IF (ALLOCATED(this%VALUE)) THEN
    DEALLOCATE (this%VALUE, stat=err)
    IF (err /= 0) CALL msg%Error(txt='Freeing Value: Deallocation error ('// &
                                 str(no_sign=.TRUE., n=err)//')', &
                                 file=__FILE__, line=__LINE__)
  END IF
END SUBROUTINE

FUNCTION DimensionsWrapper4D_R4P_DataSizeInBytes(this) RESULT(DatasizeInBytes)
  !-----------------------------------------------------------------
  !< Return the data size of the stored value in bytes
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(IN) :: this !< Dimensions wrapper 4D
  INTEGER(I4P) :: DataSizeInBytes !< Data size in bytes of the stored value
  !-----------------------------------------------------------------
  DataSizeInBytes = byte_size(this%VALUE(1, 1, 1, 1)) * SIZE(this%VALUE)
END FUNCTION DimensionsWrapper4D_R4P_DataSizeInBytes

FUNCTION DimensionsWrapper4D_R4P_isOfDataType(this, Mold) RESULT(isOfDataType)
  !-----------------------------------------------------------------
  !< Check if Mold and Value are of the same datatype
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(IN) :: this !< Dimensions wrapper 4D
  CLASS(*), INTENT(IN) :: Mold !< Mold for data type comparison
  LOGICAL :: isOfDataType !< Boolean flag to check if Value is of the same data type as Mold
  !-----------------------------------------------------------------
  isOfDataType = .FALSE.
  SELECT TYPE (Mold)
  TYPE is (REAL(R4P))
    isOfDataType = .TRUE.
  END SELECT
END FUNCTION DimensionsWrapper4D_R4P_isOfDataType

SUBROUTINE DimensionsWrapper4D_R4P_toString(this, String, Separator)
  !-----------------------------------------------------------------
  !< Return the wrapper value as a string
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(IN) :: this
  CHARACTER(len=:), ALLOCATABLE, INTENT(INOUT) :: String
  CHARACTER(len=1), OPTIONAL, INTENT(IN) :: Separator
  CHARACTER(len=1) :: Sep
  INTEGER(I4P) :: idx2, idx3, idx4
  !-----------------------------------------------------------------
  String = ''
  Sep = ','
  IF (ALLOCATED(this%VALUE)) THEN
    IF (PRESENT(Separator)) Sep = Separator
    DO idx4 = 1, SIZE(this%VALUE, 4)
      DO idx3 = 1, SIZE(this%VALUE, 3)
        DO idx2 = 1, SIZE(this%VALUE, 2)
          String = String//TRIM(str(n=this%VALUE(:, idx2, idx3, idx4)))//Sep
        END DO
      END DO
    END DO
    String = TRIM(ADJUSTL(String(:LEN(String) - 1)))
  END IF
END SUBROUTINE

SUBROUTINE DimensionsWrapper4D_R4P_Print(this, unit, prefix, iostat, iomsg)
  !-----------------------------------------------------------------
  !< Print Wrapper
  !-----------------------------------------------------------------
  CLASS(DimensionsWrapper4D_R4P_t), INTENT(IN) :: this !< DimensionsWrapper
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
        write(unit=unit,fmt='(A)', advance="no",iostat=iostatd,iomsg=iomsgd) prefd//' Data Type = R4P'//&
    ', Dimensions = '//TRIM(str(no_sign=.TRUE., n=this%GetDimensions()))// &
    ', Bytes = '//TRIM(str(no_sign=.TRUE., n=this%DataSizeInBytes()))// &
    ', Value = '
  CALL this%toString(strvalue)
  WRITE (unit=unit, fmt=*, iostat=iostatd, iomsg=iomsgd) strvalue

  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE DimensionsWrapper4D_R4P_Print

END MODULE DimensionsWrapper4D_R4P
