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

MODULE WrapperFactoryList

USE PENF, ONLY: I4P
USE WrapperFactory

IMPLICIT NONE
PRIVATE

TYPE, PUBLIC :: WrapperFactoryList_t
  PRIVATE
  CHARACTER(:), ALLOCATABLE :: Key
  CLASS(WrapperFactory_t), POINTER :: VALUE => NULL()
  TYPE(WrapperFactoryList_t), POINTER :: Next => NULL()
CONTAINS
  PRIVATE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Init => WrapperFactoryList_Init
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: HasNext => WrapperFactoryList_HasNext
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: SetNext => WrapperFactoryList_SetNext
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetNext => WrapperFactoryList_GetNext
        procedure, non_overridable, public :: NullifyNext       => WrapperFactoryList_NullifyNext
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: HasKey => WrapperFactoryList_HasKey
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: SetKey => WrapperFactoryList_SetKey
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetKey => WrapperFactoryList_GetKey
        procedure, non_overridable, public :: DeallocateKey     => WrapperFactoryList_DeallocateKey
 PROCEDURE, NON_OVERRIDABLE, PUBLIC :: HasValue => WrapperFactoryList_HasValue
 PROCEDURE, NON_OVERRIDABLE, PUBLIC :: SetValue => WrapperFactoryList_SetValue
 PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetValue => WrapperFactoryList_GetValue
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Free => WrapperFactoryList_Free
        procedure, non_overridable, public :: AddWrapperFactory => WrapperFactoryList_AddWrapperFactory
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: PRINT => WrapperFactoryList_Print
  PROCEDURE, NON_OVERRIDABLE :: WrapperFactoryList_GetFactory0D
  PROCEDURE, NON_OVERRIDABLE :: WrapperFactoryList_GetFactory1D
  PROCEDURE, NON_OVERRIDABLE :: WrapperFactoryList_GetFactory2D
  PROCEDURE, NON_OVERRIDABLE :: WrapperFactoryList_GetFactory3D
  PROCEDURE, NON_OVERRIDABLE :: WrapperFactoryList_GetFactory4D
  PROCEDURE, NON_OVERRIDABLE :: WrapperFactoryList_GetFactory5D
  PROCEDURE, NON_OVERRIDABLE :: WrapperFactoryList_GetFactory6D
  PROCEDURE, NON_OVERRIDABLE :: WrapperFactoryList_GetFactory7D
  GENERIC, PUBLIC :: GetFactory => WrapperFactoryList_GetFactory0D, &
    WrapperFactoryList_GetFactory1D, &
    WrapperFactoryList_GetFactory2D, &
    WrapperFactoryList_GetFactory3D, &
    WrapperFactoryList_GetFactory4D, &
    WrapperFactoryList_GetFactory5D, &
    WrapperFactoryList_GetFactory6D, &
    WrapperFactoryList_GetFactory7D
  FINAL :: WrapperFactoryList_Finalize
END TYPE WrapperFactoryList_t

CONTAINS

SUBROUTINE WrapperFactoryList_Init(this)
  !-----------------------------------------------------------------
  !< Initialize the node
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(INOUT) :: this !< Wrapper Factory List
  !-----------------------------------------------------------------
  IF (ALLOCATED(this%Key)) DEALLOCATE (this%Key)
  NULLIFY (this%VALUE)
  NULLIFY (this%Next)
END SUBROUTINE WrapperFactoryList_Init

FUNCTION WrapperFactoryList_HasNext(this) RESULT(hasNext)
  !-----------------------------------------------------------------
  !< Check if Next is associated for the current Node
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  LOGICAL :: hasNext !< Check if Next is associated
  !-----------------------------------------------------------------
  hasNext = ASSOCIATED(this%Next)
END FUNCTION WrapperFactoryList_HasNext

SUBROUTINE WrapperFactoryList_SetNext(this, Next)
  !-----------------------------------------------------------------
  !< Set the pointer to the Next node
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(INOUT) :: this !< Wrapper Factory List
  CLASS(WrapperFactoryList_t), TARGET, INTENT(IN) :: Next !< Pointer to Next
  !-----------------------------------------------------------------
  this%Next => Next
END SUBROUTINE WrapperFactoryList_SetNext

FUNCTION WrapperFactoryList_GetNext(this) RESULT(Next)
  !-----------------------------------------------------------------
  !< Return a pointer to the Next node
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(WrapperFactoryList_t), POINTER :: Next !< Pointer to Next
  !-----------------------------------------------------------------
  NULLIFY (Next)
  IF (this%HasNext()) Next => this%Next
END FUNCTION WrapperFactoryList_GetNext

SUBROUTINE WrapperFactoryList_NullifyNext(this)
  !-----------------------------------------------------------------
  !< Nullify Next
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(INOUT) :: this !< Wrapper Factory List
  !-----------------------------------------------------------------
  NULLIFY (this%Next)
END SUBROUTINE WrapperFactoryList_NullifyNext

FUNCTION WrapperFactoryList_HasKey(this) RESULT(hasKey)
  !-----------------------------------------------------------------
  !< Check if Key is allocated for the current Node
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  LOGICAL :: hasKey !< Check if Key is associated
  !-----------------------------------------------------------------
  hasKey = ALLOCATED(this%Key)
END FUNCTION WrapperFactoryList_HasKey

SUBROUTINE WrapperFactoryList_SetKey(this, Key)
  !-----------------------------------------------------------------
  !< Check if Next is associated for the current Node
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(INOUT) :: this !< Wrapper Factory List
  CHARACTER(len=*), INTENT(IN) :: Key !< Key
  !-----------------------------------------------------------------
  this%Key = Key
END SUBROUTINE WrapperFactoryList_SetKey

FUNCTION WrapperFactoryList_GetKey(this) RESULT(Key)
  !-----------------------------------------------------------------
  !< Check if Next is associated for the current Node
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CHARACTER(len=:), ALLOCATABLE :: Key !< Key
  !-----------------------------------------------------------------
  IF (this%HasKey()) Key = this%Key
END FUNCTION WrapperFactoryList_GetKey

SUBROUTINE WrapperFactoryList_DeallocateKey(this)
  !-----------------------------------------------------------------
  !< Deallocate Key if allocated
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(INOUT) :: this !< Wrapper Factory List
  !-----------------------------------------------------------------
  IF (this%HasKey()) DEALLOCATE (this%Key)
END SUBROUTINE WrapperFactoryList_DeallocateKey

FUNCTION WrapperFactoryList_HasValue(this) RESULT(hasValue)
  !-----------------------------------------------------------------
  !< Check if Value is allocated for the current Node
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  LOGICAL :: hasValue !< Check if Value is allocated
  !-----------------------------------------------------------------
  hasValue = ASSOCIATED(this%VALUE)
END FUNCTION WrapperFactoryList_HasValue

SUBROUTINE WrapperFactoryList_SetValue(this, VALUE)
  !-----------------------------------------------------------------
  !< Return a concrete WrapperFactory
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(INOUT) :: this !< Wrapper Factory List
  CLASS(WrapperFactory_t), TARGET, INTENT(IN) :: VALUE !< Concrete WrapperFactory
  !-----------------------------------------------------------------
  this%VALUE => VALUE
END SUBROUTINE WrapperFactoryList_SetValue

SUBROUTINE WrapperFactoryList_GetValue(this, VALUE)
  !-----------------------------------------------------------------
  !< Return a concrete WrapperFactory
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(WrapperFactory_t), POINTER, INTENT(OUT) :: VALUE !< Concrete WrapperFactory pointer
  !-----------------------------------------------------------------
  NULLIFY (VALUE)
  IF (this%HasValue()) VALUE => this%VALUE
END SUBROUTINE WrapperFactoryList_GetValue

RECURSIVE SUBROUTINE WrapperFactoryList_Free(this)
  !-----------------------------------------------------------------
  !< Free the list
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(INOUT) :: this !< Wrapper Factory List
  CLASS(WrapperFactoryList_t), POINTER :: Next !< Wrapper Factory List Node
  !-----------------------------------------------------------------
  IF (this%HasNext()) THEN
    Next => this%GetNext()
    CALL Next%Free()
    DEALLOCATE (Next)
    NULLIFY (Next)
  END IF
  IF (this%HasKey()) DEALLOCATE (this%Key)
  NULLIFY (this%Next)
  NULLIFY (this%VALUE)
END SUBROUTINE WrapperFactoryList_Free

RECURSIVE SUBROUTINE WrapperFactoryList_Finalize(this)
  !-----------------------------------------------------------------
  !< Finalize procedure
  !-----------------------------------------------------------------
  TYPE(WrapperFactoryList_t), INTENT(INOUT) :: this !< Wrapper Factory List
  !-----------------------------------------------------------------
  CALL this%Free()
END SUBROUTINE WrapperFactoryList_Finalize

    recursive subroutine WrapperFactoryList_AddWrapperFactory(this,Key, WrapperFactory)
  !-----------------------------------------------------------------
  !< Add a new Node if key does not Exist
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_T), INTENT(INOUT) :: this !< Wrapper Factory List
  CHARACTER(len=*), INTENT(IN) :: Key !< Key (unique) of the current node.
  CLASS(WrapperFactory_t), TARGET, INTENT(IN) :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  IF (this%HasKey()) THEN
    IF (this%GetKey() /= Key) THEN
      IF (.NOT. this%hasNext()) THEN
        ALLOCATE (WrapperFactoryList_t :: this%Next)
      CALL this%Next%AddWrapperFactory(Key=Key, WrapperFactory=WrapperFactory)
      ELSE
      CALL this%Next%AddWrapperFactory(Key=Key, WrapperFactory=WrapperFactory)
      END IF
    ELSE
      CALL this%SetValue(VALUE=WrapperFactory)
    END IF
  ELSE
    CALL this%SetKey(Key=Key)
    CALL this%SetValue(VALUE=WrapperFactory)
  END IF
END SUBROUTINE WrapperFactoryList_AddWrapperFactory

    recursive function WrapperFactoryList_GetFactory0D(this, Value) result(WrapperFactory)
  !-----------------------------------------------------------------
  !< Return a WrapperFactory given a value
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(*), INTENT(IN) :: VALUE !< Polymorphic Mold
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  NULLIFY (WrapperFactory)
  IF (this%HasKey() .AND. this%HasValue()) THEN
    IF (this%VALUE%HasSameType(VALUE=VALUE)) THEN
      WrapperFactory => this%VALUE
    ELSEIF (this%HasNext()) THEN
      WrapperFactory => this%Next%GetFactory(VALUE=VALUE)
    END IF
  END IF
END FUNCTION WrapperFactoryList_GetFactory0D

    recursive function WrapperFactoryList_GetFactory1D(this, Value) result(WrapperFactory)
  !-----------------------------------------------------------------
  !< Return a WrapperFactory given a value
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(*), INTENT(IN) :: VALUE(1:) !< Polymorphic Mold
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  NULLIFY (WrapperFactory)
  IF (this%HasKey() .AND. this%HasValue()) THEN
    IF (this%VALUE%HasSameType(VALUE=VALUE(1))) THEN
      WrapperFactory => this%VALUE
    ELSEIF (this%HasNext()) THEN
      WrapperFactory => this%Next%GetFactory(VALUE=VALUE)
    END IF
  END IF
END FUNCTION WrapperFactoryList_GetFactory1D

    recursive function WrapperFactoryList_GetFactory2D(this, Value) result(WrapperFactory)
  !-----------------------------------------------------------------
  !< Return a WrapperFactory given a value
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:) !< Polymorphic Mold
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  NULLIFY (WrapperFactory)
  IF (this%HasKey() .AND. this%HasValue()) THEN
    IF (this%VALUE%HasSameType(VALUE=VALUE(1, 1))) THEN
      WrapperFactory => this%VALUE
    ELSEIF (this%HasNext()) THEN
      WrapperFactory => this%Next%GetFactory(VALUE=VALUE)
    END IF
  END IF
END FUNCTION WrapperFactoryList_GetFactory2D

    recursive function WrapperFactoryList_GetFactory3D(this, Value) result(WrapperFactory)
  !-----------------------------------------------------------------
  !< Return a WrapperFactory given a value
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:) !< Polymorphic Mold
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  NULLIFY (WrapperFactory)
  IF (this%HasKey() .AND. this%HasValue()) THEN
    IF (this%VALUE%HasSameType(VALUE=VALUE(1, 1, 1))) THEN
      WrapperFactory => this%VALUE
    ELSEIF (this%HasNext()) THEN
      WrapperFactory => this%Next%GetFactory(VALUE=VALUE)
    END IF
  END IF
END FUNCTION WrapperFactoryList_GetFactory3D

    recursive function WrapperFactoryList_GetFactory4D(this, Value) result(WrapperFactory)
  !-----------------------------------------------------------------
  !< Return a WrapperFactory given a value
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:) !< Polymorphic Mold
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  NULLIFY (WrapperFactory)
  IF (this%HasKey() .AND. this%HasValue()) THEN
    IF (this%VALUE%HasSameType(VALUE=VALUE(1, 1, 1, 1))) THEN
      WrapperFactory => this%VALUE
    ELSEIF (this%HasNext()) THEN
      WrapperFactory => this%Next%GetFactory(VALUE=VALUE)
    END IF
  END IF
END FUNCTION WrapperFactoryList_GetFactory4D

    recursive function WrapperFactoryList_GetFactory5D(this, Value) result(WrapperFactory)
  !-----------------------------------------------------------------
  !< Return a WrapperFactory given a value
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:) !< Polymorphic Mold
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  NULLIFY (WrapperFactory)
  IF (this%HasKey() .AND. this%HasValue()) THEN
    IF (this%VALUE%HasSameType(VALUE=VALUE(1, 1, 1, 1, 1))) THEN
      WrapperFactory => this%VALUE
    ELSEIF (this%HasNext()) THEN
      WrapperFactory => this%Next%GetFactory(VALUE=VALUE)
    END IF
  END IF
END FUNCTION WrapperFactoryList_GetFactory5D

    recursive function WrapperFactoryList_GetFactory6D(this, Value) result(WrapperFactory)
  !-----------------------------------------------------------------
  !< Return a WrapperFactory given a value
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:) !< Polymorphic Mold
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  NULLIFY (WrapperFactory)
  IF (this%HasKey() .AND. this%HasValue()) THEN
    IF (this%VALUE%HasSameType(VALUE=VALUE(1, 1, 1, 1, 1, 1))) THEN
      WrapperFactory => this%VALUE
    ELSEIF (this%HasNext()) THEN
      WrapperFactory => this%Next%GetFactory(VALUE=VALUE)
    END IF
  END IF
END FUNCTION WrapperFactoryList_GetFactory6D

    recursive function WrapperFactoryList_GetFactory7D(this, Value) result(WrapperFactory)
  !-----------------------------------------------------------------
  !< Return a WrapperFactory given a value
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), INTENT(IN) :: this !< Wrapper Factory List
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:, 1:) !< Polymorphic Mold
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< Wrapper Factory
  !-----------------------------------------------------------------
  NULLIFY (WrapperFactory)
  IF (this%HasKey() .AND. this%HasValue()) THEN
    IF (this%VALUE%HasSameType(VALUE=VALUE(1, 1, 1, 1, 1, 1, 1))) THEN
      WrapperFactory => this%VALUE
    ELSEIF (this%HasNext()) THEN
      WrapperFactory => this%Next%GetFactory(VALUE=VALUE)
    END IF
  END IF
END FUNCTION WrapperFactoryList_GetFactory7D

SUBROUTINE WrapperFactoryList_Print(this, unit, prefix, iostat, iomsg)
  !-----------------------------------------------------------------
  !< Print the keys contained in the list
  !-----------------------------------------------------------------
  CLASS(WrapperFactoryList_t), TARGET, INTENT(IN) :: this !< Wrapper Factory List
  INTEGER(I4P), INTENT(IN) :: unit !< Logic unit.
  CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix !< Prefixing string.
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: iostat !< IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg !< IO error message.
  CHARACTER(len=:), ALLOCATABLE :: prefd !< Prefixing string.
  INTEGER(I4P) :: iostatd !< IO error.
  CHARACTER(500) :: iomsgd !< Temporary variable for IO error message.
  CLASS(WrapperFactoryList_T), POINTER :: Node !< Pointer for scanning the list.
  !-----------------------------------------------------------------
  prefd = ''; IF (PRESENT(prefix)) prefd = prefix
  Node => this
  WRITE (*, fmt='(A)') prefd//' WRAPPER FACTORY LIST KEYS:'
  DO WHILE (Node%HasKey())
            write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//'   Key = '//Node%GetKey()
    IF (Node%HasNExt()) THEN
      Node => Node%GetNext()
    ELSE
      EXIT
    END IF
  END DO
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE WrapperFactoryList_Print

END MODULE WrapperFactoryList
