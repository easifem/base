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

#define ParameterList_t ParameterList_
#define ParameterListIterator_t ParameterListIterator_

MODULE ParameterList

USE ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE ErrorMessages
USE PENF
USE ParameterEntryDictionary
USE ParameterRootEntry
USE ParameterEntry
USE WrapperFactoryListSingleton
USE WrapperFactory
USE DimensionsWrapper
USE DimensionsWrapper0D
USE DimensionsWrapper1D
USE DimensionsWrapper2D
USE DimensionsWrapper3D
USE DimensionsWrapper4D
USE DimensionsWrapper5D
USE DimensionsWrapper6D
USE DimensionsWrapper7D

IMPLICIT NONE
PRIVATE
PUBLIC :: ParameterList_t
PUBLIC :: ParameterListIterator_t

!----------------------------------------------------------------------------
!                                                          ParameterList_t
!----------------------------------------------------------------------------

TYPE :: ParameterList_t
  PRIVATE
  TYPE(ParameterEntryDictionary_t) :: Dictionary
CONTAINS
  PRIVATE
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Set0D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Set1D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Set2D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Set3D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Set4D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Set5D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Set6D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Set7D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Get0D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Get1D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Get2D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Get3D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Get4D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Get5D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Get6D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_Get7D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_GetPointer0D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_GetPointer1D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_GetPointer2D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_GetPointer3D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_GetPointer4D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_GetPointer5D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_GetPointer6D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_GetPointer7D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_IsOfDataType0D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_IsOfDataType1D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_IsOfDataType2D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_IsOfDataType3D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_IsOfDataType4D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_IsOfDataType5D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_IsOfDataType6D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_IsOfDataType7D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_isAssignable0D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_isAssignable1D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_isAssignable2D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_isAssignable3D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_isAssignable4D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_isAssignable5D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_isAssignable6D
  PROCEDURE, NON_OVERRIDABLE :: ParameterList_isAssignable7D
  GENERIC, PUBLIC :: Set => ParameterList_Set0D, &
    ParameterList_Set1D, &
    ParameterList_Set2D, &
    ParameterList_Set3D, &
    ParameterList_Set4D, &
    ParameterList_Set5D, &
    ParameterList_Set6D, &
    ParameterList_Set7D
  GENERIC, PUBLIC :: Get => ParameterList_Get0D, &
    ParameterList_Get1D, &
    ParameterList_Get2D, &
    ParameterList_Get3D, &
    ParameterList_Get4D, &
    ParameterList_Get5D, &
    ParameterList_Get6D, &
    ParameterList_Get7D
  GENERIC, PUBLIC :: GetPointer => ParameterList_GetPointer0D, &
    ParameterList_GetPointer1D, &
    ParameterList_GetPointer2D, &
    ParameterList_GetPointer3D, &
    ParameterList_GetPointer4D, &
    ParameterList_GetPointer5D, &
    ParameterList_GetPointer6D, &
    ParameterList_GetPointer7D
  GENERIC, PUBLIC :: isOfDataType => ParameterList_IsOfDataType0D, &
    ParameterList_IsOfDataType1D, &
    ParameterList_IsOfDataType2D, &
    ParameterList_IsOfDataType3D, &
    ParameterList_IsOfDataType4D, &
    ParameterList_IsOfDataType5D, &
    ParameterList_IsOfDataType6D, &
    ParameterList_IsOfDataType7D
  GENERIC, PUBLIC :: isAssignable => ParameterList_isAssignable0D, &
    ParameterList_isAssignable1D, &
    ParameterList_isAssignable2D, &
    ParameterList_isAssignable3D, &
    ParameterList_isAssignable4D, &
    ParameterList_isAssignable5D, &
    ParameterList_isAssignable6D, &
    ParameterList_isAssignable7D
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: DataSizeInBytes => &
    & ParameterList_DataSizeInBytes
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Del => ParameterList_RemoveEntry
  GENERIC, PUBLIC :: Remove => Del
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Init => ParameterList_Init
  GENERIC, PUBLIC :: Initiate => Init
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetShape => ParameterList_GetShape
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetDimensions => &
    & ParameterList_GetDimensions
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: NewSubList => ParameterList_NewSubList
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetSubList => ParameterList_GetSubList
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: isPresent => ParameterList_isPresent
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: isSubList => ParameterList_isSubList
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetAsString => &
    & ParameterList_GetAsString
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Free => ParameterList_Free
  GENERIC, PUBLIC :: DEALLOCATE => Free
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: PRINT => ParameterList_Print
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Display => ParameterList_Display
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Length => ParameterList_Length
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetIterator => &
    & ParameterList_GetIterator
  FINAL :: ParameterList_Finalize
END TYPE ParameterList_t

!----------------------------------------------------------------------------
!                                                  ParameterListIterator_t
!----------------------------------------------------------------------------

TYPE :: ParameterListIterator_t
  PRIVATE
  TYPE(ParameterRootEntry_t), POINTER :: DataBase(:) => NULL()
  TYPE(EntryListIterator_t) :: EntryListIterator
  INTEGER(I4P) :: Index = 0
  INTEGER(I4P) :: UpperBound = 0
CONTAINS
  PRIVATE
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Assignment
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Get0D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Get1D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Get2D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Get3D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Get4D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Get5D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Get6D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_Get7D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isOfDataType0D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isOfDataType1D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isOfDataType2D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isOfDataType3D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isOfDataType4D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isOfDataType5D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isOfDataType6D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isOfDataType7D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isAssignable0D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isAssignable1D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isAssignable2D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isAssignable3D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isAssignable4D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isAssignable5D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isAssignable6D
  PROCEDURE, NON_OVERRIDABLE :: ParameterListIterator_isAssignable7D
  PROCEDURE, NON_OVERRIDABLE :: GetEntry => ParameterListIterator_GetEntry
  PROCEDURE, NON_OVERRIDABLE :: GetIndex => ParameterListIterator_GetIndex
  PROCEDURE, NON_OVERRIDABLE :: PointToValue => &
    & ParameterListIterator_PointToValue
  PROCEDURE, NON_OVERRIDABLE :: NextNotEmptyListIterator => &
    & ParameterListIterator_NextNotEmptyListIterator
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: GetKey => ParameterListIterator_GetKey
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: Init => ParameterListIterator_Init
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: Begin => ParameterListIterator_Begin
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: END => ParameterListIterator_End
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: Next => ParameterListIterator_Next
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: HasFinished => &
    & ParameterListIterator_HasFinished
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: GetShape => &
    & ParameterListIterator_GetShape
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: GetDimensions => &
    & ParameterListIterator_GetDimensions
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: DataSizeInBytes => &
    & ParameterListIterator_DataSizeInBytes
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: GetAsString => &
    & ParameterListIterator_GetAsString
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: GetSubList => &
    & ParameterListIterator_GetSubList
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: isSubList => &
    & ParameterListIterator_isSubList
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: toString => &
    & ParameterListIterator_toString
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: PRINT => ParameterListIterator_Print
  PROCEDURE, PUBLIC, NON_OVERRIDABLE :: Free => ParameterListIterator_Free
  GENERIC, PUBLIC :: Get => ParameterListIterator_Get0D, &
    ParameterListIterator_Get1D, &
    ParameterListIterator_Get2D, &
    ParameterListIterator_Get3D, &
    ParameterListIterator_Get4D, &
    ParameterListIterator_Get5D, &
    ParameterListIterator_Get6D, &
    ParameterListIterator_Get7D
  GENERIC, PUBLIC :: isOfDataType => ParameterListIterator_IsOfDataType0D, &
    ParameterListIterator_IsOfDataType1D, &
    ParameterListIterator_IsOfDataType2D, &
    ParameterListIterator_IsOfDataType3D, &
    ParameterListIterator_IsOfDataType4D, &
    ParameterListIterator_IsOfDataType5D, &
    ParameterListIterator_IsOfDataType6D, &
    ParameterListIterator_IsOfDataType7D
  GENERIC, PUBLIC :: isAssignable => ParameterListIterator_isAssignable0D, &
    ParameterListIterator_isAssignable1D, &
    ParameterListIterator_isAssignable2D, &
    ParameterListIterator_isAssignable3D, &
    ParameterListIterator_isAssignable4D, &
    ParameterListIterator_isAssignable5D, &
    ParameterListIterator_isAssignable6D, &
    ParameterListIterator_isAssignable7D
  GENERIC, PUBLIC :: ASSIGNMENT(=) => ParameterListIterator_Assignment
  FINAL :: ParameterListIterator_Final
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!---------------------------------------------------------------------
!< Parameter List Procedures
!---------------------------------------------------------------------

SUBROUTINE ParameterList_Init(this, Size)

  !< Initialize the dictionary

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: Size !< Dictionary Size

  CALL this%Free()
  IF (PRESENT(Size)) THEN
    CALL this%Dictionary%Init(Size=Size)
  ELSE
    CALL this%Dictionary%Init()
  END IF
END SUBROUTINE ParameterList_Init

!----------------------------------------------------------------------------
!                                                   ParameterList_GetShape
!----------------------------------------------------------------------------

FUNCTION ParameterList_GetShape(this, Key, Shape) RESULT(FPLError)

  !< Return an allocatable array with the shape of the contained value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  INTEGER(I4P), ALLOCATABLE, INTENT(INOUT) :: SHAPE(:) !< Shape of the stored value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      CALL Wrapper%GetShape(Shape)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Unknown Wrapper. Shape was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Shape was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetShape

FUNCTION ParameterList_GetDimensions(this, Key) RESULT(Dimensions)

  !< Return an integer with the dimensions of the contained value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  INTEGER(I4P) :: Dimensions !< Dimensions of the stored value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  Dimensions = 0
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      Dimensions = Wrapper%GetDimensions()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Unknown Wrapper. Shape was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Shape was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetDimensions

SUBROUTINE ParameterList_Free(this)

  !< Free the dictionary

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List

  CALL this%Dictionary%Free()
END SUBROUTINE ParameterList_Free

SUBROUTINE ParameterList_Finalize(this)

  !< Destructor procedure

  TYPE(ParameterList_t), INTENT(INOUT) :: this !< Parameter List

  CALL this%Free()
END SUBROUTINE ParameterList_Finalize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22 
! summary:  Set a Key/Value pair into the dictionary

FUNCTION ParameterList_NewSubList(this, Key, Size) RESULT(SubListPointer)


  CLASS(ParameterList_t), INTENT(INOUT) :: this 
    !! Parameter List
  CHARACTER(*), INTENT(IN) :: Key 
    !! String Key
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: Size 
    !! Sublist Size
  TYPE(ParameterList_t), POINTER :: SublistPointer 
    !! New Sublist pointer

  ! Internal variables
  CLASS(*), POINTER :: Sublist !< New Sublist

  ALLOCATE (ParameterList_t :: SubList)
  CALL this%Dictionary%Set(Key=Key, VALUE=Sublist)
  SELECT TYPE (SubList)
  CLASS is (ParameterList_t)
    SublistPointer => SubList
    IF (PRESENT(Size)) THEN
      CALL Sublist%Init(Size=Size)
    ELSE
      CALL Sublist%Init(Size=Size)
    END IF
  END SELECT
END FUNCTION ParameterList_NewSubList

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ParameterList_GetSublist(this, Key, Sublist) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  TYPE(ParameterList_t), POINTER, INTENT(INOUT) :: Sublist !< Wrapper
  CLASS(*), POINTER :: VALUE !< Returned pointer to value
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (VALUE)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=VALUE)
  IF (ASSOCIATED(VALUE)) THEN
    SELECT TYPE (VALUE)
    CLASS IS (ParameterList_t)
      SubList => VALUE
    CLASS DEFAULT
      FPLerror = FPLSublistError
      CALL msg%Error(txt='Getting [Key="'//Key//'"]: Is not a sublist.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLSublistError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetSubList

!----------------------------------------------------------------------------
!                                                                 
!----------------------------------------------------------------------------

FUNCTION ParameterList_Set0D(this, Key, VALUE) RESULT(FPLerror)

  !< Set a Key/Value pair into the Dictionary

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE !< Unlimited polymorphic Value
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< WrapperFactory
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (WrapperFactory)
  NULLIFY (Wrapper)
  WrapperFactory => TheWrapperFactoryList%GetFactory(VALUE=VALUE)
  IF (ASSOCIATED(WrapperFactory)) THEN
    Wrapper => WrapperFactory%Wrap(VALUE=VALUE)
    IF (ASSOCIATED(Wrapper)) THEN
      CALL this%Dictionary%Set(Key=Key, VALUE=Wrapper)
    ELSE
      FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                     file=__FILE__, line=__LINE__)
    END IF
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Set0D

FUNCTION ParameterList_Set1D(this, Key, VALUE) RESULT(FPLerror)

  !< Set a Key/Value pair into the DataBase

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(:) !< Unlimited polymorphic 1D array Value
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< WrapperFactory
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (WrapperFactory)
  NULLIFY (Wrapper)
  WrapperFactory => TheWrapperFactoryList%GetFactory(VALUE=VALUE)
  IF (ASSOCIATED(WrapperFactory)) THEN
    Wrapper => WrapperFactory%Wrap(VALUE=VALUE)
    IF (ASSOCIATED(Wrapper)) THEN
      CALL this%Dictionary%Set(Key=Key, VALUE=Wrapper)
    ELSE
      FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                     file=__FILE__, line=__LINE__)
    END IF
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Set1D

FUNCTION ParameterList_Set2D(this, Key, VALUE) RESULT(FPLerror)

  !< Set a Key/Value pair into the DataBase

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(:, :) !< Unlimited polymorphic 2D array value
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< WrapperFactory
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (WrapperFactory)
  NULLIFY (Wrapper)
  WrapperFactory => TheWrapperFactoryList%GetFactory(VALUE=VALUE)
  IF (ASSOCIATED(WrapperFactory)) THEN
    Wrapper => WrapperFactory%Wrap(VALUE=VALUE)
    IF (ASSOCIATED(Wrapper)) THEN
      CALL this%Dictionary%Set(Key=Key, VALUE=Wrapper)
    ELSE
      FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                     file=__FILE__, line=__LINE__)
    END IF
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Set2D

FUNCTION ParameterList_Set3D(this, Key, VALUE) RESULT(FPLerror)

  !< Set a Key/Value pair into the DataBase

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(:, :, :) !< Unlimited Polimorphic 3D array Value
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< WrapperFactory
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (WrapperFactory)
  NULLIFY (Wrapper)
  WrapperFactory => TheWrapperFactoryList%GetFactory(VALUE=VALUE)
  IF (ASSOCIATED(WrapperFactory)) THEN
    Wrapper => WrapperFactory%Wrap(VALUE=VALUE)
    IF (ASSOCIATED(Wrapper)) THEN
      CALL this%Dictionary%Set(Key=Key, VALUE=Wrapper)
    ELSE
      FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                     file=__FILE__, line=__LINE__)
    END IF
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Set3D

FUNCTION ParameterList_Set4D(this, Key, VALUE) RESULT(FPLerror)

  !< Set a Key/Value pair into the DataBase

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(:, :, :, :) !< Unlimited Polymorphic 4D array Value
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< WrapperFactory
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (WrapperFactory)
  NULLIFY (Wrapper)
  WrapperFactory => TheWrapperFactoryList%GetFactory(VALUE=VALUE)
  IF (ASSOCIATED(WrapperFactory)) THEN
    Wrapper => WrapperFactory%Wrap(VALUE=VALUE)
    IF (ASSOCIATED(Wrapper)) THEN
      CALL this%Dictionary%Set(Key=Key, VALUE=Wrapper)
    ELSE
      FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                     file=__FILE__, line=__LINE__)
    END IF
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Set4D

FUNCTION ParameterList_Set5D(this, Key, VALUE) RESULT(FPLerror)

  !< Set a Key/Value pair into the DataBase

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(:, :, :, :, :) !< Unlimited Polymorphic 5D array Value
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< WrapperFactory
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (WrapperFactory)
  NULLIFY (Wrapper)
  WrapperFactory => TheWrapperFactoryList%GetFactory(VALUE=VALUE)
  IF (ASSOCIATED(WrapperFactory)) THEN
    Wrapper => WrapperFactory%Wrap(VALUE=VALUE)
    IF (ASSOCIATED(Wrapper)) THEN
      CALL this%Dictionary%Set(Key=Key, VALUE=Wrapper)
    ELSE
      FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                     file=__FILE__, line=__LINE__)
    END IF
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Set5D

FUNCTION ParameterList_Set6D(this, Key, VALUE) RESULT(FPLerror)

  !< Set a Key/Value pair into the DataBase

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(:, :, :, :, :, :) !< Unlimited Polymorphic 5D array Value
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< WrapperFactory
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (WrapperFactory)
  NULLIFY (Wrapper)
  WrapperFactory => TheWrapperFactoryList%GetFactory(VALUE=VALUE)
  IF (ASSOCIATED(WrapperFactory)) THEN
    Wrapper => WrapperFactory%Wrap(VALUE=VALUE)
    IF (ASSOCIATED(Wrapper)) THEN
      CALL this%Dictionary%Set(Key=Key, VALUE=Wrapper)
    ELSE
      FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                     file=__FILE__, line=__LINE__)
    END IF
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Set6D

FUNCTION ParameterList_Set7D(this, Key, VALUE) RESULT(FPLerror)

  !< Set a Key/Value pair into the DataBase

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(:, :, :, :, :, :, :) !< Unlimited Polymorphic 7D array Value
  CLASS(WrapperFactory_t), POINTER :: WrapperFactory !< WrapperFactory
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (WrapperFactory)
  NULLIFY (Wrapper)
  WrapperFactory => TheWrapperFactoryList%GetFactory(VALUE=VALUE)
  IF (ASSOCIATED(WrapperFactory)) THEN
    Wrapper => WrapperFactory%Wrap(VALUE=VALUE)
    IF (ASSOCIATED(Wrapper)) THEN
      CALL this%Dictionary%Set(Key=Key, VALUE=Wrapper)
    ELSE
      FPLerror = FPLWrapperError
                call msg%Error(txt='Setting [Key="'//Key//'"]: Nonexistent wrapper. Not added to the list.', &
                     file=__FILE__, line=__LINE__)
    END IF
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Setting [Key="'//Key//'"]: Unsupported data type. Not added to the list.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Set7D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-13
! summary: Return a scalar Value given the Key

FUNCTION ParameterList_Get0D(this, Key, VALUE) RESULT(FPLerror)
  CLASS(ParameterList_t), INTENT(IN) :: this
    !! Parameter List
  CHARACTER(*), INTENT(IN) :: Key
    !! String Key
  CLASS(*), INTENT(INOUT) :: VALUE
    !! Returned value
  CLASS(*), POINTER :: Wrapper
    !! Wrapper
  INTEGER(I4P) :: FPLerror
    !! Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper0D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
      call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
    call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Get0D

FUNCTION ParameterList_Get1D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a vector Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(INOUT) :: VALUE(:) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper1D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Get1D

FUNCTION ParameterList_Get2D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a 2D array Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(INOUT) :: VALUE(:, :) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper2D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Get2D

FUNCTION ParameterList_Get3D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a 3D array Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper3D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Get3D

FUNCTION ParameterList_Get4D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a 4D array Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper4D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Get4D

FUNCTION ParameterList_Get5D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a 5D array Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :) !< Returned value
  CLASS(*), POINTER :: Node !< Pointer to a Parameter List
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper5D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Get5D

FUNCTION ParameterList_Get6D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a 6D array Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :, :) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper6D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Get6D

FUNCTION ParameterList_Get7D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a 7D array Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :, :, :) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper7D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_Get7D

FUNCTION ParameterList_GetPointer0D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE !< Returned pointer to value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper0D_t)
      VALUE => Wrapper%GetPointer()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetPointer0D

FUNCTION ParameterList_GetPointer1D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE(:) !< Returned pointer to value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper1D_t)
      VALUE => Wrapper%GetPointer()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetPointer1D

FUNCTION ParameterList_GetPointer2D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE(:, :) !< Returned pointer to value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper2D_t)
      VALUE => Wrapper%GetPointer()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetPointer2D

FUNCTION ParameterList_GetPointer3D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE(:, :, :) !< Returned pointer to value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper3D_t)
      VALUE => Wrapper%GetPointer()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetPointer3D

FUNCTION ParameterList_GetPointer4D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE(:, :, :, :) !< Returned pointer to value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper4D_t)
      VALUE => Wrapper%GetPointer()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetPointer4D

FUNCTION ParameterList_GetPointer5D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE(:, :, :, :, :) !< Returned pointer to value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper5D_t)
      VALUE => Wrapper%GetPointer()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetPointer5D

FUNCTION ParameterList_GetPointer6D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE(:, :, :, :, :, :) !< Returned pointer to value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper6D_t)
      VALUE => Wrapper%GetPointer()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetPointer6D

FUNCTION ParameterList_GetPointer7D(this, Key, VALUE) RESULT(FPLerror)

  !< Return a Unlimited polymorphic pointer to a Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE(:, :, :, :, :, :, :) !< Returned pointer to value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper7D_t)
      VALUE => Wrapper%GetPointer()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetPointer7D

FUNCTION ParameterList_isPresent(this, Key) RESULT(isPresent)

  !< Check if a Key is present at the DataBase

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  LOGICAL :: isPresent !< Boolean flag to check if a Key is present

  isPresent = this%Dictionary%IsPresent(Key=Key)
END FUNCTION ParameterList_isPresent

FUNCTION ParameterList_isSubList(this, Key) RESULT(isSubList)

  !< Check if a Key is a SubList

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER :: SubListPointer !< Pointer to a SubList
  LOGICAL :: isSubList !< Check if is a SubList

  isSubList = .FALSE.
  NULLIFY (SubListPointer)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=SubListPointer)
  IF (ASSOCIATED(SubListPointer)) THEN
    SELECT TYPE (SubListPointer)
    CLASS is (ParameterList_t)
      isSubList = .TRUE.
    END SELECT
  END IF
END FUNCTION ParameterList_isSubList

FUNCTION ParameterList_DataSizeInBytes(this, Key) RESULT(DataSizeInBytes)

  !< Return the data size in bytes of the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: DataSizeInBytes !< Size in bytes

  DataSizeInBytes = 0
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      DataSizeInBytes = Wrapper%DataSizeInBytes()
    END SELECT
  END IF
END FUNCTION ParameterList_DataSizeInBytes

FUNCTION ParameterList_isOfDataType0D(this, Key, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: Mold !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold)
    CLASS is (ParameterList_t)
      SELECT TYPE (Mold)
      CLASS is (ParameterList_t)
        isOfDataType = .TRUE.
      END SELECT
    END SELECT
  END IF
END FUNCTION ParameterList_isOfDataType0D

FUNCTION ParameterList_isOfDataType1D(this, Key, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: Mold(1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.; NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1))
    END SELECT
  END IF
END FUNCTION ParameterList_isOfDataType1D

FUNCTION ParameterList_isOfDataType2D(this, Key, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: Mold(1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.; NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1))
    END SELECT
  END IF
END FUNCTION ParameterList_isOfDataType2D

FUNCTION ParameterList_isOfDataType3D(this, Key, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.; NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  SELECT TYPE (Wrapper)
  CLASS is (DimensionsWrapper_t)
    isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1))
  END SELECT
END FUNCTION ParameterList_isOfDataType3D

FUNCTION ParameterList_isOfDataType4D(this, Key, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.; NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterList_isOfDataType4D

FUNCTION ParameterList_isOfDataType5D(this, Key, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.; NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterList_isOfDataType5D

FUNCTION ParameterList_isOfDataType6D(this, Key, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:, 1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.; NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1, 1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterList_isOfDataType6D

FUNCTION ParameterList_isOfDataType7D(this, Key, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the value associated with Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:, 1:, 1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.; NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1, 1, 1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterList_isOfDataType7D

FUNCTION ParameterList_isAssignable0D(this, Key, VALUE) RESULT(Assignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE !< Value to compare with the stored variable
  LOGICAL :: Assignable !< Boolean flag to check compatibility
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  Assignable = .FALSE.
  NULLIFY (Wrapper)
  ! Check if present
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper0D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE)) Assignable = .TRUE.
    END SELECT
  END IF
END FUNCTION ParameterList_isAssignable0D

FUNCTION ParameterList_isAssignable1D(this, Key, VALUE) RESULT(Assignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(1:) !< Value to check against with the stored variable
  LOGICAL :: Assignable !< Boolean flag to check compatibility
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  Assignable = .FALSE.
  NULLIFY (Wrapper)
  ! Check if present
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper1D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) Assignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterList_isAssignable1D

FUNCTION ParameterList_isAssignable2D(this, Key, VALUE) RESULT(Assignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:) !< Value to check against with the stored variable
  LOGICAL :: Assignable !< Boolean flag to check compatibility
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  Assignable = .FALSE.
  NULLIFY (Wrapper)
  ! Check if present
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper2D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) Assignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterList_isAssignable2D

FUNCTION ParameterList_isAssignable3D(this, Key, VALUE) RESULT(Assignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:) !< Value to check against with the stored variable
  LOGICAL :: Assignable !< Boolean flag to check compatibility
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  Assignable = .FALSE.
  NULLIFY (Wrapper)
  ! Check if present
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper3D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) Assignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterList_isAssignable3D

FUNCTION ParameterList_isAssignable4D(this, Key, VALUE) RESULT(Assignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:) !< Value to check against the stored variable
  LOGICAL :: Assignable !< Boolean flag to check compatibility
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  Assignable = .FALSE.
  NULLIFY (Wrapper)
  ! Check if present
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper4D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) Assignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterList_isAssignable4D

FUNCTION ParameterList_isAssignable5D(this, Key, VALUE) RESULT(Assignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:) !< Value to check against the stored variable
  LOGICAL :: Assignable !< Boolean flag to check compatibility
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  Assignable = .FALSE.
  NULLIFY (Wrapper)
  ! Check if present
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper5D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) Assignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterList_isAssignable5D

FUNCTION ParameterList_isAssignable6D(this, Key, VALUE) RESULT(Assignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:) !< Value to check against the stored variable
  LOGICAL :: Assignable !< Boolean flag to check compatibility
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  Assignable = .FALSE.
  NULLIFY (Wrapper)
  ! Check if present
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper6D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1, 1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) Assignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterList_isAssignable6D

FUNCTION ParameterList_isAssignable7D(this, Key, VALUE) RESULT(Assignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:, 1:) !< Value to check against the stored variable
  LOGICAL :: Assignable !< Boolean flag to check compatibility
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  Assignable = .FALSE.
  NULLIFY (Wrapper)
  ! Check if present
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper7D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1, 1, 1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) Assignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterList_isAssignable7D

SUBROUTINE ParameterList_RemoveEntry(this, Key)

  !< Remove an Entry given a Key

  CLASS(ParameterList_t), INTENT(INOUT) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key

  CALL this%Dictionary%Del(Key=Key)
END SUBROUTINE ParameterList_RemoveEntry

FUNCTION ParameterList_Length(this) RESULT(Length)

  !< Return the number of ParameterListEntries contained in the DataBase

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  INTEGER(I4P) :: Length !< Number of parameters in database

  Length = this%Dictionary%Length()
END FUNCTION ParameterList_Length

FUNCTION ParameterList_GetIterator(this) RESULT(Iterator)

  !< Return a pointer to a Parameters Iterator

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List Entry Container Type
  TYPE(ParameterListIterator_t) :: Iterator !< Parameter List iterator

  CALL Iterator%Init(DataBase=this%Dictionary%GetDataBase())
END FUNCTION ParameterList_GetIterator

function ParameterList_GetAsString(this,Key,String,Separator) result(FPLerror)

  !< Return a scalar Value given the Key

  CLASS(ParameterList_t), INTENT(IN) :: this !< Parameter List
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CHARACTER(:), ALLOCATABLE, INTENT(INOUT) :: String !< Returned value as string
  CHARACTER(1), OPTIONAL, INTENT(IN) :: Separator !< Array values separator
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  CALL this%Dictionary%GetPointer(Key=Key, VALUE=Wrapper)
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      CALL Wrapper%toString(String=String, Separator=Separator)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//Key//'"]: Unknown Wrapper. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//Key//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterList_GetAsString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterList_Display(this, msg, unitno)

  !< Print the content of the DataBase

  CLASS(ParameterList_t), INTENT(in) :: this
  CHARACTER(*), INTENT(in) :: msg
  INTEGER(i4p), OPTIONAL, INTENT(in) :: unitno
  CALL this%PRINT(unitno, msg)
END SUBROUTINE ParameterList_Display

!----------------------------------------------------------------------------
!                                                                 Print
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-02
! summary: Print the content of the DataBase

RECURSIVE SUBROUTINE ParameterList_Print(this, unit, prefix, iostat, iomsg)
  CLASS(ParameterList_t), INTENT(IN) :: this
  !! Linked List
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: unit
  !! Logic unit.
  CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  !! Prefixing string.
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: iostat
  !! IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
  !! IO error message.
  CHARACTER(:), ALLOCATABLE :: prefd
  !! Prefixing string.
  INTEGER(I4P) :: unitd
  !! Logic unit.
  INTEGER(I4P) :: iostatd
  !! IO error.
  CHARACTER(500) :: iomsgd
  !! Temporary variable for IO error message.
  TYPE(ParameterListIterator_t) :: Iterator
  !! Dictionary Iterator
  !
  ! Internal variables
  !
  CLASS(*), POINTER :: VALUE
  !
  !
  !
  prefd = ''; IF (PRESENT(prefix)) prefd = prefix
  unitd = OUTPUT_UNIT; IF (PRESENT(unit)) unitd = unit
  Iterator = this%GetIterator()
  !
  DO WHILE (.NOT. Iterator%HasFinished())
    !!
    NULLIFY (VALUE)
    !!
    VALUE => Iterator%PointToValue()
    !!
    IF (ASSOCIATED(VALUE)) THEN
      !!
      SELECT TYPE (VALUE)
      !!
      CLASS is (DimensionsWrapper_t)
        !!
        CALL VALUE%PRINT(unit=unitd, &
          & prefix=prefd// &
          & '['//TRIM(str(no_sign=.TRUE., n=Iterator%GetIndex()))//']'// &
          & ' Key = '//Iterator%GetKey()//',', &
          & iostat=iostatd, &
          & iomsg=iomsgd)
        !!
      TYPE is (ParameterList_t)
        !!
        WRITE (unit=unitd, fmt='(A)') prefd// &
          & '['//TRIM(str(no_sign=.TRUE., n=Iterator%GetIndex()))//']'// &
          & ' Key = '//Iterator%GetKey()//', Data Type = ParameterList'
        !!
        CALL VALUE%PRINT( &
          &  unit=unitd, &
          &  prefix=prefd//'['//TRIM(str(no_sign=.TRUE., &
          & n=Iterator%GetIndex()))//'] ', &
          & iostat=iostatd, &
          & iomsg=iomsgd)
        !!
      CLASS default
        !!
        WRITE (unit=unitd, fmt='(A)') prefd// &
          & '['//TRIM(str(no_sign=.TRUE., n=Iterator%GetIndex()))//']'// &
          & ' Key = '//Iterator%GetKey()//', Data Type = Unknown Data Type!'
        !!
      END SELECT
    END IF
    CALL Iterator%Next()
  END DO
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE ParameterList_Print

!---------------------------------------------------------------------
!< Parameter List Iterator Procedures
!---------------------------------------------------------------------

SUBROUTINE ParameterListIterator_Assignment(this, ParameterListIterator)

  !< Dictionary iterator Assignment

  CLASS(ParameterListIterator_t), INTENT(INOUT) :: this ! Output Dictionary iterator
  TYPE(ParameterListIterator_t), INTENT(IN) :: ParameterListIterator ! Input Dictionary iterator

  this%DataBase(0:) => ParameterListIterator%DataBase
  this%EntryListIterator = ParameterListIterator%EntryListIterator
  this%Index = ParameterListIterator%Index
  this%UpperBound = ParameterListIterator%UpperBound
END SUBROUTINE ParameterListIterator_Assignment

SUBROUTINE ParameterListIterator_Free(this)

  !< Free the dictionary iterator

  CLASS(ParameterListIterator_t), INTENT(INOUT) :: this ! Dictionary iterator

  this%Index = 0
  this%UpperBound = 0
  NULLIFY (this%DataBase)
  CALL this%EntryListIterator%Free()
END SUBROUTINE ParameterListIterator_Free

SUBROUTINE ParameterListIterator_Final(this)

  !< Free the dictionary iterator

  TYPE(ParameterListIterator_t), INTENT(INOUT) :: this ! Dictionary iterator

  CALL this%Free()
END SUBROUTINE ParameterListIterator_Final

SUBROUTINE ParameterListIterator_Init(this, DataBase)

  !< Associate the iterator with a dictionary and rewind
  !< to the first position

  CLASS(ParameterListIterator_t), INTENT(INOUT) :: this ! Dictionary iterator
  TYPE(ParameterRootEntry_t), TARGET, INTENT(IN) :: DataBase(:) ! Entries database

  CALL this%Free()
  this%DataBase(0:) => DataBase(:)
  this%Index = -1
  this%UpperBound = SIZE(this%DataBase)
  CALL this%Next()
END SUBROUTINE ParameterListIterator_Init

SUBROUTINE ParameterListIterator_Begin(this)

  !< Rewind the iterator to the first dictionary position

  CLASS(ParameterListIterator_t), INTENT(INOUT) :: this ! Dictionary iterator
  TYPE(ParameterRootEntry_t), POINTER :: DataBase(:) ! Entries database

  DataBase => this%DataBase
  CALL this%Init(DataBase)
END SUBROUTINE ParameterListIterator_Begin

SUBROUTINE ParameterListIterator_End(this)

  !< Fast forward to the last dictionary position (HasFinished = .true.)

  CLASS(ParameterListIterator_t), INTENT(INOUT) :: this ! Dictionary iterator

  this%Index = this%UpperBound
  CALL this%EntryListIterator%Free()
END SUBROUTINE ParameterListIterator_End

SUBROUTINE ParameterListIterator_NextNotEmptyListIterator(this)

  !< The iterator points to the next associated entry

  CLASS(ParameterListIterator_t), INTENT(INOUT) :: this ! Dictionary iterator

  CALL this%EntryListIterator%Free()
  this%Index = this%Index + 1
  DO WHILE (this%Index < this%UpperBound)
    IF (this%DataBase(this%Index)%HasRoot()) THEN
      this%EntryListIterator = this%Database(this%Index)%GetIterator()
      EXIT
    END IF
    this%Index = this%Index + 1
  END DO
END SUBROUTINE ParameterListIterator_NextNotEmptyListIterator

SUBROUTINE ParameterListIterator_Next(this)

  !< The iterator points to the next associated entry

  CLASS(ParameterListIterator_t), INTENT(INOUT) :: this ! Dictionary iterator

  IF (.NOT. this%HasFinished()) THEN
    IF (.NOT. this%EntryListIterator%HasFinished()) THEN
      CALL this%EntryListIterator%Next()
    ELSE
      CALL this%NextNotEmptyListIterator()
    END IF
  END IF
END SUBROUTINE ParameterListIterator_Next

!----------------------------------------------------------------------------
!                                                                 
!----------------------------------------------------------------------------

FUNCTION ParameterListIterator_GetEntry(this) RESULT(CurrentEntry)

  !< Return the current Entry

  CLASS(ParameterListIterator_t), INTENT(IN) :: this ! Dictionary iterator
  TYPE(ParameterEntry_t), POINTER :: CurrentEntry ! Current entry
  INTEGER(I4P) :: FPLerror !< Error flag

  NULLIFY (CurrentEntry)
  CurrentEntry => this%EntryListIterator%GetEntry()
  IF (.NOT. ASSOCIATED(CurrentEntry)) THEN
    FPLerror = FPLParameterListIteratorError
 CALL msg%Error(txt='Current entry not associated. Shape was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_GetEntry

!----------------------------------------------------------------------------
!                                                                 
!----------------------------------------------------------------------------

FUNCTION ParameterListIterator_PointToValue(this) RESULT(VALUE)

  !< Return a pointer to the value stored in the current Entry

  CLASS(ParameterListIterator_t), INTENT(IN) :: this ! Dictionary iterator
  CLASS(*), POINTER :: VALUE ! Unlimited polymorphic pointer
  TYPE(ParameterEntry_t), POINTER :: CurrentEntry ! Current entry

  NULLIFY (CurrentEntry)
  NULLIFY (VALUE)
  CurrentEntry => this%GetEntry()
  IF (ASSOCIATED(CurrentEntry)) VALUE => CurrentEntry%PointToValue()
END FUNCTION ParameterListIterator_PointToValue

FUNCTION ParameterListIterator_GetKey(this) RESULT(Key)

  !< Return the Key of the current Entry

  CLASS(ParameterListIterator_t), INTENT(IN) :: this ! Dictionary iterator
  CHARACTER(:), ALLOCATABLE :: Key ! Key
  TYPE(ParameterEntry_t), POINTER :: CurrentEntry ! Current entry

  NULLIFY (CurrentEntry)
  CurrentEntry => this%GetEntry()
  IF (ASSOCIATED(CurrentEntry)) CALL CurrentEntry%GetKey(Key)
END FUNCTION ParameterListIterator_GetKey

FUNCTION ParameterListIterator_GetIndex(this) RESULT(CurrentIndex)

  !< Return the current Index

  CLASS(ParameterListIterator_t), INTENT(IN) :: this ! Dictionary iterator
  INTEGER(I4P) :: CurrentIndex ! Current index

  CurrentIndex = this%Index
END FUNCTION ParameterListIterator_GetIndex

FUNCTION ParameterListIterator_GetShape(this, Shape) RESULT(FPLError)

  !< Return an allocatable array with the shape of the contained value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  INTEGER(I4P), ALLOCATABLE, INTENT(INOUT) :: SHAPE(:) !< Shape of the stored value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      CALL Wrapper%GetShape(Shape)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Unknown Wrapper. Shape was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Shape was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_GetShape

FUNCTION ParameterListIterator_GetDimensions(this) RESULT(Dimensions)

  !< Return an allocatable array with the shape of the contained value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  INTEGER(I4P) :: Dimensions !< Dimensions of the stored value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  Dimensions = 0
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      Dimensions = Wrapper%GetDimensions()
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Unknown Wrapper. Shape was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Shape was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_GetDimensions

    function ParameterListIterator_GetAsString(this,String,Separator) result(FPLerror)

  !< Return the current value converted into a string

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CHARACTER(:), ALLOCATABLE, INTENT(INOUT) :: String !< Returned string
  CHARACTER(1), OPTIONAL, INTENT(IN) :: Separator !< Array values separator
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      CALL Wrapper%ToString(String=String, Separator=Separator)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Unknown Wrapper. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_GetAsString

FUNCTION ParameterListIterator_Get0D(this, VALUE) RESULT(FPLerror)

  !< Return the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(INOUT) :: VALUE !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper0D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_Get0D

FUNCTION ParameterListIterator_Get1D(this, VALUE) RESULT(FPLerror)

  !< Return the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(INOUT) :: VALUE(:) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper1D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_Get1D

FUNCTION ParameterListIterator_Get2D(this, VALUE) RESULT(FPLerror)

  !< Return the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(INOUT) :: VALUE(:, :) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper2D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_Get2D

FUNCTION ParameterListIterator_Get3D(this, VALUE) RESULT(FPLerror)

  !< Return the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :) !< Returned value
  TYPE(ParameterEntry_t), POINTER :: CurrentEntry !< Current entry
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper3D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_Get3D

FUNCTION ParameterListIterator_Get4D(this, VALUE) RESULT(FPLerror)

  !< Return the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :) !< Returned value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper4D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_Get4D

FUNCTION ParameterListIterator_Get5D(this, VALUE) RESULT(FPLerror)

  !< Return the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :) !< Returned value
  TYPE(ParameterEntry_t), POINTER :: CurrentEntry !< Current entry
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper5D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_Get5D

FUNCTION ParameterListIterator_Get6D(this, VALUE) RESULT(FPLerror)

  !< Return the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :, :) !< Returned value
  TYPE(ParameterEntry_t), POINTER :: CurrentEntry !< Current entry
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper6D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_Get6D

FUNCTION ParameterListIterator_Get7D(this, VALUE) RESULT(FPLerror)

  !< Return the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(INOUT) :: VALUE(:, :, :, :, :, :, :) !< Returned value
  TYPE(ParameterEntry_t), POINTER :: CurrentEntry !< Current entry
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper7D_t)
      CALL Wrapper%Get(VALUE=VALUE)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Dimensions do not match. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_Get7D

FUNCTION ParameterListIterator_GetSublist(this, Sublist) RESULT(FPLerror)

  !< Return a pointer to the current sublist

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List
  TYPE(ParameterList_t), POINTER, INTENT(INOUT) :: Sublist !< Wrapper
  CLASS(*), POINTER :: VALUE !< Returned pointer to value
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (VALUE)
  NULLIFY (Sublist)
  VALUE => this%PointToValue()
  IF (ASSOCIATED(VALUE)) THEN
    SELECT TYPE (VALUE)
    CLASS is (ParameterList_t)
      SubList => VALUE
    CLASS Default
      FPLerror = FPLSublistError
CALL msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Is not a sublist.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLSublistError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Sublist was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_GetSubList

FUNCTION ParameterListIterator_isSubList(this) RESULT(isSubList)

  !< Check if a Key is a SubList

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), POINTER :: SubList !< Sublist pointer
  LOGICAL :: isSubList !< Check if is a SubList

  isSubList = .FALSE.
  NULLIFY (Sublist)
  SubList => this%PointToValue()
  IF (ASSOCIATED(Sublist)) THEN
    SELECT TYPE (Sublist)
    CLASS is (ParameterList_t)
      isSubList = .TRUE.
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isSubList

FUNCTION ParameterListIterator_DataSizeInBytes(this) RESULT(DataSizeInBytes)

  !< Return the data size in bytes of the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  TYPE(ParameterEntry_t), POINTER :: CurrentEntry !< Current entry
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: DataSizeInBytes !< Size in bytes

  DataSizeInBytes = 0
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      DataSizeInBytes = Wrapper%DataSizeInBytes()
    END SELECT
  END IF
END FUNCTION ParameterListIterator_DataSizeInBytes

FUNCTION ParameterListIterator_isOfDataType0D(this, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: Mold !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold)
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isOfDataType0D

FUNCTION ParameterListIterator_isOfDataType1D(this, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: Mold(1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1))
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isOfDataType1D

FUNCTION ParameterListIterator_isOfDataType2D(this, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: Mold(1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1))
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isOfDataType2D

FUNCTION ParameterListIterator_isOfDataType3D(this, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isOfDataType3D

FUNCTION ParameterListIterator_isOfDataType4D(this, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isOfDataType4D

FUNCTION ParameterListIterator_isOfDataType5D(this, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isOfDataType5D

FUNCTION ParameterListIterator_isOfDataType6D(this, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:, 1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1, 1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isOfDataType6D

FUNCTION ParameterListIterator_isOfDataType7D(this, Mold) RESULT(IsOfDataType)

  !< Check if the data type of Mold agrees with the current value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: Mold(1:, 1:, 1:, 1:, 1:, 1:, 1:) !< Mold
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isOfDataType !< Check if has the same type

  isOfDataType = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      isOfDataType = Wrapper%isOfDataType(Mold=Mold(1, 1, 1, 1, 1, 1, 1))
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isOfDataType7D

function ParameterListIterator_isAssignable0D(this,Value) result(isAssignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: VALUE !< Value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isAssignable !< Check if is assignable

  isAssignable = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper0D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE)) isAssignable = .TRUE.
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isAssignable0D

function ParameterListIterator_isAssignable1D(this,Value) result(isAssignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: VALUE(1:) !< Value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isAssignable !< Check if is assignable
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  isAssignable = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper1D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) isAssignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isAssignable1D

function ParameterListIterator_isAssignable2D(this,Value) result(isAssignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:) !< Value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isAssignable !< Check if is assignable
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  isAssignable = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper2D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) isAssignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isAssignable2D

    function ParameterListIterator_isAssignable3D(this, Value) result(isAssignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:) !< Value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isAssignable !< Check if is assignable
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  isAssignable = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper3D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) isAssignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isAssignable3D

    function ParameterListIterator_isAssignable4D(this, Value) result(isAssignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:) !< Value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isAssignable !< Check if is assignable
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  isAssignable = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper4D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) isAssignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isAssignable4D

    function ParameterListIterator_isAssignable5D(this, Value) result(isAssignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:) !< Value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isAssignable !< Check if is assignable
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  isAssignable = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper5D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) isAssignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isAssignable5D

    function ParameterListIterator_isAssignable6D(this, Value) result(isAssignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:) !< Value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isAssignable !< Check if is assignable
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  isAssignable = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper6D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1, 1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) isAssignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isAssignable6D

    function ParameterListIterator_isAssignable7D(this, Value) result(isAssignable)

  !< Check if a stored variable is Assignable to Value

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CLASS(*), INTENT(IN) :: VALUE(1:, 1:, 1:, 1:, 1:, 1:, 1:) !< Value
  CLASS(*), POINTER :: Wrapper !< Wrapper
  LOGICAL :: isAssignable !< Check if is assignable
  INTEGER(I4P), ALLOCATABLE :: ValueShape(:) !< Shape of the stored value

  isAssignable = .FALSE.
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper7D_t)
      ! Check same data type
      IF (Wrapper%isOfDataType(Mold=VALUE(1, 1, 1, 1, 1, 1, 1))) THEN
        CALL Wrapper%GetShape(ValueShape)
        ! Check right shape
        IF (ALL(ValueShape == SHAPE(VALUE))) isAssignable = .TRUE.
      END IF
    END SELECT
  END IF
END FUNCTION ParameterListIterator_isAssignable7D

FUNCTION ParameterListIterator_toString(this, Separator) RESULT(String)

  !< Return a scalar Value given the Key

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter List Iterator
  CHARACTER(1), OPTIONAL, INTENT(IN) :: Separator !< Array values separator
  CHARACTER(:), ALLOCATABLE :: String !< Returned value as string
  CLASS(*), POINTER :: Wrapper !< Wrapper
  INTEGER(I4P) :: FPLerror !< Error flag

  FPLerror = FPLSuccess
  NULLIFY (Wrapper)
  Wrapper => this%PointToValue()
  IF (ASSOCIATED(Wrapper)) THEN
    SELECT TYPE (Wrapper)
    CLASS is (DimensionsWrapper_t)
      CALL Wrapper%toString(String, Separator)
    CLASS Default
      FPLerror = FPLWrapperError
                    call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Unknown Wrapper. Value was not modified.', &
                     file=__FILE__, line=__LINE__)
    END SELECT
  ELSE
    FPLerror = FPLWrapperFactoryError
            call msg%Error(txt='Getting [Key="'//this%GetKey()//'"]: Not present. Value was not modified.', &
                   file=__FILE__, line=__LINE__)
  END IF
END FUNCTION ParameterListIterator_toString

    recursive subroutine ParameterListIterator_Print(this, unit, prefix, iostat, iomsg)

  !< Print the content of the DataBase

  CLASS(ParameterListIterator_t), INTENT(IN) :: this !< Parameter Iterator
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: unit !< Logic unit.
  CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix !< Prefixing string.
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: iostat !< IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg !< IO error message.
  CHARACTER(:), ALLOCATABLE :: prefd !< Prefixing string.
  INTEGER(I4P) :: unitd !< Logic unit.
  INTEGER(I4P) :: iostatd !< IO error.
  CHARACTER(500) :: iomsgd !< Temporary variable for IO error message.
  CLASS(*), POINTER :: VALUE !< Unlimited polymorphic value

  prefd = ''; IF (PRESENT(prefix)) prefd = prefix
  unitd = OUTPUT_UNIT; IF (PRESENT(unit)) unitd = unit
  NULLIFY (VALUE)
  VALUE => this%PointToValue()
  IF (ASSOCIATED(VALUE)) THEN
    SELECT TYPE (VALUE)
    CLASS is (DimensionsWrapper_t)
      CALL VALUE%PRINT(unit=unitd, &
                       prefix=prefd// &
                    '['//TRIM(str(no_sign=.TRUE., n=this%GetIndex()))//']'// &
                       ' Key = '//this%GetKey()//',', &
                       iostat=iostatd, &
                       iomsg=iomsgd)
    END SELECT
  END IF
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE ParameterListIterator_Print

FUNCTION ParameterListIterator_HasFinished(this) RESULT(HasFinished)

  !< Check if Iterator has reached the end of the dictionary

  CLASS(ParameterListIterator_t), INTENT(INOUT) :: this ! Dictionary iterator
  LOGICAL :: HasFinished

  HasFinished = .FALSE.
  IF (this%Index == this%UpperBound) HasFinished = .TRUE.
END FUNCTION ParameterListIterator_HasFinished

END MODULE ParameterList
