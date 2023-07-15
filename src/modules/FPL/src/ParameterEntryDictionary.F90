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

!-----------------------------------------------------------------
! ParameterEntryDictionary is a datatype containing a Database
! array of ParameterListEntries made to store diferent Entries
! depending on the hash of its Key.
!
! This work takes as a starting point the previou work of
! Stefano Zaghi (@szaghi, https://github.com/szaghi).
!
! You can find the original source at:
! https://github.com/szaghi/OFF/blob/
!95691ca15e6d68128ba016e40df74e42123f1c54/
!src/Data_Type_Hash_Table.f90
!-----------------------------------------------------------------

MODULE ParameterEntryDictionary

USE ParameterEntry
USE ParameterRootEntry
USE PENF, ONLY: I4P, str

IMPLICIT NONE
PRIVATE

INTEGER(I4P), PARAMETER :: DefaultDataBaseSize = 100_I4P

TYPE :: ParameterEntryDictionary_t
  PRIVATE
  TYPE(ParameterRootEntry_t), ALLOCATABLE :: DataBase(:)
  INTEGER(I4P) :: Size = 0_I4P
CONTAINS
  PRIVATE
  PROCEDURE, NON_OVERRIDABLE :: Hash => &
  & ParameterEntryDictionary_Hash
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Init => &
  & ParameterEntryDictionary_Init
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Set => &
  & ParameterEntryDictionary_Set
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Get => &
  & ParameterEntryDictionary_Get
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetPointer => &
  & ParameterEntryDictionary_GetPointer
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: GetDatabase =>  &
  & ParameterEntryDictionary_GetDataBase
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Del => &
  & ParameterEntryDictionary_Delete
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: IsPresent => &
  & ParameterEntryDictionary_IsPresent
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Length => &
  & ParameterEntryDictionary_Length
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: PRINT => &
  & ParameterEntryDictionary_Print
  PROCEDURE, NON_OVERRIDABLE, PUBLIC :: Free => &
  & ParameterEntryDictionary_Free
  FINAL :: ParameterEntryDictionary_Finalize
END TYPE

PUBLIC :: ParameterEntryDictionary_t

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ParameterEntryDictionary_Hash(this, Key) RESULT(Hash)

  !< String hash function

  CLASS(ParameterEntryDictionary_t), INTENT(IN) :: this
  !< Parameter Entry Dictionary
  CHARACTER(*), INTENT(IN) :: Key
  !< String Key
  INTEGER(I4P) :: Hash
  !< Hash code
  CHARACTER, DIMENSION(LEN(Key)) :: CharArray
  !< Character array containing the Key
  INTEGER(I4P) :: CharIterator
  !< Char iterator index

  DO CONCURRENT(CharIterator=1:LEN(Key))
    CharArray(CharIterator) = Key(CharIterator:CharIterator)
  END DO
  Hash = MOD(SUM(ICHAR(CharArray)), this%Size)
END FUNCTION ParameterEntryDictionary_Hash

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterEntryDictionary_Init(this, Size)

  !< Allocate the database with a given Szie of DefaultDataBaseSize

  CLASS(ParameterEntryDictionary_t), INTENT(INOUT) :: this
  !< Parameter Entry Dictionary
  INTEGER(I4P), OPTIONAL, INTENT(IN) :: Size
  !< DataBase Size

  CALL this%Free()
  IF (PRESENT(Size)) THEN
    this%Size = Size
  ELSE
    this%Size = DefaultDataBaseSize
  END IF
  ALLOCATE (this%DataBase(0:this%Size - 1))
END SUBROUTINE ParameterEntryDictionary_Init

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ParameterEntryDictionary_isPresent(this, Key) RESULT(isPresent)

  !< Check if a Key is present in the DataBase

  CLASS(ParameterEntryDictionary_t), INTENT(IN) :: this
  !< Parameter Entry Dictionary
  CHARACTER(*), INTENT(IN) :: Key
  !< String Key
  LOGICAL :: isPresent
  !< Boolean flag to check if a Key is present

  isPresent = this%DataBase(this%Hash(Key=Key))%isPresent(Key=Key)
END FUNCTION ParameterEntryDictionary_isPresent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterEntryDictionary_Set(this, Key, VALUE)

  !< Set a Key/Value pair into the DataBase

  CLASS(ParameterEntryDictionary_t), INTENT(INOUT) :: this
  !< Parameter Entry Dictionary
  CHARACTER(*), INTENT(IN) :: Key
  !< String Key
  CLASS(*), POINTER, INTENT(IN) :: VALUE
  !< Value

  CALL this%DataBase(this%Hash(Key=Key))%AddEntry(Key=Key, VALUE=VALUE)
END SUBROUTINE ParameterEntryDictionary_Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterEntryDictionary_Get(this, Key, VALUE)

  !< Return a Value given the Key

  CLASS(ParameterEntryDictionary_t), INTENT(IN) :: this
  !< Parameter Entry Dictionary
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), ALLOCATABLE, INTENT(INOUT) :: VALUE
  !< Returned value
  CLASS(ParameterEntry_t), POINTER :: ENTRY
  !< Pointer to a Parameter List

  ENTRY => this%DataBase(this%Hash(Key=Key))%GetEntry(Key=Key)
  IF (ASSOCIATED(ENTRY)) CALL ENTRY%GetValue(VALUE=VALUE)
END SUBROUTINE ParameterEntryDictionary_Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterEntryDictionary_GetPointer(this, Key, VALUE)

  !< Return a Value given the Key

  CLASS(ParameterEntryDictionary_t), INTENT(IN) :: this !< Parameter Entry Dictionary
  CHARACTER(*), INTENT(IN) :: Key !< String Key
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE !< Returned value
  CLASS(ParameterEntry_t), POINTER :: ENTRY !< Pointer to a Parameter List
  INTEGER(I4P) :: Hash !< Hash code corresponding to Key

  ENTRY => this%DataBase(this%Hash(Key=Key))%GetEntry(Key=Key)
  IF (ASSOCIATED(ENTRY)) VALUE => ENTRY%PointToValue()
END SUBROUTINE ParameterEntryDictionary_GetPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ParameterEntryDictionary_GetDataBase(this) RESULT(Database)

  !< Return a pointer to a Dictionary Database

  CLASS(ParameterEntryDictionary_t), TARGET, INTENT(IN) :: this
  !< Parameter Entry Dictionary
  TYPE(ParameterRootEntry_t), POINTER :: Database(:)
  !< Dictionary Database

  DataBase => this%Database
END FUNCTION ParameterEntryDictionary_GetDataBase

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterEntryDictionary_Delete(this, Key)

  !< Remove an Entry given a Key

  CLASS(ParameterEntryDictionary_t), INTENT(INOUT) :: this
  !< Parameter Entry Dictionary
  CHARACTER(*), INTENT(IN) :: Key
  !< String Key

  CALL this%DataBase(this%Hash(Key=Key))%RemoveEntry(Key=Key)
END SUBROUTINE ParameterEntryDictionary_Delete

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION ParameterEntryDictionary_Length(this) RESULT(Length)

  !< Return the number of ParameterListEntries contained in the DataBase

  CLASS(ParameterEntryDictionary_t), INTENT(IN) :: this
  !< Parameter Entry Dictionary
  INTEGER(I4P) :: Length
  !< Number of parameters in database
  INTEGER(I4P) :: DBIterator
  !< Database Iterator index

  Length = 0
  IF (ALLOCATED(this%DataBase)) THEN
    DO DBIterator = LBOUND(this%DataBase, dim=1), UBOUND(this%DataBase, dim=1)
      Length = Length + this%DataBase(DBIterator)%Length()
    END DO
  END IF
END FUNCTION ParameterEntryDictionary_Length

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterentryDictionary_Free(this)

  !< Free ParameterListEntries and the DataBase

  CLASS(ParameterEntryDictionary_t), INTENT(INOUT) :: this
  !< Parameter Entry Dictionary
  INTEGER(I4P) :: DBIterator
  !< Database Iterator index

  IF (ALLOCATED(this%DataBase)) THEN
    DO DBIterator = LBOUND(this%DataBase, dim=1), UBOUND(this%DataBase, dim=1)
      CALL this%DataBase(DBIterator)%Free()
    END DO
    DEALLOCATE (this%DataBase)
  END IF
  this%Size = 0_I4P
END SUBROUTINE ParameterEntryDictionary_Free

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterEntryDictionary_Finalize(this)

  !< Destructor procedure

  TYPE(ParameterEntryDictionary_t), INTENT(INOUT) :: this
  !< Parameter Entry Dictionary

  CALL this%Free()
END SUBROUTINE ParameterEntryDictionary_Finalize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ParameterEntryDictionary_Print(this, unit, prefix, iostat, iomsg)

  !< Print the content of the DataBase

  CLASS(ParameterEntryDictionary_t), INTENT(IN) :: this
  !< Linked List
  INTEGER(I4P), INTENT(IN) :: unit
  !< Logic unit.
  CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  !< Prefixing string.
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: iostat
  !< IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: iomsg
  !< IO error message.
  CHARACTER(:), ALLOCATABLE :: prefd
  !< Prefixing string.
  INTEGER(I4P) :: iostatd
  !< IO error.
  CHARACTER(500) :: iomsgd
  !< Temporary variable for IO error message.
  INTEGER(I4P) :: DBIter
  !< Database iterator

  prefd = ''; IF (PRESENT(prefix)) prefd = prefix
  IF (ALLOCATED(this%DataBase)) THEN
    DO DBIter = LBOUND(this%DataBase, dim=1), UBOUND(this%DataBase, dim=1)
      CALL this%DataBase(DBIter)%PRINT(unit=unit, &
             prefix=prefd//'  ['//TRIM(str(no_sign=.TRUE., n=DBIter))//'] ', &
                                       iostat=iostatd, iomsg=iomsgd)
    END DO
  END IF
  IF (PRESENT(iostat)) iostat = iostatd
  IF (PRESENT(iomsg)) iomsg = iomsgd
END SUBROUTINE ParameterEntryDictionary_Print

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ParameterEntryDictionary
