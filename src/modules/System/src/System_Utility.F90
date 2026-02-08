! This module is mainly taken from the source:
! https://github.com/urbanjost/M_system.
! The original name of the program has been changed
! from M_SYSTEM to System_Method.
! This is to confirm to the coding sytles of easifem.
! Original program has been re-organized into module and submodule.
! If you are using easifem for getting methods defined in this
! module, then please use M_System module by using the above link.
! We would like to thank the original author Urban Jost for creating
! This useful module.

!> author: John S. Urban
! date: 2026-02-04
! summary: Fortran interface to C system interface
!
!# System_Utility
!
!    System_Method is a collection of Fortran procedures that call C
!    or a C wrapper using the ISO_C_BINDING interface to access system calls.
!    System calls are a special set of functions used by programs
!    to communicate directly with an operating system.

MODULE System_Utility
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_CHAR
USE GlobalData, ONLY: INT64
IMPLICIT NONE

PRIVATE

PUBLIC :: Anyinteger_to_64bit
PUBLIC :: Matchw
PUBLIC :: Str2_Carr
PUBLIC :: Arr2Str
PUBLIC :: C2F_String
PUBLIC :: TimeStamp

!----------------------------------------------------------------------------
!                                                     Arr2Str@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-07
! summary: convert fortran array to a string

INTERFACE
  MODULE PURE FUNCTION Arr2Str(array) RESULT(string)
    CHARACTER(len=1), INTENT(IN) :: array(:)
    CHARACTER(len=SIZE(array)) :: string
  END FUNCTION Arr2Str
END INTERFACE

!----------------------------------------------------------------------------
!                                                  C2F_String@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: converts c string to fortran string

INTERFACE
  MODULE FUNCTION C2F_String(c_string_pointer) RESULT(f_string)
    TYPE(C_PTR), INTENT(IN) :: c_string_pointer
    CHARACTER(:), ALLOCATABLE :: f_string
  END FUNCTION C2F_String
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Str2_Carr@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: convert fortran string into c char array.

INTERFACE
  MODULE PURE FUNCTION Str2_Carr(string) RESULT(array)
    CHARACTER(*), INTENT(in) :: string
    CHARACTER(len=1, kind=C_CHAR) :: array(LEN(string) + 1)
  END FUNCTION Str2_Carr
END INTERFACE

!----------------------------------------------------------------------------
!                                                   TimeStamp@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Time stamp method

INTERFACE
  MODULE FUNCTION TimeStamp() RESULT(epoch)
    INTEGER(kind=8) :: epoch
  END FUNCTION TimeStamp
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Matchw@UtilityMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION Matchw(tame, wild)
    LOGICAL :: Matchw
    CHARACTER(*), INTENT(IN) :: tame
    !! A string without wildcards
    CHARACTER(*), INTENT(IN) :: wild
    !! A (potentially) corresponding string with wildcards
  END FUNCTION Matchw
END INTERFACE

!----------------------------------------------------------------------------
!                                         Anyinteger_to_64bit@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Convert integer any kind to integer
!
!# Anyinteger_to_64bit
!
! This function uses polymorphism to allow arguments of different types
! generically. It is used to create other procedures that can take
! many scalar arguments as input options, equivalent to passing the
! parameter VALUE as INT(VALUE,0_int64).

INTERFACE
  MODULE PURE ELEMENTAL FUNCTION Anyinteger_to_64bit(intin) RESULT(ii38)
    CLASS(*), INTENT(in) :: intin
    !! Input argument of a procedure to convert to type
    !! INTEGER(KIND=int64). May be of KIND kind=int8, kind=int16,
    !! kind=int32, kind=int64.
    INTEGER(INT64) :: ii38
    !! The value of VALUIN converted to INTEGER(KIND=INT64).
  END FUNCTION Anyinteger_to_64bit
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE System_Utility
