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
!# System_Method
!
! M_system(3fm) is a collection of Fortran procedures that call C
! or a C wrapper using the ISO_C_BINDING interface to access system calls.
! System calls are a special set of functions used by programs to communicate
! directly with an operating system.
!
! Generally, system calls are slower than normal function calls because
! when you make a call control is relinquished to the operating system
! to perform the system call. In addition, depending on the nature of the
! system call, your program may be blocked by the OS until the system call
! has finished, thus making the execution time of your program even longer.
!
! One rule-of-thumb that should always be followed when calling a system
! call -- Always check the return value.

MODULE SystemEnquiry_Method
USE ISO_C_BINDING, ONLY: C_FLOAT, C_INT, C_CHAR
USE ISO_C_BINDING, ONLY: C_PTR, C_F_POINTER, C_NULL_CHAR, C_NULL_PTR
USE ISO_C_BINDING, ONLY: C_LONG, C_SHORT, C_FUNPTR

IMPLICIT NONE

PRIVATE

PUBLIC :: System_Access
!! determine filename access or existence
PUBLIC :: System_Isdir
!! determine if filename is a directory
PUBLIC :: System_Islnk
!! determine if filename is a link
PUBLIC :: System_Isreg
!! determine if filename is a regular file
PUBLIC :: System_Isblk
!! determine if filename is a block device
PUBLIC :: System_Ischr
!! determine if filename is a character device
PUBLIC :: System_Isfifo
!! determine if filename is a fifo - named pipe
PUBLIC :: System_Issock
!! determine if filename is a socket

!----------------------------------------------------------------------------
!                                               System_Access@EnquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Check accessibility or existence of a pathname
!
!# System_Access
!
! The system_access(3f) function checks pathname existence and access
! permissions. The function checks the pathname for accessibility
! according to the bit pattern contained in amode, using the real user
! ID in place of the effective user ID and the real group ID in place
! of the effective group ID.
!
! The value of amode is either the bitwise-inclusive OR of the access
! permissions to be checked (R_OK, W_OK, X_OK) or the existence test (F_OK).
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Access_test_1.F90" %}}
!```

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Access(pathname, amode)
    CHARACTER(len=*), INTENT(IN) :: pathname
    !! a character string representing a directory pathname.
    !! Trailing spaces are ignored.
    INTEGER, INTENT(IN) :: amode
    !! bitwise-inclusive OR of the values R_OK, W_OK, X_OK, or F_OK.
    LOGICAL :: System_Access
    !! Return value: If not true an error occurred or
    !! the requested access is not granted
  END FUNCTION System_Access
END INTERFACE

!----------------------------------------------------------------------------
!                                               System_Issock@EnquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: checks if argument is a socket
!
!# System_Issock
!
! The issock(3f) function checks if path is a path to a socket

INTERFACE
  MODULE FUNCTION System_Issock(pathname)
    CHARACTER(*), INTENT(IN) :: pathname
    !! a character string representing a socket pathname.
    !! Trailing spaces are ignored.
    LOGICAL :: System_Issock
    !! The system_issock() function should always be successful and no
    !! return value is reserved to indicate an error.
  END FUNCTION System_Issock
END INTERFACE

!----------------------------------------------------------------------------
!                                               System_Isfifo@EnquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: check if argument is a fifo named pipe
!
!# System_Isfifo
!
! Check if argument is a fifo named pipe.

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Isfifo(pathname)
    CHARACTER(len=*), INTENT(in) :: pathname
    !! a character string representing a fifo - named pipe pathname.
    !! Trailing spaces are ignored.
    LOGICAL :: System_Isfifo
    !! The system_isfifo() function should always be successful and no
    !! return value is reserved to indicate an error.
  END FUNCTION System_Isfifo
END INTERFACE

!----------------------------------------------------------------------------
!                                                System_Ischr@EnquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: checks if argument is a character device
!
!# System_Ischr
!
! The ischr(3f) function checks if path is a path to a character device.

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Ischr(pathname)
    CHARACTER(*), INTENT(IN) :: pathname
    !! a character string representing a character device pathname.
    !! Trailing spaces are ignored.
    LOGICAL :: System_Ischr
    !! The system_ischr() function should always be successful and no
    !! return value is reserved to indicate an error.
  END FUNCTION System_Ischr
END INTERFACE

!----------------------------------------------------------------------------
!                                                System_Isreg@EnquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: checks if argument is a regular file
!
!# System_Isreg
!
! The isreg(3f) function checks if path is a regular file
!
!## Examples 1
!
!```fortran
! {{% fortran-code file="examples/System_Isreg_test_1.F90" %}}
!```
!
!## Examples 2
!
!```fortran
! {{% fortran-code file="examples/System_Isreg_test_2.F90" %}}
!```

INTERFACE
  MODULE ELEMENTAL impure FUNCTION System_Isreg(pathname)
    CHARACTER(*), INTENT(IN) :: pathname
    !! a character string representing a pathname.
    !! Trailing spaces are ignored.
    LOGICAL :: System_Isreg
    !! The system_isreg() function should always be successful and no
    !! return value is reserved to indicate an error.
  END FUNCTION System_Isreg
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Islnk@EnquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: checks if argument is a link
!
!# System_Islnk
!
! The islnk(3f) function checks if path is a path to a link.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Islink_test_1.F90" %}}
!```

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Islnk(pathname)
    CHARACTER(len=*), INTENT(in) :: pathname
    !! a character string representing a link
    !! pathname. Trailing spaces are ignored.
    LOGICAL :: System_Islnk
    !! The system_islnk() function should always be
    !! successful and no return value is reserved to
    !! indicate an error.
  END FUNCTION System_Islnk
END INTERFACE

!----------------------------------------------------------------------------
!                                                System_Isblk@EnquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Check if argument is a block device
!
!# System_Isblk
!
! The isblk(3f) function checks if path is a path to a block device.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Isblk_test_1.F90" %}}
!```

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Isblk(pathname)
    CHARACTER(*), INTENT(IN) :: pathname
    !! a character string representing a block device pathname.
    !! Trailing spaces are ignored.
    LOGICAL :: System_Isblk
    !! The system_isblk() function should always be successful and no
    !! return value is reserved to indicate an error.
  END FUNCTION System_Isblk
END INTERFACE

!----------------------------------------------------------------------------
!                                                System_Isdir@EnquiryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: checks if argument is a directory of not
!
!# System_Isdir
!
! The system_isdir(3f) function checks if path is a directory.
!
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Isdir_test_1.F90" %}}
!```

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Isdir(dirname)
    CHARACTER(len=*), INTENT(in) :: dirname
    !! a character string representing a directory pathname.
    !! Trailing spaces are ignored.
    LOGICAL :: System_Isdir
    !! The system_isdir() function should always be successful and no
    !! return value is reserved to indicate an error.
  END FUNCTION System_Isdir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SystemEnquiry_Method
