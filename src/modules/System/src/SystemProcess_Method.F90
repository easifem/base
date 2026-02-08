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
!# SystemProcess_Method
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

MODULE SystemProcess_Method
USE ISO_C_BINDING, ONLY: C_FLOAT, C_INT, C_CHAR
USE ISO_C_BINDING, ONLY: C_PTR, C_F_POINTER, C_NULL_CHAR, C_NULL_PTR
USE ISO_C_BINDING, ONLY: C_LONG, C_SHORT, C_FUNPTR
USE GlobalData, ONLY: INT32, INT64
IMPLICIT NONE

PRIVATE

PUBLIC :: System_Perror
PUBLIC :: System_Stat
!! call stat(3c) to determine system information of file by name
PUBLIC :: System_Perm
!! create string representing file permission and type
PUBLIC :: System_Getumask
PUBLIC :: System_cpu_Time
PUBLIC :: System_Uname
PUBLIC :: System_Gethostname
PUBLIC :: System_Getlogin
PUBLIC :: System_Getpwuid
PUBLIC :: System_Getgrgid

! C types. Might be platform dependent
INTEGER, PARAMETER, PUBLIC :: mode_t = INT32
! Host names are limited to {HOST_NAME_MAX} bytes.
INTEGER(kind=mode_t), BIND(c, name="FHOST_NAME_MAX") :: HOST_NAME_MAX

!----------------------------------------------------------------------------
!                                                 System_Cpu_Time@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Get processor time by calling times
!
!# System_Cpu_Time
!
! Get processor time by calling times
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Cpu_Time" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Cpu_Time(total, user, system)
    REAL, INTENT(OUT) :: user, system, total
    !! C_Total   total processor time ( C_User + C_System )
    !! C_User    processor user time
    !! C_System  processor system time
  END SUBROUTINE System_Cpu_Time
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Getumask@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Get current umask
!
!# System_Getumask
!
! The return value from getumask(3f) is the value of the file
! creation mask, obtained by using umask(3c).
!
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Getumask_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Getumask() RESULT(Umask_Value)
    INTEGER :: Umask_Value
    !! The return value from umask() is just the previous value of the file
    !! creation mask, so that this system call can be used both to get and
    !! set the required values. Sadly, however,
    !! there is no way to get the old
    !! umask value without setting a new value at the same time.
    !! This means that in order just to see the current value,
    !! it is necessary
    !! to execute a piece of code like the following function:
  END FUNCTION System_Getumask
END INTERFACE

!----------------------------------------------------------------------------
!                                                    System_Perror@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: print error message for last C error on stderr
!
!# System_Perror
!
! Use system_perror(3f) to print an error message on stderr
! corresponding to the current value of the C global variable errno.
! Unless you use NULL as the argument prefix, the error message will
! begin with the prefix string, followed by a colon and a space
! (:). The remainder of the error message produced is one of the
! strings described for strerror(3c).
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Perror_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Perror(prefix)
    CHARACTER(len=*), INTENT(IN) :: prefix
  END SUBROUTINE System_Perror
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Getuname@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Get current system information
!
! ## System_Getuname
!
! Given a single-character selector, this routine returns the corresponding
! description of the current operating system.
!
! The `NAMEOUT` variable is assumed to be sufficiently large to hold the
! returned value.
!
! The following selector values are supported:
!
! - `s` Returns the kernel name.
! - `r` Returns the kernel release.
! - `v` Returns the kernel version.
! - `n` Returns the network node hostname.
! - `m` Returns the machine hardware name.
! - `T` Test mode: prints all information in the following order:
! `s r v n m`.

INTERFACE
  MODULE SUBROUTINE System_Uname(WHICH, NAMEOUT)
    CHARACTER(KIND=C_CHAR), INTENT(IN) :: WHICH
    CHARACTER(*), INTENT(OUT) :: NAMEOUT
  END SUBROUTINE System_Uname
END INTERFACE

!----------------------------------------------------------------------------
!                                             System_Gethostname@Getmethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Get name of current host
!
!# System_Gethostname
!
! The system_gethostname(3f) procedure returns the standard host
! name for the current machine.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Gethostname_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Gethostname(NAME, IERR)
    CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: NAME
    !! string returns the hostname.
    INTEGER, INTENT(OUT) :: IERR
    !! Upon successful completion, 0 shall be returned; otherwise, -1
    !! shall be returned.
  END SUBROUTINE System_Gethostname
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Getlogin@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Get login name
!
!## System_Getlogin
!
! The `system_getlogin(3f)` function returns a string containing the user
! name associated with the login activity of the controlling terminal of the
! current process.
!
! If the user name cannot be determined, the function returns a null string
! and sets `errno` to indicate the error.
!
! The following three user names associated with the current process can be
! determined:
!
! - `system_getpwuid(system_getuid())`
!   Returns the name associated with the real user ID of the process.
!
! - `system_getpwuid(system_geteuid())`
!   Returns the name associated with the effective user ID of the process.
!
! - `system_getlogin()`
!   Returns the name associated with the current login activity.!!
!!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Getlogin_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Getlogin() RESULT(fname)
    CHARACTER(:), ALLOCATABLE :: fname
  END FUNCTION System_Getlogin
END INTERFACE

!----------------------------------------------------------------------------
!                                                      System_Perm@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Get file type and permission as a string
!
!# System_Perm
!
! The system_perm(3f) function returns a string containing the type
! and permission of a file implied by the value of the mode value.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Perm_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Perm(mode) RESULT(perms)
    CLASS(*), INTENT(IN) :: mode
    CHARACTER(len=:), ALLOCATABLE :: perms
    !! returns the permission string in a format similar to that
    !! used by Unix commands such as ls(1).
  END FUNCTION System_Perm
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Getgrgid@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Get groupd name associated with a GID
!
!# System_Getgrgid
!
!    The System_Getgrgid() function returns a string containing the group
!    name associated with the given GID. If no match is found
!    it returns a null string and sets errno to indicate the error.
!
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Getgrgid_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Getgrgid(gid) RESULT(gname)
    CLASS(*), INTENT(IN) :: gid
    !! GID to try to look up associated group for. Can be of any
    !! INTEGER type.
    CHARACTER(len=:), ALLOCATABLE :: gname
    !! returns the group name. Blank if an error occurs
  END FUNCTION System_Getgrgid
END INTERFACE

!----------------------------------------------------------------------------
!                                                  System_Getpwuid@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Get login name associated with a UID
!
!# System_Getpwuid
!
! The system_getpwuid() function returns a string containing the user
! name associated with the given UID. If no match is found it returns
! a null string and sets errno to indicate the error.
!
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Getpwuid_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Getpwuid(uid) RESULT(uname)
    CLASS(*), INTENT(IN) :: uid
    !! UID to try to look up associated username for. Can be of any
    !! INTEGER type.
    CHARACTER(:), ALLOCATABLE :: uname
    !! returns the login name.
  END FUNCTION System_Getpwuid
END INTERFACE

!----------------------------------------------------------------------------
!                                                      System_Stat@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Get file status information
!
!# System_Stat
!
! This function returns information about a file. No permissions are
! required on the file itself, but execute (search) permission is required
! on all of the directories in path that lead to the file. The elements
! that are obtained and stored in the array VALUES:
!
! | Index | VALUES(n) | Description |
! |-------|-----------|-------------|
! | 1  | VALUES(1)  | Device ID |
! | 2  | VALUES(2)  | Inode number |
! | 3  | VALUES(3)  | File mode |
! | 4  | VALUES(4)  | Number of links |
! | 5  | VALUES(5)  | Owner UID |
! | 6  | VALUES(6)  | Owner GID |
! | 7  | VALUES(7)  | ID of device containing dir entry for file  |
! | 8  | VALUES(8)  | File size (bytes) |
! | 9  | VALUES(9)  | Last access time as a Unix Epoch time (seconds) |
! | 10 | VALUES(10) | Last modification time as a Unix Epoch time (seconds) |
! | 11 | VALUES(11) | Last file status change time as a Unix Epoch time |
! | 12 | VALUES(12) | Preferred I/O block size (-1 if not available) |
! | 13 | VALUES(13) | Number of blocks allocated (-1 if not available) |
!
! > [!NOTE]
! > Not all these elements are relevant on all systems.
! > If an element is not relevant, it is returned as `0`.!!
!
!
!## Examples
!
! ```fortran
! {{% fortran-code file="examples/System_Stat_test_1.F90" %}}
! ```

INTERFACE
  MODULE SUBROUTINE System_Stat(pathname, values, ierr)
    CHARACTER(*), INTENT(IN) :: pathname
    !! The type shall be CHARACTER, of the default kind and a valid
    !! path within the file system.
    INTEGER(INT64), INTENT(OUT) :: values(13)
    !! VALUES  The type shall be INTEGER(8), DIMENSION(13).
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr
  END SUBROUTINE System_Stat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SystemProcess_Method
