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
!    Fortran interface to C system interface.
!
!## Public objects
!
!   Public objects:
!
!    ! ENVIRONMENT
!    use M_system, only : set_environment_variable, system_unsetenv, &
!    system_putenv, system_getenv
!
!    use M_system, only :  system_intenv, system_readenv, system_clearenv
!    ! FILE SYSTEM
!    use M_system, only : system_getcwd, system_link,       &
!    system_mkfifo, system_remove, system_rename,           &
!    system_umask, system_unlink, fileglob,                 &
!    system_rmdir, system_chdir, system_mkdir,              &
!    system_stat, system_isdir, system_islnk, system_isreg, &
!    system_isblk, system_ischr, system_isfifo,             &
!    system_realpath,                                       &
!    system_access,                                         &
!    system_utime,                                          &
!    system_issock, system_perm,                            &
!    system_dir,                                            &
!    system_memcpy
!
!    !!use M_system, only : system_getc, system_putc
!    ! ERROR PROCESSING
!    use M_system, only : system_errno, system_perror
!    ! INFO
!    use M_system, only : system_getegid, system_geteuid, system_getgid, &
!    system_gethostname, system_getpid, system_getppid, system_setsid, &
!    system_getsid, system_getuid, system_uname
!    ! SIGNALS
!    use M_system, only : system_kill,system_signal
!    ! RANDOM NUMBERS
!    use M_system, only : system_rand, system_srand
!    ! PROCESS INFORMATION
!    use M_system, only : system_cpu_time
!
!##DESCRIPTION
!
!    M_system(3fm) is a collection of Fortran procedures that call C
!    or a C wrapper using the ISO_C_BINDING interface to access system calls.
!    System calls are a special set of functions used by programs to communicate
!    directly with an operating system.
!
!    Generally, system calls are slower than normal function calls because
!    when you make a call control is relinquished to the operating system
!    to perform the system call. In addition, depending on the nature of the
!    system call, your program may be blocked by the OS until the system call
!    has finished, thus making the execution time of your program even longer.
!
!    One rule-of-thumb that should always be followed when calling a system
!    call -- Always check the return value.
!
!## ENVIRONMENT ACCESS
!
!        o  system_putenv(3f):     call putenv(3c)
!        o  system_getenv(3f):     function call to get_environment_variable(3f)
!        o  system_unsetenv(3f):   call unsetenv(3c) to remove variable from environment
!        o  set_environment_variable(3f): set environment variable by calling setenv(3c)
!
!        o  system_initenv(3f):    initialize environment table for reading
!        o  system_readenv(3f):    read next entry from environment table
!        o  system_clearenv(3f):   emulate clearenv(3c) to clear environment
!
!## FILE SYSTEM
!
!        o  system_chdir(3f):      call chdir(3c) to change current directory of a process
!        o  system_getcwd(3f):     call getcwd(3c) to get pathname of current working directory
!
!        o  system_stat(3f):       determine system information of file by name
!        o  system_perm(3f):       create string representing file permission and type
!        o  system_access(3f):     determine filename access or existence
!        o  system_isdir(3f):      determine if filename is a directory
!        o  system_islnk(3f):      determine if filename is a link
!        o  system_isreg(3f):      determine if filename is a regular file
!        o  system_isblk(3f):      determine if filename is a block device
!        o  system_ischr(3f):      determine if filename is a character device
!        o  system_isfifo(3f):     determine if filename is a fifo - named pipe
!        o  system_issock(3f):     determine if filename is a socket
!        o  system_realpath(3f):   resolve a pathname
!
!        o  system_chmod(3f):      call chmod(3c) to set file permission mode
!        o  system_chown(3f):      call chown(3c) to set file owner
!        o  system_getumask(3f):   call umask(3c) to get process permission mask
!        o  system_setumask(3f):   call umask(3c) to set process permission mask
!
!        o  system_mkdir(3f):      call mkdir(3c) to create empty directory
!        o  system_mkfifo(3f):     call mkfifo(3c) to create a special FIFO file
!        o  system_link(3f):       call link(3c) to create a filename link
!
!        o  system_rename(3f):     call rename(3c) to change filename
!
!        o  system_remove(3f):     call remove(3c) to remove file
!        o  system_rmdir(3f):      call rmdir(3c) to remove empty directory
!        o  system_unlink(3f):     call unlink(3c) to remove a link to a file
!        o  system_utime(3f):      call utime(3c) to set file access and modification times
!        o  system_dir(3f):        read name of files in specified directory matching a wildcard string
!
!        o  fileglob(3f): Returns list of files using a file globbing pattern
!
!## STREAM IO
!
!        o  system_getc(3f): get a character from stdin
!        o  system_putc(3f): put a character on stdout
!
!## RANDOM NUMBERS
!
!        o  system_srand(3f): call srand(3c)
!        o  system_rand(3f): call rand(3c)
!
!## C ERROR INFORMATION
!
!        o  system_errno(3f): return errno(3c)
!        o  system_perror(3f): call perror(3c) to display last C error message
!
!## QUERIES
!
!        o  system_geteuid(3f): call geteuid(3c)
!        o  system_getuid(3f): call getuid(3c)
!        o  system_getegid(3f): call getegid(3c)
!        o  system_getgid(3f): call getgid(3c)
!        o  system_getpid(3f): call getpid(3c)
!        o  system_getppid(3f): call getppid(3c)
!        o  system_gethostname(3f): get name of current host
!        o  system_uname(3f): call my_uname(3c) which calls uname(3c)
!        o  system_getlogin(3f): get login name
!        o  system_getpwuid(3f): get login name associated with given UID
!        o  system_getgrgid(3f): get group name associated with given GID
!        o  system_cpu_time(3f) : get processor time in seconds using times(3c)
!
!## FUTURE DIRECTIONS
!
!    A good idea of what system routines are commonly required is to refer
!    to the POSIX binding standards. (Note: IEEE 1003.9-1992 was withdrawn 6
!    February 2003.) The IEEE standard covering Fortran 77 POSIX bindings
!    is available online, though currently (unfortunately) only from
!    locations with appropriate subscriptions to the IEEE server (e.g.,
!    many university networks). For those who do have such access, the link
!    is: POSIX Fortran 77 Language Interfaces (IEEE Std 1003.9-1992) (pdf)
!
!## SEE ALSO
!
!    Some vendors provide their own way to access POSIX functions and make
!    those available as modules; for instance ...
!
!       o the IFPORT module of Intel
!       o or the f90_* modules of NAG.
!       o There are also other compiler-independent efforts to make the
!         POSIX procedures accessible from Fortran...
!
!          o Posix90 (doc),
!          o flib.a platform/files and directories,
!          o fortranposix.

MODULE System_Method
USE ISO_C_BINDING, ONLY: C_FLOAT, C_INT, C_CHAR
USE ISO_C_BINDING, ONLY: C_PTR, C_F_POINTER, C_NULL_CHAR, C_NULL_PTR
USE ISO_C_BINDING, ONLY: C_LONG, C_SHORT, C_FUNPTR

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8, INT16, INT32, INT64
!!, real32, real64, real128, dp=>real128
USE SystemInterface
IMPLICIT NONE

PRIVATE

! C types. Might be platform dependent
INTEGER, PARAMETER, PUBLIC :: mode_t = INT32

PUBLIC :: system_rand
PUBLIC :: system_srand

!-!public :: system_getc
!-!public :: system_putc

PUBLIC :: system_getpid
!! return process ID
PUBLIC :: system_getppid
!! return parent process ID
PUBLIC :: system_getuid, system_geteuid
!! return user ID
PUBLIC :: system_getgid, system_getegid
!! return group ID
PUBLIC :: system_setsid
PUBLIC :: system_getsid
PUBLIC :: system_kill
!! (pid, signal) kill process (defaults: pid=0, signal=SIGTERM)
PUBLIC :: system_signal
!! (signal,[handler]) install signal handler subroutine

PUBLIC :: system_errno
PUBLIC :: system_perror

PUBLIC :: system_putenv
PUBLIC :: system_getenv
PUBLIC :: set_environment_variable
PUBLIC :: system_unsetenv

PUBLIC :: system_initenv
PUBLIC :: system_readenv
PUBLIC :: system_clearenv

PUBLIC :: system_stat
!! call stat(3c) to determine system information of file by name
PUBLIC :: system_perm
!! create string representing file permission and type
PUBLIC :: system_access
!! determine filename access or existence
PUBLIC :: system_isdir
!! determine if filename is a directory
PUBLIC :: system_islnk
!! determine if filename is a link
PUBLIC :: system_isreg
!! determine if filename is a regular file
PUBLIC :: system_isblk
!! determine if filename is a block device
PUBLIC :: system_ischr
!! determine if filename is a character device
PUBLIC :: system_isfifo
!! determine if filename is a fifo - named pipe
PUBLIC :: system_issock
!! determine if filename is a socket
PUBLIC :: system_realpath
!! resolve pathname

PUBLIC :: system_chdir
PUBLIC :: system_rmdir
PUBLIC :: system_remove
PUBLIC :: system_rename

PUBLIC :: system_mkdir
PUBLIC :: system_mkfifo
PUBLIC :: system_chmod
PUBLIC :: system_chown
PUBLIC :: system_link
PUBLIC :: system_unlink
PUBLIC :: system_utime

PUBLIC :: system_setumask
PUBLIC :: system_getumask
PUBLIC :: system_umask

PUBLIC :: system_getcwd

PUBLIC :: system_opendir
PUBLIC :: system_readdir
PUBLIC :: system_rewinddir
PUBLIC :: system_closedir

PUBLIC :: system_cpu_time

PUBLIC :: system_uname
PUBLIC :: system_gethostname
PUBLIC :: system_getlogin
PUBLIC :: system_getpwuid
PUBLIC :: system_getgrgid
PUBLIC :: fileglob

PUBLIC :: system_alarm
PUBLIC :: system_calloc
PUBLIC :: system_clock
PUBLIC :: system_time
!public :: system_time
!public :: system_qsort

PUBLIC :: system_realloc
PUBLIC :: system_malloc
PUBLIC :: system_free
PUBLIC :: system_memcpy

PUBLIC :: system_dir

PUBLIC :: R_GRP, R_OTH, R_USR, RWX_G, RWX_O, RWX_U, W_GRP, W_OTH, W_USR, X_GRP
PUBLIC :: X_OTH, X_USR, DEFFILEMODE, ACCESSPERMS
PUBLIC :: R_OK, W_OK, X_OK, F_OK
!! for system_access

!----------------------------------------------------------------------------
!                                                              dirent_SYSTEMA
!----------------------------------------------------------------------------

TYPE, BIND(C) :: dirent_SYSTEMA
  INTEGER(C_LONG) :: d_ino
  INTEGER(C_LONG) :: d_off
  INTEGER(C_SHORT) :: d_reclen
  CHARACTER(len=1, kind=C_CHAR) :: d_name(256)
END TYPE dirent_SYSTEMA

!----------------------------------------------------------------------------
!                                                              dirent_CYGWIN
!----------------------------------------------------------------------------

TYPE, BIND(C) :: dirent_CYGWIN
  INTEGER(C_INT) :: d_version
  INTEGER(C_LONG) :: d_ino
  CHARACTER(kind=C_CHAR) :: d_type
  CHARACTER(kind=C_CHAR) :: d_unused1(3)
  INTEGER(C_INT) :: d_internal1
  CHARACTER(len=1, kind=C_CHAR) :: d_name(256)
END TYPE dirent_CYGWIN

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

integer(kind=c_long),bind(c,name="longest_env_variable") :: longest_env_variable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER(kind=mode_t), BIND(c, name="FS_IRGRP") :: R_GRP
INTEGER(kind=mode_t), BIND(c, name="FS_IROTH") :: R_OTH
INTEGER(kind=mode_t), BIND(c, name="FS_IRUSR") :: R_USR
INTEGER(kind=mode_t), BIND(c, name="FS_IRWXG") :: RWX_G
INTEGER(kind=mode_t), BIND(c, name="FS_IRWXO") :: RWX_O
INTEGER(kind=mode_t), BIND(c, name="FS_IRWXU") :: RWX_U
INTEGER(kind=mode_t), BIND(c, name="FS_IWGRP") :: W_GRP
INTEGER(kind=mode_t), BIND(c, name="FS_IWOTH") :: W_OTH
INTEGER(kind=mode_t), BIND(c, name="FS_IWUSR") :: W_USR
INTEGER(kind=mode_t), BIND(c, name="FS_IXGRP") :: X_GRP
INTEGER(kind=mode_t), BIND(c, name="FS_IXOTH") :: X_OTH
INTEGER(kind=mode_t), BIND(c, name="FS_IXUSR") :: X_USR
INTEGER(kind=mode_t), BIND(c, name="FDEFFILEMODE") :: DEFFILEMODE
INTEGER(kind=mode_t), BIND(c, name="FACCESSPERMS") :: ACCESSPERMS

! Host names are limited to {HOST_NAME_MAX} bytes.
INTEGER(kind=mode_t), BIND(c, name="FHOST_NAME_MAX") :: HOST_NAME_MAX

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! for system_access(3f)
!integer(kind=c_int),bind(c,name="F_OK") :: F_OK
!integer(kind=c_int),bind(c,name="R_OK") :: R_OK
!integer(kind=c_int),bind(c,name="W_OK") :: W_OK
!integer(kind=c_int),bind(c,name="X_OK") :: X_OK
! not sure these will be the same on all systems, but above did not work
INTEGER(kind=C_INT), PARAMETER :: F_OK = 0
INTEGER(kind=C_INT), PARAMETER :: R_OK = 4
INTEGER(kind=C_INT), PARAMETER :: W_OK = 2
INTEGER(kind=C_INT), PARAMETER :: X_OK = 1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE !  mold for signal handler to be installed by system_signal
  SUBROUTINE handler(signum)
    INTEGER :: signum
  END SUBROUTINE handler
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE handler_pointer
  PROCEDURE(handler), POINTER, NOPASS :: sub
END TYPE handler_pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER, PARAMETER :: no_of_signals = 64
!!  obtained with command: kill -l

TYPE(handler_pointer), DIMENSION(no_of_signals) :: handler_ptr_array

!----------------------------------------------------------------------------
!                                                System_Signal@SignalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Install a signal handler
!
!# System_Signal
!
! Calling system_signal(NUMBER, HANDLER) causes user-defined
! subroutine HANDLER to be executed when the signal NUMBER is
! caught. The same subroutine HANDLER maybe installed to handle
! different signals. HANDLER takes only one integer argument which
! is assigned the signal number that is caught. See sample program
! below for illustration.
!
! Calling system_signal(NUMBER) installs a do-nothing handler. This
! is not equivalent to ignoring the signal NUMBER though, because
! the signal can still interrupt any sleep or idle-wait.
!
! Note that the signals SIGKILL and SIGSTOP cannot be handled
! this way.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Signal_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Signal(signum, handler_routine)
    INTEGER, INTENT(in) :: signum
    PROCEDURE(handler), OPTIONAL :: handler_routine
    TYPE(C_FUNPTR) :: ret, c_handler
  END SUBROUTINE System_Signal
END INTERFACE

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
!                                                   System_Utime@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Set file access and modification times
!
!# System_Utime
!
! The system_utime(3f) function sets the access and modification
! times of the file named by the path argument by calling utime(3c).
!
! If times() is not present the access and modification times of
! the file shall be set to the current time.
!
! To use system_utime(3f) the effective user ID of the process must
! match the owner of the file, or the process has to have write
! permission to the file or have appropriate privileges,
!
!## Errors
!
!The underlying utime(3c) function fails if:
!
!### EACCES
!
! Search permission is denied by a component of the path
! prefix; or the times argument is a null pointer and the
! effective user ID of the process does not match the owner
! of the file, the process does not have write permission
! for the file, and the process does not have appropriate
! privileges.
!
!### ELOOP
!
! A loop exists in symbolic links encountered during
! resolution of the path argument.
!
!### ENAMETOOLONG
!
! The length of a component of a pathname is longer than {NAME_MAX}.
!
!### ENOENT
!
! A component of path does not name an existing file or path is an
! empty string.
!
!### ENOTDIR
!
! A component of the path prefix names an existing file
! that is neither a directory nor a symbolic link to a
! directory, or the path argument contains at least one
! non-<slash> character and ends with one or more trailing
! <slash> characters and the last pathname component
! names an existing file that is neither a directory nor
! a symbolic link to a directory.
!
!### EPERM
!
! The times argument is not a null pointer and the effective
! user ID of the calling process does not match the owner
! of the file and the calling process does not have
! appropriate privileges.
!
!### EROFS
!
! The file system containing the file is read-only.
!
!## Note
!
! The utime() function may fail if:
!
!- ELOOP  More than {SYMLOOP_MAX} symbolic links were encountered
!during resolution of the path argument.
!
!- ENAMETOOLONG  The length of a pathname exceeds {PATH_MAX}, or
! pathname resolution of a symbolic link produced
! an intermediate result with a length that exceeds
! {PATH_MAX}.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Utime_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Utime(pathname, times)
    CHARACTER(len=*), INTENT(in) :: pathname
    !!name of the file whose access and modification times are to be updated.
    INTEGER, INTENT(in), OPTIONAL :: times(2)
    !! If present, the values will be interpreted as the access
    !! and modification times as Unix Epoch values. That is,
    !! they are times measured in seconds since the Unix Epoch.
    LOGICAL :: System_Utime
    !! Upon successful completion .TRUE. is returned. Otherwise,
    !! .FALSE. is returned and errno shall be set to indicate the error,
    !! and the file times remain unaffected.
  END FUNCTION System_Utime
END INTERFACE

!----------------------------------------------------------------------------
!                                                System_RealPath@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Resolve the relative path
!
!# System_Realpath
!
! system_realpath(3f) calls the C routine realpath(3c) to obtain
! the absolute pathname of given path
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Realpath_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Realpath(input) RESULT(string)
    CHARACTER(*), INTENT(in) :: input
    !! pathname to resolve
    CHARACTER(:), ALLOCATABLE :: string
    !! The absolute pathname of the given input pathname.
    !! The pathname shall contain no components that are dot
    !! or dot-dot, or are symbolic links. It is equal to the
    !! NULL character if an error occurred.
  END FUNCTION System_Realpath
END INTERFACE

!----------------------------------------------------------------------------
!
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Str2_Carr

INTERFACE
  MODULE PURE FUNCTION str2_carr(string) RESULT(array)
    CHARACTER(*), INTENT(in) :: string
    CHARACTER(len=1, kind=C_CHAR) :: array(LEN(string) + 1)
  END FUNCTION str2_carr
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
!                                                   System_Chown@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: change file owner and group
!
!# System_Chown
!
! Elemental impure logical function system_chown(path,owner,group)
!
! The chown(3f) function changes owner and group of a file
!
! The path argument points to a pathname naming a file. The
! user ID and group ID of the named file shall be set to the numeric
! values contained in owner and group, respectively.
!
! Only processes with an effective user ID equal to the user ID of
! the file or with appropriate privileges may change the ownership
! of a file.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Chown_test_1.F90" %}}
!```

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Chown(dirname, owner, group)
    CHARACTER(*), INTENT(IN) :: dirname
    !! A character string representing a file pathname.
    !! Trailing spaces are ignored.
    INTEGER, INTENT(IN) :: owner
    !! UID of owner that ownership is to be changed to
    INTEGER, INTENT(IN) :: group
    !! GID of group that ownership is to be changed to
    LOGICAL :: System_Chown
    !! The system_chown(3f) function should return zero 0 if successful.
    !! Otherwise, these functions shall return 1 and set errno to
    !! indicate the error. If 1 is returned, no changes are made in
    !! the user ID and group ID of the file.
  END FUNCTION System_Chown
END INTERFACE

!----------------------------------------------------------------------------
!
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
!                                                    System_Link@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: link one file to another file relative to two directory
!          descriptors
!
!# System_Link
!
! The link() function shall create a new link (directory entry)
! for the existing file, path1.
!
! The path1 argument points to a pathname naming an existing
! file. The path2 argument points to a pathname naming the
! new directory entry to be created. The link() function shall
! atomically create a new link for the existing file and the link
! count of the file shall be incremented by one.
!
! If path1 names a directory, link() shall fail unless the process
! has appropriate privileges and the implementation supports using
! link() on directories.
!
! If path1 names a symbolic link, it is implementation-defined
! whether link() follows the symbolic link, or creates a new link
! to the symbolic link itself.
!
! Upon successful completion, link() shall mark for update the
! last file status change timestamp of the file. Also, the last
! data modification and last file status change timestamps of the
! directory that contains the new entry shall be marked for update.
!
! If link() fails, no link shall be created and the link count of
! the file shall remain unchanged.
!
! The implementation may require that the calling process has
! permission to access the existing file.
!
! The linkat() function shall be equivalent to the link() function
! except that symbolic links shall be handled as specified by the
! value of flag (see below) and except in the case where either path1
! or path2 or both are relative paths. In this case a relative path
! path1 is interpreted relative to the directory associated with
! the file descriptor fd1 instead of the current working directory
! and similarly for path2 and the file descriptor fd2. If the
! file descriptor was opened without O_SEARCH, the function shall
! check whether directory searches are permitted using the current
! permissions of the directory underlying the file descriptor. If
! the file descriptor was opened with O_SEARCH, the function shall
! not perform the check.
!
! Values for flag are constructed by a bitwise-inclusive OR of
! flags from the following list, defined in <fcntl.h>:
!
! AT_SYMLINK_FOLLOW
! If path1 names a symbolic link, a new link for the target
! of the symbolic link is created.
!
! If linkat() is passed the special value AT_FDCWD in the fd1 or
! fd2 parameter, the current working directory shall be used for the
! respective path argument. If both fd1 and fd2 have value AT_FDCWD,
! the behavior shall be identical to a call to link(), except that
! symbolic links shall be handled as specified by the value of flag.
!
! Some implementations do allow links between file systems.
!
! If path1 refers to a symbolic link, application developers should
! use linkat() with appropriate flags to select whether or not the
! symbolic link should be resolved.
!
! If the AT_SYMLINK_FOLLOW flag is clear in the flag argument and
! the path1 argument names a symbolic link, a new link is created
! for the symbolic link path1 and not its target.

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Link(oldname, newname) RESULT(ierr)
    CHARACTER(len=*), INTENT(IN) :: oldname
    CHARACTER(len=*), INTENT(IN) :: newname
    INTEGER :: ierr
    !! Upon successful completion, these functions shall return
    !! 0. Otherwise, these functions shall return -1 and set errno to
    !! indicate the error.
  END FUNCTION System_Link
END INTERFACE

!----------------------------------------------------------------------------
!                                                  System_Unlink@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: remove a directory entry relative to directory file descriptor
!
!# System_Unlink
!
! The unlink() function shall remove a link to a file. If path names a
! symbolic link, unlink() shall remove the symbolic link named by path
! and shall not affect any file or directory named by the contents of
! the symbolic link. Otherwise, unlink() shall remove the link named by
! the pathname pointed to by path and shall decrement the link count of
! the file referenced by the link.
!
! When the files link count becomes 0 and no process has the file open,
! the space occupied by the file shall be freed and the file shall no
! longer be accessible. If one or more processes have the file open when
! the last link is removed, the link shall be removed before unlink()
! returns, but the removal of the file contents shall be postponed until
! all references to the file are closed.
!
! The path argument shall not name a directory unless the process has
! appropriate privileges and the implementation supports using unlink()
! on directories.
!
! Upon successful completion, unlink() shall mark for update the last
! data modification and last file status change timestamps of the parent
! directory. Also, if the file link count is not 0, the last file status
! change timestamp of the file shall be marked for update.
!
! Values for flag are constructed by a bitwise-inclusive OR of flags from
! the following list, defined in <fcntl.h>:
!
! AT_REMOVEDIR
!
! Remove the directory entry specified by fd and path as a
! directory, not a normal file.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Unlink_test_1.F90" %}}
!```

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Unlink(fname) RESULT(ierr)
    CHARACTER(len=*), INTENT(in) :: fname
    INTEGER :: ierr
    !! Upon successful completion, these functions shall return 0. Otherwise,
    !! these functions shall return -1 and set errno to indicate the error.
    !! If -1 is returned, the named file shall not be changed.
  END FUNCTION System_Unlink
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Setumask@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Set the file mode creation umask
!
!# System_Setumask
!
! The `system_umask(3f)` function sets the file mode creation mask of
! the calling process to `cmask` and returns the previous value of
! the mask.
!
! Only the file permission bits of `cmask` (see `<sys/stat.h>`) are
! used. The interpretation of any other bits is
! implementation-defined.
!
!### Effect of the file creation mask
!
! The file mode creation mask is applied to the `mode` argument
! supplied to the following functions:
!
! - `open()`, `openat()`, `creat()`
! - `mkdir()`, `mkdirat()`, `mkfifo()`, `mkfifoat()`
! - `mknod()`, `mknodat()`
! - `mq_open()`
! - `sem_open()`
!
!## Semantics
!
! - Bit positions that are set in `cmask` are cleared in the `mode`
!   of any subsequently created file or object.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Setumask_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Setumask(Umask_Value) RESULT(Old_Umask)
    INTEGER, INTENT(in) :: Umask_Value
    INTEGER :: Old_Umask
    !! The file permission bits in the value returned by umask() shall be
    !! the previous value of the file mode creation mask. The state of any
    !! other bits in that value is unspecified, except that a subsequent
    !! call to umask() with the returned value as cmask shall leave the
    !! state of the mask the same as its state before the first call,
    !! including any unspecified use of those bits.
  END FUNCTION System_Setumask
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
!                                                   System_Chdir@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: change working directory
!
!# System_Chdir
!
! The `system_chdir(3f)` procedure changes the current working directory
! of the calling process to the directory specified by `path`.
!
! The current working directory is used as the starting point for
! interpreting relative pathnames (those not beginning with `/`).
!
!## Errors
!
! On failure, an error condition is reported as described below. The
! specific error returned may depend on the underlying file system.
!
! The following errors correspond to the C `chdir()` definitions:
!
! - `EACCES`
!   Search permission is denied for one of the components of `path`.
!   See also `path_resolution(7)`.
!
! - `EFAULT`
!   `path` points outside the accessible address space.
!
! - `EIO`
!   An I/O error occurred.
!
! - `ELOOP`
!   Too many symbolic links were encountered while resolving `path`.
!
! - `ENAMETOOLONG`
!   `path` is too long.
!
! - `ENOENT`
!   The specified file does not exist.
!
! - `ENOMEM`
!   Insufficient kernel memory was available.
!
! - `ENOTDIR`
!   A component of `path` is not a directory.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Chdir_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Chdir(path, err)
    CHARACTER(len=*), INTENT(IN) :: path
    INTEGER, OPTIONAL, INTENT(OUT) :: err
    !! On success, zero is returned. On error, -1 is returned, and errno is
    !! set appropriately.
  END SUBROUTINE System_Chdir
END INTERFACE

!----------------------------------------------------------------------------
!                                                  System_Remove@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: remove a file
!
!# System_Remove
!
! Fortran supports scratch files via the OPEN(3c) command; but does
! not otherwise allow for removing files. The system_remove(3f) command
! allows for removing files by name that the user has the authority to
! remove by calling the C remove(3c) function.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Remove_test_1.F90" %}}
!```

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Remove(path) RESULT(err)
    CHARACTER(*), INTENT(in) :: path
    INTEGER(C_INT) :: err
  END FUNCTION System_Remove
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Rename@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary:  rename a system file
!
!# System_Rename
!
! Rename a file by calling rename(3c). It is not recommended that the
! rename occur while either filename is being used on a file currently
! OPEN(3f) by the program.
! Both the old and new names must be on the same device.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Rename_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Rename(input, output) RESULT(ierr)
    CHARACTER(*), INTENT(IN) :: input, output
    !! system filename of an existing file to rename
    !! system filename to be created or overwritten by INPUT file.
    !! Must be on the same device as the INPUT file.
    INTEGER :: ierr
    !! zero (0) if no error occurs. If not zero a call to
    !! system_errno(3f) or system_perror(3f) is supported
    !! to diagnose error
  END FUNCTION System_Rename
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: call chmod(3c) to change permission mode of a file
!          relative to directory file descriptor
!
!# System_Chmod
!
! The `system_chmod(3f)` function changes the `S_ISUID`, `S_ISGID`,
! `S_ISVTX`, and file permission bits of the file specified by `path`
! to the corresponding bits in the `mode` argument.
!
! The application shall ensure that the effective user ID of the
! calling process matches the owner of the file, or that the process
! has sufficient privileges.
!
! The constants `S_ISUID`, `S_ISGID`, `S_ISVTX`, and the file
! permission bits are defined in `<sys/stat.h>`.
!
!##  Privilege and group semantics
!
! - If the calling process lacks appropriate privileges, and
!   the group ID of the file does not match the effective group ID
!   or any supplementary group ID, then `S_ISGID` is cleared on
!   successful return when the file is a regular file.
!
! - Additional implementation-defined restrictions may cause the
!   `S_ISUID` and `S_ISGID` bits in `mode` to be ignored.
!
!## Timestamps
!
! - Upon successful completion, `system_chmod()` marks the last
!   file status change timestamp of the file for update.
!
!## Flags
!
! Values for `flag` are constructed using a bitwise-inclusive OR of
! the following values defined in `<fcntl.h>`:
!
! - `AT_SYMLINK_NOFOLLOW`
!   If `path` names a symbolic link, the mode of the symbolic link
!   itself is changed rather than the target.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Chmod_test_1.F90" %}}
!```
!
INTERFACE
  MODULE FUNCTION System_Chmod(filename, mode) RESULT(ierr)
    CHARACTER(*), INTENT(IN) :: filename
    INTEGER, VALUE, INTENT(IN) :: mode
    INTEGER :: ierr
    !! Upon successful completion, system_chmod(3f) returns 0.
    !! Otherwise, it returns -1 and sets errno to indicate the error. If
    !! -1 is returned, no change to the file mode occurs.
  END FUNCTION System_Chmod
END INTERFACE

!----------------------------------------------------------------------------
!                                                  System_Getcwd@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Get current working directory
!
!# System_Getcwd
!
! Get current working directory
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Getcwd_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Getcwd(output, ierr)
    CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: output
    !! The absolute pathname of the current working directory
    !! The pathname shall contain no components that are dot or dot-dot,
    !! or are symbolic links.
    INTEGER, INTENT(out) :: ierr
    !! ierr is not zero if an error occurs.
  END SUBROUTINE System_Getcwd
END INTERFACE

!----------------------------------------------------------------------------
!                                                   System_Rmdir@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: remove empty directories
!
!# System_Rmdir
!
! Remove empty directories.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Rmdir_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Rmdir(dirname) RESULT(err)
    CHARACTER(*), INTENT(IN) :: dirname
    !! The name of a directory to remove if it is empty
    INTEGER(C_INT) :: err
    !! zero (0) if no error occurred
  END FUNCTION System_Rmdir
END INTERFACE

!----------------------------------------------------------------------------
!                                                  System_Mkfifo@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: make a FIFO special file relative to directory file descriptor
!
!# System_Mkfifo
!
! A regular pipe can only connect two related processes. It is created
! by a process and vanishes when the last process closes it.
!
! A named pipe, also known as a FIFO, can connect two unrelated
! processes and exists independently of the processes using it.
! A FIFO is created using the `mkfifo()` library function.
!
!## Behavior and semantics
!
! - `mkfifo()` creates a new FIFO special file specified by `pathname`.
! - The file permission bits of the new FIFO are initialized from
!   `mode`.
! - The permission bits specified in `mode` are modified by the
!   process file creation mask.
! - If bits other than file permission bits are set in `mode`,
!   the effect is implementation-defined.
! - If `pathname` names a symbolic link, `mkfifo()` fails and sets
!   `errno` to `EEXIST`.
! - The FIFO user ID is set to the effective user ID of the process.
! - The FIFO group ID is set either to the group ID of the parent
!   directory or to the effective group ID of the process.
! - Implementations shall provide a method to initialize the FIFO
!   group ID from the parent directory.
! - Implementations may optionally provide a method to initialize
!   the FIFO group ID from the effective group ID of the caller.
! - Upon successful completion, the FIFO last access, modification,
!   and status change timestamps are marked for update.
! - The directory containing the new FIFO also has its modification
!   and status change timestamps updated.
!
!## Permission modes
!
! Predefined variables are typically used to specify permission modes.
! These variables may be combined using a bytewise OR operation.
!
! Permission bits by category:
!
! - **User**
!   - `R_USR` : read
!   - `W_USR` : write
!   - `X_USR` : execute
!
! - **Group**
!   - `R_GRP` : read
!   - `W_GRP` : write
!   - `X_GRP` : execute
!
! - **Others**
!   - `R_OTH` : read
!   - `W_OTH` : write
!   - `X_OTH` : execute
!
!## Shortcut constants
!
! The following predefined constants represent common combinations:
!
! - `RWX_U` : read, write, execute for user
! - `RWX_G` : read, write, execute for group
! - `RWX_O` : read, write, execute for others
! - `DEFFILEMODE`
!   Equivalent to octal `0666` (`rw-rw-rw-`)
! - `ACCESSPERMS`
!   Equivalent to octal `0777` (`rwxrwxrwx`)
!
!## Examples
!
! To grant read, write, and execute permissions only to the user:
!
! - `ierr = mkfifo("myfile", IANY([R_USR, W_USR, X_USR]))`
! - `ierr = mkfifo("myfile", RWX_U)`
!
! To grant full permissions to all users (mode `0777`):
!
! - `ierr = mkfifo("myfile", IANY([R_USR, W_USR, X_USR, R_GRP, W_GRP, &
!   X_GRP, R_OTH, W_OTH, X_OTH]))`
! - `ierr = mkfifo("myfile", IANY([RWX_U, RWX_G, RWX_O]))`
! - `ierr = mkfifo("myfile", ACCESSPERMS)`
!
!```fortran
! {{% fortran-code file="examples/System_Mkfifo_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Mkfifo(pathname, mode) RESULT(err)
    CHARACTER(*), INTENT(IN) :: pathname
    INTEGER, INTENT(IN) :: mode
    INTEGER :: err
    !! Upon successful completion, return 0.
    !! Otherwise, return -1 and set errno to indicate the error.
    !! If -1 is returned, no FIFO is created.
  END FUNCTION System_Mkfifo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   System_Mkdir@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: call mkdir(3c) to create a new directory
!
!# System_Mkdir
!
! Predefined variables are typically used to set permission modes.
! These variables can be combined using a bytewise OR operation to
! create commonly used permission settings.
!
! Permission bits by category:
!
! - **User**
!   - `R_USR` : read
!   - `W_USR` : write
!   - `X_USR` : execute
!
! - **Group**
!   - `R_GRP` : read
!   - `W_GRP` : write
!   - `X_GRP` : execute
!
! - **Others**
!   - `R_OTH` : read
!   - `W_OTH` : write
!   - `X_OTH` : execute
!
! Additional shortcut constants are provided. These are predefined
! bitwise-OR combinations of the permission flags listed above:
!
! - `RWX_U` : read, write, and execute for user
! - `RWX_G` : read, write, and execute for group
! - `RWX_O` : read, write, and execute for others
! - `DEFFILEMODE`
!   Equivalent to octal `0666` (`rw-rw-rw-`)
! - `ACCESSPERMS`
!   Equivalent to octal `0777` (`rwxrwxrwx`)
!
! To grant only the user read, write, and execute permissions, while
! denying all permissions to group members and others, any of the
! following `mkdir()` calls may be used equivalently:
!
! - `ierr = mkdir("mydir", IANY([R_USR, W_USR, X_USR]))`
! - `ierr = mkdir("mydir", RWX_U)`
!
! To grant full permissions to all users (mode `0777`, `rwxrwxrwx`),
! any of the following calls may be used equivalently:
!
! - `ierr = mkdir("mydir", IANY([R_USR, W_USR, X_USR, R_GRP, W_GRP, X_GRP, &
!   R_OTH, W_OTH, X_OTH]))`
! - `ierr = mkdir("mydir", IANY([RWX_U, RWX_G, RWX_O]))`
! - `ierr = mkdir("mydir", ACCESSPERMS)`
!
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Mkdir_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Mkdir(dirname, mode) RESULT(ierr)
    CHARACTER(len=*), INTENT(in) :: dirname
    INTEGER, INTENT(in) :: mode
    INTEGER :: ierr
  END FUNCTION System_Mkdir
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Opendir@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: open directory stream by calling opendir
!
!# System_Opendir
!
! The `system_opendir(3f)` procedure opens a directory stream that
! corresponds to the directory specified by the `dirname` argument.
!
! The directory stream is positioned at the first directory entry.
!
!## Return value
!
! - Upon successful completion, a pointer to a C `DIR` type is returned.
!
! - On failure, a null pointer is returned and `IERR` is set to indicate
!   the error condition.
!
!## Errors
!
! Errors correspond to the conditions described for `opendir(3c)`,
! including the following:
!
! - `EACCES`
!   Search permission is denied for a component of the path prefix of
!   `dirname`, or read permission is denied for `dirname`.
!
! - `ELOOP`
!   A loop exists in symbolic links encountered during resolution of
!   the `dirname` argument.
!
! - `ENAMETOOLONG`
!   The length of a pathname component exceeds `{NAME_MAX}`.
!
! - `ENOENT`
!   A component of `dirname` does not name an existing directory, or
!   `dirname` is an empty string.
!
! - `ENOTDIR`
!   A component of `dirname` names an existing file that is neither a
!   directory nor a symbolic link to a directory.
!
! - `ELOOP`
!   More than `{SYMLOOP_MAX}` symbolic links were encountered during
!   resolution of the `dirname` argument.
!
! - `EMFILE`
!   All file descriptors available to the process are currently open.
!
! - `ENAMETOOLONG`
!   The length of a pathname exceeds `{PATH_MAX}`, or pathname
!   resolution of a symbolic link produced an intermediate result whose
!   length exceeds `{PATH_MAX}`.
!
! - `ENFILE`
!   Too many files are currently open in the system.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Opendir_test_1.F90" %}}
!```
!
INTERFACE
  MODULE SUBROUTINE System_Opendir(dirname, dir, ierr)
    CHARACTER(len=*), INTENT(IN) :: dirname
    !! name of directory to open a directory stream for
    TYPE(C_PTR), INTENT(INOUT) :: dir
    !! pointer to directory stream. If an
    !! error occurred, it will not be associated.
    INTEGER, INTENT(OUT) :: ierr
    !! ierr  0 indicates no error occurred
  END SUBROUTINE System_Opendir
END INTERFACE

!----------------------------------------------------------------------------
!                                                 System_Readdir@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Read a directory
!
!# System_Readdir
!
! system_readdir(3f) returns the name of the directory entry at the
! current position in the directory stream specified by the argument
! DIR, and positions the directory stream at the next entry. It returns
! a null name upon reaching the end of the directory stream.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Readdir_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Readdir(dir, filename, ierr)
    TYPE(C_PTR), VALUE :: dir
    !! A pointer to the directory opened by system_opendir(3f).
    CHARACTER(len=:), INTENT(out), ALLOCATABLE :: filename
    !! the name of the directory entry at the current position in
    !! the directory stream specified by the argument DIR, and
    !! positions the directory stream at the next entry.
    !! The readdir() function does not return directory entries
    !! containing empty names. If entries for dot or dot-dot exist,
    !! one entry is returned for dot and one entry is returned
    !! for dot-dot.
    !! The entry is marked for update of the last data access
    !! timestamp each time it is read.
    !! reaching the end of the directory stream, the name is a blank name.
    INTEGER, INTENT(out) :: ierr
    !! If IERR is set to non-zero on return, an error occurred.
  END SUBROUTINE System_Readdir
END INTERFACE

!----------------------------------------------------------------------------
!                                               System_Rewinddir@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Rewind directory stream
!
!# System_Rewinddir
!
! Return to pointer to the beginning of the list for a currently open
! directory list.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Rewinddir_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Rewinddir(dir)
    TYPE(C_PTR), VALUE :: dir
    !! A C_Pointer assumed to have been allocated by a
    !! call to SYSTEM_OPENDIR(3f).
  END SUBROUTINE System_Rewinddir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Close a directory stream by calling closedir
!
!# System_Closedir
!
! The SYSTEM_CLOSEDIR(3f) function closes the directory stream
!  referred to by the argument DIR. Upon return, the value of DIR may no
!  longer point to an accessible object.
!
! system_closedir(3f) may fail if:
!
!- EBADF:  The dirp argument does not refer to an open directory stream.
!- EINTR:  The closedir() function was interrupted by a signal.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Closedir_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Closedir(dir, ierr)
    TYPE(C_PTR), VALUE :: dir
    !! directory stream pointer opened by SYSTEM_OPENDIR(3f).
    INTEGER, INTENT(out), OPTIONAL :: ierr
    !! Upon successful completion, SYSTEM_CLOSEDIR(3f) returns 0;
    !! otherwise, an error has occurred.
  END SUBROUTINE System_Closedir
END INTERFACE

!----------------------------------------------------------------------------
!                                           System_Putenv@EnvironmentMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Set environment variable from Fortran
!
!# System_Putenv
!
! The system_putenv() function adds or changes the value
!  of environment variables.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Putenv_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Putenv(string, err)
    CHARACTER(len=*), INTENT(in) :: string
    !! string of format "NAME=value".
    !! If name does not already exist in the environment,
    !! then string is added to the environment.
    !! If name does exist, then the value of name in the environment is
    !! changed to value.
    !! The string passed to putenv(3c) becomes part of the environment,
    !! so this routine creates a string each time it is called that
    !! increases the amount of
    !! memory the program uses.
    INTEGER, OPTIONAL, INTENT(out) :: err
    !! The system_putenv() function returns zero on success,
    !! or nonzero if an error occurs.
    !! A non-zero error usually indicates sufficient memory
    !! does not exist to store the
    !! variable.
  END SUBROUTINE System_Putenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                     arr2str@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-07
! summary: convert fortran array to a string

INTERFACE
  MODULE PURE FUNCTION arr2str(array) RESULT(string)
    CHARACTER(len=1), INTENT(IN) :: array(:)
    CHARACTER(len=SIZE(array)) :: string
  END FUNCTION arr2str
END INTERFACE

!----------------------------------------------------------------------------
!                                           System_Getenv@EnvironmentMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: get environment variable
!
!# System_Getenv
!
! The system_getenv() function gets the value of an environment variable.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Getenv_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Getenv(name, default) RESULT(VALUE)
    CHARACTER(*), INTENT(IN) :: name
    !! Return the value of the specified environment variable or
    !! blank if the variable is not defined.
    CHARACTER(*), INTENT(IN), OPTIONAL :: default
    !! If the value returned would be blank this value will be used
    !! instead.
    CHARACTER(:), ALLOCATABLE :: VALUE
  END FUNCTION System_Getenv
END INTERFACE

!----------------------------------------------------------------------------
!                                Set_Environment_Variable@EnvironmentMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: call setenv(3c) to set environment variable
!
!# Set_Environment
!
! The set_environment_variable() procedure adds or changes the value of
! environment variables.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/Set_Environment_Variable_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE Set_Environment_Variable(NAME, VALUE, STATUS)
    CHARACTER(*), INTENT(IN) :: NAME
    !! If name does not already exist in the environment,
    !! then string is added to the environment.
    !! If name does exist, then the value of name in the environment
    !! is changed to value.
    CHARACTER(*), INTENT(IN) :: VALUE
    !! Value to assign to environment variable NAME
    INTEGER, OPTIONAL, INTENT(OUT) :: STATUS
    !! returns zero on success, or nonzero if an error occurs.
    !! A non-zero error usually indicates sufficient memory does
    !! not exist to store the
    !! variable.
  END SUBROUTINE Set_Environment_Variable
END INTERFACE

!----------------------------------------------------------------------------
!                                         System_Clearenv@EnvironmentMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Clear environment by calling clearenv(3c)
!
!# System_Clearenv
!
! The System_Clearenv() procedure clears the environment of all name-value
! pairs. Typically used in security-conscious applications or ones where
! configuration control requires ensuring specific variables are set.
!
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Clearenv_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE System_Clearenv(ierr)
    INTEGER, INTENT(OUT), OPTIONAL :: ierr
    !! returns zero on success, and a nonzero value on failure. Optional.
    !! If not present and an error occurs the program stops.
  END SUBROUTINE System_Clearenv
END INTERFACE

!----------------------------------------------------------------------------
!                                         System_Unsetenv@EnvironmentMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: delete an environment variable by calling unsetenv(3c)
!
!# System_Unsetenv
!
! The System_Unsetenv(3f) function deletes the variable name from the
! environment.

INTERFACE
  MODULE SUBROUTINE System_Unsetenv(name, ierr)
    CHARACTER(len=*), INTENT(in) :: name
    !! name of variable to delete.
    !! If name does not exist in the environment, then the
    !! function succeeds, and the environment is unchanged.
    INTEGER, INTENT(out), OPTIONAL :: ierr
    !! The system_unsetenv(3f) function returns zero on success,
    !! or -1 on error.
    !! name is NULL, points to a string of length 0, or
    !! contains an '=' character.
    !! Insufficient memory to add a new variable to the environment.
  END SUBROUTINE System_Unsetenv
END INTERFACE

!----------------------------------------------------------------------------
!                                          System_Readenv@EnvironmentMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: step thru and read environment table
!
! ## System_Readenv
!
! This routine provides a simple interface for reading the environment
! variable table of the current process.
!
! The recommended usage pattern is as follows:
!
! - Call `system_initenv(3f)` to initialize access to the environment
!   table.
!
! - Repeatedly call `system_readenv(3f)` to read entries from the
!   environment table.
!
! - Reading terminates when a blank line is returned.
!
! ### Notes
!
! - If more than one thread reads the environment simultaneously, the
!   results are undefined.
!
! - If the environment is modified while it is being read, the results
!   are also undefined.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Readenv_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Readenv() RESULT(string)
    CHARACTER(len=:), ALLOCATABLE :: string
    !! the string returned from the environment of the form "NAME=VALUE"
  END FUNCTION System_Readenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Fileglob@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: Read output of an ls(1) command from Fortran
!
!# Fileglob
!
! Non-portable procedure uses the shell and the ls(1) command
! to expand a filename
! and returns a pointer to a list of expanded filenames.
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/Fileglob_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE Fileglob(glob, list)
    CHARACTER(*), INTENT(IN) :: glob
    !! Pattern for the filenames (like: *.txt)
    CHARACTER(*), POINTER, INTENT(INOUT) :: list(:)
    !! Allocated list of filenames (returned), the caller must deallocate it.
  END SUBROUTINE Fileglob
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
! ## System_Getlogin
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
!                                                               System_Stat
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
!                                                                  System_Dir
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Return filenames in a directory matching specific wildcard strings
!
!# System_Dir
!
! returns an array of filenames in the specified directory matching
! the wildcard string (which defaults to "*").
!
!## Examples
!
!```fortran
! {{% fortran-code file="examples/System_Dir_test_1.F90" %}}
!```

INTERFACE
  MODULE FUNCTION System_Dir(directory, pattern)
    CHARACTER(*), INTENT(IN), OPTIONAL :: directory
    !! name of directory to match filenames in. Defaults to ".".
    CHARACTER(*), INTENT(IN), OPTIONAL :: pattern
    !! wildcard string matching the rules of the matchw(3f) function.
    !! Basically "*" matches anything, "?" matches any single character
    CHARACTER(:), ALLOCATABLE :: System_Dir(:)
    !!System_Dir   An array right-padded to the length of the longest
    !!filename. Note that this means filenames actually containing
    !!trailing spaces in their names may be incorrect.
  END FUNCTION System_Dir
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
    INTEGER(kind=INT64) :: ii38
    !! The value of VALUIN converted to INTEGER(KIND=INT64).
  END FUNCTION Anyinteger_to_64bit
END INTERFACE

!----------------------------------------------------------------------------
!                                                    f_handler@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: handler

INTERFACE
  MODULE SUBROUTINE f_handler(signum) BIND(c)
    INTEGER(C_INT), INTENT(IN), VALUE :: signum
  END SUBROUTINE f_handler
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE System_Method
