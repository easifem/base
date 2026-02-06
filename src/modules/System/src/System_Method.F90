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
USE, INTRINSIC :: ISO_C_BINDING
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
!                                                              system_signal
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
!
!## Usage
!
!```fortran
! program demo_system_signal
! use M_system, only : system_signal
! implicit none
! logical :: loop=.true.
! integer, parameter :: SIGINT=2,SIGQUIT=3
! call system_signal(SIGINT,exitloop)
! call system_signal(SIGQUIT,quit)
! write(*,*)'Starting infinite loop. Press Ctrl+C to exit.'
! do while(loop)
! enddo
! write(*,*)'Reporting from outside the infinite loop.'
! write(*,*)'Starting another loop. Do Ctrl+\ anytime to quit.'
! loop=.true.
! call system_signal(2)
! write(*,*)'Just installed do-nothing handler for SIGINT. Try Ctrl+C to test.'
! do while(loop)
! enddo
! write(*,*)'You should never see this line when running this demo.'
!
! contains
!
! subroutine exitloop(signum)
!   integer :: signum
!   write(*,*)'Caught SIGINT. Exiting infinite loop.'
!   loop=.false.
! end subroutine exitloop
!
! subroutine quit(signum)
!   integer :: signum
!   STOP 'Caught SIGQUIT. Stopping demo.'
! end subroutine quit
! end program demo_system_signal
! ```

INTERFACE
  MODULE SUBROUTINE System_Signal(signum, handler_routine)
    INTEGER, INTENT(in) :: signum
    PROCEDURE(handler), OPTIONAL :: handler_routine
    TYPE(C_FUNPTR) :: ret, c_handler
  END SUBROUTINE System_Signal
END INTERFACE

!----------------------------------------------------------------------------
!                                                              System_Access
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Check accessibility or existence of a pathname
!
!# System_Access
!
!
!The system_access(3f) function checks pathname existence and access
!permissions. The function checks the pathname for accessibility
!according to the bit pattern contained in amode, using the real user
!ID in place of the effective user ID and the real group ID in place
!of the effective group ID.
!
!The value of amode is either the bitwise-inclusive OR of the access
!permissions to be checked (R_OK, W_OK, X_OK) or the existence test (F_OK).
!
!- pathname: a character string representing a directory pathname.
!             Trailing spaces are ignored.
!- amode: bitwise-inclusive OR of the values R_OK, W_OK, X_OK, or F_OK.
!- Return value: If not true an error occurred or
!                the requested access is not granted
!
!
!## Examples
!
! Check if filename is accessible
!
!```fortran
! program demo_system_access
! use M_system, only : system_access, F_OK, R_OK, W_OK, X_OK
! implicit none
! integer                     :: i
! character(len=80),parameter :: names(*)=[ &
! '/usr/bin/bash   ', &
! '/tmp/NOTTHERE   ', &
! '/usr/local      ', &
! '.               ', &
! 'PROBABLY_NOT    ']
! do i=1,size(names)
!    write(*,*)' does ',trim(names(i)),' exist?    ', &
!    system_access(names(i),F_OK)
!
!    write(*,*)' is ',trim(names(i)),' readable?     ', &
!    system_access(names(i),R_OK)
!
!    write(*,*)' is ',trim(names(i)),' writable?     ', &
!    system_access(names(i),W_OK)
!
!    write(*,*)' is ',trim(names(i)),' executable?   ', &
!    system_access(names(i),X_OK)
!
! enddo
! end program demo_system_access
!```

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Access(pathname, amode)
    CHARACTER(len=*), INTENT(IN) :: pathname
    INTEGER, INTENT(IN) :: amode
    LOGICAL :: System_Access
  END FUNCTION System_Access
END INTERFACE

!----------------------------------------------------------------------------
!                                                               System_Utime
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
!## Arguments
!
!### times
!
!If present, the values will be interpreted as the access
!and modification times as Unix Epoch values. That is,
!they are times measured in seconds since the Unix Epoch.
!
!### pathname
!
!name of the file whose access and modification times are to be updated.
!
!## Return values
!
!Upon successful completion .TRUE. is returned. Otherwise,
!.FALSE. is returned and errno shall be set to indicate the error,
!and the file times remain unaffected.
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
!
!## Usage
!
!```fortran
! program demo_system_utime
! use M_system, only : system_utime, system_perror
! implicit none
! character(len=4096) :: pathname
! integer             :: times(2)
! integer             :: i
!    do i=1,command_argument_count()
!       call get_command_argument(i, pathname)
!       if(.not.system_utime(pathname,times))then
!          call system_perror('*demo_system_utime*')
!       endif
!    enddo
! end program demo_system_utime
!```

INTERFACE
  MODULE FUNCTION system_utime(pathname, times)
    CHARACTER(len=*), INTENT(in) :: pathname
    INTEGER, INTENT(in), OPTIONAL :: times(2)
    LOGICAL :: system_utime
  END FUNCTION System_Utime
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!       system_realpath(3f) - [M_system:FILE_SYSTEM] call realpath(3c) to resolve a pathname
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!       function system_realpath(input) result(output)
!!
!!        character(len=*),intent(in)  :: input
!!        character(len=:),allocatable :: output
!!##DESCRIPTION
!!        system_realpath(3f) calls the C routine realpath(3c) to obtain the absolute pathname of given path
!!##OPTIONS
!!
!!        INPUT     pathname to resolve
!!
!!##RETURN VALUE
!!        OUTPUT    The absolute pathname of the given input pathname.
!!                  The pathname shall contain no components that are dot
!!                  or dot-dot, or are symbolic links. It is equal to the
!!                  NULL character if an error occurred.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_realpath
!!    use M_system, only : system_realpath, system_perror
!!    implicit none
!!    ! resolve each pathname given on command line
!!    character(len=:),allocatable :: pathi,patho
!!    integer                      :: i
!!    integer                      :: filename_length
!!       do i = 1, command_argument_count()
!!          ! get pathname from command line arguments
!!          call get_command_argument (i , length=filename_length)
!!          if(allocated(pathi))deallocate(pathi)
!!          allocate(character(len=filename_length) :: pathi)
!!          call get_command_argument (i , value=pathi)
!!          !
!!          ! resolve each pathname
!!          patho=system_realpath(pathi)
!!          if(patho.ne.char(0))then
!!             write(*,*)trim(pathi),'=>',trim(patho)
!!          else
!!             call system_perror('*system_realpath* error for pathname '//trim(pathi)//':')
!!             write(*,*)trim(pathi),'=>',trim(patho)
!!          endif
!!          deallocate(pathi)
!!       enddo
!!       ! if there were no pathnames given resolve the pathname "."
!!       if(i.eq.1)then
!!          patho=system_realpath('.')
!!          write(*,*)'.=>',trim(patho)
!!       endif
!!    end program demo_system_realpath
!!
!!  Example usage:
!!
!!   demo_system_realpath
!!   .=>/home/urbanjs/V600
!!
!!   cd /usr/share/man
!!   demo_system_realpath . .. NotThere
!!   .=>/usr/share/man
!!   ..=>/usr/share
!!   *system_realpath* error for pathname NotThere:: No such file or directory
!!   NotThere=>NotThere

INTERFACE
  MODULE FUNCTION system_realpath(input) RESULT(string)
    CHARACTER(len=*), INTENT(in) :: input
    CHARACTER(len=:), ALLOCATABLE :: string
  END FUNCTION system_realpath
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_issock(3f) - [M_system:QUERY_FILE] checks if argument is a socket
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   elemental impure logical function system_issock(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_issock
!!
!!##DESCRIPTION
!!        The issock(3f) function checks if path is a path to a socket
!!
!!##OPTIONS
!!        path   a character string representing a socket pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_issock() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a socket
!!
!!    program demo_system_issock
!!    use M_system, only : system_issock
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'sock.test       ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a socket? ', system_issock(names(i))
!!    enddo
!!    end program demo_system_issock

INTERFACE
  MODULE FUNCTION System_Issock(pathname)
    CHARACTER(len=*), INTENT(in) :: pathname
    LOGICAL :: System_Issock
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
    TYPE(C_PTR), INTENT(in) :: c_string_pointer
    CHARACTER(len=:), ALLOCATABLE :: f_string
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
    CHARACTER(len=*), INTENT(in) :: string
    CHARACTER(len=1, kind=C_CHAR) :: array(LEN(string) + 1)
  END FUNCTION str2_carr
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  TimeStamp
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
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_isfifo(3f) - [M_system:QUERY_FILE] checks if argument is a fifo - named pipe
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   elemental impure logical function system_isfifo(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_isfifo
!!
!!##DESCRIPTION
!!        The isfifo(3f) function checks if path is a path to a fifo - named pipe.
!!
!!##OPTIONS
!!        path   a character string representing a fifo - named pipe pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_isfifo() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a FIFO file
!!
!!    program demo_system_isfifo
!!    use M_system, only : system_isfifo
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'fifo.test       ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a fifo(named pipe)? ', system_isfifo(names(i))
!!    enddo
!!    end program demo_system_isfifo

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Isfifo(pathname)
    CHARACTER(len=*), INTENT(in) :: pathname
    LOGICAL :: System_Isfifo
  END FUNCTION System_Isfifo
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_ischr(3f) - [M_system:QUERY_FILE] checks if argument is a character device
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   elemental impure logical function system_ischr(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_ischr
!!
!!##DESCRIPTION
!!        The ischr(3f) function checks if path is a path to a character device.
!!
!!##OPTIONS
!!        path   a character string representing a character device pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_ischr() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a character file
!!
!!    program demo_system_ischr
!!    use M_system, only : system_ischr
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'char_dev.test   ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a character device? ', system_ischr(names(i))
!!    enddo
!!    end program demo_system_ischr
!!
!!   Results:

INTERFACE
  MODULE ELEMENTAL impure FUNCTION System_Ischr(pathname)
    CHARACTER(len=*), INTENT(in) :: pathname
    LOGICAL :: System_Ischr
  END FUNCTION System_Ischr
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_isreg(3f) - [M_system:QUERY_FILE] checks if argument is a regular file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   elemental impure logical function system_isreg(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_isreg
!!
!!##DESCRIPTION
!!        The isreg(3f) function checks if path is a regular file
!!
!!##OPTIONS
!!        path   a character string representing a pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_isreg() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_islnk(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a regular file
!!
!!    program simple
!!    use M_system, only : system_isreg
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    'test.txt        ', &
!!    '~/.bashrc       ', &
!!    '.bashrc         ', &
!!    '.               ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a regular file? ', system_isreg(names(i))
!!    enddo
!!    end program simple
!!
!!   EXTENDED EXAMPLE
!!   list readable non-hidden regular files and links in current directory
!!
!!    program demo_system_isreg
!!    use M_system, only : isreg=>system_isreg, islnk=>system_islnk
!!    use M_system, only : access=>system_access, R_OK
!!    use M_system, only : system_dir
!!    implicit none
!!    character(len=1024),allocatable :: filenames(:) ! BUG: cannot use len=: in gfortran 8.3.1
!!    logical,allocatable :: mymask(:)
!!    integer                         :: i
!!         ! list readable non-hidden regular files and links in current directory
!!         filenames=system_dir(pattern='*')                ! make list of all files in current directory
!!         mymask= isreg(filenames).or.islnk(filenames)   ! select regular files and links
!!         where(mymask) mymask=filenames(:)(1:1).ne.'.'  ! skip hidden directories in those
!!         where(mymask) mymask=access(filenames,R_OK)    ! select readable files in those
!!         filenames=pack(filenames,mask=mymask)
!!         write(*,'(a)')(trim(filenames(i)),i=1,size(filenames))
!!    end program demo_system_isreg

INTERFACE
  MODULE ELEMENTAL impure FUNCTION system_isreg(pathname)
    CHARACTER(len=*), INTENT(in) :: pathname
    LOGICAL :: system_isreg
  END FUNCTION system_isreg
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_islnk(3f) - [M_system:QUERY_FILE] checks if argument is a link
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental impure logical function system_islnk(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_islnk
!!
!!##DESCRIPTION
!!        The islnk(3f) function checks if path is a path to a link.
!!
!!##OPTIONS
!!    path          a character string representing a link
!!                  pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!    system_islnk  The system_islnk() function should always be
!!                  successful and no return value is reserved to
!!                  indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program demo_system_islnk
!!    use M_system, only : system_islnk
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'link.test       ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a link? ', system_islnk(names(i))
!!    enddo
!!    end program demo_system_islnk
!!
!!   Results:

INTERFACE
  MODULE ELEMENTAL impure FUNCTION System_Islnk(pathname)
    CHARACTER(len=*), INTENT(in) :: pathname
    LOGICAL :: System_Islnk
  END FUNCTION System_Islnk
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!! system_isblk(3f) - [M_system:QUERY_FILE] checks if argument is a block device
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   elemental impure logical function system_isblk(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_isblk
!!
!!##DESCRIPTION
!! The isblk(3f) function checks if path is a path to a block device.
!!
!!##OPTIONS
!! path   a character string representing a block device pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_isblk() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_isreg(3f), system_stat(3f), system_isdir(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!   check if filename is a block device
!!
!!    program demo_system_isblk
!!    use M_system, only : system_isblk
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'block_device.tst', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!        write(*,*)' is ',trim(names(i)),' a block device? ', system_isblk(names(i))
!!    enddo
!!    end program demo_system_isblk
!!
!!   Results:

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Isblk(pathname)
    CHARACTER(len=*), INTENT(IN) :: pathname
    LOGICAL :: System_Isblk
  END FUNCTION System_Isblk
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_isdir(3f) - [M_system:QUERY_FILE] checks if argument is a directory path
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   elemental impure logical function system_isdir(pathname)
!!
!!    character(len=*),intent(in) :: pathname
!!    logical                     :: system_isdir
!!
!!##DESCRIPTION
!!        The system_isdir(3f) function checks if path is a directory.
!!
!!##OPTIONS
!!        path   a character string representing a directory pathname. Trailing spaces are ignored.
!!
!!##RETURN VALUE
!!        The system_isdir() function should always be successful and no
!!        return value is reserved to indicate an error.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##SEE ALSO
!!    system_islnk(3f), system_stat(3f), isreg(3f), system_perm(3f)
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!    program demo_system_isdir
!!    use M_system, only : system_isdir
!!    use M_system, only : access=>system_access, R_OK
!!    use M_system, only : system_dir
!!    implicit none
!!    character(len=1024),allocatable :: filenames(:) ! BUG: cannot use len=: in gfortran 8.3.1
!!    integer                         :: i
!!    character(len=80),parameter     :: names(*)=[ &
!!    & '/tmp            ', &
!!    & '/tmp/NOTTHERE   ', &
!!    & '/usr/local      ', &
!!    & '.               ', &
!!    & 'PROBABLY_NOT    ']
!!       !
!!       do i=1,size(names)
!!          write(*,*)' is ',trim(names(i)),' a directory? ', system_isdir(names(i))
!!       enddo
!!       !
!!       ! EXTENDED EXAMPLE: list readable non-hidden directories in current directory
!!       filenames=system_dir(pattern='*') ! list all files in current directory
!!       ! select readable directories
!!       filenames=pack(filenames,system_isdir(filenames).and.access(filenames,R_OK))
!!       filenames=pack(filenames,filenames(:)(1:1) .ne.'.') ! skip hidden directories
!!       do i=1,size(filenames)
!!          write(*,*)' ',trim(filenames(i)),' is a directory'
!!       enddo
!!       !
!!    end program demo_system_isdir
!!
!!
!!   Results:
!!
!!      is /tmp a directory?  T
!!      is /tmp/NOTTHERE a directory?  F
!!      is /usr/local a directory?  T
!!      is . a directory?  T
!!      is PROBABLY_NOT a directory?  F
!!
!!      TEST is a directory
!!      EXAMPLE is a directory

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Isdir(dirname)
    CHARACTER(len=*), INTENT(in) :: dirname
    LOGICAL :: System_Isdir
  END FUNCTION System_Isdir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_chown(3f) - [M_system:FILE_SYSTEM] change file owner and group
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   elemental impure logical function system_chown(path,owner,group)
!!
!!    character(len=*),intent(in) :: path
!!    integer,intent(in)          :: owner
!!    integer,intent(in)          :: group
!!
!!##DESCRIPTION
!!        The chown(3f) function changes owner and group of a file
!!
!!       The path argument points to a pathname naming a file. The
!!       user ID and group ID of the named file shall be set to the numeric
!!       values contained in owner and group, respectively.
!!
!!       Only processes with an effective user ID equal to the user ID of
!!       the file or with appropriate privileges may change the ownership
!!       of a file.
!!
!!##OPTIONS
!!       path   a character string representing a file pathname.
!!              Trailing spaces are ignored.
!!       owner  UID of owner that ownership is to be changed to
!!       group  GID of group that ownership is to be changed to
!!
!!##RETURN VALUE
!!       The system_chown(3f) function should return zero 0 if successful.
!!       Otherwise, these functions shall return 1 and set errno to
!!       indicate the error. If 1 is returned, no changes are made in
!!       the user ID and group ID of the file.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program demo_system_chown
!!    use M_system, only : system_chown
!!    use M_system, only : system_getuid
!!    use M_system, only : system_getgid
!!    use M_system, only : system_perror
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[character(len=80) :: 'myfile1','/usr/local']
!!    do i=1,size(names)
!!       if(.not. system_chown(&
!!       & trim(names(i)),  &
!!       & system_getuid(), &
!!       & system_getgid()) &
!!          )then
!!          call system_perror('*demo_system_chown* '//trim(names(i)))
!!       endif
!!    enddo
!!    end program demo_system_chown

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION System_Chown(dirname, owner, group)
    CHARACTER(len=*), INTENT(in) :: dirname
    INTEGER, INTENT(in) :: owner
    INTEGER, INTENT(in) :: group
    LOGICAL :: System_Chown
  END FUNCTION System_Chown
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!        system_cpu_time(3f) - [M_system] get processor time by calling times(3c)
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine system_cpu_time(c_user, c_system, c_total)
!!
!!         real,intent(out) :: c_total
!!         real,intent(out) :: c_user
!!         real,intent(out) :: c_system
!!
!!##DESCRIPTION
!!
!!##OUTPUT
!!         c_total   total processor time ( c_user + c_system )
!!         c_user    processor user time
!!         c_system  processor system time
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##EXAMPLES
!!
!!
!!   Sample program:
!!
!!    program demo_system_cpu_time
!!
!!    use M_system, only : system_cpu_time
!!    use ISO_C_BINDING, only : c_float
!!    implicit none
!!    real    :: user_start, system_start, total_start
!!    real    :: user_finish, system_finish, total_finish
!!    integer :: i
!!    integer :: itimes=1000000
!!    real    :: value
!!
!!       call system_cpu_time(total_start,user_start,system_start)
!!
!!       value=0.0
!!       do i=1,itimes
!!          value=sqrt(real(i)+value)
!!       enddo
!!       write(10,*)value
!!       flush(10)
!!       write(*,*)'average sqrt value=',value/itimes
!!       call system_cpu_time(total_finish,user_finish,system_finish)
!!       write(*,*)'USER ......',user_finish-user_start
!!       write(*,*)'SYSTEM ....',system_finish-system_start
!!       write(*,*)'TOTAL .....',total_finish-total_start
!!
!!    end program demo_system_cpu_time
!!
!!   Typical Results:
!-! GET ERRORS ABOUT MISSING LONGEST_ENV_VARIABLE IN GFORTRAN 6.4.0 IF JUST USE INTERFACE INSTEAD OF MAKING SUBROUTINE
!-!interface
!-!   subroutine system_cpu_time(c_total,c_user,c_system) bind (C,NAME='my_cpu_time')
!-!      import c_float
!-!      real(kind=c_float) :: c_user,c_system,c_total
!-!   end subroutine system_cpu_time
!-!end interface

INTERFACE
  MODULE SUBROUTINE system_cpu_time(total, user, system)
    REAL, INTENT(OUT) :: user, system, total
  END SUBROUTINE system_cpu_time
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!        system_link(3f) - [M_system:FILE_SYSTEM] link one file to another
!!                          file relative to two directory file descriptors
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental impure integer function link(oldpath,newpath);
!!
!!     character(len=*),intent(in) :: oldpath
!!     character(len=*),intent(in) :: newpath
!!
!!##DESCRIPTION
!!        The link() function shall create a new link (directory entry)
!!        for the existing file, path1.
!!
!!        The path1 argument points to a pathname naming an existing
!!        file. The path2 argument points to a pathname naming the
!!        new directory entry to be created. The link() function shall
!!        atomically create a new link for the existing file and the link
!!        count of the file shall be incremented by one.
!!
!!        If path1 names a directory, link() shall fail unless the process
!!        has appropriate privileges and the implementation supports using
!!        link() on directories.
!!
!!        If path1 names a symbolic link, it is implementation-defined
!!        whether link() follows the symbolic link, or creates a new link
!!        to the symbolic link itself.
!!
!!        Upon successful completion, link() shall mark for update the
!!        last file status change timestamp of the file. Also, the last
!!        data modification and last file status change timestamps of the
!!        directory that contains the new entry shall be marked for update.
!!
!!        If link() fails, no link shall be created and the link count of
!!        the file shall remain unchanged.
!!
!!        The implementation may require that the calling process has
!!        permission to access the existing file.
!!
!!        The linkat() function shall be equivalent to the link() function
!!        except that symbolic links shall be handled as specified by the
!!        value of flag (see below) and except in the case where either path1
!!        or path2 or both are relative paths. In this case a relative path
!!        path1 is interpreted relative to the directory associated with
!!        the file descriptor fd1 instead of the current working directory
!!        and similarly for path2 and the file descriptor fd2. If the
!!        file descriptor was opened without O_SEARCH, the function shall
!!        check whether directory searches are permitted using the current
!!        permissions of the directory underlying the file descriptor. If
!!        the file descriptor was opened with O_SEARCH, the function shall
!!        not perform the check.
!!
!!        Values for flag are constructed by a bitwise-inclusive OR of
!!        flags from the following list, defined in <fcntl.h>:
!!
!!        AT_SYMLINK_FOLLOW
!!              If path1 names a symbolic link, a new link for the target
!!              of the symbolic link is created.
!!
!!        If linkat() is passed the special value AT_FDCWD in the fd1 or
!!        fd2 parameter, the current working directory shall be used for the
!!        respective path argument. If both fd1 and fd2 have value AT_FDCWD,
!!        the behavior shall be identical to a call to link(), except that
!!        symbolic links shall be handled as specified by the value of flag.
!!
!!        Some implementations do allow links between file systems.
!!
!!        If path1 refers to a symbolic link, application developers should
!!        use linkat() with appropriate flags to select whether or not the
!!        symbolic link should be resolved.
!!
!!        If the AT_SYMLINK_FOLLOW flag is clear in the flag argument and
!!        the path1 argument names a symbolic link, a new link is created
!!        for the symbolic link path1 and not its target.
!!
!!##RETURN VALUE
!!        Upon successful completion, these functions shall return
!!        0. Otherwise, these functions shall return -1 and set errno to
!!        indicate the error.
!!
!!##EXAMPLES
!!
!!   Creating a Link to a File
!!
!!    program demo_system_link
!!    use M_system, only : system_link, system_perror
!!    integer :: ierr
!!    ierr = system_link('myfile1','myfile2')
!!    if(ierr.ne.0)then
!!       call system_perror('*demo_system_link*')
!!    endif
!!    end program demo_system_link

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION system_link(oldname, newname) RESULT(ierr)
    CHARACTER(len=*), INTENT(in) :: oldname
    CHARACTER(len=*), INTENT(in) :: newname
    INTEGER :: ierr
  END FUNCTION system_link
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!        system_unlink(3f) - [M_system:FILE_SYSTEM] remove a directory
!!        entry relative to directory file descriptor
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental impure integer function unlink(path);
!!
!!     character(len=*) :: path
!!
!!##DESCRIPTION
!!    The unlink() function shall remove a link to a file. If path names a
!!    symbolic link, unlink() shall remove the symbolic link named by path
!!    and shall not affect any file or directory named by the contents of
!!    the symbolic link. Otherwise, unlink() shall remove the link named by
!!    the pathname pointed to by path and shall decrement the link count of
!!    the file referenced by the link.
!!
!!    When the files link count becomes 0 and no process has the file open,
!!    the space occupied by the file shall be freed and the file shall no
!!    longer be accessible. If one or more processes have the file open when
!!    the last link is removed, the link shall be removed before unlink()
!!    returns, but the removal of the file contents shall be postponed until
!!    all references to the file are closed.
!!
!!    The path argument shall not name a directory unless the process has
!!    appropriate privileges and the implementation supports using unlink()
!!    on directories.
!!
!!    Upon successful completion, unlink() shall mark for update the last
!!    data modification and last file status change timestamps of the parent
!!    directory. Also, if the file link count is not 0, the last file status
!!    change timestamp of the file shall be marked for update.
!!
!!    Values for flag are constructed by a bitwise-inclusive OR of flags from
!!    the following list, defined in <fcntl.h>:
!!
!!       AT_REMOVEDIR
!!
!!     Remove the directory entry specified by fd and path as a
!!     directory, not a normal file.
!!
!!##RETURN VALUE
!!
!!    Upon successful completion, these functions shall return 0. Otherwise,
!!    these functions shall return -1 and set errno to indicate the error. If
!!    -1 is returned, the named file shall not be changed.
!!
!!##EXAMPLES
!!
!!   Removing a link to a file
!!
!!    program demo_system_unlink
!!    use M_system, only : system_unlink, system_perror
!!    integer :: ierr
!!    ierr = system_unlink('myfile1')
!!    if(ierr.ne.0)then
!!       call system_perror('*demo_system_unlink*')
!!    endif
!!    end program demo_system_unlink

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION system_unlink(fname) RESULT(ierr)
    CHARACTER(len=*), INTENT(in) :: fname
    INTEGER :: ierr
  END FUNCTION system_unlink
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!    system_setumask(3f) - [M_system:FILE_SYSTEM] set the file mode creation umask
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer function system_setumask(new_umask) result (old_umask)
!!
!!     integer,intent(in)  :: new_umask
!!     integer(kind=c_int) :: umask_c
!!
!!##DESCRIPTION
!!        The system_umask(3f) function sets the file mode creation mask of the
!!        process to cmask and return the previous value of the mask. Only
!!        the file permission bits of cmask (see <sys/stat.h>) are used;
!!        the meaning of the other bits is implementation-defined.
!!
!!        The file mode creation mask of the process is used to turn off
!!        permission bits in the mode argument supplied during calls to
!!        the following functions:
!!
!!         *  open(), openat(), creat(), mkdir(), mkdirat(), mkfifo(), and mkfifoat()
!!         *  mknod(), mknodat()
!!         *  mq_open()
!!         *  sem_open()
!!
!!        Bit positions that are set in cmask are cleared in the mode of
!!        the created file.
!!
!!##RETURN VALUE
!!        The file permission bits in the value returned by umask() shall be
!!        the previous value of the file mode creation mask. The state of any
!!        other bits in that value is unspecified, except that a subsequent
!!        call to umask() with the returned value as cmask shall leave the
!!        state of the mask the same as its state before the first call,
!!        including any unspecified use of those bits.
!!
!!##ERRORS
!!        No errors are defined.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_setumask
!!    use M_system, only : system_getumask, system_setumask
!!    integer :: newmask
!!    integer :: i
!!    integer :: old_umask
!!    write(*,101)(system_getumask(),i=1,4)
!!    101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
!!    newmask=63
!!    old_umask=system_setumask(newmask)
!!    write(*,*)'NEW'
!!    write(*,101)(system_getumask(),i=1,4)
!!    end program demo_setumask
!!
!!   Expected output
!!
!!     18 O'022' Z"12' B'000010010"
!!     NEW
!!     63 O'077' Z"3F' B'000111111"

INTERFACE
  MODULE FUNCTION system_setumask(umask_value) RESULT(old_umask)
    INTEGER, INTENT(in) :: umask_value
    INTEGER :: old_umask
  END FUNCTION system_setumask
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!    system_getumask(3f) - [M_system:QUERY_FILE] get current umask
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   integer function system_getumask() result (umask_value)
!!##DESCRIPTION
!!   The return value from getumask(3f) is the value of the file
!!   creation mask, obtained by using umask(3c).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_getumask
!!    use M_system, only : system_getumask, system_setumask
!!    integer :: i
!!    write(*,101)(system_getumask(),i=1,4)
!!    101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
!!    end program demo_getumask
!!
!!   Expected output
!!
!!     18 O'022' Z"12' B'000010010"
INTERFACE
  MODULE FUNCTION system_getumask() RESULT(umask_value)
! The return value from umask() is just the previous value of the file
! creation mask, so that this system call can be used both to get and
! set the required values. Sadly, however, there is no way to get the old
! umask value without setting a new value at the same time.

! This means that in order just to see the current value, it is necessary
! to execute a piece of code like the following function:
    INTEGER :: umask_value
  END FUNCTION system_getumask
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!      perror(3f) - [M_system:ERROR_PROCESSING] print error message for last C error on stderr
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      subroutine system_perror(prefix)
!!
!!       character(len=*),intent(in) :: prefix
!!
!!##DESCRIPTION
!!    Use system_perror(3f) to print an error message on stderr
!!    corresponding to the current value of the C global variable errno.
!!    Unless you use NULL as the argument prefix, the error message will
!!    begin with the prefix string, followed by a colon and a space
!!    (:). The remainder of the error message produced is one of the
!!    strings described for strerror(3c).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_perror
!!    use M_system, only : system_perror,system_rmdir
!!    implicit none
!!    character(len=:),allocatable :: DIRNAME
!!    DIRNAME='/NOT/THERE/OR/ANYWHERE'
!!    ! generate an error with a routine that supports errno and perror(3c)
!!    if(system_rmdir(DIRNAME).ne.0)then
!!       call system_perror('*demo_system_perror*:'//DIRNAME)
!!    endif
!!    write(*,'(a)')"That is all Folks!"
!!    end program demo_system_perror
!!
!!   Expected results:
!!
!!    *demo_system_perror*:/NOT/THERE/OR/ANYWHERE: No such file or directory
!!    That is all Folks!

INTERFACE
  MODULE SUBROUTINE system_perror(prefix)
    CHARACTER(len=*), INTENT(in) :: prefix
  END SUBROUTINE system_perror
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_chdir(3f) - [M_system_FILE_SYSTEM] call chdir(3c) from Fortran to change working directory
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine system_chdir(path, err)
!!
!!     character(len=*)               :: path
!!     integer, optional, intent(out) :: err
!!
!!##DESCRIPTION
!!
!!    system_chdir(3f) changes the current working directory of the calling
!!    process to the directory specified in path. The current working
!!    directory is the starting point for interpreting relative pathnames
!!    (those not starting with '/').
!!
!!##RETURN VALUE
!!
!!    On success, zero is returned. On error, -1 is returned, and errno is
!!    set appropriately.
!!
!!
!!    Depending on the file system, other errors can be returned. The more
!!    general errors for chdir() are listed below, by their C definitions:
!!
!!    Errors
!!    EACCES        Search permission is denied for one of the components of path.
!!                  (See also path_resolution(7).)
!!    EFAULT        path points outside your accessible address space.
!!    EIO           An I/O error occurred.
!!    ELOOP         Too many symbolic links were encountered in resolving path.
!!    ENAMETOOLONG  path is too long.
!!    ENOENT        The file does not exist.
!!    ENOMEM        Insufficient kernel memory was available.
!!    ENOTDIR       A component of path is not a directory.
!!
!!##SEE ALSO
!!
!!    chroot(2), getcwd(3), path_resolution(7)
!!
!!##EXAMPLE
!!
!!    Change working directory from Fortran
!!
!!      program demo_system_chdir
!!      use M_system, only : system_chdir
!!      implicit none
!!      integer :: ierr
!!
!!      call execute_command_line('pwd')
!!      call system_chdir('/tmp',ierr)
!!      call execute_command_line('pwd')
!!      write(*,*)'*CHDIR TEST* IERR=',ierr
!!
!!      end program demo_system_chdir
!!
!!##RESULTS:
!!   Sample run output:
!!
!!      /home/urbanjs/V600
!!      /tmp
!!      *CHDIR TEST* IERR=           0

INTERFACE
  MODULE SUBROUTINE system_chdir(path, err)
    CHARACTER(len=*) :: path
    INTEGER, OPTIONAL, INTENT(out) :: err
  END SUBROUTINE system_chdir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!      system_remove(3f) - [M_system_FILE_SYSTEM] call remove(3c) to remove file
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!   elemental impure function system_remove(path) result(err)
!!
!!    character(*),intent(in) :: path
!!    integer(c_int)          :: err
!!
!!##DESCRIPTION
!!    Fortran supports scratch files via the OPEN(3c) command; but does
!!    not otherwise allow for removing files. The system_remove(3f) command
!!    allows for removing files by name that the user has the authority to
!!    remove by calling the C remove(3c) function.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_remove
!!    use M_system, only : system_remove
!!    character(len=*),parameter :: FILE='MyJunkFile.txt'
!!    integer :: ierr
!!    write(*,*)'BEFORE CREATED '//FILE
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    ! note intentionally causes error if file exists
!!    open(unit=10,file=FILE,status='NEW')
!!    write(*,*)'AFTER OPENED '//FILE
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    write(10,'(a)') 'This is a file I want to delete'
!!    close(unit=10)
!!    write(*,*)'AFTER CLOSED '
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    ierr=system_remove(FILE)
!!    write(*,*)'AFTER REMOVED',IERR
!!    call execute_command_line('ls -l '//FILE)
!!    write(*,*)
!!
!!    end program demo_system_remove
!!
!!   Expected Results:
!!
!!    >  BEFORE CREATED MyJunkFile.txt
!!    > ls: cannot access 'MyJunkFile.txt': No such file or directory
!!    >
!!    >  AFTER OPENED MyJunkFile.txt
!!    > -rw-r--r-- 1 JSU None 0 Nov 19 19:32 MyJunkFile.txt
!!    >
!!    >  AFTER CLOSED
!!    > -rw-r--r-- 1 JSU None 32 Nov 19 19:32 MyJunkFile.txt
!!    >
!!    >  AFTER REMOVED           0
!!    > ls: cannot access 'MyJunkFile.txt': No such file or directory
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE ELEMENTAL IMPURE FUNCTION system_remove(path) RESULT(err)
    CHARACTER(*), INTENT(in) :: path
    INTEGER(C_INT) :: err
  END FUNCTION system_remove
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!      system_rename(3f) - [M_system_FILE_SYSTEM] call rename(3c) to rename a system file
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function system_rename(input,output) result(ierr)
!!
!!    character(*),intent(in)    :: input,output
!!    integer                    :: ierr
!!##DESCRIPTION
!!     Rename a file by calling rename(3c). It is not recommended that the
!!     rename occur while either filename is being used on a file currently
!!     OPEN(3f) by the program.
!!
!!     Both the old and new names must be on the same device.
!!##OPTIONS
!!     INPUT   system filename of an existing file to rename
!!     OUTPUT  system filename to be created or overwritten by INPUT file.
!!             Must be on the same device as the INPUT file.
!!##RETURNS
!!     IERR    zero (0) if no error occurs. If not zero a call to
!!             system_errno(3f) or system_perror(3f) is supported
!!             to diagnose error
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_system_rename
!!      use M_system, only : system_rename
!!      use M_system, only : system_remove
!!      use M_system, only : system_perror
!!      implicit none
!!      character(len=256) :: string
!!      integer            :: ios, ierr
!!
!!      ! try to remove junk files just in case
!!      ierr=system_remove('_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      call system_perror('*demo_system_rename*')
!!      ierr=system_remove('_renamed_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      call system_perror('*demo_system_rename*')
!!
!!      ! create scratch file to rename
!!      open(unit=10,file='_scratch_file_',status='new')
!!      write(10,'(a)') 'Test by renaming "_scratch_file_" to "_renamed_scratch_file_"'
!!      write(10,'(a)') 'IF YOU SEE THIS ON OUTPUT THE RENAME WORKED'
!!      close(10)
!!      ! rename scratch file
!!      ierr=system_rename('_scratch_file_','_renamed_scratch_file_')
!!      if(ierr.ne.0)then
!!         write(*,*)'ERROR RENAMING FILE ',ierr
!!      endif
!!      ! read renamed file
!!      open(unit=11,file='_renamed_scratch_file_',status='old')
!!      INFINITE: do
!!         read(11,'(a)',iostat=ios)string
!!         if(ios.ne.0)exit INFINITE
!!         write(*,'(a)')trim(string)
!!      enddo INFINITE
!!      close(unit=11)
!!
!!      ! clean up
!!      ierr=system_remove('_scratch_file_')
!!      write(*,'(a,i0)') 'should not be zero ',ierr
!!      ierr=system_remove('_renamed_scratch_file_')
!!      write(*,'(a,i0)') 'should be zero ',ierr
!!
!!      end program demo_system_rename
!!
!!   Expected output:
!!
!!    > should not be zero -1
!!    > *demo_system_rename*: No such file or directory
!!    > should not be zero -1
!!    > *demo_system_rename*: No such file or directory
!!    > Test by renaming "_scratch_file_" to "_renamed_scratch_file_"
!!    > IF YOU SEE THIS ON OUTPUT THE RENAME WORKED
!!    > should not be zero -1
!!    > should be zero 0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE FUNCTION system_rename(input, output) RESULT(ierr)
    CHARACTER(*), INTENT(in) :: input, output
    INTEGER :: ierr
  END FUNCTION system_rename
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!       system_chmod(3f) - [M_system_FILE_SYSTEM] call chmod(3c) to change
!!       permission mode of a file relative to directory file descriptor
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function system_chmod(filename,mode) result(ierr)
!!
!!       character(len=*),intent(in)  :: filename
!!       integer,value,intent(in)     :: mode
!!       integer                      :: ierr
!!
!!##DESCRIPTION
!!        The system_chmod(3f) function shall change UID, _ISGID, S_ISVTX, and the
!!        file permission bits of the file named by the pathname pointed
!!        to by the path argument to the corresponding bits in the mode
!!        argument. The application shall ensure that the effective user
!!        ID of the process matches the owner of the file or the process
!!        has appropriate privileges in order to do this.
!!
!!        S_ISUID, S_ISGID, S_ISVTX, and the file permission bits are
!!        described in <sys/stat.h>.
!!
!!        If the calling process does not have appropriate privileges,
!!        and if the group ID of the file does not match the effective
!!        group ID or one of the supplementary group IDs and if the file
!!        is a regular file, bit S_ISGID (set-group-ID on execution) in the
!!        file mode shall be cleared upon successful return from chmod().
!!
!!        Additional implementation-defined restrictions may cause the
!!        S_ISUID and S_ISGID bits in mode to be ignored.
!!
!!        Upon successful completion, system_chmod() marks for update the
!!        last file status change timestamp of the file.
!!
!!        Values for flag are constructed by a bitwise-inclusive OR of
!!        flags from the following list, defined in <fcntl.h>:
!!
!!        AT_SYMLINK_NOFOLLOW
!!              If path names a symbolic link, then the mode of the symbolic
!!              link is changed.
!!
!!
!!##RETURN VALUE
!!        Upon successful completion, system_chmod(3f) returns 0.
!!        Otherwise, it returns -1 and sets errno to indicate the error. If
!!        -1 is returned, no change to the file mode occurs.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_system_chmod
!!    use M_system, only : system_chmod
!!    use M_system, only : system_stat
!!    use M_system, only : R_GRP,R_OTH,R_USR, RWX_G, RWX_U, W_OTH, X_GRP
!!    !use M_system, only : RWX_O, W_GRP,W_USR,X_OTH,X_USR
!!    !use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    use,intrinsic     :: iso_fortran_env, only : int64
!!    implicit none
!!    integer         :: ierr
!!    integer         :: status
!!    integer(kind=int64) :: buffer(13)
!!       !Setting Read Permissions for User, Group, and Others
!!       ! The following example sets read permissions for the owner, group, and others.
!!       open(file='_test1',unit=10)
!!       write(10,*)'TEST FILE 1'
!!       close(unit=10)
!!       ierr=system_chmod('_test1', IANY([R_USR,R_GRP,R_OTH]))
!!
!!       !Setting Read, Write, and Execute Permissions for the Owner Only
!!       ! The following example sets read, write, and execute permissions for the owner, and no permissions for group and others.
!!       open(file='_test2',unit=10)
!!       write(10,*)'TEST FILE 2'
!!       close(unit=10)
!!       ierr=system_chmod('_test2', RWX_U)
!!
!!       !Setting Different Permissions for Owner, Group, and Other
!!       ! The following example sets owner permissions for CHANGEFILE to read, write, and execute, group permissions to read and
!!       ! execute, and other permissions to read.
!!       open(file='_test3',unit=10)
!!       write(10,*)'TEST FILE 3'
!!       close(unit=10)
!!       ierr=system_chmod('_test3', IANY([RWX_U,R_GRP,X_GRP,R_OTH]));
!!
!!       !Setting and Checking File Permissions
!!       ! The following example sets the file permission bits for a file named /home/cnd/mod1, then calls the stat() function to
!!       ! verify the permissions.
!!
!!       ierr=system_chmod("home/cnd/mod1", IANY([RWX_U,RWX_G,R_OTH,W_OTH]))
!!       call system_stat("home/cnd/mod1", buffer,status)
!!
!!       ! In order to ensure that the S_ISUID and S_ISGID bits are set, an application requiring this should use stat() after a
!!       ! successful chmod() to verify this.
!!
!!       !    Any files currently open could possibly become invalid if the mode
!!       !    of the file is changed to a value which would deny access to
!!       !    that process.
!!
!!    end program demo_system_chmod
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE FUNCTION system_chmod(filename, mode) RESULT(ierr)
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER, VALUE, INTENT(in) :: mode
    INTEGER :: ierr
  END FUNCTION system_chmod
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!       system_getcwd(3f) - [M_system:QUERY_FILE] call getcwd(3c) to get the pathname of the current working directory
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine system_getcwd(output,ierr)
!!
!!        character(len=:),allocatable,intent(out) :: output
!!        integer,intent(out)                      :: ierr
!!##DESCRIPTION
!!        system_getcwd(3f) calls the C routine getcwd(3c) to obtain the absolute pathname of the current working directory.
!!
!!##RETURN VALUE
!!        OUTPUT   The absolute pathname of the current working directory
!!                 The pathname shall contain no components that are dot or dot-dot,
!!                 or are symbolic links.
!!        IERR     is not zero if an error occurs.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_system_getcwd
!!      use M_system, only : system_getcwd
!!      implicit none
!!      character(len=:),allocatable :: dirname
!!      integer                      :: ierr
!!      call system_getcwd(dirname,ierr)
!!      if(ierr.eq.0)then
!!         write(*,*)'CURRENT DIRECTORY ',trim(dirname)
!!      else
!!         write(*,*)'ERROR OBTAINING CURRENT DIRECTORY NAME'
!!      endif
!!      end program demo_system_getcwd
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_getcwd(output, ierr)
    CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: output
    INTEGER, INTENT(out) :: ierr
  END SUBROUTINE system_getcwd
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!       system_rmdir(3f) - [M_system:FILE_SYSTEM] call rmdir(3c) to remove empty directories
!!       (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function system_rmdir(dirname) result(err)
!!
!!     character(*),intent(in) :: dirname
!!     integer(c_int) :: err
!!
!!##DESCRIPTION
!!        DIRECTORY  The name of a directory to remove if it is empty
!!        err        zero (0) if no error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_rmdir
!!    use M_system, only : system_perror
!!    use M_system, only : system_rmdir, system_mkdir
!!    use M_system, only : RWX_U
!!    implicit none
!!    integer :: ierr
!!    write(*,*)'BEFORE TRY TO CREATE _scratch/'
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO CREATE _scratch/'
!!    ierr=system_mkdir('_scratch',RWX_U)
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO REMOVE _scratch/'
!!    ierr=system_rmdir('_scratch')
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    write(*,*)'TRY TO REMOVE _scratch when it should be gone/'
!!    ierr=system_rmdir('_scratch')
!!    call system_perror('*test of system_rmdir*')
!!    write(*,*)'IERR=',ierr
!!    call execute_command_line('ls -ld _scratch')
!!
!!    end program demo_system_rmdir
!!
!!   Expected output:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE FUNCTION system_rmdir(dirname) RESULT(err)
    CHARACTER(*), INTENT(in) :: dirname
    INTEGER(C_INT) :: err
  END FUNCTION system_rmdir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!        system_mkfifo(3f)  - [M_system:FILE_SYSTEM] make a FIFO special file relative to directory file descriptor
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function system_mkfifo(pathname,mode) result(ierr)
!!
!!    character(len=*),intent(in)       :: pathname
!!    integer,intent(in)                :: mode
!!    integer :: ierr
!!
!!##DESCRIPTION
!!    A regular pipe can only connect two related processes. It is created by
!!    a process and will vanish when the last process closes it.
!!
!!    A named pipe, also called a FIFO for its behavior, can be used to connect
!!    two unrelated processes and exists independently of the processes;
!!    meaning it can exist even if no one is using it. A FIFO is created using
!!    the mkfifo() library function.
!!
!!    The mkfifo() function creates a new FIFO special file named by the
!!    pathname.
!!
!!    The file permission bits of the new FIFO are initialized from mode.
!!
!!    The file permission bits of the mode argument are modified by the
!!    process file creation mask.
!!
!!    When bits in mode other than the file permission bits are set, the
!!    effect is implementation-defined.
!!
!!    If path names a symbolic link, mkfifo() shall fail and set errno to
!!    [EEXIST].
!!
!!    The FIFOs user ID will be set to the process effective user ID.
!!
!!    The FIFOs group ID shall be set to the group ID of the parent
!!    directory or to the effective group ID of the process.
!!
!!    Implementations shall provide a way to initialize the FIFOs group
!!    ID to the group ID of the parent directory.
!!
!!    Implementations may, but need not, provide an implementation-defined
!!    way to initialize the FIFOs group ID to the effective group ID of
!!    the calling process.
!!
!!    Upon successful completion, mkfifo() shall mark for update the
!!    last data access, last data modification, and last file status change
!!    timestamps of the file.
!!
!!    Also, the last data modification and last file status change
!!    timestamps of the directory that contains the new entry shall be
!!    marked for update.
!!
!!    Predefined variables are typically used to set permission modes.
!!
!!    You can bytewise-OR together these variables to create the most
!!    common permissions mode:
!!
!!     User:    R_USR  (read),  W_USR  (write),  X_USR(execute)
!!     Group:   R_GRP  (read),  W_GRP  (write),  X_GRP(execute)
!!     Others:  R_OTH  (read),  W_OTH  (write),  X_OTH(execute)
!!
!!    Additionally, some shortcuts are provided (basically a bitwise-OR
!!    combination of the above):
!!
!!      Read + Write + Execute: RWX_U (User), RWX_G (Group), RWX_O (Others)
!!      DEFFILEMODE: Equivalent of 0666 =rw-rw-rw-
!!      ACCESSPERMS: Equivalent of 0777 = rwxrwxrwx
!!
!!    Therefore, to give only the user rwx (read+write+execute) rights whereas
!!    group members and others may not do anything, you can use any of the
!!    following mkfifo() calls equivalently:
!!
!!      ierr= mkfifo("myfile", IANY([R_USR, W_USR, X_USR]));
!!      ierr= mkfifo("myfile", RWX_U);
!!
!!    In order to give anyone any rights (mode 0777 = rwxrwxrwx), you can
!!    use any of the following calls equivalently:
!!
!!      ierr= mkfifo("myfile",IANY([R_USR,W_USR,X_USR,R_GRP,W_GRP,X_GRP,R_OTH,W_OTH,X_OTH]));
!!      ierr= mkfifo("myfile",IANY([RWX_U,RWX_G,RWX_O]));
!!      ierr= mkfifo("myfile",ACCESSPERMS);
!!##RETURN VALUE
!!    Upon successful completion, return 0.
!!    Otherwise, return -1 and set errno to indicate the error.
!!    If -1 is returned, no FIFO is created.
!!
!!##EXAMPLES
!!
!!   The following example shows how to create a FIFO file named
!!   /home/cnd/mod_done, with read/write permissions for owner, and
!!   with read permissions for group and others.
!!
!!    program demo_system_mkfifo
!!    use M_system, only : system_mkfifo, system_perror
!!    !use M_system, only : R_GRP,R_OTH,R_USR,RWX_G,RWX_O
!!    !use M_system, only : RWX_U,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!    !use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    use M_system, only : W_USR, R_USR, R_GRP, R_OTH
!!    implicit none
!!       integer :: status
!!       status = system_mkfifo("/tmp/buffer", IANY([W_USR, R_USR, R_GRP, R_OTH]))
!!       if(status.ne.0)then
!!          call system_perror('*mkfifo* error:')
!!       endif
!!    end program demo_system_mkfifo
!!
!!   Now some other process (or this one) can read from /tmp/buffer while this program
!!   is running or after, consuming the data as it is read.
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE FUNCTION system_mkfifo(pathname, mode) RESULT(err)
    CHARACTER(len=*), INTENT(in) :: pathname
    INTEGER, INTENT(in) :: mode
    INTEGER :: err
  END FUNCTION system_mkfifo
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!        system_mkdir(3f) - [M_system:FILE_SYSTEM] call mkdir(3c) to create a new directory
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!
!!    Predefined variables are typically used to set permission modes.
!!    You can bytewise-OR together these variables to create the most common
!!    permissions mode:
!!
!!     User:    R_USR  (read),  W_USR  (write),  X_USR(execute)
!!     Group:   R_GRP  (read),  W_GRP  (write),  X_GRP(execute)
!!     Others:  R_OTH  (read),  W_OTH  (write),  X_OTH(execute)
!!
!!    Additionally, some shortcuts are provided (basically a bitwise-OR combination of the above):
!!
!!      Read + Write + Execute: RWX_U (User), RWX_G (Group), RWX_O (Others)
!!      DEFFILEMODE: Equivalent of 0666 =rw-rw-rw-
!!      ACCESSPERMS: Equivalent of 0777 = rwxrwxrwx
!!
!!    Therefore, to give only the user rwx (read+write+execute) rights whereas
!!    group members and others may not do anything, you can use any of the
!!    following mkdir() calls equivalently:
!!
!!      ierr= mkdir("mydir", IANY([R_USR, W_USR, X_USR]));
!!      ierr= mkdir("mydir", RWX_U);
!!
!!    In order to give anyone any rights (mode 0777 = rwxrwxrwx), you can
!!    use any of the following calls equivalently:
!!
!!      ierr= mkdir("mydir",IANY([R_USR,W_USR,X_USR,R_GRP,W_GRP,X_GRP,R_OTH,W_OTH,X_OTH]));
!!      ierr= mkdir("mydir",IANY([RWX_U,RWX_G,RWX_O]));
!!      ierr= mkdir("mydir",ACCESSPERMS);
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_mkdir
!!    use M_system, only : system_perror
!!    use M_system, only : system_mkdir
!!    use M_system, only : R_GRP,R_OTH,R_USR,RWX_G,RWX_O
!!    use M_system, only : RWX_U,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
!!    use M_system, only : DEFFILEMODE, ACCESSPERMS
!!    implicit none
!!    integer :: ierr
!!    ierr=system_mkdir('_scratch',IANY([R_USR,W_USR,X_USR]))
!!    end program demo_system_mkdir
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE FUNCTION system_mkdir(dirname, mode) RESULT(ierr)
    CHARACTER(len=*), INTENT(in) :: dirname
    INTEGER, INTENT(in) :: mode
    INTEGER :: ierr
  END FUNCTION system_mkdir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_opendir(3f) - [M_system:QUERY_FILE] open directory stream by calling opendir(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine system_opendir(dirname,dir,ierr)
!!
!!    character(len=*), intent(in) :: dirname
!!    type(c_ptr)                  :: dir
!!    integer,intent(out)          :: ierr
!!
!!##DESCRIPTION
!!        The system_opendir(3f) procedure opens a directory stream
!!        corresponding to the directory named by the dirname argument.
!!        The directory stream is positioned at the first entry.
!!
!!##RETURN VALUE
!!        Upon successful completion, a pointer to a C dir type is returned.
!!        Otherwise, these functions shall return a null pointer and set
!!        IERR to indicate the error.
!!
!!##ERRORS
!!
!!        An error corresponds to a condition described in opendir(3c):
!!
!!        EACCES    Search permission is denied for the component of the
!!                  path prefix of dirname or read permission is denied
!!                  for dirname.
!!
!!        ELOOP     A loop exists in symbolic links encountered during
!!                  resolution of the dirname argument.
!!
!!        ENAMETOOLONG  The length of a component of a pathname is longer than {NAME_MAX}.
!!
!!        ENOENT        A component of dirname does not name an existing directory or dirname is an empty string.
!!
!!        ENOTDIR       A component of dirname names an existing file that is neither a directory nor a symbolic link to a directory.
!!
!!        ELOOP         More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the dirname argument.
!!
!!        EMFILE        All file descriptors available to the process are currently open.
!!
!!        ENAMETOOLONG  The length of a pathname exceeds {PATH_MAX},
!!                      or pathname resolution of a symbolic link produced an intermediate
!!                      result with a length that exceeds {PATH_MAX}.
!!
!!        ENFILE        Too many files are currently open in the system.
!!
!!##APPLICATION USAGE
!!        The opendir() function should be used in conjunction with readdir(), closedir(), and rewinddir() to examine the contents
!!        of the directory (see the EXAMPLES section in readdir()). This method is recommended for portability.
!!##OPTIONS
!!       dirname name of directory to open a directory stream for
!!##RETURNS
!!       dir   pointer to directory stream. If an
!!             error occurred, it will not be associated.
!!       ierr  0 indicates no error occurred
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_opendir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_closedir
!!    use iso_c_binding
!!    implicit none
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    if(ierr.eq.0)then
!!       !--- read directory stream
!!       do
!!          call system_readdir(dir,filename,ierr)
!!          if(filename.eq.' ')exit
!!          write(*,*)filename
!!       enddo
!!    endif
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!    end program demo_system_opendir
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_opendir(dirname, dir, ierr)
    CHARACTER(len=*), INTENT(in) :: dirname
    TYPE(C_PTR) :: dir
    INTEGER, INTENT(out) :: ierr
  END SUBROUTINE system_opendir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!    system_readdir(3f) - [M_system:QUERY_FILE] read a directory using readdir(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine system_readdir(dir,filename,ierr)
!!
!!  type(c_ptr),value                         :: dir
!!  character(len=:),intent(out),allocatable  :: filename
!!  integer,intent(out)                       :: ierr
!!
!!##DESCRIPTION
!!
!!    system_readdir(3f) returns the name of the directory entry at the
!!    current position in the directory stream specified by the argument
!!    DIR, and positions the directory stream at the next entry. It returns
!!    a null name upon reaching the end of the directory stream.
!!
!!##OPTIONS
!!
!!    DIR       A pointer to the directory opened by system_opendir(3f).
!!
!!##RETURNS
!!
!!    FILENAME  the name of the directory entry at the current position in
!!              the directory stream specified by the argument DIR, and
!!              positions the directory stream at the next entry.
!!
!!              The readdir() function does not return directory entries
!!              containing empty names. If entries for dot or dot-dot exist,
!!              one entry is returned for dot and one entry is returned
!!              for dot-dot.
!!
!!              The entry is marked for update of the last data access
!!              timestamp each time it is read.
!!
!!              reaching the end of the directory stream, the name is a blank name.
!!
!!    IERR      If IERR is set to non-zero on return, an error occurred.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_readdir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_rewinddir,system_closedir
!!    use iso_c_binding
!!    implicit none
!!
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: i, ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    if(ierr.eq.0)then
!!       !--- read directory stream twice
!!       do i=1,2
!!          write(*,'(a,i0)')'PASS ',i
!!          do
!!             call system_readdir(dir,filename,ierr)
!!             if(filename.eq.' ')exit
!!             write(*,*)filename
!!          enddo
!!          call system_rewinddir(dir)
!!       enddo
!!    endif
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!
!!    end program demo_system_readdir
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_readdir(dir, filename, ierr)
    TYPE(C_PTR), VALUE :: dir
    CHARACTER(len=:), INTENT(out), ALLOCATABLE :: filename
    INTEGER, INTENT(out) :: ierr
  END SUBROUTINE system_readdir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!       system_rewinddir(3f) - [M_system:QUERY_FILE] call rewinddir(3c) to rewind directory stream
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine system_rewinddir(dir)
!!
!!     type(c_ptr),value :: dir
!!
!!##DESCRIPTION
!!     Return to pointer to the beginning of the list for a currently open directory list.
!!
!!##OPTIONS
!!     DIR  A C_pointer assumed to have been allocated by a call to SYSTEM_OPENDIR(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_rewinddir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_rewinddir,system_closedir
!!    use iso_c_binding
!!    implicit none
!!
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: i, ierr
!!    !>>> open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    !>>> read directory stream twice
!!    do i=1,2
!!       write(*,'(a,i0)')'PASS ',i
!!       do
!!          call system_readdir(dir,filename,ierr)
!!          if(filename.eq.' ')exit
!!          write(*,*)filename
!!       enddo
!!       !>>> rewind directory stream
!!       call system_rewinddir(dir)
!!    enddo
!!    !>>> close directory stream
!!    call system_closedir(dir,ierr)
!!
!!    end program demo_system_rewinddir
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_rewinddir(dir)
    TYPE(C_PTR), VALUE :: dir
  END SUBROUTINE system_rewinddir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!        system_closedir(3f) - [M_system:QUERY_FILE] close a directory stream by calling closedir(3c)
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        subroutine system_closedir(dir,ierr)
!!
!!         type(c_ptr)         :: dir
!!         integer,intent(out) :: ierr
!!##DESCRIPTION
!!        The SYSTEM_CLOSEDIR(3f) function closes the directory stream referred to by the argument DIR.
!!        Upon return, the value of DIR may no longer point to an accessible object.
!!##OPTIONS
!!        dir     directory stream pointer opened by SYSTEM_OPENDIR(3f).
!!        ierr    Upon successful completion, SYSTEM_CLOSEDIR(3f) returns 0;
!!                otherwise, an error has occurred.
!!##ERRORS
!!        system_closedir(3f) may fail if:
!!
!!        EBADF    The dirp argument does not refer to an open directory stream.
!!        EINTR    The closedir() function was interrupted by a signal.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_system_closedir
!!    use M_system, only : system_opendir,system_readdir
!!    use M_system, only : system_closedir, system_rewinddir
!!    use iso_c_binding, only : c_ptr
!!    implicit none
!!    type(c_ptr)                  :: dir
!!    character(len=:),allocatable :: filename
!!    integer                      :: ierr
!!    !--- open directory stream to read from
!!    call system_opendir('.',dir,ierr)
!!    !--- read directory stream
!!    do
!!       call system_readdir(dir,filename,ierr)
!!       if(filename.eq.' ')exit
!!       write(*,*)filename
!!    enddo
!!    call system_rewinddir(dir)
!!    !--- close directory stream
!!    call system_closedir(dir,ierr)
!!    end program demo_system_closedir
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_closedir(dir, ierr)
    TYPE(C_PTR), VALUE :: dir
    INTEGER, INTENT(out), OPTIONAL :: ierr
  END SUBROUTINE system_closedir
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!    system_putenv(3f) - [M_system:ENVIRONMENT] set environment variable from Fortran by calling putenv(3c)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine system_putenv(string, err)
!!
!!     character(len=*),intent(in)    :: string
!!     integer, optional, intent(out) :: err
!!
!!##DESCRIPTION
!!    The system_putenv() function adds or changes the value of environment variables.
!!
!!##OPTIONS
!!    string  string of format "NAME=value".
!!            If name does not already exist in the environment, then string is added to the environment.
!!            If name does exist, then the value of name in the environment is changed to value.
!!            The string passed to putenv(3c) becomes part of the environment,
!!            so this routine creates a string each time it is called that increases the amount of
!!            memory the program uses.
!!    err     The system_putenv() function returns zero on success, or nonzero if an error occurs.
!!            A non-zero error usually indicates sufficient memory does not exist to store the
!!            variable.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!     program demo_system_putenv
!!     use M_system, only : system_putenv
!!     use iso_c_binding
!!     implicit none
!!     integer :: ierr
!!        !
!!        write(*,'(a)')'no environment variables containing "GRU":'
!!        call execute_command_line('env|grep GRU')
!!        !
!!        call system_putenv('GRU=this is the value',ierr)
!!        write(*,'(a,i0)')'now "GRU" should be defined: ',ierr
!!        call execute_command_line('env|grep GRU')
!!        !
!!        call system_putenv('GRU2=this is the second value',ierr)
!!        write(*,'(a,i0)')'now "GRU" and "GRU2" should be defined: ',ierr
!!        call execute_command_line('env|grep GRU')
!!        !
!!        call system_putenv('GRU2',ierr)
!!        call system_putenv('GRU',ierr)
!!        write(*,'(a,i0)')'should be gone, varies with different putenv(3c): ',ierr
!!        call execute_command_line('env|grep GRU')
!!        write(*,'(a)')'system_unsetenv(3f) is a better way to remove variables'
!!        !
!!     end program demo_system_putenv
!!
!!   Results:
!!
!!    no environment variables containing "GRU":
!!    now "GRU" should be defined: 0
!!    GRU=this is the value
!!    now "GRU" and "GRU2" should be defined: 0
!!    GRU2=this is the second value
!!    GRU=this is the value
!!    should be gone, varies with different putenv(3c): 0
!!    system_unsetenv(3f) is a better way to remove variables
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_putenv(string, err)
    CHARACTER(len=*), INTENT(in) :: string
    INTEGER, OPTIONAL, INTENT(out) :: err
  END SUBROUTINE system_putenv
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION arr2str(array) RESULT(string)
    CHARACTER(len=1), INTENT(in) :: array(:)
    CHARACTER(len=SIZE(array)) :: string
  END FUNCTION arr2str
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!    system_getenv(3f) - [M_system:ENVIRONMENT] get environment variable
!!    from Fortran by calling get_environment_variable(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function system_getenv(name,default)
!!
!!     character(len=:),allocatable         :: system_getenv
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: default
!!
!!##DESCRIPTION
!!    The system_getenv() function gets the value of an environment variable.
!!
!!##OPTIONS
!!    name     Return the value of the specified environment variable or
!!             blank if the variable is not defined.
!!    default  If the value returned would be blank this value will be used
!!             instead.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!    program demo_system_getenv
!!    use M_system, only : system_getenv
!!    implicit none
!!       write(*,'("USER     : ",a)')system_getenv('USER')
!!       write(*,'("LOGNAME  : ",a)')system_getenv('LOGNAME')
!!       write(*,'("USERNAME : ",a)')system_getenv('USERNAME')
!!    end program demo_system_getenv
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE FUNCTION system_getenv(name, default) RESULT(VALUE)
    CHARACTER(len=*), INTENT(in) :: name
    CHARACTER(len=*), INTENT(in), OPTIONAL :: default
    INTEGER :: howbig
    INTEGER :: stat
    CHARACTER(len=:), ALLOCATABLE :: VALUE
  END FUNCTION system_getenv
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!    set_environment_variable(3f) - [M_system:ENVIRONMENT] call setenv(3c) to set environment variable
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine set_environment_variable(NAME, VALUE, STATUS)
!!
!!    character(len=*)               :: NAME
!!    character(len=*)               :: VALUE
!!    integer, optional, intent(out) :: STATUS
!!
!!##DESCRIPTION
!!    The set_environment_variable() procedure adds or changes the value of environment variables.
!!
!!##OPTIONS
!!    NAME    If name does not already exist in the environment, then string is added to the environment.
!!            If name does exist, then the value of name in the environment is changed to value.
!!    VALUE   Value to assign to environment variable NAME
!!    STATUS  returns zero on success, or nonzero if an error occurs.
!!            A non-zero error usually indicates sufficient memory does not exist to store the
!!            variable.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!    program demo_set_environment_variable
!!    use M_system, only : set_environment_variable
!!    use iso_c_binding
!!    implicit none
!!    integer :: ierr
!!       !!
!!       write(*,'(a)')'no environment variables containing "GRU":'
!!       call execute_command_line('env|grep GRU')
!!       !!
!!       call set_environment_variable('GRU','this is the value',ierr)
!!       write(*,'(a,i0)')'now "GRU" should be defined, status=',ierr
!!       call execute_command_line('env|grep GRU')
!!       !!
!!       call set_environment_variable('GRU2','this is the second value',ierr)
!!       write(*,'(a,i0)')'now "GRU" and "GRU2" should be defined, status =',ierr
!!       !!
!!       call execute_command_line('env|grep GRU')
!!    end program demo_set_environment_variable
!!
!!   Results:
!!
!!    no environment variables containing "GRU":
!!    now "GRU" should be defined, status=0
!!    GRU=this is the value
!!    now "GRU" and "GRU2" should be defined, status =0
!!    GRU2=this is the second value
!!    GRU=this is the value
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE set_environment_variable(NAME, VALUE, STATUS)
    CHARACTER(len=*) :: NAME
    CHARACTER(len=*) :: VALUE
    INTEGER, OPTIONAL, INTENT(out) :: STATUS
  END SUBROUTINE set_environment_variable
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!    system_clearenv(3f) - [M_system:ENVIRONMENT] clear environment by calling clearenv(3c)
!!    (LICENSE:PD)
!!
!!
!!##SYNOPSIS
!!
!!    subroutine system_clearenv(ierr)
!!
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!    The clearenv() procedure clears the environment of all name-value
!!    pairs. Typically used in security-conscious applications or ones where
!!    configuration control requires ensuring specific variables are set.
!!
!!##RETURN VALUES
!!    ierr  returns zero on success, and a nonzero value on failure. Optional.
!!          If not present and an error occurs the program stops.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!      program demo_system_clearenv
!!      use M_system, only : system_clearenv
!!      implicit none
!!      ! environment before clearing
!!      call execute_command_line('env|wc')
!!      ! environment after clearing (not necessarily blank!!)
!!      call system_clearenv()
!!      call execute_command_line('env')
!!      end program demo_system_clearenv
!!
!!   Typical output:
!!
!!      89     153    7427
!!      PWD=/home/urbanjs/V600
!!      SHLVL=1
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_clearenv(ierr)
    INTEGER, INTENT(out), OPTIONAL :: ierr
  END SUBROUTINE system_clearenv
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_unsetenv(3f) - [M_system:ENVIRONMENT] delete an environment variable by calling unsetenv(3c)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine system_unsetenv(name,ierr)
!!
!!    character(len=*),intent(in)  :: name
!!    integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!
!!    The system_unsetenv(3f) function deletes the variable name from the
!!    environment.
!!
!!##OPTIONS
!!    name   name of variable to delete.
!!           If name does not exist in the environment, then the
!!           function succeeds, and the environment is unchanged.
!!
!!    ierr   The system_unsetenv(3f) function returns zero on success, or -1 on error.
!!           name is NULL, points to a string of length 0, or contains an '=' character.
!!           Insufficient memory to add a new variable to the environment.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_system_unsetenv
!!      use M_system, only : system_unsetenv, system_putenv
!!      implicit none
!!      call system_putenv('GRU=this is the value')
!!      write(*,'(a)')'The variable GRU should be set'
!!      call execute_command_line('env|grep GRU')
!!      call system_unsetenv('GRU')
!!      write(*,'(a)')'The variable GRU should not be set'
!!      call execute_command_line('env|grep GRU')
!!      end program demo_system_unsetenv
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_unsetenv(name, ierr)
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER, INTENT(out), OPTIONAL :: ierr
  END SUBROUTINE system_unsetenv
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!    system_readenv(3f) - [M_system:ENVIRONMENT] step thru and read environment table
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!       function system_readenv() result(string)
!!
!!        character(len=:),allocatable  :: string
!!##DESCRIPTION
!!    A simple interface allows reading the environment variable table of the process. Call
!!    system_initenv(3f) to initialize reading the environment table, then call system_readenv(3f) can
!!    be called until a blank line is returned. If more than one thread
!!    reads the environment or the environment is changed while being read the results are undefined.
!!##OPTIONS
!!    string  the string returned from the environment of the form "NAME=VALUE"
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_readenv
!!    use M_system, only : system_initenv, system_readenv
!!    character(len=:),allocatable :: string
!!       call system_initenv()
!!       do
!!          string=system_readenv()
!!          if(string.eq.'')then
!!             exit
!!          else
!!             write(*,'(a)')string
!!          endif
!!       enddo
!!    end program demo_system_readenv
!!
!!   Sample results:
!!
!!    USERDOMAIN_ROAMINGPROFILE=buzz
!!    HOMEPATH=\Users\JSU
!!    APPDATA=C:\Users\JSU\AppData\Roaming
!!    MANPATH=/home/urbanjs/V600/LIBRARY/libGPF/download/tmp/man:/home/urbanjs/V600/doc/man:::
!!    DISPLAYNUM=0
!!    ProgramW6432=C:\Program Files
!!    HOSTNAME=buzz
!!    XKEYSYMDB=/usr/share/X11/XKeysymDB
!!    PUBLISH_CMD=
!!    OnlineServices=Online Services
!!         :
!!         :
!!         :
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE FUNCTION system_readenv() RESULT(string)
    CHARACTER(len=:), ALLOCATABLE :: string
  END FUNCTION system_readenv
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!   fileglob(3f) - [M_system:QUERY_FILE] Read output of an ls(1) command from Fortran
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine fileglob(glob,list)
!!
!!    character(len=*),intent(in)   :: glob
!!    character(len=*),pointer      :: list(:)
!!
!!##DESCRIPTION
!!    Non-portable procedure uses the shell and the ls(1) command to expand a filename
!!    and returns a pointer to a list of expanded filenames.
!!
!!##OPTIONS
!!    glob   Pattern for the filenames (like: *.txt)
!!    list   Allocated list of filenames (returned), the caller must deallocate it.
!!
!!##EXAMPLE
!!
!!   Read output of an ls(1) command from Fortran
!!
!!    program demo_fileglob  ! simple unit test
!!       call tryit('*.*')
!!       call tryit('/tmp/__notthere.txt')
!!    contains
!!
!!    subroutine tryit(string)
!!       use M_system, only : fileglob
!!       character(len=255),pointer :: list(:)
!!       character(len=*) :: string
!!       call fileglob(string, list)
!!       write(*,*)'Files:',size(list)
!!       write(*,'(a)')(trim(list(i)),i=1,size(list))
!!       deallocate(list)
!!    end subroutine tryit
!!
!!    end program demo_fileglob  ! simple unit test
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE fileglob(glob, list)
    CHARACTER(len=*), INTENT(in) :: glob
    !! Pattern for the filenames (like: *.txt)
    CHARACTER(len=*), POINTER :: list(:)
    !! Allocated list of filenames (returned), the caller must deallocate it.
  END SUBROUTINE fileglob
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>
!!##NAME
!!   system_uname(3f) - [M_system] call a C wrapper that calls uname(3c) to get current system information from Fortran
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine system_uname(WHICH,NAMEOUT)
!!
!!     character(KIND=C_CHAR),intent(in) :: WHICH
!!     character(len=*),intent(out)      :: NAMEOUT
!!##DESCRIPTION
!!        Given a letter, return a corresponding description of the current operating system.
!!        The NAMEOUT variable is assumed sufficiently large enough to hold the value.
!!
!!        s   return the kernel name
!!        r   return the kernel release
!!        v   return the kernel version
!!        n   return the network node hostname
!!        m   return the machine hardware name
!!        T   test mode -- print all information, in the following order - srvnm
!!
!!##EXAMPLE
!!
!!   Call uname(3c) from Fortran
!!
!!    program demo_system_uname
!!       use M_system, only : system_uname
!!       implicit none
!!       integer,parameter          :: is=100
!!       integer                    :: i
!!       character(len=*),parameter :: letters='srvnmxT'
!!       character(len=is)          :: string=' '
!!
!!       do i=1,len(letters)
!!          write(*,'(80("="))')
!!          call system_uname(letters(i:i),string)
!!          write(*,*)'=====> TESTING system_uname('//letters(i:i)//')--->'//trim(string)
!!       enddo
!!
!!    end program demo_system_uname
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_uname(WHICH, NAMEOUT)
    CHARACTER(KIND=C_CHAR), INTENT(in) :: WHICH
    CHARACTER(len=*), INTENT(out) :: NAMEOUT
  END SUBROUTINE system_uname
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
!!##NAME
!!        system_gethostname(3f) - [M_system:QUERY] get name of current host
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine system_gethostname(string,ierr)
!!
!!        character(len=:),allocatable,intent(out) :: NAME
!!        integer,intent(out)                      :: IERR
!!##DESCRIPTION
!!        The system_gethostname(3f) procedure returns the standard host
!!        name for the current machine.
!!
!!##OPTIONS
!!        string  returns the hostname. Must be an allocatable CHARACTER variable.
!!        ierr    Upon successful completion, 0 shall be returned; otherwise, -1
!!                shall be returned.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_system_gethostname
!!    use M_system, only : system_gethostname
!!    implicit none
!!    character(len=:),allocatable :: name
!!    integer                      :: ierr
!!       call system_gethostname(name,ierr)
!!       if(ierr.eq.0)then
!!          write(*,'("hostname[",a,"]")')name
!!       else
!!          write(*,'(a)')'ERROR: could not get hostname'
!!       endif
!!    end program demo_system_gethostname
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

INTERFACE
  MODULE SUBROUTINE system_gethostname(NAME, IERR)
    CHARACTER(len=:), ALLOCATABLE, INTENT(out) :: NAME
    INTEGER, INTENT(out) :: IERR
  END SUBROUTINE system_gethostname
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
    CLASS(*), INTENT(in) :: gid
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
    CLASS(*), INTENT(in) :: uid
    !! UID to try to look up associated username for. Can be of any
    !! INTEGER type.
    CHARACTER(len=:), ALLOCATABLE :: uname
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
    CHARACTER(len=*), INTENT(IN) :: pathname
    !! The type shall be CHARACTER, of the default kind and a valid
    !! path within the file system.
    INTEGER(kind=INT64), INTENT(OUT) :: values(13)
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
    CHARACTER(len=*) :: tame
    !! A string without wildcards
    CHARACTER(len=*) :: wild
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
