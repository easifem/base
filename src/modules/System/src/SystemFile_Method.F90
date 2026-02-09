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
!# SystemFile_Method
!
! SystemFile_Method is a collection of Fortran procedures that call C
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

MODULE SystemFile_Method
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_PTR
IMPLICIT NONE

PRIVATE
PUBLIC :: System_Utime
PUBLIC :: System_RealPath
PUBLIC :: System_Chown
PUBLIC :: System_Link
PUBLIC :: System_Unlink
PUBLIC :: System_Setumask
PUBLIC :: System_Chdir
PUBLIC :: System_Remove
PUBLIC :: System_Rename
PUBLIC :: System_Chmod
PUBLIC :: System_Getcwd
PUBLIC :: System_Rmdir
PUBLIC :: System_Mkfifo
PUBLIC :: System_Mkdir
PUBLIC :: System_Opendir
PUBLIC :: System_Readdir
PUBLIC :: System_Rewinddir
PUBLIC :: System_Closedir
PUBLIC :: Fileglob
PUBLIC :: System_Dir

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
!                                                   System_Chmod@FileMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-06
! summary: call chmod to change permission mode of a file
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
!                                                 System_Closedir@FileMethods
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
!                                                     System_Dir@FileMethods
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
!
!----------------------------------------------------------------------------

END MODULE SystemFile_Method
