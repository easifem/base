! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

MODULE SystemInterface
USE ISO_C_BINDING, ONLY: C_INT, C_SIZE_T, C_INTPTR_T, C_LONG, C_FLOAT
USE ISO_C_BINDING, ONLY: C_LONG_LONG, C_PTR, C_FUNPTR, C_CHAR, C_LONG
IMPLICIT NONE

PRIVATE
PUBLIC :: System_Alarm
PUBLIC :: System_Calloc
PUBLIC :: System_Clock
PUBLIC :: System_Memcpy
PUBLIC :: System_Free
PUBLIC :: System_Malloc
PUBLIC :: System_Realloc
PUBLIC :: System_Time
PUBLIC :: System_Srand
PUBLIC :: System_Kill
PUBLIC :: System_Errno
PUBLIC :: System_Geteuid
PUBLIC :: System_Getuid
PUBLIC :: System_Getegid
PUBLIC :: System_Getgid
PUBLIC :: System_Setsid
PUBLIC :: System_Getsid
PUBLIC :: System_Getpid
PUBLIC :: System_Getppid
PUBLIC :: System_Umask
PUBLIC :: System_Rand
PUBLIC :: System_Initenv

PUBLIC :: C_Flush
PUBLIC :: C_Signal
PUBLIC :: C_Access
PUBLIC :: C_Utime
PUBLIC :: C_RealPath
PUBLIC :: C_Issock
PUBLIC :: C_Time
PUBLIC :: C_Chown
PUBLIC :: C_Link
PUBLIC :: C_Unlink
PUBLIC :: C_Chdir
PUBLIC :: C_Remove
PUBLIC :: C_Rename
PUBLIC :: C_Chmod
PUBLIC :: C_Setenv
PUBLIC :: C_Unsetenv
PUBLIC :: C_Readenv
PUBLIC :: C_Putenv
PUBLIC :: C_Isfifo
PUBLIC :: C_Ischr
PUBLIC :: C_Isreg
PUBLIC :: C_Islnk
PUBLIC :: C_Isblk
PUBLIC :: C_Isdir
PUBLIC :: C_CPU_Time
PUBLIC :: C_Perror
PUBLIC :: C_Uname
PUBLIC :: C_Gethostname
PUBLIC :: C_Getlogin
PUBLIC :: C_Perm
PUBLIC :: C_Getgrgid
PUBLIC :: C_Getpwuid
PUBLIC :: C_Stat

!----------------------------------------------------------------------------
!                                                               System_Alarm
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION System_Alarm(seconds) BIND(c, name="alarm")
    IMPORT :: C_INT
    INTEGER(kind=C_INT), VALUE :: seconds
    INTEGER(kind=C_INT) :: System_Alarm
  END FUNCTION System_Alarm
END INTERFACE

!----------------------------------------------------------------------------
!                                                               System_Calloc
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION System_Calloc(nelem, elsize) BIND(c, name="calloc")
    IMPORT :: C_SIZE_T, C_INTPTR_T
    INTEGER(C_SIZE_T), VALUE :: nelem, elsize
    INTEGER(C_INTPTR_T) :: System_Calloc
  END FUNCTION System_Calloc
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SYSTEM_CLOCK
!----------------------------------------------------------------------------

INTERFACE
  PURE FUNCTION SYSTEM_CLOCK() BIND(c, name="clock")
    IMPORT :: C_LONG
    INTEGER(C_LONG) :: system_clock
  END FUNCTION SYSTEM_CLOCK
END INTERFACE

!----------------------------------------------------------------------------
!                                                              System_Memcpy
!----------------------------------------------------------------------------

! Copy N bytes of SRC to DEST, no aliasing or overlapping allowed.
! extern void *memcpy (void *dest, const void *src, size_t n);
INTERFACE
  SUBROUTINE System_Memcpy(dest, src, n) BIND(C, name='memcpy')
    IMPORT :: C_INTPTR_T, C_SIZE_T
    INTEGER(C_INTPTR_T), VALUE :: dest
    INTEGER(C_INTPTR_T), VALUE :: src
    INTEGER(C_SIZE_T), VALUE :: n
  END SUBROUTINE System_Memcpy
END INTERFACE

!----------------------------------------------------------------------------
!                                                                System_Free
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE System_Free(ptr) BIND(c, name="free")
    IMPORT :: C_INTPTR_T
    INTEGER(C_INTPTR_T), VALUE :: ptr
  END SUBROUTINE System_Free
END INTERFACE

!----------------------------------------------------------------------------
!                                                              System_Malloc
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION System_Malloc(size) BIND(c, name="malloc")
    IMPORT :: C_SIZE_T, C_INTPTR_T
    INTEGER(C_SIZE_T), VALUE :: size
    INTEGER(C_INTPTR_T) :: System_Malloc
  END FUNCTION System_Malloc
END INTERFACE

!----------------------------------------------------------------------------
!                                                             System_Realloc
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION System_Realloc(ptr, size) BIND(c, name="realloc")
    IMPORT :: C_SIZE_T, C_INTPTR_T
    INTEGER(C_INTPTR_T), VALUE :: ptr
    INTEGER(C_SIZE_T), VALUE :: size
    INTEGER(C_INTPTR_T) :: System_Realloc
  END FUNCTION System_Realloc
END INTERFACE

!----------------------------------------------------------------------------
!                                                                System_Time
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION System_Time(tloc) BIND(c, name="time")
    ! tloc argument should be loaded via C_LOC from iso_c_binding
    IMPORT :: C_PTR, C_LONG
    TYPE(C_PTR), VALUE :: tloc
    INTEGER(C_LONG) :: System_Time
  END FUNCTION System_Time
END INTERFACE

!----------------------------------------------------------------------------
!                                                               System_Srand
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Set seed for pseudo-random number generator system_rand(3f)
!
!# System_Srand
!
! System_Srand(3f) calls the C routine srand(3c) The
! srand(3c)/System_Srand(3f) function uses its argument as the seed
! for a new sequence of pseudo-random integers to be returned by
! system_rand(3f)/rand(3c). These sequences are repeatable by calling
! System_Srand(3f) with the same seed value. If no seed value is
! provided, the system_rand(3f) function is automatically seeded with
! a value of 1.
!
!
!## Usage
!
!```fortran
!       program System_Srand
!       use M_system, only : System_Srand, system_rand
!       implicit none
!       integer :: i,j
!       do j=1,2
!          call System_Srand(1001)
!          do i=1,10
!             write(*,*)system_rand()
!          enddo
!          write(*,*)
!       enddo
!       end program System_Srand
!```

INTERFACE
  SUBROUTINE System_Srand(seed) BIND(c, name='srand')
    IMPORT C_INT
    INTEGER(kind=C_INT), INTENT(in) :: seed
  END SUBROUTINE System_Srand
END INTERFACE

!----------------------------------------------------------------------------
!                                                                System_Kill
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-04
! summary: sends a signal to a process or a group of processes
!
!# System_Kill
!
! The kill() function shall send a signal to a process or a group of
! processes specified by pid. The signal to be sent is specified by sig
! and is either one from the list given in <signal.h> or 0. If sig is 0
! (the null signal), error checking is performed but no signal is actually
! sent. The null signal can be used to check the validity of pid.
!
! For a process to have permission to send a signal to a process designated
! by pid, unless the sending process has appropriate privileges, the real
! or effective user ID of the sending process shall match the real or
! saved set-user-ID of the receiving process.
!
! If pid is greater than 0, sig shall be sent to the process whose process
! ID is equal to pid.
!
! If pid is 0, sig shall be sent to all processes (excluding an unspecified
! set of system processes) whose process group ID is equal to the process
! group ID of the sender, and for which the process has permission to send
! a signal.
!
! If pid is -1, sig shall be sent to all processes (excluding an unspecified
! set of system processes) for which the process has permission to send
! that signal.
!
! If pid is negative, but not -1, sig shall be sent to all processes
! (excluding an unspecified set of system processes) whose process group
! ID is equal to the absolute value of pid, and for which the process has
! permission to send a signal.
!
! If the value of pid causes sig to be generated for the sending process,
! and if sig is not blocked for the calling thread and if no other thread
! has sig unblocked or is waiting in a sigwait() function for sig, either
! sig or at least one pending unblocked signal shall be delivered to the
! sending thread before kill() returns.
!
! The user ID tests described above shall not be applied when sending
! SIGCONT to a process that is a member of the same session as the sending
! process.
!
! An implementation that provides extended security controls may impose
! further implementation-defined restrictions on the sending of signals,
! including the null signal. In particular, the system may deny the
! existence of some or all of the processes specified by pid.
!
! The kill() function is successful if the process has permission to send
! sig to any of the processes specified by pid. If kill() fails, no signal
! shall be sent.
!
!
! Upon successful completion, 0 shall be returned. Otherwise, -1 shall be
! returned and errno set to indicate the error.
!
!## ERRORS
!
! The kill() function shall fail if:
!
! EINVAL The value of the sig argument is an invalid or unsupported signal
!       number.
!
! EPERM The process does not have permission to send the signal to
!       any receiving process.
!
! ESRCH No process or process group can be found corresponding to
!       that specified by pid. The following sections are informative.
!
!## Examples
!
!```fortran
!    program demo_system_kill
!    use M_system, only : system_kill
!    use M_system, only : system_perror
!    implicit none
!    integer           :: i,pid,ios,ierr,signal=9
!    character(len=80) :: argument
!
!       do i=1,command_argument_count()
!          ! get arguments from command line
!          call get_command_argument(i, argument)
!          ! convert arguments to integers assuming they are PID numbers
!          read(argument,'(i80)',iostat=ios) pid
!          if(ios.ne.0)then
!             write(*,*)'bad PID=',trim(argument)
!          else
!             write(*,*)'kill SIGNAL=',signal,' PID=',pid
!          ! send signal SIGNAL to pid PID
!             ierr=system_kill(pid,signal)
!          ! write message if an error was detected
!             if(ierr.ne.0)then
!                call system_perror('*demo_system_kill*')
!             endif
!          endif
!       enddo
!    end program demo_system_kill
!```

INTERFACE
  FUNCTION System_Kill(pid, signal) BIND(c, name="kill") RESULT(c_ierr)
    IMPORT C_INT
    INTEGER(kind=C_INT), VALUE, INTENT(in) :: pid
    INTEGER(kind=C_INT), VALUE, INTENT(in) :: signal
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION System_Kill
END INTERFACE

!----------------------------------------------------------------------------
!                                                               System_Errno
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-04
! summary: C error return value
!
!# System_Errno
!
! Many C routines return an error code which can be queried by errno.
! The M_system(3fm) is primarily composed of Fortran routines that call
! C routines. In the cases where an error code is returned vi
! system_errno(3f) these routines will indicate it.
!
!## Examples
!
!   Sample program:
!
!```fortran
! program demo_system_errno
! use M_system, only : system_errno, system_unlink, system_perror
! implicit none
! integer :: stat
! stat=system_unlink('not there/OR/anywhere')
! if(stat.ne.0)then
!         write(*,*)'err=',system_errno()
!         call system_perror('*demo_system_errno*')
! endif
! end program demo_system_errno
!```
!
!```txt
! Typical Results:
! err=           2
! *demo_system_errno*: No such file or directory
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Errno() BIND(C, name="my_errno")
    IMPORT C_INT
  END FUNCTION System_Errno
END INTERFACE

!----------------------------------------------------------------------------
!                                                             System_Geteuid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Get effective UID of current process from Fortran
!
!# System_Geteuid
!
! The system_geteuid(3f) function shall return the effective user
! ID of the calling process. The geteuid() function shall always be
! successful and no return value is reserved to indicate the error.
!
!## Examples
!
!```fortran
! program demo_system_geteuid
! use M_system, only : system_geteuid
! implicit none
! write(*,*)'EFFECTIVE UID=',system_geteuid()
! end program demo_system_geteuid
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Geteuid() BIND(C, name="geteuid")
    IMPORT C_INT
  END FUNCTION System_Geteuid
END INTERFACE

!----------------------------------------------------------------------------
!                                                               System_Getuid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: get real UID of current process from Fortran
!
!# System_Getuid
!
! The system_getuid(3f) function shall return the real user ID
! of the calling process. The getuid() function shall always be
! successful and no return value is reserved to indicate the error.
!
!## Examples
!
!```fortran
!    program demo_system_getuid
!    use M_system, only : system_getuid
!    implicit none
!       write(*,*)'UID=',system_getuid()
!    end program demo_system_getuid
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Getuid() BIND(C, name="getuid")
    IMPORT C_INT
  END FUNCTION System_Getuid
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: get the effective group ID (GID) of current process from Fortran
!
!# System_Getegid
!
! The getegid() function returns the effective group ID of the
! calling process.
!
! The getegid() should always be successful and no return value is
! reserved to indicate an error.
!
!## Examples
!
!```fortran
!    program demo_system_getegid
!    use M_system, only : system_getegid
!    implicit none
!       write(*,*)'GID=',system_getegid()
!    end program demo_system_getegid
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Getegid() BIND(C, name="getegid")
    IMPORT C_INT
  END FUNCTION System_Getegid
END INTERFACE

!----------------------------------------------------------------------------
!                                                               System_Getgid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: get the real group ID (GID) of current process from Fortran
!
!# System_Getgid
!
! The getgid() function returns the real group ID of the calling process.
!
! The getgid() should always be successful and no return value is
! reserved to indicate an error.
!
!## Examples
!
!```fortran
!    program demo_system_getgid
!    use M_system, only : system_getgid
!    implicit none
!       write(*,*)'GID=',system_getgid()
!    end program demo_system_getgid
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Getgid() BIND(C, name="getgid")
    IMPORT C_INT
  END FUNCTION System_Getgid
END INTERFACE

!----------------------------------------------------------------------------
!                                                              System_Setsid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: create session and set the process group ID of a session leader
!
!# System_Setsid
!
! The setsid() function creates a new session, if the calling process
! is not a process group leader. Upon return the
! calling process shall be the session leader of this new session,
!  shall be the process group leader of a new process
! group, and shall have no controlling terminal.
!  The process group ID of the calling process shall be set equal to the
! process ID of the calling process.
!  The calling process shall be the only process in the new process group
!  and the only process in the new session.
!
!  Upon successful completion, setsid() shall return the value of
!  the new process group ID of the calling process. Otherwise,
!   it shall return �-1 and set errno to indicate the error.
!
!## Errors
!
! The setsid() function shall fail if:
!
!- The calling process is already a process group leader
!- the process group ID of a process other than the calling
! process matches the process ID of the calling process.
!
!## Examples
!
!```fortran
!    program demo_system_setsid
!    use M_system,      only : system_setsid
!    implicit none
!       write(*,*)'SID=',system_setsid()
!    end program demo_system_setsid
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Setsid() BIND(C, name="setsid")
    IMPORT C_INT
  END FUNCTION System_Setsid
END INTERFACE

!----------------------------------------------------------------------------
!                                                               System_Getsid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Get the process group ID of a session leader
!
!# System_Getsid
!
! The system_getsid() function obtains the process group ID of the
! process that is the session leader of the process specified by pid.
! If pid is 0, it specifies the calling process.
!
! Upon successful completion, system_getsid() shall return the process group
! ID of the session leader of the specified process. Otherwise,
! it shall return -1 and set errno to indicate the error.
!
!
!## Usage
!
!```fortran
!    program demo_system_getsid
!    use M_system,      only : system_getsid
!    use ISO_C_BINDING, only : c_int
!    implicit none
!       write(*,*)'SID=',system_getsid(0_c_int)
!    end program demo_system_getsid
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Getsid(c_pid) BIND(C, name="getsid")
    IMPORT C_INT
    INTEGER(kind=C_INT) :: c_pid
  END FUNCTION System_Getsid
END INTERFACE

!----------------------------------------------------------------------------
!                                                               System_Getpid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Get PID (process ID) of current process from Fortran
!
!# System_Getpid
!
! The system_getpid() function returns the process ID of the
! calling process.
!
! The value returned is the integer process ID. The system_getpid()
! function shall always be successful and no return value is reserved
! to indicate an error.
!
!
!## Usage
!
!```fortran
!    program demo_system_getpid
!    use M_system, only : system_getpid
!    implicit none
!       write(*,*)'PID=',system_getpid()
!    end program demo_system_getpid
!```

INTERFACE
  PURE INTEGER(kind=C_INT) FUNCTION System_Getpid() BIND(C, name="getpid")
    IMPORT C_INT
  END FUNCTION System_Getpid
END INTERFACE

!----------------------------------------------------------------------------
!                                                              System_Getppid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: get parent process ID (PPID) of current process from Fortran
!
!# System_Getppid
!
! The system_getppid() function returns the parent process ID of
! the calling process.
!
! The system_getppid() function should always be successful and no
! return value is reserved to indicate an error.
!
!## Examples
!
!```fortran
!    program demo_system_getppid
!    use M_system, only : system_getppid
!    implicit none
!       write(*,*)'PPID=',system_getppid()
!    end program demo_system_getppid
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Getppid() BIND(C, name="getppid")
    IMPORT C_INT
  END FUNCTION System_Getppid
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary:  Set and get the file mode creation mask
!
!# System_Umask
!
! The system_umask() function shall set the file mode creation mask of the
! process to cmask and return the previous value of the mask. Only
! the file permission bits of cmask (see <sys/stat.h>) are used;
! the meaning of the other bits is implementation-defined.
!
! The file mode creation mask of the process is used to turn off
! permission bits in the mode argument supplied during calls to
! the following functions:
!
! Bit positions that are set in cmask are cleared in the mode of
!  the created file.
!
! The file permission bits in the value returned by umask() shall be
! the previous value of the file mode creation mask. The state of any
! other bits in that value is unspecified, except that a subsequent
! call to umask() with the returned value as cmask shall leave the
! state of the mask the same as its state before the first call,
! including any unspecified use of those bits.
!
!
!## Examples
!
!```fortran
!    program demo_system_umask
!    use M_system, only : system_getumask, system_setumask
!    implicit none
!    integer value
!    integer mask
!    mask=O'002'
!    value=system_setumask(mask)
!    write(*,'(a,"octal=",O4.4," decimal=",i0)')'OLD VALUE=',value,value
!    value=system_getumask()
!    write(*,'(a,"octal=",O4.4," decimal=",i0)')'MASK=',mask,mask
!    write(*,'(a,"octal=",O4.4," decimal=",i0)')'NEW VALUE=',value,value
!    end program demo_system_umask
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Umask(umask_value) BIND(C, name="umask")
    IMPORT C_INT
    INTEGER(kind=C_INT), VALUE :: umask_value
  END FUNCTION System_Umask
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 System_Rand
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Call pseudo-random number generator rand(3c)
!
!# System_Rand
!
! Use rand(3c) to generate pseudo-random numbers.
!
!## Examples
!
!## Usage
!
!```fortran
!       program demo_system_rand
!       use M_system, only : system_srand, system_rand
!       implicit none
!       integer :: i
!
!       call system_srand(1001)
!       do i=1,10
!          write(*,*)system_rand()
!       enddo
!       write(*,*)
!       end program demo_system_rand
!```

INTERFACE
  INTEGER(kind=C_INT) FUNCTION System_Rand() BIND(C, name="rand")
    IMPORT C_INT
  END FUNCTION System_Rand
END INTERFACE

!----------------------------------------------------------------------------
!                                                             System_Initenv
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-05
! summary: Initialize environment table pointer and size
!          so table can be read by readenv(3f)
!
!# System_Initenv
!
! A simple interface allows reading the environment variable table
! of the process. Call system_initenv(3f) to initialize reading the
! environment table, then call system_readenv(3f) until a blank line
! is returned. If more than one thread reads the environment or the
! environment is changed while being read the results are undefined.
!
!
!## Examples
!
!```fortran
!    program demo_system_initenv
!    use M_system, only : system_initenv, system_readenv
!    character(len=:),allocatable :: string
!       call system_initenv()
!       do
!          string=system_readenv()
!          if(string.eq.'')then
!             exit
!          else
!             write(*,'(a)')string
!          endif
!       enddo
!    end program demo_system_initenv
!```

INTERFACE
  SUBROUTINE System_Initenv() BIND(C, NAME='my_initenv')
  END SUBROUTINE System_Initenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Flush
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE C_Flush() BIND(C, name="my_flush")
  END SUBROUTINE C_Flush
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   C_Signal
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Signal(signal, sighandler) BIND(c, name='signal')
    IMPORT :: C_INT, C_FUNPTR
    INTEGER(C_INT), VALUE, INTENT(in) :: signal
    TYPE(C_FUNPTR), VALUE, INTENT(in) :: sighandler
    TYPE(C_FUNPTR) :: C_Signal
  END FUNCTION C_Signal
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Access
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Access(c_pathname, c_amode) BIND(C, name="my_access") &
    RESULT(c_ierr)
    IMPORT :: C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: c_pathname(*)
    INTEGER(kind=C_INT), VALUE :: c_amode
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Access
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Utime
!----------------------------------------------------------------------------

INTERFACE
 FUNCTION C_Utime(c_pathname, c_times) BIND(C, name="my_utime") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: c_pathname(*)
    INTEGER(kind=C_INT), INTENT(in) :: c_times(2)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Utime
END INTERFACE

!----------------------------------------------------------------------------
!                                                            System_RealPath
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_RealPath(c_input) BIND(c, name="my_realpath") RESULT(c_buffer)
    IMPORT C_CHAR, C_SIZE_T, C_PTR, C_INT
    CHARACTER(kind=C_CHAR), INTENT(in) :: c_input(*)
    TYPE(C_PTR) :: c_buffer
  END FUNCTION C_RealPath
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   C_Issock
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Issock(pathname) BIND(C, name="my_issock") RESULT(c_ierr)
    IMPORT :: C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Issock
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     C_Time
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Time(tloc) BIND(c, name='time')
    IMPORT :: C_LONG
    INTEGER(kind=C_LONG), INTENT(in), VALUE :: tloc
    INTEGER(kind=C_LONG) :: C_Time
  END FUNCTION C_Time
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     C_Chown
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Chown(c_dirname, c_owner, c_group) &
    BIND(C, name="my_chown") RESULT(c_ierr)
    IMPORT :: C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(IN) :: c_dirname(*)
    INTEGER(kind=C_INT), INTENT(IN), VALUE :: c_owner
    INTEGER(kind=C_INT), INTENT(IN), VALUE :: c_group
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Chown
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     C_Link
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Link(C_Oldname, C_Newname) &
    BIND(C, name="link") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: c_oldname(*)
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: c_newname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Link
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Unlink
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Unlink(C_Fname) &
    BIND(C, name="unlink") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1) :: c_fname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Unlink
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Chdir
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(kind=C_INT) FUNCTION C_Chdir(C_Path) &
    BIND(C, name="chdir")
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR) :: c_path(*)
  END FUNCTION C_Chdir
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   C_Remove
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Remove(C_Path) BIND(c, name="remove") RESULT(c_err)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: C_Path(*)
    INTEGER(C_INT) :: c_err
  END FUNCTION C_Remove
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   C_Rename
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Rename(C_Input, C_Output) BIND(c, name="rename") RESULT(C_Err)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR), INTENT(in) :: C_Input(*)
    CHARACTER(kind=C_CHAR), INTENT(in) :: C_Output(*)
    INTEGER(C_INT) :: C_Err
  END FUNCTION C_Rename
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Chmod
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Chmod(filename, mode) BIND(c, name="chmod") RESULT(ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR), INTENT(IN) :: filename(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: mode
    INTEGER(C_INT) :: ierr
  END FUNCTION C_Chmod
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   C_Setenv
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(kind=C_INT) FUNCTION C_Setenv(C_Name, C_VALUE) &
    BIND(C, NAME="setenv")
    IMPORT C_INT, C_CHAR
    CHARACTER(kind=C_CHAR) :: C_Name(*)
    CHARACTER(kind=C_CHAR) :: C_VALUE(*)
  END FUNCTION C_Setenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 C_Unsetenv
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(kind=C_INT) FUNCTION C_Unsetenv(C_Name) &
    BIND(C, NAME="unsetenv")
    IMPORT C_INT, C_CHAR
    CHARACTER(len=1, kind=C_CHAR) :: C_Name(*)
  END FUNCTION C_Unsetenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  C_Readenv
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE C_Readenv(C_String) &
    BIND(C, NAME='my_readenv')
    IMPORT C_CHAR, C_INT, C_PTR, C_SIZE_T
    CHARACTER(kind=C_CHAR), INTENT(OUT) :: c_string(*)
  END SUBROUTINE C_Readenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   C_Putenv
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(kind=C_INT) FUNCTION C_Putenv(C_String) &
    BIND(C, name="putenv")
    IMPORT C_INT, C_CHAR
    CHARACTER(kind=C_CHAR) :: C_String(*)
  END FUNCTION C_Putenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   C_Isfifo
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Isfifo(pathname) &
    BIND(C, name="my_isfifo") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Isfifo
END INTERFACE

!----------------------------------------------------------------------------
!                                                        C_Ischr
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Ischr(pathname) &
    BIND(C, name="my_ischr") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Ischr
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Isreg
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Isreg(pathname) &
    BIND(C, name="my_isreg") RESULT(c_ierr)
    IMPORT :: C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Isreg
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Islnk
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Islnk(pathname) &
    BIND(C, name="my_islnk") RESULT(c_ierr)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(IN) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Islnk
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Isblk
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Isblk(pathname) &
    BIND(C, name="my_isblk") RESULT(c_ierr)
    IMPORT :: C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(IN) :: pathname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Isblk
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Isdir
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Isdir(dirname) &
    BIND(C, name="my_isdir") RESULT(c_ierr)
    IMPORT :: C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(IN) :: dirname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Isdir
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 C_CPU_Time
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE C_CPU_Time(total, user, system) &
    BIND(C, NAME='my_cpu_time')
    IMPORT :: C_FLOAT
    REAL(C_FLOAT) :: total, user, system
  END SUBROUTINE C_CPU_Time
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   C_Perror
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE C_Perror(prefix) BIND(C, name="perror")
    IMPORT C_CHAR
    CHARACTER(kind=C_CHAR) :: prefix(*)
  END SUBROUTINE C_Perror
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    C_Uname
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE C_Uname(WHICH, BUF, BUFLEN) BIND(C, NAME='my_uname')
    IMPORT :: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(KIND=C_CHAR), INTENT(in) :: WHICH
    CHARACTER(KIND=C_CHAR), INTENT(out) :: BUF(*)
    INTEGER(C_INT), INTENT(in) :: BUFLEN
  END SUBROUTINE C_Uname
END INTERFACE

!----------------------------------------------------------------------------
!                                                              C_Gethostname
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Gethostname(c_buf, c_buflen) BIND(C, NAME='gethostname')
    IMPORT :: C_CHAR, C_INT
    INTEGER(kind=C_INT) :: C_Gethostname
    CHARACTER(KIND=C_CHAR), INTENT(out) :: c_buf(*)
    INTEGER(C_INT), INTENT(in), VALUE :: c_buflen
  END FUNCTION C_Gethostname
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  C_Getlogin
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Getlogin() &
    BIND(c, name="getlogin") RESULT(c_username)
    IMPORT C_INT, C_PTR
    TYPE(C_PTR) :: c_username
  END FUNCTION C_Getlogin
END INTERFACE

INTERFACE
  FUNCTION C_Perm(c_mode) &
    BIND(c, name="my_get_perm") RESULT(c_permissions)
    IMPORT C_INT, C_PTR, C_LONG
    INTEGER(kind=C_LONG), VALUE :: c_mode
    TYPE(C_PTR) :: c_permissions
  END FUNCTION C_Perm
END INTERFACE

INTERFACE
  FUNCTION C_Getgrgid(C_Gid, C_Groupname) &
    BIND(c, name="my_getgrgid") RESULT(c_ierr)
    IMPORT C_INT, C_PTR, C_CHAR, C_LONG_LONG
    INTEGER(kind=C_LONG_LONG), VALUE, INTENT(in) :: c_gid
    CHARACTER(kind=C_CHAR), INTENT(out) :: c_groupname(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Getgrgid
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION C_Getpwuid(C_Uid, C_Username) &
    BIND(c, name="my_getpwuid") RESULT(c_ierr)
    IMPORT C_INT, C_PTR, C_CHAR, C_LONG_LONG
    INTEGER(kind=C_LONG_LONG), VALUE, INTENT(in) :: c_uid
    CHARACTER(kind=C_CHAR), INTENT(out) :: c_username(*)
    INTEGER(kind=C_INT) :: c_ierr
  END FUNCTION C_Getpwuid
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     C_Stat
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE C_Stat(buffer, cvalues, cierr, cdebug) &
    BIND(c, name="my_stat")
    IMPORT :: C_CHAR, C_SIZE_T, C_PTR, C_INT, C_LONG
    CHARACTER(kind=C_CHAR), INTENT(IN) :: buffer(*)
    INTEGER(C_LONG), INTENT(OUT) :: cvalues(*)
    INTEGER(C_INT) :: cierr
    INTEGER(C_INT), INTENT(in) :: cdebug
  END SUBROUTINE C_Stat
END INTERFACE
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SystemInterface
