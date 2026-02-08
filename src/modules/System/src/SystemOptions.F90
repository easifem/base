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
!# SystemOptions
!
! System_Method is a collection of Fortran procedures that call C
! or a C wrapper using the ISO_C_BINDING interface to access system calls.
! System calls are a special set of functions used by programs to communicate
! directly with an operating system.
!
!  Generally, system calls are slower than normal function calls because
!  when you make a call control is relinquished to the operating system
!  to perform the system call. In addition, depending on the nature of the
!  system call, your program may be blocked by the OS until the system call
!  has finished, thus making the execution time of your program even longer.
!
!  One rule-of-thumb that should always be followed when calling a system
!  call -- Always check the return value.

MODULE SystemOptions
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_CHAR
USE ISO_C_BINDING, ONLY: C_LONG
USE ISO_C_BINDING, ONLY: C_SHORT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT32
IMPLICIT NONE

PRIVATE

INTEGER(I4B), PARAMETER, PUBLIC :: System_mode_t = INT32
!! mode_t: This is a specific data type (usually an unsigned integer) used in
!! POSIX systems to store file mode information, such as permissions.

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IRGRP") :: R_GRP
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IROTH") :: R_OTH
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IRUSR") :: R_USR
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IRWXG") :: RWX_G
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IRWXO") :: RWX_O
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IRWXU") :: RWX_U
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IWGRP") :: W_GRP
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IWOTH") :: W_OTH
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IWUSR") :: W_USR
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IXGRP") :: X_GRP
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IXOTH") :: X_OTH
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FS_IXUSR") :: X_USR
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FDEFFILEMODE") :: DEFFILEMODE
INTEGER(System_mode_t), PUBLIC, BIND(c, name="FACCESSPERMS") :: ACCESSPERMS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER(C_INT), PUBLIC, PARAMETER :: F_OK = 0
INTEGER(C_INT), PUBLIC, PARAMETER :: R_OK = 4
INTEGER(C_INT), PUBLIC, PARAMETER :: W_OK = 2
INTEGER(C_INT), PUBLIC, PARAMETER :: X_OK = 1

INTEGER(I4B), PARAMETER :: MAX_STR_LEN = 256

!----------------------------------------------------------------------------
!                                                              dirent_SYSTEMA
!----------------------------------------------------------------------------

TYPE, BIND(C) :: dirent_SYSTEMA
  INTEGER(C_LONG) :: d_ino
  INTEGER(C_LONG) :: d_off
  INTEGER(C_SHORT) :: d_reclen
  CHARACTER(len=1, kind=C_CHAR) :: d_name(MAX_STR_LEN)
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
  CHARACTER(len=1, kind=C_CHAR) :: d_name(MAX_STR_LEN)
END TYPE dirent_CYGWIN

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SystemOptions
