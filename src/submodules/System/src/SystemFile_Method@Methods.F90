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
!

SUBMODULE(SystemFile_Method) Methods
USE ISO_C_BINDING, ONLY: C_ASSOCIATED
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_LONG
USE ISO_C_BINDING, ONLY: C_CHAR
USE System_Utility, ONLY: Matchw
USE System_Utility, ONLY: Arr2Str
USE System_Utility, ONLY: C2F_String
USE System_Utility, ONLY: Str2_Carr
USE System_Utility, ONLY: TimeStamp

USE SystemInterface, ONLY: C_Utime
USE SystemInterface, ONLY: C_RealPath
USE SystemInterface, ONLY: C_Chown
USE SystemInterface, ONLY: C_Link
USE SystemInterface, ONLY: C_Unlink
USE SystemInterface, ONLY: C_Chdir
USE SystemInterface, ONLY: C_Remove
USE SystemInterface, ONLY: C_Rename
USE SystemInterface, ONLY: C_Chmod
USE SystemInterface, ONLY: System_Getpid
USE SystemInterface, ONLY: System_Errno
USE SystemInterface, ONLY: System_Umask

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                System_Utime
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Utime
INTEGER(kind=8) :: times_local(2)
LOGICAL :: isok

!-! int my_utime(const char *path, int times[2])
IF (PRESENT(times)) THEN
  times_local = times
ELSE
  times_local = timestamp()
END IF

isok = C_Utime(Str2_Carr(TRIM(pathname)), INT(Times_Local, kind=C_INT)) &
       .EQ. 0

IF (isok) THEN
  system_utime = .TRUE.
ELSE
  system_utime = .FALSE.
END IF
END PROCEDURE System_Utime

!----------------------------------------------------------------------------
!                                                             System_RealPath
!----------------------------------------------------------------------------

MODULE PROCEDURE System_RealPath
TYPE(C_PTR) :: c_output
c_output = C_RealPath(str2_carr(TRIM(input)))
IF (.NOT. C_ASSOCIATED(c_output)) THEN
  string = CHAR(0)
ELSE
  string = C2F_string(c_output)
END IF
END PROCEDURE System_RealPath

!----------------------------------------------------------------------------
!                                                               System_Chown
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Chown
LOGICAL :: isok

isok = C_Chown( &
       Str2_Carr(TRIM(dirname)), &
       INT(owner, kind=C_INT), &
       INT(group, kind=C_INT)) .EQ. 1

IF (isok) THEN
  System_Chown = .TRUE.
ELSE
  System_Chown = .FALSE.
END IF

END PROCEDURE System_Chown

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_link
INTEGER(C_INT) :: c_ierr
C_Ierr = C_Link(Str2_Carr(TRIM(oldname)), Str2_Carr(TRIM(newname)))
ierr = c_ierr
END PROCEDURE system_link

!----------------------------------------------------------------------------
!                                                              System_Unlink
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Unlink
ierr = C_Unlink(Str2_Carr(TRIM(fname)))
END PROCEDURE System_Unlink

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Setumask
INTEGER(C_INT) :: Umask_C
Umask_C = Umask_Value
Old_Umask = System_Umask(Umask_C)
END PROCEDURE System_Setumask

!----------------------------------------------------------------------------
!                                                               System_Chdir
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Chdir
INTEGER :: Loc_Err
Loc_Err = C_Chdir(Str2_Carr(TRIM(path)))
IF (PRESENT(err)) THEN
  err = Loc_Err
END IF
END PROCEDURE System_Chdir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Remove
err = C_Remove(Str2_Carr(TRIM(path)))
END PROCEDURE System_Remove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Rename
ierr = C_Rename(Str2_Carr(TRIM(input)), Str2_Carr(TRIM(output)))
END PROCEDURE System_Rename

!----------------------------------------------------------------------------
!                                                               System_Chmod
!----------------------------------------------------------------------------

MODULE PROCEDURE System_Chmod
ierr = C_Chmod(Str2_Carr(TRIM(filename)), INT(mode, KIND(0_C_INT)))
END PROCEDURE System_Chmod

!----------------------------------------------------------------------------
!                                                              System_Getcwd
!----------------------------------------------------------------------------

MODULE PROCEDURE system_getcwd
INTEGER(kind=C_LONG), PARAMETER :: length = 4097_C_LONG
CHARACTER(kind=C_CHAR, len=1) :: buffer(length)
TYPE(C_PTR) :: buffer2
INTERFACE
  FUNCTION c_getcwd(buffer, size) BIND(c, name="getcwd") RESULT(buffer_result)
    IMPORT C_CHAR, C_SIZE_T, C_PTR
    CHARACTER(kind=C_CHAR), INTENT(out) :: buffer(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(in) :: size
    TYPE(C_PTR) :: buffer_result
  END FUNCTION
END INTERFACE

buffer = ' '
buffer2 = c_getcwd(buffer, length)
IF (.NOT. C_ASSOCIATED(buffer2)) THEN
  output = ''
  ierr = -1
ELSE
  output = TRIM(arr2str(buffer))
  ierr = 0
END IF
END PROCEDURE system_getcwd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_rmdir
INTERFACE
  FUNCTION c_rmdir(c_path) BIND(c, name="rmdir") RESULT(c_err)
    IMPORT C_CHAR, C_INT
    CHARACTER(kind=C_CHAR, len=1), INTENT(in) :: c_path(*)
    INTEGER(C_INT) :: c_err
  END FUNCTION
END INTERFACE

err = c_rmdir(str2_carr(TRIM(dirname)))
IF (err .NE. 0) err = system_errno()
END PROCEDURE system_rmdir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_mkfifo
INTEGER :: c_mode
INTERFACE
  FUNCTION c_mkfifo(c_path, c_mode) BIND(c, name="mkfifo") RESULT(c_err)
    IMPORT C_CHAR, C_INT
    CHARACTER(len=1, kind=C_CHAR), INTENT(in) :: c_path(*)
    INTEGER(C_INT), INTENT(in), VALUE :: c_mode
    INTEGER(C_INT) :: c_err
  END FUNCTION c_mkfifo
END INTERFACE

c_mode = mode
err = c_mkfifo(str2_carr(TRIM(pathname)), c_mode)
END PROCEDURE system_mkfifo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_mkdir
INTEGER :: c_mode
INTEGER(kind=C_INT) :: err

INTERFACE
  FUNCTION c_mkdir(c_path, c_mode) BIND(c, name="mkdir") RESULT(c_err)
    IMPORT C_CHAR, C_INT
    CHARACTER(len=1, kind=C_CHAR), INTENT(in) :: c_path(*)
    INTEGER(C_INT), INTENT(in), VALUE :: c_mode
    INTEGER(C_INT) :: c_err
  END FUNCTION c_mkdir
END INTERFACE

INTERFACE
  SUBROUTINE my_mkdir(string, c_mode, c_err) BIND(C, name="my_mkdir")
    USE ISO_C_BINDING, ONLY: C_CHAR, C_INT
    CHARACTER(kind=C_CHAR) :: string(*)
    INTEGER(C_INT), INTENT(in), VALUE :: c_mode
    INTEGER(C_INT) :: c_err
  END SUBROUTINE my_mkdir
END INTERFACE

c_mode = mode
IF (INDEX(dirname, '/') .NE. 0) THEN
  CALL my_mkdir(str2_carr(TRIM(dirname)), c_mode, err)
ELSE
  err = c_mkdir(str2_carr(TRIM(dirname)), c_mode)
END IF
ierr = err ! c_int to default integer kind
END PROCEDURE system_mkdir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_opendir
INTERFACE
  FUNCTION c_opendir(c_dirname) BIND(c, name="opendir") RESULT(c_dir)
    IMPORT C_CHAR, C_INT, C_PTR
    CHARACTER(kind=C_CHAR), INTENT(in) :: c_dirname(*)
    TYPE(C_PTR) :: c_dir
  END FUNCTION c_opendir
END INTERFACE

ierr = 0
dir = c_opendir(str2_carr(TRIM(dirname)))
IF (.NOT. C_ASSOCIATED(dir)) THEN
  WRITE (*, '(a)') '*system_opendir* Error opening '//TRIM(dirname)
  ierr = -1
END IF
END PROCEDURE system_opendir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_readdir
INTEGER(kind=C_INT) :: ierr_local
CHARACTER(kind=C_CHAR, len=1) :: buf(4097)

INTERFACE
  SUBROUTINE c_readdir(c_dir, c_filename, c_ierr) BIND(C, NAME='my_readdir')
    IMPORT C_CHAR, C_INT, C_PTR
    TYPE(C_PTR), VALUE :: c_dir
    CHARACTER(kind=C_CHAR) :: c_filename(*)
    INTEGER(kind=C_INT) :: c_ierr
  END SUBROUTINE c_readdir
END INTERFACE

buf = ' '
ierr_local = 0
CALL c_readdir(dir, buf, ierr_local)
filename = TRIM(arr2str(buf))
ierr = ierr_local
END PROCEDURE system_readdir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_rewinddir
INTERFACE
  SUBROUTINE c_rewinddir(c_dir) BIND(c, name="rewinddir")
    IMPORT C_CHAR, C_INT, C_PTR
    TYPE(C_PTR), VALUE :: c_dir
  END SUBROUTINE c_rewinddir
END INTERFACE

CALL c_rewinddir(dir)
END PROCEDURE system_rewinddir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_closedir
INTEGER :: ierr_local

INTERFACE
  FUNCTION c_closedir(c_dir) BIND(c, name="closedir") RESULT(c_err)
    IMPORT C_CHAR, C_INT, C_PTR
    TYPE(C_PTR), VALUE :: c_dir
    INTEGER(kind=C_INT) :: c_err
  END FUNCTION c_closedir
END INTERFACE

ierr_local = c_closedir(dir)
IF (PRESENT(ierr)) THEN
  ierr = ierr_local
ELSE
  IF (ierr_local /= 0) THEN
    PRINT *, "*system_closedir* error", ierr_local
    STOP 3
  END IF
END IF
END PROCEDURE system_closedir

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE fileglob
CHARACTER(len=255) :: tmpfile
! scratch filename to hold expanded file list
CHARACTER(len=255) :: cmd
! string to build system command in
INTEGER :: iotmp
! needed to open unique scratch file for holding file list
INTEGER :: i, ios, icount
write(tmpfile,'(*(g0))')'/tmp/__filelist_',timestamp(),'_',system_getpid()
! preliminary scratch file name
cmd = 'ls -d '//TRIM(glob)//'>'//TRIM(tmpfile)//' '
! build command string
CALL execute_command_line(cmd)
! Execute the command specified by the string.
OPEN (newunit=iotmp, file=tmpfile, iostat=ios)
! open unique scratch filename
IF (ios .NE. 0) RETURN
! the open failed
icount = 0
! number of filenames in expanded list
DO
! count the number of lines (assumed ==files) so know what to allocate
  READ (iotmp, '(a)', iostat=ios)
  ! move down a line in the file to count number of lines
  IF (ios .NE. 0) EXIT
  ! hopefully, this is because end of file was encountered so done
  icount = icount + 1
  ! increment line count
END DO
REWIND (iotmp)
! rewind file list so can read and store it
ALLOCATE (list(icount))
! allocate and fill the array
DO i = 1, icount
  READ (iotmp, '(a)') list(i)
  ! read a filename from a line
END DO
CLOSE (iotmp, status='delete', iostat=ios)
! close and delete scratch file
END PROCEDURE fileglob

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE system_dir
CHARACTER(len=:), ALLOCATABLE :: wild
TYPE(C_PTR) :: dir
CHARACTER(len=:), ALLOCATABLE :: filename
INTEGER :: i, ierr, icount, longest
longest = 0
icount = 0
IF (PRESENT(pattern)) THEN
  wild = pattern
ELSE
  wild = '*'
END IF
IF (PRESENT(directory)) THEN !--- open directory stream to read from
  CALL system_opendir(directory, dir, ierr)
ELSE
  CALL system_opendir('.', dir, ierr)
END IF
IF (ierr .EQ. 0) THEN
  DO i = 1, 2 !--- read directory stream twice, first time to get size
    DO
      CALL system_readdir(dir, filename, ierr)
      IF (filename .EQ. ' ') EXIT
      IF (wild .NE. '*') THEN
        IF (.NOT. matchw(filename, wild)) CYCLE ! Call a wildcard matching routine.
      END IF
      icount = icount + 1
      SELECT CASE (i)
      CASE (1)
        longest = MAX(longest, LEN(filename))
      CASE (2)
        system_dir(icount) = filename
      END SELECT
    END DO
    IF (i .EQ. 1) THEN
      CALL system_rewinddir(dir)
      IF (ALLOCATED(system_dir)) DEALLOCATE (system_dir)
      ALLOCATE (CHARACTER(len=longest) :: system_dir(icount))
      icount = 0
    END IF
  END DO
END IF
CALL system_closedir(dir, ierr) !--- close directory stream
END PROCEDURE system_dir

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

END SUBMODULE Methods
