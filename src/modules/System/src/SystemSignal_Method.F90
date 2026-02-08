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
!# System_Signal
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

MODULE SystemSignal_Method
USE ISO_C_BINDING, ONLY: C_FUNPTR
USE ISO_C_BINDING, ONLY: C_INT
IMPLICIT NONE

PRIVATE
PUBLIC :: System_Signal
PUBLIC :: handler
PUBLIC :: handler_ptr_array
PUBLIC :: f_handler

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  ! mold for signal handler to be installed by system_signal
  SUBROUTINE handler(signum)
    IMPORT :: C_INT
    INTEGER(C_INT), INTENT(IN) :: signum
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

INTEGER, PARAMETER :: NO_OF_SIGNALS = 64
!!  obtained with command: kill -l
TYPE(handler_pointer) :: handler_ptr_array(NO_OF_SIGNALS)

!----------------------------------------------------------------------------
!                                                          f_handler@Methods
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
!                                                      System_Signal@Methods
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
    INTEGER, INTENT(IN) :: signum
    PROCEDURE(handler), OPTIONAL :: handler_routine
    TYPE(C_FUNPTR) :: ret, c_handler
  END SUBROUTINE System_Signal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SystemSignal_Method
