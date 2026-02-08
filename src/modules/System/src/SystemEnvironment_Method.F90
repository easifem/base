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

MODULE SystemEnvironment_Method
USE ISO_C_BINDING, ONLY: C_LONG
IMPLICIT NONE

PRIVATE
PUBLIC :: System_Putenv
PUBLIC :: System_Getenv
PUBLIC :: Set_Environment_Variable
PUBLIC :: System_Unsetenv
PUBLIC :: System_Readenv
PUBLIC :: System_Clearenv

INTEGER(C_LONG), BIND(c, name="longest_env_variable") :: &
  LONGEST_ENV_VARIABLE

!----------------------------------------------------------------------------
!                                                       System_Putenv@Methods
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
    CHARACTER(*), INTENT(IN) :: string
    !! string of format "NAME=value".
    !! If name does not already exist in the environment,
    !! then string is added to the environment.
    !! If name does exist, then the value of name in the environment is
    !! changed to value.
    !! The string passed to putenv(3c) becomes part of the environment,
    !! so this routine creates a string each time it is called that
    !! increases the amount of
    !! memory the program uses.
    INTEGER, OPTIONAL, INTENT(OUT) :: err
    !! The system_putenv() function returns zero on success,
    !! or nonzero if an error occurs.
    !! A non-zero error usually indicates sufficient memory
    !! does not exist to store the
    !! variable.
  END SUBROUTINE System_Putenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                      System_Getenv@Methods
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
!                                            Set_Environment_Variable@Methods
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
!                                                     System_Clearenv@Methods
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
!                                                     System_Unsetenv@Methods
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
    CHARACTER(*), INTENT(IN) :: name
    !! name of variable to delete.
    !! If name does not exist in the environment, then the
    !! function succeeds, and the environment is unchanged.
    INTEGER, INTENT(OUT), OPTIONAL :: ierr
    !! The system_unsetenv(3f) function returns zero on success,
    !! or -1 on error.
    !! name is NULL, points to a string of length 0, or
    !! contains an '=' character.
    !! Insufficient memory to add a new variable to the environment.
  END SUBROUTINE System_Unsetenv
END INTERFACE

!----------------------------------------------------------------------------
!                                                      System_Readenv@Methods
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
    CHARACTER(:), ALLOCATABLE :: string
    !! the string returned from the environment of the form "NAME=VALUE"
  END FUNCTION System_Readenv
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SystemEnvironment_Method
